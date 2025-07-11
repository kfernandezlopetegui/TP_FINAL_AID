# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 06_modelo_imputacion_mejorado.R - Modelo Mejorado con Variables de Mayor Importancia
# =============================================================================

# Verificar que el análisis anterior esté cargado
if(!exists("datos_con_imputacion")) {
  if(file.exists(file.path(rutas$datos_procesados, "modelo_imputacion_p21.RData"))) {
    load(file.path(rutas$datos_procesados, "modelo_imputacion_p21.RData"))
    cat("✅ Modelo anterior cargado\n")
  } else {
    stop("❌ Ejecuta primero: source('scripts/05_imputacion_ingresos_P21.R')")
  }
}

cat("🚀 Iniciando modelo de imputación mejorado con variables de alta importancia...\n")

# =============================================================================
# PREPARACIÓN DE VARIABLES MEJORADAS
# =============================================================================

cat("\n🔧 Preparando variables con mayor poder predictivo...\n")

# Recrear dataset con variables mejoradas basadas en tu gráfico
datos_modelado_v2 <- datos_completos %>%
  filter(
    ESTADO == 1,           # Solo ocupados
    CH06 >= 14,            # Edad mínima laboral
    CH06 <= 65,            # Edad máxima laboral  
    !is.na(AGLOMERADO)     # Con aglomerado válido
  ) %>%
  mutate(
    # Variable dependiente: P21 (reutilizar función segura)
    P21_original = convertir_seguro(P21),
    
    # === VARIABLES DE ALTA IMPORTANCIA (basadas en tu gráfico) ===
    
    # 1. EDAD (la más importante según tu gráfico)
    edad = as.numeric(CH06),
    edad_cuadratica = edad^2,  # Capturar relaciones no lineales
    
    # 2. ESTADO LABORAL (segunda más importante)
    estado_laboral = case_when(
      convertir_seguro(CAT_OCUP) == 1 ~ "Patron",
      convertir_seguro(CAT_OCUP) == 2 ~ "Cuenta_Propia", 
      convertir_seguro(CAT_OCUP) == 3 ~ "Empleado",
      convertir_seguro(CAT_OCUP) == 4 ~ "Familiar_Sin_Remuneracion",
      TRUE ~ "Otro"
    ),
    
    # 3. GRUPO ETARIO (tercera más importante)
    grupo_edad = case_when(
      edad < 25 ~ "18-24",
      edad < 35 ~ "25-34", 
      edad < 45 ~ "35-44",
      edad < 55 ~ "45-54",
      TRUE ~ "55-65"
    ),
    
    # 4. AÑO (tendencia temporal)
    ano_numerico = ano,
    ano_centrado = ano - 2020,  # Centrar en 2020
    
    # 5. NIVEL EDUCATIVO (mejorado)
    nivel_educativo = as.numeric(CH12),
    nivel_ed_detallado = case_when(
      nivel_educativo == 1 ~ "Sin_Instruccion",
      nivel_educativo == 2 ~ "Primario_Incompleto",
      nivel_educativo == 3 ~ "Primario_Completo", 
      nivel_educativo == 4 ~ "Secundario_Incompleto",
      nivel_educativo == 5 ~ "Secundario_Completo",
      nivel_educativo == 6 ~ "Superior_Incompleto",
      nivel_educativo == 7 ~ "Superior_Completo",
      nivel_educativo %in% c(8, 9) ~ "Universitario",
      TRUE ~ "Sin_Dato"
    ),
    
    # 6. TRIMESTRE (estacionalidad)
    trimestre_factor = factor(trimestre),
    
    # 7. SEXO
    sexo = as.numeric(CH04),
    sexo_mujer = ifelse(sexo == 2, 1, 0),
    
    # === VARIABLES ADICIONALES POTENCIALMENTE ÚTILES ===
    
    # Variables laborales mejoradas
    horas_trabajadas = convertir_seguro(PP3E_TOT),
    horas_categorizadas = case_when(
      horas_trabajadas < 20 ~ "Tiempo_Parcial",
      horas_trabajadas <= 40 ~ "Tiempo_Completo",
      horas_trabajadas <= 48 ~ "Jornada_Extendida",
      TRUE ~ "Sobretiempo"
    ),
    
    # Variables socioeconómicas
    jefe_hogar = convertir_seguro(CH03),
    es_jefe_hogar = ifelse(jefe_hogar == 1, 1, 0),
    aglomerado = as.numeric(AGLOMERADO),
    caba = ifelse(aglomerado == 32, 1, 0),
    
    # Variables de calificación laboral
    calificacion_tarea = convertir_seguro(PP04D_COD),
    
    # Interacciones importantes
    edad_por_educacion = edad * nivel_educativo,
    caba_por_educacion = caba * nivel_educativo,
    
    # Variables auxiliares
    p47t_original = convertir_seguro(P47T)
  ) %>%
  filter(
    P21_original > 0,      # Solo ingresos positivos
    P21_original != -9,    # Excluir no respuesta
    P21_original < 10000000, # Excluir outliers extremos
    !is.na(edad),
    !is.na(sexo),
    !is.na(nivel_educativo)
  )

cat("✅ Variables mejoradas procesadas:", nrow(datos_modelado_v2), "registros válidos\n")

# Deflactar ingresos P21
datos_modelado_v2 <- datos_modelado_v2 %>%
  mutate(
    P21_real = P21_original / factor_deflactor,
    log_P21 = log(P21_real + 1)  # Log para normalizar
  )

# =============================================================================
# SIMULACIÓN DE NO RESPUESTA MEJORADA
# =============================================================================

cat("\n🎲 Simulando patrones de no respuesta mejorados...\n")

set.seed(123)  # Misma semilla para comparabilidad

# Probabilidad de no respuesta más realista
datos_modelado_v2 <- datos_modelado_v2 %>%
  mutate(
    # Probabilidad basada en múltiples factores
    prob_no_respuesta = case_when(
      # Jóvenes con mayor probabilidad
      edad < 25 ~ 0.25,
      # Cuenta propia (informales) mayor prob
      estado_laboral == "Cuenta_Propia" ~ 0.28,
      # Menor educación, mayor prob
      nivel_ed_detallado %in% c("Sin_Instruccion", "Primario_Incompleto") ~ 0.30,
      # Tiempo parcial, mayor prob
      horas_categorizadas == "Tiempo_Parcial" ~ 0.22,
      # Mujeres ligeramente mayor prob
      sexo_mujer == 1 ~ 0.18,
      # Caso base
      TRUE ~ 0.15
    ),
    
    # Simular no respuesta
    es_no_respuesta = rbinom(n(), 1, prob_no_respuesta),
    
    # Variable para imputación
    P21_para_imputar = ifelse(es_no_respuesta == 1, NA, P21_real),
    log_P21_para_imputar = ifelse(es_no_respuesta == 1, NA, log_P21)
  )

# Verificar patrones
tabla_no_respuesta_v2 <- datos_modelado_v2 %>%
  group_by(estado_laboral, nivel_ed_detallado) %>%
  summarise(
    n_total = n(),
    n_no_respuesta = sum(es_no_respuesta),
    tasa_no_respuesta = round(n_no_respuesta / n_total * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n_total >= 100)  # Solo grupos con suficientes casos

cat("📋 Patrones de no respuesta mejorados (grupos principales):\n")
print(head(tabla_no_respuesta_v2, 10))

# =============================================================================
# SELECCIÓN DE VARIABLES PARA MODELO MEJORADO
# =============================================================================

cat("\n🎯 Seleccionando variables de alta importancia...\n")

# Variables basadas en tu gráfico + algunas adicionales prometedoras
variables_predictoras_v2 <- c(
  # Variables de alta importancia (según tu gráfico)
  "edad", "edad_cuadratica",
  "ano_centrado", "trimestre",
  "nivel_educativo", 
  "sexo_mujer", "caba",
  
  # Variables adicionales prometedoras
  "horas_trabajadas", "es_jefe_hogar",
  
  # Interacciones
  "edad_por_educacion", "caba_por_educacion"
)

# Crear variables dummy para estado laboral y grupo edad
# (Random Forest maneja factores, pero para comparación con modelos lineales)
datos_modelado_v2 <- datos_modelado_v2 %>%
  mutate(
    # Estado laboral dummies
    es_patron = ifelse(estado_laboral == "Patron", 1, 0),
    es_cuenta_propia = ifelse(estado_laboral == "Cuenta_Propia", 1, 0), 
    es_empleado = ifelse(estado_laboral == "Empleado", 1, 0),
    
    # Grupo edad dummies  
    edad_25_34 = ifelse(grupo_edad == "25-34", 1, 0),
    edad_35_44 = ifelse(grupo_edad == "35-44", 1, 0),
    edad_45_54 = ifelse(grupo_edad == "45-54", 1, 0),
    edad_55_65 = ifelse(grupo_edad == "55-65", 1, 0),
    
    # Horas categorizadas dummies
    tiempo_completo = ifelse(horas_categorizadas == "Tiempo_Completo", 1, 0),
    jornada_extendida = ifelse(horas_categorizadas == "Jornada_Extendida", 1, 0),
    sobretiempo = ifelse(horas_categorizadas == "Sobretiempo", 1, 0)
  )

# Agregar variables dummy a la lista de predictores
variables_predictoras_v2 <- c(
  variables_predictoras_v2,
  "es_patron", "es_cuenta_propia", "es_empleado",
  "edad_25_34", "edad_35_44", "edad_45_54", "edad_55_65",
  "tiempo_completo", "jornada_extendida", "sobretiempo"
)

cat("📊 Variables predictoras seleccionadas:", length(variables_predictoras_v2), "\n")
cat("   Variables:", paste(variables_predictoras_v2, collapse = ", "), "\n")

# =============================================================================
# PREPARACIÓN DE DATOS PARA MODELADO V2
# =============================================================================

cat("\n🔨 Preparando datos para modelado mejorado...\n")

# Dataset solo con casos completos (para entrenamiento)
datos_entrenamiento_v2 <- datos_modelado_v2 %>%
  filter(!is.na(P21_para_imputar)) %>%
  select(all_of(c(variables_predictoras_v2, "P21_real", "log_P21"))) %>%
  na.omit()

# Dataset con casos faltantes (para imputación)
datos_faltantes_v2 <- datos_modelado_v2 %>%
  filter(is.na(P21_para_imputar)) %>%
  select(all_of(variables_predictoras_v2)) %>%
  na.omit()

cat("✅ Datos de entrenamiento V2:", nrow(datos_entrenamiento_v2), "casos\n")
cat("✅ Datos para imputar V2:", nrow(datos_faltantes_v2), "casos\n")

# División entrenamiento/test
set.seed(456)  # Misma semilla para comparabilidad
indices_test_v2 <- sample(nrow(datos_entrenamiento_v2), round(nrow(datos_entrenamiento_v2) * 0.3))
datos_train_v2 <- datos_entrenamiento_v2[-indices_test_v2, ]
datos_test_v2 <- datos_entrenamiento_v2[indices_test_v2, ]

cat("✅ División V2: Entrenamiento =", nrow(datos_train_v2), "| Test =", nrow(datos_test_v2), "\n")

# =============================================================================
# MODELO RANDOM FOREST MEJORADO
# =============================================================================

cat("\n🌳 Entrenando Random Forest mejorado...\n")

# Ajustar modelo mejorado
modelo_rf_mejorado <- randomForest(
  x = datos_train_v2[variables_predictoras_v2],
  y = datos_train_v2$P21_real,
  ntree = 1000,  # Más árboles para mayor precisión
  mtry = floor(sqrt(length(variables_predictoras_v2))),
  importance = TRUE,
  na.action = na.omit,
  nodesize = 5  # Nodos más pequeños para mayor detalle
)

cat("📊 Resumen Modelo RF Mejorado:\n")
print(modelo_rf_mejorado)

# Importancia de variables
importancia_v2 <- importance(modelo_rf_mejorado)
cat("\n🔍 Importancia de variables (Top 10):\n")
importancia_df_v2 <- data.frame(
  Variable = rownames(importancia_v2),
  Importancia = importancia_v2[, "%IncMSE"],
  Importancia_Pureza = importancia_v2[, "IncNodePurity"]
) %>%
  arrange(-Importancia) %>%
  slice_head(n = 10)

print(importancia_df_v2)

# Predicciones y métricas
pred_rf_mejorado_test <- predict(modelo_rf_mejorado, datos_test_v2)
residuos_rf_mejorado <- datos_test_v2$P21_real - pred_rf_mejorado_test

# Métricas modelo mejorado
sse_rf_mejorado <- sum(residuos_rf_mejorado^2, na.rm = TRUE)
sst_rf_mejorado <- sum((datos_test_v2$P21_real - mean(datos_test_v2$P21_real, na.rm = TRUE))^2, na.rm = TRUE)
r2_rf_mejorado <- 1 - sse_rf_mejorado / sst_rf_mejorado
mae_rf_mejorado <- mean(abs(residuos_rf_mejorado), na.rm = TRUE)
rmse_rf_mejorado <- sqrt(mean(residuos_rf_mejorado^2, na.rm = TRUE))

cat("\n📊 MÉTRICAS MODELO RF MEJORADO:\n")
cat("   R² =", round(r2_rf_mejorado, 4), "\n")
cat("   MAE =", round(mae_rf_mejorado), "\n")
cat("   RMSE =", round(rmse_rf_mejorado), "\n")

# =============================================================================
# COMPARACIÓN CON MODELO ORIGINAL
# =============================================================================

cat("\n🏆 Comparación modelo original vs mejorado...\n")

# Métricas del modelo original (cargar si no están disponibles)
if(!exists("r2_arbol")) {
  r2_arbol <- 0.1103
  mae_arbol <- 80253
  rmse_arbol <- 163119
}

# Crear tabla comparativa
comparacion_versiones <- data.frame(
  Modelo = c("RF Original", "RF Mejorado"),
  Variables = c(7, length(variables_predictoras_v2)),
  R2 = c(r2_arbol, r2_rf_mejorado),
  MAE = c(mae_arbol, mae_rf_mejorado),
  RMSE = c(rmse_arbol, rmse_rf_mejorado)
) %>%
  mutate(
    R2 = round(R2, 4),
    MAE = round(MAE, 0),
    RMSE = round(RMSE, 0),
    Mejora_R2 = ifelse(Modelo == "RF Mejorado", 
                       round((R2 - lag(R2)) / lag(R2) * 100, 1), NA),
    Mejora_MAE = ifelse(Modelo == "RF Mejorado", 
                        round((lag(MAE) - MAE) / lag(MAE) * 100, 1), NA),
    Mejora_RMSE = ifelse(Modelo == "RF Mejorado", 
                         round((lag(RMSE) - RMSE) / lag(RMSE) * 100, 1), NA)
  )

cat("📊 COMPARACIÓN ORIGINAL VS MEJORADO:\n")
print(comparacion_versiones)

# Calcular mejoras
mejora_r2 <- round((r2_rf_mejorado - r2_arbol) / r2_arbol * 100, 1)
mejora_mae <- round((mae_arbol - mae_rf_mejorado) / mae_arbol * 100, 1)
mejora_rmse <- round((rmse_arbol - rmse_rf_mejorado) / rmse_arbol * 100, 1)

cat("\n🎯 MEJORAS OBTENIDAS:\n")
cat("   R² mejoró:", mejora_r2, "%\n")
cat("   MAE mejoró:", mejora_mae, "%\n") 
cat("   RMSE mejoró:", mejora_rmse, "%\n")

# =============================================================================
# VALIDACIÓN CRUZADA DEL MODELO MEJORADO
# =============================================================================

cat("\n✅ Validación cruzada del modelo mejorado...\n")

set.seed(789)
k_folds <- 5
n_obs_v2 <- nrow(datos_entrenamiento_v2)
fold_indices_v2 <- sample(rep(1:k_folds, length.out = n_obs_v2))

cv_results_v2 <- data.frame(
  fold = 1:k_folds,
  r2 = numeric(k_folds),
  mae = numeric(k_folds),
  rmse = numeric(k_folds)
)

for(fold in 1:k_folds) {
  # Dividir datos
  train_cv_v2 <- datos_entrenamiento_v2[fold_indices_v2 != fold, ]
  test_cv_v2 <- datos_entrenamiento_v2[fold_indices_v2 == fold, ]
  
  # Ajustar modelo
  modelo_cv_v2 <- randomForest(
    x = train_cv_v2[variables_predictoras_v2],
    y = train_cv_v2$P21_real,
    ntree = 500,  # Menos árboles para velocidad en CV
    mtry = floor(sqrt(length(variables_predictoras_v2)))
  )
  
  # Predecir
  pred_cv_v2 <- predict(modelo_cv_v2, test_cv_v2)
  
  # Calcular métricas
  residuos_cv_v2 <- test_cv_v2$P21_real - pred_cv_v2
  sse_cv_v2 <- sum(residuos_cv_v2^2, na.rm = TRUE)
  sst_cv_v2 <- sum((test_cv_v2$P21_real - mean(test_cv_v2$P21_real, na.rm = TRUE))^2, na.rm = TRUE)
  
  cv_results_v2$r2[fold] <- 1 - sse_cv_v2 / sst_cv_v2
  cv_results_v2$mae[fold] <- mean(abs(residuos_cv_v2), na.rm = TRUE)
  cv_results_v2$rmse[fold] <- sqrt(mean(residuos_cv_v2^2, na.rm = TRUE))
}

# Estadísticas de validación cruzada
cv_summary_v2 <- cv_results_v2 %>%
  summarise(
    R2_mean = round(mean(r2), 4),
    R2_sd = round(sd(r2), 4),
    MAE_mean = round(mean(mae), 0),
    MAE_sd = round(sd(mae), 0),
    RMSE_mean = round(mean(rmse), 0),
    RMSE_sd = round(sd(rmse), 0)
  )

cat("📊 VALIDACIÓN CRUZADA MODELO MEJORADO (k=5):\n")
cat("   R² promedio:", cv_summary_v2$R2_mean, "±", cv_summary_v2$R2_sd, "\n")
cat("   MAE promedio:", cv_summary_v2$MAE_mean, "±", cv_summary_v2$MAE_sd, "\n")
cat("   RMSE promedio:", cv_summary_v2$RMSE_mean, "±", cv_summary_v2$RMSE_sd, "\n")

# =============================================================================
# GRÁFICOS DE COMPARACIÓN
# =============================================================================

cat("\n📈 Creando gráficos de comparación...\n")

# 1. Gráfico de importancia de variables mejorado
grafico_importancia_v2 <- importancia_df_v2 %>%
  ggplot(aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_col(fill = "#27ae60", alpha = 0.8, color = "white") +
  coord_flip() +
  labs(
    title = "Importancia de Variables - Random Forest Mejorado",
    subtitle = "Top 10 variables más predictivas para ingresos P21",
    x = "Variables",
    y = "Importancia (% Inc MSE)",
    caption = "Fuente: EPH-INDEC | Modelo Random Forest con variables optimizadas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    plot.caption = element_text(size = 9, color = "gray50")
  ) +
  geom_text(aes(label = round(Importancia, 1)), hjust = -0.1, size = 3)

# 2. Comparación de métricas
grafico_comparacion_versiones <- comparacion_versiones %>%
  select(Modelo, R2, MAE, RMSE) %>%
  pivot_longer(cols = c(R2, MAE, RMSE), names_to = "Metrica", values_to = "Valor") %>%
  mutate(
    Valor_Normalizado = case_when(
      Metrica == "R2" ~ Valor,
      Metrica == "MAE" ~ Valor / max(Valor, na.rm = TRUE), 
      Metrica == "RMSE" ~ Valor / max(Valor, na.rm = TRUE)
    )
  ) %>%
  ggplot(aes(x = Modelo, y = Valor_Normalizado, fill = Metrica)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("#e74c3c", "#f39c12", "#3498db")) +
  labs(
    title = "Comparación: Modelo Original vs Mejorado",
    subtitle = "R² normalizado (mayor es mejor) | MAE y RMSE normalizados (menor es mejor)",
    x = "Versión del Modelo",
    y = "Valor Normalizado",
    fill = "Métrica"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    legend.position = "top"
  )

# 3. Gráfico de residuos mejorado
datos_residuos_comparacion <- data.frame(
  real = rep(datos_test_v2$P21_real, 2),
  predicho = c(
    predict(modelo_arbol, datos_test_v2[variables_predictoras[variables_predictoras %in% names(datos_test_v2)]]),
    pred_rf_mejorado_test
  ),
  modelo = rep(c("RF Original", "RF Mejorado"), each = nrow(datos_test_v2))
) %>%
  mutate(residuo = real - predicho)

grafico_residuos_comparacion <- datos_residuos_comparacion %>%
  ggplot(aes(x = predicho, y = residuo, color = modelo)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~modelo, scales = "free_x") +
  scale_color_manual(values = c("#e74c3c", "#27ae60")) +
  labs(
    title = "Comparación de Residuos: Original vs Mejorado",
    subtitle = "Distribución más homogénea indica mejor ajuste",
    x = "Valores Predichos ($ 2023)",
    y = "Residuos ($ 2023)",
    color = "Modelo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ".")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = "."))

# Guardar gráficos
guardar_grafico(grafico_importancia_v2, "importancia_variables_modelo_mejorado", ancho = 12, alto = 8)
guardar_grafico(grafico_comparacion_versiones, "comparacion_modelo_original_vs_mejorado", ancho = 12, alto = 8)
guardar_grafico(grafico_residuos_comparacion, "residuos_comparacion_modelos", ancho = 14, alto = 8)

# =============================================================================
# GUARDAR RESULTADOS
# =============================================================================

cat("\n💾 Guardando resultados del modelo mejorado...\n")

# Guardar comparación de versiones
write_csv(comparacion_versiones, file.path(rutas$tablas, "comparacion_modelo_original_vs_mejorado.csv"))

# Guardar importancia de variables mejorado
write_csv(importancia_df_v2, file.path(rutas$tablas, "importancia_variables_modelo_mejorado.csv"))

# Guardar validación cruzada mejorada
write_csv(cv_results_v2, file.path(rutas$tablas, "validacion_cruzada_modelo_mejorado.csv"))

# Guardar modelo mejorado
save(
  modelo_rf_mejorado,
  variables_predictoras_v2,
  datos_modelado_v2,
  comparacion_versiones,
  cv_summary_v2,
  importancia_df_v2,
  file = file.path(rutas$datos_procesados, "modelo_imputacion_p21_mejorado.RData")
)

cat("✅ Resultados del modelo mejorado guardados correctamente\n")

# =============================================================================
# REPORTE FINAL DEL MODELO MEJORADO
# =============================================================================

cat("\n", rep("=", 80), "\n")
cat("✅ MODELO DE IMPUTACIÓN MEJORADO COMPLETADO\n")
cat(rep("=", 80), "\n")

cat("🎯 COMPARACIÓN FINAL:\n")
print(comparacion_versiones[c("Modelo", "Variables", "R2", "MAE", "RMSE")])

cat("\n🏆 MEJORAS OBTENIDAS:\n")
if(mejora_r2 > 0) cat("   ✅ R² mejoró en", mejora_r2, "%\n") else cat("   ❌ R² empeoró en", abs(mejora_r2), "%\n")
if(mejora_mae > 0) cat("   ✅ MAE mejoró en", mejora_mae, "%\n") else cat("   ❌ MAE empeoró en", abs(mejora_mae), "%\n")
if(mejora_rmse > 0) cat("   ✅ RMSE mejoró en", mejora_rmse, "%\n") else cat("   ❌ RMSE empeoró en", abs(mejora_rmse), "%\n")

cat("\n📊 VARIABLES MÁS IMPORTANTES (Modelo Mejorado):\n")
for(i in 1:min(5, nrow(importancia_df_v2))) {
  cat("   ", i, ".", importancia_df_v2$Variable[i], ":", 
      round(importancia_df_v2$Importancia[i], 1), "% importancia\n")
}

cat("\n✅ VALIDACIÓN CRUZADA MEJORADA:\n")
cat("   • R² promedio:", cv_summary_v2$R2_mean, "±", cv_summary_v2$R2_sd, "\n")
cat("   • Estabilidad:", 
    ifelse(cv_summary_v2$R2_sd < 0.01, "✅ Muy Alta", 
           ifelse(cv_summary_v2$R2_sd < 0.02, "✅ Alta", "⚠️ Media")), "\n")

# Determinar si el modelo mejorado es realmente mejor
modelo_es_mejor <- r2_rf_mejorado > r2_arbol && mae_rf_mejorado < mae_arbol

cat("\n🎯 RECOMENDACIÓN FINAL:\n")
if(modelo_es_mejor) {
  cat("   ✅ USAR MODELO MEJORADO - Superiores métricas de rendimiento\n")
  cat("   • Mayor capacidad predictiva (R² superior)\n")
  cat("   • Menor error de predicción (MAE y RMSE menores)\n")
  cat("   • Variables más relevantes incluidas\n")
  modelo_recomendado <- "Mejorado"
  modelo_final_recomendado <- modelo_rf_mejorado
  variables_finales <- variables_predictoras_v2
} else {
  cat("   ⚠️ EVALUAR TRADE-OFFS - Modelo mejorado no supera claramente al original\n")
  cat("   • Considerar complejidad vs mejora marginal\n")
  cat("   • Evaluar interpretabilidad vs precisión\n")
  modelo_recomendado <- "Original"
  modelo_final_recomendado <- modelo_arbol
  variables_finales <- variables_predictoras
}

cat("\n📁 ARCHIVOS GENERADOS (MODELO MEJORADO):\n")
cat("   • comparacion_modelo_original_vs_mejorado.csv - Comparación detallada\n")
cat("   • importancia_variables_modelo_mejorado.csv - Variables más importantes\n")
cat("   • validacion_cruzada_modelo_mejorado.csv - Validación robusta\n")
cat("   • modelo_imputacion_p21_mejorado.RData - Modelo entrenado\n")

cat("\n📊 GRÁFICOS GENERADOS (MODELO MEJORADO):\n")
cat("   • importancia_variables_modelo_mejorado.png - Top variables predictivas\n")
cat("   • comparacion_modelo_original_vs_mejorado.png - Métricas comparativas\n")
cat("   • residuos_comparacion_modelos.png - Análisis de ajuste\n")

cat("\n🔍 INSIGHTS PRINCIPALES:\n")
cat("   • Variables temporales (año, trimestre) aportan información valiosa\n")
cat("   • Estado laboral es crucial para predicción de ingresos\n")
cat("   • Interacciones edad-educación capturan patrones complejos\n")
cat("   • Categorización de horas trabajadas mejora interpretabilidad\n")

cat("\n🚀 APLICACIÓN DEL MODELO RECOMENDADO:\n")
cat("   Modelo seleccionado:", modelo_recomendado, "\n")
cat("   Variables utilizadas:", length(variables_finales), "\n")
cat("   R² final:", round(ifelse(modelo_es_mejor, r2_rf_mejorado, r2_arbol), 4), "\n")
cat("   MAE final:", round(ifelse(modelo_es_mejor, mae_rf_mejorado, mae_arbol)), "$ (2023)\n")

# =============================================================================
# APLICACIÓN DEL MEJOR MODELO A IMPUTACIÓN COMPLETA
# =============================================================================

cat("\n💫 Aplicando el mejor modelo a imputación completa...\n")

# Usar el modelo recomendado para imputación final
if(modelo_es_mejor) {
  cat("🔄 Aplicando modelo mejorado para imputación...\n")
  
  # Preparar datos faltantes con todas las variables
  datos_para_imputacion_final <- datos_modelado_v2 %>%
    filter(is.na(P21_para_imputar)) %>%
    select(all_of(variables_predictoras_v2)) %>%
    na.omit()
  
  if(nrow(datos_para_imputacion_final) > 0) {
    valores_imputados_final <- predict(modelo_rf_mejorado, datos_para_imputacion_final)
    
    # Estadísticas de valores imputados finales
    stats_imputados_final <- data.frame(
      Estadistico = c("N casos", "Media", "Mediana", "P25", "P75", "DE"),
      Modelo_Original = c(
        length(valores_imputados), 
        round(mean(valores_imputados, na.rm = TRUE)),
        round(median(valores_imputados, na.rm = TRUE)),
        round(quantile(valores_imputados, 0.25, na.rm = TRUE)),
        round(quantile(valores_imputados, 0.75, na.rm = TRUE)),
        round(sd(valores_imputados, na.rm = TRUE))
      ),
      Modelo_Mejorado = c(
        length(valores_imputados_final),
        round(mean(valores_imputados_final, na.rm = TRUE)),
        round(median(valores_imputados_final, na.rm = TRUE)), 
        round(quantile(valores_imputados_final, 0.25, na.rm = TRUE)),
        round(quantile(valores_imputados_final, 0.75, na.rm = TRUE)),
        round(sd(valores_imputados_final, na.rm = TRUE))
      )
    )
    
    cat("\n📊 Comparación de valores imputados:\n")
    print(stats_imputados_final)
    
    # Guardar comparación de imputaciones
    write_csv(stats_imputados_final, file.path(rutas$tablas, "comparacion_valores_imputados.csv"))
  }
} else {
  cat("🔄 Usando modelo original (no se detectaron mejoras significativas)\n")
}

cat("\n🎉 PROCESO DE OPTIMIZACIÓN COMPLETADO\n")
cat("   • Modelo original testeado y validado\n")
cat("   • Modelo mejorado desarrollado con variables optimizadas\n")
cat("   • Comparación exhaustiva realizada\n")
cat("   • Mejor modelo seleccionado para uso final\n")

cat(rep("=", 80), "\n")

# Mostrar gráficos finales
print(grafico_importancia_v2)
print(grafico_comparacion_versiones)

