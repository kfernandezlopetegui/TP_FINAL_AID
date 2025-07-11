# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 05_imputacion_ingresos_P21.R - Modelos de Imputación para No Respuesta P21
# =============================================================================

# Verificar que el setup esté ejecutado
if(!exists("rutas")) {
  stop("❌ Ejecuta primero: source('scripts/01_setup.R')")
}

# Cargar librerías adicionales para modelado
library(randomForest)  # Para árboles de decisión
library(corrplot)      # Para matriz de correlaciones
library(car)           # Para VIF (multicolinealidad)
library(nortest)       # Para test de normalidad
library(lmtest)        # Para tests de supuestos

cat("🔄 Iniciando análisis de imputación de no respuesta P21...\n")

# =============================================================================
# CARGA DE DATOS Y VERIFICACIÓN INICIAL
# =============================================================================

# Verificar datos EPH
if(!exists("datos_gba")) {
  if(file.exists(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))) {
    load(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))
    cat("✅ Datos GBA cargados desde archivo\n")
  } else {
    stop("❌ Ejecuta primero: source('scripts/02_carga_datos.R')")
  }
}

# Verificar IPC para deflactación
if(!exists("ipc_con_factores")) {
  if(file.exists(file.path(rutas$datos_procesados, "ipc_factores.RData"))) {
    load(file.path(rutas$datos_procesados, "ipc_factores.RData"))
    cat("✅ Factores IPC cargados desde archivo\n")
  } else {
    cat("⚠️ IPC no disponible, usando datos nominales\n")
    ipc_con_factores <- data.frame(
      anio = 2016:2024,
      trimestre = rep(1:4, length.out = 36),
      factor_deflactor = 1
    )
  }
}

# =============================================================================
# PREPARACIÓN DEL DATASET PARA IMPUTACIÓN
# =============================================================================

cat("\n📊 Preparando dataset para imputación...\n")

# Combinar todos los períodos en un solo dataset
datos_completos <- map_dfr(names(datos_gba), function(periodo) {
  
  partes <- strsplit(periodo, "_T")[[1]]
  ano <- as.numeric(partes[1])
  trimestre <- as.numeric(partes[2])
  
  # Buscar factor deflactor
  factor_periodo <- ipc_con_factores$factor_deflactor[
    ipc_con_factores$anio == ano & ipc_con_factores$trimestre == trimestre
  ]
  
  if(length(factor_periodo) == 0) factor_periodo <- 1
  
  datos_periodo <- datos_gba[[periodo]]$personas
  
  if(!is.null(datos_periodo)) {
    datos_periodo %>%
      mutate(
        ano = ano,
        trimestre = trimestre,
        periodo = periodo,
        factor_deflactor = factor_periodo
      )
  }
})

cat("✅ Dataset combinado:", nrow(datos_completos), "registros\n")

# =============================================================================
# FUNCIÓN PARA CONVERSIÓN SEGURA DE VARIABLES
# =============================================================================

convertir_seguro <- function(x) {
  if(is.numeric(x)) {
    return(x)
  } else if(is.character(x) || is.factor(x)) {
    x_limpio <- gsub("[^0-9.-]", "", as.character(x))
    x_num <- as.numeric(x_limpio)
    return(ifelse(is.na(x_num), -999, x_num))
  } else {
    return(as.numeric(x))
  }
}

# =============================================================================
# PROCESAMIENTO Y LIMPIEZA DE VARIABLES
# =============================================================================

cat("\n🔧 Procesando variables para modelado...\n")

# Crear dataset de trabajo con variables de interés
datos_modelado <- datos_completos %>%
  filter(
    ESTADO == 1,           # Solo ocupados
    CH06 >= 14,            # Edad mínima laboral
    CH06 <= 65,            # Edad máxima laboral  
    !is.na(AGLOMERADO)     # Con aglomerado válido
  ) %>%
  mutate(
    # Variable dependiente: P21 (convertir de forma segura)
    P21_original = convertir_seguro(P21),
    
    # Variables predictoras principales
    edad = as.numeric(CH06),
    sexo = as.numeric(CH04),
    nivel_educativo = as.numeric(CH12),
    calificacion = convertir_seguro(CAT_OCUP),
    categoria_ocupacional = convertir_seguro(CAT_INAC),
    aglomerado = as.numeric(AGLOMERADO),
    
    # Variables laborales adicionales
    horas_trabajadas = convertir_seguro(PP3E_TOT),
    tiene_segundo_trabajo = convertir_seguro(PP04A),
    
    # Variables socioeconómicas
    jefe_hogar = convertir_seguro(CH03),
    estado_civil = convertir_seguro(CH07),
    
    # Variables auxiliares
    p47t_original = convertir_seguro(P47T),
    
    # Crear dummies para variables categóricas
    sexo_mujer = ifelse(sexo == 2, 1, 0),
    es_jefe_hogar = ifelse(jefe_hogar == 1, 1, 0),
    caba = ifelse(aglomerado == 32, 1, 0),
    
    # Grupos etarios
    grupo_edad = case_when(
      edad < 25 ~ "18-24",
      edad < 35 ~ "25-34",
      edad < 45 ~ "35-44",
      edad < 55 ~ "45-54",
      TRUE ~ "55-65"
    ),
    
    # Niveles educativos agrupados
    nivel_ed_agrupado = case_when(
      nivel_educativo %in% c(1, 2, 3) ~ "Primario",
      nivel_educativo %in% c(4, 5) ~ "Secundario",
      nivel_educativo %in% c(6, 7, 8, 9) ~ "Superior",
      TRUE ~ "Sin_Dato"
    )
  ) %>%
  filter(
    P21_original > 0,      # Solo ingresos positivos
    P21_original != -9,    # Excluir no respuesta
    P21_original < 10000000, # Excluir outliers extremos
    !is.na(edad),
    !is.na(sexo)
  )

cat("✅ Variables procesadas:", nrow(datos_modelado), "registros válidos\n")

# Deflactar ingresos P21
datos_modelado <- datos_modelado %>%
  mutate(
    P21_real = P21_original / factor_deflactor,
    log_P21 = log(P21_real + 1)  # Log para normalizar
  )

# =============================================================================
# ANÁLISIS EXPLORATORIO PARA IMPUTACIÓN
# =============================================================================

cat("\n📈 Análisis exploratorio para modelado...\n")

# Distribución de la variable dependiente
summary_p21 <- datos_modelado %>%
  summarise(
    n_casos = n(),
    media = round(mean(P21_real, na.rm = TRUE)),
    mediana = round(median(P21_real, na.rm = TRUE)),
    p25 = round(quantile(P21_real, 0.25, na.rm = TRUE)),
    p75 = round(quantile(P21_real, 0.75, na.rm = TRUE)),
    cv = round(sd(P21_real, na.rm = TRUE) / mean(P21_real, na.rm = TRUE) * 100, 1)
  )

cat("📊 Estadísticas P21 real:\n")
print(summary_p21)

# =============================================================================
# SIMULACIÓN DE NO RESPUESTA
# =============================================================================

cat("\n🎲 Simulando patrones de no respuesta...\n")

# Para simular no respuesta realista, crear patrones basados en características observables
set.seed(123)

# Probabilidad de no respuesta basada en características
datos_modelado <- datos_modelado %>%
  mutate(
    # Probabilidad mayor de no respuesta en ciertos grupos
    prob_no_respuesta = case_when(
      # Jóvenes tienen mayor prob de no responder
      edad < 25 ~ 0.25,
      # Mujeres ligeramente mayor prob
      sexo_mujer == 1 ~ 0.20,
      # Menor educación, mayor prob
      nivel_ed_agrupado == "Primario" ~ 0.22,
      # Trabajadores informales (proxy)
      horas_trabajadas < 20 ~ 0.30,
      # Caso base
      TRUE ~ 0.15
    ),
    
    # Simular no respuesta
    es_no_respuesta = rbinom(n(), 1, prob_no_respuesta),
    
    # Variable para imputación (NA donde hay no respuesta)
    P21_para_imputar = ifelse(es_no_respuesta == 1, NA, P21_real),
    log_P21_para_imputar = ifelse(es_no_respuesta == 1, NA, log_P21)
  )

# Verificar patrones de no respuesta
tabla_no_respuesta <- datos_modelado %>%
  group_by(nivel_ed_agrupado, sexo_mujer) %>%
  summarise(
    n_total = n(),
    n_no_respuesta = sum(es_no_respuesta),
    tasa_no_respuesta = round(n_no_respuesta / n_total * 100, 1),
    .groups = "drop"
  )

cat("📋 Patrones de no respuesta simulados:\n")
print(tabla_no_respuesta)

# =============================================================================
# PREPARACIÓN DE DATOS PARA MODELADO
# =============================================================================

cat("\n🔨 Preparando datos para modelado...\n")

# Seleccionar variables predictoras
variables_predictoras <- c(
  "edad", "sexo_mujer", "es_jefe_hogar", "caba",
  "nivel_educativo", "horas_trabajadas", "tiene_segundo_trabajo"
)

# Dataset solo con casos completos (para entrenamiento)
datos_entrenamiento <- datos_modelado %>%
  filter(!is.na(P21_para_imputar)) %>%
  select(all_of(c(variables_predictoras, "P21_real", "log_P21"))) %>%
  na.omit()

# Dataset con casos faltantes (para imputación)
datos_faltantes <- datos_modelado %>%
  filter(is.na(P21_para_imputar)) %>%
  select(all_of(variables_predictoras)) %>%
  na.omit()

cat("✅ Datos de entrenamiento:", nrow(datos_entrenamiento), "casos\n")
cat("✅ Datos para imputar:", nrow(datos_faltantes), "casos\n")

# División entrenamiento/test
set.seed(456)
indices_test <- sample(nrow(datos_entrenamiento), round(nrow(datos_entrenamiento) * 0.3))
datos_train <- datos_entrenamiento[-indices_test, ]
datos_test <- datos_entrenamiento[indices_test, ]

cat("✅ División: Entrenamiento =", nrow(datos_train), "| Test =", nrow(datos_test), "\n")

# =============================================================================
# MODELO 1: REGRESIÓN LINEAL SIMPLE
# =============================================================================

cat("\n📈 Modelo 1: Regresión Lineal Simple...\n")

# Análisis de correlaciones para seleccionar mejor predictor
correlaciones <- datos_train %>%
  select(all_of(variables_predictoras), P21_real) %>%
  cor(use = "complete.obs")

correlacion_p21 <- correlaciones["P21_real", variables_predictoras]
mejor_predictor <- names(which.max(abs(correlacion_p21[variables_predictoras])))

cat("🔍 Correlaciones con P21:\n")
print(round(correlacion_p21, 3))
cat("🥇 Mejor predictor simple:", mejor_predictor, 
    "| Correlación:", round(correlacion_p21[mejor_predictor], 3), "\n")

# Ajustar modelo simple
formula_simple <- as.formula(paste("P21_real ~", mejor_predictor))
modelo_simple <- lm(formula_simple, data = datos_train)

# Resumen del modelo
cat("\n📊 Resumen Modelo Simple:\n")
print(summary(modelo_simple))

# Predicciones y métricas
pred_simple_test <- predict(modelo_simple, datos_test)
residuos_simple <- datos_test$P21_real - pred_simple_test

# Métricas modelo simple
r2_simple <- summary(modelo_simple)$r.squared
r2_adj_simple <- summary(modelo_simple)$adj.r.squared
mae_simple <- mean(abs(residuos_simple), na.rm = TRUE)
rmse_simple <- sqrt(mean(residuos_simple^2, na.rm = TRUE))

cat("📊 MÉTRICAS MODELO SIMPLE:\n")
cat("   R² =", round(r2_simple, 4), "\n")
cat("   R² Ajustado =", round(r2_adj_simple, 4), "\n")
cat("   MAE =", round(mae_simple), "\n")
cat("   RMSE =", round(rmse_simple), "\n")

# =============================================================================
# MODELO 2: REGRESIÓN LINEAL MÚLTIPLE
# =============================================================================

cat("\n📈 Modelo 2: Regresión Lineal Múltiple...\n")

# Análisis de correlación entre predictores (multicolinealidad)
cat("🔍 Matriz de correlaciones entre predictores:\n")
cor_predictores <- datos_train %>%
  select(all_of(variables_predictoras)) %>%
  cor(use = "complete.obs")

print(round(cor_predictores, 3))

# Modelo completo
formula_multiple <- as.formula(paste("P21_real ~", paste(variables_predictoras, collapse = " + ")))
modelo_multiple <- lm(formula_multiple, data = datos_train)

cat("\n📊 Resumen Modelo Múltiple:\n")
print(summary(modelo_multiple))

# Verificar multicolinealidad con VIF
cat("\n🔍 Factor de Inflación de Varianza (VIF):\n")
vif_values <- car::vif(modelo_multiple)
print(round(vif_values, 2))

# Detectar problemas de multicolinealidad
vif_problemas <- vif_values[vif_values > 5]
if(length(vif_problemas) > 0) {
  cat("⚠️ Variables con VIF > 5 (posible multicolinealidad):\n")
  print(vif_problemas)
}

# Predicciones y métricas
pred_multiple_test <- predict(modelo_multiple, datos_test)
residuos_multiple <- datos_test$P21_real - pred_multiple_test

# Métricas modelo múltiple
r2_multiple <- summary(modelo_multiple)$r.squared
r2_adj_multiple <- summary(modelo_multiple)$adj.r.squared
mae_multiple <- mean(abs(residuos_multiple), na.rm = TRUE)
rmse_multiple <- sqrt(mean(residuos_multiple^2, na.rm = TRUE))

cat("\n📊 MÉTRICAS MODELO MÚLTIPLE:\n")
cat("   R² =", round(r2_multiple, 4), "\n")
cat("   R² Ajustado =", round(r2_adj_multiple, 4), "\n")
cat("   MAE =", round(mae_multiple), "\n")
cat("   RMSE =", round(rmse_multiple), "\n")

# =============================================================================
# VERIFICACIÓN DE SUPUESTOS - REGRESIÓN MÚLTIPLE
# =============================================================================

cat("\n🔍 Verificación de supuestos del modelo múltiple...\n")

# 1. Normalidad de residuos
shapiro_test <- shapiro.test(residuos_multiple[1:min(5000, length(residuos_multiple))])
cat("📊 Test de normalidad (Shapiro-Wilk):\n")
cat("   p-value =", round(shapiro_test$p.value, 6), "\n")
cat("   Normalidad:", ifelse(shapiro_test$p.value > 0.05, "✅ Se acepta", "❌ Se rechaza"), "\n")

# 2. Homocedasticidad (Breusch-Pagan)
bp_test <- lmtest::bptest(modelo_multiple)
cat("\n📊 Test de homocedasticidad (Breusch-Pagan):\n")
cat("   p-value =", round(bp_test$p.value, 6), "\n")
cat("   Homocedasticidad:", ifelse(bp_test$p.value > 0.05, "✅ Se acepta", "❌ Se rechaza"), "\n")

# 3. Independencia (Durbin-Watson)
dw_test <- lmtest::dwtest(modelo_multiple)
cat("\n📊 Test de independencia (Durbin-Watson):\n")
cat("   Estadístico DW =", round(dw_test$statistic, 4), "\n")
cat("   p-value =", round(dw_test$p.value, 6), "\n")
cat("   Independencia:", ifelse(dw_test$p.value > 0.05, "✅ Se acepta", "❌ Se rechaza"), "\n")

# =============================================================================
# MODELO 3: ÁRBOLES DE DECISIÓN
# =============================================================================

cat("\n🌳 Modelo 3: Árboles de Decisión...\n")

# Ajustar modelo de árbol (Random Forest para mejor performance)
modelo_arbol <- randomForest(
  x = datos_train[variables_predictoras],
  y = datos_train$P21_real,
  ntree = 500,
  mtry = floor(sqrt(length(variables_predictoras))),
  importance = TRUE,
  na.action = na.omit
)

cat("📊 Resumen Modelo Árbol:\n")
print(modelo_arbol)

# Importancia de variables
importancia <- importance(modelo_arbol)
cat("\n🔍 Importancia de variables:\n")
print(round(importancia, 2))

# Predicciones y métricas
pred_arbol_test <- predict(modelo_arbol, datos_test)
residuos_arbol <- datos_test$P21_real - pred_arbol_test

# Métricas modelo árbol
# Para árboles, calculamos R² como 1 - SSE/SST
sse_arbol <- sum(residuos_arbol^2, na.rm = TRUE)
sst_arbol <- sum((datos_test$P21_real - mean(datos_test$P21_real, na.rm = TRUE))^2, na.rm = TRUE)
r2_arbol <- 1 - sse_arbol / sst_arbol
mae_arbol <- mean(abs(residuos_arbol), na.rm = TRUE)
rmse_arbol <- sqrt(mean(residuos_arbol^2, na.rm = TRUE))

cat("\n📊 MÉTRICAS MODELO ÁRBOL:\n")
cat("   R² =", round(r2_arbol, 4), "\n")
cat("   MAE =", round(mae_arbol), "\n")
cat("   RMSE =", round(rmse_arbol), "\n")

# =============================================================================
# COMPARACIÓN DE MODELOS
# =============================================================================

cat("\n🏆 Comparación de modelos...\n")

# Crear tabla comparativa
comparacion_modelos <- data.frame(
  Modelo = c("Lineal Simple", "Lineal Múltiple", "Árbol de Decisión"),
  R2 = c(r2_simple, r2_multiple, r2_arbol),
  R2_Ajustado = c(r2_adj_simple, r2_adj_multiple, NA),
  MAE = c(mae_simple, mae_multiple, mae_arbol),
  RMSE = c(rmse_simple, rmse_multiple, rmse_arbol)
) %>%
  mutate(
    R2 = round(R2, 4),
    R2_Ajustado = round(R2_Ajustado, 4),
    MAE = round(MAE, 0),
    RMSE = round(RMSE, 0)
  )

cat("📊 TABLA COMPARATIVA DE MODELOS:\n")
print(comparacion_modelos)

# Identificar mejor modelo
mejor_modelo_r2 <- comparacion_modelos[which.max(comparacion_modelos$R2), "Modelo"]
mejor_modelo_mae <- comparacion_modelos[which.min(comparacion_modelos$MAE), "Modelo"]
mejor_modelo_rmse <- comparacion_modelos[which.min(comparacion_modelos$RMSE), "Modelo"]

cat("\n🥇 MEJORES MODELOS POR MÉTRICA:\n")
cat("   Mejor R²:", mejor_modelo_r2, "\n")
cat("   Mejor MAE:", mejor_modelo_mae, "\n")
cat("   Mejor RMSE:", mejor_modelo_rmse, "\n")

# =============================================================================
# GRÁFICOS DE EVALUACIÓN
# =============================================================================

cat("\n📈 Creando gráficos de evaluación...\n")

# Datos para gráficos
datos_graficos <- data.frame(
  real = rep(datos_test$P21_real, 3),
  predicho = c(pred_simple_test, pred_multiple_test, pred_arbol_test),
  modelo = rep(c("Lineal Simple", "Lineal Múltiple", "Árbol de Decisión"), 
               each = length(pred_simple_test)),
  residuo = c(residuos_simple, residuos_multiple, residuos_arbol)
)

# 1. Gráfico de valores reales vs predichos
grafico_predicciones <- datos_graficos %>%
  ggplot(aes(x = real, y = predicho, color = modelo)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~modelo, scales = "free") +
  scale_color_manual(values = c("#e74c3c", "#3498db", "#27ae60")) +
  labs(
    title = "Valores Reales vs Predichos por Modelo",
    subtitle = "Línea diagonal = predicción perfecta",
    x = "P21 Real ($ 2023)",
    y = "P21 Predicho ($ 2023)",
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

# 2. Gráfico de residuos
grafico_residuos <- datos_graficos %>%
  ggplot(aes(x = predicho, y = residuo, color = modelo)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~modelo, scales = "free_x") +
  scale_color_manual(values = c("#e74c3c", "#3498db", "#27ae60")) +
  labs(
    title = "Gráfico de Residuos por Modelo",
    subtitle = "Distribución aleatoria alrededor de 0 indica buen ajuste",
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

# 3. Distribución de residuos
grafico_dist_residuos <- datos_graficos %>%
  ggplot(aes(x = residuo, fill = modelo)) +
  geom_histogram(alpha = 0.7, bins = 30, color = "white") +
  facet_wrap(~modelo, scales = "free") +
  scale_fill_manual(values = c("#e74c3c", "#3498db", "#27ae60")) +
  labs(
    title = "Distribución de Residuos por Modelo",
    subtitle = "Distribución normal centrada en 0 es deseable",
    x = "Residuos ($ 2023)",
    y = "Frecuencia",
    fill = "Modelo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = "."))

# 4. Comparación de métricas
grafico_metricas <- comparacion_modelos %>%
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
    title = "Comparación de Métricas de Rendimiento",
    subtitle = "R² normalizado (mayor es mejor) | MAE y RMSE normalizados (menor es mejor)",
    x = "Modelo",
    y = "Valor Normalizado",
    fill = "Métrica"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Guardar gráficos
guardar_grafico(grafico_predicciones, "evaluacion_predicciones_modelos", ancho = 16, alto = 10)
guardar_grafico(grafico_residuos, "evaluacion_residuos_modelos", ancho = 16, alto = 10)
guardar_grafico(grafico_dist_residuos, "distribucion_residuos_modelos", ancho = 16, alto = 10)
guardar_grafico(grafico_metricas, "comparacion_metricas_modelos", ancho = 12, alto = 8)

# =============================================================================
# SELECCIÓN DEL MEJOR MODELO E IMPUTACIÓN
# =============================================================================

cat("\n🎯 Selección del mejor modelo...\n")

# Ranking de modelos (combinando métricas)
ranking_modelos <- comparacion_modelos %>%
  mutate(
    rank_r2 = rank(-R2),
    rank_mae = rank(MAE),
    rank_rmse = rank(RMSE),
    puntuacion_total = rank_r2 + rank_mae + rank_rmse
  ) %>%
  arrange(puntuacion_total)

cat("📊 Ranking final de modelos:\n")
print(ranking_modelos[c("Modelo", "R2", "MAE", "RMSE", "puntuacion_total")])

mejor_modelo_final <- ranking_modelos$Modelo[1]

cat("🏆 MEJOR MODELO SELECCIONADO:", mejor_modelo_final, "\n")

# Aplicar imputación con el mejor modelo
cat("\n💫 Aplicando imputación con el mejor modelo...\n")

if(mejor_modelo_final == "Lineal Simple") {
  valores_imputados <- predict(modelo_simple, datos_faltantes)
  modelo_final <- modelo_simple
} else if(mejor_modelo_final == "Lineal Múltiple") {
  valores_imputados <- predict(modelo_multiple, datos_faltantes)
  modelo_final <- modelo_multiple
} else {
  valores_imputados <- predict(modelo_arbol, datos_faltantes)
  modelo_final <- modelo_arbol
}

cat("✅ Imputación completada:", length(valores_imputados), "valores imputados\n")

# Estadísticas de valores imputados
stats_imputados <- data.frame(
  Estadistico = c("Media", "Mediana", "P25", "P75", "Min", "Max"),
  Valor_Original = c(
    round(mean(datos_entrenamiento$P21_real, na.rm = TRUE)),
    round(median(datos_entrenamiento$P21_real, na.rm = TRUE)),
    round(quantile(datos_entrenamiento$P21_real, 0.25, na.rm = TRUE)),
    round(quantile(datos_entrenamiento$P21_real, 0.75, na.rm = TRUE)),
    round(min(datos_entrenamiento$P21_real, na.rm = TRUE)),
    round(max(datos_entrenamiento$P21_real, na.rm = TRUE))
  ),
  Valor_Imputado = c(
    round(mean(valores_imputados, na.rm = TRUE)),
    round(median(valores_imputados, na.rm = TRUE)),
    round(quantile(valores_imputados, 0.25, na.rm = TRUE)),
    round(quantile(valores_imputados, 0.75, na.rm = TRUE)),
    round(min(valores_imputados, na.rm = TRUE)),
    round(max(valores_imputados, na.rm = TRUE))
  )
)

cat("\n📊 Comparación estadísticas originales vs imputadas:\n")
print(stats_imputados)

# =============================================================================
# ANÁLISIS DE LA INFLUENCIA DE VARIABLES
# =============================================================================

cat("\n🔍 Análisis de influencia de variables...\n")

if(mejor_modelo_final %in% c("Lineal Simple", "Lineal Múltiple")) {
  
  # Para modelos lineales: coeficientes e interpretación
  coeficientes <- summary(modelo_final)$coefficients
  
  cat("📊 COEFICIENTES DEL MODELO FINAL:\n")
  print(round(coeficientes, 4))
  
  cat("\n📝 INTERPRETACIÓN DE COEFICIENTES:\n")
  
  # Interpretar cada coeficiente significativo
  coef_significativos <- coeficientes[coeficientes[, "Pr(>|t|)"] < 0.05, ]
  
  if(nrow(coef_significativos) > 1) {  # Más que solo el intercepto
    for(i in 2:nrow(coef_significativos)) {
      var_name <- rownames(coef_significativos)[i]
      coef_val <- coef_significativos[i, "Estimate"]
      p_val <- coef_significativos[i, "Pr(>|t|)"]
      
      cat("   •", var_name, ":\n")
      cat("     - Coeficiente:", round(coef_val, 2), "\n")
      cat("     - Interpretación: Por cada unidad adicional de", var_name, 
          ", el ingreso P21 cambia en $", round(coef_val, 0), "\n")
      cat("     - Significancia: p =", round(p_val, 4), 
          ifelse(p_val < 0.001, " (***)", ifelse(p_val < 0.01, " (**)", " (*)")) , "\n\n")
    }
  }
  
} else {
  # Para árboles: importancia de variables
  cat("📊 IMPORTANCIA DE VARIABLES (ÁRBOL DE DECISIÓN):\n")
  
  importancia_df <- data.frame(
    Variable = rownames(importancia),
    Importancia = importancia[, "%IncMSE"],
    Importancia_Pureza = importancia[, "IncNodePurity"]
  ) %>%
    arrange(-Importancia)
  
  print(importancia_df)
  
  cat("\n📝 INTERPRETACIÓN DE IMPORTANCIA:\n")
  for(i in 1:min(5, nrow(importancia_df))) {
    cat("   •", importancia_df$Variable[i], 
        ": Importancia =", round(importancia_df$Importancia[i], 2), "%\n")
  }
}

# =============================================================================
# GRÁFICO DE COEFICIENTES / IMPORTANCIA
# =============================================================================

if(mejor_modelo_final %in% c("Lineal Simple", "Lineal Múltiple")) {
  
  # Gráfico de coeficientes para modelos lineales
  coef_df <- data.frame(
    Variable = rownames(coeficientes)[-1],  # Excluir intercepto
    Coeficiente = coeficientes[-1, "Estimate"],
    Error_Std = coeficientes[-1, "Std. Error"],
    P_Value = coeficientes[-1, "Pr(>|t|)"]
  ) %>%
    mutate(
      Significativo = ifelse(P_Value < 0.05, "Sí", "No"),
      IC_Lower = Coeficiente - 1.96 * Error_Std,
      IC_Upper = Coeficiente + 1.96 * Error_Std
    )
  
  grafico_coeficientes <- coef_df %>%
    ggplot(aes(x = reorder(Variable, Coeficiente), y = Coeficiente, 
               color = Significativo, fill = Significativo)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_pointrange(aes(ymin = IC_Lower, ymax = IC_Upper), 
                    size = 1, alpha = 0.8) +
    coord_flip() +
    scale_color_manual(values = c("No" = "gray50", "Sí" = "#e74c3c")) +
    scale_fill_manual(values = c("No" = "gray50", "Sí" = "#e74c3c")) +
    labs(
      title = paste("Coeficientes del Modelo:", mejor_modelo_final),
      subtitle = "Intervalos de confianza al 95% | Línea en 0 = sin efecto",
      x = "Variables",
      y = "Coeficiente ($ 2023)",
      color = "Significativo",
      fill = "Significativo"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray60"),
      legend.position = "top"
    )
  
} else {
  
  # Gráfico de importancia para árboles
  grafico_coeficientes <- importancia_df %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(Variable, Importancia), y = Importancia)) +
    geom_col(fill = "#27ae60", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Importancia de Variables - Árbol de Decisión",
      subtitle = "Porcentaje de incremento en MSE al remover la variable",
      x = "Variables",
      y = "Importancia (% Inc MSE)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray60")
    )
}

guardar_grafico(grafico_coeficientes, "influencia_variables_modelo_final", ancho = 12, alto = 8)

# =============================================================================
# VALIDACIÓN ADICIONAL DEL MODELO SELECCIONADO
# =============================================================================

cat("\n✅ Validación adicional del modelo seleccionado...\n")

# Validación cruzada simple (k=5)
set.seed(789)
k_folds <- 5
n_obs <- nrow(datos_entrenamiento)
fold_indices <- sample(rep(1:k_folds, length.out = n_obs))

cv_results <- data.frame(
  fold = 1:k_folds,
  r2 = numeric(k_folds),
  mae = numeric(k_folds),
  rmse = numeric(k_folds)
)

for(fold in 1:k_folds) {
  # Dividir datos
  train_cv <- datos_entrenamiento[fold_indices != fold, ]
  test_cv <- datos_entrenamiento[fold_indices == fold, ]
  
  # Ajustar modelo según el tipo
  if(mejor_modelo_final == "Lineal Simple") {
    modelo_cv <- lm(formula_simple, data = train_cv)
    pred_cv <- predict(modelo_cv, test_cv)
  } else if(mejor_modelo_final == "Lineal Múltiple") {
    modelo_cv <- lm(formula_multiple, data = train_cv)
    pred_cv <- predict(modelo_cv, test_cv)
  } else {
    modelo_cv <- randomForest(
      x = train_cv[variables_predictoras],
      y = train_cv$P21_real,
      ntree = 500,
      mtry = floor(sqrt(length(variables_predictoras)))
    )
    pred_cv <- predict(modelo_cv, test_cv)
  }
  
  # Calcular métricas
  residuos_cv <- test_cv$P21_real - pred_cv
  sse_cv <- sum(residuos_cv^2, na.rm = TRUE)
  sst_cv <- sum((test_cv$P21_real - mean(test_cv$P21_real, na.rm = TRUE))^2, na.rm = TRUE)
  
  cv_results$r2[fold] <- 1 - sse_cv / sst_cv
  cv_results$mae[fold] <- mean(abs(residuos_cv), na.rm = TRUE)
  cv_results$rmse[fold] <- sqrt(mean(residuos_cv^2, na.rm = TRUE))
}

# Estadísticas de validación cruzada
cv_summary <- cv_results %>%
  summarise(
    R2_mean = round(mean(r2), 4),
    R2_sd = round(sd(r2), 4),
    MAE_mean = round(mean(mae), 0),
    MAE_sd = round(sd(mae), 0),
    RMSE_mean = round(mean(rmse), 0),
    RMSE_sd = round(sd(rmse), 0)
  )

cat("📊 VALIDACIÓN CRUZADA (k=5):\n")
cat("   R² promedio:", cv_summary$R2_mean, "±", cv_summary$R2_sd, "\n")
cat("   MAE promedio:", cv_summary$MAE_mean, "±", cv_summary$MAE_sd, "\n")
cat("   RMSE promedio:", cv_summary$RMSE_mean, "±", cv_summary$RMSE_sd, "\n")

# =============================================================================
# APLICACIÓN A ANÁLISIS DE INGRESOS
# =============================================================================

cat("\n🔄 Aplicando imputación al análisis de ingresos completo...\n")

# Recrear el dataset completo con valores imputados
datos_con_imputacion <- datos_modelado %>%
  mutate(
    P21_final = case_when(
      es_no_respuesta == 1 ~ NA_real_,  # Será llenado con imputación
      TRUE ~ P21_real
    )
  )

# Aplicar imputación a todos los casos faltantes en el dataset original
indices_faltantes <- which(is.na(datos_con_imputacion$P21_final))

if(length(indices_faltantes) > 0) {
  # Preparar datos para imputación
  datos_para_imputar <- datos_con_imputacion[indices_faltantes, variables_predictoras]
  
  # Aplicar modelo
  if(mejor_modelo_final == "Lineal Simple") {
    valores_nuevos <- predict(modelo_simple, datos_para_imputar)
  } else if(mejor_modelo_final == "Lineal Múltiple") {
    valores_nuevos <- predict(modelo_multiple, datos_para_imputar)
  } else {
    valores_nuevos <- predict(modelo_arbol, datos_para_imputar)
  }
  
  # Asegurar valores positivos
  valores_nuevos <- pmax(valores_nuevos, 1000)  # Mínimo $1000
  
  # Asignar valores imputados
  datos_con_imputacion$P21_final[indices_faltantes] <- valores_nuevos
}

# Comparar estadísticas antes y después de imputación
comparacion_final <- data.frame(
  Estadistico = c("N casos", "Media", "Mediana", "P25", "P75", "CV (%)"),
  Sin_Imputacion = c(
    sum(!is.na(datos_con_imputacion$P21_real)),
    round(mean(datos_con_imputacion$P21_real, na.rm = TRUE)),
    round(median(datos_con_imputacion$P21_real, na.rm = TRUE)),
    round(quantile(datos_con_imputacion$P21_real, 0.25, na.rm = TRUE)),
    round(quantile(datos_con_imputacion$P21_real, 0.75, na.rm = TRUE)),
    round(sd(datos_con_imputacion$P21_real, na.rm = TRUE) / 
            mean(datos_con_imputacion$P21_real, na.rm = TRUE) * 100, 1)
  ),
  Con_Imputacion = c(
    sum(!is.na(datos_con_imputacion$P21_final)),
    round(mean(datos_con_imputacion$P21_final, na.rm = TRUE)),
    round(median(datos_con_imputacion$P21_final, na.rm = TRUE)),
    round(quantile(datos_con_imputacion$P21_final, 0.25, na.rm = TRUE)),
    round(quantile(datos_con_imputacion$P21_final, 0.75, na.rm = TRUE)),
    round(sd(datos_con_imputacion$P21_final, na.rm = TRUE) / 
            mean(datos_con_imputacion$P21_final, na.rm = TRUE) * 100, 1)
  )
)

cat("\n📊 COMPARACIÓN ANTES Y DESPUÉS DE IMPUTACIÓN:\n")
print(comparacion_final)

# =============================================================================
# GUARDAR RESULTADOS
# =============================================================================

cat("\n💾 Guardando resultados del análisis de imputación...\n")

# Guardar tabla comparativa de modelos
write_csv(comparacion_modelos, file.path(rutas$tablas, "comparacion_modelos_imputacion.csv"))

# Guardar estadísticas de validación cruzada
write_csv(cv_results, file.path(rutas$tablas, "validacion_cruzada_modelos.csv"))

# Guardar comparación final
write_csv(comparacion_final, file.path(rutas$tablas, "comparacion_antes_despues_imputacion.csv"))

# Guardar coeficientes/importancia según modelo
if(mejor_modelo_final %in% c("Lineal Simple", "Lineal Múltiple")) {
  write_csv(coef_df, file.path(rutas$tablas, "coeficientes_modelo_final.csv"))
} else {
  write_csv(importancia_df, file.path(rutas$tablas, "importancia_variables_modelo_final.csv"))
}

# Guardar modelo y datos para uso posterior
save(
  modelo_final, 
  mejor_modelo_final,
  variables_predictoras,
  datos_con_imputacion,
  comparacion_modelos,
  cv_summary,
  file = file.path(rutas$datos_procesados, "modelo_imputacion_p21.RData")
)

cat("✅ Resultados guardados correctamente\n")

# =============================================================================
# REPORTE FINAL DE IMPUTACIÓN
# =============================================================================

cat("\n", rep("=", 80), "\n")
cat("✅ ANÁLISIS DE IMPUTACIÓN P21 COMPLETADO\n")
cat(rep("=", 80), "\n")

cat("🎯 MODELO SELECCIONADO:", mejor_modelo_final, "\n")

cat("\n📊 RENDIMIENTO DEL MEJOR MODELO:\n")
best_metrics <- comparacion_modelos[comparacion_modelos$Modelo == mejor_modelo_final, ]
cat("   • R²:", best_metrics$R2, "\n")
if(!is.na(best_metrics$R2_Ajustado)) {
  cat("   • R² Ajustado:", best_metrics$R2_Ajustado, "\n")
}
cat("   • MAE:", format(best_metrics$MAE, big.mark = ","), "$ (2023)\n")
cat("   • RMSE:", format(best_metrics$RMSE, big.mark = ","), "$ (2023)\n")

cat("\n✅ VALIDACIÓN CRUZADA:\n")
cat("   • R² promedio:", cv_summary$R2_mean, "±", cv_summary$R2_sd, "\n")
cat("   • Estabilidad: ", 
    ifelse(cv_summary$R2_sd < 0.05, "✅ Alta", 
           ifelse(cv_summary$R2_sd < 0.1, "⚠️ Media", "❌ Baja")), "\n")

cat("\n🔍 CUMPLIMIENTO DE SUPUESTOS (si aplica):\n")
if(mejor_modelo_final %in% c("Lineal Simple", "Lineal Múltiple")) {
  cat("   • Normalidad de residuos:", 
      ifelse(shapiro_test$p.value > 0.05, "✅ Se cumple", "❌ Se viola"), "\n")
  cat("   • Homocedasticidad:", 
      ifelse(bp_test$p.value > 0.05, "✅ Se cumple", "❌ Se viola"), "\n")
  cat("   • Independencia de errores:", 
      ifelse(dw_test$p.value > 0.05, "✅ Se cumple", "❌ Se viola"), "\n")
  if(exists("vif_problemas") && length(vif_problemas) > 0) {
    cat("   • Multicolinealidad: ⚠️ Detectada en", length(vif_problemas), "variables\n")
  } else {
    cat("   • Multicolinealidad: ✅ No detectada\n")
  }
} else {
  cat("   • No paramétrico: ✅ Sin supuestos requeridos\n")
}

cat("\n💫 IMPACTO DE LA IMPUTACIÓN:\n")
incremento_casos <- as.numeric(comparacion_final[1, "Con_Imputacion"]) - 
  as.numeric(comparacion_final[1, "Sin_Imputacion"])
cat("   • Casos adicionales:", incremento_casos, "\n")
cat("   • Incremento en muestra:", 
    round(incremento_casos / as.numeric(comparacion_final[1, "Sin_Imputacion"]) * 100, 1), "%\n")

diferencia_media <- as.numeric(comparacion_final[2, "Con_Imputacion"]) - 
  as.numeric(comparacion_final[2, "Sin_Imputacion"])
cat("   • Cambio en media:", 
    ifelse(abs(diferencia_media) < 1000, "✅ Mínimo", 
           ifelse(abs(diferencia_media) < 5000, "⚠️ Moderado", "❌ Significativo")),
    " (", round(diferencia_media), "$)\n")

cat("\n📈 VARIABLES MÁS INFLUYENTES:\n")
if(mejor_modelo_final %in% c("Lineal Simple", "Lineal Múltiple")) {
  top_vars <- coef_df %>% 
    filter(Significativo == "Sí") %>% 
    arrange(-abs(Coeficiente)) %>% 
    slice_head(n = 3)
  for(i in 1:nrow(top_vars)) {
    cat("   •", top_vars$Variable[i], ": $", round(top_vars$Coeficiente[i]), 
        " por unidad adicional\n")
  }
} else {
  top_vars <- importancia_df %>% slice_head(n = 3)
  for(i in 1:nrow(top_vars)) {
    cat("   •", top_vars$Variable[i], ": ", round(top_vars$Importancia[i], 1), 
        "% de importancia\n")
  }
}

cat("\n📁 ARCHIVOS GENERADOS:\n")
cat("   • comparacion_modelos_imputacion.csv - Métricas comparativas\n")
cat("   • validacion_cruzada_modelos.csv - Resultados CV\n")
cat("   • comparacion_antes_despues_imputacion.csv - Impacto imputación\n")
if(mejor_modelo_final %in% c("Lineal Simple", "Lineal Múltiple")) {
  cat("   • coeficientes_modelo_final.csv - Interpretación coeficientes\n")
} else {
  cat("   • importancia_variables_modelo_final.csv - Importancia variables\n")
}
cat("   • modelo_imputacion_p21.RData - Modelo entrenado\n")

cat("\n📊 GRÁFICOS GENERADOS:\n")
cat("   • evaluacion_predicciones_modelos.png - Valores reales vs predichos\n")
cat("   • evaluacion_residuos_modelos.png - Análisis de residuos\n")
cat("   • distribucion_residuos_modelos.png - Distribución de errores\n")
cat("   • comparacion_metricas_modelos.png - Métricas comparativas\n")
cat("   • influencia_variables_modelo_final.png - Interpretación modelo\n")



cat(rep("=", 80), "\n")

# Mostrar gráficos finales
print(grafico_predicciones)
print(grafico_metricas)
print(grafico_coeficientes)
