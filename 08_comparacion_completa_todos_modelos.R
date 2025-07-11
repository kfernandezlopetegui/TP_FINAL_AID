# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 08_comparacion_completa_todos_modelos.R - Comparación exhaustiva de TODOS los modelos
# =============================================================================

cat("📊 Creando comparación completa de TODOS los modelos de imputación...\n")

# =============================================================================
# CONSOLIDAR RESULTADOS DE TODOS LOS MODELOS
# =============================================================================

# Resultados del primer análisis (script 05)
resultados_originales <- list(
  lineal_simple = list(r2 = 0.021, r2_adj = 0.021, mae = 86768, rmse = 171341),
  lineal_multiple = list(r2 = 0.0501, r2_adj = 0.050, mae = 84191, rmse = 168490),
  random_forest_v1 = list(r2 = 0.1103, mae = 80253, rmse = 163119)
)

# Resultados del modelo mejorado
resultados_mejorado <- list(
  random_forest_v2 = list(r2 = 0.4429, mae = 51213, rmse = 124635)
)

cat("✅ Resultados consolidados de 4 modelos\n")

# =============================================================================
# CREAR TABLA COMPARATIVA COMPLETA
# =============================================================================

cat("\n📋 Creando tabla comparativa de todos los modelos...\n")

comparacion_completa <- data.frame(
  Modelo = c(
    "Regresión Lineal Simple",
    "Regresión Lineal Múltiple", 
    "Random Forest Original",
    "Random Forest Mejorado"
  ),
  Tipo = c("Paramétrico", "Paramétrico", "No Paramétrico", "No Paramétrico"),
  Variables = c(1, 7, 7, 21),
  R2 = c(
    resultados_originales$lineal_simple$r2,
    resultados_originales$lineal_multiple$r2,
    resultados_originales$random_forest_v1$r2,
    resultados_mejorado$random_forest_v2$r2
  ),
  R2_Ajustado = c(
    resultados_originales$lineal_simple$r2_adj,
    resultados_originales$lineal_multiple$r2_adj,
    NA,  # No aplica para Random Forest
    NA   # No aplica para Random Forest
  ),
  MAE = c(
    resultados_originales$lineal_simple$mae,
    resultados_originales$lineal_multiple$mae,
    resultados_originales$random_forest_v1$mae,
    resultados_mejorado$random_forest_v2$mae
  ),
  RMSE = c(
    resultados_originales$lineal_simple$rmse,
    resultados_originales$lineal_multiple$rmse,
    resultados_originales$random_forest_v1$rmse,
    resultados_mejorado$random_forest_v2$rmse
  )
) %>%
  mutate(
    R2_Porcentaje = round(R2 * 100, 1),
    MAE_Miles = round(MAE / 1000, 1),
    RMSE_Miles = round(RMSE / 1000, 1),
    Ranking_R2 = rank(-R2),
    Ranking_MAE = rank(MAE),
    Ranking_RMSE = rank(RMSE),
    Puntaje_Total = Ranking_R2 + Ranking_MAE + Ranking_RMSE
  ) %>%
  arrange(Puntaje_Total)

print(comparacion_completa)

# =============================================================================
# ANÁLISIS DE SUPUESTOS POR MODELO
# =============================================================================

cat("\n🔍 Análisis de cumplimiento de supuestos...\n")

supuestos_modelos <- data.frame(
  Modelo = c(
    "Regresión Lineal Simple",
    "Regresión Lineal Múltiple", 
    "Random Forest Original",
    "Random Forest Mejorado"
  ),
  Normalidad = c("❌ Violado", "❌ Violado", "✅ No aplica", "✅ No aplica"),
  Homocedasticidad = c("❌ Violado", "❌ Violado", "✅ No aplica", "✅ No aplica"),
  Independencia = c("❌ Violado", "❌ Violado", "✅ No aplica", "✅ No aplica"),
  Multicolinealidad = c("✅ No aplica", "✅ No detectada", "✅ No aplica", "✅ No aplica"),
  Supuestos_Cumplidos = c("0/3", "1/3", "N/A", "N/A"),
  Notas = c(
    "Modelo muy simple",
    "Supuestos violados por naturaleza de datos",
    "Sin supuestos requeridos",
    "Sin supuestos requeridos, optimizado"
  )
)

print(supuestos_modelos)

# =============================================================================
# GRÁFICOS COMPARATIVOS COMPLETOS
# =============================================================================

cat("\n📈 Creando gráficos comparativos completos...\n")

# 1. Comparación de R² todos los modelos
grafico_r2_completo <- comparacion_completa %>%
  ggplot(aes(x = reorder(Modelo, R2), y = R2_Porcentaje, fill = Tipo)) +
  geom_col(alpha = 0.8, color = "white", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Paramétrico" = "#e74c3c", "No Paramétrico" = "#27ae60")) +
  labs(
    title = "Comparación R² - Todos los Modelos de Imputación",
    subtitle = "Porcentaje de varianza explicada en ingresos P21",
    x = "Modelos",
    y = "R² (%)",
    fill = "Tipo de Modelo",
    caption = "Fuente: EPH-INDEC | Mayor R² indica mejor capacidad predictiva"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = paste0(R2_Porcentaje, "%")), hjust = -0.1, size = 4, fontface = "bold")

# 2. Comparación de errores (MAE y RMSE)
datos_errores <- comparacion_completa %>%
  select(Modelo, MAE, RMSE) %>%
  pivot_longer(cols = c(MAE, RMSE), names_to = "Tipo_Error", values_to = "Error") %>%
  mutate(Error_Miles = Error / 1000)

grafico_errores_completo <- datos_errores %>%
  ggplot(aes(x = reorder(Modelo, Error), y = Error_Miles, fill = Tipo_Error)) +
  geom_col(position = "dodge", alpha = 0.8, color = "white") +
  scale_fill_manual(values = c("MAE" = "#f39c12", "RMSE" = "#e74c3c")) +
  labs(
    title = "Comparación de Errores - Todos los Modelos",
    subtitle = "Menor error indica mejor precisión en predicción",
    x = "Modelos",
    y = "Error (Miles de $ 2023)",
    fill = "Tipo de Error",
    caption = "MAE = Error Absoluto Medio | RMSE = Raíz Error Cuadrático Medio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = paste0(round(Error_Miles, 1), "K")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3)

# 3. Ranking combinado de modelos
grafico_ranking <- comparacion_completa %>%
  select(Modelo, Ranking_R2, Ranking_MAE, Ranking_RMSE) %>%
  pivot_longer(cols = starts_with("Ranking"), names_to = "Metrica", values_to = "Ranking") %>%
  mutate(
    Metrica = case_when(
      Metrica == "Ranking_R2" ~ "R²",
      Metrica == "Ranking_MAE" ~ "MAE",
      Metrica == "Ranking_RMSE" ~ "RMSE"
    )
  ) %>%
  ggplot(aes(x = Metrica, y = Ranking, color = Modelo, group = Modelo)) +
  geom_line(size = 1.5, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  scale_y_reverse(breaks = 1:4, labels = c("1° (Mejor)", "2°", "3°", "4° (Peor)")) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Ranking por Métrica - Todos los Modelos",
    subtitle = "Posición relativa en cada métrica de evaluación",
    x = "Métricas de Evaluación",
    y = "Ranking (1 = Mejor)",
    color = "Modelo",
    caption = "Líneas hacia abajo indican mejor rendimiento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# 4. Evolución de complejidad vs rendimiento
grafico_complejidad <- comparacion_completa %>%
  ggplot(aes(x = Variables, y = R2_Porcentaje, color = Tipo, size = MAE)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(3, 8), trans = "reverse") +
  scale_color_manual(values = c("Paramétrico" = "#e74c3c", "No Paramétrico" = "#27ae60")) +
  labs(
    title = "Complejidad vs Rendimiento - Trade-off de Modelos",
    subtitle = "Tamaño del punto = MAE (menor es mejor)",
    x = "Número de Variables",
    y = "R² (%)",
    color = "Tipo de Modelo",
    size = "MAE",
    caption = "Esquina superior derecha = modelos óptimos (alta precisión, baja complejidad relativa)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = case_when(
    Variables == 1 ~ "Simple",
    Variables == 7 & Tipo == "Paramétrico" ~ "Múltiple",
    Variables == 7 & Tipo == "No Paramétrico" ~ "RF v1",
    Variables == 21 ~ "RF v2"
  )), vjust = -1, size = 3)

# Guardar gráficos
guardar_grafico(grafico_r2_completo, "comparacion_r2_todos_modelos", ancho = 12, alto = 8)
guardar_grafico(grafico_errores_completo, "comparacion_errores_todos_modelos", ancho = 14, alto = 8)
guardar_grafico(grafico_ranking, "ranking_todos_modelos", ancho = 12, alto = 8)
guardar_grafico(grafico_complejidad, "complejidad_vs_rendimiento", ancho = 12, alto = 8)

# =============================================================================
# ANÁLISIS DE MEJORAS INCREMENTALES
# =============================================================================

cat("\n📈 Análisis de mejoras incrementales...\n")

mejoras_incrementales <- data.frame(
  Paso = c("Baseline", "Multiple vs Simple", "RF v1 vs Multiple", "RF v2 vs RF v1"),
  Modelo_Base = c("-", "Lineal Simple", "Lineal Múltiple", "RF Original"),
  Modelo_Nuevo = c("Lineal Simple", "Lineal Múltiple", "RF Original", "RF Mejorado"),
  R2_Base = c(0, 0.021, 0.0501, 0.1103),
  R2_Nuevo = c(0.021, 0.0501, 0.1103, 0.4429),
  Mejora_R2_Absoluta = c(0.021, 0.0291, 0.0602, 0.3326),
  Mejora_R2_Relativa = c(Inf, 138.4, 120.2, 301.5)
) %>%
  mutate(
    Mejora_R2_Relativa = ifelse(Mejora_R2_Relativa == Inf, NA, Mejora_R2_Relativa),
    Impacto = case_when(
      Mejora_R2_Relativa < 50 ~ "Bajo",
      Mejora_R2_Relativa < 150 ~ "Moderado", 
      Mejora_R2_Relativa < 250 ~ "Alto",
      TRUE ~ "Muy Alto"
    )
  )

print(mejoras_incrementales)

# =============================================================================
# INTERPRETACIÓN METODOLÓGICA
# =============================================================================

cat("\n📝 Generando interpretación metodológica...\n")

interpretacion_modelos <- data.frame(
  Modelo = c(
    "Regresión Lineal Simple",
    "Regresión Lineal Múltiple", 
    "Random Forest Original",
    "Random Forest Mejorado"
  ),
  Fortalezas = c(
    "Simplicidad, interpretabilidad directa",
    "Múltiples predictores, interpretable",
    "Sin supuestos, captura no linealidades", 
    "Máxima precisión, variables optimizadas"
  ),
  Debilidades = c(
    "Muy limitado, subestima complejidad",
    "Supuestos violados, precisión limitada",
    "Menor precisión que versión optimizada",
    "Mayor complejidad, menos interpretable"
  ),
  Uso_Recomendado = c(
    "Análisis exploratorio inicial",
    "Baseline paramétrico, comparación",
    "Alternativa robusta a modelos lineales",
    "Imputación operativa de alta precisión"
  ),
  Limitaciones = c(
    "Inadecuado para uso práctico",
    "No cumple supuestos estadísticos",
    "Precisión sub-óptima",
    "Requiere más variables y computación"
  )
)

# =============================================================================
# GUARDAR RESULTADOS COMPLETOS
# =============================================================================

cat("\n💾 Guardando análisis completo...\n")

# Guardar tabla comparativa principal
write_csv(comparacion_completa, file.path(rutas$tablas, "comparacion_completa_todos_modelos.csv"))

# Guardar análisis de supuestos
write_csv(supuestos_modelos, file.path(rutas$tablas, "supuestos_por_modelo.csv"))

# Guardar mejoras incrementales
write_csv(mejoras_incrementales, file.path(rutas$tablas, "mejoras_incrementales_modelos.csv"))

# Guardar interpretación metodológica
write_csv(interpretacion_modelos, file.path(rutas$tablas, "interpretacion_metodologica_modelos.csv"))

# Crear resumen ejecutivo completo
resumen_final_completo <- data.frame(
  Aspecto = c(
    "Mejor Modelo", "R² Máximo", "Menor MAE", "Menor RMSE",
    "Modelo Más Simple", "Modelo Más Interpretable", "Modelo Más Robusto",
    "Mejora R² Simple→Múltiple", "Mejora R² Múltiple→RF v1", "Mejora R² RF v1→RF v2",
    "Supuestos Cumplidos (Lineales)", "Recomendación Final"
  ),
  Resultado = c(
    "Random Forest Mejorado",
    "44.3% (RF Mejorado)",
    "$51,213 (RF Mejorado)",
    "$124,635 (RF Mejorado)",
    "Regresión Lineal Simple",
    "Regresión Lineal Múltiple",
    "Random Forest (ambas versiones)",
    "+138.4%",
    "+120.2%", 
    "+301.5%",
    "Ninguno (datos de ingresos violan supuestos)",
    "Random Forest Mejorado para imputación operativa"
  )
)

write_csv(resumen_final_completo, file.path(rutas$tablas, "resumen_final_completo.csv"))

cat("✅ Análisis completo guardado\n")

# =============================================================================
# REPORTE FINAL COMPLETO
# =============================================================================

cat("\n", rep("=", 80), "\n")
cat("🎯 COMPARACIÓN COMPLETA DE MODELOS DE IMPUTACIÓN\n")
cat(rep("=", 80), "\n")

cat("📊 RANKING FINAL DE MODELOS:\n")
for(i in 1:nrow(comparacion_completa)) {
  modelo <- comparacion_completa[i, ]
  cat(sprintf("   %d. %s\n", i, modelo$Modelo))
  cat(sprintf("      R²: %s%% | MAE: $%s | RMSE: $%s\n", 
              modelo$R2_Porcentaje, 
              format(modelo$MAE, big.mark = ","),
              format(modelo$RMSE, big.mark = ",")))
}

cat("\n🏆 GANADOR ABSOLUTO: Random Forest Mejorado\n")
cat("   • Supera a TODOS los demás modelos en TODAS las métricas\n")
cat("   • R²: 44.3% vs 2.1% del modelo más simple (+2,011% mejora)\n")
cat("   • MAE: $51,213 vs $86,768 del modelo más simple (-41% error)\n")

cat("\n📈 PROGRESIÓN DE MEJORAS:\n")
cat("   Simple → Múltiple:  +138% mejora en R²\n")
cat("   Múltiple → RF v1:   +120% mejora en R²\n")
cat("   RF v1 → RF v2:      +302% mejora en R² ⭐\n")

cat("\n🔍 CUMPLIMIENTO DE SUPUESTOS:\n")
cat("   • Modelos Lineales: ❌ Violan supuestos (normalidad, homocedasticidad)\n")
cat("   • Random Forest: ✅ Sin supuestos requeridos (método no paramétrico)\n")

cat("\n💡 CONCLUSIONES METODOLÓGICAS:\n")
cat("   1. Modelos lineales INADECUADOS para datos de ingresos\n")
cat("   2. Random Forest SUPERIOR por naturaleza no paramétrica\n")
cat("   3. Variables temporales CLAVE para precisión (año explica muchísimo)\n")
cat("   4. Interacciones edad×educación FUNDAMENTALES\n")
cat("   5. Optimización de variables CRÍTICA (+302% mejora)\n")

cat("\n🎯 RECOMENDACIÓN FINAL:\n")
cat("   ✅ USAR: Random Forest Mejorado para imputación operativa\n")
cat("   ✅ DOCUMENTAR: Proceso completo desde simple hasta optimizado\n")
cat("   ✅ JUSTIFICAR: Superioridad basada en métricas objetivas\n")

cat("\n📁 ARCHIVOS PARA EL INFORME:\n")
cat("   • comparacion_completa_todos_modelos.csv - Tabla maestra\n")
cat("   • supuestos_por_modelo.csv - Validación estadística\n") 
cat("   • mejoras_incrementales_modelos.csv - Progresión de mejoras\n")
cat("   • interpretacion_metodologica_modelos.csv - Análisis cualitativo\n")

cat(rep("=", 80), "\n")

# Mostrar gráficos principales
print(grafico_r2_completo)
print(grafico_ranking)

cat("\n✨ ANÁLISIS EXHAUSTIVO DE 4 MODELOS COMPLETADO ✨\n")
cat("¡Listos todos los elementos para el trabajo final!\n")