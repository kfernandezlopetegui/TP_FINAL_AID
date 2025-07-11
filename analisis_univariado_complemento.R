# =============================================================================
# ANÁLISIS UNIVARIADO - ELEMENTOS COMPLEMENTARIOS PARA EL INFORME
# =============================================================================

library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

cat("📊 Generando elementos para análisis univariado...\n")

# Verificar datos
if(!exists("serie_tasas_gba")) {
  load(file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.RData"))
}

# =============================================================================
# 1. TABLA DE ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

cat("📋 Creando tabla de estadísticas descriptivas...\n")

estadisticas_univariadas <- serie_tasas_gba %>%
  summarise(
    # Tasa de Actividad
    Actividad_Media = round(mean(Tasa_Actividad, na.rm = TRUE), 1),
    Actividad_Mediana = round(median(Tasa_Actividad, na.rm = TRUE), 1),
    Actividad_Desvio = round(sd(Tasa_Actividad, na.rm = TRUE), 1),
    Actividad_Min = round(min(Tasa_Actividad, na.rm = TRUE), 1),
    Actividad_Max = round(max(Tasa_Actividad, na.rm = TRUE), 1),
    Actividad_CV = round((sd(Tasa_Actividad, na.rm = TRUE) / mean(Tasa_Actividad, na.rm = TRUE)) * 100, 1),
    
    # Tasa de Empleo
    Empleo_Media = round(mean(Tasa_Empleo, na.rm = TRUE), 1),
    Empleo_Mediana = round(median(Tasa_Empleo, na.rm = TRUE), 1),
    Empleo_Desvio = round(sd(Tasa_Empleo, na.rm = TRUE), 1),
    Empleo_Min = round(min(Tasa_Empleo, na.rm = TRUE), 1),
    Empleo_Max = round(max(Tasa_Empleo, na.rm = TRUE), 1),
    Empleo_CV = round((sd(Tasa_Empleo, na.rm = TRUE) / mean(Tasa_Empleo, na.rm = TRUE)) * 100, 1),
    
    # Tasa de Desocupación
    Desocupacion_Media = round(mean(Tasa_Desocupacion, na.rm = TRUE), 1),
    Desocupacion_Mediana = round(median(Tasa_Desocupacion, na.rm = TRUE), 1),
    Desocupacion_Desvio = round(sd(Tasa_Desocupacion, na.rm = TRUE), 1),
    Desocupacion_Min = round(min(Tasa_Desocupacion, na.rm = TRUE), 1),
    Desocupacion_Max = round(max(Tasa_Desocupacion, na.rm = TRUE), 1),
    Desocupacion_CV = round((sd(Tasa_Desocupacion, na.rm = TRUE) / mean(Tasa_Desocupacion, na.rm = TRUE)) * 100, 1)
  )

# Transformar a formato tabla para el informe
tabla_estadisticas <- data.frame(
  Estadístico = c("Media", "Mediana", "Desvío Estándar", "Mínimo", "Máximo", "Coef. Variación (%)"),
  `Tasa de Actividad` = c(
    estadisticas_univariadas$Actividad_Media,
    estadisticas_univariadas$Actividad_Mediana,
    estadisticas_univariadas$Actividad_Desvio,
    estadisticas_univariadas$Actividad_Min,
    estadisticas_univariadas$Actividad_Max,
    estadisticas_univariadas$Actividad_CV
  ),
  `Tasa de Empleo` = c(
    estadisticas_univariadas$Empleo_Media,
    estadisticas_univariadas$Empleo_Mediana,
    estadisticas_univariadas$Empleo_Desvio,
    estadisticas_univariadas$Empleo_Min,
    estadisticas_univariadas$Empleo_Max,
    estadisticas_univariadas$Empleo_CV
  ),
  `Tasa de Desocupación` = c(
    estadisticas_univariadas$Desocupacion_Media,
    estadisticas_univariadas$Desocupacion_Mediana,
    estadisticas_univariadas$Desocupacion_Desvio,
    estadisticas_univariadas$Desocupacion_Min,
    estadisticas_univariadas$Desocupacion_Max,
    estadisticas_univariadas$Desocupacion_CV
  ),
  stringsAsFactors = FALSE
)

# Guardar tabla
write.csv(tabla_estadisticas, file.path(rutas$tablas, "estadisticas_descriptivas_univariado.csv"), row.names = FALSE)

cat("✅ Tabla de estadísticas creada\n")
print(tabla_estadisticas)

# =============================================================================
# 2. ANÁLISIS DE PERÍODOS CRÍTICOS
# =============================================================================

cat("\n🔍 Identificando períodos críticos...\n")

# Identificar máximos y mínimos
periodo_max_desocupacion <- serie_tasas_gba[which.max(serie_tasas_gba$Tasa_Desocupacion), ]
periodo_min_desocupacion <- serie_tasas_gba[which.min(serie_tasas_gba$Tasa_Desocupacion), ]
periodo_max_empleo <- serie_tasas_gba[which.max(serie_tasas_gba$Tasa_Empleo), ]
periodo_min_empleo <- serie_tasas_gba[which.min(serie_tasas_gba$Tasa_Empleo), ]

eventos_criticos <- data.frame(
  Evento = c(
    "Máxima Desocupación", "Mínima Desocupación",
    "Máximo Empleo", "Mínimo Empleo"
  ),
  Período = c(
    periodo_max_desocupacion$Periodo, periodo_min_desocupacion$Periodo,
    periodo_max_empleo$Periodo, periodo_min_empleo$Periodo
  ),
  Valor = c(
    paste0(periodo_max_desocupacion$Tasa_Desocupacion, "%"),
    paste0(periodo_min_desocupacion$Tasa_Desocupacion, "%"),
    paste0(periodo_max_empleo$Tasa_Empleo, "%"),
    paste0(periodo_min_empleo$Tasa_Empleo, "%")
  ),
  stringsAsFactors = FALSE
)

cat("📊 Eventos críticos identificados:\n")
print(eventos_criticos)

# =============================================================================
# 3. GRÁFICO DE DISTRIBUCIONES (HISTOGRAMAS)
# =============================================================================

cat("\n📊 Creando gráficos de distribución...\n")

# Preparar datos para histogramas
datos_distribuciones <- serie_tasas_gba %>%
  select(Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Indicador", values_to = "Valor") %>%
  mutate(
    Indicador = case_when(
      Indicador == "Tasa_Actividad" ~ "Tasa de Actividad",
      Indicador == "Tasa_Empleo" ~ "Tasa de Empleo",
      Indicador == "Tasa_Desocupacion" ~ "Tasa de Desocupación"
    )
  )

# Crear histogramas
grafico_distribuciones <- datos_distribuciones %>%
  ggplot(aes(x = Valor, fill = Indicador)) +
  geom_histogram(alpha = 0.7, bins = 15, color = "white") +
  facet_wrap(~ Indicador, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(
    "Tasa de Actividad" = "#3498db",
    "Tasa de Empleo" = "#27ae60", 
    "Tasa de Desocupación" = "#e74c3c"
  )) +
  labs(
    title = "Distribución de Tasas Laborales",
    subtitle = "Gran Buenos Aires 2016-2024",
    x = "Tasa (%)",
    y = "Frecuencia",
    caption = "Fuente: EPH-INDEC"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# Guardar gráfico
guardar_grafico(grafico_distribuciones, "distribuciones_tasas_laborales", alto = 10)

cat("✅ Gráfico de distribuciones guardado\n")

# =============================================================================
# 4. ANÁLISIS DE VOLATILIDAD POR AÑO
# =============================================================================

cat("\n📈 Analizando volatilidad anual...\n")

volatilidad_anual <- serie_tasas_gba %>%
  group_by(ANO4) %>%
  summarise(
    Vol_Actividad = round(sd(Tasa_Actividad, na.rm = TRUE), 2),
    Vol_Empleo = round(sd(Tasa_Empleo, na.rm = TRUE), 2),
    Vol_Desocupacion = round(sd(Tasa_Desocupacion, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("Vol_"), names_to = "Indicador", values_to = "Volatilidad") %>%
  mutate(
    Indicador = case_when(
      Indicador == "Vol_Actividad" ~ "Actividad",
      Indicador == "Vol_Empleo" ~ "Empleo",
      Indicador == "Vol_Desocupacion" ~ "Desocupación"
    )
  )

# Gráfico de volatilidad
grafico_volatilidad <- volatilidad_anual %>%
  ggplot(aes(x = ANO4, y = Volatilidad, color = Indicador, group = Indicador)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Actividad" = "#3498db",
    "Empleo" = "#27ae60",
    "Desocupación" = "#e74c3c"
  )) +
  labs(
    title = "Volatilidad Intra-anual de Tasas Laborales",
    subtitle = "Desvío estándar por año - Gran Buenos Aires",
    x = "Año",
    y = "Volatilidad (Desvío Estándar)",
    color = "Indicador",
    caption = "Fuente: EPH-INDEC"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 2016:2024)

# Guardar gráfico
guardar_grafico(grafico_volatilidad, "volatilidad_anual_tasas_laborales")

cat("✅ Gráfico de volatilidad guardado\n")

# =============================================================================
# 5. CUARTILES Y PERCENTILES
# =============================================================================

cat("\n📊 Calculando cuartiles y percentiles...\n")

percentiles_tasas <- serie_tasas_gba %>%
  summarise(
    # Tasa de Actividad
    Act_P10 = round(quantile(Tasa_Actividad, 0.10, na.rm = TRUE), 1),
    Act_Q1 = round(quantile(Tasa_Actividad, 0.25, na.rm = TRUE), 1),
    Act_Q3 = round(quantile(Tasa_Actividad, 0.75, na.rm = TRUE), 1),
    Act_P90 = round(quantile(Tasa_Actividad, 0.90, na.rm = TRUE), 1),
    
    # Tasa de Empleo
    Emp_P10 = round(quantile(Tasa_Empleo, 0.10, na.rm = TRUE), 1),
    Emp_Q1 = round(quantile(Tasa_Empleo, 0.25, na.rm = TRUE), 1),
    Emp_Q3 = round(quantile(Tasa_Empleo, 0.75, na.rm = TRUE), 1),
    Emp_P90 = round(quantile(Tasa_Empleo, 0.90, na.rm = TRUE), 1),
    
    # Tasa de Desocupación
    Des_P10 = round(quantile(Tasa_Desocupacion, 0.10, na.rm = TRUE), 1),
    Des_Q1 = round(quantile(Tasa_Desocupacion, 0.25, na.rm = TRUE), 1),
    Des_Q3 = round(quantile(Tasa_Desocupacion, 0.75, na.rm = TRUE), 1),
    Des_P90 = round(quantile(Tasa_Desocupacion, 0.90, na.rm = TRUE), 1)
  )

# Transformar a tabla
tabla_percentiles <- data.frame(
  Percentil = c("P10", "Q1 (P25)", "Q3 (P75)", "P90"),
  `Tasa de Actividad` = c(percentiles_tasas$Act_P10, percentiles_tasas$Act_Q1, 
                          percentiles_tasas$Act_Q3, percentiles_tasas$Act_P90),
  `Tasa de Empleo` = c(percentiles_tasas$Emp_P10, percentiles_tasas$Emp_Q1,
                       percentiles_tasas$Emp_Q3, percentiles_tasas$Emp_P90),
  `Tasa de Desocupación` = c(percentiles_tasas$Des_P10, percentiles_tasas$Des_Q1,
                             percentiles_tasas$Des_Q3, percentiles_tasas$Des_P90),
  stringsAsFactors = FALSE
)

write.csv(tabla_percentiles, file.path(rutas$tablas, "percentiles_tasas_laborales.csv"), row.names = FALSE)

cat("✅ Tabla de percentiles creada\n")
print(tabla_percentiles)

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ ELEMENTOS PARA ANÁLISIS UNIVARIADO COMPLETADOS\n")
cat(rep("=", 60), "\n")

cat("📊 ELEMENTOS GENERADOS:\n")
cat("   • Tabla de estadísticas descriptivas completa\n")
cat("   • Identificación de eventos críticos\n") 
cat("   • Gráfico de distribuciones (histogramas)\n")
cat("   • Análisis de volatilidad anual\n")
cat("   • Tabla de cuartiles y percentiles\n")

cat("\n📝 PARA TU INFORME - ANÁLISIS UNIVARIADO:\n")
cat("   • Usa tu gráfico de evolución temporal como Figura principal\n")
cat("   • Incluye la tabla de estadísticas descriptivas\n")
cat("   • Agrega el gráfico de distribuciones\n")
cat("   • Menciona los eventos críticos identificados\n")
cat("   • Analiza la volatilidad por período\n")

cat("\n💡 PUNTOS CLAVE PARA REDACTAR:\n")
cat("   • Coeficiente de variación mayor en desocupación (más volátil)\n")
cat("   • Período 2020 como punto de quiebre\n")
cat("   • Recuperación 2021-2023\n")
cat("   • Distribuciones y patrones estacionales\n")

cat(rep("=", 60), "\n")