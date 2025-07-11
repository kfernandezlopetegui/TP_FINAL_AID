# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 03_analisis_descriptivo.R - Análisis descriptivo y series temporales
# =============================================================================

# Verificar que los datos estén cargados
if(!exists("datos_gba")) {
  cat("🔄 Cargando datos procesados...\n")
  load(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))
  cat("✅ Datos cargados desde archivo\n")
}

# Verificar que las constantes estén definidas
if(!exists("colores_tasas")) {
  cat("⚠️ Definiendo colores por defecto...\n")
  colores_tasas <- c(
    "Desocupación" = "#e74c3c", 
    "Empleo" = "#27ae60", 
    "Actividad" = "#3498db"
  )
}

if(!exists("colores_años")) {
  cat("⚠️ Definiendo colores de años por defecto...\n")
  colores_años <- c(
    "2016" = "#1f77b4", "2017" = "#ff7f0e", "2018" = "#2ca02c", "2019" = "#d62728",
    "2020" = "#9467bd", "2021" = "#8c564b", "2022" = "#e377c2", "2023" = "#7f7f7f", 
    "2024" = "#bcbd22"
  )
}

cat("📊 Iniciando análisis descriptivo...\n")
cat("📈 Datos disponibles:", length(datos_gba), "períodos\n")

# =============================================================================
# CÁLCULO DE SERIES DE TASAS LABORALES
# =============================================================================

cat("\n🔢 Calculando series de tasas laborales...\n")

# Calcular tasas laborales para todos los períodos
serie_tasas_gba <- calcular_serie_tasas(datos_gba)

cat("✅ Serie de tasas calculada para", nrow(serie_tasas_gba), "períodos\n")

# Mostrar primeros registros
cat("\n📋 Primeros registros de la serie:\n")
print(head(serie_tasas_gba))

# =============================================================================
# ESTADÍSTICAS DESCRIPTIVAS GENERALES
# =============================================================================

cat("\n📊 Estadísticas descriptivas de las tasas laborales:\n")

# Calcular estadísticas descriptivas
estadisticas_tasas <- resumen_tasas_laborales(serie_tasas_gba)
print(estadisticas_tasas)

# Identificar valores extremos
cat("\n🔍 Valores extremos identificados:\n")

# Período con mayor desocupación
max_desocup <- serie_tasas_gba[which.max(serie_tasas_gba$Tasa_Desocupacion), ]
cat("   • Mayor desocupación:", max_desocup$Tasa_Desocupacion, "% en", max_desocup$Periodo, "\n")

# Período con menor desocupación
min_desocup <- serie_tasas_gba[which.min(serie_tasas_gba$Tasa_Desocupacion), ]
cat("   • Menor desocupación:", min_desocup$Tasa_Desocupacion, "% en", min_desocup$Periodo, "\n")

# Período con mayor empleo
max_empleo <- serie_tasas_gba[which.max(serie_tasas_gba$Tasa_Empleo), ]
cat("   • Mayor empleo:", max_empleo$Tasa_Empleo, "% en", max_empleo$Periodo, "\n")

# Período con menor empleo
min_empleo <- serie_tasas_gba[which.min(serie_tasas_gba$Tasa_Empleo), ]
cat("   • Menor empleo:", min_empleo$Tasa_Empleo, "% en", min_empleo$Periodo, "\n")

# =============================================================================
# GRÁFICO 1: BARRAS TRIMESTRALES PARA TODOS LOS PERÍODOS
# =============================================================================

cat("\n📊 Generando gráfico de barras trimestrales...\n")

# Crear gráfico de barras por trimestre
grafico_barras_todos <- grafico_barras_trimestral(
  serie_tasas_gba, 
  titulo = "Tasas Laborales por Trimestre - GBA 2016-2024"
)

# Guardar gráfico
guardar_grafico(
  grafico_barras_todos, 
  "tasas_laborales_barras_trimestrales_2016_2024"
)

# Mostrar gráfico
print(grafico_barras_todos)

# =============================================================================
# GRÁFICO 2: EVOLUCIÓN TEMPORAL (LÍNEAS)
# =============================================================================

cat("\n📈 Generando gráfico de evolución temporal...\n")

# Crear gráfico de líneas
grafico_evolucion <- grafico_evolucion_temporal(
  serie_tasas_gba,
  titulo = "Evolución de Tasas Laborales - GBA 2016-2024"
)

# Guardar gráfico
guardar_grafico(
  grafico_evolucion,
  "tasas_laborales_evolucion_temporal_2016_2024"
)

# Mostrar gráfico
print(grafico_evolucion)

# =============================================================================
# GRÁFICO 3: COMPARACIÓN POR AÑOS ESPECÍFICOS
# =============================================================================

cat("\n📊 Generando comparación por años específicos...\n")

# Seleccionar años importantes para comparar
anos_importantes <- c(2016, 2018, 2020, 2022, 2024)

# Filtrar datos para años importantes
datos_anos_importantes <- serie_tasas_gba %>%
  filter(ANO4 %in% anos_importantes) %>%
  mutate(Año = factor(ANO4))

# Preparar datos para gráfico comparativo
datos_comparacion <- datos_anos_importantes %>%
  select(ANO4, TRIMESTRE, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
  pivot_longer(
    cols = starts_with("Tasa_"),
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  mutate(
    Indicador = case_when(
      Indicador == "Tasa_Actividad" ~ "Actividad",
      Indicador == "Tasa_Empleo" ~ "Empleo",
      Indicador == "Tasa_Desocupacion" ~ "Desocupación"
    ),
    Trimestre_Label = paste("Trimestre", TRIMESTRE)
  )

# Crear gráfico de comparación por años
grafico_comparacion_anos <- datos_comparacion %>%
  ggplot(aes(x = Trimestre_Label, y = Valor, color = factor(ANO4), group = ANO4)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~ Indicador, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = colores_años[as.character(anos_importantes)],
    name = "Año"
  ) +
  labs(
    title = "Comparación de Tasas Laborales por Año",
    subtitle = "Años seleccionados: 2016, 2018, 2020, 2022, 2024",
    x = "Trimestre",
    y = "Tasa (%)",
    caption = "Fuente: EPH - INDEC"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Guardar gráfico
guardar_grafico(
  grafico_comparacion_anos,
  "comparacion_anos_importantes_2016_2024",
  alto = 10
)

# Mostrar gráfico
print(grafico_comparacion_anos)

# =============================================================================
# GRÁFICO 4: ANÁLISIS ESTACIONAL
# =============================================================================

cat("\n📅 Generando análisis estacional...\n")

# Crear gráfico de patrones estacionales
datos_estacionales <- serie_tasas_gba %>%
  select(ANO4, TRIMESTRE, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
  pivot_longer(
    cols = starts_with("Tasa_"),
    names_to = "Indicador", 
    values_to = "Valor"
  ) %>%
  mutate(
    Indicador = case_when(
      Indicador == "Tasa_Actividad" ~ "Actividad",
      Indicador == "Tasa_Empleo" ~ "Empleo",
      Indicador == "Tasa_Desocupacion" ~ "Desocupación"
    )
  ) %>%
  group_by(TRIMESTRE, Indicador) %>%
  summarise(
    Valor_Promedio = mean(Valor, na.rm = TRUE),
    Valor_Min = min(Valor, na.rm = TRUE),
    Valor_Max = max(Valor, na.rm = TRUE),
    .groups = "drop"
  )

grafico_estacional <- datos_estacionales %>%
  ggplot(aes(x = factor(TRIMESTRE), y = Valor_Promedio, fill = Indicador)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = Valor_Min, ymax = Valor_Max),
    position = position_dodge(width = 0.9),
    width = 0.2,
    alpha = 0.6
  ) +
  scale_fill_manual(values = colores_tasas) +
  labs(
    title = "Patrones Estacionales de las Tasas Laborales",
    subtitle = "Promedio por trimestre (2016-2024) con rango min-max",
    x = "Trimestre",
    y = "Tasa (%)",
    fill = "Indicador",
    caption = "Fuente: EPH - INDEC"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60")
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Guardar gráfico
guardar_grafico(
  grafico_estacional,
  "analisis_estacional_trimestres_2016_2024"
)

# Mostrar gráfico
print(grafico_estacional)

# =============================================================================
# TABLA RESUMEN PARA INFORME
# =============================================================================

cat("\n📋 Generando tabla resumen...\n")

# Crear tabla resumen por año
tabla_resumen_anual <- serie_tasas_gba %>%
  group_by(ANO4) %>%
  summarise(
    Tasa_Actividad_Prom = round(mean(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Empleo_Prom = round(mean(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Desocup_Prom = round(mean(Tasa_Desocupacion, na.rm = TRUE), 1),
    Tasa_Actividad_Min = round(min(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Actividad_Max = round(max(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Empleo_Min = round(min(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Empleo_Max = round(max(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Desocup_Min = round(min(Tasa_Desocupacion, na.rm = TRUE), 1),
    Tasa_Desocup_Max = round(max(Tasa_Desocupacion, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Guardar tabla
write_csv(tabla_resumen_anual, file.path(rutas$tablas, "resumen_anual_tasas_laborales.csv"))

cat("\n📊 Tabla resumen anual:\n")
print(tabla_resumen_anual)

# =============================================================================
# GUARDAR SERIE TEMPORAL PROCESADA
# =============================================================================

cat("\n💾 Guardando serie temporal procesada...\n")

# Guardar serie de tasas
write_csv(serie_tasas_gba, file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.csv"))
save(serie_tasas_gba, file = file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.RData"))

cat("✅ Serie temporal guardada en formato CSV y RData\n")

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ ANÁLISIS DESCRIPTIVO COMPLETADO\n")
cat(rep("=", 60), "\n")

cat("📊 GRÁFICOS GENERADOS:\n")
cat("   1. Barras trimestrales (todos los períodos)\n")
cat("   2. Evolución temporal (líneas)\n")
cat("   3. Comparación años importantes\n")
cat("   4. Análisis estacional\n")

cat("\n📋 TABLAS GENERADAS:\n")
cat("   • Resumen anual de tasas laborales\n")
cat("   • Serie temporal completa\n")

cat("\n📁 ARCHIVOS GUARDADOS EN:\n")
cat("   • Gráficos:", rutas$graficos, "\n")
cat("   • Tablas:", rutas$tablas, "\n")
cat("   • Datos procesados:", rutas$datos_procesados, "\n")



cat(rep("=", 60), "\n")
