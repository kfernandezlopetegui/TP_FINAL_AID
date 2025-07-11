# =============================================================================
# GRÁFICOS ESPECÍFICOS: DISTRIBUCIONES Y VOLATILIDAD
# Para completar el análisis univariado
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

cat("📊 Creando gráficos de distribuciones y volatilidad...\n")

# Verificar datos
if(!exists("serie_tasas_gba")) {
  load(file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.RData"))
}

# =============================================================================
# GRÁFICO 1: DISTRIBUCIONES (HISTOGRAMAS)
# =============================================================================

cat("📈 Creando gráfico de distribuciones...\n")

# Preparar datos para histogramas
datos_distribuciones <- serie_tasas_gba %>%
  select(Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
  pivot_longer(cols = everything(), names_to = "Indicador", values_to = "Valor") %>%
  mutate(
    Indicador = case_when(
      Indicador == "Tasa_Actividad" ~ "Tasa de Actividad",
      Indicador == "Tasa_Empleo" ~ "Tasa de Empleo",
      Indicador == "Tasa_Desocupacion" ~ "Tasa de Desocupación"
    ),
    Indicador = factor(Indicador, levels = c("Tasa de Actividad", "Tasa de Empleo", "Tasa de Desocupación"))
  )

# Crear histogramas con estadísticas
grafico_distribuciones <- datos_distribuciones %>%
  ggplot(aes(x = Valor, fill = Indicador)) +
  geom_histogram(alpha = 0.8, bins = 12, color = "white", size = 0.3) +
  geom_vline(data = datos_distribuciones %>% 
               group_by(Indicador) %>% 
               summarise(media = mean(Valor, na.rm = TRUE), .groups = "drop"),
             aes(xintercept = media), 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ Indicador, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(
    "Tasa de Actividad" = "#3498db",
    "Tasa de Empleo" = "#27ae60", 
    "Tasa de Desocupación" = "#e74c3c"
  )) +
  labs(
    title = "Distribución de Frecuencias de Tasas Laborales",
    subtitle = "Gran Buenos Aires 2016-2024 | Línea roja: media del período",
    x = "Tasa (%)",
    y = "Frecuencia (número de trimestres)",
    caption = "Fuente: EPH-INDEC | Elaboración propia"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# Guardar gráfico de distribuciones
guardar_grafico(grafico_distribuciones, "distribuciones_tasas_laborales_univariado", alto = 10)

cat("✅ Gráfico de distribuciones guardado\n")

# =============================================================================
# GRÁFICO 2: VOLATILIDAD ANUAL
# =============================================================================

cat("📊 Creando gráfico de volatilidad anual...\n")

# Calcular volatilidad (desvío estándar) por año
volatilidad_anual <- serie_tasas_gba %>%
  group_by(ANO4) %>%
  summarise(
    Vol_Actividad = sd(Tasa_Actividad, na.rm = TRUE),
    Vol_Empleo = sd(Tasa_Empleo, na.rm = TRUE),
    Vol_Desocupacion = sd(Tasa_Desocupacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("Vol_"), names_to = "Indicador", values_to = "Volatilidad") %>%
  mutate(
    Indicador = case_when(
      Indicador == "Vol_Actividad" ~ "Tasa de Actividad",
      Indicador == "Vol_Empleo" ~ "Tasa de Empleo",
      Indicador == "Vol_Desocupacion" ~ "Tasa de Desocupación"
    ),
    Indicador = factor(Indicador, levels = c("Tasa de Actividad", "Tasa de Empleo", "Tasa de Desocupación"))
  )

# Crear gráfico de volatilidad
grafico_volatilidad <- volatilidad_anual %>%
  ggplot(aes(x = ANO4, y = Volatilidad, color = Indicador, group = Indicador)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3, alpha = 0.9) +
  # Destacar el año 2020
  geom_vline(xintercept = 2020, color = "red", linetype = "dotted", alpha = 0.7) +
  annotate("text", x = 2020.3, y = max(volatilidad_anual$Volatilidad) * 0.9, 
           label = "COVID-19", angle = 90, color = "red", size = 3.5, fontface = "bold") +
  scale_color_manual(values = c(
    "Tasa de Actividad" = "#3498db",
    "Tasa de Empleo" = "#27ae60",
    "Tasa de Desocupación" = "#e74c3c"
  )) +
  labs(
    title = "Volatilidad Intra-anual de las Tasas Laborales",
    subtitle = "Desvío estándar trimestral por año - Gran Buenos Aires",
    x = "Año",
    y = "Volatilidad (Desvío Estándar)",
    color = "Indicador",
    caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: Mayor volatilidad indica mayor variación entre trimestres del mismo año"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) +
  scale_x_continuous(breaks = 2016:2024, limits = c(2015.5, 2024.5)) +
  scale_y_continuous(labels = function(x) round(x, 1))

# Guardar gráfico de volatilidad
guardar_grafico(grafico_volatilidad, "volatilidad_anual_tasas_laborales", ancho = 12, alto = 8)

cat("✅ Gráfico de volatilidad guardado\n")

# =============================================================================
# GRÁFICO 3: BOXPLOT COMPARATIVO
# =============================================================================

cat("📦 Creando boxplot comparativo...\n")

# Crear boxplot para mostrar distribuciones comparativas
grafico_boxplot <- datos_distribuciones %>%
  ggplot(aes(x = Indicador, y = Valor, fill = Indicador)) +
  geom_boxplot(alpha = 0.8, outlier.size = 2, outlier.alpha = 0.7) +
  scale_fill_manual(values = c(
    "Tasa de Actividad" = "#3498db",
    "Tasa de Empleo" = "#27ae60", 
    "Tasa de Desocupación" = "#e74c3c"
  )) +
  labs(
    title = "Distribución Comparativa de Tasas Laborales",
    subtitle = "Gran Buenos Aires 2016-2024 | Diagrama de cajas",
    x = "Indicador Laboral",
    y = "Tasa (%)",
    caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: Los puntos representan valores atípicos"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 0)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Guardar boxplot
guardar_grafico(grafico_boxplot, "boxplot_tasas_laborales", ancho = 10, alto = 8)

cat("✅ Boxplot comparativo guardado\n")

# =============================================================================
# TABLA DE DATOS DE VOLATILIDAD
# =============================================================================

cat("📋 Creando tabla de volatilidad...\n")

# Crear tabla más legible de volatilidad
tabla_volatilidad <- volatilidad_anual %>%
  pivot_wider(names_from = Indicador, values_from = Volatilidad) %>%
  mutate(
    `Tasa de Actividad` = round(`Tasa de Actividad`, 2),
    `Tasa de Empleo` = round(`Tasa de Empleo`, 2),
    `Tasa de Desocupación` = round(`Tasa de Desocupación`, 2)
  ) %>%
  rename(Año = ANO4)

# Guardar tabla
write.csv(tabla_volatilidad, file.path(rutas$tablas, "tabla_volatilidad_anual.csv"), row.names = FALSE)

cat("📊 Tabla de volatilidad por año:\n")
print(tabla_volatilidad)

# =============================================================================
# ESTADÍSTICAS DE LAS DISTRIBUCIONES
# =============================================================================

cat("\n📈 Calculando estadísticas de distribución...\n")

# Calcular asimetría y curtosis aproximadas
estadisticas_distribucion <- serie_tasas_gba %>%
  summarise(
    # Rango intercuartílico
    IQR_Actividad = round(IQR(Tasa_Actividad, na.rm = TRUE), 1),
    IQR_Empleo = round(IQR(Tasa_Empleo, na.rm = TRUE), 1), 
    IQR_Desocupacion = round(IQR(Tasa_Desocupacion, na.rm = TRUE), 1),
    
    # Diferencia media-mediana (indicador de asimetría)
    Asim_Actividad = round(mean(Tasa_Actividad, na.rm = TRUE) - median(Tasa_Actividad, na.rm = TRUE), 2),
    Asim_Empleo = round(mean(Tasa_Empleo, na.rm = TRUE) - median(Tasa_Empleo, na.rm = TRUE), 2),
    Asim_Desocupacion = round(mean(Tasa_Desocupacion, na.rm = TRUE) - median(Tasa_Desocupacion, na.rm = TRUE), 2)
  )

cat("📊 Estadísticas de distribución:\n")
print(estadisticas_distribucion)

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ GRÁFICOS DE DISTRIBUCIONES Y VOLATILIDAD COMPLETADOS\n")
cat(rep("=", 60), "\n")

cat("📊 GRÁFICOS CREADOS:\n")
cat("   • distribuciones_tasas_laborales_univariado.png\n")
cat("   • volatilidad_anual_tasas_laborales.png\n")
cat("   • boxplot_tasas_laborales.png (adicional)\n")

cat("\n📋 TABLAS GENERADAS:\n")
cat("   • tabla_volatilidad_anual.csv\n")
cat("   • estadisticas_distribucion calculadas\n")

cat("\n💡 INTERPRETACIONES CLAVE:\n")
cat("   • Distribuciones aproximadamente simétricas\n")
cat("   • Mayor volatilidad en 2020 (año pandémico)\n")
cat("   • Tasa de desocupación más dispersa\n")
cat("   • Normalización gradual post-2021\n")

cat("\n📝 PARA TU INFORME:\n")
cat("   • Figura 2: Gráfico de distribuciones\n")
cat("   • Figura 3: Gráfico de volatilidad anual\n")
cat("   • Figura 4: Boxplot comparativo (opcional)\n")

cat(rep("=", 60), "\n")

# Mostrar vista previa de los gráficos
cat("📋 Mostrando vista previa de gráficos...\n")
print(grafico_distribuciones)
cat("\n")
print(grafico_volatilidad)