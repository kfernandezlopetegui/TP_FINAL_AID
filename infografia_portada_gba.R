# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Infografía de Portada estilo INDEC para GBA 2016-2024
# =============================================================================

# Cargar librerías
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(scales)
library(stringr)

cat("🎨 Creando infografía de portada estilo INDEC...\n")

# Verificar datos
if(!exists("serie_tasas_gba")) {
  load(file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.RData"))
}

# =============================================================================
# PREPARAR DATOS PARA LA INFOGRAFÍA
# =============================================================================

# Calcular datos del período más reciente (2024 T4)
datos_recientes <- serie_tasas_gba %>%
  filter(ANO4 == 2024, TRIMESTRE == 4) %>%
  slice(1)

# Calcular datos del período inicial (2016 T4) para comparación
datos_iniciales <- serie_tasas_gba %>%
  filter(ANO4 == 2016, TRIMESTRE == 4) %>%
  slice(1)

# Calcular promedios del período completo
promedios_periodo <- serie_tasas_gba %>%
  summarise(
    Tasa_Actividad_Prom = round(mean(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Empleo_Prom = round(mean(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Desocupacion_Prom = round(mean(Tasa_Desocupacion, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Calcular cambios en el período
if(nrow(datos_iniciales) > 0 && nrow(datos_recientes) > 0) {
  cambio_actividad <- datos_recientes$Tasa_Actividad - datos_iniciales$Tasa_Actividad
  cambio_empleo <- datos_recientes$Tasa_Empleo - datos_iniciales$Tasa_Empleo
  cambio_desocupacion <- datos_recientes$Tasa_Desocupacion - datos_iniciales$Tasa_Desocupacion
} else {
  cambio_actividad <- 0
  cambio_empleo <- 0
  cambio_desocupacion <- 0
}

cat("📊 Datos para infografía:\n")
cat("   • Tasa Actividad 2024:", datos_recientes$Tasa_Actividad, "%\n")
cat("   • Tasa Empleo 2024:", datos_recientes$Tasa_Empleo, "%\n") 
cat("   • Tasa Desocupación 2024:", datos_recientes$Tasa_Desocupacion, "%\n")

# =============================================================================
# FUNCIONES PARA CREAR ELEMENTOS GRÁFICOS
# =============================================================================

# Función para crear gráficos de dona
crear_grafico_dona <- function(valor, titulo, subtitulo, color_principal, color_fondo = "#f0f0f0") {
  
  # Preparar datos
  datos_dona <- data.frame(
    categoria = c("Valor", "Resto"),
    valor = c(valor, 100 - valor),
    stringsAsFactors = FALSE
  )
  
  # Crear gráfico
  p <- ggplot(datos_dona, aes(x = 2, y = valor, fill = categoria)) +
    geom_col(width = 1, alpha = 0.8) +
    coord_polar(theta = "y", start = 0) +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = c("Valor" = color_principal, "Resto" = color_fondo)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(title = titulo, subtitle = subtitulo) +
    # Agregar texto en el centro
    annotate("text", x = 0.5, y = 0, label = paste0(valor, "%"), 
             size = 8, fontface = "bold", color = color_principal)
  
  return(p)
}

# Función para crear barras de comparación
crear_barra_comparacion <- function(valor_2016, valor_2024, titulo, color) {
  
  datos_barra <- data.frame(
    periodo = c("2016", "2024"),
    valor = c(valor_2016, valor_2024),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot(datos_barra, aes(x = periodo, y = valor, fill = periodo)) +
    geom_col(alpha = 0.8, width = 0.6) +
    scale_fill_manual(values = c("2016" = paste0(color, "80"), "2024" = color)) +
    labs(title = titulo, y = "Tasa (%)") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    geom_text(aes(label = paste0(valor, "%")), 
              vjust = -0.5, size = 4, fontface = "bold")
  
  return(p)
}

# =============================================================================
# CREAR ELEMENTOS DE LA INFOGRAFÍA
# =============================================================================

# Título principal
titulo_principal <- textGrob(
  "Mercado de Trabajo - Gran Buenos Aires\nAnálisis EPH 2016-2024", 
  gp = gpar(fontsize = 18, fontface = "bold", col = "#2c3e50")
)

# Subtítulo
subtitulo <- textGrob(
  "Resumen ejecutivo del análisis temporal", 
  gp = gpar(fontsize = 14, col = "#7f8c8d")
)

# Información del aglomerado
info_aglomerado <- textGrob(
  paste("GRAN BUENOS AIRES\nCódigos 32 (CABA) y 33 (Partidos GBA)\nPeríodo analizado: 2016-2024"), 
  gp = gpar(fontsize = 12, col = "#34495e")
)

# Gráficos de dona principales
dona_actividad <- crear_grafico_dona(
  valor = datos_recientes$Tasa_Actividad,
  titulo = "Tasa de Actividad",
  subtitulo = "4º Trimestre 2024",
  color_principal = "#3498db"
)

dona_empleo <- crear_grafico_dona(
  valor = datos_recientes$Tasa_Empleo,
  titulo = "Tasa de Empleo", 
  subtitulo = "4º Trimestre 2024",
  color_principal = "#27ae60"
)

dona_desocupacion <- crear_grafico_dona(
  valor = datos_recientes$Tasa_Desocupacion,
  titulo = "Tasa de Desocupación",
  subtitulo = "4º Trimestre 2024", 
  color_principal = "#e74c3c"
)

# Gráficos de comparación temporal
barra_actividad <- crear_barra_comparacion(
  valor_2016 = ifelse(nrow(datos_iniciales) > 0, datos_iniciales$Tasa_Actividad, 0),
  valor_2024 = datos_recientes$Tasa_Actividad,
  titulo = "Evolución Actividad",
  color = "#3498db"
)

barra_empleo <- crear_barra_comparacion(
  valor_2016 = ifelse(nrow(datos_iniciales) > 0, datos_iniciales$Tasa_Empleo, 0),
  valor_2024 = datos_recientes$Tasa_Empleo,
  titulo = "Evolución Empleo", 
  color = "#27ae60"
)

barra_desocupacion <- crear_barra_comparacion(
  valor_2016 = ifelse(nrow(datos_iniciales) > 0, datos_iniciales$Tasa_Desocupacion, 0),
  valor_2024 = datos_recientes$Tasa_Desocupacion,
  titulo = "Evolución Desocupación",
  color = "#e74c3c"
)

# =============================================================================
# CREAR MINI GRÁFICO DE EVOLUCIÓN TEMPORAL
# =============================================================================

# Preparar datos de evolución
datos_evolucion <- serie_tasas_gba %>%
  mutate(Fecha = as.Date(paste(ANO4, (TRIMESTRE-1)*3 + 1, "01", sep = "-"))) %>%
  select(Fecha, Tasa_Desocupacion) %>%
  arrange(Fecha)

# Mini gráfico de línea
mini_evolucion <- ggplot(datos_evolucion, aes(x = Fecha, y = Tasa_Desocupacion)) +
  geom_line(color = "#e74c3c", size = 1.2, alpha = 0.8) +
  geom_point(color = "#e74c3c", size = 1.5, alpha = 0.8) +
  labs(
    title = "Evolución Tasa de Desocupación 2016-2024",
    y = "Tasa (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# =============================================================================
# CREAR TABLA RESUMEN
# =============================================================================

# Preparar datos para tabla
tabla_datos <- data.frame(
  Indicador = c("Tasa de Actividad", "Tasa de Empleo", "Tasa de Desocupación"),
  `2024` = c(datos_recientes$Tasa_Actividad, 
             datos_recientes$Tasa_Empleo, 
             datos_recientes$Tasa_Desocupacion),
  `Promedio 2016-2024` = c(promedios_periodo$Tasa_Actividad_Prom,
                           promedios_periodo$Tasa_Empleo_Prom,
                           promedios_periodo$Tasa_Desocupacion_Prom),
  `Cambio vs 2016` = c(
    ifelse(nrow(datos_iniciales) > 0, 
           paste0(ifelse(cambio_actividad >= 0, "+", ""), round(cambio_actividad, 1), " pp"), 
           "N/A"),
    ifelse(nrow(datos_iniciales) > 0, 
           paste0(ifelse(cambio_empleo >= 0, "+", ""), round(cambio_empleo, 1), " pp"), 
           "N/A"),
    ifelse(nrow(datos_iniciales) > 0, 
           paste0(ifelse(cambio_desocupacion >= 0, "+", ""), round(cambio_desocupacion, 1), " pp"), 
           "N/A")
  ),
  stringsAsFactors = FALSE
)

# Convertir tabla a grob
tabla_grob <- tableGrob(tabla_datos, 
                        theme = ttheme_minimal(
                          core = list(fg_params = list(fontsize = 10),
                                      bg_params = list(fill = c("#f8f9fa", "#ffffff"))),
                          colhead = list(fg_params = list(fontsize = 11, fontface = "bold"),
                                         bg_params = list(fill = "#e9ecef"))
                        ))

# =============================================================================
# COMBINAR TODO EN LA INFOGRAFÍA
# =============================================================================

cat("🎨 Combinando elementos en infografía...\n")

# Crear layout principal
infografia_completa <- grid.arrange(
  # Fila 1: Título y subtítulo
  arrangeGrob(titulo_principal, subtitulo, ncol = 1, heights = c(0.6, 0.4)),
  
  # Fila 2: Info del aglomerado y mini evolución
  arrangeGrob(info_aglomerado, mini_evolucion, ncol = 2, widths = c(0.3, 0.7)),
  
  # Fila 3: Gráficos de dona principales
  arrangeGrob(dona_actividad, dona_empleo, dona_desocupacion, ncol = 3),
  
  # Fila 4: Gráficos de comparación temporal
  arrangeGrob(barra_actividad, barra_empleo, barra_desocupacion, ncol = 3),
  
  # Fila 5: Tabla resumen
  tabla_grob,
  
  # Fila 6: Pie de página
  textGrob("Fuente: EPH - INDEC | Elaboración propia", 
           gp = gpar(fontsize = 10, col = "#7f8c8d")),
  
  ncol = 1,
  heights = c(0.15, 0.15, 0.25, 0.20, 0.20, 0.05)
)

# =============================================================================
# GUARDAR INFOGRAFÍA
# =============================================================================

cat("💾 Guardando infografía...\n")

# Guardar como PNG de alta resolución
ruta_infografia <- file.path(rutas$graficos, "infografia_portada_gba_2016_2024.png")

png(ruta_infografia, width = 12, height = 16, units = "in", res = 300, bg = "white")
grid.draw(infografia_completa)
dev.off()

cat("✅ Infografía guardada en:", ruta_infografia, "\n")

# También crear versión PDF
ruta_pdf <- file.path(rutas$graficos, "infografia_portada_gba_2016_2024.pdf")

pdf(ruta_pdf, width = 12, height = 16, bg = "white")
grid.draw(infografia_completa)
dev.off()

cat("✅ Versión PDF guardada en:", ruta_pdf, "\n")

# =============================================================================
# CREAR VERSIÓN HORIZONTAL PARA PRESENTACIONES
# =============================================================================

cat("🖼️ Creando versión horizontal...\n")

infografia_horizontal <- grid.arrange(
  # Columna izquierda
  arrangeGrob(
    titulo_principal,
    subtitulo,
    info_aglomerado,
    tabla_grob,
    ncol = 1,
    heights = c(0.2, 0.1, 0.15, 0.55)
  ),
  
  # Columna derecha
  arrangeGrob(
    # Donas arriba
    arrangeGrob(dona_actividad, dona_empleo, dona_desocupacion, ncol = 3),
    # Barras abajo
    arrangeGrob(barra_actividad, barra_empleo, barra_desocupacion, ncol = 3),
    # Evolución al final
    mini_evolucion,
    ncol = 1,
    heights = c(0.35, 0.35, 0.3)
  ),
  
  ncol = 2,
  widths = c(0.4, 0.6)
)

# Guardar versión horizontal
ruta_horizontal <- file.path(rutas$graficos, "infografia_horizontal_gba_2016_2024.png")

png(ruta_horizontal, width = 16, height = 10, units = "in", res = 300, bg = "white")
grid.draw(infografia_horizontal)
dev.off()

cat("✅ Versión horizontal guardada en:", ruta_horizontal, "\n")

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ INFOGRAFÍA DE PORTADA COMPLETADA\n")
cat(rep("=", 60), "\n")

cat("🎨 ARCHIVOS CREADOS:\n")
cat("   • infografia_portada_gba_2016_2024.png (vertical, ideal para portada)\n")
cat("   • infografia_portada_gba_2016_2024.pdf (versión PDF)\n") 
cat("   • infografia_horizontal_gba_2016_2024.png (para presentaciones)\n")

cat("\n📊 ELEMENTOS INCLUIDOS:\n")
cat("   • Gráficos de dona con tasas principales (2024)\n")
cat("   • Comparación temporal 2016 vs 2024\n")
cat("   • Mini gráfico de evolución\n")
cat("   • Tabla resumen con promedios\n")
cat("   • Información del aglomerado\n")

cat("\n💡 DATOS DESTACADOS:\n")
cat("   • Tasa de Actividad 2024:", datos_recientes$Tasa_Actividad, "%\n")
cat("   • Tasa de Empleo 2024:", datos_recientes$Tasa_Empleo, "%\n") 
cat("   • Tasa de Desocupación 2024:", datos_recientes$Tasa_Desocupacion, "%\n")

if(nrow(datos_iniciales) > 0) {
  cat("   • Cambio Actividad 2016-2024:", 
      ifelse(cambio_actividad >= 0, "+", ""), round(cambio_actividad, 1), "pp\n")
  cat("   • Cambio Empleo 2016-2024:", 
      ifelse(cambio_empleo >= 0, "+", ""), round(cambio_empleo, 1), "pp\n")
  cat("   • Cambio Desocupación 2016-2024:", 
      ifelse(cambio_desocupacion >= 0, "+", ""), round(cambio_desocupacion, 1), "pp\n")
}

cat("\n🎯 RECOMENDACIÓN PARA TU INFORME:\n")
cat("   • Usa la versión vertical como portada principal\n")
cat("   • La versión horizontal es perfecta para presentaciones\n")
cat("   • Ambas tienen calidad para impresión profesional\n")

cat(rep("=", 60), "\n")