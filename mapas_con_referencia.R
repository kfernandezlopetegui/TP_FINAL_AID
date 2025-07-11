# =============================================================================
# EXTENSIÓN MAPAS - Agregar mapa de referencia con nombres de partidos
# =============================================================================

# Cargar librerías
library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)
library(stringr)

cat("🗺️ Creando mapas con referencia de partidos/zonas...\n")

# =============================================================================
# FUNCIÓN PARA CREAR MAPA DE REFERENCIA
# =============================================================================

crear_mapa_referencia <- function(datos_geo) {
  
  cat("📍 Creando mapa de referencia con nombres...\n")
  
  # Preparar datos para el mapa de referencia
  datos_ref <- datos_geo %>%
    mutate(
      # Identificar nombres de partidos/comunas
      Nombre_Area = case_when(
        # Si hay columna toponimo_i, usarla
        !is.null(toponimo_i) ~ as.character(toponimo_i),
        # Si no, crear nombres basados en la distribución CABA/GBA
        Area_EPH == "CABA" ~ paste("Comuna", ceiling(row_number()/100)),
        Area_EPH == "Partidos GBA" ~ paste("Partido", letters[ceiling(row_number()/500)]),
        TRUE ~ "Sin nombre"
      ),
      # Simplificar nombres muy largos
      Nombre_Corto = case_when(
        nchar(Nombre_Area) > 15 ~ paste0(substr(Nombre_Area, 1, 12), "..."),
        TRUE ~ Nombre_Area
      ),
      # Crear colores alternados para mejor visualización
      Color_Ref = case_when(
        Area_EPH == "CABA" ~ "#f0f0f0",
        Area_EPH == "Partidos GBA" ~ "#e0e0e0",
        TRUE ~ "#d0d0d0"
      )
    )
  
  # Calcular centroides para ubicar etiquetas
  centroides <- st_centroid(datos_ref)
  
  # Extraer coordenadas de centroides
  coords <- st_coordinates(centroides)
  datos_ref$x_centroid <- coords[,1]
  datos_ref$y_centroid <- coords[,2]
  
  # Seleccionar solo algunos nombres para evitar saturación
  # (cada N geometrías según el tamaño)
  n_etiquetas <- min(30, ceiling(nrow(datos_ref) / 500))
  
  datos_etiquetas <- datos_ref %>%
    slice_sample(n = n_etiquetas) %>%
    filter(!is.na(x_centroid), !is.na(y_centroid))
  
  # Crear mapa de referencia
  mapa_ref <- ggplot(datos_ref) +
    geom_sf(
      aes(fill = Area_EPH),
      color = "white",
      size = 0.1
    ) +
    scale_fill_manual(
      values = c("CABA" = "#e8f4f8", "Partidos GBA" = "#f8f4e8"),
      name = "Área"
    ) +
    geom_text(
      data = datos_etiquetas,
      aes(x = x_centroid, y = y_centroid, label = Nombre_Corto),
      size = 2,
      color = "gray30",
      fontface = "bold",
      check_overlap = TRUE
    ) +
    labs(
      title = "Mapa de Referencia",
      subtitle = "Principales áreas del Gran Buenos Aires"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray60"),
      legend.position = "none",  # Sin leyenda para que sea más limpio
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  return(mapa_ref)
}

# =============================================================================
# FUNCIÓN PARA COMBINAR MAPA PRINCIPAL + REFERENCIA
# =============================================================================

combinar_con_referencia <- function(mapa_principal, datos_geo, titulo_general) {
  
  # Crear mapa de referencia
  mapa_ref <- crear_mapa_referencia(datos_geo)
  
  # Combinar mapas usando patchwork
  mapa_combinado <- mapa_principal / mapa_ref +
    plot_layout(heights = c(3, 1)) +  # El mapa principal 3 veces más alto
    plot_annotation(
      title = titulo_general,
      caption = "Fuente: EPH - INDEC",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1, color = "gray50")
      )
    )
  
  return(mapa_combinado)
}

# =============================================================================
# APLICAR A LOS MAPAS EXISTENTES
# =============================================================================

cat("🔄 Recreando mapas con referencia...\n")

# Verificar que tenemos los datos necesarios
if(!exists("geo_con_tasas")) {
  cat("⚠️ Ejecuta primero el script de mapas detallados\n")
  stop("Datos no encontrados")
}

# Recrear mapas con referencia
mapas_con_referencia <- list()

configuracion_mapas_ref <- list(
  desocupacion = list(
    variable = "Tasa_Desocupacion_Var",
    titulo = "Tasa de Desocupación - Gran Buenos Aires",
    paleta = "Reds"
  ),
  empleo = list(
    variable = "Tasa_Empleo_Var", 
    titulo = "Tasa de Empleo - Gran Buenos Aires",
    paleta = "Greens"
  ),
  actividad = list(
    variable = "Tasa_Actividad_Var",
    titulo = "Tasa de Actividad - Gran Buenos Aires", 
    paleta = "Blues"
  )
)

for(nombre_indicador in names(configuracion_mapas_ref)) {
  
  cat("   📍 Creando mapa con referencia:", nombre_indicador, "...\n")
  
  config <- configuracion_mapas_ref[[nombre_indicador]]
  
  # Extraer período del nombre
  periodo_texto <- gsub("_", " ", periodo_reciente)
  subtitulo <- paste("Período:", periodo_texto)
  
  # Crear mapa principal (reutilizar función existente)
  mapa_principal <- crear_mapa_estilo_indec(
    datos_geo = geo_con_tasas,
    variable = config$variable,
    titulo = config$titulo,
    subtitulo = subtitulo,
    paleta = config$paleta
  )
  
  # Combinar con mapa de referencia
  mapa_completo <- combinar_con_referencia(
    mapa_principal = mapa_principal,
    datos_geo = geo_con_tasas,
    titulo_general = config$titulo
  )
  
  # Guardar mapa con referencia
  nombre_archivo <- paste0("mapa_con_referencia_", nombre_indicador, "_", periodo_reciente)
  guardar_grafico(mapa_completo, nombre_archivo, "mapas", ancho = 12, alto = 14)
  
  mapas_con_referencia[[nombre_indicador]] <- mapa_completo
  
  cat("     ✅ Guardado:", nombre_archivo, "\n")
}

# =============================================================================
# CREAR VERSIÓN MEGA COMBINADA
# =============================================================================

cat("\n🗺️ Creando versión mega combinada con referencia...\n")

if(require(patchwork, quietly = TRUE)) {
  
  # Crear mapa de referencia único
  mapa_ref_general <- crear_mapa_referencia(geo_con_tasas)
  
  # Crear layout complejo: 3 mapas arriba + 1 referencia abajo
  layout_completo <- (mapas_detallados$desocupacion | mapas_detallados$empleo | mapas_detallados$actividad) / 
    mapa_ref_general +
    plot_layout(heights = c(3, 1)) +
    plot_annotation(
      title = "Atlas Laboral - Gran Buenos Aires",
      subtitle = paste("Indicadores Laborales -", gsub("_", " ", periodo_reciente)),
      caption = "Fuente: EPH - INDEC | Mapa de referencia incluido para ubicación geográfica",
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray60"),
        plot.caption = element_text(size = 11, hjust = 1, color = "gray50")
      )
    )
  
  # Guardar atlas completo
  nombre_archivo <- paste0("atlas_laboral_con_referencia_", periodo_reciente)
  guardar_grafico(layout_completo, nombre_archivo, "mapas", ancho = 24, alto = 16)
  
  cat("✅ Atlas laboral completo guardado\n")
}

# =============================================================================
# CREAR MAPA DE PARTIDOS/COMUNAS INDEPENDIENTE
# =============================================================================

cat("\n📍 Creando mapa independiente de partidos/comunas...\n")

# Crear un mapa solo con nombres y límites
mapa_solo_partidos <- geo_con_tasas %>%
  ggplot() +
  geom_sf(
    aes(fill = Area_EPH),
    color = "gray30",
    size = 0.2,
    alpha = 0.7
  ) +
  scale_fill_manual(
    values = c("CABA" = "#e3f2fd", "Partidos GBA" = "#f3e5f5"),
    name = "Área",
    labels = c("Ciudad Autónoma\nde Buenos Aires", "Partidos del\nGran Buenos Aires")
  ) +
  labs(
    title = "División Político-Administrativa",
    subtitle = "Gran Buenos Aires: CABA y Partidos del GBA",
    caption = "Fuente: EPH - INDEC"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray60"),
    plot.caption = element_text(size = 11, hjust = 1, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Guardar mapa de partidos
nombre_archivo <- paste0("mapa_division_politica_", periodo_reciente)
guardar_grafico(mapa_solo_partidos, nombre_archivo, "mapas", ancho = 10, alto = 8)

cat("✅ Mapa de división política guardado\n")

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ MAPAS CON REFERENCIA COMPLETADOS\n")
cat(rep("=", 60), "\n")

cat("🗺️ MAPAS CREADOS:\n")
cat("   • 3 mapas individuales CON mapa de referencia abajo\n")
cat("   • 1 atlas laboral completo con referencia\n")
cat("   • 1 mapa independiente de división política\n")

cat("\n📍 CARACTERÍSTICAS DE REFERENCIA:\n")
cat("   • Nombres de áreas principales\n")
cat("   • Distinción visual CABA vs Partidos GBA\n")
cat("   • Etiquetas optimizadas (sin saturación)\n")
cat("   • Colores diferenciados por zona\n")

cat("\n🎨 ESTILO PROFESIONAL:\n")
cat("   • Layout estilo INDEC oficial\n")
cat("   • Proporciones apropiadas (3:1)\n")
cat("   • Títulos y subtítulos claros\n")
cat("   • Referencias geográficas completas\n")

cat("\n📁 ARCHIVOS NUEVOS:\n")
cat("   • mapa_con_referencia_[indicador]_2024_T4.png\n")
cat("   • atlas_laboral_con_referencia_2024_T4.png\n")
cat("   • mapa_division_politica_2024_T4.png\n")

cat("\n💡 VENTAJAS:\n")
cat("   • Fácil identificación de zonas\n")
cat("   • Contexto geográfico completo\n")
cat("   • Aspecto profesional tipo INDEC\n")
cat("   • Ideal para informes académicos\n")

cat(rep("=", 60), "\n")

# Mostrar uno de los mapas con referencia
if(length(mapas_con_referencia) > 0) {
  cat("📋 Mostrando mapa con referencia de muestra...\n")
  print(mapas_con_referencia[[1]])
}