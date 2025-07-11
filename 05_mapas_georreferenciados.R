# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 05_mapas_simples.R - Mapas usando solo ggplot2 (sin tmap)
# =============================================================================

# Verificar que los datos estén cargados
if(!exists("datos_gba")) {
  cat("🔄 Cargando datos procesados...\n")
  load(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))
  cat("✅ Datos EPH cargados desde archivo\n")
}

if(!exists("serie_tasas_gba")) {
  cat("🔄 Cargando serie de tasas...\n")
  load(file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.RData"))
  cat("✅ Serie de tasas cargada\n")
}

cat("🗺️ Iniciando creación de mapas (solo ggplot2)...\n")

# =============================================================================
# CARGAR SOLO LIBRERÍAS BÁSICAS
# =============================================================================

# Cargar librerías necesarias (que ya están en tu setup)
library(dplyr)      # Para slice(), filter(), etc.
library(ggplot2)    # Para los mapas
library(stringr)    # Para str_wrap()

# Solo instalar sf si no está disponible
if(!require(sf, quietly = TRUE)) {
  install.packages("sf", type = "binary")
  library(sf)
}

cat("✅ Librerías básicas cargadas (dplyr, ggplot2, sf, stringr)\n")

# =============================================================================
# CONFIGURACIÓN
# =============================================================================

# Ruta de archivos georreferenciados
ruta_mapas <- "C:/Users/kflop/OneDrive/Documentos/TP_FINAL/mapas_indec"

# Verificar que la ruta existe
if(!dir.exists(ruta_mapas)) {
  stop("❌ La ruta de mapas no existe: ", ruta_mapas)
}

# =============================================================================
# FUNCIONES SIMPLIFICADAS
# =============================================================================

#' Cargar datos georreferenciados (versión simple)
cargar_datos_geo_simple <- function(ruta_base) {
  
  cat("📁 Buscando archivos .shp en:", ruta_base, "\n")
  
  # Buscar archivos .shp
  archivos_shp <- list.files(ruta_base, pattern = "\\.shp$", full.names = TRUE)
  
  if(length(archivos_shp) == 0) {
    stop("❌ No se encontraron archivos .shp")
  }
  
  cat("📋 Archivos encontrados:", length(archivos_shp), "\n")
  
  # Cargar el primer archivo disponible
  archivo_principal <- archivos_shp[1]
  cat("📥 Cargando:", basename(archivo_principal), "\n")
  
  datos_geo <- st_read(archivo_principal, quiet = TRUE)
  
  cat("✅ Cargado:", nrow(datos_geo), "geometrías\n")
  
  return(datos_geo)
}

#' Crear mapa simple con ggplot2
crear_mapa_ggplot <- function(datos_geo, valor, titulo, subtitulo, color_bajo, color_alto) {
  
  # Asignar valor a los datos geográficos
  datos_geo$valor_mapa <- valor
  
  # Crear el mapa
  mapa <- datos_geo %>%
    ggplot() +
    geom_sf(
      aes(fill = valor_mapa),
      color = "white",
      size = 0.8
    ) +
    scale_fill_gradient(
      name = stringr::str_wrap(titulo, 10),  # Usar stringr:: explícitamente
      low = color_bajo,
      high = color_alto,
      labels = function(x) paste0(round(x, 1), "%")
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      caption = "Fuente: EPH - INDEC"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray60"),
      plot.caption = element_text(size = 10, hjust = 1, color = "gray50"),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.5, "cm"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(mapa)
}

# =============================================================================
# CARGAR DATOS ESPACIALES
# =============================================================================

cat("📥 Cargando datos georreferenciados...\n")

tryCatch({
  datos_geo <- cargar_datos_geo_simple(ruta_mapas)
  
  # Mostrar información básica
  cat("📊 Información de los datos:\n")
  cat("   • Geometrías:", nrow(datos_geo), "\n")
  cat("   • CRS:", st_crs(datos_geo)$input, "\n")
  cat("   • Columnas:", ncol(datos_geo), "\n")
  
}, error = function(e) {
  stop("❌ Error cargando datos georreferenciados: ", e$message)
})

# =============================================================================
# PREPARAR DATOS
# =============================================================================

cat("🔢 Preparando datos para mapeo...\n")

# Obtener el período más reciente
periodo_reciente <- serie_tasas_gba %>%
  arrange(desc(ANO4), desc(TRIMESTRE)) %>%
  dplyr::slice(1)  # Usar dplyr:: explícitamente

cat("📅 Usando período:", periodo_reciente$Periodo, "\n")
cat("📊 Valores a mapear:\n")
cat("   • Tasa de Actividad:", periodo_reciente$Tasa_Actividad, "%\n")
cat("   • Tasa de Empleo:", periodo_reciente$Tasa_Empleo, "%\n")
cat("   • Tasa de Desocupación:", periodo_reciente$Tasa_Desocupacion, "%\n")

# =============================================================================
# CREAR MAPAS INDIVIDUALES
# =============================================================================

cat("\n🗺️ Creando mapas individuales...\n")

# Definir configuración para cada mapa
configuracion_mapas <- list(
  desocupacion = list(
    valor = periodo_reciente$Tasa_Desocupacion,
    titulo = "Tasa de Desocupación",
    color_bajo = "#fee5d9",
    color_alto = "#a50f15"
  ),
  empleo = list(
    valor = periodo_reciente$Tasa_Empleo,
    titulo = "Tasa de Empleo",
    color_bajo = "#edf8e9", 
    color_alto = "#006d2c"
  ),
  actividad = list(
    valor = periodo_reciente$Tasa_Actividad,
    titulo = "Tasa de Actividad",
    color_bajo = "#eff3ff",
    color_alto = "#08519c"
  )
)

# Crear y guardar cada mapa
mapas_creados <- list()

for(nombre in names(configuracion_mapas)) {
  
  cat("   📍 Creando mapa de", nombre, "...\n")
  
  config <- configuracion_mapas[[nombre]]
  
  subtitulo <- paste("Gran Buenos Aires -", periodo_reciente$Periodo)
  
  mapa <- crear_mapa_ggplot(
    datos_geo = datos_geo,
    valor = config$valor,
    titulo = config$titulo,
    subtitulo = subtitulo,
    color_bajo = config$color_bajo,
    color_alto = config$color_alto
  )
  
  # Guardar mapa
  nombre_archivo <- paste0("mapa_", nombre, "_", periodo_reciente$Periodo)
  guardar_grafico(mapa, nombre_archivo, "mapas", ancho = 10, alto = 8)
  
  mapas_creados[[nombre]] <- mapa
  
  cat("     ✅ Guardado:", nombre_archivo, "\n")
}

# =============================================================================
# CREAR MAPA COMBINADO
# =============================================================================

cat("\n🗺️ Creando mapa combinado...\n")

if(require(patchwork, quietly = TRUE)) {
  
  mapa_combinado <- mapas_creados$desocupacion + mapas_creados$empleo + mapas_creados$actividad +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Indicadores Laborales - Gran Buenos Aires",
      subtitle = paste("Período:", periodo_reciente$Periodo),
      caption = "Fuente: EPH - INDEC",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 1)
      )
    )
  
  # Guardar mapa combinado
  nombre_archivo <- paste0("mapas_combinados_", periodo_reciente$Periodo)
  guardar_grafico(mapa_combinado, nombre_archivo, "mapas", ancho = 16, alto = 12)
  
  cat("✅ Mapa combinado guardado\n")
  
} else {
  cat("⚠️ patchwork no disponible, solo mapas individuales\n")
}

# =============================================================================
# CREAR SERIE TEMPORAL SIMPLIFICADA
# =============================================================================

cat("\n📅 Creando mapas para períodos clave...\n")

# Períodos clave (solo algunos para no saturar)
periodos_clave <- serie_tasas_gba %>%
  filter(ANO4 %in% c(2020, 2022, 2024), TRIMESTRE == 4) %>%
  arrange(ANO4)

cat("📋 Creando mapas para", nrow(periodos_clave), "períodos\n")

for(i in 1:nrow(periodos_clave)) {
  
  periodo_actual <- periodos_clave[i, ]
  
  cat("   📅 Período:", periodo_actual$Periodo, "\n")
  
  # Solo crear mapa de desocupación para la serie
  mapa_periodo <- crear_mapa_ggplot(
    datos_geo = datos_geo,
    valor = periodo_actual$Tasa_Desocupacion,
    titulo = "Tasa de Desocupación",
    subtitulo = paste("Gran Buenos Aires -", periodo_actual$Periodo),
    color_bajo = "#fee5d9",
    color_alto = "#a50f15"
  )
  
  # Guardar
  nombre_archivo <- paste0("serie_desocupacion_", periodo_actual$Periodo)
  guardar_grafico(mapa_periodo, nombre_archivo, "mapas", ancho = 8, alto = 6)
}

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ MAPAS COMPLETADOS (VERSIÓN SIMPLIFICADA)\n")
cat(rep("=", 60), "\n")

cat("🗺️ MAPAS GENERADOS:\n")
cat("   • 3 mapas individuales (desocupación, empleo, actividad)\n")
if(exists("mapa_combinado")) {
  cat("   • 1 mapa combinado\n")
}
cat("   •", nrow(periodos_clave), "mapas de serie temporal\n")

cat("\n📁 UBICACIÓN:\n")
cat("   • Carpeta:", rutas$mapas, "\n")

cat("\n📊 PERÍODO ANALIZADO:\n")
cat("   •", periodo_reciente$Periodo, "\n")


cat(rep("=", 60), "\n")

# Mostrar uno de los mapas
if(length(mapas_creados) > 0) {
  cat("📋 Mostrando mapa de muestra...\n")
  print(mapas_creados[[1]])
}
