# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 01_setup.R - Configuración inicial del proyecto
# =============================================================================

# Limpiar entorno y empezar de nuevo
rm(list = ls())

# =============================================================================
# CONFIGURACIÓN DE DIRECTORIOS
# =============================================================================

# Obtener directorio actual del proyecto
proyecto_dir <- getwd()
cat("Directorio del proyecto:", proyecto_dir, "\n")

# Crear estructura de carpetas
carpetas <- c(
  "datos",
  "datos/raw",
  "datos/processed", 
  "datos/external",
  "scripts",
  "output",
  "output/graficos",
  "output/tablas",
  "output/mapas",
  "doc"
)

# Crear carpetas si no existen
for(carpeta in carpetas) {
  ruta_carpeta <- file.path(proyecto_dir, carpeta)
  if(!dir.exists(ruta_carpeta)) {
    dir.create(ruta_carpeta, recursive = TRUE)
    cat("✓ Carpeta creada:", carpeta, "\n")
  } else {
    cat("✓ Carpeta existe:", carpeta, "\n")
  }
}

# =============================================================================
# CONFIGURACIÓN DE RUTAS
# =============================================================================

# Definir rutas principales
rutas <- list(
  proyecto = proyecto_dir,
  datos_raw = file.path(proyecto_dir, "datos", "raw"),
  datos_procesados = file.path(proyecto_dir, "datos", "processed"),
  datos_externos = file.path(proyecto_dir, "datos", "external"),
  scripts = file.path(proyecto_dir, "scripts"),
  graficos = file.path(proyecto_dir, "output", "graficos"),
  tablas = file.path(proyecto_dir, "output", "tablas"),
  mapas = file.path(proyecto_dir, "output", "mapas")
)

# Mostrar rutas configuradas
cat("\n=== RUTAS CONFIGURADAS ===\n")
for(nombre in names(rutas)) {
  cat(sprintf("%-15s: %s\n", nombre, rutas[[nombre]]))
}

# =============================================================================
# CARGA DE LIBRERÍAS
# =============================================================================

# Lista de paquetes necesarios
paquetes_necesarios <- c(
  # Manipulación de datos
  "tidyverse", "dplyr", "readr", "stringr", "lubridate",
  
  # Visualización
  "ggplot2", "scales", "viridis", "patchwork", "gridExtra",
  
  # Análisis estadístico
  "corrplot", "ggcorrplot", 
  
  # Mapas y datos espaciales
  "sf", "tmap", "leaflet", "RColorBrewer",
  
  # Tablas y reportes
  "knitr", "kableExtra", "DT",
  
  # Otros utiles
  "here", "janitor"
)

# Función para instalar paquetes si no están instalados
instalar_si_necesario <- function(paquetes) {
  nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
  if(length(nuevos) > 0) {
    cat("Instalando paquetes faltantes:", paste(nuevos, collapse = ", "), "\n")
    install.packages(nuevos, dependencies = TRUE)
  }
  
  # Cargar todos los paquetes
  invisible(lapply(paquetes, function(x) {
    suppressPackageStartupMessages(library(x, character.only = TRUE))
    cat("✓", x, "\n")
  }))
}

# Instalar y cargar paquetes
cat("\n=== CARGANDO LIBRERÍAS ===\n")
instalar_si_necesario(paquetes_necesarios)

# =============================================================================
# CONFIGURACIÓN VISUAL Y CONSTANTES
# =============================================================================

# Paleta de colores institucional (similar INDEC)
colores_indec <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#592E83")

# Colores específicos para tasas laborales
colores_tasas <- c(
  "Desocupación" = "#e74c3c", 
  "Empleo" = "#27ae60", 
  "Actividad" = "#3498db"
)

# Colores para comparaciones temporales
colores_años <- c(
  "2016" = "#1f77b4", "2017" = "#ff7f0e", "2018" = "#2ca02c", "2019" = "#d62728",
  "2020" = "#9467bd", "2021" = "#8c564b", "2022" = "#e377c2", "2023" = "#7f7f7f", 
  "2024" = "#bcbd22"
)

# Configuración de ggplot2
theme_set(theme_minimal(base_size = 12))

# Configurar opciones globales
options(
  scipen = 999,  # Evitar notación científica
  digits = 2,    # Número de decimales por defecto
  OutDec = ",",  # Usar coma como separador decimal
  encoding = "UTF-8"
)

# =============================================================================
# CONSTANTES DEL PROYECTO
# =============================================================================

# Códigos de aglomerados GBA
CODIGOS_GBA <- c(32, 33)  # 32 = CABA, 33 = Partidos del GBA

# Período de análisis
ANIO_INICIO <- 2016
ANIO_FIN <- 2024
TRIMESTRES <- 1:4

# Configuración para gráficos
CONFIG_GRAFICOS <- list(
  ancho = 12,
  alto = 8,
  dpi = 300,
  formato = "png"
)

# =============================================================================
# FUNCIONES AUXILIARES DE CONFIGURACIÓN
# =============================================================================

# Función para guardar gráficos con configuración estándar
guardar_grafico <- function(plot, nombre_archivo, carpeta = "graficos", 
                            ancho = CONFIG_GRAFICOS$ancho, 
                            alto = CONFIG_GRAFICOS$alto) {
  
  ruta_completa <- file.path(rutas[[carpeta]], paste0(nombre_archivo, ".png"))
  
  ggsave(
    filename = ruta_completa,
    plot = plot,
    width = ancho,
    height = alto,
    dpi = CONFIG_GRAFICOS$dpi,
    bg = "white"
  )
  
  cat("✓ Gráfico guardado:", ruta_completa, "\n")
  return(ruta_completa)
}

# Función para crear nombres de archivos estandarizados
crear_nombre_archivo <- function(tipo, descripcion, extension = "") {
  fecha <- format(Sys.Date(), "%Y%m%d")
  nombre <- paste(tipo, descripcion, fecha, sep = "_")
  
  if(extension != "") {
    nombre <- paste0(nombre, ".", extension)
  }
  
  return(nombre)
}

# =============================================================================
# VALIDACIÓN DE CONFIGURACIÓN
# =============================================================================

cat("\n=== VALIDACIÓN DE CONFIGURACIÓN ===\n")

# Verificar que todas las carpetas existen
todas_carpetas_ok <- all(sapply(rutas, dir.exists))
cat("Todas las carpetas creadas:", ifelse(todas_carpetas_ok, "✓ SÍ", "✗ NO"), "\n")

# Verificar paquetes cargados
paquetes_cargados <- sapply(paquetes_necesarios, function(x) x %in% (.packages()))
cat("Paquetes cargados correctamente:", sum(paquetes_cargados), "de", length(paquetes_necesarios), "\n")

# Mostrar información de sesión
cat("\n=== INFORMACIÓN DE SESIÓN ===\n")
cat("R version:", R.version.string, "\n")
cat("Fecha y hora:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Directorio de trabajo:", getwd(), "\n")

# =============================================================================
# MENSAJE FINAL
# =============================================================================

cat("\n" + rep("=", 50), "\n")
cat("✅ CONFIGURACIÓN INICIAL COMPLETADA\n")
cat("✅ Estructura de proyecto creada\n")
cat("✅ Librerías cargadas\n") 
cat("✅ Constantes definidas\n")
cat("✅ Funciones auxiliares disponibles\n")
cat(rep("=", 50), "\n")

# Limpiar variables temporales
rm(carpetas, paquetes_necesarios, todas_carpetas_ok, paquetes_cargados)