# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 99_funciones.R - Funciones auxiliares del proyecto
# =============================================================================

# =============================================================================
# FUNCIONES DE CARGA DE DATOS
# =============================================================================

#' Cargar un período específico desde archivos .txt
#' 
#' @param carpeta_periodo Ruta a la carpeta que contiene los archivos del período
#' @return Lista con dos elementos: hogares y personas
cargar_periodo_txt <- function(carpeta_periodo) {
  
  # Buscar archivos de hogares e individuos
  archivos <- list.files(carpeta_periodo, pattern = "\\.txt$", full.names = TRUE)
  
  archivo_hogar <- archivos[grepl("hogar", archivos, ignore.case = TRUE)]
  archivo_individual <- archivos[grepl("individual", archivos, ignore.case = TRUE)]
  
  datos <- list()
  
  # Leer hogares
  if(length(archivo_hogar) > 0) {
    cat("  Leyendo hogares:", basename(archivo_hogar[1]), "\n")
    datos$hogares <- read_delim(archivo_hogar[1], 
                                delim = ";", 
                                locale = locale(encoding = "latin1"),
                                col_types = cols(.default = "c"),
                                show_col_types = FALSE)
  }
  
  # Leer individuos
  if(length(archivo_individual) > 0) {
    cat("  Leyendo personas:", basename(archivo_individual[1]), "\n")
    datos$personas <- read_delim(archivo_individual[1], 
                                 delim = ";", 
                                 locale = locale(encoding = "latin1"),
                                 col_types = cols(.default = "c"),
                                 show_col_types = FALSE)
  }
  
  return(datos)
}

#' Procesar datos para filtrar únicamente GBA
#' 
#' @param datos_periodo Lista con datos de hogares y personas de un período
#' @return Lista con datos filtrados para GBA
procesar_datos_gba <- function(datos_periodo) {
  
  resultado <- list()
  
  # Procesar personas
  if(!is.null(datos_periodo$personas)) {
    resultado$personas <- datos_periodo$personas %>%
      # Convertir variables clave a numéricas
      mutate(
        AGLOMERADO = as.numeric(AGLOMERADO),
        CH06 = as.numeric(CH06),  # Edad
        CH04 = as.numeric(CH04),  # Sexo
        ESTADO = as.numeric(ESTADO),  # Condición de actividad
        PONDERA = as.numeric(PONDERA),  # Ponderador
        NIVEL_ED = as.numeric(NIVEL_ED),  # Nivel educativo
        ANO4 = as.numeric(ANO4),
        TRIMESTRE = as.numeric(TRIMESTRE)
      ) %>%
      # Filtrar por GBA (códigos 32 = CABA, 33 = Partidos del GBA)
      filter(AGLOMERADO %in% CODIGOS_GBA) %>%
      # Filtrar casos válidos
      filter(!is.na(PONDERA) & PONDERA > 0) %>%
      filter(!is.na(CH06))  # Edad válida
  }
  
  # Procesar hogares
  if(!is.null(datos_periodo$hogares)) {
    resultado$hogares <- datos_periodo$hogares %>%
      mutate(
        AGLOMERADO = as.numeric(AGLOMERADO),
        PONDERA = as.numeric(PONDERA),
        ANO4 = as.numeric(ANO4),
        TRIMESTRE = as.numeric(TRIMESTRE)
      ) %>%
      filter(AGLOMERADO %in% CODIGOS_GBA) %>%
      filter(!is.na(PONDERA) & PONDERA > 0)
  }
  
  return(resultado)
}

# =============================================================================
# FUNCIONES DE ANÁLISIS LABORAL
# =============================================================================

#' Calcular tasas laborales para un conjunto de datos
#' 
#' @param datos Data frame con datos de personas
#' @return Data frame con las tasas laborales calculadas
calcular_tasas_laborales <- function(datos) {
  
  # Verificar que existan las variables necesarias
  vars_necesarias <- c("CH06", "ESTADO", "PONDERA")
  if(!all(vars_necesarias %in% names(datos))) {
    stop("Faltan variables necesarias: ", 
         paste(setdiff(vars_necesarias, names(datos)), collapse = ", "))
  }
  
  # Calcular componentes de las tasas laborales
  resumen <- datos %>%
    filter(CH06 >= 10) %>%  # Población en Edad de Trabajar (PET)
    summarise(
      # Población total en edad de trabajar
      PET = sum(PONDERA, na.rm = TRUE),
      
      # Población Económicamente Activa (Ocupados + Desocupados)
      PEA = sum(PONDERA[ESTADO %in% c(1, 2)], na.rm = TRUE),
      
      # Ocupados
      Ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE),
      
      # Desocupados  
      Desocupados = sum(PONDERA[ESTADO == 2], na.rm = TRUE),
      
      # Inactivos
      Inactivos = sum(PONDERA[ESTADO == 3], na.rm = TRUE)
    ) %>%
    mutate(
      # Calcular tasas
      Tasa_Actividad = round((PEA / PET) * 100, 1),
      Tasa_Empleo = round((Ocupados / PET) * 100, 1),
      Tasa_Desocupacion = round((Desocupados / PEA) * 100, 1)
    )
  
  return(resumen)
}

#' Calcular tasas laborales por período
#' 
#' @param datos_lista Lista de datos por período
#' @return Data frame con tasas laborales por año y trimestre
calcular_serie_tasas <- function(datos_lista) {
  
  serie_tasas <- map_dfr(names(datos_lista), function(periodo) {
    
    if(!is.null(datos_lista[[periodo]]$personas)) {
      
      # Extraer año y trimestre del nombre del período (formato AAAA_TX)
      ano <- as.numeric(str_extract(periodo, "^[0-9]{4}"))
      trimestre <- as.numeric(str_extract(periodo, "(?<=_T)[0-9]$"))
      
      # Calcular tasas para este período
      tasas <- calcular_tasas_laborales(datos_lista[[periodo]]$personas)
      
      # Agregar información temporal
      tasas %>%
        mutate(
          ANO4 = ano,
          TRIMESTRE = trimestre,
          Periodo = periodo,
          .before = everything()
        )
    }
  })
  
  return(serie_tasas)
}

# =============================================================================
# FUNCIONES DE VISUALIZACIÓN
# =============================================================================

#' Crear gráfico de barras para tasas laborales por trimestre
#' 
#' @param datos Data frame con tasas laborales por período
#' @param titulo Título del gráfico
#' @return Objeto ggplot
grafico_barras_trimestral <- function(datos, titulo = "Tasas Laborales por Trimestre") {
  
  # Preparar datos para el gráfico
  datos_largo <- datos %>%
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
      Trimestre_Año = paste0(ANO4, "T", TRIMESTRE)
    )
  
  # Crear gráfico
  p <- datos_largo %>%
    ggplot(aes(x = Trimestre_Año, y = Valor, fill = Indicador)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = colores_tasas) +
    labs(
      title = titulo,
      subtitle = "Gran Buenos Aires (CABA + Partidos del GBA)",
      x = "Período",
      y = "Tasa (%)",
      fill = "Indicador",
      caption = "Fuente: EPH - INDEC"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60")
    ) +
    scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(scale = 1))
  
  return(p)
}

#' Crear gráfico de líneas para evolución temporal
#' 
#' @param datos Data frame con tasas laborales por período
#' @param titulo Título del gráfico
#' @return Objeto ggplot
grafico_evolucion_temporal <- function(datos, titulo = "Evolución de Tasas Laborales") {
  
  # Preparar datos
  datos_largo <- datos %>%
    mutate(Fecha = as.Date(paste(ANO4, (TRIMESTRE-1)*3 + 1, "01", sep = "-"))) %>%
    select(Fecha, ANO4, TRIMESTRE, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
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
    )
  
  # Crear gráfico
  p <- datos_largo %>%
    ggplot(aes(x = Fecha, y = Valor, color = Indicador)) +
    geom_line(size = 1.2, alpha = 0.9) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = colores_tasas) +
    labs(
      title = titulo,
      subtitle = "Gran Buenos Aires (CABA + Partidos del GBA)",
      x = "Período",
      y = "Tasa (%)", 
      color = "Indicador",
      caption = "Fuente: EPH - INDEC"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60")
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  return(p)
}

# =============================================================================
# FUNCIONES DE UTILIDADES
# =============================================================================

#' Guardar gráficos con configuración estándar
#' 
#' @param plot Objeto ggplot a guardar
#' @param nombre_archivo Nombre del archivo (sin extensión)
#' @param carpeta Carpeta destino ("graficos", "tablas", "mapas")
#' @param ancho Ancho del gráfico
#' @param alto Alto del gráfico
#' @return Ruta completa del archivo guardado
guardar_grafico <- function(plot, nombre_archivo, carpeta = "graficos", 
                            ancho = 12, alto = 8) {
  
  # Verificar que rutas esté definido
  if(!exists("rutas")) {
    stop("❌ Variable 'rutas' no encontrada. Ejecuta source('scripts/01_setup.R')")
  }
  
  ruta_completa <- file.path(rutas[[carpeta]], paste0(nombre_archivo, ".png"))
  
  ggsave(
    filename = ruta_completa,
    plot = plot,
    width = ancho,
    height = alto,
    dpi = 300,
    bg = "white"
  )
  
  cat("✓ Gráfico guardado:", ruta_completa, "\n")
  return(ruta_completa)
}

#' Crear nombres de archivos estandarizados
#' 
#' @param tipo Tipo de archivo
#' @param descripcion Descripción del contenido
#' @param extension Extensión del archivo
#' @return Nombre de archivo estandarizado
crear_nombre_archivo <- function(tipo, descripcion, extension = "") {
  fecha <- format(Sys.Date(), "%Y%m%d")
  nombre <- paste(tipo, descripcion, fecha, sep = "_")
  
  if(extension != "") {
    nombre <- paste0(nombre, ".", extension)
  }
  
  return(nombre)
}

#' Verificar estructura de datos cargados
#' 
#' @param datos_lista Lista de datos por período
#' @return Data frame con resumen de la estructura
verificar_estructura_datos <- function(datos_lista) {
  
  resumen <- map_dfr(names(datos_lista), function(periodo) {
    
    tibble(
      Periodo = periodo,
      Personas_Filas = ifelse(!is.null(datos_lista[[periodo]]$personas), 
                              nrow(datos_lista[[periodo]]$personas), 0),
      Personas_Columnas = ifelse(!is.null(datos_lista[[periodo]]$personas), 
                                 ncol(datos_lista[[periodo]]$personas), 0),
      Hogares_Filas = ifelse(!is.null(datos_lista[[periodo]]$hogares), 
                             nrow(datos_lista[[periodo]]$hogares), 0),
      Hogares_Columnas = ifelse(!is.null(datos_lista[[periodo]]$hogares), 
                                ncol(datos_lista[[periodo]]$hogares), 0)
    )
  })
  
  return(resumen)
}

#' Mostrar resumen de tasas laborales
#' 
#' @param serie_tasas Data frame con serie de tasas laborales
#' @return Data frame con estadísticas descriptivas
resumen_tasas_laborales <- function(serie_tasas) {
  
  resumen <- serie_tasas %>%
    summarise(
      across(c(Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion), 
             list(
               min = ~ round(min(.x, na.rm = TRUE), 1),
               max = ~ round(max(.x, na.rm = TRUE), 1),
               media = ~ round(mean(.x, na.rm = TRUE), 1),
               mediana = ~ round(median(.x, na.rm = TRUE), 1),
               desvio = ~ round(sd(.x, na.rm = TRUE), 1)
             ),
             .names = "{.col}_{.fn}")
    )
  
  return(resumen)
}

# =============================================================================
# MENSAJE DE CARGA
# =============================================================================

cat("✅ Funciones auxiliares cargadas correctamente\n")
cat("✅ Funciones disponibles:\n")
cat("   - cargar_periodo_txt()\n")
cat("   - procesar_datos_gba()\n") 
cat("   - calcular_tasas_laborales()\n")
cat("   - calcular_serie_tasas()\n")
cat("   - grafico_barras_trimestral()\n")
cat("   - grafico_evolucion_temporal()\n")
cat("   - guardar_grafico()\n")
cat("   - crear_nombre_archivo()\n")
cat("   - verificar_estructura_datos()\n")
cat("   - resumen_tasas_laborales()\n")