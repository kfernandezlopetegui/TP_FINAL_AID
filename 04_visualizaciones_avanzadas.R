# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 04_visualizaciones_avanzadas.R - Gráficos estilo INDEC
# =============================================================================

# Verificar que los datos estén cargados
if(!exists("serie_tasas_gba")) {
  cat("🔄 Cargando serie de tasas...\n")
  load(file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.RData"))
}

cat("📊 Creando visualizaciones estilo INDEC...\n")

# =============================================================================
# FUNCIONES DE VISUALIZACIÓN ESTILO INDEC
# =============================================================================

#' Crear gráfico estilo INDEC para un período de años
#' 
#' @param datos Data frame con serie de tasas
#' @param anos_inicio Año de inicio del período
#' @param anos_fin Año de fin del período
#' @param titulo Título del gráfico
crear_grafico_indec <- function(datos, anos_inicio, anos_fin, titulo = NULL) {
  
  # Filtrar datos para el período especificado
  datos_periodo <- datos %>%
    filter(ANO4 >= anos_inicio & ANO4 <= anos_fin)
  
  # Preparar datos para gráfico
  datos_grafico <- datos_periodo %>%
    select(ANO4, TRIMESTRE, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
    pivot_longer(
      cols = starts_with("Tasa_"),
      names_to = "Indicador",
      values_to = "Valor"
    ) %>%
    mutate(
      Indicador = case_when(
        Indicador == "Tasa_Actividad" ~ "Tasa de actividad",
        Indicador == "Tasa_Empleo" ~ "Tasa de empleo",
        Indicador == "Tasa_Desocupacion" ~ "Tasa de desocupación"
      ),
      Año_Trimestre = factor(paste0(ANO4, "\n", "T", TRIMESTRE))
    )
  
  # Título automático si no se especifica
  if(is.null(titulo)) {
    titulo <- paste("Principales tasas del mercado laboral. GBA.", 
                    paste(range(c(anos_inicio, anos_fin)), collapse = "-"))
  }
  
  # Colores estilo INDEC
  colores_indec_tasas <- c(
    "Tasa de actividad" = "#F4B942",  # Amarillo/dorado
    "Tasa de empleo" = "#2E7D57",     # Verde
    "Tasa de desocupación" = "#1F4E79" # Azul oscuro
  )
  
  # Crear gráfico
  p <- datos_grafico %>%
    ggplot(aes(x = Año_Trimestre, y = Valor, fill = Indicador)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
    
    # Agregar valores en las barras
    geom_text(
      aes(label = paste0(round(Valor, 1))),
      position = position_dodge(width = 0.7),
      vjust = -0.3,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # Escalas y colores
    scale_fill_manual(values = colores_indec_tasas) +
    scale_y_continuous(
      limits = c(0, max(datos_grafico$Valor) * 1.1),
      breaks = seq(0, 60, 10),
      expand = c(0, 0)
    ) +
    
    # Etiquetas
    labs(
      title = titulo,
      x = "",
      y = "",
      fill = "",
      caption = "Fuente: EPH - INDEC"
    ) +
    
    # Tema estilo INDEC
    theme_minimal(base_size = 11) +
    theme(
      # Título
      plot.title = element_text(
        size = 13,
        face = "bold",
        hjust = 0,
        margin = margin(b = 20)
      ),
      
      # Leyenda
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.margin = margin(t = 15),
      
      # Ejes
      axis.text.x = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 9),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(size = 0.3),
      
      # Grilla
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", size = 0.3),
      
      # Fondo
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Caption
      plot.caption = element_text(
        size = 8,
        color = "grey60",
        hjust = 1,
        margin = margin(t = 10)
      ),
      
      # Márgenes
      plot.margin = margin(15, 15, 15, 15)
    )
  
  return(p)
}

#' Crear múltiples gráficos por períodos
#' 
#' @param datos Data frame con serie de tasas
#' @param anos_por_grafico Número de años por gráfico
crear_graficos_por_periodos <- function(datos, anos_por_grafico = 4) {
  
  # Obtener rango de años
  anos_disponibles <- sort(unique(datos$ANO4))
  ano_min <- min(anos_disponibles)
  ano_max <- max(anos_disponibles)
  
  # Crear secuencias de períodos
  periodos <- list()
  inicio <- ano_min
  
  while(inicio <= ano_max) {
    fin <- min(inicio + anos_por_grafico - 1, ano_max)
    periodos[[length(periodos) + 1]] <- c(inicio, fin)
    inicio <- fin + 1
  }
  
  # Crear gráficos para cada período
  graficos <- list()
  
  for(i in seq_along(periodos)) {
    periodo <- periodos[[i]]
    
    # Título del período
    titulo <- paste("Principales tasas del mercado laboral. GBA.", 
                    paste(periodo, collapse = "-"))
    
    # Crear gráfico
    grafico <- crear_grafico_indec(datos, periodo[1], periodo[2], titulo)
    
    # Nombre para el archivo
    nombre_archivo <- paste0("tasas_laborales_indec_", periodo[1], "_", periodo[2])
    
    # Guardar gráfico
    guardar_grafico(grafico, nombre_archivo, ancho = 10, alto = 6)
    
    # Almacenar en lista
    graficos[[paste0("periodo_", periodo[1], "_", periodo[2])]] <- grafico
    
    cat("✅ Gráfico creado:", titulo, "\n")
  }
  
  return(graficos)
}

# =============================================================================
# GENERAR GRÁFICOS ESTILO INDEC POR PERÍODOS
# =============================================================================

cat("\n📊 Generando gráficos estilo INDEC por períodos...\n")

# Crear gráficos divididos en períodos de 4 años
graficos_indec <- crear_graficos_por_periodos(serie_tasas_gba, anos_por_grafico = 4)

# Mostrar los gráficos
for(nombre_grafico in names(graficos_indec)) {
  cat("📈 Mostrando:", nombre_grafico, "\n")
  print(graficos_indec[[nombre_grafico]])
  cat("\n")
}

# =============================================================================
# GRÁFICO ESPECÍFICO: ÚLTIMOS 5 AÑOS (2020-2024)
# =============================================================================

cat("📊 Creando gráfico específico para período 2020-2024 (estilo INDEC)...\n")

grafico_2020_2024 <- crear_grafico_indec(
  serie_tasas_gba, 
  2020, 
  2024,
  "Principales tasas del mercado laboral. GBA. 2020-2024"
)

# Guardar y mostrar
guardar_grafico(grafico_2020_2024, "tasas_laborales_indec_2020_2024_destacado", ancho = 10, alto = 6)
print(grafico_2020_2024)

# =============================================================================
# GRÁFICO COMPARATIVO POR TRIMESTRES
# =============================================================================

cat("\n📊 Creando gráfico comparativo por trimestres...\n")

#' Crear gráfico de comparación por trimestres
crear_grafico_trimestres <- function(datos, anos_seleccionados = c(2016, 2019, 2020, 2022, 2024)) {
  
  # Filtrar años seleccionados
  datos_trimestres <- datos %>%
    filter(ANO4 %in% anos_seleccionados) %>%
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
      Trimestre_Label = paste("T", TRIMESTRE, sep = "")
    )
  
  # Colores por año
  colores_anos_sel <- c(
    "2016" = "#1f77b4", "2019" = "#d62728", "2020" = "#9467bd", 
    "2022" = "#e377c2", "2024" = "#bcbd22"
  )
  
  # Crear gráfico
  p <- datos_trimestres %>%
    ggplot(aes(x = Trimestre_Label, y = Valor, color = factor(ANO4), group = ANO4)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 2.5, alpha = 0.9) +
    facet_wrap(~ Indicador, scales = "free_y", ncol = 1) +
    
    scale_color_manual(values = colores_anos_sel[as.character(anos_seleccionados)], name = "Año") +
    
    labs(
      title = "Evolución trimestral de las tasas laborales por año seleccionado",
      subtitle = "Gran Buenos Aires - Años comparados: 2016, 2019, 2020, 2022, 2024",
      x = "Trimestre",
      y = "Tasa (%)",
      caption = "Fuente: EPH - INDEC"
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey40"),
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    
    scale_y_continuous(labels = scales::percent_format(scale = 1))
  
  return(p)
}

# Crear gráfico de trimestres
grafico_trimestres <- crear_grafico_trimestres(serie_tasas_gba)
guardar_grafico(grafico_trimestres, "comparacion_trimestral_anos_seleccionados", alto = 10)
print(grafico_trimestres)

# =============================================================================
# TABLA RESUMIDA ESTILO INDEC
# =============================================================================

cat("\n📋 Creando tabla resumen estilo INDEC...\n")

# Crear tabla para el período 2020-2024
tabla_indec_2020_2024 <- serie_tasas_gba %>%
  filter(ANO4 >= 2020) %>%
  select(ANO4, TRIMESTRE, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
  mutate(
    Periodo = paste0(ANO4, " T", TRIMESTRE),
    `Tasa de actividad` = paste0(round(Tasa_Actividad, 1), "%"),
    `Tasa de empleo` = paste0(round(Tasa_Empleo, 1), "%"),
    `Tasa de desocupación` = paste0(round(Tasa_Desocupacion, 1), "%")
  ) %>%
  select(Periodo, `Tasa de actividad`, `Tasa de empleo`, `Tasa de desocupación`)

# Guardar tabla
write_csv(tabla_indec_2020_2024, file.path(rutas$tablas, "tabla_indec_2020_2024.csv"))

cat("📊 Tabla resumen 2020-2024 (estilo INDEC):\n")
print(tabla_indec_2020_2024)

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ VISUALIZACIONES ESTILO INDEC COMPLETADAS\n")
cat(rep("=", 60), "\n")

cat("📊 GRÁFICOS GENERADOS:\n")
cat("   • Gráficos por períodos (estilo INDEC)\n")
cat("   • Gráfico destacado 2020-2024\n")
cat("   • Comparación trimestral por años\n")

cat("\n📋 CARACTERÍSTICAS DE LOS GRÁFICOS:\n")
cat("   ✓ Barras agrupadas por trimestre\n")
cat("   ✓ Valores mostrados en cada barra\n")
cat("   ✓ Colores institucionales\n")
cat("   ✓ Formato profesional\n")
cat("   ✓ Leyenda horizontal inferior\n")

cat("\n📁 ARCHIVOS GUARDADOS:\n")
for(nombre in names(graficos_indec)) {
  cat("   •", nombre, "\n")
}



cat(rep("=", 60), "\n")
