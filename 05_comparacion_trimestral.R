# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 05_comparacion_trimestral.R - Comparaciones trimestrales estilo INDEC
# =============================================================================

# Verificar que los datos estén cargados
if(!exists("serie_tasas_gba")) {
  cat("🔄 Cargando serie de tasas...\n")
  load(file.path(rutas$datos_procesados, "serie_tasas_gba_2016_2024.RData"))
}

cat("📊 Creando comparaciones trimestrales estilo INDEC...\n")

# =============================================================================
# FUNCIÓN PARA COMPARACIÓN TRIMESTRAL
# =============================================================================

#' Crear gráfico de comparación para un trimestre específico
#' 
#' @param datos Data frame con serie de tasas
#' @param trimestre_num Número del trimestre (1, 2, 3, 4)
#' @param periodo1_inicio Año de inicio del primer período
#' @param periodo1_fin Año de fin del primer período
#' @param periodo2_inicio Año de inicio del segundo período  
#' @param periodo2_fin Año de fin del segundo período
#' @param etiqueta_periodo1 Etiqueta para el primer período
#' @param etiqueta_periodo2 Etiqueta para el segundo período
crear_comparacion_trimestral <- function(datos, trimestre_num, 
                                         periodo1_inicio, periodo1_fin,
                                         periodo2_inicio, periodo2_fin,
                                         etiqueta_periodo1 = NULL,
                                         etiqueta_periodo2 = NULL) {
  
  # Etiquetas automáticas si no se especifican
  if(is.null(etiqueta_periodo1)) {
    etiqueta_periodo1 <- paste0(periodo1_inicio, "-", periodo1_fin)
  }
  if(is.null(etiqueta_periodo2)) {
    etiqueta_periodo2 <- paste0(periodo2_inicio, "-", periodo2_fin)
  }
  
  # Filtrar datos para el trimestre específico
  datos_comparacion <- datos %>%
    filter(TRIMESTRE == trimestre_num) %>%
    mutate(
      Periodo_Grupo = case_when(
        ANO4 >= periodo1_inicio & ANO4 <= periodo1_fin ~ etiqueta_periodo1,
        ANO4 >= periodo2_inicio & ANO4 <= periodo2_fin ~ etiqueta_periodo2,
        TRUE ~ "Otro"
      )
    ) %>%
    filter(Periodo_Grupo != "Otro") %>%
    select(ANO4, Periodo_Grupo, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
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
      Año_Label = as.character(ANO4)
    )
  
  # Colores estilo INDEC
  colores_indec_tasas <- c(
    "Tasa de actividad" = "#F4B942",
    "Tasa de empleo" = "#2E7D57",
    "Tasa de desocupación" = "#1F4E79"
  )
  
  # Crear gráfico
  p <- datos_comparacion %>%
    ggplot(aes(x = Año_Label, y = Valor, fill = Indicador)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
    
    # Agregar valores en las barras
    geom_text(
      aes(label = paste0(round(Valor, 1))),
      position = position_dodge(width = 0.7),
      vjust = -0.3,
      size = 3,
      fontface = "bold"
    ) +
    
    # Separar por períodos
    facet_wrap(~ Periodo_Grupo, scales = "free_x", ncol = 2) +
    
    # Escalas y colores
    scale_fill_manual(values = colores_indec_tasas) +
    scale_y_continuous(
      limits = c(0, max(datos_comparacion$Valor) * 1.15),
      breaks = seq(0, 60, 10),
      expand = c(0, 0)
    ) +
    
    # Etiquetas
    labs(
      title = paste0("Comparación Trimestre ", trimestre_num, " - Tasas del mercado laboral GBA"),
      subtitle = paste("Comparación entre períodos:", etiqueta_periodo1, "vs", etiqueta_periodo2),
      x = "Año",
      y = "Tasa (%)",
      fill = "",
      caption = "Fuente: EPH - INDEC"
    ) +
    
    # Tema estilo INDEC
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, color = "grey40", margin = margin(b = 15)),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      strip.text = element_text(size = 11, face = "bold", margin = margin(b = 10)),
      axis.text.x = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 9),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", size = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.caption = element_text(size = 8, color = "grey60", hjust = 1, margin = margin(t = 10)),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  return(p)
}

# =============================================================================
# GENERAR COMPARACIONES PARA TODOS LOS TRIMESTRES
# =============================================================================

cat("\n📊 Generando comparaciones trimestrales: 2016-2019 vs 2020-2024...\n")

# Crear comparaciones para cada trimestre
for(trim in 1:4) {
  
  cat("📈 Creando comparación para Trimestre", trim, "\n")
  
  # Crear gráfico de comparación
  grafico_comp <- crear_comparacion_trimestral(
    datos = serie_tasas_gba,
    trimestre_num = trim,
    periodo1_inicio = 2016,
    periodo1_fin = 2019, 
    periodo2_inicio = 2020,
    periodo2_fin = 2024,
    etiqueta_periodo1 = "2016-2019\n(Pre-pandemia)",
    etiqueta_periodo2 = "2020-2024\n(Pandemia y recuperación)"
  )
  
  # Nombre del archivo
  nombre_archivo <- paste0("comparacion_trimestre_", trim, "_pre_post_pandemia")
  
  # Guardar gráfico
  guardar_grafico(grafico_comp, nombre_archivo, ancho = 12, alto = 7)
  
  # Mostrar gráfico
  print(grafico_comp)
  cat("\n")
}

# =============================================================================
# COMPARACIÓN ALTERNATIVA: TRES PERÍODOS
# =============================================================================

cat("📊 Generando comparaciones con tres períodos...\n")

#' Crear comparación con tres períodos
crear_comparacion_tres_periodos <- function(datos, trimestre_num) {
  
  # Filtrar y clasificar por períodos
  datos_tres_periodos <- datos %>%
    filter(TRIMESTRE == trimestre_num) %>%
    mutate(
      Periodo_Grupo = case_when(
        ANO4 >= 2016 & ANO4 <= 2018 ~ "2016-2018\n(Estabilidad)",
        ANO4 >= 2019 & ANO4 <= 2021 ~ "2019-2021\n(Crisis y pandemia)",
        ANO4 >= 2022 & ANO4 <= 2024 ~ "2022-2024\n(Recuperación)",
        TRUE ~ "Otro"
      )
    ) %>%
    filter(Periodo_Grupo != "Otro") %>%
    select(ANO4, Periodo_Grupo, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
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
      Año_Label = as.character(ANO4)
    )
  
  # Colores para tres períodos
  colores_tres_periodos <- c(
    "Actividad" = "#F4B942",
    "Empleo" = "#2E7D57", 
    "Desocupación" = "#1F4E79"
  )
  
  # Crear gráfico
  p <- datos_tres_periodos %>%
    ggplot(aes(x = Año_Label, y = Valor, fill = Indicador)) +
    geom_col(position = "dodge", width = 0.6, alpha = 0.9) +
    
    geom_text(
      aes(label = paste0(round(Valor, 1))),
      position = position_dodge(width = 0.6),
      vjust = -0.3,
      size = 2.8,
      fontface = "bold"
    ) +
    
    facet_wrap(~ Periodo_Grupo, scales = "free_x", ncol = 3) +
    
    scale_fill_manual(values = colores_tres_periodos) +
    scale_y_continuous(
      limits = c(0, max(datos_tres_periodos$Valor) * 1.15),
      breaks = seq(0, 60, 10),
      expand = c(0, 0)
    ) +
    
    labs(
      title = paste0("Evolución Trimestre ", trimestre_num, " por ciclos económicos - GBA"),
      subtitle = "Comparación entre períodos: Pre-crisis, Crisis-Pandemia y Recuperación",
      x = "Año",
      y = "Tasa (%)",
      fill = "",
      caption = "Fuente: EPH - INDEC"
    ) +
    
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  return(p)
}

# Crear comparaciones de tres períodos solo para T1 y T3 (representativos)
for(trim in c(1, 3)) {
  
  cat("📈 Creando comparación tres períodos para Trimestre", trim, "\n")
  
  grafico_tres <- crear_comparacion_tres_periodos(serie_tasas_gba, trim)
  nombre_archivo <- paste0("comparacion_tres_periodos_trimestre_", trim)
  
  guardar_grafico(grafico_tres, nombre_archivo, ancho = 14, alto = 6)
  print(grafico_tres)
  cat("\n")
}

# =============================================================================
# TABLA RESUMEN POR TRIMESTRES
# =============================================================================

cat("📋 Creando tabla resumen por trimestres...\n")

# Crear tabla resumen de promedios por trimestre y período
tabla_resumen_trimestres <- serie_tasas_gba %>%
  mutate(
    Periodo = case_when(
      ANO4 >= 2016 & ANO4 <= 2019 ~ "2016-2019",
      ANO4 >= 2020 & ANO4 <= 2024 ~ "2020-2024",
      TRUE ~ "Otro"
    )
  ) %>%
  filter(Periodo != "Otro") %>%
  group_by(Periodo, TRIMESTRE) %>%
  summarise(
    Tasa_Actividad_Prom = round(mean(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Empleo_Prom = round(mean(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Desocup_Prom = round(mean(Tasa_Desocupacion, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    Trimestre_Label = paste("T", TRIMESTRE, sep = "")
  ) %>%
  select(Periodo, Trimestre_Label, everything(), -TRIMESTRE)

# Guardar tabla
write_csv(tabla_resumen_trimestres, file.path(rutas$tablas, "resumen_trimestral_por_periodos.csv"))

cat("📊 Resumen promedio por trimestres y períodos:\n")
print(tabla_resumen_trimestres)

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ COMPARACIONES TRIMESTRALES COMPLETADAS\n")
cat(rep("=", 60), "\n")

cat("📊 GRÁFICOS GENERADOS:\n")
cat("   • 4 comparaciones trimestrales (2016-2019 vs 2020-2024)\n")
cat("   • 2 comparaciones de tres períodos (T1 y T3)\n")

cat("\n🎯 ANÁLISIS DESTACADOS:\n")
cat("   • Impacto de la pandemia por trimestre\n")
cat("   • Patrones estacionales pre y post crisis\n")
cat("   • Ciclos económicos diferenciados\n")



cat(rep("=", 60), "\n")
