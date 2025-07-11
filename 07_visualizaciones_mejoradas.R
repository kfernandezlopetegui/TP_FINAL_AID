# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 07_visualizaciones_mejoradas.R - Gráficos separados y heatmaps
# =============================================================================

# Cargar datos si no están disponibles
if(!exists("tasas_por_edad")) {
  cat("🔄 Cargando datos multivariados...\n")
  load(file.path(rutas$datos_procesados, "tasas_por_edad.RData"))
  load(file.path(rutas$datos_procesados, "tasas_por_sexo.RData"))
}

cat("📊 Creando visualizaciones mejoradas...\n")

# =============================================================================
# FUNCIONES PARA VISUALIZACIONES MEJORADAS
# =============================================================================

#' Crear gráfico de líneas para un indicador específico
crear_grafico_separado <- function(datos, variable_grupo, indicador, titulo) {
  
  # Filtrar solo el indicador específico
  col_indicador <- paste0("Tasa_", indicador)
  
  datos_grafico <- datos %>%
    mutate(Fecha = as.Date(paste(ANO4, (TRIMESTRE-1)*3 + 1, "01", sep = "-"))) %>%
    select(Fecha, all_of(variable_grupo), all_of(col_indicador)) %>%
    rename(Valor = all_of(col_indicador))
  
  # Colores diferenciados por grupo
  if(variable_grupo == "Grupo_Edad") {
    colores <- c(
      "18-24 años" = "#e74c3c",      # Rojo - jóvenes
      "25-34 años" = "#f39c12",      # Naranja
      "35-49 años" = "#27ae60",      # Verde
      "50-64 años" = "#3498db"       # Azul - mayores
    )
  } else if(variable_grupo == "Sexo") {
    colores <- c(
      "Mujer" = "#e91e63",           # Rosa
      "Varón" = "#2196f3"            # Azul
    )
  }
  
  # Crear gráfico
  p <- datos_grafico %>%
    ggplot(aes(x = Fecha, y = Valor, color = !!sym(variable_grupo))) +
    geom_line(size = 1.5, alpha = 0.9) +
    geom_point(size = 2, alpha = 0.8) +
    
    scale_color_manual(values = colores) +
    
    labs(
      title = titulo,
      subtitle = "Gran Buenos Aires - 2016-2024",
      x = "Período",
      y = paste("Tasa de", tolower(indicador), "(%)"),
      color = str_to_title(str_replace(variable_grupo, "_", " ")),
      caption = "Fuente: EPH - INDEC"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 15)),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.5)
    ) +
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::number_format(suffix = "%"))
  
  return(p)
}

#' Crear heatmap para un indicador
crear_heatmap <- function(datos, variable_grupo, indicador, titulo) {
  
  # Preparar datos para heatmap
  col_indicador <- paste0("Tasa_", indicador)
  
  datos_heatmap <- datos %>%
    group_by(ANO4, !!sym(variable_grupo)) %>%
    summarise(
      Valor = mean(!!sym(col_indicador), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Valor_Redondeado = round(Valor, 1)
    )
  
  # Crear heatmap
  p <- datos_heatmap %>%
    ggplot(aes(x = factor(ANO4), y = !!sym(variable_grupo), fill = Valor)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = Valor_Redondeado), color = "white", size = 3.5, fontface = "bold") +
    
    scale_fill_viridis_c(
      name = paste("Tasa de\n", tolower(indicador), " (%)"),
      option = "plasma",
      direction = -1
    ) +
    
    labs(
      title = titulo,
      subtitle = "Promedio anual por grupo - Gran Buenos Aires",
      x = "Año",
      y = str_to_title(str_replace(variable_grupo, "_", " ")),
      caption = "Fuente: EPH - INDEC"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey40"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}

# =============================================================================
# GRÁFICOS SEPARADOS POR GRUPO ETARIO
# =============================================================================

cat("\n📅 Creando gráficos separados por grupo etario...\n")

# Filtrar grupos principales
grupos_principales <- c("18-24 años", "25-34 años", "35-49 años", "50-64 años")
tasas_edad_principales <- tasas_por_edad %>%
  filter(Grupo_Edad %in% grupos_principales)

# 1. Gráfico de Actividad por edad
grafico_actividad_edad <- crear_grafico_separado(
  tasas_edad_principales, 
  "Grupo_Edad", 
  "Actividad",
  "Evolución de la Tasa de Actividad por Grupo Etario"
)

guardar_grafico(grafico_actividad_edad, "actividad_por_edad_separado", ancho = 12, alto = 7)
print(grafico_actividad_edad)

# 2. Gráfico de Empleo por edad
grafico_empleo_edad <- crear_grafico_separado(
  tasas_edad_principales,
  "Grupo_Edad",
  "Empleo", 
  "Evolución de la Tasa de Empleo por Grupo Etario"
)

guardar_grafico(grafico_empleo_edad, "empleo_por_edad_separado", ancho = 12, alto = 7)
print(grafico_empleo_edad)

# 3. Gráfico de Desocupación por edad
grafico_desocupacion_edad <- crear_grafico_separado(
  tasas_edad_principales,
  "Grupo_Edad",
  "Desocupacion",
  "Evolución de la Tasa de Desocupación por Grupo Etario"
)

guardar_grafico(grafico_desocupacion_edad, "desocupacion_por_edad_separado", ancho = 12, alto = 7)
print(grafico_desocupacion_edad)

# 4. Heatmap de Desocupación por edad
heatmap_desocupacion_edad <- crear_heatmap(
  tasas_edad_principales,
  "Grupo_Edad",
  "Desocupacion",
  "Heatmap: Tasa de Desocupación por Grupo Etario y Año"
)

guardar_grafico(heatmap_desocupacion_edad, "heatmap_desocupacion_edad", ancho = 10, alto = 6)
print(heatmap_desocupacion_edad)

# =============================================================================
# GRÁFICOS SEPARADOS POR SEXO
# =============================================================================

cat("\n👥 Creando gráficos separados por sexo...\n")

# 1. Gráfico de Actividad por sexo
grafico_actividad_sexo <- crear_grafico_separado(
  tasas_por_sexo,
  "Sexo",
  "Actividad", 
  "Evolución de la Tasa de Actividad por Sexo"
)

guardar_grafico(grafico_actividad_sexo, "actividad_por_sexo_separado", ancho = 12, alto = 7)
print(grafico_actividad_sexo)

# 2. Gráfico de Empleo por sexo
grafico_empleo_sexo <- crear_grafico_separado(
  tasas_por_sexo,
  "Sexo",
  "Empleo",
  "Evolución de la Tasa de Empleo por Sexo"
)

guardar_grafico(grafico_empleo_sexo, "empleo_por_sexo_separado", ancho = 12, alto = 7)
print(grafico_empleo_sexo)

# 3. Gráfico de Desocupación por sexo
grafico_desocupacion_sexo <- crear_grafico_separado(
  tasas_por_sexo,
  "Sexo", 
  "Desocupacion",
  "Evolución de la Tasa de Desocupación por Sexo"
)

guardar_grafico(grafico_desocupacion_sexo, "desocupacion_por_sexo_separado", ancho = 12, alto = 7)
print(grafico_desocupacion_sexo)

# 4. Heatmap de Desocupación por sexo
heatmap_desocupacion_sexo <- crear_heatmap(
  tasas_por_sexo,
  "Sexo",
  "Desocupacion",
  "Heatmap: Tasa de Desocupación por Sexo y Año"
)

guardar_grafico(heatmap_desocupacion_sexo, "heatmap_desocupacion_sexo", ancho = 10, alto = 5)
print(heatmap_desocupacion_sexo)

# =============================================================================
# GRÁFICO COMPARATIVO DE BRECHAS
# =============================================================================

cat("\n📊 Creando gráficos de brechas...\n")

# Calcular brechas por edad (jóvenes vs mayores)
brechas_edad <- tasas_edad_principales %>%
  filter(Grupo_Edad %in% c("18-24 años", "50-64 años")) %>%
  select(ANO4, TRIMESTRE, Grupo_Edad, Tasa_Desocupacion) %>%
  pivot_wider(names_from = Grupo_Edad, values_from = Tasa_Desocupacion) %>%
  mutate(
    Brecha_Generacional = `18-24 años` - `50-64 años`,
    Fecha = as.Date(paste(ANO4, (TRIMESTRE-1)*3 + 1, "01", sep = "-"))
  )

# Calcular brechas por sexo
brechas_sexo <- tasas_por_sexo %>%
  select(ANO4, TRIMESTRE, Sexo, Tasa_Desocupacion) %>%
  pivot_wider(names_from = Sexo, values_from = Tasa_Desocupacion) %>%
  mutate(
    Brecha_Genero = Mujer - Varón,
    Fecha = as.Date(paste(ANO4, (TRIMESTRE-1)*3 + 1, "01", sep = "-"))
  )

# Gráfico de brechas
grafico_brechas <- ggplot() +
  geom_line(data = brechas_edad, aes(x = Fecha, y = Brecha_Generacional, color = "Jóvenes vs Mayores"), 
            size = 1.5, alpha = 0.8) +
  geom_line(data = brechas_sexo, aes(x = Fecha, y = Brecha_Genero, color = "Mujeres vs Varones"), 
            size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  scale_color_manual(
    values = c("Jóvenes vs Mayores" = "#e74c3c", "Mujeres vs Varones" = "#e91e63"),
    name = "Brecha en Desocupación"
  ) +
  
  labs(
    title = "Evolución de las Brechas en Desocupación",
    subtitle = "Diferencias en puntos porcentuales - Gran Buenos Aires 2016-2024",
    x = "Período",
    y = "Diferencia en Tasa de Desocupación (puntos porcentuales)",
    caption = "Fuente: EPH - INDEC\nNota: Valores positivos indican mayor desocupación del primer grupo"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

guardar_grafico(grafico_brechas, "evolucion_brechas_desocupacion", ancho = 12, alto = 7)
print(grafico_brechas)

# =============================================================================
# TABLAS RESUMEN MEJORADAS
# =============================================================================

cat("\n📋 Creando tablas resumen...\n")

# Tabla de máximos y mínimos por grupo etario
tabla_extremos_edad <- tasas_edad_principales %>%
  group_by(Grupo_Edad) %>%
  summarise(
    Desocup_Max = max(Tasa_Desocupacion, na.rm = TRUE),
    Año_Max = ANO4[which.max(Tasa_Desocupacion)],
    Trim_Max = TRIMESTRE[which.max(Tasa_Desocupacion)],
    Desocup_Min = min(Tasa_Desocupacion, na.rm = TRUE),
    Año_Min = ANO4[which.min(Tasa_Desocupacion)],
    Trim_Min = TRIMESTRE[which.min(Tasa_Desocupacion)],
    .groups = "drop"
  ) %>%
  mutate(
    Periodo_Max = paste0(Año_Max, "T", Trim_Max),
    Periodo_Min = paste0(Año_Min, "T", Trim_Min)
  ) %>%
  select(Grupo_Edad, Desocup_Max, Periodo_Max, Desocup_Min, Periodo_Min)

cat("📊 Extremos de desocupación por grupo etario:\n")
print(tabla_extremos_edad)

# Guardar tabla
write_csv(tabla_extremos_edad, file.path(rutas$tablas, "extremos_desocupacion_edad.csv"))

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ VISUALIZACIONES MEJORADAS COMPLETADAS\n")
cat(rep("=", 60), "\n")

cat("📊 GRÁFICOS GENERADOS:\n")
cat("   • 3 gráficos separados por grupo etario (Actividad, Empleo, Desocupación)\n")
cat("   • 3 gráficos separados por sexo (Actividad, Empleo, Desocupación)\n")
cat("   • 2 heatmaps de desocupación (edad y sexo)\n")
cat("   • 1 gráfico de evolución de brechas\n")





cat(rep("=", 60), "\n")
