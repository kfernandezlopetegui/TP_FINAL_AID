# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 06_analisis_multivariado.R - Análisis por sexo, edad y educación
# =============================================================================

# Verificar que los datos estén cargados
if(!exists("datos_gba")) {
  cat("🔄 Cargando datos procesados...\n")
  load(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))
}

cat("📊 Iniciando análisis multivariado...\n")

# =============================================================================
# FUNCIONES PARA ANÁLISIS MULTIVARIADO
# =============================================================================

#' Calcular tasas laborales por subgrupos
#' 
#' @param datos Data frame de personas
#' @param variables_agrupacion Variables para agrupar
calcular_tasas_por_grupos <- function(datos, variables_agrupacion) {
  
  # Crear expresión de agrupación
  agrupacion <- syms(variables_agrupacion)
  
  # Calcular tasas
  resultado <- datos %>%
    filter(CH06 >= 10) %>%  # PET
    group_by(ANO4, TRIMESTRE, !!!agrupacion) %>%
    summarise(
      PET = sum(PONDERA, na.rm = TRUE),
      PEA = sum(PONDERA[ESTADO %in% c(1, 2)], na.rm = TRUE),
      Ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE),
      Desocupados = sum(PONDERA[ESTADO == 2], na.rm = TRUE),
      Inactivos = sum(PONDERA[ESTADO == 3], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Tasa_Actividad = round((PEA / PET) * 100, 1),
      Tasa_Empleo = round((Ocupados / PET) * 100, 1),
      Tasa_Desocupacion = round((Desocupados / PEA) * 100, 1)
    ) %>%
    filter(PET > 0)  # Eliminar grupos sin población
  
  return(resultado)
}

#' Crear gráfico de evolución por grupos
crear_grafico_evolucion_grupos <- function(datos, variable_grupo, titulo) {
  
  # Preparar datos para el gráfico
  datos_grafico <- datos %>%
    mutate(Fecha = as.Date(paste(ANO4, (TRIMESTRE-1)*3 + 1, "01", sep = "-"))) %>%
    select(Fecha, all_of(variable_grupo), Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion) %>%
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
  p <- datos_grafico %>%
    ggplot(aes(x = Fecha, y = Valor, color = !!sym(variable_grupo))) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 1.5, alpha = 0.7) +
    facet_wrap(~ Indicador, scales = "free_y", ncol = 1) +
    
    labs(
      title = titulo,
      subtitle = "Gran Buenos Aires - 2016-2024",
      x = "Período",
      y = "Tasa (%)",
      color = str_to_title(variable_grupo),
      caption = "Fuente: EPH - INDEC"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey40"),
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  return(p)
}

# =============================================================================
# ANÁLISIS POR SEXO
# =============================================================================

cat("\n👥 Análisis por sexo...\n")

# Preparar datos con etiquetas de sexo
datos_con_sexo <- map_dfr(names(datos_gba), function(periodo) {
  if(!is.null(datos_gba[[periodo]]$personas)) {
    datos_gba[[periodo]]$personas %>%
      mutate(
        Sexo = case_when(
          CH04 == 1 ~ "Varón",
          CH04 == 2 ~ "Mujer",
          TRUE ~ "No especificado"
        )
      ) %>%
      filter(Sexo != "No especificado")
  }
})

# Calcular tasas por sexo
tasas_por_sexo <- calcular_tasas_por_grupos(datos_con_sexo, "Sexo")

cat("✅ Tasas calculadas por sexo para", nrow(tasas_por_sexo), "registros\n")

# Crear gráfico de evolución por sexo
grafico_sexo <- crear_grafico_evolucion_grupos(
  tasas_por_sexo, 
  "Sexo", 
  "Evolución de tasas laborales por sexo"
)

guardar_grafico(grafico_sexo, "evolucion_tasas_por_sexo", alto = 10)
print(grafico_sexo)

# Tabla resumen por sexo (últimos 5 años)
tabla_sexo_reciente <- tasas_por_sexo %>%
  filter(ANO4 >= 2020) %>%
  group_by(Sexo) %>%
  summarise(
    Tasa_Actividad_Prom = round(mean(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Empleo_Prom = round(mean(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Desocup_Prom = round(mean(Tasa_Desocupacion, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n📊 Promedios por sexo (2020-2024):\n")
print(tabla_sexo_reciente)

# =============================================================================
# ANÁLISIS POR GRUPOS DE EDAD
# =============================================================================

cat("\n📅 Análisis por grupos de edad...\n")

# Preparar datos con grupos etarios
datos_con_edad <- map_dfr(names(datos_gba), function(periodo) {
  if(!is.null(datos_gba[[periodo]]$personas)) {
    datos_gba[[periodo]]$personas %>%
      mutate(
        Grupo_Edad = case_when(
          CH06 >= 10 & CH06 <= 17 ~ "10-17 años",
          CH06 >= 18 & CH06 <= 24 ~ "18-24 años", 
          CH06 >= 25 & CH06 <= 34 ~ "25-34 años",
          CH06 >= 35 & CH06 <= 49 ~ "35-49 años",
          CH06 >= 50 & CH06 <= 64 ~ "50-64 años",
          CH06 >= 65 ~ "65+ años",
          TRUE ~ "No especificado"
        )
      ) %>%
      filter(Grupo_Edad != "No especificado")
  }
})

# Calcular tasas por grupo de edad
tasas_por_edad <- calcular_tasas_por_grupos(datos_con_edad, "Grupo_Edad")

cat("✅ Tasas calculadas por edad para", nrow(tasas_por_edad), "registros\n")

# Crear gráfico de evolución por edad (solo grupos principales)
grupos_principales <- c("18-24 años", "25-34 años", "35-49 años", "50-64 años")

grafico_edad <- tasas_por_edad %>%
  filter(Grupo_Edad %in% grupos_principales) %>%
  crear_grafico_evolucion_grupos("Grupo_Edad", "Evolución de tasas laborales por grupo etario")

guardar_grafico(grafico_edad, "evolucion_tasas_por_edad", alto = 10)
print(grafico_edad)

# Tabla resumen por edad (período reciente)
tabla_edad_reciente <- tasas_por_edad %>%
  filter(ANO4 >= 2020, Grupo_Edad %in% grupos_principales) %>%
  group_by(Grupo_Edad) %>%
  summarise(
    Tasa_Actividad_Prom = round(mean(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Empleo_Prom = round(mean(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Desocup_Prom = round(mean(Tasa_Desocupacion, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n📊 Promedios por grupo etario (2020-2024):\n")
print(tabla_edad_reciente)

# =============================================================================
# ANÁLISIS POR NIVEL EDUCATIVO
# =============================================================================

cat("\n🎓 Análisis por nivel educativo...\n")

# Preparar datos con nivel educativo
datos_con_educacion <- map_dfr(names(datos_gba), function(periodo) {
  if(!is.null(datos_gba[[periodo]]$personas)) {
    datos_gba[[periodo]]$personas %>%
      mutate(
        Nivel_Educativo = case_when(
          NIVEL_ED == 1 ~ "Sin instrucción",
          NIVEL_ED == 2 ~ "Primaria incompleta",
          NIVEL_ED == 3 ~ "Primaria completa", 
          NIVEL_ED == 4 ~ "Secundaria incompleta",
          NIVEL_ED == 5 ~ "Secundaria completa",
          NIVEL_ED == 6 ~ "Superior/Universitario incompleto",
          NIVEL_ED == 7 ~ "Superior/Universitario completo",
          TRUE ~ "No especificado"
        ),
        Nivel_Ed_Agrupado = case_when(
          NIVEL_ED %in% c(1, 2, 3) ~ "Hasta primaria",
          NIVEL_ED %in% c(4, 5) ~ "Secundaria",
          NIVEL_ED %in% c(6, 7) ~ "Superior/Universitario",
          TRUE ~ "No especificado"
        )
      ) %>%
      filter(Nivel_Ed_Agrupado != "No especificado")
  }
})

# Calcular tasas por nivel educativo
tasas_por_educacion <- calcular_tasas_por_grupos(datos_con_educacion, "Nivel_Ed_Agrupado")

cat("✅ Tasas calculadas por educación para", nrow(tasas_por_educacion), "registros\n")

# Crear gráfico de evolución por educación
grafico_educacion <- crear_grafico_evolucion_grupos(
  tasas_por_educacion, 
  "Nivel_Ed_Agrupado", 
  "Evolución de tasas laborales por nivel educativo"
)

guardar_grafico(grafico_educacion, "evolucion_tasas_por_educacion", alto = 10)
print(grafico_educacion)

# Tabla resumen por educación
tabla_educacion_reciente <- tasas_por_educacion %>%
  filter(ANO4 >= 2020) %>%
  group_by(Nivel_Ed_Agrupado) %>%
  summarise(
    Tasa_Actividad_Prom = round(mean(Tasa_Actividad, na.rm = TRUE), 1),
    Tasa_Empleo_Prom = round(mean(Tasa_Empleo, na.rm = TRUE), 1),
    Tasa_Desocup_Prom = round(mean(Tasa_Desocupacion, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n📊 Promedios por nivel educativo (2020-2024):\n")
print(tabla_educacion_reciente)

# =============================================================================
# ANÁLISIS COMBINADO: SEXO Y EDAD
# =============================================================================

cat("\n👥📅 Análisis combinado: sexo y edad...\n")

# Preparar datos con sexo y edad combinados
datos_sexo_edad <- map_dfr(names(datos_gba), function(periodo) {
  if(!is.null(datos_gba[[periodo]]$personas)) {
    datos_gba[[periodo]]$personas %>%
      mutate(
        Sexo = case_when(
          CH04 == 1 ~ "Varón",
          CH04 == 2 ~ "Mujer",
          TRUE ~ "No especificado"
        ),
        Grupo_Edad_Simple = case_when(
          CH06 >= 18 & CH06 <= 29 ~ "Jóvenes (18-29)",
          CH06 >= 30 & CH06 <= 49 ~ "Adultos (30-49)",
          CH06 >= 50 & CH06 <= 64 ~ "Adultos mayores (50-64)",
          TRUE ~ "Otros"
        )
      ) %>%
      filter(Sexo != "No especificado", Grupo_Edad_Simple != "Otros")
  }
})

# Calcular tasas por sexo y edad
tasas_sexo_edad <- calcular_tasas_por_grupos(datos_sexo_edad, c("Sexo", "Grupo_Edad_Simple"))

# Crear gráfico de barras comparativo para período reciente
datos_barras_sexo_edad <- tasas_sexo_edad %>%
  filter(ANO4 >= 2022) %>%  # Solo últimos años para claridad
  group_by(Sexo, Grupo_Edad_Simple) %>%
  summarise(
    Tasa_Actividad = mean(Tasa_Actividad, na.rm = TRUE),
    Tasa_Empleo = mean(Tasa_Empleo, na.rm = TRUE),
    Tasa_Desocupacion = mean(Tasa_Desocupacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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

grafico_sexo_edad <- datos_barras_sexo_edad %>%
  ggplot(aes(x = Grupo_Edad_Simple, y = Valor, fill = Sexo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~ Indicador, scales = "free_y") +
  
  labs(
    title = "Tasas laborales por sexo y grupo etario",
    subtitle = "Promedio 2022-2024 - Gran Buenos Aires",
    x = "Grupo etario",
    y = "Tasa (%)",
    fill = "Sexo",
    caption = "Fuente: EPH - INDEC"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  scale_y_continuous(labels = scales::percent_format(scale = 1))

guardar_grafico(grafico_sexo_edad, "tasas_por_sexo_y_edad_2022_2024")
print(grafico_sexo_edad)

# =============================================================================
# GUARDAR TODOS LOS DATASETS
# =============================================================================

cat("\n💾 Guardando datasets multivariados...\n")

# Guardar datasets
save(tasas_por_sexo, file = file.path(rutas$datos_procesados, "tasas_por_sexo.RData"))
save(tasas_por_edad, file = file.path(rutas$datos_procesados, "tasas_por_edad.RData"))
save(tasas_por_educacion, file = file.path(rutas$datos_procesados, "tasas_por_educacion.RData"))
save(tasas_sexo_edad, file = file.path(rutas$datos_procesados, "tasas_sexo_edad.RData"))

# Guardar tablas resumen
write_csv(tabla_sexo_reciente, file.path(rutas$tablas, "resumen_por_sexo_2020_2024.csv"))
write_csv(tabla_edad_reciente, file.path(rutas$tablas, "resumen_por_edad_2020_2024.csv"))
write_csv(tabla_educacion_reciente, file.path(rutas$tablas, "resumen_por_educacion_2020_2024.csv"))

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ ANÁLISIS MULTIVARIADO COMPLETADO\n")
cat(rep("=", 60), "\n")

cat("📊 ANÁLISIS REALIZADOS:\n")
cat("   • Tasas laborales por sexo\n")
cat("   • Tasas laborales por grupo etario\n")
cat("   • Tasas laborales por nivel educativo\n")
cat("   • Análisis combinado sexo-edad\n")

cat("\n📈 GRÁFICOS GENERADOS:\n")
cat("   • Evolución temporal por sexo\n")
cat("   • Evolución temporal por edad\n")
cat("   • Evolución temporal por educación\n")
cat("   • Comparación sexo-edad (2022-2024)\n")




cat(rep("=", 60), "\n")
