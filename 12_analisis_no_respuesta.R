# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 12_analisis_no_respuesta.R - Análisis de no respuesta y comparación muestral
# =============================================================================

# Verificar datos
if(!exists("datos_gba")) {
  cat("🔄 Cargando datos procesados...\n")
  load(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))
}

cat("🔍 ANÁLISIS DETALLADO DE NO RESPUESTA Y PONDERADORES...\n\n")

# =============================================================================
# ANÁLISIS 1: PATRÓN DE NO RESPUESTA POR AÑO
# =============================================================================

# Función para preparar datos con información completa de respuesta
preparar_datos_respuesta <- function() {
  map_dfr(names(datos_gba), function(periodo) {
    if(!is.null(datos_gba[[periodo]]$personas)) {
      datos_gba[[periodo]]$personas %>%
        mutate(
          ANO4 = as.numeric(ANO4),
          TRIMESTRE = as.numeric(TRIMESTRE),
          CH04 = as.numeric(CH04),
          CH06 = as.numeric(CH06),
          ESTADO = as.numeric(ESTADO),
          NIVEL_ED = as.numeric(NIVEL_ED),
          PONDERA = as.numeric(PONDERA),
          
          # Analizar P47T (ingreso total individual)
          P47T_Original = P47T,
          P47T_Numerico = suppressWarnings(as.numeric(P47T)),
          
          # Clasificar tipos de respuesta
          Tipo_Respuesta_P47T = case_when(
            is.na(P47T) ~ "Missing_Sistema",
            P47T == "" ~ "String_Vacio",
            P47T == "-9" ~ "No_Sabe_No_Responde",
            P47T == "0" ~ "Cero_Declarado",
            suppressWarnings(as.numeric(P47T)) > 0 ~ "Respuesta_Valida",
            TRUE ~ "Otro"
          ),
          
          # Variables sociodemográficas
          Sexo = case_when(
            CH04 == 1 ~ "Varón",
            CH04 == 2 ~ "Mujer",
            TRUE ~ NA_character_
          ),
          
          Grupo_Edad = case_when(
            CH06 >= 18 & CH06 <= 29 ~ "18-29 años",
            CH06 >= 30 & CH06 <= 49 ~ "30-49 años",
            CH06 >= 50 & CH06 <= 64 ~ "50-64 años",
            TRUE ~ "Otros"
          ),
          
          Nivel_Ed_Agrupado = case_when(
            NIVEL_ED %in% c(1, 2, 3) ~ "Hasta primaria",
            NIVEL_ED %in% c(4, 5) ~ "Secundaria",
            NIVEL_ED %in% c(6, 7) ~ "Superior/Universitario",
            TRUE ~ "No especificado"
          ),
          
          Estado_Laboral = case_when(
            ESTADO == 1 ~ "Ocupado",
            ESTADO == 2 ~ "Desocupado", 
            ESTADO == 3 ~ "Inactivo",
            TRUE ~ "Otro"
          )
        ) %>%
        filter(
          !is.na(Sexo), Grupo_Edad != "Otros", 
          Nivel_Ed_Agrupado != "No especificado",
          !is.na(PONDERA), PONDERA > 0
        )
    }
  })
}

datos_respuesta <- preparar_datos_respuesta()

cat("=== ANÁLISIS 1: PATRONES DE NO RESPUESTA ===\n")

# Análisis de no respuesta por año
no_respuesta_anual <- datos_respuesta %>%
  group_by(ANO4) %>%
  summarise(
    Total_Casos = n(),
    Poblacion_Ponderada = sum(PONDERA, na.rm = TRUE),
    
    # Casos por tipo de respuesta
    Respuesta_Valida = sum(Tipo_Respuesta_P47T == "Respuesta_Valida"),
    No_Sabe_No_Responde = sum(Tipo_Respuesta_P47T == "No_Sabe_No_Responde"),
    Missing_Sistema = sum(Tipo_Respuesta_P47T == "Missing_Sistema"),
    Cero_Declarado = sum(Tipo_Respuesta_P47T == "Cero_Declarado"),
    
    # Tasas de respuesta
    Tasa_Respuesta = round((Respuesta_Valida / Total_Casos) * 100, 1),
    Tasa_No_Respuesta = round(((No_Sabe_No_Responde + Missing_Sistema) / Total_Casos) * 100, 1),
    
    .groups = "drop"
  )

cat("📊 PATRONES DE NO RESPUESTA POR AÑO:\n")
print(no_respuesta_anual)

# =============================================================================
# ANÁLISIS 2: ¿QUIÉN NO RESPONDE? (ANÁLISIS CON PONDERADORES)
# =============================================================================

cat("\n=== ANÁLISIS 2: PERFIL DE NO RESPONDENTES ===\n")

# Análisis con ponderadores de quién no responde
perfil_no_respuesta <- datos_respuesta %>%
  filter(ANO4 %in% c(2016, 2024)) %>%  # Comparar años extremos
  mutate(
    No_Responde = Tipo_Respuesta_P47T %in% c("No_Sabe_No_Responde", "Missing_Sistema")
  ) %>%
  group_by(ANO4, Sexo, Estado_Laboral, No_Responde) %>%
  summarise(
    Casos = n(),
    Poblacion_Ponderada = sum(PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ANO4, Sexo, Estado_Laboral) %>%
  mutate(
    Prop_Ponderada = round((Poblacion_Ponderada / sum(Poblacion_Ponderada)) * 100, 1)
  ) %>%
  filter(No_Responde == TRUE) %>%
  select(ANO4, Sexo, Estado_Laboral, Prop_Ponderada)

cat("📊 PROPORCIÓN DE NO RESPUESTA POR GRUPO (PONDERADA):\n")
print(perfil_no_respuesta)

# =============================================================================
# ANÁLISIS 3: COMPARACIÓN DE MUESTRAS 2016 vs 2024
# =============================================================================

cat("\n=== ANÁLISIS 3: COMPARACIÓN MUESTRAL 2016 vs 2024 ===\n")

# Función para calcular estadísticas ponderadas por grupo
calcular_estadisticas_comparativas <- function(año_focus) {
  datos_respuesta %>%
    filter(ANO4 == año_focus, Tipo_Respuesta_P47T == "Respuesta_Valida") %>%
    mutate(P47T_Clean = suppressWarnings(as.numeric(P47T_Original))) %>%
    filter(!is.na(P47T_Clean), P47T_Clean > 0) %>%
    group_by(Sexo, Estado_Laboral, Nivel_Ed_Agrupado) %>%
    summarise(
      Casos = n(),
      Poblacion_Ponderada = sum(PONDERA, na.rm = TRUE),
      Ingreso_Mediano = round(Hmisc::wtd.quantile(P47T_Clean, PONDERA, probs = 0.5, na.rm = TRUE)),
      Ingreso_Promedio = round(weighted.mean(P47T_Clean, PONDERA, na.rm = TRUE)),
      .groups = "drop"
    )
}

# Instalar Hmisc si no está disponible
if(!require(Hmisc, quietly = TRUE)) {
  install.packages("Hmisc")
  library(Hmisc)
}

# Comparar composición de muestras
composicion_2016 <- calcular_estadisticas_comparativas(2016) %>% mutate(Año = 2016)
composicion_2024 <- calcular_estadisticas_comparativas(2024) %>% mutate(Año = 2024)

# Combinar y comparar
comparacion_muestral <- bind_rows(composicion_2016, composicion_2024) %>%
  select(Año, everything()) %>%
  arrange(Sexo, Estado_Laboral, Nivel_Ed_Agrupado, Año)

cat("📊 COMPOSICIÓN MUESTRAL DETALLADA:\n")
print(comparacion_muestral)

# Análisis de cambios en composición
cambios_composicion <- composicion_2016 %>%
  left_join(composicion_2024, by = c("Sexo", "Estado_Laboral", "Nivel_Ed_Agrupado"), suffix = c("_2016", "_2024")) %>%
  mutate(
    Cambio_Casos = Casos_2024 - Casos_2016,
    Cambio_Poblacion = round((Poblacion_Ponderada_2024 / Poblacion_Ponderada_2016 - 1) * 100, 1),
    Cambio_Ingreso_Mediano = round((Ingreso_Mediano_2024 / Ingreso_Mediano_2016 - 1) * 100, 1)
  ) %>%
  select(Sexo, Estado_Laboral, Nivel_Ed_Agrupado, 
         Casos_2016, Casos_2024, Cambio_Casos,
         Cambio_Poblacion, Cambio_Ingreso_Mediano)

cat("\n📊 CAMBIOS EN COMPOSICIÓN 2016-2024:\n")
print(cambios_composicion)

# =============================================================================
# ANÁLISIS 4: VALORES EXTREMOS ELIMINADOS
# =============================================================================

cat("\n=== ANÁLISIS 4: ANÁLISIS DE OUTLIERS ===\n")

# Analizar qué valores se eliminaron
analizar_outliers <- function(año_focus) {
  datos_año <- datos_respuesta %>%
    filter(ANO4 == año_focus, Tipo_Respuesta_P47T == "Respuesta_Valida") %>%
    mutate(P47T_Clean = suppressWarnings(as.numeric(P47T_Original))) %>%
    filter(!is.na(P47T_Clean), P47T_Clean > 0)
  
  # Estadísticas antes de filtros
  stats_original <- list(
    n_total = nrow(datos_año),
    min_valor = min(datos_año$P47T_Clean),
    max_valor = max(datos_año$P47T_Clean),
    p99 = quantile(datos_año$P47T_Clean, 0.99),
    p95 = quantile(datos_año$P47T_Clean, 0.95),
    casos_over_1M = sum(datos_año$P47T_Clean > 1000000),
    casos_under_500 = sum(datos_año$P47T_Clean < 500)
  )
  
  return(stats_original)
}

outliers_2016 <- analizar_outliers(2016)
outliers_2024 <- analizar_outliers(2024)

cat("📊 ANÁLISIS DE VALORES EXTREMOS:\n")
cat("\n2016:\n")
cat("   • Total casos:", outliers_2016$n_total, "\n")
cat("   • Valor mínimo:", format(outliers_2016$min_valor, big.mark = ","), "\n")
cat("   • Valor máximo:", format(outliers_2016$max_valor, big.mark = ","), "\n")
cat("   • P95:", format(outliers_2016$p95, big.mark = ","), "\n")
cat("   • P99:", format(outliers_2016$p99, big.mark = ","), "\n")
cat("   • Casos > $1M:", outliers_2016$casos_over_1M, "\n")
cat("   • Casos < $500:", outliers_2016$casos_under_500, "\n")

cat("\n2024:\n")
cat("   • Total casos:", outliers_2024$n_total, "\n")
cat("   • Valor mínimo:", format(outliers_2024$min_valor, big.mark = ","), "\n")
cat("   • Valor máximo:", format(outliers_2024$max_valor, big.mark = ","), "\n")
cat("   • P95:", format(outliers_2024$p95, big.mark = ","), "\n")
cat("   • P99:", format(outliers_2024$p99, big.mark = ","), "\n")
cat("   • Casos > $1M:", outliers_2024$casos_over_1M, "\n")
cat("   • Casos < $500:", outliers_2024$casos_under_500, "\n")

# =============================================================================
# ANÁLISIS 5: IMPACTO DE INCLUIR NO RESPONDENTES CON PONDERADORES
# =============================================================================

cat("\n=== ANÁLISIS 5: IMPACTO DE NO RESPUESTA EN ESTIMACIONES ===\n")

# Calcular estadísticas incluyendo no respondentes con ponderadores
estadisticas_con_no_respuesta <- datos_respuesta %>%
  filter(ANO4 %in% c(2016, 2024)) %>%
  group_by(ANO4) %>%
  summarise(
    # Total población
    Poblacion_Total = sum(PONDERA, na.rm = TRUE),
    
    # Solo respondentes
    Poblacion_Respondentes = sum(PONDERA[Tipo_Respuesta_P47T == "Respuesta_Valida"], na.rm = TRUE),
    
    # Sesgo de cobertura
    Cobertura_Poblacional = round((Poblacion_Respondentes / Poblacion_Total) * 100, 1),
    
    .groups = "drop"
  )

cat("📊 COBERTURA POBLACIONAL POR RESPUESTA:\n")
print(estadisticas_con_no_respuesta)

# =============================================================================
# RECOMENDACIONES FINALES
# =============================================================================

cat("\n=== RECOMENDACIONES BASADAS EN EL ANÁLISIS ===\n")

cat("🔍 HALLAZGOS CLAVE:\n")
cat("   1. Tasa de no respuesta por año\n")
cat("   2. Perfil diferencial de no respondentes\n")
cat("   3. Cambios en composición muestral\n")
cat("   4. Valores extremos por año\n")

cat("\n🎯 PARA MEJORAR EL ANÁLISIS:\n")
cat("   • Usar ponderadores específicos para ingresos (PONDII)\n")
cat("   • Aplicar imputación para no respondentes\n")
cat("   • Ajustar por sesgos de selección\n")
cat("   • Filtros más conservadores por percentiles\n")

cat("\n📊 PRÓXIMO PASO RECOMENDADO:\n")
cat("   • Modelo de imputación para no respuesta\n")

# Guardar resultados
save(no_respuesta_anual, file = file.path(rutas$datos_procesados, "analisis_no_respuesta.RData"))
save(comparacion_muestral, file = file.path(rutas$datos_procesados, "comparacion_muestral.RData"))

write_csv(no_respuesta_anual, file.path(rutas$tablas, "no_respuesta_por_año.csv"))
write_csv(comparacion_muestral, file.path(rutas$tablas, "comparacion_muestral_2016_2024.csv"))

cat("\n✅ Análisis completado y guardado\n")