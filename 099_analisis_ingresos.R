# =============================================================================
# ANÁLISIS DE INGRESOS CON IPC TRIMESTRAL REAL
# Versión definitiva con datos oficiales INDEC por trimestre
# =============================================================================

library(dplyr)
library(ggplot2)
library(readr)
library(scales)

cat("💰 Análisis de ingresos con IPC trimestral real...\n")

# Verificar datos EPH
if(!exists("datos_gba")) {
  load(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))
}

# =============================================================================
# CARGAR IPC TRIMESTRAL REAL
# =============================================================================

cat("📊 Cargando IPC trimestral real...\n")

# Cargar datos IPC
ruta_ipc <- file.path(rutas$datos_externos, "ipc_trimestral.csv")

if(!file.exists(ruta_ipc)) {
  stop("❌ No se encontró el archivo: ", ruta_ipc, 
       "\nColoca el archivo 'ipc_trimestral.csv' en: ", rutas$datos_externos)
}

ipc_trimestral <- read_csv(ruta_ipc, locale = locale(encoding = "UTF-8"))

cat("✅ IPC trimestral cargado:", nrow(ipc_trimestral), "registros\n")

# Mostrar estructura
cat("📋 Estructura del IPC trimestral:\n")
print(head(ipc_trimestral))
cat("📊 Rango de años:", min(ipc_trimestral$anio), "-", max(ipc_trimestral$anio), "\n")

# =============================================================================
# CALCULAR FACTORES DE DEFLACTACIÓN CON BASE 2023
# =============================================================================

cat("\n🔢 Calculando factores de deflactación (base 2023)...\n")

# Calcular IPC promedio de 2023 como base
ipc_base_2023 <- ipc_trimestral %>%
  filter(anio == 2023) %>%
  summarise(ipc_promedio_2023 = mean(ipc, na.rm = TRUE)) %>%
  pull(ipc_promedio_2023)

cat("📊 IPC promedio 2023 (base):", round(ipc_base_2023, 2), "\n")

# Calcular factores de deflactación para cada trimestre
ipc_con_factores <- ipc_trimestral %>%
  mutate(
    factor_deflactor = ipc / ipc_base_2023,
    periodo = paste0(anio, "_T", trimestre)
  ) %>%
  arrange(anio, trimestre)

cat("✅ Factores calculados para", nrow(ipc_con_factores), "trimestres\n")

# Mostrar algunos factores
cat("📋 Muestra de factores de deflactación:\n")
print(ipc_con_factores %>% 
        filter(anio %in% c(2016, 2020, 2023, 2024)) %>%
        select(periodo, ipc, factor_deflactor))

# =============================================================================
# FUNCIÓN PARA DEFLACTAR CON PRECISION TRIMESTRAL
# =============================================================================

deflactar_trimestral <- function(ingreso_nominal, ano_periodo, trim_periodo, tabla_ipc) {
  
  # Buscar factor exacto para el año-trimestre
  factor <- tabla_ipc$factor_deflactor[
    tabla_ipc$anio == ano_periodo & tabla_ipc$trimestre == trim_periodo
  ]
  
  if(length(factor) == 0 || is.na(factor)) {
    return(NA)
  }
  
  # Deflactar: dividir por factor para obtener pesos de 2023
  ingreso_real <- ingreso_nominal / factor
  
  return(ingreso_real)
}

# =============================================================================
# PROCESAR INGRESOS CON DEFLACTACIÓN TRIMESTRAL
# =============================================================================

cat("\n💰 Procesando ingresos con deflactación trimestral precisa...\n")

ingresos_trimestral <- list()

for(periodo_nombre in names(datos_gba)) {
  
  # Extraer año y trimestre
  partes <- strsplit(periodo_nombre, "_T")[[1]]
  ano <- as.numeric(partes[1])
  trimestre <- as.numeric(partes[2])
  
  # Buscar factor en tabla IPC
  factor_periodo <- ipc_con_factores$factor_deflactor[
    ipc_con_factores$anio == ano & ipc_con_factores$trimestre == trimestre
  ]
  
  if(length(factor_periodo) == 0) {
    cat("📅", periodo_nombre, "- Sin IPC disponible\n")
    next
  }
  
  cat("📅", periodo_nombre, "- Factor:", round(factor_periodo, 3), "...")
  
  datos_periodo <- datos_gba[[periodo_nombre]]$personas
  
  if(is.null(datos_periodo)) {
    cat(" Sin datos EPH\n")
    next
  }
  
  # Filtrar ocupados con ingresos
  ocupados_ingresos <- datos_periodo %>%
    filter(
      ESTADO == 1,          # Solo ocupados
      !is.na(P47T),         # Con ingresos
      P47T > 0,             # Positivos
      P47T < 10000000       # Filtrar extremos nominales
    )
  
  if(nrow(ocupados_ingresos) == 0) {
    cat(" Sin ocupados con ingresos\n")
    next
  }
  
  # Deflactar con precisión trimestral
  ingresos_deflactados <- ocupados_ingresos %>%
    mutate(
      ano = ano,
      trimestre = trimestre,
      periodo = periodo_nombre,
      ingreso_nominal = as.numeric(P47T),
      ingreso_real_2023 = ingreso_nominal / factor_periodo  # Deflactación directa
    ) %>%
    filter(
      !is.na(ingreso_real_2023),
      ingreso_real_2023 > 0,
      ingreso_real_2023 < 10000000  # Filtrar extremos reales
    )
  
  # Eliminar outliers (P1 y P99)
  if(nrow(ingresos_deflactados) > 20) {
    p1 <- quantile(ingresos_deflactados$ingreso_real_2023, 0.01, na.rm = TRUE)
    p99 <- quantile(ingresos_deflactados$ingreso_real_2023, 0.99, na.rm = TRUE)
    
    ingresos_deflactados <- ingresos_deflactados %>%
      filter(ingreso_real_2023 >= p1, ingreso_real_2023 <= p99)
  }
  
  if(nrow(ingresos_deflactados) > 0) {
    ingresos_trimestral[[periodo_nombre]] <- ingresos_deflactados
    cat(" ✅", nrow(ingresos_deflactados), "registros\n")
  } else {
    cat(" ❌ Sin datos válidos post-filtros\n")
  }
}

# Combinar todos los resultados
if(length(ingresos_trimestral) > 0) {
  ingresos_final_trimestral <- bind_rows(ingresos_trimestral)
  cat("\n✅ TOTAL PROCESADO:", nrow(ingresos_final_trimestral), "registros\n")
} else {
  stop("❌ No se procesaron ingresos")
}

# =============================================================================
# ESTADÍSTICAS CON DEFLACTACIÓN TRIMESTRAL
# =============================================================================

cat("\n📊 Calculando estadísticas con deflactación trimestral...\n")

# Verificar Hmisc para percentiles ponderados
usar_hmisc <- require(Hmisc, quietly = TRUE)

estadisticas_trimestral <- ingresos_final_trimestral %>%
  group_by(ano, trimestre) %>%
  summarise(
    n_casos = n(),
    # Media y mediana (ponderadas si es posible)
    media = if(usar_hmisc) {
      round(weighted.mean(ingreso_real_2023, PONDERA, na.rm = TRUE))
    } else {
      round(mean(ingreso_real_2023, na.rm = TRUE))
    },
    mediana = if(usar_hmisc) {
      round(Hmisc::wtd.quantile(ingreso_real_2023, PONDERA, probs = 0.5, na.rm = TRUE))
    } else {
      round(median(ingreso_real_2023, na.rm = TRUE))
    },
    # Percentiles
    p25 = round(quantile(ingreso_real_2023, 0.25, na.rm = TRUE)),
    p75 = round(quantile(ingreso_real_2023, 0.75, na.rm = TRUE)),
    p10 = round(quantile(ingreso_real_2023, 0.10, na.rm = TRUE)),
    p90 = round(quantile(ingreso_real_2023, 0.90, na.rm = TRUE)),
    # Dispersión
    desvio = round(sd(ingreso_real_2023, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    periodo = paste0(ano, "_T", trimestre),
    fecha = as.Date(paste(ano, (trimestre-1)*3 + 2, "01", sep = "-")),
    cv = round((desvio / media) * 100, 1)
  ) %>%
  arrange(ano, trimestre)

cat("✅ Estadísticas trimestrales calculadas:", nrow(estadisticas_trimestral), "períodos\n")

# =============================================================================
# GRÁFICO CON DEFLACTACIÓN TRIMESTRAL
# =============================================================================

cat("\n📈 Creando gráfico con deflactación trimestral...\n")

# Preparar datos para gráfico
datos_grafico_trim <- estadisticas_trimestral %>%
  select(fecha, ano, media, mediana) %>%
  tidyr::pivot_longer(cols = c(media, mediana), names_to = "tipo", values_to = "valor")

# Crear gráfico mejorado
grafico_trimestral <- datos_grafico_trim %>%
  ggplot(aes(x = fecha, y = valor, color = tipo, group = tipo)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 0.8) +
  # Líneas de referencia importantes
  geom_vline(xintercept = as.Date("2020-03-01"), 
             color = "red", linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = as.Date("2023-12-01"), 
             color = "green", linetype = "dotted", alpha = 0.7) +
  # Anotaciones
  annotate("text", x = as.Date("2020-06-01"), 
           y = max(datos_grafico_trim$valor) * 0.9,
           label = "COVID-19", angle = 90, color = "red", size = 3.5) +
  annotate("text", x = as.Date("2023-09-01"), 
           y = max(datos_grafico_trim$valor) * 0.8,
           label = "Base 2023", angle = 90, color = "green", size = 3) +
  scale_color_manual(
    values = c("media" = "#e74c3c", "mediana" = "#3498db"),
    labels = c("Media", "Mediana"),
    name = ""
  ) +
  labs(
    title = "Evolución de Ingresos Laborales Reales (Deflactación Trimestral)",
    subtitle = "Gran Buenos Aires 2016-2024 | Pesos constantes de 2023 | IPC oficial INDEC",
    x = "Período",
    y = "Ingreso Real ($ de 2023)",
    caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: Deflactación con IPC trimestral oficial"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Guardar gráfico
guardar_grafico(grafico_trimestral, "ingresos_deflactacion_trimestral", ancho = 14, alto = 9)

cat("✅ Gráfico con deflactación trimestral guardado\n")

# =============================================================================
# TABLA COMPARATIVA: ANUAL VS TRIMESTRAL
# =============================================================================

cat("\n📊 Creando tabla anual con deflactación trimestral...\n")

tabla_anual_trimestral <- estadisticas_trimestral %>%
  group_by(ano) %>%
  summarise(
    media_anual = round(mean(media, na.rm = TRUE)),
    mediana_anual = round(mean(mediana, na.rm = TRUE)),
    p25_anual = round(mean(p25, na.rm = TRUE)),
    p75_anual = round(mean(p75, na.rm = TRUE)),
    casos_totales = sum(n_casos),
    .groups = "drop"
  ) %>%
  mutate(
    var_media = round(((media_anual / lag(media_anual) - 1) * 100), 1),
    var_mediana = round(((mediana_anual / lag(mediana_anual) - 1) * 100), 1)
  )

# Tabla final para el informe
tabla_final_informe <- tabla_anual_trimestral %>%
  select(ano, media_anual, mediana_anual, p25_anual, p75_anual, var_media) %>%
  rename(
    Año = ano,
    Media = media_anual,
    Mediana = mediana_anual,
    P25 = p25_anual,
    P75 = p75_anual,
    `Var. %` = var_media
  )

cat("📋 Tabla anual con deflactación trimestral:\n")
print(tabla_final_informe)

# =============================================================================
# GUARDAR RESULTADOS FINALES
# =============================================================================

cat("\n💾 Guardando resultados finales...\n")

# Guardar datos
write_csv(estadisticas_trimestral, file.path(rutas$tablas, "ingresos_deflactacion_trimestral.csv"))
write_csv(tabla_final_informe, file.path(rutas$tablas, "tabla_ingresos_trimestral_final.csv"))
save(ingresos_final_trimestral, estadisticas_trimestral, ipc_con_factores, 
     file = file.path(rutas$datos_procesados, "ingresos_deflactacion_trimestral.RData"))

cat("✅ Archivos guardados\n")

# =============================================================================
# COMPARACIÓN CON ANÁLISIS ANTERIOR
# =============================================================================

cat("\n🔍 Comparando con análisis anterior...\n")

# Si existe el análisis anterior, hacer comparación
if(exists("estadisticas") && file.exists(file.path(rutas$datos_procesados, "ingresos_final.RData"))) {
  
  cat("📊 Cargando análisis anterior para comparación...\n")
  load(file.path(rutas$datos_procesados, "ingresos_final.RData"))
  
  # Comparar 2024
  media_2024_anterior <- estadisticas %>% filter(ano == 2024) %>% summarise(m = mean(media)) %>% pull(m)
  media_2024_trimestral <- estadisticas_trimestral %>% filter(ano == 2024) %>% summarise(m = mean(media)) %>% pull(m)
  
  cat("📈 Comparación 2024:\n")
  cat("   • Análisis anterior (anual):", format(round(media_2024_anterior), big.mark = ","), "\n")
  cat("   • Análisis trimestral:", format(round(media_2024_trimestral), big.mark = ","), "\n")
  cat("   • Diferencia:", format(round(media_2024_trimestral - media_2024_anterior), big.mark = ","), "\n")
}

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 70), "\n")
cat("✅ ANÁLISIS DE INGRESOS CON DEFLACTACIÓN TRIMESTRAL COMPLETADO\n")
cat(rep("=", 70), "\n")

cat("💰 DATOS PROCESADOS:\n")
cat("   • Total registros:", format(nrow(ingresos_final_trimestral), big.mark = ","), "\n")
cat("   • Períodos analizados:", length(unique(estadisticas_trimestral$periodo)), "\n")
cat("   • Deflactación: IPC trimestral oficial INDEC\n")
cat("   • Base: Promedio 2023 =", round(ipc_base_2023, 2), "\n")

ultimo_periodo <- tail(estadisticas_trimestral, 1)
cat("\n📊 SITUACIÓN ACTUAL (", ultimo_periodo$periodo, "):\n", sep = "")
cat("   • Ingreso medio:", format(ultimo_periodo$media, big.mark = ","), "$ (2023)\n")
cat("   • Ingreso mediano:", format(ultimo_periodo$mediana, big.mark = ","), "$ (2023)\n")

if(nrow(tabla_anual_trimestral) > 1) {
  ultima_var <- tail(tabla_anual_trimestral$var_media[!is.na(tabla_anual_trimestral$var_media)], 1)
  if(length(ultima_var) > 0) {
    cat("   • Variación anual 2024:", ultima_var, "%\n")
  }
}

cat("\n📈 ARCHIVOS PARA INFORME:\n")
cat("   • ingresos_deflactacion_trimestral.png\n")
cat("   • tabla_ingresos_trimestral_final.csv\n")
cat("   • ingresos_deflactacion_trimestral.csv (datos completos)\n")

cat("\n🎯 METODOLOGÍA MEJORADA:\n")
cat("   • Deflactación trimestre a trimestre (más precisa)\n")
cat("   • IPC oficial INDEC por trimestre\n")
cat("   • Base 2023 calculada como promedio anual\n")
cat("   • Eliminación de aproximaciones anuales\n")

cat(rep("=", 70), "\n")

# Mostrar gráfico final
print(grafico_trimestral)