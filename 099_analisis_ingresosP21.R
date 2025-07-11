# =============================================================================
# ANÁLISIS P21 CORREGIDO + ANÁLISIS POR EDUCACIÓN
# Fix inmediato para ponderadores + gráficos separados
# =============================================================================

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)

cat("💼 Análisis P21 corregido con análisis por educación...\n")

# Verificar datos EPH
if(!exists("datos_gba")) {
  load(file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData"))
}

# =============================================================================
# FUNCIÓN PARA CONVERTIR PONDERADORES A NUMÉRICO
# =============================================================================

convertir_ponderador_seguro <- function(x) {
  if(is.numeric(x)) {
    return(x)
  } else if(is.character(x)) {
    # Limpiar caracteres especiales y convertir
    x_limpio <- gsub("[^0-9.-]", "", x)
    x_num <- as.numeric(x_limpio)
    return(ifelse(is.na(x_num) | x_num <= 0, 1, x_num))  # Default 1 si hay problemas
  } else if(is.factor(x)) {
    x_num <- as.numeric(as.character(x))
    return(ifelse(is.na(x_num) | x_num <= 0, 1, x_num))
  } else {
    return(rep(1, length(x)))  # Ponderador uniforme por defecto
  }
}

# =============================================================================
# FUNCIÓN MEJORADA PARA CONVERSIÓN SEGURA
# =============================================================================

convertir_p21_seguro <- function(x) {
  if(is.numeric(x)) {
    return(x)
  } else if(is.character(x)) {
    x_limpio <- gsub("[^0-9.-]", "", x)
    return(as.numeric(x_limpio))
  } else if(is.factor(x)) {
    return(as.numeric(as.character(x)))
  } else {
    return(as.numeric(x))
  }
}

# =============================================================================
# CARGAR IPC (REUTILIZAR)
# =============================================================================

if(exists("ipc_con_factores")) {
  cat("✅ Usando factores IPC ya cargados\n")
} else {
  # Código de carga IPC aquí si es necesario
  stop("❌ Ejecuta primero el análisis de ingresos para cargar IPC")
}

cat("📊 IPC base 2023:", round(ipc_base_2023, 2), "\n")

# =============================================================================
# PROCESAMIENTO P21 CON PONDERADORES CORREGIDOS
# =============================================================================

cat("\n💼 Procesando P21 con ponderadores corregidos...\n")

ingresos_p21_trimestral <- list()

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
  
  # Filtrar ocupados con P21
  ocupados_p21 <- datos_periodo %>%
    filter(
      ESTADO == 1,          # Solo ocupados
      !is.na(P21),          # Con ingresos P21
      P21 != -9,            # Excluir no respuesta
      P21 != 0              # Excluir ceros
    )
  
  if(nrow(ocupados_p21) == 0) {
    cat(" Sin ocupados con P21\n")
    next
  }
  
  # Convertir variables de forma segura
  ocupados_p21 <- ocupados_p21 %>%
    mutate(
      p21_numerico = convertir_p21_seguro(P21),
      p47t_numerico = convertir_p21_seguro(P47T),
      # Corregir ponderadores
      pondera_corregido = convertir_ponderador_seguro(PONDERA),
      pondiio_corregido = convertir_ponderador_seguro(PONDIIO),
      pondii_corregido = convertir_ponderador_seguro(PONDII)
    ) %>%
    filter(
      !is.na(p21_numerico),
      p21_numerico > 0,
      p21_numerico < 10000000
    )
  
  if(nrow(ocupados_p21) == 0) {
    cat(" Sin datos válidos post-conversión\n")
    next
  }
  
  # Deflactar y agregar variables de análisis
  ingresos_p21_deflactados <- ocupados_p21 %>%
    mutate(
      ano = ano,
      trimestre = trimestre,
      periodo = periodo_nombre,
      p21_real_2023 = p21_numerico / factor_periodo,
      p47t_real_2023 = ifelse(!is.na(p47t_numerico) & p47t_numerico > 0, 
                              p47t_numerico / factor_periodo, NA),
      # Crear variable de educación
      nivel_educativo = case_when(
        CH12 %in% c(1, 2, 3) ~ "Hasta Primario",
        CH12 %in% c(4, 5) ~ "Secundario",
        CH12 %in% c(6, 7, 8, 9) ~ "Superior/Universitario",
        TRUE ~ "Sin clasificar"
      ),
      # Edad categorizada
      grupo_edad = case_when(
        CH06 < 30 ~ "18-29",
        CH06 < 40 ~ "30-39", 
        CH06 < 50 ~ "40-49",
        CH06 < 60 ~ "50-59",
        TRUE ~ "60+"
      )
    )
  
  # Eliminar outliers P21 (P1 y P99)
  if(nrow(ingresos_p21_deflactados) > 20) {
    p1 <- quantile(ingresos_p21_deflactados$p21_real_2023, 0.01, na.rm = TRUE)
    p99 <- quantile(ingresos_p21_deflactados$p21_real_2023, 0.99, na.rm = TRUE)
    
    ingresos_p21_deflactados <- ingresos_p21_deflactados %>%
      filter(p21_real_2023 >= p1, p21_real_2023 <= p99)
  }
  
  if(nrow(ingresos_p21_deflactados) > 0) {
    ingresos_p21_trimestral[[periodo_nombre]] <- ingresos_p21_deflactados
    cat(" ✅", nrow(ingresos_p21_deflactados), "registros\n")
  } else {
    cat(" ❌ Sin datos válidos post-filtros\n")
  }
}

# Combinar todos los resultados P21
if(length(ingresos_p21_trimestral) > 0) {
  ingresos_p21_final <- bind_rows(ingresos_p21_trimestral)
  cat("\n✅ TOTAL P21 PROCESADO:", nrow(ingresos_p21_final), "registros\n")
} else {
  stop("❌ No se procesaron ingresos P21")
}

# =============================================================================
# ESTADÍSTICAS P21 BÁSICAS (SIN PONDERACIÓN PROBLEMÁTICA)
# =============================================================================

cat("\n📊 Calculando estadísticas P21 básicas...\n")

usar_hmisc <- require(Hmisc, quietly = TRUE)

estadisticas_p21_trimestral <- ingresos_p21_final %>%
  group_by(ano, trimestre) %>%
  summarise(
    n_casos = n(),
    # Usar ponderación simple con PONDERA corregido
    media_p21 = if(usar_hmisc) {
      round(weighted.mean(p21_real_2023, pondera_corregido, na.rm = TRUE))
    } else {
      round(mean(p21_real_2023, na.rm = TRUE))
    },
    mediana_p21 = round(median(p21_real_2023, na.rm = TRUE)),  # Sin ponderar por simplicidad
    p25_p21 = round(quantile(p21_real_2023, 0.25, na.rm = TRUE)),
    p75_p21 = round(quantile(p21_real_2023, 0.75, na.rm = TRUE)),
    desvio_p21 = round(sd(p21_real_2023, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    periodo = paste0(ano, "_T", trimestre),
    fecha = as.Date(paste(ano, (trimestre-1)*3 + 2, "01", sep = "-")),
    cv_p21 = round((desvio_p21 / media_p21) * 100, 1)
  ) %>%
  arrange(ano, trimestre)

cat("✅ Estadísticas P21 calculadas para", nrow(estadisticas_p21_trimestral), "períodos\n")

# =============================================================================
# GRÁFICO 1: SOLO P21 - EVOLUCIÓN TEMPORAL
# =============================================================================

cat("\n📈 Creando gráfico 1: Evolución P21 únicamente...\n")

datos_grafico_p21 <- estadisticas_p21_trimestral %>%
  select(fecha, ano, media_p21, mediana_p21) %>%
  tidyr::pivot_longer(cols = c(media_p21, mediana_p21), names_to = "tipo", values_to = "valor") %>%
  mutate(tipo = case_when(
    tipo == "media_p21" ~ "Media",
    tipo == "mediana_p21" ~ "Mediana"
  ))

grafico_p21_solo <- datos_grafico_p21 %>%
  ggplot(aes(x = fecha, y = valor, color = tipo, group = tipo)) +
  geom_line(size = 1.4, alpha = 0.9) +
  geom_point(size = 2.8, alpha = 0.8) +
  geom_vline(xintercept = as.Date("2020-03-01"), 
             color = "red", linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = as.Date("2023-12-01"), 
             color = "green", linetype = "dotted", alpha = 0.7) +
  annotate("text", x = as.Date("2020-06-01"), 
           y = max(datos_grafico_p21$valor) * 0.9,
           label = "COVID-19", angle = 90, color = "red", size = 3.5) +
  annotate("text", x = as.Date("2023-09-01"), 
           y = max(datos_grafico_p21$valor) * 0.8,
           label = "Base 2023", angle = 90, color = "green", size = 3) +
  scale_color_manual(
    values = c("Media" = "#e74c3c", "Mediana" = "#3498db"),
    name = ""
  ) +
  labs(
    title = "Evolución de Ingresos de la Ocupación Principal (P21)",
    subtitle = "Gran Buenos Aires 2016-2024 | Pesos constantes de 2023 | Deflactación trimestral",
    x = "Período",
    y = "Ingreso Real P21 ($ de 2023)",
    caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: Solo ingresos de ocupación principal, excluye transferencias y otros ingresos"
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

guardar_grafico(grafico_p21_solo, "evolucion_p21_solo", ancho = 14, alto = 9)

# =============================================================================
# ESTADÍSTICAS COMPARATIVAS P21 vs P47T
# =============================================================================

cat("\n🔄 Calculando comparación P21 vs P47T...\n")

estadisticas_comparativa <- ingresos_p21_final %>%
  filter(!is.na(p47t_real_2023), p47t_real_2023 > 0) %>%
  group_by(ano, trimestre) %>%
  summarise(
    n_casos_comp = n(),
    media_p21 = round(mean(p21_real_2023, na.rm = TRUE)),
    mediana_p21 = round(median(p21_real_2023, na.rm = TRUE)),
    media_p47t = round(mean(p47t_real_2023, na.rm = TRUE)),
    mediana_p47t = round(median(p47t_real_2023, na.rm = TRUE)),
    ratio_p47t_p21_media = round(media_p47t / media_p21, 2),
    ratio_p47t_p21_mediana = round(mediana_p47t / mediana_p21, 2),
    diferencia_absoluta = media_p47t - media_p21,
    .groups = "drop"
  ) %>%
  mutate(
    periodo = paste0(ano, "_T", trimestre),
    fecha = as.Date(paste(ano, (trimestre-1)*3 + 2, "01", sep = "-"))
  ) %>%
  arrange(ano, trimestre)

cat("✅ Comparación P21/P47T calculada para", nrow(estadisticas_comparativa), "períodos\n")

# =============================================================================
# GRÁFICO 2: COMPARACIÓN P21 vs P47T
# =============================================================================

cat("\n📈 Creando gráfico 2: Comparación P21 vs P47T...\n")

datos_grafico_comp <- estadisticas_comparativa %>%
  select(fecha, ano, media_p21, mediana_p21, media_p47t, mediana_p47t) %>%
  tidyr::pivot_longer(
    cols = c(media_p21, mediana_p21, media_p47t, mediana_p47t), 
    names_to = "indicador", 
    values_to = "valor"
  ) %>%
  mutate(
    variable = case_when(
      str_detect(indicador, "p21") ~ "P21 (Ocupación Principal)",
      str_detect(indicador, "p47t") ~ "P47T (Ingreso Total)",
      TRUE ~ indicador
    ),
    tipo = case_when(
      str_detect(indicador, "media") ~ "Media",
      str_detect(indicador, "mediana") ~ "Mediana",
      TRUE ~ "Otro"
    )
  )

grafico_comparativo <- datos_grafico_comp %>%
  ggplot(aes(x = fecha, y = valor, color = variable, linetype = tipo)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.7) +
  geom_vline(xintercept = as.Date("2020-03-01"), 
             color = "red", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = as.Date("2023-12-01"), 
             color = "green", linetype = "dotted", alpha = 0.6) +
  annotate("text", x = as.Date("2020-06-01"), 
           y = max(datos_grafico_comp$valor) * 0.9,
           label = "COVID-19", angle = 90, color = "red", size = 3) +
  annotate("text", x = as.Date("2023-09-01"), 
           y = max(datos_grafico_comp$valor) * 0.8,
           label = "Base 2023", angle = 90, color = "green", size = 3) +
  scale_color_manual(
    values = c("P21 (Ocupación Principal)" = "#e74c3c", "P47T (Ingreso Total)" = "#3498db"),
    name = "Variable"
  ) +
  scale_linetype_manual(
    values = c("Media" = "solid", "Mediana" = "dashed"),
    name = "Estadístico"
  ) +
  labs(
    title = "Comparación P21 (Ocupación Principal) vs P47T (Ingreso Total)",
    subtitle = "Gran Buenos Aires 2016-2024 | Pesos constantes de 2023 | Deflactación trimestral",
    x = "Período",
    y = "Ingreso Real ($ de 2023)",
    caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: P21 incluye solo ingresos laborales principales; P47T incluye todos los ingresos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

guardar_grafico(grafico_comparativo, "comparacion_p21_p47t", ancho = 16, alto = 10)

# =============================================================================
# GRÁFICO 3: RATIOS P47T/P21
# =============================================================================

cat("\n📈 Creando gráfico 3: Ratios P47T/P21...\n")

grafico_ratios <- estadisticas_comparativa %>%
  select(fecha, ratio_p47t_p21_media, ratio_p47t_p21_mediana) %>%
  tidyr::pivot_longer(
    cols = c(ratio_p47t_p21_media, ratio_p47t_p21_mediana),
    names_to = "tipo_ratio",
    values_to = "ratio"
  ) %>%
  mutate(
    tipo = case_when(
      tipo_ratio == "ratio_p47t_p21_media" ~ "Media",
      tipo_ratio == "ratio_p47t_p21_mediana" ~ "Mediana"
    )
  ) %>%
  ggplot(aes(x = fecha, y = ratio, color = tipo)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5, color = "gray50") +
  geom_vline(xintercept = as.Date("2020-03-01"), 
             color = "red", linetype = "dashed", alpha = 0.6) +
  annotate("text", x = as.Date("2018-01-01"), y = 1.05, 
           label = "P47T = P21", color = "gray50", size = 3) +
  annotate("text", x = as.Date("2020-06-01"), y = max(estadisticas_comparativa$ratio_p47t_p21_media) * 0.9,
           label = "COVID-19", angle = 90, color = "red", size = 3) +
  scale_color_manual(
    values = c("Media" = "#e67e22", "Mediana" = "#9b59b6"),
    name = "Estadístico"
  ) +
  labs(
    title = "Ratio P47T/P21: Ingresos Totales vs Ocupación Principal",
    subtitle = "Valores > 1 indican que los ingresos totales superan a los laborales principales",
    x = "Período",
    y = "Ratio P47T/P21",
    caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: Ratio = 1 significa igualdad; > 1 indica presencia de ingresos no laborales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

guardar_grafico(grafico_ratios, "ratios_p47t_p21", ancho = 14, alto = 9)

# =============================================================================
# ANÁLISIS POR NIVEL EDUCATIVO
# =============================================================================

cat("\n🎓 Creando análisis por nivel educativo...\n")

# Estadísticas por educación y año
estadisticas_educacion <- ingresos_p21_final %>%
  filter(nivel_educativo != "Sin clasificar") %>%
  group_by(ano, nivel_educativo) %>%
  summarise(
    n_casos = n(),
    media_p21 = round(mean(p21_real_2023, na.rm = TRUE)),
    mediana_p21 = round(median(p21_real_2023, na.rm = TRUE)),
    p25_p21 = round(quantile(p21_real_2023, 0.25, na.rm = TRUE)),
    p75_p21 = round(quantile(p21_real_2023, 0.75, na.rm = TRUE)),
    .groups = "drop"
  )

# Gráfico por educación
grafico_educacion <- estadisticas_educacion %>%
  ggplot(aes(x = ano, y = media_p21, color = nivel_educativo, group = nivel_educativo)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_vline(xintercept = 2020, color = "red", linetype = "dashed", alpha = 0.6) +
  scale_color_manual(
    values = c("Hasta Primario" = "#e74c3c", 
               "Secundario" = "#f39c12", 
               "Superior/Universitario" = "#27ae60"),
    name = "Nivel Educativo"
  ) +
  labs(
    title = "Evolución de Ingresos P21 por Nivel Educativo",
    subtitle = "Gran Buenos Aires 2016-2024 | Pesos constantes de 2023",
    x = "Año",
    y = "Ingreso Medio P21 ($ de 2023)",
    caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: Solo ingresos de ocupación principal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray60", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_continuous(breaks = seq(2016, 2024, 2))

guardar_grafico(grafico_educacion, "p21_por_educacion", ancho = 14, alto = 9)

# =============================================================================
# TABLAS FINALES
# =============================================================================

cat("\n📋 Creando tablas finales...\n")

# Tabla P21 solo
tabla_p21_anual <- estadisticas_p21_trimestral %>%
  group_by(ano) %>%
  summarise(
    media_p21 = round(mean(media_p21, na.rm = TRUE)),
    mediana_p21 = round(mean(mediana_p21, na.rm = TRUE)),
    p25_p21 = round(mean(p25_p21, na.rm = TRUE)),
    p75_p21 = round(mean(p75_p21, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(var_p21 = round(((media_p21 / lag(media_p21) - 1) * 100), 1))

# Tabla comparativa
tabla_comparativa_anual <- estadisticas_comparativa %>%
  group_by(ano) %>%
  summarise(
    media_p21 = round(mean(media_p21, na.rm = TRUE)),
    media_p47t = round(mean(media_p47t, na.rm = TRUE)),
    ratio_medio = round(mean(ratio_p47t_p21_media, na.rm = TRUE), 2),
    diferencia = round(mean(diferencia_absoluta, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    var_p21 = round(((media_p21 / lag(media_p21) - 1) * 100), 1),
    var_p47t = round(((media_p47t / lag(media_p47t) - 1) * 100), 1)
  )

# Tabla educación (últimos años)
tabla_educacion_final <- estadisticas_educacion %>%
  filter(ano %in% c(2016, 2019, 2022, 2024)) %>%
  select(ano, nivel_educativo, media_p21, n_casos) %>%
  pivot_wider(names_from = ano, values_from = c(media_p21, n_casos))

cat("📋 Tabla P21 anual:\n")
print(tabla_p21_anual)

cat("\n📋 Tabla comparativa P21 vs P47T:\n")
print(tabla_comparativa_anual)

cat("\n📋 Tabla por educación (años seleccionados):\n")
print(tabla_educacion_final)

# =============================================================================
# GUARDAR RESULTADOS
# =============================================================================

cat("\n💾 Guardando resultados...\n")

write_csv(tabla_p21_anual, file.path(rutas$tablas, "tabla_p21_anual_final.csv"))
write_csv(tabla_comparativa_anual, file.path(rutas$tablas, "tabla_comparativa_final.csv"))
write_csv(estadisticas_educacion, file.path(rutas$tablas, "p21_por_educacion.csv"))

save(ingresos_p21_final, estadisticas_p21_trimestral, estadisticas_comparativa, estadisticas_educacion,
     file = file.path(rutas$datos_procesados, "analisis_p21_completo.RData"))

cat("✅ Análisis P21 completo guardado\n")

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 80), "\n")
cat("✅ ANÁLISIS P21 COMPLETO FINALIZADO\n")
cat(rep("=", 80), "\n")

cat("📈 GRÁFICOS GENERADOS:\n")
cat("   1. evolucion_p21_solo.png - Solo evolución P21\n")
cat("   2. comparacion_p21_p47t.png - Comparación P21 vs P47T\n")
cat("   3. ratios_p47t_p21.png - Ratios de divergencia\n")
cat("   4. p21_por_educacion.png - P21 por nivel educativo\n")

cat("\n📊 DATOS PROCESADOS:\n")
cat("   • Total registros P21:", format(nrow(ingresos_p21_final), big.mark = ","), "\n")
cat("   • Períodos analizados:", length(unique(estadisticas_p21_trimestral$periodo)), "\n")

ultimo_p21 <- tail(tabla_p21_anual, 1)
cat("\n📊 SITUACIÓN 2024:\n")
cat("   • P21 medio 2024:", format(ultimo_p21$media_p21, big.mark = ","), "$ (2023)\n")
cat("   • Crecimiento 2024:", ultimo_p21$var_p21, "%\n")

ultimo_comp <- tail(tabla_comparativa_anual, 1)
cat("   • Ratio P47T/P21 en 2024:", ultimo_comp$ratio_medio, "\n")
cat("   • Diferencia promedio:", format(ultimo_comp$diferencia, big.mark = ","), "$ (2023)\n")

# Estadísticas de educación 2024
educacion_2024 <- estadisticas_educacion %>% filter(ano == 2024)
if(nrow(educacion_2024) > 0) {
  cat("\n🎓 BRECHAS EDUCATIVAS 2024:\n")
  ed_max <- educacion_2024[which.max(educacion_2024$media_p21), ]
  ed_min <- educacion_2024[which.min(educacion_2024$media_p21), ]
  brecha <- round((ed_max$media_p21 / ed_min$media_p21), 1)
  cat("   • Mayor ingreso:", ed_max$nivel_educativo, "-", format(ed_max$media_p21, big.mark = ","), "$ (2023)\n")
  cat("   • Menor ingreso:", ed_min$nivel_educativo, "-", format(ed_min$media_p21, big.mark = ","), "$ (2023)\n")
  cat("   • Brecha multiplicativa:", brecha, "x\n")
}

cat("\n📁 ARCHIVOS PARA INFORME:\n")
cat("   • tabla_p21_anual_final.csv - Evolución anual P21\n")
cat("   • tabla_comparativa_final.csv - P21 vs P47T por año\n")
cat("   • p21_por_educacion.csv - Análisis por educación\n")

cat("\n🔍 HALLAZGOS PRINCIPALES:\n")

# Crecimiento más alto
if(nrow(tabla_p21_anual) > 1) {
  max_crecimiento <- tabla_p21_anual[which.max(tabla_p21_anual$var_p21, na.rm = TRUE), ]
  if(!is.na(max_crecimiento$var_p21)) {
    cat("   • Mayor crecimiento P21:", max_crecimiento$var_p21, "% en", max_crecimiento$ano, "\n")
  }
}

# Período COVID
covid_impact <- tabla_comparativa_anual %>% filter(ano %in% c(2020, 2021))
if(nrow(covid_impact) > 0) {
  ratio_covid <- mean(covid_impact$ratio_medio, na.rm = TRUE)
  cat("   • Ratio promedio COVID (2020-2021):", round(ratio_covid, 2), "\n")
}

# Tendencia reciente
if(nrow(tabla_p21_anual) >= 3) {
  tendencia_reciente <- tail(tabla_p21_anual, 3)
  crecimiento_promedio <- mean(tendencia_reciente$var_p21, na.rm = TRUE)
  cat("   • Crecimiento promedio 2022-2024:", round(crecimiento_promedio, 1), "%\n")
}

cat(rep("=", 80), "\n")

# Mostrar gráficos
cat("\n📊 VISUALIZACIONES:\n")
print(grafico_p21_solo)
cat("\n")
print(grafico_comparativo)
cat("\n")
print(grafico_ratios)
cat("\n")
print(grafico_educacion)