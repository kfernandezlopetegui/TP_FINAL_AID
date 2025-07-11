# =============================================================================
# FIX PARA MEJORAR GRÁFICOS P21 - EVITAR CONFUSIÓN EN CAÍDA FINAL
# =============================================================================

# Opción 1: Verificar y filtrar último período si tiene pocos datos
verificar_ultimo_periodo <- function() {
  cat("🔍 Verificando último período...\n")
  
  # Revisar cantidad de casos en último trimestre
  ultimo_trim <- tail(estadisticas_p21_trimestral, 1)
  penultimo_trim <- tail(estadisticas_p21_trimestral, 2)[1,]
  
  cat("   Último período (", ultimo_trim$periodo, "): ", ultimo_trim$n_casos, " casos\n", sep = "")
  cat("   Penúltimo período (", penultimo_trim$periodo, "): ", penultimo_trim$n_casos, " casos\n", sep = "")
  
  # Si el último período tiene menos del 70% de casos que el penúltimo, sugerir filtro
  ratio_casos <- ultimo_trim$n_casos / penultimo_trim$n_casos
  
  if(ratio_casos < 0.7) {
    cat("⚠️ El último período tiene significativamente menos casos (", round(ratio_casos*100, 1), "%)\n", sep = "")
    cat("   Recomendación: Considerar excluir del gráfico o marcar como preliminar\n")
    return(FALSE)
  } else {
    cat("✅ El último período tiene datos comparables\n")
    return(TRUE)
  }
}

# Opción 2: Crear gráfico con último período marcado como preliminar
crear_grafico_p21_mejorado <- function(incluir_ultimo = TRUE, marcar_preliminar = TRUE) {
  
  cat("📈 Creando gráfico P21 mejorado...\n")
  
  # Preparar datos
  datos_grafico <- estadisticas_p21_trimestral
  
  # Opcionalmente excluir último período
  if(!incluir_ultimo) {
    datos_grafico <- datos_grafico[-nrow(datos_grafico), ]
    cat("   • Excluyendo último período por datos incompletos\n")
  }
  
  # Transformar datos para gráfico
  datos_plot <- datos_grafico %>%
    select(fecha, ano, media_p21, mediana_p21) %>%
    tidyr::pivot_longer(cols = c(media_p21, mediana_p21), names_to = "tipo", values_to = "valor") %>%
    mutate(
      tipo = case_when(
        tipo == "media_p21" ~ "Media",
        tipo == "mediana_p21" ~ "Mediana"
      ),
      # Marcar últimos 2 períodos como preliminares si se solicita
      preliminar = if(marcar_preliminar) {
        fecha >= max(fecha) - months(6)  # Últimos 6 meses
      } else {
        FALSE
      }
    )
  
  # Crear gráfico base
  p <- datos_plot %>%
    ggplot(aes(x = fecha, y = valor, color = tipo)) +
    # Líneas principales (datos consolidados)
    geom_line(data = filter(datos_plot, !preliminar), 
              aes(group = tipo), size = 1.4, alpha = 0.9) +
    geom_point(data = filter(datos_plot, !preliminar), 
               size = 2.8, alpha = 0.8)
  
  # Agregar datos preliminares si existen
  if(marcar_preliminar && any(datos_plot$preliminar)) {
    p <- p +
      # Líneas preliminares (punteadas)
      geom_line(data = filter(datos_plot, preliminar), 
                aes(group = tipo), size = 1.2, alpha = 0.7, linetype = "dashed") +
      geom_point(data = filter(datos_plot, preliminar), 
                 size = 2.5, alpha = 0.6, shape = 1)  # Círculos vacíos
  }
  
  # Completar gráfico
  p <- p +
    geom_vline(xintercept = as.Date("2020-03-01"), 
               color = "red", linetype = "dashed", alpha = 0.7) +
    geom_vline(xintercept = as.Date("2023-12-01"), 
               color = "green", linetype = "dotted", alpha = 0.7) +
    annotate("text", x = as.Date("2020-06-01"), 
             y = max(datos_plot$valor) * 0.9,
             label = "COVID-19", angle = 90, color = "red", size = 3.5) +
    annotate("text", x = as.Date("2023-09-01"), 
             y = max(datos_plot$valor) * 0.8,
             label = "Base 2023", angle = 90, color = "green", size = 3) +
    scale_color_manual(
      values = c("Media" = "#e74c3c", "Mediana" = "#3498db"),
      name = ""
    ) +
    labs(
      title = "Evolución de Ingresos de la Ocupación Principal (P21)",
      subtitle = paste0("Gran Buenos Aires 2016-2024 | Pesos constantes de 2023",
                        if(marcar_preliminar) " | Líneas punteadas: datos preliminares" else ""),
      x = "Período",
      y = "Ingreso Real P21 ($ de 2023)",
      caption = paste0("Fuente: EPH-INDEC | Elaboración propia\n",
                       "Nota: Solo ingresos de ocupación principal",
                       if(marcar_preliminar) " | Últimos datos pueden ser preliminares" else "")
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
  
  return(p)
}

# Opción 3: Crear gráfico hasta 2023 + proyección 2024
crear_grafico_p21_conservador <- function() {
  
  cat("📈 Creando gráfico P21 conservador (hasta 2023)...\n")
  
  # Filtrar solo hasta 2023
  datos_hasta_2023 <- estadisticas_p21_trimestral %>%
    filter(ano <= 2023)
  
  # Agregar promedio 2024 como punto final
  promedio_2024 <- estadisticas_p21_trimestral %>%
    filter(ano == 2024) %>%
    summarise(
      fecha = as.Date("2024-07-01"),  # Punto medio del año
      media_p21 = round(mean(media_p21, na.rm = TRUE)),
      mediana_p21 = round(mean(mediana_p21, na.rm = TRUE))
    )
  
  # Transformar datos
  datos_plot <- datos_hasta_2023 %>%
    select(fecha, media_p21, mediana_p21) %>%
    tidyr::pivot_longer(cols = c(media_p21, mediana_p21), names_to = "tipo", values_to = "valor") %>%
    mutate(tipo = case_when(
      tipo == "media_p21" ~ "Media",
      tipo == "mediana_p21" ~ "Mediana"
    ))
  
  # Punto 2024
  punto_2024 <- promedio_2024 %>%
    tidyr::pivot_longer(cols = c(media_p21, mediana_p21), names_to = "tipo", values_to = "valor") %>%
    mutate(tipo = case_when(
      tipo == "media_p21" ~ "Media",
      tipo == "mediana_p21" ~ "Mediana"
    ))
  
  # Crear gráfico
  p <- datos_plot %>%
    ggplot(aes(x = fecha, y = valor, color = tipo, group = tipo)) +
    geom_line(size = 1.4, alpha = 0.9) +
    geom_point(size = 2.8, alpha = 0.8) +
    # Agregar punto 2024
    geom_point(data = punto_2024, size = 4, alpha = 0.8, shape = 18) +  # Diamante
    # Referencias
    geom_vline(xintercept = as.Date("2020-03-01"), 
               color = "red", linetype = "dashed", alpha = 0.7) +
    geom_vline(xintercept = as.Date("2023-12-01"), 
               color = "green", linetype = "dotted", alpha = 0.7) +
    # Anotaciones
    annotate("text", x = as.Date("2020-06-01"), 
             y = max(datos_plot$valor) * 0.9,
             label = "COVID-19", angle = 90, color = "red", size = 3.5) +
    annotate("text", x = as.Date("2024-07-01"), 
             y = max(punto_2024$valor) * 1.1,
             label = "Promedio\n2024", color = "black", size = 3, hjust = 0.5) +
    scale_color_manual(
      values = c("Media" = "#e74c3c", "Mediana" = "#3498db"),
      name = ""
    ) +
    labs(
      title = "Evolución de Ingresos de la Ocupación Principal (P21)",
      subtitle = "Gran Buenos Aires 2016-2024 | Pesos constantes de 2023 | Punto 2024: promedio anual",
      x = "Período",
      y = "Ingreso Real P21 ($ de 2023)",
      caption = "Fuente: EPH-INDEC | Elaboración propia\nNota: Solo ingresos de ocupación principal, excluye transferencias"
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
  
  return(p)
}

# =============================================================================
# EJECUTAR DIAGNÓSTICO Y CREAR GRÁFICOS MEJORADOS
# =============================================================================

# Verificar último período
periodo_ok <- verificar_ultimo_periodo()

# Crear gráficos alternativos
cat("\n📊 Generando gráficos alternativos...\n")

# Opción 1: Gráfico con datos preliminares marcados
grafico_preliminar <- crear_grafico_p21_mejorado(incluir_ultimo = TRUE, marcar_preliminar = TRUE)
guardar_grafico(grafico_preliminar, "evolucion_p21_preliminar", ancho = 14, alto = 9)

# Opción 2: Gráfico conservador hasta 2023
grafico_conservador <- crear_grafico_p21_conservador()
guardar_grafico(grafico_conservador, "evolucion_p21_hasta_2023", ancho = 14, alto = 9)

cat("✅ Gráficos alternativos generados:\n")
cat("   • evolucion_p21_preliminar.png - Con últimos datos marcados como preliminares\n")
cat("   • evolucion_p21_hasta_2023.png - Conservador, promedio 2024 como punto\n")

# Mostrar gráficos
print(grafico_preliminar)
cat("\n")
print(grafico_conservador)