# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 02_carga_datos.R - Carga y preparación de datos EPH
# =============================================================================

# Verificar que el setup esté ejecutado
if(!exists("rutas")) {
  stop("❌ Ejecuta primero: source('scripts/01_setup.R')")
}

if(!exists("cargar_periodo_txt")) {
  stop("❌ Ejecuta primero: source('scripts/99_funciones.R')")
}

# Verificar constantes necesarias
if(!exists("ANIO_INICIO")) {
  cat("⚠️ Constante ANIO_INICIO no encontrada, usando valor por defecto: 2016\n")
  ANIO_INICIO <- 2016
}

if(!exists("ANIO_FIN")) {
  cat("⚠️ Constante ANIO_FIN no encontrada, usando valor por defecto: 2024\n")
  ANIO_FIN <- 2024
}

if(!exists("TRIMESTRES")) {
  cat("⚠️ Constante TRIMESTRES no encontrada, usando valor por defecto: 1:4\n")
  TRIMESTRES <- 1:4
}

if(!exists("CODIGOS_GBA")) {
  cat("⚠️ Constante CODIGOS_GBA no encontrada, usando valor por defecto: c(32, 33)\n")
  CODIGOS_GBA <- c(32, 33)
}

cat("🔄 Iniciando carga de datos EPH...\n")

# =============================================================================
# CONFIGURACIÓN DE RUTAS DE DATOS
# =============================================================================

# Ruta donde están los datos (adaptar según tu estructura actual)
# Opción 1: Si ya moviste los datos a la nueva estructura
ruta_datos_fuente <- rutas$datos_raw

# Opción 2: Si mantienes la ruta original (temporalmente)
# ruta_datos_fuente <- "C:/Users/kflop/OneDrive/Documentos/TP_FINAL/microbase/datos_descomprimidos"

cat("📁 Ruta de datos fuente:", ruta_datos_fuente, "\n")

# Verificar que la ruta existe
if(!dir.exists(ruta_datos_fuente)) {
  stop("❌ La ruta de datos no existe: ", ruta_datos_fuente)
}

# =============================================================================
# DESCUBRIMIENTO DE PERÍODOS DISPONIBLES
# =============================================================================

cat("🔍 Buscando períodos disponibles...\n")

# Buscar todas las carpetas de períodos
carpetas_periodos <- list.dirs(ruta_datos_fuente, recursive = FALSE)
nombres_periodos <- basename(carpetas_periodos)

# Filtrar solo carpetas que coincidan con el patrón AAAA_TX
patron_periodo <- "^[0-9]{4}_T[1-4]$"
nombres_validos <- nombres_periodos[grepl(patron_periodo, nombres_periodos)]

cat("📊 Períodos encontrados:", length(nombres_validos), "\n")
if(length(nombres_validos) > 0) {
  cat("📋 Lista de períodos:\n")
  for(periodo in sort(nombres_validos)) {
    cat("   -", periodo, "\n")
  }
} else {
  stop("❌ No se encontraron períodos válidos en: ", ruta_datos_fuente)
}

# =============================================================================
# CARGA PROGRESIVA DE DATOS
# =============================================================================

cat("\n🔄 Iniciando carga de datos...\n")

# Inicializar contenedor de datos
datos_eph_raw <- list()
errores_carga <- c()

# Contador de progreso
total_periodos <- length(nombres_validos)
contador <- 0

# Cargar cada período
for(periodo in sort(nombres_validos)) {
  
  contador <- contador + 1
  cat(sprintf("\n[%d/%d] Cargando período: %s\n", contador, total_periodos, periodo))
  
  # Ruta completa de la carpeta del período
  carpeta_periodo <- file.path(ruta_datos_fuente, periodo)
  
  # Intentar cargar el período
  tryCatch({
    
    # Cargar datos del período
    datos_temp <- cargar_periodo_txt(carpeta_periodo)
    
    # Verificar que se cargaron datos
    if(length(datos_temp) > 0) {
      
      # Extraer año y trimestre del nombre (formato AAAA_TX)
      ano <- as.numeric(str_extract(periodo, "^[0-9]{4}"))
      trimestre <- as.numeric(str_extract(periodo, "(?<=_T)[0-9]$"))
      
      # Validar año y trimestre
      if(ano >= ANIO_INICIO && ano <= ANIO_FIN && trimestre %in% TRIMESTRES) {
        
        # Agregar información temporal a los datos
        if(!is.null(datos_temp$personas)) {
          datos_temp$personas$ANO4 <- ano
          datos_temp$personas$TRIMESTRE <- trimestre
        }
        
        if(!is.null(datos_temp$hogares)) {
          datos_temp$hogares$ANO4 <- ano
          datos_temp$hogares$TRIMESTRE <- trimestre
        }
        
        # Guardar en la lista principal
        datos_eph_raw[[periodo]] <- datos_temp
        
        # Mostrar resumen del período cargado
        if(!is.null(datos_temp$personas)) {
          cat("   ✅ Personas:", format(nrow(datos_temp$personas), big.mark = ","), "registros\n")
        }
        if(!is.null(datos_temp$hogares)) {
          cat("   ✅ Hogares:", format(nrow(datos_temp$hogares), big.mark = ","), "registros\n")
        }
        
      } else {
        cat("   ⚠️ Período fuera del rango de análisis\n")
      }
      
    } else {
      cat("   ❌ No se pudieron cargar datos\n")
      errores_carga <- c(errores_carga, periodo)
    }
    
  }, error = function(e) {
    cat("   ❌ Error al cargar:", e$message, "\n")
    errores_carga <- c(errores_carga, periodo)
  })
}

# =============================================================================
# PROCESAMIENTO PARA GBA
# =============================================================================

cat("\n🔄 Procesando datos para GBA...\n")

# Aplicar filtros para GBA
datos_gba <- map(datos_eph_raw, procesar_datos_gba)

# Eliminar períodos vacíos
datos_gba <- datos_gba[lengths(datos_gba) > 0]

cat("✅ Períodos procesados para GBA:", length(datos_gba), "\n")

# =============================================================================
# VERIFICACIÓN DE DATOS CARGADOS
# =============================================================================

cat("\n📊 Verificando estructura de datos...\n")

# Verificar estructura usando función auxiliar
estructura_datos <- verificar_estructura_datos(datos_gba)

# Mostrar resumen
cat("\n=== RESUMEN DE DATOS CARGADOS ===\n")
print(estructura_datos)

# Estadísticas globales
total_personas <- sum(estructura_datos$Personas_Filas)
total_hogares <- sum(estructura_datos$Hogares_Filas)

cat("\n📈 ESTADÍSTICAS GLOBALES:\n")
cat("   Total períodos:", nrow(estructura_datos), "\n")
cat("   Total registros personas:", format(total_personas, big.mark = ","), "\n")
cat("   Total registros hogares:", format(total_hogares, big.mark = ","), "\n")

# Verificar cobertura temporal
anos_disponibles <- sort(unique(estructura_datos$Periodo %>% str_extract("^[0-9]{4}") %>% as.numeric()))
cat("   Años cubiertos:", paste(range(anos_disponibles), collapse = " - "), "\n")

# =============================================================================
# VERIFICACIÓN DE CALIDAD
# =============================================================================

cat("\n🔍 Verificación de calidad...\n")

# Verificar un período de ejemplo para control de calidad
periodo_ejemplo <- names(datos_gba)[1]
cat("📋 Verificando período ejemplo:", periodo_ejemplo, "\n")

if(!is.null(datos_gba[[periodo_ejemplo]]$personas)) {
  
  ejemplo_personas <- datos_gba[[periodo_ejemplo]]$personas
  
  # Verificar distribución por aglomerado
  dist_aglo <- ejemplo_personas %>%
    count(AGLOMERADO, wt = PONDERA, name = "Población") %>%
    mutate(
      Aglomerado = case_when(
        AGLOMERADO == 32 ~ "CABA",
        AGLOMERADO == 33 ~ "Partidos GBA",
        TRUE ~ "Otro"
      ),
      Porcentaje = round(Población / sum(Población) * 100, 1)
    )
  
  cat("📊 Distribución por aglomerado en", periodo_ejemplo, ":\n")
  print(dist_aglo)
  
  # Verificar distribución por edad
  cat("\n📊 Distribución etaria:\n")
  cat("   Edad mínima:", min(ejemplo_personas$CH06, na.rm = TRUE), "\n")
  cat("   Edad máxima:", max(ejemplo_personas$CH06, na.rm = TRUE), "\n")
  cat("   Edad promedio:", round(weighted.mean(ejemplo_personas$CH06, ejemplo_personas$PONDERA, na.rm = TRUE), 1), "\n")
  
  # Verificar condición de actividad
  dist_estado <- ejemplo_personas %>%
    filter(CH06 >= 10) %>%  # Solo PET
    count(ESTADO, wt = PONDERA, name = "Población") %>%
    mutate(
      Estado = case_when(
        ESTADO == 1 ~ "Ocupado",
        ESTADO == 2 ~ "Desocupado", 
        ESTADO == 3 ~ "Inactivo",
        TRUE ~ "Otro"
      ),
      Porcentaje = round(Población / sum(Población) * 100, 1)
    )
  
  cat("\n📊 Distribución por condición de actividad (≥10 años):\n")
  print(dist_estado)
}

# =============================================================================
# GUARDAR DATOS PROCESADOS
# =============================================================================

cat("\n💾 Guardando datos procesados...\n")

# Guardar datos GBA procesados
archivo_gba <- file.path(rutas$datos_procesados, "datos_gba_2016_2024.RData")
save(datos_gba, file = archivo_gba)
cat("✅ Datos GBA guardados en:", archivo_gba, "\n")

# Guardar estructura de datos
archivo_estructura <- file.path(rutas$datos_procesados, "estructura_datos.csv")
write_csv(estructura_datos, archivo_estructura)
cat("✅ Estructura guardada en:", archivo_estructura, "\n")

# Guardar metadatos
metadatos <- list(
  fecha_procesamiento = Sys.time(),
  periodos_cargados = names(datos_gba),
  total_personas = total_personas,
  total_hogares = total_hogares,
  anos_cubiertos = anos_disponibles,
  errores_carga = errores_carga,
  ruta_fuente = ruta_datos_fuente
)

archivo_metadatos <- file.path(rutas$datos_procesados, "metadatos_carga.RData")
save(metadatos, file = archivo_metadatos)
cat("✅ Metadatos guardados en:", archivo_metadatos, "\n")

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n" + rep("=", 60), "\n")
cat("✅ CARGA DE DATOS COMPLETADA\n")
cat(rep("=", 60), "\n")

cat("📊 RESUMEN FINAL:\n")
cat("   • Períodos cargados:", length(datos_gba), "\n")
cat("   • Registros de personas:", format(total_personas, big.mark = ","), "\n")
cat("   • Registros de hogares:", format(total_hogares, big.mark = ","), "\n")
cat("   • Cobertura temporal:", paste(range(anos_disponibles), collapse = " - "), "\n")

if(length(errores_carga) > 0) {
  cat("⚠️  Errores en:", length(errores_carga), "períodos\n")
  cat("   • Períodos con errores:", paste(errores_carga, collapse = ", "), "\n")
}



cat("\n📁 DATOS DISPONIBLES EN MEMORIA:\n")
cat("   • datos_gba: Lista con datos por período\n")
cat("   • estructura_datos: Resumen de la estructura\n")
cat("   • metadatos: Información de la carga\n")

cat(rep("=", 60), "\n")

# Limpiar variables temporales
rm(datos_eph_raw, datos_temp, contador, total_periodos, carpeta_periodo, 
   periodo_ejemplo, ejemplo_personas, dist_aglo, dist_estado,
   archivo_gba, archivo_estructura, archivo_metadatos)
