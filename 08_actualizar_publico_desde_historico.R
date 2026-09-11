# ============================================================
# 08_actualizar_publico_desde_historico.R
#
# Actualizar datos públicos del Radar Bonaerense
# utilizando el histórico ya actualizado.
#
# IMPORTANTE:
#   Este script NO descarga noticias.
#   Utiliza noticias_historicas.rds generado previamente por
#   07_actualizar_radar.R
#
# Flujo:
#
#   07_actualizar_radar.R
#          ↓
#   noticias_historicas.rds
#          ↓
#   08_actualizar_publico_desde_historico.R
#          ↓
#   data/noticias_7d.json
#
# ============================================================


cat("\n")
cat("============================================================\n")
cat("     ACTUALIZACIÓN DE DATOS PÚBLICOS DESDE HISTÓRICO\n")
cat("============================================================\n")
cat("\n")


# ------------------------------------------------------------
# 1. Detectar directorio del proyecto
# ------------------------------------------------------------

args <- commandArgs(trailingOnly = FALSE)

archivo_script <- grep(
  "^--file=",
  args,
  value = TRUE
)

if (length(archivo_script) > 0) {
  
  ruta_script <- sub(
    "^--file=",
    "",
    archivo_script[1]
  )
  
  directorio_proyecto <- dirname(
    normalizePath(ruta_script)
  )
  
  setwd(directorio_proyecto)
  
} else {
  
  directorio_proyecto <- getwd()
  
}


cat("Directorio del proyecto:\n")
cat(normalizePath(getwd()), "\n\n")


# ------------------------------------------------------------
# 2. Configuración
# ------------------------------------------------------------

ARCHIVO_HISTORICO <- "noticias_historicas.rds"

ARCHIVO_PUBLICO <- "data/noticias_7d.json"

ZONA_HORARIA <- "America/Argentina/Buenos_Aires"


# ------------------------------------------------------------
# 3. Verificar histórico
# ------------------------------------------------------------

if (!file.exists(ARCHIVO_HISTORICO)) {
  
  stop(
    paste0(
      "No se encontró el archivo histórico: ",
      ARCHIVO_HISTORICO
    )
  )
  
}


# ------------------------------------------------------------
# 4. Cargar histórico
# ------------------------------------------------------------

cat("Cargando histórico...\n")

historico <- readRDS(
  ARCHIVO_HISTORICO
)

cat(
  "Noticias históricas:",
  nrow(historico),
  "\n\n"
)


# ------------------------------------------------------------
# 5. Verificar estructura mínima
# ------------------------------------------------------------

columnas_necesarias <- c(
  "id_medio",
  "medio",
  "partido",
  "localidad",
  "tipo_medio",
  "titulo",
  "fecha",
  "url",
  "descripcion",
  "web",
  "feed_url"
)

faltantes <- setdiff(
  columnas_necesarias,
  names(historico)
)

if (length(faltantes) > 0) {
  
  stop(
    paste0(
      "El histórico no contiene las columnas necesarias:\n",
      paste(faltantes, collapse = ", ")
    )
  )
  
}


# ------------------------------------------------------------
# 6. Preparar fechas
# ------------------------------------------------------------

historico$fecha <- as.POSIXct(
  historico$fecha,
  tz = ZONA_HORARIA
)


# ------------------------------------------------------------
# 7. Agregar fecha de captura si no existe
# ------------------------------------------------------------

if (!"fecha_captura" %in% names(historico)) {
  
  historico$fecha_captura <- NA
  
}


historico$fecha_captura <- as.POSIXct(
  historico$fecha_captura,
  tz = ZONA_HORARIA
)


# ------------------------------------------------------------
# 8. Asegurar columna seccion
# ------------------------------------------------------------

if (!"seccion" %in% names(historico)) {
  
  historico$seccion <- NA_character_
  
}


# ------------------------------------------------------------
# 9. Seleccionar estructura pública
# ------------------------------------------------------------

columnas_publicas <- c(
  "id_medio",
  "medio",
  "partido",
  "localidad",
  "tipo_medio",
  "titulo",
  "fecha",
  "url",
  "descripcion",
  "web",
  "feed_url",
  "seccion",
  "fecha_captura"
)

publico <- historico[
  ,
  intersect(
    columnas_publicas,
    names(historico)
  )
]


# ------------------------------------------------------------
# 10. Deduplicar por URL
# ------------------------------------------------------------

publico <- publico |>
  dplyr::distinct(
    url,
    .keep_all = TRUE
  )


# ------------------------------------------------------------
# 11. Filtrar últimos 7 días
# ------------------------------------------------------------

ahora <- Sys.time()

limite <- ahora - lubridate::days(7)

publico_7d <- publico |>
  dplyr::filter(
    !is.na(fecha),
    fecha >= limite
  ) |>
  dplyr::arrange(
    dplyr::desc(fecha)
  )


# ------------------------------------------------------------
# 12. Crear directorio data si no existe
# ------------------------------------------------------------

if (!dir.exists("data")) {
  
  dir.create(
    "data",
    recursive = TRUE
  )
  
}


# ------------------------------------------------------------
# 13. Guardar JSON público
# ------------------------------------------------------------

jsonlite::write_json(
  publico_7d,
  ARCHIVO_PUBLICO,
  pretty = FALSE,
  auto_unbox = TRUE,
  na = "null"
)


# ------------------------------------------------------------
# 14. Información del archivo generado
# ------------------------------------------------------------

tamano_mb <- file.info(
  ARCHIVO_PUBLICO
)$size / 1024^2


cat("\n")
cat("============================================================\n")
cat("              PROCESO TERMINADO\n")
cat("============================================================\n")

cat(
  "Noticias históricas:",
  nrow(historico),
  "\n"
)

cat(
  "Noticias públicas (7 días):",
  nrow(publico_7d),
  "\n"
)

cat(
  "Archivo generado:",
  ARCHIVO_PUBLICO,
  "\n"
)

cat(
  "Tamaño:",
  round(tamano_mb, 2),
  "MB\n"
)

cat("\n")
cat("============================================================\n")
cat("     ACTUALIZACIÓN PÚBLICA COMPLETADA - OK\n")
cat("============================================================\n")

cat(
  "Finalización:",
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n"
)

cat("\n")