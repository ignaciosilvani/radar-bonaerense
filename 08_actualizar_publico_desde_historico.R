# ============================================================
# 08_actualizar_publico_desde_historico.R
# Actualiza el JSON público del Radar Bonaerense.
#
# IMPORTANTE:
#   - No descarga noticias.
#   - Usa noticias_historicas.rds para los datos.
#   - Usa vistas_noticias.rds para publicar las secciones
#     clasificadas por 05_generar_vistas.R (V2.9.1).
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" ACTUALIZACIÓN DE DATOS PÚBLICOS DESDE HISTÓRICO Y VISTAS\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. Detectar directorio del proyecto
# ------------------------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
archivo_script <- grep("^--file=", args, value = TRUE)

if (length(archivo_script) > 0) {
  ruta_script <- sub("^--file=", "", archivo_script[1])
  directorio_proyecto <- dirname(normalizePath(ruta_script))
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
ARCHIVO_VISTAS <- "vistas_noticias.rds"
ARCHIVO_PUBLICO <- "data/noticias_7d.json"
ZONA_HORARIA <- "America/Argentina/Buenos_Aires"

# ------------------------------------------------------------
# 3. Verificar archivos necesarios
# ------------------------------------------------------------
if (!file.exists(ARCHIVO_HISTORICO)) {
  stop("No se encontró el archivo histórico: ", ARCHIVO_HISTORICO)
}

if (!file.exists(ARCHIVO_VISTAS)) {
  stop(
    "No se encontró ", ARCHIVO_VISTAS,
    ". No se publicará el JSON para evitar conservar secciones antiguas. ",
    "Ejecutá primero 05_generar_vistas.R."
  )
}

# ------------------------------------------------------------
# 4. Cargar histórico y vistas clasificadas
# ------------------------------------------------------------
cat("Cargando histórico...\n")
historico <- readRDS(ARCHIVO_HISTORICO)
cat("Noticias históricas:", nrow(historico), "\n\n")

cat("Cargando vistas generadas por 05_generar_vistas.R...\n")
vistas <- readRDS(ARCHIVO_VISTAS)

if (is.null(vistas$historico) || !is.data.frame(vistas$historico)) {
  stop("El archivo de vistas no contiene un data.frame en vistas$historico.")
}

clasificadas <- vistas$historico
columnas_clasificacion <- c("url", "seccion")
columnas_faltantes <- setdiff(columnas_clasificacion, names(clasificadas))
if (length(columnas_faltantes) > 0) {
  stop(
    "Las vistas no contienen las columnas necesarias para transferir la clasificación: ",
    paste(columnas_faltantes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 5. Verificar estructura mínima del histórico
# ------------------------------------------------------------
columnas_necesarias <- c(
  "id_medio", "medio", "partido", "localidad", "tipo_medio",
  "titulo", "fecha", "url", "descripcion", "web", "feed_url"
)

faltantes <- setdiff(columnas_necesarias, names(historico))
if (length(faltantes) > 0) {
  stop(
    "El histórico no contiene las columnas necesarias:\n",
    paste(faltantes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 6. Transferir la sección clasificada desde las vistas
# ------------------------------------------------------------
# Se vincula por URL porque las vistas conservan las noticias del
# histórico y agregan la sección calculada por V2.9.1.
clave_url <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

clasificadas <- clasificadas[, columnas_clasificacion, drop = FALSE]
clasificadas$url <- clave_url(clasificadas$url)
clasificadas$seccion <- as.character(clasificadas$seccion)

# Una URL debe aportar una sola clasificación al cruce.
clasificadas <- clasificadas[
  !is.na(clasificadas$url) & !duplicated(clasificadas$url),
  , drop = FALSE
]

historico$url_clave_aux <- clave_url(historico$url)
indice_clasificacion <- match(historico$url_clave_aux, clasificadas$url)
coincide <- !is.na(indice_clasificacion)

# Mantener la sección calculada en las vistas para todas las noticias
# que coincidan; las no coincidentes conservan su valor histórico.
seccion_anterior <- if ("seccion" %in% names(historico)) {
  as.character(historico$seccion)
} else {
  rep(NA_character_, nrow(historico))
}
seccion_nueva <- rep(NA_character_, nrow(historico))
seccion_nueva[coincide] <- clasificadas$seccion[indice_clasificacion[coincide]]

if (!"seccion" %in% names(historico)) {
  historico$seccion <- seccion_anterior
}
historico$seccion <- as.character(historico$seccion)
historico$seccion[coincide] <- seccion_nueva[coincide]
historico$url_clave_aux <- NULL

cat("Cruce de clasificación completado.\n")
cat("Noticias del histórico:", nrow(historico), "\n")
cat("Noticias con URL coincidente en vistas:", sum(coincide), "\n")
cat("Noticias sin coincidencia en vistas:", sum(!coincide), "\n")
cat("Secciones actualizadas desde V2.9.1:", sum(coincide), "\n\n")

# ------------------------------------------------------------
# 7. Preparar fechas y columnas auxiliares
# ------------------------------------------------------------
historico$fecha <- as.POSIXct(historico$fecha, tz = ZONA_HORARIA)

if (!"fecha_captura" %in% names(historico)) {
  historico$fecha_captura <- NA
}
historico$fecha_captura <- as.POSIXct(
  historico$fecha_captura,
  tz = ZONA_HORARIA
)

# ------------------------------------------------------------
# 8. Seleccionar estructura pública
# ------------------------------------------------------------
columnas_publicas <- c(
  "id_medio", "medio", "partido", "localidad", "tipo_medio",
  "titulo", "fecha", "url", "descripcion", "web", "feed_url",
  "seccion", "fecha_captura"
)

publico <- historico[, intersect(columnas_publicas, names(historico)), drop = FALSE]

# ------------------------------------------------------------
# 9. Deduplicar por URL
# ------------------------------------------------------------
publico <- publico |>
  dplyr::distinct(url, .keep_all = TRUE)

# ------------------------------------------------------------
# 10. Filtrar últimos 7 días
# ------------------------------------------------------------
ahora <- Sys.time()
limite <- ahora - lubridate::days(7)

publico_7d <- publico |>
  dplyr::filter(
    !is.na(fecha),
    fecha >= limite
  ) |>
  dplyr::arrange(dplyr::desc(fecha))

# ------------------------------------------------------------
# 11. Crear directorio data si no existe
# ------------------------------------------------------------
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# ------------------------------------------------------------
# 12. Guardar JSON público
# ------------------------------------------------------------
jsonlite::write_json(
  publico_7d,
  ARCHIVO_PUBLICO,
  pretty = FALSE,
  auto_unbox = TRUE,
  na = "null"
)

# ------------------------------------------------------------
# 13. Resumen y diagnóstico
# ------------------------------------------------------------
tamano_mb <- file.info(ARCHIVO_PUBLICO)$size / 1024^2

cat("\n============================================================\n")
cat("              PROCESO TERMINADO\n")
cat("============================================================\n")
cat("Noticias históricas:", nrow(historico), "\n")
cat("Noticias públicas (7 días):", nrow(publico_7d), "\n")
cat("Archivo generado:", ARCHIVO_PUBLICO, "\n")
cat("Tamaño:", round(tamano_mb, 2), "MB\n\n")

cat("Distribución de secciones publicadas (7 días):\n")
print(publico_7d |>
  dplyr::count(seccion, sort = TRUE))

cat("\n============================================================\n")
cat(" ACTUALIZACIÓN PÚBLICA COMPLETADA - OK\n")
cat("============================================================\n")
cat(
  "Finalización:",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = ZONA_HORARIA),
  "\n\n"
)
