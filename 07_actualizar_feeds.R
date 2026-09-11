# ============================================================
# 07_actualizar_feeds.R
#
# Obtener medios desde la API pública del Mapa de Medios
# y detectar sus feeds RSS / Atom.
#
# Este script NO modifica el histórico de noticias.
# ============================================================


# ------------------------------------------------------------
# PAQUETES
# ------------------------------------------------------------

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)


# ------------------------------------------------------------
# CONFIGURACIÓN
# ------------------------------------------------------------

URL_API <- paste0(
  "https://mapademedios.comunicacionpublica.gba.gob.ar",
  "/api/medios/publico"
)

RESULTADOS_POR_PAGINA <- 50


# ------------------------------------------------------------
# 1. OBTENER MEDIOS DE UNA PÁGINA
# ------------------------------------------------------------

obtener_pagina_medios <- function(pagina) {
  
  url <- paste0(
    URL_API,
    "?pagina=",
    pagina,
    "&resultados=",
    RESULTADOS_POR_PAGINA
  )
  
  cat("Consultando página:", pagina, "\n")
  
  request(url) |>
    req_perform() |>
    resp_body_json()
}


# ------------------------------------------------------------
# 2. OBTENER TODOS LOS MEDIOS
# ------------------------------------------------------------

primera_respuesta <- obtener_pagina_medios(1)

paginas <- primera_respuesta$paginas

cat("\nPáginas totales:", paginas, "\n")


respuestas <- map(
  seq_len(paginas),
  obtener_pagina_medios
)


# ------------------------------------------------------------
# 3. UNIR MEDIOS
# ------------------------------------------------------------

medios_lista <- respuestas |>
  map("medios") |>
  purrr::flatten()


cat(
  "Medios obtenidos:",
  length(medios_lista),
  "\n"
)


medios_df <- tibble(
  medio = medios_lista
) |>
  tidyr::unnest_wider(medio)


cat(
  "Filas en medios_df:",
  nrow(medios_df),
  "\n"
)


# ------------------------------------------------------------
# 4. PREPARAR ESTRUCTURA PARA 02_detectar_feeds.R
# ------------------------------------------------------------

# Primero inspeccionamos los nombres provenientes de la API.

cat("\nColumnas disponibles en la API:\n")

print(names(medios_df))


# ------------------------------------------------------------
# IMPORTANTE
# ------------------------------------------------------------
#
# 02_detectar_feeds.R necesita exactamente:
#
# id
# nombre
# web
# partido
# localidad
# tipo
#
# Si los nombres de la API coinciden, esto funciona
# directamente. Si alguno difiere, lo adaptamos aquí.
# ------------------------------------------------------------


medios_para_feeds <- medios_df |>
  select(
    id,
    nombre,
    web,
    partido,
    localidad,
    tipo
  )


# ------------------------------------------------------------
# 5. CARGAR DETECTOR DE FEEDS
# ------------------------------------------------------------

source(
  "02_detectar_feeds.R"
)


# ------------------------------------------------------------
# 6. DETECTAR FEEDS
# ------------------------------------------------------------

cat("\n")
cat("============================================\n")
cat(" DETECCIÓN DE FEEDS\n")
cat("============================================\n\n")


feeds_detectados <- detectar_feeds(
  medios_df = medios_para_feeds,
  archivo = "feeds_detectados.rds"
)


# ------------------------------------------------------------
# 7. RESUMEN
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" RESUMEN\n")
cat("============================================\n\n")


print(
  feeds_detectados |>
    count(
      estado,
      sort = TRUE
    )
)


cat(
  "\nMedios con FEED_OK:",
  sum(feeds_detectados$estado == "FEED_OK", na.rm = TRUE),
  "\n"
)


cat(
  "Total de medios:",
  nrow(feeds_detectados),
  "\n"
)


cat("\nProceso terminado.\n")