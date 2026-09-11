# ============================================================
# 09_actualizar_radar_completo.R
#
# Script maestro del Radar Bonaerense
#
# Ejecuta, en orden:
#
#   1. 07_actualizar_feeds.R
#   2. 07_actualizar_radar.R
#   3. 08_actualizar_publico_desde_historico.R
#
# IMPORTANTE:
#   NO modifica ninguno de los scripts anteriores.
#
# Objetivo:
#   Ejecutar toda la actualización del Radar Bonaerense
#   mediante un único comando.
#
# Optimización:
#   La etapa 3 NO vuelve a descargar noticias.
#   Utiliza directamente noticias_historicas.rds,
#   generado por 07_actualizar_radar.R.
#
# ============================================================


cat("\n")
cat("============================================================\n")
cat("              RADAR BONAERENSE\n")
cat("          ACTUALIZACIÓN COMPLETA\n")
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
# 2. Verificar scripts necesarios
# ------------------------------------------------------------

scripts_necesarios <- c(
  "07_actualizar_feeds.R",
  "07_actualizar_radar.R",
  "08_actualizar_publico_desde_historico.R"
)

faltantes <- scripts_necesarios[
  !file.exists(scripts_necesarios)
]

if (length(faltantes) > 0) {
  
  stop(
    paste0(
      "No se encontraron los siguientes scripts:\n",
      paste(faltantes, collapse = "\n")
    )
  )
  
}


# ------------------------------------------------------------
# 3. Función auxiliar para ejecutar cada etapa
# ------------------------------------------------------------

ejecutar_etapa <- function(
    archivo,
    nombre
) {
  
  cat("\n")
  cat("============================================================\n")
  cat(nombre, "\n")
  cat("============================================================\n")
  cat("\n")
  
  inicio <- Sys.time()
  
  resultado <- tryCatch(
    
    {
      
      source(
        archivo,
        local = .GlobalEnv
      )
      
      TRUE
      
    },
    
    error = function(e) {
      
      cat("\n")
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      cat("ERROR EN:", nombre, "\n")
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      cat("\n")
      
      cat(
        "Mensaje:",
        conditionMessage(e),
        "\n"
      )
      
      FALSE
      
    }
    
  )
  
  fin <- Sys.time()
  
  duracion <- as.numeric(
    difftime(
      fin,
      inicio,
      units = "secs"
    )
  )
  
  cat("\n")
  cat(
    "Duración de",
    nombre,
    ":",
    round(duracion, 1),
    "segundos\n"
  )
  
  if (!resultado) {
    
    stop(
      paste0(
        "La actualización se detuvo porque falló: ",
        nombre
      )
    )
    
  }
  
  invisible(TRUE)
  
}


# ------------------------------------------------------------
# 4. ETAPA 1
#    Actualizar listado de medios y feeds
# ------------------------------------------------------------

ejecutar_etapa(
  "07_actualizar_feeds.R",
  "ETAPA 1 — ACTUALIZAR FEEDS"
)


# ------------------------------------------------------------
# 5. ETAPA 2
#    Descargar noticias, actualizar histórico y vistas
# ------------------------------------------------------------

ejecutar_etapa(
  "07_actualizar_radar.R",
  "ETAPA 2 — ACTUALIZAR RADAR E HISTÓRICO"
)


# ------------------------------------------------------------
# 6. ETAPA 3
#    Actualizar JSON público desde el histórico
#
#    IMPORTANTE:
#    Este script NO descarga noticias nuevamente.
# ------------------------------------------------------------

ejecutar_etapa(
  "08_actualizar_publico_desde_historico.R",
  "ETAPA 3 — ACTUALIZAR DATOS PÚBLICOS DESDE HISTÓRICO"
)


# ------------------------------------------------------------
# 7. Verificación final
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("                 VERIFICACIÓN FINAL\n")
cat("============================================================\n")
cat("\n")


archivos_finales <- c(
  "feeds_detectados.rds",
  "noticias_historicas.rds",
  "vistas_noticias.rds",
  "data/noticias_7d.json"
)

for (archivo in archivos_finales) {
  
  existe <- file.exists(archivo)
  
  cat(
    ifelse(existe, "[OK] ", "[FALTA] "),
    archivo,
    "\n"
  )
  
}


# ------------------------------------------------------------
# 8. Verificar archivos faltantes
# ------------------------------------------------------------

faltantes_finales <- archivos_finales[
  !file.exists(archivos_finales)
]

cat("\n")

if (length(faltantes_finales) > 0) {
  
  cat("============================================================\n")
  cat("                 ACTUALIZACIÓN INCOMPLETA\n")
  cat("============================================================\n")
  cat("\n")
  
  cat(
    "Faltan los siguientes archivos:\n",
    paste(
      faltantes_finales,
      collapse = "\n"
    ),
    "\n"
  )
  
  stop(
    "La actualización terminó con archivos faltantes."
  )
  
}


# ------------------------------------------------------------
# 9. Estado final
# ------------------------------------------------------------

cat("============================================================\n")
cat("          ACTUALIZACIÓN COMPLETA - OK\n")
cat("============================================================\n")
cat("\n")

cat(
  "Finalización:",
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n"
)

cat("\n")

cat(
  "El Radar Bonaerense está actualizado.\n"
)

cat("\n")