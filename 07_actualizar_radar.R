# ============================================================
# 07_actualizar_radar.R
#
# Actualizar Radar Bonaerense
#
# Flujo:
#
#   feeds_detectados.rds
#          ↓
#   03_obtener_noticias.R
#          ↓
#   04_persistir_noticias.R
#          ↓
#   05_generar_vistas.R
#          ↓
#   vistas_noticias.rds
#          ↓
#   data/noticias_7d.json
#
# Este script NO ejecuta 06_app.R
# ============================================================


# ============================================================
# 0. UBICACIÓN DEL PROYECTO
# ============================================================

# Cuando Rscript ejecuta este archivo, detectamos automáticamente
# la carpeta donde está ubicado 07_actualizar_radar.R.
#
# Esto evita depender del directorio de trabajo de Windows.

args <- commandArgs(trailingOnly = FALSE)

archivo_script <- grep(
  "^--file=",
  args,
  value = TRUE
)

if (length(archivo_script) > 0) {
  
  ruta_script <- normalizePath(
    sub("^--file=", "", archivo_script[1]),
    winslash = "/",
    mustWork = TRUE
  )
  
  setwd(
    dirname(ruta_script)
  )
  
}


# ============================================================
# CONFIGURACIÓN
# ============================================================

ARCHIVO_FEEDS <- "feeds_detectados.rds"
ARCHIVO_HISTORICO <- "noticias_historicas.rds"
ARCHIVO_VISTAS <- "vistas_noticias.rds"


inicio <- Sys.time()


# ============================================================
# INICIO
# ============================================================

cat("\n")
cat("============================================================\n")
cat("        ACTUALIZACIÓN RADAR BONAERENSE\n")
cat("============================================================\n")
cat("\n")

cat(
  "Directorio de trabajo:",
  getwd(),
  "\n"
)

cat(
  "Inicio:",
  format(inicio, "%Y-%m-%d %H:%M:%S"),
  "\n\n"
)


# ============================================================
# EJECUCIÓN
# ============================================================

tryCatch(
  
  {
    
    # ----------------------------------------------------------
    # 1. VERIFICAR ARCHIVO DE FEEDS
    # ----------------------------------------------------------
    
    cat("------------------------------------------------------------\n")
    cat("1. VERIFICANDO FEEDS\n")
    cat("------------------------------------------------------------\n")
    
    if (!file.exists(ARCHIVO_FEEDS)) {
      
      stop(
        "No se encontró el archivo '",
        ARCHIVO_FEEDS,
        "'. Ejecutar previamente 02_detectar_feeds.R."
      )
      
    }
    
    
    # ----------------------------------------------------------
    # 2. CARGAR FEEDS
    # ----------------------------------------------------------
    
    feeds <- readRDS(
      ARCHIVO_FEEDS
    )
    
    
    if (!is.data.frame(feeds)) {
      
      stop(
        "El archivo '",
        ARCHIVO_FEEDS,
        "' no contiene un data.frame/tibble."
      )
      
    }
    
    
    if (!"feed_url" %in% names(feeds)) {
      
      stop(
        "El objeto `feeds` no contiene la columna `feed_url`."
      )
      
    }
    
    
    feeds_ok <- feeds |>
      dplyr::filter(
        estado == "FEED_OK",
        !is.na(feed_url),
        nzchar(feed_url)
      )
    
    
    cat(
      "Feeds detectados:",
      nrow(feeds),
      "\n"
    )
    
    cat(
      "Feeds funcionando:",
      nrow(feeds_ok),
      "\n"
    )
    
    
    if (nrow(feeds_ok) == 0) {
      
      stop(
        "No hay feeds con estado FEED_OK."
      )
      
    }
    
    
    cat("\n")
    
    
    # ----------------------------------------------------------
    # 3. CARGAR 03
    # ----------------------------------------------------------
    
    cat("------------------------------------------------------------\n")
    cat("2. OBTENIENDO NOTICIAS\n")
    cat("------------------------------------------------------------\n")
    
    
    source(
      "03_obtener_noticias.R",
      local = .GlobalEnv
    )
    
    
    if (!exists("obtener_noticias")) {
      
      stop(
        "No se encontró la función `obtener_noticias()` después de cargar 03."
      )
      
    }
    
    
    noticias <- obtener_noticias(
      feeds
    )
    
    
    if (!is.data.frame(noticias)) {
      
      stop(
        "03_obtener_noticias.R no devolvió un data.frame/tibble."
      )
      
    }
    
    
    cat("\n")
    
    cat(
      "Noticias obtenidas:",
      nrow(noticias),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 4. CARGAR 04
    # ----------------------------------------------------------
    
    cat("\n")
    cat("------------------------------------------------------------\n")
    cat("3. ACTUALIZANDO HISTÓRICO\n")
    cat("------------------------------------------------------------\n")
    
    
    source(
      "04_persistir_noticias.R",
      local = .GlobalEnv
    )
    
    
    if (!exists("persistir_noticias")) {
      
      stop(
        "No se encontró la función `persistir_noticias()` después de cargar 04."
      )
      
    }
    
    
    historico <- persistir_noticias(
      noticias,
      archivo = ARCHIVO_HISTORICO
    )
    
    
    if (!is.data.frame(historico)) {
      
      stop(
        "04_persistir_noticias.R no devolvió un data.frame/tibble."
      )
      
    }
    
    
    cat("\n")
    
    cat(
      "Noticias en histórico:",
      nrow(historico),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 5. CARGAR 05
    # ----------------------------------------------------------
    
    cat("\n")
    cat("------------------------------------------------------------\n")
    cat("4. GENERANDO VISTAS\n")
    cat("------------------------------------------------------------\n")
    
    
    # IMPORTANTE:
    #
    # 05_generar_vistas.R detecta que existe `historico`
    # y ejecuta automáticamente:
    #
    #   vistas_noticias <- generar_vistas(historico)
    #
    # No volvemos a ejecutar generar_vistas() acá.
    
    
    source(
      "05_generar_vistas.R",
      local = .GlobalEnv
    )
    
    
    # ----------------------------------------------------------
    # 6. VERIFICAR RESULTADO
    # ----------------------------------------------------------
    
    if (!file.exists(ARCHIVO_VISTAS)) {
      
      stop(
        "05_generar_vistas.R terminó pero no se encontró '",
        ARCHIVO_VISTAS,
        "'."
      )
      
    }
    
    
    vistas_noticias <- readRDS(
      ARCHIVO_VISTAS
    )
    
    
    cat("\n")
    
    cat(
      "Archivo generado:",
      ARCHIVO_VISTAS,
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 7. GENERAR DATOS PÚBLICOS
    # ----------------------------------------------------------
    
    cat("\n")
    cat("------------------------------------------------------------\n")
    cat("5. GENERANDO DATOS PÚBLICOS (ÚLTIMOS 7 DÍAS)\n")
    cat("------------------------------------------------------------\n")
    
    
    # Creamos la carpeta data si todavía no existe.
    
    dir.create(
      "data",
      showWarnings = FALSE,
      recursive = TRUE
    )
    
    
    # Tomamos únicamente las noticias de los últimos 7 días.
    
    noticias_7d <- historico |>
      dplyr::filter(
        !is.na(fecha),
        fecha >= Sys.time() - lubridate::days(7)
      )
    
    
    # Guardamos el resultado en JSON.
    
    jsonlite::write_json(
      noticias_7d,
      "data/noticias_7d.json",
      auto_unbox = TRUE,
      pretty = FALSE,
      na = "null"
    )
    
    
    cat(
      "Noticias publicadas:",
      nrow(noticias_7d),
      "\n"
    )
    
    
    cat(
      "Tamaño JSON:",
      round(
        file.info("data/noticias_7d.json")$size / 1024^2,
        2
      ),
      "MB\n"
    )
    
    
    # ----------------------------------------------------------
    # 8. RESUMEN FINAL
    # ----------------------------------------------------------
    
    fin <- Sys.time()
    
    duracion <- difftime(
      fin,
      inicio,
      units = "secs"
    )
    
    
    cat("\n")
    cat("============================================================\n")
    cat("        ACTUALIZACIÓN COMPLETADA - OK\n")
    cat("============================================================\n")
    
    
    cat(
      "Noticias obtenidas:",
      nrow(noticias),
      "\n"
    )
    
    
    cat(
      "Noticias en histórico:",
      nrow(historico),
      "\n"
    )
    
    
    cat(
      "Noticias públicas (7 días):",
      nrow(noticias_7d),
      "\n"
    )
    
    
    cat(
      "Vistas disponibles:",
      paste(
        names(vistas_noticias),
        collapse = ", "
      ),
      "\n"
    )
    
    
    cat(
      "Duración:",
      round(
        as.numeric(duracion),
        1
      ),
      "segundos\n"
    )
    
    
    cat(
      "Finalización:",
      format(
        fin,
        "%Y-%m-%d %H:%M:%S"
      ),
      "\n"
    )
    
    
    cat("Estado: OK\n")
    
    cat("============================================================\n")
    cat("\n")
    
    
    # ----------------------------------------------------------
    # Resultado
    # ----------------------------------------------------------
    
    invisible(
      list(
        noticias = noticias,
        historico = historico,
        noticias_7d = noticias_7d,
        vistas = vistas_noticias,
        inicio = inicio,
        fin = fin,
        duracion = duracion,
        estado = "OK"
      )
    )
    
  },
  
  
  # ==========================================================
  # MANEJO DE ERRORES
  # ==========================================================
  
  error = function(e) {
    
    fin <- Sys.time()
    
    duracion <- difftime(
      fin,
      inicio,
      units = "secs"
    )
    
    
    cat("\n")
    cat("============================================================\n")
    cat("        ERROR EN LA ACTUALIZACIÓN\n")
    cat("============================================================\n")
    
    
    cat(
      "Error:",
      conditionMessage(e),
      "\n"
    )
    
    
    cat(
      "Hora:",
      format(
        fin,
        "%Y-%m-%d %H:%M:%S"
      ),
      "\n"
    )
    
    
    cat(
      "Duración:",
      round(
        as.numeric(duracion),
        1
      ),
      "segundos\n"
    )
    
    
    cat(
      "Estado: ERROR\n"
    )
    
    
    cat("============================================================\n")
    cat("\n")
    
    
    # Importantísimo:
    # devuelve un código de error a Windows.
    
    quit(
      save = "no",
      status = 1,
      runLast = FALSE
    )
    
  }
  
)