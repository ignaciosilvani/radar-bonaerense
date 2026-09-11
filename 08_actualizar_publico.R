# ============================================================
# 08_actualizar_publico.R
#
# Actualizar datos públicos del Radar Bonaerense
#
# Este script NO modifica el histórico.
#
# Flujo:
#
#   feeds_detectados.rds
#          ↓
#   03_obtener_noticias.R
#          ↓
#   noticias nuevas
#          +
#   data/noticias_7d.json existente
#          ↓
#   unir + deduplicar por URL
#          ↓
#   últimos 7 días
#          ↓
#   data/noticias_7d.json
#
# La lógica de deduplicación por URL replica la utilizada
# en 04_persistir_noticias.R.
#
# ============================================================


# ============================================================
# 0. UBICACIÓN DEL PROYECTO
# ============================================================

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

ARCHIVO_PUBLICO <- "data/noticias_7d.json"

# Zona horaria utilizada por el proyecto
ZONA_HORARIA <- "America/Argentina/Buenos_Aires"


inicio <- Sys.time()


# ============================================================
# INICIO
# ============================================================

cat("\n")
cat("============================================================\n")
cat("        ACTUALIZACIÓN DATOS PÚBLICOS\n")
cat("        RADAR BONAERENSE\n")
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
    # 1. VERIFICAR FEEDS
    # ----------------------------------------------------------
    
    cat("------------------------------------------------------------\n")
    cat("1. VERIFICANDO FEEDS\n")
    cat("------------------------------------------------------------\n")
    
    
    if (!file.exists(ARCHIVO_FEEDS)) {
      
      stop(
        "No se encontró el archivo '",
        ARCHIVO_FEEDS,
        "'."
      )
      
    }
    
    
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
    
    
    # ----------------------------------------------------------
    # 2. CARGAR DATOS PÚBLICOS EXISTENTES
    # ----------------------------------------------------------
    
    cat("\n")
    cat("------------------------------------------------------------\n")
    cat("2. CARGANDO DATOS PÚBLICOS EXISTENTES\n")
    cat("------------------------------------------------------------\n")
    
    
    if (file.exists(ARCHIVO_PUBLICO)) {
      
      noticias_existentes <- jsonlite::read_json(
        ARCHIVO_PUBLICO,
        simplifyVector = TRUE
      )
      
      
      if (!is.data.frame(noticias_existentes)) {
        
        stop(
          "El archivo '",
          ARCHIVO_PUBLICO,
          "' no pudo convertirse en un data.frame."
        )
        
      }
      
      
      # --------------------------------------------------------
      # IMPORTANTE:
      #
      # jsonlite lee las fechas del JSON como character.
      #
      # Las noticias nuevas provenientes de 03 tienen fecha
      # como POSIXct/datetime.
      #
      # Convertimos aquí las fechas existentes nuevamente a
      # POSIXct para que puedan combinarse correctamente.
      # --------------------------------------------------------
      
      if ("fecha" %in% names(noticias_existentes)) {
        
        noticias_existentes$fecha <- lubridate::ymd_hms(
          noticias_existentes$fecha,
          tz = ZONA_HORARIA,
          quiet = TRUE
        )
        
      } else {
        
        stop(
          "El archivo '",
          ARCHIVO_PUBLICO,
          "' no contiene la columna 'fecha'."
        )
        
      }
      
      
      # --------------------------------------------------------
      # NORMALIZAR FECHA DE CAPTURA
      #
      # El JSON lee fecha_captura como character.
      # Las noticias nuevas utilizan Sys.time(), que devuelve
      # POSIXct/datetime.
      #
      # Convertimos la columna existente a POSIXct para que
      # bind_rows() pueda combinar ambos objetos.
      # --------------------------------------------------------
      
      if ("fecha_captura" %in% names(noticias_existentes)) {
        
        noticias_existentes$fecha_captura <- lubridate::ymd_hms(
          noticias_existentes$fecha_captura,
          tz = ZONA_HORARIA,
          quiet = TRUE
        )
        
      }
      
      
      cat(
        "Noticias existentes:",
        nrow(noticias_existentes),
        "\n"
      )
      
    } else {
      
      noticias_existentes <- tibble::tibble()
      
      cat(
        "No existe archivo público previo.\n"
      )
      
    }
    
    
    # ----------------------------------------------------------
    # 3. OBTENER NOTICIAS NUEVAS
    # ----------------------------------------------------------
    
    cat("\n")
    cat("------------------------------------------------------------\n")
    cat("3. OBTENIENDO NOTICIAS NUEVAS\n")
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
    
    
    noticias_nuevas <- obtener_noticias(
      feeds
    )
    
    
    if (!is.data.frame(noticias_nuevas)) {
      
      stop(
        "03_obtener_noticias.R no devolvió un data.frame/tibble."
      )
      
    }
    
    
    cat(
      "Noticias nuevas obtenidas:",
      nrow(noticias_nuevas),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 4. PREPARAR NOTICIAS NUEVAS
    # ----------------------------------------------------------
    
    # Igual que en 04:
    #
    # Se conserva la estructura producida por 03.
    # La fecha_captura se registra para esta ejecución.
    
    
    noticias_nuevas <- noticias_nuevas |>
      dplyr::mutate(
        fecha_captura = Sys.time()
      ) |>
      dplyr::select(
        id_medio,
        medio,
        partido,
        localidad,
        tipo_medio,
        titulo,
        fecha,
        url,
        descripcion,
        web,
        feed_url,
        seccion,
        fecha_captura
      )
    
    
    # ----------------------------------------------------------
    # 4.1 NORMALIZAR TIPO DE FECHA
    # ----------------------------------------------------------
    
    # Aseguramos que las noticias nuevas utilicen también
    # POSIXct, igual que las noticias existentes recuperadas
    # desde el JSON.
    
    noticias_nuevas$fecha <- as.POSIXct(
      noticias_nuevas$fecha,
      tz = ZONA_HORARIA
    )
    
    
    # ----------------------------------------------------------
    # 5. ELIMINAR DUPLICADOS DE LA DESCARGA
    # ----------------------------------------------------------
    
    noticias_nuevas <- noticias_nuevas |>
      dplyr::distinct(
        url,
        .keep_all = TRUE
      )
    
    
    cat(
      "Noticias únicas recibidas:",
      nrow(noticias_nuevas),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 6. IDENTIFICAR NOTICIAS REALMENTE NUEVAS
    # ----------------------------------------------------------
    
    if (nrow(noticias_existentes) == 0) {
      
      noticias_para_agregar <- noticias_nuevas
      
    } else {
      
      urls_existentes <- noticias_existentes$url
      
      noticias_para_agregar <- noticias_nuevas |>
        dplyr::filter(
          !url %in% urls_existentes
        )
      
    }
    
    
    cat(
      "Noticias nuevas para agregar:",
      nrow(noticias_para_agregar),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 7. UNIR DATOS
    # ----------------------------------------------------------
    
    if (nrow(noticias_existentes) == 0) {
      
      combinado <- noticias_para_agregar
      
    } else {
      
      combinado <- dplyr::bind_rows(
        noticias_existentes,
        noticias_para_agregar
      )
      
    }
    
    
    cat(
      "Noticias combinadas:",
      nrow(combinado),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 8. CONTROL FINAL DE DUPLICADOS
    # ----------------------------------------------------------
    
    combinado <- combinado |>
      dplyr::distinct(
        url,
        .keep_all = TRUE
      )
    
    
    cat(
      "Noticias después de deduplicar:",
      nrow(combinado),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 9. FILTRAR ÚLTIMOS 7 DÍAS
    # ----------------------------------------------------------
    
    noticias_7d <- combinado |>
      dplyr::filter(
        !is.na(fecha),
        fecha >= Sys.time() - lubridate::days(7)
      ) |>
      dplyr::arrange(
        dplyr::desc(fecha)
      )
    
    
    cat(
      "Noticias en últimos 7 días:",
      nrow(noticias_7d),
      "\n"
    )
    
    
    # ----------------------------------------------------------
    # 10. GUARDAR JSON
    # ----------------------------------------------------------
    
    dir.create(
      "data",
      showWarnings = FALSE,
      recursive = TRUE
    )
    
    
    jsonlite::write_json(
      noticias_7d,
      ARCHIVO_PUBLICO,
      auto_unbox = TRUE,
      pretty = FALSE,
      na = "null"
    )
    
    
    cat("\n")
    
    cat(
      "Archivo generado:",
      ARCHIVO_PUBLICO,
      "\n"
    )
    
    cat(
      "Tamaño:",
      round(
        file.info(ARCHIVO_PUBLICO)$size / 1024^2,
        2
      ),
      "MB\n"
    )
    
    
    # ----------------------------------------------------------
    # 11. RESUMEN FINAL
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
      "Noticias existentes:",
      nrow(noticias_existentes),
      "\n"
    )
    
    
    cat(
      "Noticias obtenidas:",
      nrow(noticias_nuevas),
      "\n"
    )
    
    
    cat(
      "Noticias nuevas para agregar:",
      nrow(noticias_para_agregar),
      "\n"
    )
    
    
    cat(
      "Noticias públicas (7 días):",
      nrow(noticias_7d),
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
    
    
    invisible(
      list(
        noticias_existentes = noticias_existentes,
        noticias_nuevas = noticias_nuevas,
        noticias_7d = noticias_7d,
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
    cat("        ERROR EN LA ACTUALIZACIÓN PÚBLICA\n")
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
    
    
    # IMPORTANTE:
    #
    # No usamos quit() aquí.
    #
    # Un error normal del script no debe terminar la sesión
    # completa de RStudio.
    
    cat(
      "La ejecución fue detenida debido al error.\n"
    )
    
    
    invisible(NULL)
    
  }
  
)