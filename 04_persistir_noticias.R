# ============================================================

# 04_persistir_noticias.R

#

# Persistencia histórica de noticias

#

# Recibe las noticias obtenidas por 03_obtener_noticias.R

# y las incorpora a un archivo histórico.

#

# Características:

#

# - Conserva noticias de ejecuciones anteriores

# - Agrega solamente noticias nuevas

# - Evita duplicados por URL

# - Registra cuándo fue detectada cada noticia

# - Conserva la sección RSS de cada noticia

# - Permite ejecutar el proceso periódicamente

# - No elimina noticias históricas

#

# ============================================================

# ------------------------------------------------------------

# PAQUETES

# ------------------------------------------------------------

library(dplyr)
library(tibble)

# ------------------------------------------------------------

# CONFIGURACIÓN

# ------------------------------------------------------------

ARCHIVO_HISTORICO <- "noticias_historicas.rds"

# ============================================================

# 1. FUNCIÓN PRINCIPAL

# ============================================================

persistir_noticias <- function(
    noticias,
    archivo = ARCHIVO_HISTORICO
) {
  
  # ----------------------------------------------------------
  
  # Validaciones
  
  # ----------------------------------------------------------
  
  if (!is.data.frame(noticias)) {
    
    
    stop(
      "El objeto `noticias` debe ser un data.frame o tibble."
    )
    
    
  }
  
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
    "feed_url",
    "seccion"
  )
  
  faltantes <- setdiff(
    columnas_necesarias,
    names(noticias)
  )
  
  if (length(faltantes) > 0) {
    
    
    stop(
      "Faltan columnas en `noticias`: ",
      paste(
        faltantes,
        collapse = ", "
      )
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Mensaje inicial
  
  # ----------------------------------------------------------
  
  cat("\n")
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "PERSISTENCIA HISTÓRICA DE NOTICIAS\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Noticias recibidas:",
    nrow(noticias),
    "\n"
  )
  
  # ==========================================================
  
  # 2. PREPARAR NOTICIAS RECIBIDAS
  
  # ==========================================================
  
  # ----------------------------------------------------------
  
  # Registrar momento de captura
  
  # ----------------------------------------------------------
  
  fecha_captura <- Sys.time()
  
  noticias_nuevas <- noticias |>
    mutate(
      
      
      fecha_captura = fecha_captura
      
    ) |>
    select(
      
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
  
  # Eliminar duplicados dentro de la propia descarga
  
  # ----------------------------------------------------------
  
  noticias_nuevas <- noticias_nuevas |>
    distinct(
      url,
      .keep_all = TRUE
    )
  
  cat(
    "Noticias únicas recibidas:",
    nrow(noticias_nuevas),
    "\n"
  )
  
  # ==========================================================
  
  # 3. RECUPERAR HISTÓRICO
  
  # ==========================================================
  
  if (file.exists(archivo)) {
    
    
    cat(
      "\nRecuperando histórico existente...\n"
    )
    
    
    historico <- readRDS(
      archivo
    )
    
    
    if (!is.data.frame(historico)) {
      
      stop(
        "El archivo histórico no contiene un data.frame válido."
      )
      
    }
    
    
    cat(
      "Noticias en histórico:",
      nrow(historico),
      "\n"
    )
    
    
  } else {
    
    
    cat(
      "\nNo existe histórico previo. Se creará uno nuevo.\n"
    )
    
    
    historico <- tibble()
    
    
  }
  
  # ==========================================================
  
  # 4. COMPATIBILIDAD CON HISTÓRICOS ANTERIORES
  
  # ==========================================================
  
  # ----------------------------------------------------------
  
  # Si el histórico fue creado antes de incorporar
  
  # `fecha_captura`, agregar la columna.
  
  # ----------------------------------------------------------
  
  if (
    nrow(historico) > 0 &&
    !"fecha_captura" %in% names(historico)
  ) {
    
    
    historico <- historico |>
      mutate(
        
        fecha_captura = as.POSIXct(
          NA_real_,
          origin = "1970-01-01",
          tz = "America/Argentina/Buenos_Aires"
        )
        
      )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Si el histórico fue creado antes de incorporar
  
  # `seccion`, agregar la columna.
  
  #
  
  # Las noticias históricas anteriores quedan con NA.
  
  # Las nuevas conservarán la sección obtenida por 03.
  
  # ----------------------------------------------------------
  
  if (
    nrow(historico) > 0 &&
    !"seccion" %in% names(historico)
  ) {
    
    
    historico$seccion <- NA_character_
    
    
  }
  
  # ----------------------------------------------------------
  
  # Normalizar estructura del histórico
  
  #
  
  # Esto evita problemas si el archivo histórico fue creado
  
  # con una estructura ligeramente diferente.
  
  # ----------------------------------------------------------
  
  if (nrow(historico) > 0) {
    
    
    columnas_historico_faltantes <- setdiff(
      names(noticias_nuevas),
      names(historico)
    )
    
    
    if (
      length(columnas_historico_faltantes) > 0
    ) {
      
      for (
        columna in columnas_historico_faltantes
      ) {
        
        historico[[columna]] <- NA
        
      }
      
    }
    
    
    historico <- historico |>
      select(
        all_of(
          names(noticias_nuevas)
        )
      )
    
    
  }
  
  # ==========================================================
  
  # 5. IDENTIFICAR NOTICIAS REALMENTE NUEVAS
  
  # ==========================================================
  
  if (nrow(historico) == 0) {
    
    
    noticias_para_agregar <- noticias_nuevas
    
    
  } else {
    
    
    # --------------------------------------------------------
    # Las URLs que ya existen en el histórico no vuelven
    # a incorporarse.
    # --------------------------------------------------------
    
    urls_historicas <- historico$url
    
    
    noticias_para_agregar <- noticias_nuevas |>
      filter(
        !url %in% urls_historicas
      )
    
    
  }
  
  cat(
    "Noticias realmente nuevas:",
    nrow(noticias_para_agregar),
    "\n"
  )
  
  # ==========================================================
  
  # 6. AGREGAR AL HISTÓRICO
  
  # ==========================================================
  
  if (nrow(noticias_para_agregar) > 0) {
    
    
    historico <- bind_rows(
      historico,
      noticias_para_agregar
    )
    
    
  }
  
  # ==========================================================
  
  # 7. CONTROL FINAL DE DUPLICADOS
  
  # ==========================================================
  
  historico <- historico |>
    distinct(
      url,
      .keep_all = TRUE
    )
  
  # ----------------------------------------------------------
  
  # Orden cronológico
  
  #
  
  # Las noticias más recientes quedan primero.
  
  # ----------------------------------------------------------
  
  historico <- historico |>
    arrange(
      desc(fecha)
    )
  
  # ==========================================================
  
  # 8. GUARDAR HISTÓRICO
  
  # ==========================================================
  
  saveRDS(
    historico,
    archivo
  )
  
  # ==========================================================
  
  # 9. RESUMEN FINAL
  
  # ==========================================================
  
  cat("\n")
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "PERSISTENCIA FINALIZADA\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Noticias recibidas:",
    nrow(noticias),
    "\n"
  )
  
  cat(
    "Noticias únicas recibidas:",
    nrow(noticias_nuevas),
    "\n"
  )
  
  cat(
    "Noticias realmente nuevas:",
    nrow(noticias_para_agregar),
    "\n"
  )
  
  cat(
    "Noticias históricas:",
    nrow(historico),
    "\n"
  )
  
  cat(
    "Medios con noticias:",
    n_distinct(historico$medio),
    "\n"
  )
  
  cat(
    "Noticias con fecha:",
    sum(!is.na(historico$fecha)),
    "\n"
  )
  
  cat(
    "Noticias sin fecha:",
    sum(is.na(historico$fecha)),
    "\n"
  )
  
  cat(
    "Noticias con sección:",
    sum(!is.na(historico$seccion)),
    "\n"
  )
  
  cat(
    "Noticias sin sección:",
    sum(is.na(historico$seccion)),
    "\n"
  )
  
  cat(
    "Archivo:",
    archivo,
    "\n"
  )
  
  cat("\n")
  
  # ----------------------------------------------------------
  
  # Resultado
  
  # ----------------------------------------------------------
  
  historico
  
}
