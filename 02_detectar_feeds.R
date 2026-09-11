# ============================================================

# 02_detectar_feeds.R

#

# Detectar feeds RSS / Atom de medios de comunicación de la PBA

#

# Versión para Windows

#

# - Detecta feeds declarados en HTML

# - Prueba URLs convencionales

# - Procesa medios en paralelo

# - Guarda resultados cada 20 medios

# - Permite continuar una ejecución interrumpida

# - Conserva información del medio:

# id, nombre, web, partido, localidad y tipo

# ============================================================

# ------------------------------------------------------------

# PAQUETES

# ------------------------------------------------------------

library(httr2)
library(xml2)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(future)
library(future.apply)

# ------------------------------------------------------------

# CONFIGURACIÓN

# ------------------------------------------------------------

ARCHIVO_RESULTADOS <- "feeds_detectados.rds"

# Cantidad de procesos simultáneos.

#

# En Windows recomendamos empezar con 6.

#

# Si funciona bien podemos subirlo posteriormente.

WORKERS <- 6

TIMEOUT <- 10

# ------------------------------------------------------------

# 1. NORMALIZAR URL

# ------------------------------------------------------------

.normalizar_url <- function(url) {
  
  if (length(url) == 0 ||
      is.na(url) ||
      !nzchar(trimws(url))) {
    
    
    return(NA_character_)
    
    
  }
  
  url <- trimws(url)
  
  if (!grepl(
    "^https?://",
    url,
    ignore.case = TRUE
  )) {
    
    
    url <- paste0(
      "https://",
      url
    )
    
    
  }
  
  url
}

# ------------------------------------------------------------

# 2. CONSTRUIR URL ABSOLUTA

# ------------------------------------------------------------

.url_absoluta <- function(
    url,
    base_url) {
  
  if (is.na(url) ||
      !nzchar(url)) {
    
    
    return(NA_character_)
    
    
  }
  
  tryCatch(
    
    
    xml2::url_absolute(
      url,
      base_url
    ),
    
    error = function(e) {
      NA_character_
    }
    
    
  )
}

# ------------------------------------------------------------

# 3. DETECTAR FEEDS DECLARADOS EN HTML

# ------------------------------------------------------------

.detectar_feed_html <- function(
    doc,
    base_url) {
  
  links <- html_elements(
    doc,
    "link[rel='alternate']"
  )
  
  if (length(links) == 0) {
    
    
    return(
      tibble(
        feed_url = character(),
        tipo_feed = character(),
        metodo = character()
      )
    )
    
    
  }
  
  resultados <- map_dfr(
    
    
    links,
    
    function(x) {
      
      tipo <- html_attr(
        x,
        "type"
      )
      
      href <- html_attr(
        x,
        "href"
      )
      
      
      if (is.na(tipo) ||
          is.na(href)) {
        
        return(tibble())
      }
      
      
      tipo_lower <- tolower(tipo)
      
      
      es_feed <- str_detect(
        tipo_lower,
        "rss|atom"
      )
      
      
      if (!es_feed) {
        
        return(tibble())
      }
      
      
      href <- .url_absoluta(
        href,
        base_url
      )
      
      
      if (is.na(href)) {
        
        return(tibble())
      }
      
      
      tipo_feed <- case_when(
        
        str_detect(
          tipo_lower,
          "atom"
        ) ~ "Atom",
        
        str_detect(
          tipo_lower,
          "rss"
        ) ~ "RSS",
        
        TRUE ~ "XML"
      )
      
      
      tibble(
        feed_url = href,
        tipo_feed = tipo_feed,
        metodo = "html_link"
      )
    }
    
    
  )
  
  resultados |>
    distinct(
      feed_url,
      .keep_all = TRUE
    )
}

# ------------------------------------------------------------

# 4. URLS CONVENCIONALES

# ------------------------------------------------------------

.detectar_feed_convencional <- function(
    base_url) {
  
  candidatos <- c(
    "/rss",
    "/rss.xml",
    "/feed",
    "/feed.xml",
    "/atom.xml",
    "/feeds/posts/default",
    "/index.xml"
  )
  
  tibble(
    feed_url = paste0(
      sub(
        "/$",
        "",
        base_url
      ),
      candidatos
    ),
    tipo_feed = NA_character_,
    metodo = "url_convencional"
  )
}

# ------------------------------------------------------------

# 5. VERIFICAR FEED

# ------------------------------------------------------------

.verificar_feed <- function(
    feed_url,
    tipo_feed = NA_character_,
    metodo = NA_character_) {
  
  resultado <- tibble(
    
    
    feed_url = feed_url,
    
    tipo_feed = tipo_feed,
    
    metodo = metodo,
    
    estado = "ERROR",
    
    http_status = NA_integer_,
    
    titulo_feed = NA_character_,
    
    cantidad_items = NA_integer_,
    
    ultimo_item = NA_character_,
    
    fecha_ultimo_item = NA_character_,
    
    error = NA_character_
    
    
  )
  
  if (is.na(feed_url) ||
      !nzchar(feed_url)) {
    
    
    resultado$estado <- "SIN_URL"
    
    return(resultado)
    
    
  }
  
  respuesta <- tryCatch(
    
    
    {
      
      request(feed_url) |>
        
        req_user_agent(
          "Mozilla/5.0 (compatible; detector-feeds-PBA/1.0)"
        ) |>
        
        req_timeout(
          TIMEOUT
        ) |>
        
        req_perform()
      
    },
    
    error = function(e) {
      
      resultado$error <<-
        conditionMessage(e)
      
      NULL
    }
    
    
  )
  
  if (is.null(respuesta)) {
    
    
    return(resultado)
    
    
  }
  
  resultado$http_status <-
    resp_status(respuesta)
  
  if (
    resultado$http_status < 200 ||
    resultado$http_status >= 400
  ) {
    
    
    resultado$estado <-
      "HTTP_ERROR"
    
    return(resultado)
    
    
  }
  
  contenido <- tryCatch(
    
    
    resp_body_string(
      respuesta
    ),
    
    error = function(e) {
      NULL
    }
    
    
  )
  
  if (is.null(contenido)) {
    
    
    resultado$estado <-
      "SIN_CONTENIDO"
    
    return(resultado)
    
    
  }
  
  doc <- tryCatch(
    
    
    read_xml(
      contenido
    ),
    
    error = function(e) {
      NULL
    }
    
    
  )
  
  if (is.null(doc)) {
    
    
    resultado$estado <-
      "NO_XML"
    
    return(resultado)
    
    
  }
  
  root <- xml_name(
    xml_root(doc)
  )
  
  # ----------------------------------------------------------
  
  # RSS
  
  # ----------------------------------------------------------
  
  if (
    tolower(root) == "rss"
  ) {
    
    
    resultado$tipo_feed <- "RSS"
    
    
    items <- xml_find_all(
      doc,
      ".//*[local-name()='item']"
    )
    
    
    titulos <- xml_text(
      xml_find_all(
        items,
        "./*[local-name()='title']"
      )
    )
    
    
    fechas <- xml_text(
      xml_find_all(
        items,
        "./*[local-name()='pubDate']"
      )
    )
    
    
    # --------------------------------------------------------
    # ATOM
    # --------------------------------------------------------
    
    
  } else if (
    tolower(root) == "feed"
  ) {
    
    
    resultado$tipo_feed <- "Atom"
    
    
    items <- xml_find_all(
      doc,
      ".//*[local-name()='entry']"
    )
    
    
    titulos <- xml_text(
      xml_find_all(
        items,
        "./*[local-name()='title']"
      )
    )
    
    
    fechas <- xml_text(
      xml_find_all(
        items,
        "./*[local-name()='published' or local-name()='updated']"
      )
    )
    
    
    
  } else {
    
    
    resultado$estado <-
      "NO_FEED"
    
    return(resultado)
    
    
  }
  
  # ----------------------------------------------------------
  
  # TÍTULO
  
  # ----------------------------------------------------------
  
  titulo <- xml_text(
    xml_find_first(
      doc,
      "/*/*[local-name()='channel']/*[local-name()='title'] |
/*/*[local-name()='title']"
    )
  )
  
  if (
    length(titulo) == 0 ||
    !nzchar(titulo)
  ) {
    
    
    titulo <- NA_character_
    
    
  }
  
  resultado$titulo_feed <-
    titulo
  
  resultado$cantidad_items <-
    length(items)
  
  if (
    length(titulos) > 0
  ) {
    
    
    resultado$ultimo_item <-
      titulos[1]
    
    
  }
  
  if (
    length(fechas) > 0
  ) {
    
    
    resultado$fecha_ultimo_item <-
      fechas[1]
    
    
  }
  
  if (
    length(items) > 0
  ) {
    
    
    resultado$estado <-
      "FEED_OK"
    
    
  } else {
    
    
    resultado$estado <-
      "FEED_SIN_ITEMS"
    
    
  }
  
  resultado
}

# ------------------------------------------------------------

# 6. PROCESAR UN MEDIO

# ------------------------------------------------------------

.procesar_medio <- function(
    id,
    nombre,
    web,
    partido,
    localidad,
    tipo) {
  
  web <- .normalizar_url(
    web
  )
  
  # ----------------------------------------------------------
  
  # Información básica del medio
  
  #
  
  # Estos campos se conservan en TODOS los resultados,
  
  # independientemente de si el medio tiene feed o presenta
  
  # algún error.
  
  # ----------------------------------------------------------
  
  resultado_base <- tibble(
    
    
    id = id,
    
    nombre = nombre,
    
    web = web,
    
    partido = partido,
    
    localidad = localidad,
    
    tipo = tipo
    
    
  )
  
  # ----------------------------------------------------------
  
  # Sin web
  
  # ----------------------------------------------------------
  
  if (is.na(web)) {
    
    
    return(
      resultado_base |>
        mutate(
          
          feed_url = NA_character_,
          
          tipo_feed = NA_character_,
          
          metodo = NA_character_,
          
          estado = "SIN_WEB",
          
          http_status = NA_integer_,
          
          titulo_feed = NA_character_,
          
          cantidad_items = NA_integer_,
          
          ultimo_item = NA_character_,
          
          fecha_ultimo_item = NA_character_,
          
          error = NA_character_
        )
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Descargar página
  
  # ----------------------------------------------------------
  
  respuesta <- tryCatch(
    
    
    {
      
      request(web) |>
        
        req_user_agent(
          "Mozilla/5.0 (compatible; detector-feeds-PBA/1.0)"
        ) |>
        
        req_timeout(
          TIMEOUT
        ) |>
        
        req_perform()
      
    },
    
    error = function(e) {
      NULL
    }
    
    
  )
  
  if (is.null(respuesta)) {
    
    
    return(
      resultado_base |>
        mutate(
          
          feed_url = NA_character_,
          
          tipo_feed = NA_character_,
          
          metodo = NA_character_,
          
          estado = "WEB_NO_RESPONDE",
          
          http_status = NA_integer_,
          
          titulo_feed = NA_character_,
          
          cantidad_items = NA_integer_,
          
          ultimo_item = NA_character_,
          
          fecha_ultimo_item = NA_character_,
          
          error = "No se pudo acceder al sitio"
        )
    )
    
    
  }
  
  status <- resp_status(
    respuesta
  )
  
  if (
    status < 200 ||
    status >= 400
  ) {
    
    
    return(
      resultado_base |>
        mutate(
          
          feed_url = NA_character_,
          
          tipo_feed = NA_character_,
          
          metodo = NA_character_,
          
          estado = "WEB_HTTP_ERROR",
          
          http_status = status,
          
          titulo_feed = NA_character_,
          
          cantidad_items = NA_integer_,
          
          ultimo_item = NA_character_,
          
          fecha_ultimo_item = NA_character_,
          
          error = NA_character_
        )
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Leer HTML
  
  # ----------------------------------------------------------
  
  html <- tryCatch(
    
    
    resp_body_html(
      respuesta
    ),
    
    error = function(e) {
      NULL
    }
    
    
  )
  
  if (is.null(html)) {
    
    
    return(
      resultado_base |>
        mutate(
          
          feed_url = NA_character_,
          
          tipo_feed = NA_character_,
          
          metodo = NA_character_,
          
          estado = "HTML_ERROR",
          
          http_status = status,
          
          titulo_feed = NA_character_,
          
          cantidad_items = NA_integer_,
          
          ultimo_item = NA_character_,
          
          fecha_ultimo_item = NA_character_,
          
          error = NA_character_
        )
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Buscar feeds declarados
  
  # ----------------------------------------------------------
  
  feeds <- .detectar_feed_html(
    html,
    web
  )
  
  # ----------------------------------------------------------
  
  # Si no hay, probar convencionales
  
  # ----------------------------------------------------------
  
  if (
    nrow(feeds) == 0
  ) {
    
    
    feeds <-
      .detectar_feed_convencional(
        web
      )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Verificar candidatos
  
  # ----------------------------------------------------------
  
  verificados <- map_dfr(
    
    
    seq_len(
      nrow(feeds)
    ),
    
    function(i) {
      
      .verificar_feed(
        
        feeds$feed_url[i],
        
        feeds$tipo_feed[i],
        
        feeds$metodo[i]
      )
    }
    
    
  )
  
  # ----------------------------------------------------------
  
  # Buscar feed funcional
  
  # ----------------------------------------------------------
  
  ok <- verificados |>
    filter(
      estado %in%
        c(
          "FEED_OK",
          "FEED_SIN_ITEMS"
        )
    )
  
  if (
    nrow(ok) > 0
  ) {
    
    
    return(
      resultado_base |>
        bind_cols(
          ok[1, ]
        )
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Ningún feed funcional
  
  # ----------------------------------------------------------
  
  resultado_base |>
    bind_cols(
      verificados[1, ]
    ) |>
    mutate(
      estado = "SIN_FEED"
    )
}

# ============================================================

# 7. FUNCIÓN PRINCIPAL

# ============================================================

detectar_feeds <- function(
    
  medios_df,
  
  archivo = ARCHIVO_RESULTADOS,
  
  workers = WORKERS,
  
  guardar_cada = 20) {
  
  # ----------------------------------------------------------
  
  # Validaciones
  
  # ----------------------------------------------------------
  
  if (!is.data.frame(
    medios_df
  )) {
    
    
    stop(
      "`medios_df` debe ser un data.frame."
    )
    
    
  }
  
  columnas_necesarias <- c(
    "id",
    "nombre",
    "web",
    "partido",
    "localidad",
    "tipo"
  )
  
  faltantes <- setdiff(
    columnas_necesarias,
    names(medios_df)
  )
  
  if (
    length(faltantes) > 0
  ) {
    
    
    stop(
      "Faltan columnas en `medios_df`: ",
      paste(
        faltantes,
        collapse = ", "
      )
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Recuperar resultados anteriores
  
  # ----------------------------------------------------------
  
  if (
    file.exists(archivo)
  ) {
    
    
    cat(
      "\nRecuperando resultados anteriores...\n"
    )
    
    
    resultados <- readRDS(
      archivo
    )
    
    
  } else {
    
    
    resultados <- tibble()
    
    
  }
  
  # ----------------------------------------------------------
  
  # Identificar pendientes
  
  # ----------------------------------------------------------
  
  #
  
  # En una primera ejecución `resultados` es un tibble vacío
  
  # sin columnas. Evitamos acceder directamente a resultados$id.
  
  #
  
  ids_procesados <- if (
    "id" %in% names(resultados)
  ) {
    
    
    resultados$id
    
    
  } else {
    
    
    numeric()
    
    
  }
  
  pendientes <- medios_df |>
    filter(
      !id %in% ids_procesados
    )
  
  # ----------------------------------------------------------
  
  # Resumen inicial
  
  # ----------------------------------------------------------
  
  cat(
    "\n============================================\n"
  )
  
  cat(
    " DETECCIÓN DE FEEDS - MEDIOS PBA\n"
  )
  
  cat(
    "============================================\n"
  )
  
  cat(
    "Total de medios: ",
    nrow(medios_df),
    "\n",
    sep = ""
  )
  
  cat(
    "Ya procesados: ",
    length(ids_procesados),
    "\n",
    sep = ""
  )
  
  cat(
    "Pendientes: ",
    nrow(pendientes),
    "\n",
    sep = ""
  )
  
  cat(
    "Workers: ",
    workers,
    "\n\n",
    sep = ""
  )
  
  # ----------------------------------------------------------
  
  # Nada pendiente
  
  # ----------------------------------------------------------
  
  if (
    nrow(pendientes) == 0
  ) {
    
    
    cat(
      "Todos los medios ya fueron procesados.\n"
    )
    
    return(
      resultados
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Configurar paralelización
  
  # ----------------------------------------------------------
  
  future::plan(
    future::multisession,
    workers = workers
  )
  
  on.exit(
    future::plan(
      future::sequential
    ),
    add = TRUE
  )
  
  # ----------------------------------------------------------
  
  # Procesamiento por bloques
  
  # ----------------------------------------------------------
  
  for (
    
    
    inicio in seq(
      1,
      nrow(pendientes),
      by = guardar_cada
    )
    
    
  ) {
    
    
    fin <- min(
      inicio + guardar_cada - 1,
      nrow(pendientes)
    )
    
    
    bloque <- pendientes[
      inicio:fin,
      ,
      drop = FALSE
    ]
    
    
    cat(
      "\nProcesando ",
      inicio,
      " a ",
      fin,
      " de ",
      nrow(pendientes),
      " pendientes...\n",
      sep = ""
    )
    
    
    # --------------------------------------------------------
    # Procesamiento paralelo
    # --------------------------------------------------------
    
    resultados_bloque <-
      future.apply::future_lapply(
        
        seq_len(
          nrow(bloque)
        ),
        
        function(i) {
          
          medio <-
            bloque[i, ]
          
          
          resultado <- tryCatch(
            
            {
              
              .procesar_medio(
                
                id = medio$id,
                
                nombre = medio$nombre,
                
                web = medio$web,
                
                partido = medio$partido,
                
                localidad = medio$localidad,
                
                tipo = medio$tipo
              )
              
            },
            
            error = function(e) {
              
              tibble(
                
                id = medio$id,
                
                nombre = medio$nombre,
                
                web = medio$web,
                
                partido = medio$partido,
                
                localidad = medio$localidad,
                
                tipo = medio$tipo,
                
                feed_url = NA_character_,
                
                tipo_feed = NA_character_,
                
                metodo = NA_character_,
                
                estado = "ERROR",
                
                http_status = NA_integer_,
                
                titulo_feed = NA_character_,
                
                cantidad_items = NA_integer_,
                
                ultimo_item = NA_character_,
                
                fecha_ultimo_item = NA_character_,
                
                error =
                  conditionMessage(e)
              )
            }
          )
          
          
          resultado
        },
        
        future.seed = TRUE
      )
    
    
    resultados_bloque <-
      bind_rows(
        resultados_bloque
      )
    
    
    # --------------------------------------------------------
    # Agregar
    # --------------------------------------------------------
    
    resultados <-
      bind_rows(
        resultados,
        resultados_bloque
      ) |>
      distinct(
        id,
        .keep_all = TRUE
      )
    
    
    # --------------------------------------------------------
    # Guardar
    # --------------------------------------------------------
    
    saveRDS(
      resultados,
      archivo
    )
    
    
    # --------------------------------------------------------
    # Mostrar progreso
    # --------------------------------------------------------
    
    cat(
      "\nResultados del bloque:\n"
    )
    
    
    print(
      resultados_bloque |>
        count(
          estado,
          sort = TRUE
        )
    )
    
    
    cat(
      "\nGuardado en: ",
      archivo,
      "\n",
      sep = ""
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Resultado final
  
  # ----------------------------------------------------------
  
  cat(
    "\n============================================\n"
  )
  
  cat(
    " PROCESO FINALIZADO\n"
  )
  
  cat(
    "============================================\n\n"
  )
  
  print(
    resultados |>
      count(
        estado,
        sort = TRUE
      )
  )
  
  resultados
}
