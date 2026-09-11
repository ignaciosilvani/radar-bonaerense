# ============================================================
# 03_obtener_noticias.R
#
# Obtener noticias desde los feeds RSS detectados
# de los medios de comunicación de la Provincia de Buenos Aires
#
# ============================================================


# ------------------------------------------------------------
# Paquetes
# ------------------------------------------------------------

library(httr2)
library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)


# ------------------------------------------------------------
# 1. Función auxiliar: limpiar texto
# ------------------------------------------------------------

.limpiar_texto <- function(x) {
  
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  x <- as.character(x)
  
  x <- gsub("<[^>]+>", " ", x)
  
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  
  x <- gsub("[\r\n\t]+", " ", x)
  
  x <- gsub("\\s+", " ", x)
  
  x <- trimws(x)
  
  ifelse(nzchar(x), x, NA_character_)
}


# ------------------------------------------------------------
# 2. Función auxiliar: leer RSS
# ------------------------------------------------------------

.leer_rss <- function(url) {
  
  resultado <- tryCatch(
    
    {
      
      respuesta <- request(url) |>
        req_user_agent(
          "Mozilla/5.0 (compatible; noticias-PBA/1.0)"
        ) |>
        req_timeout(20) |>
        req_perform()
      
      contenido <- resp_body_string(
        respuesta
      )
      
      xml <- read_xml(
        contenido,
        options = "RECOVER"
      )
      
      xml
      
    },
    
    error = function(e) {
      
      NULL
      
    }
    
  )
  
  resultado
  
}


# ------------------------------------------------------------
# 3. Función auxiliar: convertir fecha RSS
# ------------------------------------------------------------

.convertir_fecha_rss <- function(x) {
  
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  
  x <- trimws(as.character(x))
  
  if (!nzchar(x)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  
  
  # ----------------------------------------------------------
  # RSS estándar con GMT
  #
  # Mon, 31 Aug 2026 17:57:00 GMT
  # ----------------------------------------------------------
  
  partes <- str_match(
    x,
    "^[A-Za-z]{3},\\s+(\\d{1,2})\\s+([A-Za-z]{3})\\s+(\\d{4})\\s+(\\d{2}):(\\d{2}):(\\d{2})\\s+GMT$"
  )
  
  
  if (!is.na(partes[1, 1])) {
    
    dia <- partes[1, 2]
    mes <- partes[1, 3]
    anio <- partes[1, 4]
    hora <- partes[1, 5]
    minuto <- partes[1, 6]
    segundo <- partes[1, 7]
    
    meses <- c(
      Jan = "01",
      Feb = "02",
      Mar = "03",
      Apr = "04",
      May = "05",
      Jun = "06",
      Jul = "07",
      Aug = "08",
      Sep = "09",
      Oct = "10",
      Nov = "11",
      Dec = "12"
    )
    
    mes_numero <- meses[mes]
    
    if (!is.na(mes_numero)) {
      
      fecha_texto <- paste0(
        anio, "-",
        mes_numero, "-",
        sprintf("%02d", as.integer(dia)),
        " ",
        hora, ":",
        minuto, ":",
        segundo
      )
      
      return(
        as.POSIXct(
          fecha_texto,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      )
      
    }
    
  }
  
  
  # ----------------------------------------------------------
  # RSS estándar con zona horaria numérica
  #
  # Tue, 01 Sep 2026 18:47:48 +0000
  # Tue, 01 Sep 2026 00:00:00 -0300
  # ----------------------------------------------------------
  
  partes <- str_match(
    x,
    "^[A-Za-z]{3},\\s+(\\d{1,2})\\s+([A-Za-z]{3})\\s+(\\d{4})\\s+(\\d{2}):(\\d{2}):(\\d{2})\\s+([+-]\\d{4})$"
  )
  
  
  if (!is.na(partes[1, 1])) {
    
    dia <- partes[1, 2]
    mes <- partes[1, 3]
    anio <- partes[1, 4]
    hora <- partes[1, 5]
    minuto <- partes[1, 6]
    segundo <- partes[1, 7]
    zona <- partes[1, 8]
    
    meses <- c(
      Jan = "01",
      Feb = "02",
      Mar = "03",
      Apr = "04",
      May = "05",
      Jun = "06",
      Jul = "07",
      Aug = "08",
      Sep = "09",
      Oct = "10",
      Nov = "11",
      Dec = "12"
    )
    
    mes_numero <- meses[mes]
    
    if (!is.na(mes_numero)) {
      
      fecha_texto <- paste0(
        anio, "-",
        mes_numero, "-",
        sprintf("%02d", as.integer(dia)),
        " ",
        hora, ":",
        minuto, ":",
        segundo,
        " ",
        zona
      )
      
      resultado <- suppressWarnings(
        as.POSIXct(
          fecha_texto,
          format = "%Y-%m-%d %H:%M:%S %z",
          tz = "UTC"
        )
      )
      
      if (!is.na(resultado)) {
        return(resultado)
      }
      
    }
    
  }
  
  
  # ----------------------------------------------------------
  # ISO 8601
  #
  # 2026-08-31T17:57:00Z
  # ----------------------------------------------------------
  
  resultado <- suppressWarnings(
    as.POSIXct(
      x,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  )
  
  if (!is.na(resultado)) {
    return(resultado)
  }
  
  
  # ----------------------------------------------------------
  # ISO 8601 con zona horaria
  #
  # 2026-08-31T17:57:00+0000
  # ----------------------------------------------------------
  
  resultado <- suppressWarnings(
    as.POSIXct(
      x,
      format = "%Y-%m-%dT%H:%M:%S%z",
      tz = "UTC"
    )
  )
  
  if (!is.na(resultado)) {
    return(resultado)
  }
  
  
  # ----------------------------------------------------------
  # Si no se pudo convertir
  # ----------------------------------------------------------
  
  as.POSIXct(NA, tz = "UTC")
  
}


# ------------------------------------------------------------
# 4. Función auxiliar: extraer categoría RSS
# ------------------------------------------------------------

.extraer_seccion_rss <- function(item) {
  
  if (is.null(item) || length(item) == 0) {
    return(NA_character_)
  }
  
  
  # ----------------------------------------------------------
  # RSS estándar
  #
  # <category>Política</category>
  # ----------------------------------------------------------
  
  nodos <- xml_find_all(
    item,
    "./category"
  )
  
  valores <- xml_text(nodos)
  
  
  # ----------------------------------------------------------
  # Dublin Core
  #
  # <dc:subject>Política</dc:subject>
  #
  # Se busca por local-name() para no depender
  # del prefijo del namespace.
  # ----------------------------------------------------------
  
  if (length(valores) == 0 || all(!nzchar(trimws(valores)))) {
    
    nodos <- xml_find_all(
      item,
      "./*[local-name()='subject']"
    )
    
    valores <- xml_text(nodos)
    
  }
  
  
  # ----------------------------------------------------------
  # Atom
  #
  # <category term="Política"/>
  # ----------------------------------------------------------
  
  if (length(valores) == 0 || all(!nzchar(trimws(valores)))) {
    
    nodos <- xml_find_all(
      item,
      "./*[local-name()='category']"
    )
    
    valores <- xml_attr(
      nodos,
      "term"
    )
    
    valores <- valores[
      !is.na(valores) &
        nzchar(trimws(valores))
    ]
    
  }
  
  
  # ----------------------------------------------------------
  # media:category
  # ----------------------------------------------------------
  
  if (length(valores) == 0 || all(!nzchar(trimws(valores)))) {
    
    nodos <- xml_find_all(
      item,
      "./*[local-name()='category']"
    )
    
    valores_texto <- xml_text(nodos)
    
    valores_atributo <- xml_attr(
      nodos,
      "term"
    )
    
    valores <- c(
      valores_atributo[
        !is.na(valores_atributo) &
          nzchar(trimws(valores_atributo))
      ],
      valores_texto[
        !is.na(valores_texto) &
          nzchar(trimws(valores_texto))
      ]
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Limpiar valores
  # ----------------------------------------------------------
  
  valores <- valores[
    !is.na(valores) &
      nzchar(trimws(valores))
  ]
  
  if (length(valores) == 0) {
    return(NA_character_)
  }
  
  
  valores <- .limpiar_texto(valores)
  
  valores <- valores[
    !is.na(valores) &
      nzchar(trimws(valores))
  ]
  
  if (length(valores) == 0) {
    return(NA_character_)
  }
  
  
  # ----------------------------------------------------------
  # Si hay varias categorías:
  #
  # tomamos la primera válida.
  #
  # No concatenamos categorías porque 05_generar_vistas.R
  # necesita una sección individual para normalizarla.
  # ----------------------------------------------------------
  
  valores[[1]]
  
}


# ------------------------------------------------------------
# 5. Función auxiliar: extraer items RSS
# ------------------------------------------------------------

.extraer_items_rss <- function(xml) {
  
  if (is.null(xml)) {
    return(tibble())
  }
  
  
  # ----------------------------------------------------------
  # RSS
  # ----------------------------------------------------------
  
  items <- xml_find_all(
    xml,
    ".//item"
  )
  
  
  # ----------------------------------------------------------
  # Atom
  # ----------------------------------------------------------
  
  if (length(items) == 0) {
    
    items <- xml_find_all(
      xml,
      ".//entry"
    )
    
  }
  
  
  if (length(items) == 0) {
    return(tibble())
  }
  
  
  # ----------------------------------------------------------
  # Extraer noticias
  # ----------------------------------------------------------
  
  tibble(
    
    titulo = map_chr(
      items,
      \(x) {
        
        nodo <- xml_find_first(
          x,
          "./title"
        )
        
        valor <- xml_text(nodo)
        
        .limpiar_texto(valor)
        
      }
    ),
    
    
    url = map_chr(
      items,
      \(x) {
        
        nodo <- xml_find_first(
          x,
          "./link"
        )
        
        valor <- xml_text(nodo)
        
        
        # Atom: link href="..."
        
        if (
          is.na(valor) ||
          !nzchar(trimws(valor))
        ) {
          
          href <- xml_attr(
            nodo,
            "href"
          )
          
          if (!is.na(href)) {
            valor <- href
          }
          
        }
        
        .limpiar_texto(valor)
        
      }
    ),
    
    
    fecha = map_chr(
      items,
      \(x) {
        
        # RSS
        nodo <- xml_find_first(
          x,
          "./pubDate"
        )
        
        valor <- xml_text(nodo)
        
        
        # Atom: published
        
        if (
          is.na(valor) ||
          !nzchar(trimws(valor))
        ) {
          
          nodo <- xml_find_first(
            x,
            "./published"
          )
          
          valor <- xml_text(nodo)
          
        }
        
        
        # Atom: updated
        
        if (
          is.na(valor) ||
          !nzchar(trimws(valor))
        ) {
          
          nodo <- xml_find_first(
            x,
            "./updated"
          )
          
          valor <- xml_text(nodo)
          
        }
        
        .limpiar_texto(valor)
        
      }
    ),
    
    
    descripcion = map_chr(
      items,
      \(x) {
        
        nodo <- xml_find_first(
          x,
          "./description"
        )
        
        valor <- xml_text(nodo)
        
        
        # Atom: summary
        
        if (
          is.na(valor) ||
          !nzchar(trimws(valor))
        ) {
          
          nodo <- xml_find_first(
            x,
            "./summary"
          )
          
          valor <- xml_text(nodo)
          
        }
        
        .limpiar_texto(valor)
        
      }
    ),
    
    
    # --------------------------------------------------------
    # NUEVO:
    # Categoría / sección declarada por el RSS
    # --------------------------------------------------------
    
    seccion = map_chr(
      items,
      .extraer_seccion_rss
    )
    
  )
  
}


# ------------------------------------------------------------
# 6. Función principal
# ------------------------------------------------------------

obtener_noticias <- function(feeds) {
  
  
  # ----------------------------------------------------------
  # Verificar estructura
  # ----------------------------------------------------------
  
  if (!is.data.frame(feeds)) {
    
    stop(
      "El objeto 'feeds' debe ser un data.frame o tibble."
    )
    
  }
  
  
  if (!"feed_url" %in% names(feeds)) {
    
    stop(
      "El objeto 'feeds' no contiene la columna 'feed_url'."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Seleccionar feeds funcionando
  # ----------------------------------------------------------
  
  feeds_ok <- feeds |>
    filter(
      estado == "FEED_OK",
      !is.na(feed_url),
      nzchar(feed_url)
    ) |>
    distinct(
      id,
      .keep_all = TRUE
    )
  
  
  # ----------------------------------------------------------
  # Mensaje inicial
  # ----------------------------------------------------------
  
  cat("\n")
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "OBTENIENDO NOTICIAS\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Feeds RSS disponibles:",
    nrow(feeds_ok),
    "\n\n"
  )
  
  
  # ----------------------------------------------------------
  # Lista de noticias
  # ----------------------------------------------------------
  
  noticias_lista <- vector(
    "list",
    nrow(feeds_ok)
  )
  
  
  # ----------------------------------------------------------
  # Recorrer feeds
  # ----------------------------------------------------------
  
  for (i in seq_len(nrow(feeds_ok))) {
    
    medio <- feeds_ok$nombre[i]
    
    feed_url <- feeds_ok$feed_url[i]
    
    cat(
      sprintf(
        "[%d/%d] %s\n",
        i,
        nrow(feeds_ok),
        medio
      )
    )
    
    
    xml <- .leer_rss(
      feed_url
    )
    
    
    noticias <- .extraer_items_rss(
      xml
    )
    
    
    if (nrow(noticias) > 0) {
      
      noticias <- noticias |>
        mutate(
          
          id_medio = feeds_ok$id[i],
          
          medio = medio,
          
          partido = feeds_ok$partido[i],
          
          localidad = feeds_ok$localidad[i],
          
          tipo_medio = feeds_ok$tipo[i],
          
          web = feeds_ok$web[i],
          
          feed_url = feed_url
          
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
          seccion,
          web,
          feed_url
          
        )
      
      
      noticias_lista[[i]] <- noticias
      
      
      cat(
        "    ->",
        nrow(noticias),
        "noticias\n"
      )
      
    } else {
      
      cat(
        "    -> sin noticias\n"
      )
      
    }
    
  }
  
  
  # ----------------------------------------------------------
  # Unir todos los feeds
  # ----------------------------------------------------------
  
  noticias <- noticias_lista |>
    compact() |>
    bind_rows()
  
  
  # ----------------------------------------------------------
  # Eliminar duplicados
  # ----------------------------------------------------------
  
  if (nrow(noticias) > 0) {
    
    noticias <- noticias |>
      distinct(
        url,
        .keep_all = TRUE
      )
    
  }
  
  
  # ----------------------------------------------------------
  # Convertir fechas
  #
  # ESTA PARTE SE MANTIENE SIN CAMBIOS EN SU LÓGICA.
  # ----------------------------------------------------------
  
  if (nrow(noticias) > 0) {
    
    noticias <- noticias |>
      mutate(
        
        fecha = map_chr(
          fecha,
          \(x) {
            
            resultado <- .convertir_fecha_rss(x)
            
            if (is.na(resultado)) {
              return(NA_character_)
            }
            
            format(
              resultado,
              "%Y-%m-%d %H:%M:%S",
              tz = "UTC"
            )
            
          }
        ),
        
        fecha = as.POSIXct(
          fecha,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
        
      )
    
  }
  
  
  # ----------------------------------------------------------
  # Resultado
  # ----------------------------------------------------------
  
  cat("\n")
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "PROCESO TERMINADO\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Noticias obtenidas:",
    nrow(noticias),
    "\n"
  )
  
  cat(
    "Medios con noticias:",
    n_distinct(noticias$medio),
    "\n"
  )
  
  cat(
    "Noticias con fecha:",
    sum(!is.na(noticias$fecha)),
    "\n"
  )
  
  cat(
    "Noticias con sección RSS:",
    sum(!is.na(noticias$seccion)),
    "\n"
  )
  
  
  noticias
  
}