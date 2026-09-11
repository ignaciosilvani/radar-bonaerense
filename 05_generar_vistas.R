# ============================================================

# 05_generar_vistas.R

# Preparación de vistas para la aplicación de noticias

# ============================================================

library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# ============================================================

# 1. EXTRAER SECCIÓN DESDE LA URL

# ============================================================

.extraer_seccion_url <- function(url) {
  
  if (is.na(url) || !nzchar(trimws(url))) {
    return(NA_character_)
  }
  
  x <- url
  
  # ----------------------------------------------------------
  
  # Eliminar protocolo y dominio
  
  # ----------------------------------------------------------
  
  x <- sub(
    "^https?://[^/]+",
    "",
    x
  )
  
  # ----------------------------------------------------------
  
  # Eliminar query string y fragmentos
  
  # ----------------------------------------------------------
  
  x <- sub(
    "[?#].*$",
    "",
    x
  )
  
  # ----------------------------------------------------------
  
  # Separar segmentos
  
  # ----------------------------------------------------------
  
  segmentos <- unlist(
    strsplit(
      x,
      "/",
      fixed = TRUE
    )
  )
  
  segmentos <- segmentos[
    nzchar(segmentos)
  ]
  
  if (length(segmentos) == 0) {
    return(NA_character_)
  }
  
  # ----------------------------------------------------------
  
  # Decodificar URL
  
  # ----------------------------------------------------------
  
  segmentos <- map_chr(
    segmentos,
    ~ tryCatch(
      utils::URLdecode(.x),
      error = function(e) .x
    )
  )
  
  # ----------------------------------------------------------
  
  # Limpiar segmentos
  
  # ----------------------------------------------------------
  
  segmentos <- segmentos |>
    str_replace_all(
      "-",
      " "
    ) |>
    str_replace_all(
      "_",
      " "
    ) |>
    str_squish()
  
  # ----------------------------------------------------------
  
  # Normalización auxiliar para comparar
  
  #
  
  # Se reemplaza "." por espacio.
  
  # ----------------------------------------------------------
  
  segmentos_comparacion <- segmentos |>
    str_to_lower() |>
    str_replace_all(
      "[.]",
      " "
    ) |>
    str_squish()
  
  # ==========================================================
  
  # SEGMENTOS QUE NUNCA SON SECCIONES
  
  # ==========================================================
  
  excluir <- c(
    
    
    "feed",
    "rss",
    "atom",
    "home",
    "inicio",
    "ahora",
    "single",
    "single post",
    "post",
    "posts",
    "index",
    "index php",
    "article",
    "articulo",
    "articulos",
    "nota",
    "notas",
    "noticia",
    "noticias",
    "news",
    
    "wp",
    "wp content",
    "wp admin",
    "wp includes",
    "wp json",
    
    "amp",
    "tag",
    "tags",
    "category",
    "categorias",
    "categoria",
    "author",
    "autor",
    "page",
    "pagina",
    "search",
    "buscar",
    "archivo",
    "archives"
    
    
  )
  
  # ----------------------------------------------------------
  
  # Eliminar segmentos técnicos
  
  # ----------------------------------------------------------
  
  candidatos <- segmentos[
    !segmentos_comparacion %in% excluir
  ]
  
  candidatos_comparacion <- segmentos_comparacion[
    !segmentos_comparacion %in% excluir
  ]
  
  if (length(candidatos) == 0) {
    return(NA_character_)
  }
  
  # ==========================================================
  
  # ELIMINAR SEGMENTOS QUE SON SOLO NÚMEROS
  
  # ==========================================================
  
  mantener <- !str_detect(
    candidatos_comparacion,
    "^[0-9]+$"
  )
  
  candidatos <- candidatos[
    mantener
  ]
  
  candidatos_comparacion <- candidatos_comparacion[
    mantener
  ]
  
  if (length(candidatos) == 0) {
    return(NA_character_)
  }
  
  # ==========================================================
  
  # ELIMINAR FECHAS
  
  # ==========================================================
  
  es_fecha <- str_detect(
    candidatos_comparacion,
    "^[0-9]{1,4}([ -/][0-9]{1,2}){1,2}$"
  )
  
  candidatos <- candidatos[
    !es_fecha
  ]
  
  candidatos_comparacion <- candidatos_comparacion[
    !es_fecha
  ]
  
  if (length(candidatos) == 0) {
    return(NA_character_)
  }
  
  # ==========================================================
  
  # SECCIONES CONOCIDAS
  
  # ==========================================================
  
  secciones_conocidas <- c(
    
    
    "politica",
    "política",
    
    "economia",
    "economía",
    
    "sociedad",
    
    "policiales",
    "policial",
    "seguridad",
    "judiciales",
    "judicial",
    
    "deportes",
    "deporte",
    
    "cultura",
    
    "espectaculos",
    "espectáculos",
    "entretenimiento",
    
    "provincia",
    
    "tecnologia",
    "tecnología",
    
    "turismo",
    
    "opinion",
    "opinión",
    
    "local"
    
    
  )
  
  secciones_conocidas_comparacion <-
    str_to_lower(
      secciones_conocidas
    ) |>
    str_replace_all(
      "[.]",
      " "
    ) |>
    str_squish()
  
  # ----------------------------------------------------------
  
  # Buscar una sección conocida
  
  # ----------------------------------------------------------
  
  idx <- match(
    candidatos_comparacion,
    secciones_conocidas_comparacion
  )
  
  if (any(!is.na(idx))) {
    
    
    return(
      candidatos[
        which(!is.na(idx))[1]
      ]
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Si no encontramos una sección conocida, no usamos el
  
  # primer segmento restante como sección.
  
  # ----------------------------------------------------------
  
  NA_character_
}

# ============================================================

# 2. NORMALIZAR SECCIONES RSS

# ============================================================

.normalizar_seccion_rss <- function(
    seccion
) {
  
  if (
    is.na(seccion) ||
    !nzchar(trimws(seccion))
  ) {
    return(NA_character_)
  }
  
  x <- str_to_lower(
    str_squish(seccion)
  )
  
  # ----------------------------------------------------------
  # CATEGORÍAS TEMÁTICAS ESPECÍFICAS
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "elecciones 2027"
    )
  ) {
    return("Política")
  }
  
  if (
    str_detect(
      x,
      "energia|energía|empresas|noticias del campo"
    )
  ) {
    return("Economía")
  }
  
  if (
    str_detect(
      x,
      "la memoria y ddhh|sindicales|solidaridad|fallecimientos|necrológicas|necrologicas|servicios"
    )
  ) {
    return("Sociedad")
  }
  
  if (
    str_detect(
      x,
      "accidentes|crónicas rojas|cronicas rojas"
    )
  ) {
    return("Policiales")
  }
  
  if (
    str_detect(
      x,
      "deporte universitario|noticias de deportes"
    )
  ) {
    return("Deportes")
  }
  
  if (
    str_detect(
      x,
      "noticias nacionales"
    )
  ) {
    return("Nacional")
  }
  
  if (
    str_detect(
      x,
      "tecnósfera|tecnosfera"
    )
  ) {
    return("Tecnología")
  }
  
  if (
    str_detect(
      x,
      "concejo deliberante"
    )
  ) {
    return("Local")
  }
  
  if (
    str_detect(
      x,
      "moda & estilo|tv/plataformas"
    )
  ) {
    return("Espectáculos")
  }
  
  if (
    str_detect(
      x,
      "reseñas|resenas"
    )
  ) {
    return("Cultura")
  }
  
  if (
    str_detect(
      x,
      "^universidad$"
    )
  ) {
    return("Educación")
  }
  
  # ----------------------------------------------------------
  # CATEGORÍAS GENÉRICAS / EDITORIALES
  # ----------------------------------------------------------
  
  if (
    x %in% c(
      "noticias",
      "noticia",
      "general",
      "generales",
      "actualidad",
      "actualidades",
      "la actualidad",
      "informacion general",
      "información general",
      "info general",
      "interes general",
      "interés general",
      "destacadas",
      "destacados",
      "destacada",
      "destacado",
      "noticias destacadas",
      "notas destacadas",
      "portada",
      "tapa",
      "nota de tapa",
      "titulares",
      "ahora",
      "de última",
      "@ lo último",
      "editoriales",
      "editorial",
      "comunicados",
      "institucionales",
      "institucional",
      "sin categoría",
      "sin categoria",
      "uncategorized",
      "archivo",
      "notas",
      "artículos",
      "articulos",
      "featured",
      "especiales",
      "recomendados",
      "★★★",
      "aa",
      "estudio",
      "secciones"
    )
  ) {
    return(NA_character_)
  }
  
  # ----------------------------------------------------------
  # SECCIONES PRINCIPALES
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "politic|política"
    )
  ) {
    return("Política")
  }
  
  if (
    str_detect(
      x,
      "econom|economía"
    )
  ) {
    return("Economía")
  }
  
  if (
    str_detect(
      x,
      "policial|seguridad|judicial|justicia|crimen|sucesos"
    )
  ) {
    return("Policiales")
  }
  
  if (
    str_detect(
      x,
      "^deporte[s]?$|futbol|fútbol|basquet|básquet|tenis|rugby|hockey|ajedrez|automovilismo"
    )
  ) {
    return("Deportes")
  }
  
  if (
    str_detect(
      x,
      "cultura|cultural|arte|artes|libros|literatura|musica|música|teatro|cine"
    )
  ) {
    return("Cultura")
  }
  
  if (
    str_detect(
      x,
      "espectaculo|espectáculo|entretenimiento|famosos|celebridades"
    )
  ) {
    return("Espectáculos")
  }
  
  if (
    str_detect(
      x,
      "^provincia$|^provincial$|^provinciales$|la provincia|a provincias"
    )
  ) {
    return("Provincia")
  }
  
  if (
    str_detect(
      x,
      "^pais$|^país$|^nacional$|^nacionales$|^nación$|^nacion$|^el pais$|^el país$|del pais|del país|internacional|internacionales|^mundo$|^el mundo$"
    )
  ) {
    return("Nacional")
  }
  
  if (
    str_detect(
      x,
      "^sociedad$|sociales|humanidades|derechos humanos|infancias|juventudes|género|genero"
    )
  ) {
    return("Sociedad")
  }
  
  if (
    str_detect(
      x,
      "educacion|educación"
    )
  ) {
    return("Educación")
  }
  
  if (
    str_detect(
      x,
      "^salud$|salud y"
    )
  ) {
    return("Salud")
  }
  
  if (
    str_detect(
      x,
      "ecologia|ecología|ambiente|medio ambiente|agricultura y medio ambiente"
    )
  ) {
    return("Ambiente")
  }
  
  if (
    str_detect(
      x,
      "tecnolog|tecnología|software|ia$"
    )
  ) {
    return("Tecnología")
  }
  
  if (
    str_detect(
      x,
      "turismo|turistico|turístico|viaje|viajes|gastronomia|gastronomía"
    )
  ) {
    return("Turismo")
  }
  
  if (
    str_detect(
      x,
      "opinion|opinión|editorial|columna|columnas|reflexiones"
    )
  ) {
    return("Opinión")
  }
  
  if (
    str_detect(
      x,
      "^local$|^locales$|^regionales$|^region$|^región$|^zonal$|^conurbano$|^municipios$|^municipales$|^municipios ba$|^descubre tu ciudad$"
    )
  ) {
    return("Local")
  }
  
  # ----------------------------------------------------------
  # TODO LO DEMÁS NO SE CONSIDERA SECCIÓN
  # ----------------------------------------------------------
  
  NA_character_
}

# ============================================================

# 3. NORMALIZAR SECCIONES

# ============================================================

.normalizar_seccion <- function(x) {
  
  if (
    is.na(x) ||
    !nzchar(trimws(x))
  ) {
    return("Sin sección")
  }
  
  x <- str_to_lower(
    x
  )
  
  x <- str_replace_all(
    x,
    "_",
    " "
  )
  
  x <- str_replace_all(
    x,
    "-",
    " "
  )
  
  x <- str_squish(
    x
  )
  
  # ----------------------------------------------------------
  
  # Ausencia de sección
  
  # ----------------------------------------------------------
  
  if (
    x %in% c(
      "sin seccion",
      "sin sección",
      "sin categoria",
      "sin categoría",
      "na",
      "n a",
      "null"
    )
  ) {
    return("Sin sección")
  }
  
  # ----------------------------------------------------------
  
  # Política
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "politic|política"
    )
  ) {
    return("Política")
  }
  
  # ----------------------------------------------------------
  
  # Economía
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "econom|economía"
    )
  ) {
    return("Economía")
  }
  
  # ----------------------------------------------------------
  
  # Sociedad
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "^sociedad$"
    )
  ) {
    return("Sociedad")
  }
  
  # ----------------------------------------------------------
  
  # Policiales
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "policial|seguridad|judicial|justicia|crimen|sucesos"
    )
  ) {
    return("Policiales")
  }
  
  # ----------------------------------------------------------
  
  # Deportes
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "^deporte[s]?$|futbol|fútbol|basquet|básquet|tenis|rugby"
    )
  ) {
    return("Deportes")
  }
  
  # ----------------------------------------------------------
  
  # Cultura
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "cultura|cultural|arte|libros|literatura|musica|música"
    )
  ) {
    return("Cultura")
  }
  
  # ----------------------------------------------------------
  
  # Espectáculos
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "espectaculo|espectáculo|entretenimiento|famosos|celebridades"
    )
  ) {
    return("Espectáculos")
  }
  
  # ----------------------------------------------------------
  
  # Provincia
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "^provincia$|^provincial$"
    )
  ) {
    return("Provincia")
  }
  
  # ----------------------------------------------------------
  
  # Tecnología
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "tecnolog|tecnología"
    )
  ) {
    return("Tecnología")
  }
  
  # ----------------------------------------------------------
  
  # Turismo
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "turismo|turistico|turístico|viaje|viajes"
    )
  ) {
    return("Turismo")
  }
  
  # ----------------------------------------------------------
  
  # Opinión
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "opinion|opinión|editorial|columna|columnas"
    )
  ) {
    return("Opinión")
  }
  
  # ----------------------------------------------------------
  
  # Local
  
  # ----------------------------------------------------------
  
  if (
    str_detect(
      x,
      "^local$|locales"
    )
  ) {
    return("Local")
  }
  
  # ----------------------------------------------------------
  
  # No contemplada
  
  # ----------------------------------------------------------
  
  return("Otros")
}

# ============================================================

# 4. PREPARAR SECCIONES

# ============================================================

.preparar_secciones <- function(
    noticias
) {
  
  # ----------------------------------------------------------
  
  # Obtener sección RSS
  
  # ----------------------------------------------------------
  
  if (
    "seccion" %in% names(noticias)
  ) {
    
    
    fuente_rss <- noticias$seccion
    
    fuente_rss <- ifelse(
      is.na(fuente_rss) |
        !nzchar(trimws(fuente_rss)),
      NA_character_,
      fuente_rss
    )
    
    
  } else {
    
    
    fuente_rss <- rep(
      NA_character_,
      nrow(noticias)
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Normalizar RSS
  
  # ----------------------------------------------------------
  
  seccion_rss <- map_chr(
    fuente_rss,
    .normalizar_seccion_rss
  )
  
  # ----------------------------------------------------------
  
  # Extraer sección desde URL
  
  # ----------------------------------------------------------
  
  if (
    "url" %in% names(noticias)
  ) {
    
    
    seccion_url <- map_chr(
      noticias$url,
      .extraer_seccion_url
    )
    
    
  } else {
    
    
    seccion_url <- rep(
      NA_character_,
      nrow(noticias)
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Normalizar sección URL
  
  # ----------------------------------------------------------
  
  seccion_url_normalizada <- map_chr(
    seccion_url,
    .normalizar_seccion
  )
  
  # ----------------------------------------------------------
  
  # PRIORIDAD:
  
  #
  
  # 1. RSS reconocido
  
  # 2. URL
  
  # 3. Sin sección
  
  # ----------------------------------------------------------
  
  seccion_final <- ifelse(
    !is.na(seccion_rss),
    seccion_rss,
    seccion_url_normalizada
  )
  
  # ----------------------------------------------------------
  
  # Conservar sección original
  
  # ----------------------------------------------------------
  
  seccion_original <- ifelse(
    !is.na(fuente_rss),
    fuente_rss,
    seccion_url
  )
  
  noticias$seccion_original <-
    seccion_original
  
  noticias$seccion <-
    seccion_final
  
  noticias
}

# ============================================================

# 5. FECHA RELATIVA

# ============================================================

.fecha_relativa <- function(
    fecha,
    ahora
) {
  
  if (
    is.na(fecha)
  ) {
    return(NA_character_)
  }
  
  segundos <- as.numeric(
    difftime(
      ahora,
      fecha,
      units = "secs"
    )
  )
  
  if (
    segundos < 0
  ) {
    return("ahora")
  }
  
  if (
    segundos < 60
  ) {
    return(
      "hace menos de 1 min"
    )
  }
  
  minutos <- floor(
    segundos / 60
  )
  
  if (
    minutos < 60
  ) {
    
    
    return(
      paste0(
        "hace ",
        minutos,
        " min"
      )
    )
    
    
  }
  
  horas <- floor(
    minutos / 60
  )
  
  if (
    horas < 24
  ) {
    
    
    return(
      paste0(
        "hace ",
        horas,
        ifelse(
          horas == 1,
          " hora",
          " horas"
        )
      )
    )
    
    
  }
  
  dias <- floor(
    horas / 24
  )
  
  if (
    dias < 7
  ) {
    
    
    return(
      paste0(
        "hace ",
        dias,
        ifelse(
          dias == 1,
          " día",
          " días"
        )
      )
    )
    
    
  }
  
  format(
    fecha,
    "%d/%m/%Y"
  )
}

# ============================================================

# 6. PREPARAR NOTICIAS

# ============================================================

.preparar_noticias <- function(
    noticias
) {
  
  # ----------------------------------------------------------
  
  # Validaciones
  
  # ----------------------------------------------------------
  
  if (
    !"fecha" %in% names(noticias)
  ) {
    
    
    stop(
      "El histórico no contiene la columna 'fecha'."
    )
    
    
  }
  
  if (
    !"titulo" %in% names(noticias)
  ) {
    
    
    stop(
      "El histórico no contiene la columna 'titulo'."
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Asegurar fecha POSIXct
  
  # ----------------------------------------------------------
  
  noticias <- noticias |>
    mutate(
      fecha = as.POSIXct(
        fecha
      )
    )
  
  # ----------------------------------------------------------
  
  # Preparar secciones
  
  # ----------------------------------------------------------
  
  noticias <- .preparar_secciones(
    noticias
  )
  
  # ----------------------------------------------------------
  
  # Momento de referencia
  
  # ----------------------------------------------------------
  
  ahora <- max(
    noticias$fecha,
    na.rm = TRUE
  )
  
  # ----------------------------------------------------------
  
  # Hora
  
  # ----------------------------------------------------------
  
  noticias <- noticias |>
    mutate(
      hora = format(
        fecha,
        "%H:%M"
      )
    )
  
  # ----------------------------------------------------------
  
  # Fecha relativa
  
  # ----------------------------------------------------------
  
  noticias <- noticias |>
    mutate(
      fecha_relativa = map_chr(
        fecha,
        ~ .fecha_relativa(
          .x,
          ahora
        )
      )
    )
  
  # ----------------------------------------------------------
  
  # Texto para búsqueda
  
  # ----------------------------------------------------------
  
  columnas_busqueda <- intersect(
    c(
      "titulo",
      "descripcion",
      "medio",
      "partido",
      "localidad",
      "seccion"
    ),
    names(noticias)
  )
  
  texto_busqueda <- apply(
    noticias[
      ,
      columnas_busqueda,
      drop = FALSE
    ],
    1,
    paste,
    collapse = " "
  )
  
  noticias$texto_busqueda <-
    texto_busqueda
  
  # ----------------------------------------------------------
  
  # Orden cronológico
  
  # ----------------------------------------------------------
  
  noticias <- noticias |>
    arrange(
      desc(fecha)
    )
  
  noticias
}

# ============================================================

# 7. GENERAR VISTAS

# ============================================================

generar_vistas <- function(
    historico,
    guardar = TRUE,
    archivo = "vistas_noticias.rds"
) {
  
  # ----------------------------------------------------------
  
  # Validación
  
  # ----------------------------------------------------------
  
  if (
    is.null(historico) ||
    nrow(historico) == 0
  ) {
    
    
    stop(
      "El objeto 'historico' está vacío."
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Preparar histórico
  
  # ----------------------------------------------------------
  
  historico_preparado <-
    .preparar_noticias(
      historico
    )
  
  # ----------------------------------------------------------
  
  # Momento de referencia
  
  # ----------------------------------------------------------
  
  ahora <- max(
    historico_preparado$fecha,
    na.rm = TRUE
  )
  
  # ----------------------------------------------------------
  
  # Límites temporales
  
  # ----------------------------------------------------------
  
  limite_24h <-
    ahora - (24 * 60 * 60)
  
  limite_7d <-
    ahora - (7 * 24 * 60 * 60)
  
  # ==========================================================
  
  # AHORA
  
  # ==========================================================
  
  ahora_vista <-
    historico_preparado |>
    arrange(
      desc(fecha)
    ) |>
    slice_head(
      n = 50
    )
  
  # ==========================================================
  
  # ÚLTIMAS 24 HORAS
  
  # ==========================================================
  
  ultimas_24h <-
    historico_preparado |>
    filter(
      !is.na(fecha),
      fecha >= limite_24h,
      fecha <= ahora
    ) |>
    arrange(
      desc(fecha)
    )
  
  # ==========================================================
  
  # ÚLTIMOS 7 DÍAS
  
  # ==========================================================
  
  ultimos_7_dias <-
    historico_preparado |>
    filter(
      !is.na(fecha),
      fecha >= limite_7d,
      fecha <= ahora
    ) |>
    arrange(
      desc(fecha)
    )
  
  # ==========================================================
  
  # FILTROS
  
  # ==========================================================
  
  filtros <- list(
    
    
    partido = sort(
      unique(
        na.omit(
          historico_preparado$partido
        )
      )
    ),
    
    localidad = sort(
      unique(
        na.omit(
          historico_preparado$localidad
        )
      )
    ),
    
    medio = sort(
      unique(
        na.omit(
          historico_preparado$medio
        )
      )
    ),
    
    seccion = sort(
      unique(
        historico_preparado$seccion
      )
    )
    
    
  )
  
  # ==========================================================
  
  # OBJETO FINAL
  
  # ==========================================================
  
  vistas <- list(
    
    
    generado = Sys.time(),
    
    referencia = ahora,
    
    ahora = ahora_vista,
    
    ultimas_24h = ultimas_24h,
    
    ultimos_7_dias = ultimos_7_dias,
    
    historico = historico_preparado,
    
    filtros = filtros
    
    
  )
  
  # ==========================================================
  
  # GUARDAR
  
  # ==========================================================
  
  if (
    guardar
  ) {
    
    
    saveRDS(
      vistas,
      archivo
    )
    
    
  }
  
  # ==========================================================
  
  # RESUMEN
  
  # ==========================================================
  
  cat("\n")
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "VISTAS DE NOTICIAS GENERADAS\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Histórico:",
    nrow(historico_preparado),
    "noticias\n"
  )
  
  cat(
    "Ahora:",
    nrow(ahora_vista),
    "noticias\n"
  )
  
  cat(
    "Últimas 24 horas:",
    nrow(ultimas_24h),
    "noticias\n"
  )
  
  cat(
    "Últimos 7 días:",
    nrow(ultimos_7_dias),
    "noticias\n"
  )
  
  cat(
    "Partidos disponibles:",
    length(filtros$partido),
    "\n"
  )
  
  cat(
    "Localidades disponibles:",
    length(filtros$localidad),
    "\n"
  )
  
  cat(
    "Medios disponibles:",
    length(filtros$medio),
    "\n"
  )
  
  cat(
    "Secciones normalizadas:",
    length(filtros$seccion),
    "\n"
  )
  
  cat("\n")
  
  cat(
    "Distribución de secciones:\n"
  )
  
  print(
    historico_preparado |>
      count(
        seccion,
        sort = TRUE
      )
  )
  
  if (
    guardar
  ) {
    
    
    cat("\n")
    
    cat(
      "Archivo:",
      archivo,
      "\n"
    )
    
    
  }
  
  cat(
    "============================================================\n"
  )
  
  invisible(
    vistas
  )
}

# ============================================================

# 8. EJECUCIÓN

# ============================================================

if (
  exists("historico")
) {
  
  vistas_noticias <-
    generar_vistas(
      historico
    )
  
} else {
  
  if (
    file.exists(
      "noticias_historicas.rds"
    )
  ) {
    
    
    historico <-
      readRDS(
        "noticias_historicas.rds"
      )
    
    vistas_noticias <-
      generar_vistas(
        historico
      )
    
    
  } else {
    
    
    stop(
      paste0(
        "No existe el objeto 'historico' y tampoco ",
        "se encontró 'noticias_historicas.rds'."
      )
    )
    
    
  }
}
