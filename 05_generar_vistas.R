# ============================================================

# 05_generar_vistas.R
# VERSIÓN DEFINITIVA
#
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
# CLASIFICADOR DE TÍTULOS
# ============================================================

# ============================================================
# Radar Bonaerense - Clasificador de títulos FINAL
#
# Archivo de prueba. NO modifica 05_generar_vistas.R
#
# Cadena efectiva reconstruida:
#   V2.7.2 -> V2.7.1 -> V2.7 -> V2.6 -> V2.5 -> V2.3
#
# V2.4 NO forma parte de la cadena final: V2.5 hace fallback
# directamente a V2.3.
# ============================================================

rx <- function(...) {
  paste(c(...), collapse = "|")
}

# ============================================================
# clasificar_seccion_titulo_v23 (recuperada del desarrollo validado)
# ============================================================

clasificar_seccion_titulo_v23 <- function(titulo) {
  
  if (is.na(titulo) || !nzchar(trimws(titulo))) {
    return(NA_character_)
  }
  
  x <- stringr::str_to_lower(titulo)
  x <- stringr::str_squish(x)
  
  
  # ==========================================================
  # 1. POLICIALES
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "asesin",
        "homicid",
        "femicid",
        
        "\\brob[a-záéíóúñ]*\\b",
        "asalt",
        "deten",
        "detuv",
        "aprehend",
        "allanamiento",
        "allanaron",
        
        "\\barmas?\\b",
        "pistola",
        "revólver",
        "revolver",
        "balazo",
        "balead",
        "balear",
        "dispar",
        "tiroteo",
        
        "narcotráfico",
        "narcotrafico",
        "drogas",
        "estupefacientes",
        "preso",
        "cárcel",
        "carcel",
        "secuestro",
        "estafa",
        "agresión",
        "agresion",
        "violencia",
        
        "incendio",
        "choque",
        "vuelco",
        "accidente",
        "siniestro vial",
        "tragedia vial",
        "atropell",
        "embist",
        "bomberos",
        
        "fiscal",
        "fiscales",
        "juicio",
        "juzgar",
        "delito",
        "crimen",
        "policía",
        "policia",
        "policial",
        "motochorro",
        "delincuente",
        "delincuentes",
        
        "ahogad",
        "desaparecid",
        "paradero",
        "autopsia",
        "coima",
        "cohecho",
        "entradera",
        
        "persecución",
        "persecucion",
        "quedó atrapado",
        "quedo atrapado",
        "accidente laboral",
        
        # Contextos específicos con jueces
        "jueza.*entradera",
        "juez.*entradera",
        "magistrad.*entradera",
        "jueza.*caja fuerte",
        "juez.*caja fuerte"
      ),
      collapse = "|"
    )
  )) {
    return("Policiales")
  }
  
  
  # ==========================================================
  # 2. DEPORTES
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        # Clubes y fútbol
        "gimnasia",
        "\\blobo\\b",
        "estudiantes de la plata",
        "estudiantes lp",
        "\\bpincha\\b",
        "\\bpinchas\\b",
        "el pincha",
        "los pinchas",
        
        "deportivo morón",
        "morón.*ascenso",
        "ascenso.*morón",
        "morón.*fútbol",
        "morón.*futbol",
        "fútbol.*morón",
        "futbol.*morón",
        
        "sarmiento.*primera",
        "primera.*sarmiento",
        "sarmiento.*liga",
        "liga.*sarmiento",
        "sarmiento.*fútbol",
        "sarmiento.*futbol",
        "fútbol.*sarmiento",
        "futbol.*sarmiento",
        "sarmiento.*torneo",
        "torneo.*sarmiento",
        "sarmiento.*ascenso",
        "ascenso.*sarmiento",
        
        "boca juniors",
        "\\bboca\\b.*(fútbol|futbol|gol|partido|copa|torneo|jugador|volante|bombonera)",
        "river plate",
        "\\briver\\b.*(fútbol|futbol|gol|partido|copa|torneo|jugador|monumental)",
        "riquelme",
        "conmebol",
        "sudamericana",
        "libertadores",
        
        "colapinto",
        "cristiano ronaldo",
        "fórmula 1",
        "formula 1",
        "automovilismo",
        "patin",
        "club de automóviles sport",
        "club de automoviles sport",
        
        "peñarol",
        "quilmes",
        "turismo 40",
        "turismo carretera",
        
        "juegos bonaerenses",
        "juegos suramericanos",
        "aprevide",
        "primera división",
        "primera division",
        "liga profesional",
        "torneo apertura",
        "torneo clausura",
        "ascenso",
        "torneo",
        "campeón",
        "campeon",
        "campeones",
        "árbitro",
        "arbitro",
        "árbitros",
        "arbitros",
        "fútbol",
        "futbol",
        "básquet",
        "basquet",
        "hockey",
        "tenis",
        "pádel",
        "padel",
        "maratón",
        "maraton",
        "polideportivo",
        "deportes electrónicos",
        "deportes electronicos",
        "juegos odesur",
        "odesur"
      ),
      collapse = "|"
    )
  )) {
    return("Deportes")
  }
  
  
  # ==========================================================
  # 3. POLÍTICA
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        # Personas / actores políticos muy específicos
        "kicillof",
        "milei",
        "katopodis",
        "caputo",
        "abad",
        "natalia gómez",
        "natalia gomez",
        "nardin[ií]",
        "santilli",
        "alak",
        "berni",
        "bullrich",
        "massa",
        "macri",
        "mayra mendoza",
        "jorge macri",
        "petrecca",
        "bianco",
        "secco",
        
        # Cargos / instituciones políticas
        "presidente",
        "presidenta",
        "gobernador",
        "gobernadora",
        "intendente",
        "intendenta",
        "intendencia",
        "senador",
        "senadora",
        "diputado",
        "diputada",
        "legislador",
        "legisladora",
        "concejal",
        "concejales",
        "hcd",
        "concejo deliberante",
        "concejo municipal",
        
        "candidato",
        "candidata",
        "elecciones",
        "elección",
        "eleccion",
        "votación",
        "votacion",
        "urna",
        
        "partido político",
        "partido politico",
        "coalición",
        "coalicion",
        
        "funcionario",
        "funcionaria",
        "ministro",
        "ministra",
        "secretario",
        "secretaria",
        "subsecretario",
        "subsecretaria",
        
        "proyecto de ley",
        "reforma de la ley",
        "nueva ley",
        "ley penal juvenil",
        "ley de baja",
        "ley de imputabilidad",
        
        "decreto",
        "ordenanza",
        
        "cámara electoral",
        "camara electoral",
        "legislatura bonaerense",
        "legislatura",
        "senado",
        "cámara de diputados",
        "camara de diputados",
        
        "paso",
        "la libertad avanza",
        "fuerza patria",
        "ucr",
        "peronismo",
        "kirchnerismo",
        "radicales",
        "radicalismo",
        
        "gobierno nacional",
        "gobierno bonaerense",
        "gobierno provincial",
        "poder ejecutivo",
        
        # Política exterior / relaciones institucionales
        "embajador",
        "embajada",
        
        # Democracia / política explícita
        "democracia",
        "agenda política",
        "agenda politica",
        "alternativa política",
        "alternativa politica",
        "armado político",
        "armado politico",
        "acuerdo político",
        "acuerdo politico",
        "alianza política",
        "alianza politica",
        "interna política",
        "interna politica",
        "política.*2027",
        "politica.*2027"
      ),
      collapse = "|"
    )
  )) {
    return("Política")
  }
  
  
  # ==========================================================
  # 4. ECONOMÍA
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "economía",
        "economia",
        "económico",
        "economico",
        "precios",
        "inflación",
        "inflacion",
        "canasta",
        "salarios",
        "salario",
        "empleo",
        "desempleo",
        "trabajo",
        "trabajadores",
        "ingresos",
        "consumo",
        "crédito",
        "credito",
        "deuda",
        "bono",
        "mercado",
        "inversión",
        "inversion",
        "inversiones",
        "empresa",
        "empresas",
        "industria",
        "industrial",
        "producción",
        "produccion",
        "productor",
        "productores",
        "productivo",
        "agro",
        "agricultura",
        "ganadería",
        "ganaderia",
        "campo",
        "comercio",
        "ventas",
        "compras",
        "emprendedores",
        "emprendimiento",
        "emprender",
        "reintegros",
        "concesionario",
        "refinería",
        "refineria",
        "ypf",
        "dólar",
        "dolar",
        "puerto quequén",
        "puerto quequen",
        "puerto de quequén",
        "puerto de quequen",
        "exportaciones",
        "importaciones",
        "mercado libre",
        "e-commerce",
        "cashback",
        "descuentos",
        "promociones"
      ),
      collapse = "|"
    )
  )) {
    return("Economía")
  }
  
  
  # ==========================================================
  # 5. EDUCACIÓN
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "educación",
        "educacion",
        "educativo",
        "educativa",
        "escuela",
        "escuelas",
        "colegio",
        "colegios",
        "universidad",
        "universidades",
        "universitario",
        "universitaria",
        "facso",
        "unicen",
        "unlp",
        "estudiantes",
        "docente",
        "docentes",
        "maestro",
        "maestras",
        "profesor",
        "profesores",
        "jardín de infantes",
        "jardin de infantes",
        "jardines de infantes",
        "parlamento juvenil",
        "extensión.*universidad",
        "extensión.*unicen",
        "feria nacional de ciencias",
        "feria de ciencias",
        "paritaria docente",
        "paro universitario"
      ),
      collapse = "|"
    )
  )) {
    return("Educación")
  }
  
  
  # ==========================================================
  # 6. SALUD
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "salud",
        "hospital",
        "hospitales",
        "médico",
        "medico",
        "médica",
        "medica",
        "medicina",
        "paciente",
        "pacientes",
        "vacuna",
        "vacunación",
        "vacunacion",
        "epidemia",
        "pandemia",
        "nutrición",
        "nutricion",
        "suicidio",
        "suicidios",
        "salud mental",
        "adicción",
        "adiccion",
        "pami",
        "higa"
      ),
      collapse = "|"
    )
  )) {
    return("Salud")
  }
  
  
  # ==========================================================
  # 7. TECNOLOGÍA
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "tecnología",
        "tecnologia",
        "innovación",
        "innovacion",
        "\\bia\\b",
        "inteligencia artificial",
        "chatgpt",
        "algoritmo",
        "software",
        "aplicación",
        "aplicacion",
        "digital",
        "drones",
        "apple",
        "iphone",
        "gps"
      ),
      collapse = "|"
    )
  )) {
    return("Tecnología")
  }
  
  
  # ==========================================================
  # 8. CULTURA
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "cultura",
        "cultural",
        "arte",
        "artista",
        "artistas",
        "museo",
        "museos",
        "libro",
        "libros",
        "literatura",
        "literario",
        "poesía",
        "poesia",
        "escritor",
        "escritora",
        "autor",
        "autora",
        "banda",
        "música",
        "musica",
        "concierto",
        "teatro",
        "teatral",
        "cine",
        "película",
        "pelicula",
        "exposición",
        "exposicion",
        "exhibición",
        "exhibicion",
        "santaolalla",
        "borges",
        "walsh",
        "josé sabogal",
        "jose sabogal",
        "memoria",
        "patrimonio",
        "campanella",
        "festival",
        "coral",
        "café concert",
        "cafe concert",
        "guitarras del mundo"
      ),
      collapse = "|"
    )
  )) {
    return("Cultura")
  }
  
  
  # ==========================================================
  # 9. ESPECTÁCULOS
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "netflix",
        "serie",
        "series",
        "trailer",
        "estreno",
        "mercedes morán",
        "mercedes moran",
        "masterchef",
        "celebrity",
        "televisión",
        "television",
        "streaming",
        "actor",
        "actriz",
        "cantante",
        "disney+",
        "disney plus"
      ),
      collapse = "|"
    )
  )) {
    return("Espectáculos")
  }
  
  
  # ==========================================================
  # 10. SOCIEDAD
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "sociedad",
        "comunidad",
        "familias",
        "familia",
        "jóvenes",
        "jovenes",
        "adolescentes",
        "adultos mayores",
        "tercera edad",
        "derechos humanos",
        "derechos de las personas",
        "solidaridad",
        "homenaje",
        "fallecimiento",
        "memoria colectiva",
        "organización social",
        "organizaciones sociales",
        "voluntariado",
        "inclusión",
        "inclusion",
        "discapacidad",
        "género",
        "genero",
        "violencia de género",
        "violencia de genero",
        "diversidad",
        "inmigrantes",
        "inmigración",
        "inmigracion",
        "italianos",
        "parroquia",
        "iglesia",
        "cementerio",
        "tradición",
        "tradicion",
        "fiesta patronal",
        "patrono",
        "colectividades",
        "inmigrante",
        "religiones",
        "religión",
        "religion"
      ),
      collapse = "|"
    )
  )) {
    return("Sociedad")
  }
  
  
  # ==========================================================
  # 11. NACIONAL
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "casa rosada",
        "congreso de la nación",
        "congreso de la nacion",
        "congreso nacional",
        "corte suprema",
        "corte suprema de justicia",
        "anses",
        "gobierno nacional",
        "poder ejecutivo nacional",
        "sector público nacional",
        "sector publico nacional",
        "ministerio de nación",
        "ministerio de nacion",
        "estados unidos",
        "brasil",
        "uruguay",
        "china",
        "rusia",
        "cuba",
        "cancillería",
        "cancilleria",
        "malvinas",
        "islas malvinas",
        "fuerzas armadas",
        "papa león",
        "papa leon"
      ),
      collapse = "|"
    )
  )) {
    return("Nacional")
  }
  
  
  # ==========================================================
  # 12. AMBIENTE
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "ambiente",
        "ambiental",
        "contaminación",
        "contaminacion",
        "recicl",
        "residuos",
        "basural",
        "relleno sanitario",
        "plásticos",
        "plasticos",
        "inundación",
        "inundacion",
        "tormenta",
        "temporal",
        "sequía",
        "sequia",
        "heladas",
        "helada",
        "humedal",
        "energía limpia",
        "energia limpia",
        "cambio climático",
        "cambio climatico",
        "riesgo climático",
        "riesgo climatico",
        "el niño",
        "super niño",
        "súper niño",
        "super nino"
      ),
      collapse = "|"
    )
  )) {
    return("Ambiente")
  }
  
  
  # ==========================================================
  # 13. TURISMO
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "turismo",
        "turístico",
        "turistica",
        "turística",
        "destino turístico",
        "destino turistico",
        "vacaciones",
        "escapada",
        "hotel",
        "hoteles"
      ),
      collapse = "|"
    )
  )) {
    return("Turismo")
  }
  
  
  # ==========================================================
  # 14. OPINIÓN
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "opinión",
        "opinion",
        "editorial",
        "columna",
        "análisis",
        "analisis"
      ),
      collapse = "|"
    )
  )) {
    return("Opinión")
  }
  
  
  # ==========================================================
  # 15. LOCAL
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "tránsito",
        "transito",
        "tráfico",
        "trafico",
        "guardia urbana",
        "licencias",
        "licencia de conducir",
        "red cloacal",
        "cloacas",
        "agua corriente",
        "servicio de agua",
        "alumbrado",
        "obra pública",
        "obra publica",
        "plaza",
        "vereda",
        "calles",
        "calle",
        "semáforo",
        "semaforo",
        "recolección",
        "recoleccion",
        "municipal",
        "municipales",
        "cementerio municipal",
        "delegación municipal",
        "delegacion municipal",
        "infraestructura vial",
        "obras viales",
        "pavimentación",
        "pavimentacion",
        "bacheo",
        "puente",
        "tránsito asistido",
        "transito asistido"
      ),
      collapse = "|"
    )
  )) {
    return("Local")
  }
  
  
  # ==========================================================
  # SIN CLASIFICAR
  # ==========================================================
  
  return(NA_character_)
}

# ============================================================
# clasificar_seccion_titulo_v25 (recuperada del desarrollo validado)
# ============================================================

clasificar_seccion_titulo_v25 <- function(titulo) {
  
  if (is.na(titulo) || !nzchar(trimws(titulo))) {
    return(NA_character_)
  }
  
  x <- titulo |>
    stringr::str_to_lower() |>
    stringr::str_squish()
  
  
  # ----------------------------------------------------------
  # 1. POLICIALES
  # Señales fuertes: tienen prioridad sobre cualquier
  # categoría temática secundaria.
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "asesin", "homicid", "femicid",
    "robo", "robó", "robaron", "robado",
    "asalt", "asalto",
    "deten", "aprehend",
    "allanamiento", "allanaron", "allanan",
    "armas?", "pistola", "revólver", "revolver",
    "balazo", "balead", "balear", "dispar", "tiroteo",
    "narcotráfico", "drogas", "estupefacientes",
    "motochorro",
    "entradera",
    "secuestro", "secuestraron",
    "estafa",
    "agresión", "agresion", "golpiza", "golpeó", "golpeado",
    "violencia",
    "delito", "crimen",
    "policía", "policia", "policial",
    "fiscal", "fiscales",
    "juicio", "juzgarán", "juzgaran",
    "coima", "cohecho",
    "pericia.*horror",
    "autopsia",
    "desaparecid", "desapareció", "desaparecio",
    "paradero",
    "atropell",
    "embist",
    "tragedia vial",
    "siniestro vial",
    "accidente laboral",
    "accidente",
    "volcó", "volco", "volcadura",
    "vuelco",
    "chocó", "choco", "chocaron",
    "colisionó", "colisiono", "colisionaron",
    "incendio", "incendió", "incendio",
    "bomberos",
    "quedó atrapado", "quedo atrapado",
    "murió.*(choque|accidente|vuelco|atropell|disparo)",
    "falleció.*(choque|accidente|vuelco|atropell|disparo)"
  ))) {
    return("Policiales")
  }
  
  
  # ----------------------------------------------------------
  # 2. DEPORTES
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "fútbol", "futbol",
    "primera c", "primera nacional",
    "ascenso",
    "clausura", "apertura",
    "reserva",
    "fecha [0-9]+",
    "liga",
    "torneo",
    "conmebol",
    "libertadores",
    "sudamericana",
    "gol(es)?",
    "ganó", "gano", "goleó", "goleo",
    "hinchas?",
    "jugadores?",
    "árbitro", "arbitro", "árbitros", "arbitros",
    "boca", "river", "racing", "independiente",
    "san lorenzo",
    "estudiantes",
    "gimnasia",
    "aldosivi",
    "morón", "moron",
    "sarmiento",
    "belgrano",
    "quilmes",
    "peñarol",
    "villa dálmine", "villa dalmine",
    "excursionistas",
    "central ballester",
    "puerto nuevo",
    "riquelme", "riquelme",
    "ponzio",
    "takahara",
    "colapinto",
    "silverstone",
    "formula 1", "fórmula 1",
    "automovilismo",
    "hockey",
    "pádel", "padel",
    "tenis",
    "maratón", "maraton",
    "patín", "patin"
  ))) {
    return("Deportes")
  }
  
  
  # ----------------------------------------------------------
  # 3. POLÍTICA
  # Señales políticas fuertes y específicas.
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "kicillof",
    "milei",
    "massa",
    "santilli",
    "ritondo",
    "abad",
    "katopodis",
    "nardini",
    "alak",
    "espinoza",
    "zamora",
    "mariel fernández", "mariel fernandez",
    "magdalena sierra",
    "secco",
    "salomón", "salomon",
    "ucr",
    "peronismo", "peronista",
    "la libertad avanza",
    "\\blla\\b",
    "\\bpro\\b",
    "elecciones",
    "electoral",
    "candidato", "candidata",
    "candidatos", "candidatas",
    "reelección", "reeleccion",
    "alianza electoral",
    "coalición electoral", "coalicion electoral",
    "paso",
    "diputados?",
    "senadores?",
    "senado",
    "cámara de diputados", "camara de diputados",
    "congreso.*(nación|nacional)",
    "casa rosada",
    "gobierno.*(nacional|provincia)",
    "nación.*gobierno", "nacion.*gobierno",
    "embajador",
    "presidente.*(nación|nacional|argentina)",
    "presidenta.*(nación|nacional|argentina)"
  ))) {
    return("Política")
  }
  
  
  # ----------------------------------------------------------
  # 4. CULTURA
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "feria del libro",
    "libro", "libros",
    "literatura", "literario", "literaria",
    "poesía", "poesia", "poeta",
    "escritor", "escritora",
    "borges", "walsh", "santaolalla",
    "sabogal",
    "teatro", "teatral",
    "cine", "película", "pelicula",
    "director.*cine",
    "campanella",
    "música", "musica", "musical",
    "concierto",
    "orquesta",
    "coro",
    "cantante",
    "flamenco",
    "festival",
    "exposición", "exposicion",
    "muestra artística", "muestra artistica",
    "pintura", "pintor",
    "arte", "artes",
    "artista", "artistas",
    "agenda cultural",
    "cultura local"
  ))) {
    return("Cultura")
  }
  
  
  # ----------------------------------------------------------
  # 5. ESPECTÁCULOS
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "lady gaga",
    "netflix",
    "masterchef",
    "celebrity",
    "serie",
    "trailer",
    "estreno",
    "famoso", "famosa",
    "mirtha legrand",
    "soledad villamil",
    "keira dugasse"
  ))) {
    return("Espectáculos")
  }
  
  
  # ----------------------------------------------------------
  # 6. EDUCACIÓN
  # Usamos señales educativas concretas.
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "escuela", "escuelas",
    "colegio", "colegios",
    "universidad", "universidades",
    "universitario", "universitaria",
    "facultad",
    "docente", "docentes",
    "maestro", "maestra", "maestros", "maestras",
    "alumno", "alumna", "alumnos", "alumnas",
    "estudiante", "estudiantes",
    "jardín de infantes", "jardin de infantes",
    "pruebas pisa",
    "educativo", "educativa",
    "educación", "educacion",
    "aprendizajes",
    "aula", "aulas",
    "parlamento juvenil",
    "extensión.*universidad",
    "extensión.*unicen",
    "facso",
    "unicen"
  ))) {
    return("Educación")
  }
  
  
  # ----------------------------------------------------------
  # 7. SALUD
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "suicidio", "suicidios",
    "salud mental",
    "hospital", "hospitales",
    "médico", "medico", "médica", "medica",
    "paciente", "pacientes",
    "enfermedad", "enfermedades",
    "vacuna", "vacunación", "vacunacion",
    "dengue",
    "autismo",
    "zoonosis",
    "epidemia",
    "sanitario", "sanitaria",
    "nutrición", "nutricion",
    "cirugía", "cirugia",
    "odontológica", "odontologica",
    "mamografías", "mamografias"
  ))) {
    return("Salud")
  }
  
  
  # ----------------------------------------------------------
  # 8. ECONOMÍA
  # Palabras económicas específicas, evitando términos
  # excesivamente genéricos.
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "canasta alimentaria",
    "canasta básica", "canasta basica",
    "inflación", "inflacion",
    "precios",
    "dólar", "dolar",
    "salario", "salarios",
    "empleo",
    "desempleo",
    "pobreza",
    "empresa", "empresas",
    "industrial", "industria",
    "producción", "produccion",
    "productivo", "productiva",
    "comercio", "comercial",
    "inversión", "inversion",
    "inversiones",
    "mercado", "mercados",
    "pymes",
    "agropecuario", "agropecuaria",
    "agricultura",
    "ganadería", "ganaderia",
    "siembra",
    "cosecha",
    "exportación", "exportacion",
    "importación", "importacion",
    "bono",
    "deuda",
    "endeudamiento",
    "crédito", "credito",
    "mora",
    "quiebra",
    "quiebras",
    "actividad económica", "actividad economica",
    "desarrollo productivo",
    "sector productivo",
    "corredor norte productivo",
    "congreso productivo",
    "puerto quequén", "puerto quequen",
    "vaca muerta",
    "ypf",
    "patentamiento",
    "inmobiliaria", "inmobiliarias",
    "locales comerciales",
    "cerraron.*locales"
  ))) {
    return("Economía")
  }
  
  
  # ----------------------------------------------------------
  # 9. LOCAL
  # Señales territoriales / municipales concretas.
  # ----------------------------------------------------------
  
  if (stringr::str_detect(x, rx(
    "obra pública", "obra publica",
    "infraestructura vial",
    "infraestructura urbana",
    "pavimentación", "pavimentacion",
    "pavimento",
    "calles", "calle",
    "caminos rurales",
    "alumbrado público", "alumbrado publico",
    "iluminación pública", "iluminacion publica",
    "veredas",
    "baches",
    "zanjón", "zanjon",
    "cloacas", "cloacal",
    "desagües", "desagues",
    "obra hidráulica", "obra hidraulica",
    "cuenca hídrica", "cuenca hidrica",
    "inundaciones",
    "tránsito", "transito",
    "pasos a nivel",
    "semáforo", "semaforo",
    "limpieza de calles",
    "mantenimiento de calles",
    "espacio público", "espacio publico",
    "guardia urbana",
    "licencias de conducir",
    "oficina de licencias",
    "corte de energía", "corte de energia",
    "corte de luz",
    "forestación urbana", "forestacion urbana"
  ))) {
    return("Local")
  }
  
  
  # ----------------------------------------------------------
  # 10. FALLBACK
  # Mantiene todo lo que ya funcionaba en V2.3.
  # ----------------------------------------------------------
  
  clasificar_seccion_titulo_v23(titulo)
}

# ============================================================
# clasificar_seccion_titulo_v26 (recuperada del desarrollo validado)
# ============================================================

clasificar_seccion_titulo_v26 <- function(titulo) {
  
  if (is.na(titulo) || !nzchar(trimws(titulo))) {
    return(NA_character_)
  }
  
  x <- titulo |>
    stringr::str_to_lower() |>
    stringr::str_squish()
  
  
  # ==========================================================
  # 1. POLICIALES CONTEXTUALES
  # ==========================================================
  
  if (stringr::str_detect(x, rx(
    "robo|robó|robaron|robado",
    "asalt|asalto",
    "allanamiento|allanaron|allanan",
    "deten|aprehend",
    "asesin|homicid|femicid|mataron",
    "balead|balear|dispar|tiroteo",
    "estupefacientes|narcotráfico|drogas",
    "entradera",
    "motochorro",
    "secuestro.*(persona|familia|trabajador|pareja)",
    "(persona|familia|pareja).*secuestrad",
    "abuso sexual",
    "material.*abuso sexual",
    "material de abuso",
    "armas blancas",
    "pistola|revólver|revolver",
    "golpiza|agredió|agredido|atacó|atacado",
    "mecheras",
    "autopsia",
    "desaparecid|paradero",
    "pericia.*(muerte|horror|crimen)",
    "incendio.*(vehículo|auto|camión|casa|vivienda|comercio)",
    "(vehículo|auto|camión).*incend",
    "volcó|volco|vuelco",
    "chocó|choco|chocaron",
    "colisionó|colisiono|colisionaron",
    "atropell",
    "embist",
    "tragedia vial",
    "siniestro vial",
    "accidente.*(murió|falleció|herido|heridos)",
    "(murió|falleció).*accidente",
    "(murió|falleció).*choc",
    "quedó atrapado|quedo atrapado"
  ))) {
    return("Policiales")
  }
  
  
  # Casos judiciales/criminales donde la palabra aislada
  # "juicio" o "fiscal" sí tiene contexto suficiente.
  
  if (stringr::str_detect(x, rx(
    "juicio.*(secuestro|asesinato|homicidio|crimen|abuso|trabajadores)",
    "juzgarán.*(gestor|acusado|acusada|hombre|mujer)",
    "fiscal.*(investiga|investigó|acusó|acusado)",
    "coima.*(deuda|soborno|dólares|pesos)",
    "gestor.*(cobró|cobro).*trámite",
    "imputad[oa].*(droga|estupefacientes|robo|delito)"
  ))) {
    return("Policiales")
  }
  
  
  # ==========================================================
  # 2. DEPORTES CONTEXTUALES
  # ==========================================================
  
  if (stringr::str_detect(x, rx(
    "estudiantes de la plata",
    "estudiantes lp",
    "estudiantes reclama.*conmebol",
    "reserva de estudiantes",
    "estudiantes.*(conmebol|clásico|clasico|partido|gol|torneo|liga|fútbol|futbol)",
    "sarmiento.*(fútbol|futbol|partido|liga|torneo|primera|river|belgrano)",
    "(fútbol|futbol|partido|liga|torneo|primera).*sarmiento",
    "morón.*(fútbol|futbol|partido|liga|torneo|ascenso|racing)",
    "(fútbol|futbol|partido|liga|torneo|ascenso).*morón",
    "lobo.*(fútbol|futbol|partido|torneo|liga|clásico|clasico)",
    "goleó|goleo|goleada",
    "conmebol",
    "libertadores",
    "sudamericana",
    "ascenso",
    "reserva.*(fútbol|futbol|partido)",
    "árbitro|arbitro|árbitros|arbitros",
    "jugador|jugadores",
    "hinchas",
    "fútbol|futbol",
    "hockey",
    "pádel|padel",
    "tenis",
    "maratón|maraton",
    "automovilismo",
    "formula 1|fórmula 1",
    "silverstone"
  ))) {
    return("Deportes")
  }
  
  
  # ==========================================================
  # 3. LOCAL FUERTE
  # Si hay una obra/servicio concreto, priorizamos Local.
  # ==========================================================
  
  if (stringr::str_detect(x, rx(
    "pasos a nivel",
    "obra pública|obra publica",
    "obras de infraestructura",
    "infraestructura vial",
    "infraestructura urbana",
    "infraestructura hídrica|infraestructura hidrica",
    "obras hídricas|obras hidricas",
    "pavimentación|pavimentacion|pavimento",
    "calles|calle",
    "caminos rurales",
    "alumbrado público|alumbrado publico",
    "iluminación pública|iluminacion publica",
    "veredas",
    "baches",
    "cloacas|cloacal",
    "desagües|desagues",
    "obra hidráulica|obra hidraulica",
    "saneamiento.*cuenca",
    "cuenca.*(arroyo|hídrica|hidrica)",
    "guardia urbana",
    "licencias de conducir",
    "corte de energía|corte de energia",
    "corte de luz",
    "semáforos|semaforos",
    "tránsito|transito"
  ))) {
    return("Local")
  }
  
  
  # ==========================================================
  # 4. POLÍTICA FUERTE
  # ==========================================================
  
  if (stringr::str_detect(x, rx(
    "kicillof.*(milei|cruzó|cruzo|cuestionó|cuestiono|ajuste)",
    "milei.*(kicillof|gobierno|provincia)",
    "katopodis.*(defendió|defendio|cuestionó|cuestiono|ajuste)",
    "katopodis.*gestión|katopodis.*gestion",
    "nardini.*(elección|eleccion|política|politica|gobierno|peronismo)",
    "alak.*(gobierno|política|politica|elección|eleccion)",
    "elecciones|electoral",
    "candidato|candidata",
    "reelección|reeleccion",
    "alianza electoral",
    "coalición|coalicion",
    "la libertad avanza",
    "\\blla\\b",
    "\\bpro\\b",
    "\\bucr\\b",
    "peronismo|peronista",
    "diputados",
    "senadores",
    "senado",
    "cámara de diputados|camara de diputados",
    "concejo deliberante.*(sesión|sesion|aprobó|aprobo|proyecto)",
    "oposición|oposicion",
    "oficialismo"
  ))) {
    return("Política")
  }
  
  
  # ==========================================================
  # 5. RESTO DE V2.5
  # ==========================================================
  
  clasificar_seccion_titulo_v25(titulo)
}

# ============================================================
# clasificar_seccion_titulo_v27 (recuperada del desarrollo validado)
# ============================================================

clasificar_seccion_titulo_v27 <- function(titulo) {
  
  resultado <- clasificar_seccion_titulo_v26(titulo)
  
  # Solo trabajar sobre los que V2.6 dejó sin clasificar
  if (!is.na(resultado)) {
    return(resultado)
  }
  
  x <- stringr::str_to_lower(
    stringr::str_squish(
      dplyr::coalesce(titulo, "")
    )
  )
  
  # ============================================================
  # 1. POLICIALES
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\brobo\\b",
      "\\brobaron\\b",
      "\\brobó\\b",
      "\\brobo de\\b",
      "\\brobada\\b",
      "\\brobado\\b",
      "\\brobos\\b",
      "\\ballanamiento\\b",
      "\\ballanaron\\b",
      "\\ballanó\\b",
      "\\bestupefacientes\\b",
      "\\bdrogas\\b",
      "\\bcocaína\\b",
      "\\bmarihuana\\b",
      "\\barma de fuego\\b",
      "\\brevólver\\b",
      "\\bpistola\\b",
      "\\bbalazo\\b",
      "\\btiroteo\\b",
      "\\baprehendid",
      "\\bdetenid",
      "\\bpersecución policial\\b",
      "\\bpolicía .* rescat",
      "\\bvioló la restricción\\b",
      "\\bagredió .* policía\\b",
      "\\bmecheras\\b",
      "\\bsiniestro vial\\b",
      "\\bchoque .* murió\\b",
      "\\bchocó .* murió\\b",
      "\\bvuelco .* herid",
      "\\bvolcó .* herid",
      "\\bincendio .* murió\\b",
      "\\bincendio .* falleció\\b",
      "\\bincendio .* muertos\\b",
      "\\bfue encontrada sin vida\\b"
    ), collapse = "|")
  )) {
    return("Policiales")
  }
  
  
  # ============================================================
  # 2. DEPORTES
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bascenso\\b",
      "\\bfútbol\\b",
      "\\bpartido\\b",
      "\\bjuega\\b",
      "\\bjuegan\\b",
      "\\bjugador\\b",
      "\\bjugadores\\b",
      "\\btorneo\\b",
      "\\btenis\\b",
      "\\bautomovilismo\\b",
      "\\bfórmula 1\\b",
      "\\bcolapinto\\b",
      "\\bjuegos bonaerenses\\b",
      "\\bjuegos odesur\\b",
      "\\bconmebol\\b",
      "\\briver\\b.*\\bpartido\\b",
      "\\bsarmiento\\b.*\\bprimera\\b",
      "\\bsarmiento\\b.*\\bpartido\\b",
      "\\bmorón\\b.*\\bascenso\\b",
      "\\bmorón\\b.*\\bjuega\\b",
      "\\blobo\\b.*\\bvisita\\b",
      "\\bpinchas\\b",
      "\\baldosivi\\b",
      "\\bexcursionistas\\b",
      "\\bferro\\b.*\\breserva\\b",
      "\\bcristiano ronaldo\\b",
      "\\briquelme\\b.*\\bboca\\b",
      "\\bpatinadora\\b",
      "\\bcampeón\\b.*\\bpartido\\b"
    ), collapse = "|")
  )) {
    return("Deportes")
  }
  
  
  # ============================================================
  # 3. CULTURA
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bhuésped de honor\\b",
      "\\bsantaolalla\\b",
      "\\blibro\\b",
      "\\bliteratura\\b",
      "\\bescritor\\b",
      "\\bescritora\\b",
      "\\bpoesía\\b",
      "\\bdanza\\b",
      "\\bfolklóric",
      "\\bfolklore\\b",
      "\\bteatro\\b",
      "\\bcine\\b",
      "\\bconcierto\\b",
      "\\bcoral\\b",
      "\\bartes\\b",
      "\\bartista\\b",
      "\\bexposición\\b",
      "\\bmural\\b",
      "\\bcafé concert\\b",
      "\\bcartelera\\b",
      "\\bmemoria.*leer\\b"
    ), collapse = "|")
  )) {
    return("Cultura")
  }
  
  
  # ============================================================
  # 4. ESPECTÁCULOS
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\blady gaga\\b",
      "\\bstreamer\\b",
      "\\bcelebrity\\b",
      "\\bshow\\b",
      "\\bshows\\b",
      "\\bfauda\\b",
      "\\bnetflix\\b",
      "\\btelevisión\\b",
      "\\btv\\b",
      "\\bcelebró.*amor\\b"
    ), collapse = "|")
  )) {
    return("Espectáculos")
  }
  
  
  # ============================================================
  # 5. EDUCACIÓN
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bfacso\\b",
      "\\bunicen\\b",
      "\\bunlp\\b.*\\bestudiant",
      "\\buba\\b.*\\bfacultad\\b",
      "\\bunq\\b",
      "\\bespacios? educativos?\\b",
      "\\bescuela\\b",
      "\\bescuelas\\b",
      "\\bmaestra\\b",
      "\\bmaestras\\b",
      "\\bmaestro\\b",
      "\\bdocente\\b",
      "\\bdocentes\\b",
      "\\bprofesor\\b",
      "\\bprofesores\\b",
      "\\baula\\b",
      "\\bestudiantes\\b",
      "\\bestudiant\\b.*\\bescuela\\b",
      "\\bcurso gratuito\\b",
      "\\bcapacitación\\b",
      "\\bfacultad\\b",
      "\\buniversidad\\b",
      "\\buniversitario\\b",
      "\\buniversitaria\\b",
      "\\bparitaria\\b.*\\bdocent",
      "\\bdía del maestro\\b"
    ), collapse = "|")
  )) {
    return("Educación")
  }
  
  
  # ============================================================
  # 6. SALUD
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bsalud mental\\b",
      "\\bsuicidio\\b",
      "\\bprevención del suicidio\\b",
      "\\bhospital\\b",
      "\\bhospitalizada\\b",
      "\\bhospitalizado\\b",
      "\\bterapia intensiva\\b",
      "\\bmédica\\b.*\\bfacultad de medicina\\b",
      "\\bmedicina\\b.*\\bfacultad\\b",
      "\\bgeriátrico\\b",
      "\\bgeriátrica\\b"
    ), collapse = "|")
  )) {
    return("Salud")
  }
  
  
  # ============================================================
  # 7. ECONOMÍA
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bcanasta alimentaria\\b",
      "\\bcrédito a las familias\\b",
      "\\bmora\\b",
      "\\bproductor\\b",
      "\\bproductores\\b",
      "\\bproducción\\b",
      "\\bproductivo\\b",
      "\\bindustria\\b",
      "\\bindustrial\\b",
      "\\bcomercio\\b",
      "\\bempresas\\b",
      "\\bempresa\\b",
      "\\binversiones\\b",
      "\\binversión\\b",
      "\\bfinancier",
      "\\bmercado\\b",
      "\\bagropecuario\\b",
      "\\bagro\\b",
      "\\bsemillas\\b",
      "\\bconcesionario\\b",
      "\\bmonotributo\\b",
      "\\bemprendedores\\b",
      "\\beconomía\\b",
      "\\bcrisis económica\\b"
    ), collapse = "|")
  )) {
    return("Economía")
  }
  
  
  # ============================================================
  # 8. LOCAL
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bobras? municipales?\\b",
      "\\bobras? públicas?\\b",
      "\\basfalto\\b",
      "\\bpaviment",
      "\\bcalles\\b",
      "\\bveredas\\b",
      "\\biluminación\\b",
      "\\benergía\\b.*\\bservicio\\b",
      "\\bcorte de energía\\b",
      "\\bservicio de agua\\b",
      "\\btránsito\\b",
      "\\bradares\\b",
      "\\bcámaras\\b.*\\bmunicip",
      "\\bmunicipio\\b.*\\bobras\\b",
      "\\bmunicipalidad\\b.*\\bobras\\b",
      "\\bguardia urbana\\b",
      "\\bvecinos\\b.*\\bcalles\\b",
      "\\bentrega de escrituras\\b"
    ), collapse = "|")
  )) {
    return("Local")
  }
  
  
  # ============================================================
  # 9. POLÍTICA
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\boficialismo\\b",
      "\\boposición\\b",
      "\\blla\\b",
      "\\bperonismo\\b",
      "\\bcandidato\\b",
      "\\bcandidata\\b",
      "\\belecciones\\b",
      "\\b2027\\b",
      "\\bproyecto para modificar\\b",
      "\\bproyecto reclamando\\b",
      "\\bdiputada\\b",
      "\\bdiputado\\b",
      "\\bsenador\\b",
      "\\bsenadora\\b",
      "\\bconcejal\\b.*\\bradical\\b",
      "\\bconcejales\\b.*\\bradical\\b",
      "\\bconcejo deliberante\\b",
      "\\bquórum\\b",
      "\\bpliego\\b.*\\btransporte\\b",
      "\\bkatopodis\\b.*\\bconvenio\\b",
      "\\bkicillof\\b.*\\banuncio",
      "\\bmilei\\b",
      "\\bfurlán\\b",
      "\\buocra\\b.*\\bcausa\\b"
    ), collapse = "|")
  )) {
    return("Política")
  }
  
  
  # ============================================================
  # 10. AMBIENTE
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bel niño\\b",
      "\\bcuenca hídrica\\b",
      "\\brío salado\\b",
      "\\bcambio climático\\b",
      "\\bclima\\b.*\\btemperatura\\b",
      "\\balerta amarilla\\b",
      "\\bfrío extremo\\b"
    ), collapse = "|")
  )) {
    return("Ambiente")
  }
  
  
  # ============================================================
  # 11. TECNOLOGÍA
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\binteligencia artificial\\b",
      "\\bdesarrollo de la ia\\b",
      "\\btecnología\\b",
      "\\btecnológica\\b",
      "\\bcelulares\\b",
      "\\binnovación\\b"
    ), collapse = "|")
  )) {
    return("Tecnología")
  }
  
  
  # ============================================================
  # 12. TURISMO
  # ============================================================
  
  if (stringr::str_detect(
    x,
    paste(c(
      "\\bcircuito turístico\\b",
      "\\bturismo\\b",
      "\\bturística\\b",
      "\\bturístico\\b"
    ), collapse = "|")
  )) {
    return("Turismo")
  }
  
  
  NA_character_
}

# ============================================================
# clasificar_seccion_titulo_v271 (recuperada del desarrollo validado)
# ============================================================

clasificar_seccion_titulo_v271 <- function(titulo) {
  
  x <- stringr::str_to_lower(
    stringr::str_squish(
      dplyr::coalesce(titulo, "")
    )
  )
  
  # ==========================================================
  # CORRECCIONES DE ALTA PRECISIÓN
  # ==========================================================
  
  # 1. Hechos violentos: prioridad sobre Salud
  if (stringr::str_detect(
    x,
    paste(
      c(
        "atacad[oa] a martillazos",
        "apuñalad",
        "balead",
        "asesinad",
        "fue asesinad",
        "murió tras ser atacad",
        "atacado a golpes",
        "atacada a golpes"
      ),
      collapse = "|"
    )
  )) {
    return("Policiales")
  }
  
  # 2. Educación explícita
  if (stringr::str_detect(
    x,
    paste(
      c(
        "\\beducadores?\\b",
        "\\bdocentes?\\b",
        "\\bmaestros?\\b",
        "\\bprofesores?\\b",
        "\\bescuelas?\\b",
        "\\buniversidad\\b",
        "\\buniversidades\\b",
        "\\bfacultad\\b",
        "\\bestudiantes?\\b",
        "\\bcapacitación\\b",
        "\\bcapacitacion\\b",
        "\\bcurso gratuito\\b"
      ),
      collapse = "|"
    )
  )) {
    return("Educación")
  }
  
  # 3. Cultura / espectáculos
  if (stringr::str_detect(
    x,
    paste(
      c(
        "\\bcarnaval\\b",
        "\\bfiesta del asado\\b",
        "\\bshow\\b",
        "\\brecital\\b",
        "\\bconcierto\\b",
        "\\bteatro\\b",
        "\\bmúsica\\b",
        "\\bmusica\\b"
      ),
      collapse = "|"
    )
  )) {
    return("Espectáculos")
  }
  
  # 4. Contexto deportivo fuerte
  if (stringr::str_detect(
    x,
    paste(
      c(
        "\\bautódromo\\b",
        "\\bautodromo\\b",
        "\\bcarrera\\b.*\\bautomovil",
        "\\bautomovilismo\\b",
        "\\bfútbol\\b",
        "\\bfutbol\\b",
        "\\bpartido\\b.*\\bequipo\\b"
      ),
      collapse = "|"
    )
  )) {
    return("Deportes")
  }
  
  # ==========================================================
  # V2.7 ORIGINAL COMO FALLBACK
  # ==========================================================
  
  clasificar_seccion_titulo_v27(titulo)
}

# ============================================================
# clasificar_seccion_titulo_v272 (recuperada del desarrollo validado)
# ============================================================

clasificar_seccion_titulo_v272 <- function(titulo) {
  
  x <- stringr::str_to_lower(
    stringr::str_squish(
      dplyr::coalesce(titulo, "")
    )
  )
  
  # ==========================================================
  # CORRECCIÓN: causas judiciales por muertes / hechos graves
  # ==========================================================
  
  if (stringr::str_detect(
    x,
    paste(
      c(
        "\\bcausa\\b.*\\bmuert",
        "\\bcausa\\b.*\\basesinat",
        "\\bcausa\\b.*\\bhomicidio",
        "\\bcorte\\b.*\\bmuert",
        "\\bjuicio\\b.*\\bmuert"
      ),
      collapse = "|"
    )
  )) {
    return("Policiales")
  }
  
  # ==========================================================
  # V2.7.1
  # ==========================================================
  
  clasificar_seccion_titulo_v271(titulo)
}

# ============================================================
# FUNCIÓN FINAL CONSOLIDADA
# ============================================================
# Esta es la única función que usaremos para las pruebas.
# Las versiones históricas se mantienen arriba únicamente para
# reproducir exactamente la cadena validada.
# ============================================================

clasificar_seccion_titulo_final <- function(titulo) {
  clasificar_seccion_titulo_v272(titulo)
}


# ============================================================
# CLASIFICADOR DE TÍTULOS - V2.8
# ============================================================

clasificar_seccion_titulo_v28 <- function(titulo) {
    if (is.na(titulo) || !nzchar(trimws(titulo))) {
        return(NA_character_)
    }
    x <- stringr::str_squish(stringr::str_to_lower(titulo))
    if (stringr::str_detect(x, rx("prevención del suicidio|prevencion del suicidio", 
        "suicidio", "donación de sangre|donacion de sangre", 
        "terapia ocupacional", "alzheimer", "coqueluche", "controles oftalmológicos|controles oftalmologicos", 
        "control.*oftalmológ|control.*oftalmolog", "acompañantes terapéuticos|acompanantes terapeuticos", 
        "salud mental", "bienestar.*(salud|emocional)", "riesgos de beber alcohol", 
        "alcohol y manejar"))) 
        return("Salud")
    if (stringr::str_detect(x, rx("pronóstico|pronostico", "panorama meteorológico|panorama meteorologico", 
        "clima", "tiempo.*(lluvia|frío|frio|viento|temperatura)", 
        "lluvia|lluvias", "temperaturas?.*(frío|frio|cálid|calid|primaveral)", 
        "descenso térmico|descenso termico", "alerta.*(viento|lluvia|meteorológ|meteorolog)", 
        "sudestada", "forestación|forestacion", "arbolado urbano", 
        "árboles añosos|arboles añosos", "plantan .*sauces", 
        "problemas hídricos|problemas hidricos", "prevención hídrica|prevencion hidrica", 
        "canales rurales.*(hídric|hidric)", "lagunas glaciares", 
        "aluviones|avalanchas"))) 
        return("Ambiente")
    if (stringr::str_detect(x, rx("\\btc\\b.*(copa|piloto|líder|lider)", 
        "ferrari.*f1|f1.*ferrari", "fórmula 1|formula 1", "piloto argentino.*f1", 
        "juegos.*(clasific|compet|deport)", "juegos bonaerenses", 
        "sudamericano.*u19", "juvenil.*(derrota|certamen|torneo)", 
        "nadadores?.*(aguas|lago)", "título argentino|titulo argentino", 
        "supermedianos", "fútbol|futbol", "partido|torneo|liga", 
        "pedalear", "carrera.*(deport|jardín|jardin)"))) 
        return("Deportes")
    if (stringr::str_detect(x, rx("poesía|poesia", "literatura", 
        "literario|literaria", "libro|libros", "novela", "narrador|narradores", 
        "muestra.*(museo|arte)", "museo", "planetario", "encuentro de ciencia", 
        "historia.*(vida|pensamiento|argentin)", "efemérides|efemerides", 
        "acontecimientos e hitos históricos|acontecimientos e hitos historicos", 
        "folclore|folklore", "tango", "orquesta", "cuarteto de cuerdas", 
        "teatro", "escenario", "arte"))) 
        return("Cultura")
    if (stringr::str_detect(x, rx("álbum|album", "nuevo disco", 
        "nuevo ep", "lanzó.*(álbum|album|disco|ep)", "lanzó.*adelanto|lanzo.*adelanto", 
        "presenta.*show", "llega.*show", "recital", "documental.*(banda|grupo|música|musica)", 
        "actor|actriz", "humorista", "stand.?up", "moria casán|moria casan", 
        "babasónicos|babasonicos", "oasis.*(documental|escena|reencuentro)", 
        "iván noble|ivan noble", "los tabaleros", "bebe.*argentina"))) 
        return("Espectáculos")
    if (stringr::str_detect(x, rx("informe pisa", "ausentismo escolar", 
        "rendimiento académico|rendimiento academico", "desigualdad socioeconómica|desigualdad socioeconomica", 
        "escuela|escolar|escolares", "alumnos|alumnas", "docentes", 
        "universidad", "facultad", "carrera.*(educativ|universitari)", 
        "jardín 902|jardin 902", "jardín 918|jardin 918"))) 
        return("Educación")
    if (stringr::str_detect(x, rx("openai", "anthropic", "modelos.*(ia|inteligencia artificial)", 
        "inteligencia artificial", "agentes autónomos|agentes autonomos", 
        "videojuegos", "videojuego", "whatsapp.*(comunicación|comunicacion|infraestructura)", 
        "infraestructura de comunicación social"))) 
        return("Tecnología")
    if (stringr::str_detect(x, rx("búsqueda laboral|busqueda laboral", 
        "búsqueda de personal|busqueda de personal", "empleo", 
        "despidos", "reincorporación.*(trabajador|empleado|fletero)", 
        "costo de la construcción|costo de la construccion", 
        "construcción.*(cayó|cayo|aument)", "aumento.*(tarifa|boleto|gas|electric)", 
        "tarifas eléctricas|tarifas electricas", "aumentos en el gas", 
        "recaudación.*impuesto", "concesión.*tren|concesion.*tren", 
        "tren de cargas", "comerciantes", "comercio", "empresa", 
        "invirtió|invirtio", "inversiones", "real estate", "retail", 
        "outlet", "frigorífico|frigorifico", "cargill", "techint", 
        "puertos.*(carga|negocio|comerc)", "feria.*emprendedora", 
        "vibra emprendedora", "marcas locales"))) 
        return("Economía")
    if (stringr::str_detect(x, rx("cordón cuneta|cordon cuneta", 
        "reasfaltado", "pavimentación|pavimentacion", "renovación integral.*ruta|renovacion integral.*ruta", 
        "ruta provincial", "ruta [0-9]+", "obras.*barrio", "nuevas obras.*barrio", 
        "obras hidráulicas|obras hidraulicas", "red de agua", 
        "corte.*energía|corte.*energia", "sin luz", "servicio.*agua", 
        "servicio.*energía|servicio.*energia", "wifi gratuitos", 
        "wi-fi gratuitos", "limpieza en los barrios", "operativos.*limpieza", 
        "vehículos secuestrados|vehiculos secuestrados", "transporte.*(whatsapp|servicio|línea|linea)", 
        "boleto.*colectivos", "municipio", "concejo deliberante", 
        "sum del barrio", "ordenamiento territorial", "hábitat|habitat", 
        "loteo irregular"))) 
        return("Local")
    if (stringr::str_detect(x, rx("donación.*(fundación|fundacion)", 
        "fundación|fundacion", "organizaciones", "redes de apoyo", 
        "red feminista", "feminismos", "derechos humanos", "desaparecid", 
        "periodismo", "gremios", "sindicatos", "sindical", "movilización.*(trabajadores|gremios|sindicato)", 
        "bomberos voluntarios", "bomberos", "comunidad", "solidari", 
        "dioces", "peregrinación|peregrinacion", "Papa", "lactancia", 
        "infancias"))) 
        return("Sociedad")
    if (stringr::str_detect(x, rx("alcoholemia.*g/l", "dio [0-9]+[,\\.]?[0-9]* g/l", 
        "delictivos", "hechos delictivos", "inseguridad", "asesinat", 
        "apuñal", "apuñaló|apunaló|apuno", "robo", "allanamiento", 
        "detenid|detuvieron", "cámaras.*sospechoso|camaras.*sospechoso", 
        "amenazas.*cárcel|amenazas.*carcel", "forzando puertas", 
        "fuego.*explosión|fuego.*explosion", "muertes.*al volante", 
        "murió.*vías|murio.*vias", "accidente", "homicidio", 
        "femicidio|feminicidio", "secuestro", "estafa"))) 
        return("Policiales")
    if (stringr::str_detect(x, rx("turismo", "turístico|turistico", 
        "naufragio.*(conocer|visitar)", "visita.*(lugares|ciudad)", 
        "qué hacer.*(mar del plata|necochea|tandil|ciudad)", 
        "que hacer.*(mar del plata|necochea|tandil|ciudad)"))) 
        return("Turismo")
    clasificar_seccion_titulo_final(titulo)
}


# ============================================================
# V2.8.1 - CORRECCIONES VALIDADAS MANUALMENTE
# ============================================================

clasificar_seccion_titulo_v281 <- function(titulo) {

  if (is.na(titulo) || !nzchar(trimws(titulo))) {
    return(NA_character_)
  }

  x <- stringr::str_to_lower(
    stringr::str_squish(titulo)
  )

  base <- clasificar_seccion_titulo_final(titulo)

  if (!is.na(base)) {
    return(base)
  }

  nueva <- clasificar_seccion_titulo_v28(titulo)

  if (stringr::str_detect(
    x,
    "mejora el clima.*scooter el[eé]ctrico"
  )) {
    return("Local")
  }

  if (stringr::str_detect(
    x,
    "6ª expo fr[ií]o calor argentina"
  )) {
    return(NA_character_)
  }

  if (stringr::str_detect(
    x,
    "luis toro.*voz.*camino.*escenarios"
  )) {
    return("Espectáculos")
  }

  if (stringr::str_detect(
    x,
    "oasis hace historia en argentina"
  )) {
    return("Espectáculos")
  }

  if (stringr::str_detect(
    x,
    "el rap del barrio.*escenario"
  )) {
    return("Espectáculos")
  }

  if (stringr::str_detect(
    x,
    "alvear.*municipio.*sindicato.*aumento del 12%"
  )) {
    return("Economía")
  }

  if (stringr::str_detect(
    x,
    "municipio ofreci[oó] un aumento del 7%.*gremios"
  )) {
    return("Economía")
  }

  if (stringr::str_detect(
    x,
    "municipio.*edad.*responsabilidad.*penal"
  )) {
    return("Política")
  }

  if (stringr::str_detect(
    x,
    "ley de criminalizaci[oó]n de las infancias"
  )) {
    return("Política")
  }

  if (stringr::str_detect(
    x,
    "corte.*revoc[oó].*sobreseimiento.*pata.*medina"
  )) {
    return("Política")
  }

  if (stringr::str_detect(
    x,
    "granja tres arroyos.*sindicatos.*naci[oó]n.*intervenci[oó]n"
  )) {
    return("Economía")
  }

  if (stringr::str_detect(
    x,
    "muri[oó] chiche gelblung"
  )) {
    return("Espectáculos")
  }

  if (stringr::str_detect(
    x,
    "comit[eé] de lactancia.*primer encuentro"
  )) {
    return("Salud")
  }

  if (!is.na(nueva)) {
    return(nueva)
  }

  NA_character_
}

# ============================================================

# V2.9 / V2.9.1: ampliación conservadora validada
# Se invoca solo si V2.7.2 y V2.8.1 no clasificaron.

clasificar_seccion_titulo_v29 <- function(titulo) {
  
  if (is.na(titulo) || !nzchar(trimws(titulo))) {
    return(NA_character_)
  }
  
  x <- stringr::str_squish(
    stringr::str_to_lower(titulo)
  )
  
  # ============================================================
  # BASE: V2.8.1
  # ============================================================
  
  base <- clasificar_seccion_titulo_v281(titulo)
  
  if (!is.na(base)) {
    return(base)
  }
  
  
  # ============================================================
  # 1. DEPORTES
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "\\bmls\\b",
      "premier league",
      "conmebol",
      "libertadores",
      "sudamericana",
      "liga profesional",
      "ascenso",
      "fútbol|futbol",
      "futbolista",
      "entrenador",
      "jugador|jugadores",
      "partido|partidos",
      "torneo|torneos",
      "copa.*(fútbol|futbol|liga)",
      "de paul.*(city|madrid|m[sí]l)",
      "kimberley.*(salta|partido|zona)",
      "chacarita.*(atlanta|partido|chance)",
      "argentino de merlo",
      "deportivo merlo",
      "ituzaingó.*(ganar|descender|punto|partido)",
      "dálmine|dalmine",
      "arsenal.*(partido|dálmine|dalmine)",
      "ciclismo|ciclista|ciclistas",
      "sexta etapa",
      "taekwondo",
      "skyl.*(campe[oó]n|ganador|mejor)",
      "juegos.*(ensena|ensenada|clasific|deport)",
      "agrarias.*ferro",
      "ferroviarios",
      "patos.*(partido|torneo)",
      "bocha ciantini"
    )
  )) {
    return("Deportes")
  }
  
  
  # ============================================================
  # 2. ESPECTÁCULOS
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "daniel agostini",
      "boquita de caramelo",
      "rené lavand|rene lavand",
      "titanes en el ring",
      "chiche lagar",
      "oreiro love",
      "oasis.*(gran pantalla|película|pelicula|documental)",
      "eterna inocencia",
      "manal.*(toca|recital|show)",
      "chiche gelblung",
      "agó páez vilat|ago paez vilat",
      "maría josé gentile.*(flor vign|encuentro)",
      "pato benegas.*espectáculo",
      "pato benegas.*espectaculo"
    )
  )) {
    return("Espectáculos")
  }
  
  
  # ============================================================
  # 3. CULTURA
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "castagnino.*(muestra|puertas)",
      "seminario de chi kung",
      "mercedes pérez sabbi|mercedes perez sabbi",
      "la grasita",
      "la brisa de la paz",
      "respuesta entre letras",
      "amauta.*cien años",
      "amauta.*instrucciones",
      "antecedentes.*intuiciones.*amauta",
      "muestra.*museo",
      "muestra.*arte",
      "museo.*(muestra|exposición|exposicion)",
      "literatura",
      "poesía|poesia"
    )
  )) {
    return("Cultura")
  }
  
  
  # ============================================================
  # 4. ECONOMÍA
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "cámara textil",
      "camara textil",
      "cuenta dni",
      "carrefour",
      "transferencias.*provincias",
      "garbarino",
      "cerró.*sucursal|cerro.*sucursal",
      "sucursales.*cerr",
      "cierra.*sucursal",
      "edelap.*(recurso|usuarios|bonificación|bonificacion)",
      "fleteros.*(medidas|gremiales).*puertos",
      "puertos.*(fleteros|carga|negocio)",
      "oseba.*facturas",
      "oceba.*facturas",
      "plan gas",
      "facturas de luz",
      "subas.*luz",
      "subas.*electric",
      "tarifas.*eléctricas|tarifas.*electricas",
      "trading de cfds",
      "plataformas de trading",
      "actividad.*(caída|caida)",
      "actividad.*(cerr|cierre)",
      "marca de ropa.*(persiana|cerr)",
      "baja.*persiana",
      "baj[aó].*persianas",
      "recaudación",
      "inversiones",
      "invirtió|invirtio",
      "empleo",
      "despidos",
      "actividad económica|actividad economica",
      "actividad industrial",
      "construcción.*(caída|caida|baja|aumento)",
      "construccion.*(caida|baja|aumento)"
    )
  )) {
    return("Economía")
  }
  
  
  # ============================================================
  # 5. POLÍTICA
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "grave denuncia contra axel",
      "axel.*(gobierno|provincia|milei|cruzó|cruzo|cuestionó|cuestiono)",
      "régimen penal juvenil|regimen penal juvenil",
      "jury.*(montenegro|intendente|juez)",
      "montenegro.*jury",
      "expulsión.*cúneo|expulsion.*cuneo",
      "santiago cúneo|santiago cuneo",
      "\\bpj bonaerense\\b",
      "momento de andar de campaña",
      "andar de campaña",
      "unidad de control.*(polémica|polemica)",
      "gobierno.*aliados parlamentarios",
      "aliados parlamentarios",
      "dirigentes.*massista",
      "dirigentes.*(sánchez|sanchez)",
      "villarruel.*chivilcoy",
      "villarruel",
      "campaña.*(gobierno|elección|eleccion)",
      "elección.*(gobierno|dirigente|partido)",
      "eleccion.*(gobierno|dirigente|partido)"
    )
  )) {
    return("Política")
  }
  
  
  # ============================================================
  # 6. NACIONAL
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "hamás|hamas",
      "netanyahu",
      "af[d] .*mayoría absoluta|af[d].*mayoria absoluta",
      "putin.*trump",
      "enviados de trump",
      "córdoba elevó|cordoba elevó|cordoba elevo",
      "llaryora",
      "tercero arriba",
      "nación servicios.*sube",
      "nación.*(recursos|reparto)",
      "nacion.*(recursos|reparto)"
    )
  )) {
    return("Nacional")
  }
  
  
  # ============================================================
  # 7. LOCAL
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "alejandro korn.*obra",
      "obra.*alejandro korn",
      "licitación de colectivos.*moreno|licitacion de colectivos.*moreno",
      "peatonal laprida",
      "tareas con hormigón|tareas con hormigon",
      "hormigón.*(calle|avenida|ruta)|hormigon.*(calle|avenida|ruta)",
      "basura clandestina",
      "internet libre para sarasa",
      "stmc.*obras",
      "la plata.*ganador.*(millones|millón|millon)",
      "qué hacer.*moreno|que hacer.*moreno",
      "fin de semana.*moreno",
      "finde.*escobar",
      "tigre.*túnel|tigre.*tunel",
      "chacabuco.*enciso.*túnel|chacabuco.*enciso.*tunel",
      "aySA.*calidad del servicio",
      "aysa.*calidad del servicio",
      "obras.*(barrio|calle)",
      "calle.*(obra|pavimento|peatonal)",
      "servicio.*agua",
      "corte.*agua",
      "municipio.*(obra|calle|barrio|servicio)"
    )
  )) {
    return("Local")
  }
  
  
  # ============================================================
  # 8. AMBIENTE
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "viento y frío|viento y frio",
      "fin de semana frío|fin de semana frio",
      "fin de semana.*bajas temperaturas",
      "nevada.*bahía blanca|nevada.*bahia blanca",
      "tiempo.*necochea",
      "cielo parcialmente nublado",
      "jornada fría y nublada|jornada fria y nublada",
      "eclipse",
      "pronóstico|pronostico",
      "temperaturas.*(frío|frio)",
      "temperaturas.*(baja|bajas)",
      "mal tiempo"
    )
  )) {
    return("Ambiente")
  }
  
  
  # ============================================================
  # 9. TURISMO
  # ============================================================
  
  if (stringr::str_detect(
    x,
    rx(
      "tesoro escondido.*mar del plata",
      "historia secreta.*mar del plata",
      "qué hacer.*fin de semana|que hacer.*fin de semana",
      "imperdibles.*fin de semana",
      "lugares.*visitar",
      "qué visitar|que visitar",
      "viajero",
      "viajar",
      "turismo",
      "turístico|turistico",
      "conocer.*(ciudad|lugar|lugares)",
      "naufragio.*conocer"
    )
  )) {
    return("Turismo")
  }
  
  
  # ============================================================
  # SIN CLASIFICACIÓN
  # ============================================================
  
  NA_character_
}

clasificar_seccion_titulo_v291 <- function(titulo) {
  
  if (is.na(titulo) || !nzchar(trimws(titulo))) {
    return(NA_character_)
  }
  
  x <- stringr::str_squish(
    stringr::str_to_lower(titulo)
  )
  
  base <- clasificar_seccion_titulo_v281(titulo)
  
  if (!is.na(base)) {
    return(base)
  }
  
  v29 <- clasificar_seccion_titulo_v29(titulo)
  
  # Corrección específica:
  # motociclista herido tras chocar = hecho vial/policial
  if (stringr::str_detect(
    x,
    "12 de octubre.*torcuato de alvear.*motociclista.*herido.*choc"
  )) {
    return("Policiales")
  }
  
  v29
}


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
  # PRIORIDAD ORIGINAL:
  #
  # 1. RSS reconocido
  # 2. URL reconocida
  # 3. Sin sección
  # ----------------------------------------------------------
  
  # ----------------------------------------------------------
  # SECCIÓN FINAL
  #
  # Prioridad:
  #   1. RSS
  #   2. URL
  #   3. Clasificador por título
  #
  # "Sin sección" se considera equivalente a ausencia de sección
  # para permitir que intervenga el clasificador por título.
  # ----------------------------------------------------------

  seccion_final <- ifelse(
    !is.na(seccion_rss) & seccion_rss != "Sin sección",
    seccion_rss,
    seccion_url_normalizada
  )

  # Si la URL tampoco aporta sección, convertir "Sin sección"
  # explícito en NA para habilitar el clasificador por título.
  seccion_final[
    !is.na(seccion_final) & seccion_final == "Sin sección"
  ] <- NA_character_

  necesita_titulo <- is.na(seccion_final)

  if (any(necesita_titulo)) {

    titulos <- if ("titulo" %in% names(noticias)) {
      noticias$titulo
    } else {
      rep(NA_character_, nrow(noticias))
    }

    # ----------------------------------------------------------
    # PRIMERA ETAPA: clasificador V2.7.2
    # ----------------------------------------------------------

    clasificacion_v272 <- rep(
      NA_character_,
      nrow(noticias)
    )

    clasificacion_v272[necesita_titulo] <-
      vapply(
        titulos[necesita_titulo],
        clasificar_seccion_titulo_final,
        character(1)
      )

    usar_v272 <- necesita_titulo &
      !is.na(clasificacion_v272)

    seccion_final[usar_v272] <-
      clasificacion_v272[usar_v272]

    # ----------------------------------------------------------
    # SEGUNDA ETAPA: V2.8.1
    #
    # Solo se aplica cuando V2.7.2 no pudo clasificar.
    # V2.8.1 preserva todas las clasificaciones anteriores.
    # ----------------------------------------------------------

    necesita_v281 <-
      is.na(seccion_final) &
      necesita_titulo

    if (any(necesita_v281)) {

      clasificacion_v281 <- rep(
        NA_character_,
        nrow(noticias)
      )

      clasificacion_v281[necesita_v281] <-
        vapply(
          titulos[necesita_v281],
          clasificar_seccion_titulo_v281,
          character(1)
        )

      usar_v281 <- necesita_v281 &
        !is.na(clasificacion_v281)

      seccion_final[usar_v281] <-
        clasificacion_v281[usar_v281]
    }

    # ----------------------------------------------------------
    # TERCERA ETAPA: V2.9.1
    # Solo para títulos que siguen sin sección tras V2.7.2/V2.8.1.
    # V2.9.1 conserva la clasificación previa y corrige el caso
    # validado del motociclista herido tras chocar.
    # ----------------------------------------------------------

    necesita_v291 <-
      is.na(seccion_final) &
      necesita_titulo

    if (any(necesita_v291)) {
      clasificacion_v291 <- rep(
        NA_character_,
        nrow(noticias)
      )

      clasificacion_v291[necesita_v291] <-
        vapply(
          titulos[necesita_v291],
          clasificar_seccion_titulo_v291,
          character(1)
        )

      usar_v291 <- necesita_v291 &
        !is.na(clasificacion_v291)

      seccion_final[usar_v291] <-
        clasificacion_v291[usar_v291]
    }
  }

  # Lo que finalmente no pudo ser clasificado
  # queda explícitamente como "Sin sección".
  seccion_final[
    is.na(seccion_final)
  ] <- "Sin sección"
  
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
