# ============================================================
# RADAR BONAERENSE
# Aplicación Shiny de monitoreo de medios
# ============================================================

library(shiny)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(htmltools)

# ============================================================
# CONFIGURACIÓN
# ============================================================

ARCHIVO_VISTAS <- "vistas_noticias.rds"

# Actualización automática cada 15 minutos
INTERVALO_ACTUALIZACION <- 15 * 60 * 1000


# ============================================================
# CARGA INICIAL
# ============================================================

if (!file.exists(ARCHIVO_VISTAS)) {
  stop(
    "No se encontró el archivo 'vistas_noticias.rds'. ",
    "Ejecutá primero 05_generar_vistas.R."
  )
}

vistas_iniciales <- readRDS(ARCHIVO_VISTAS)


# ============================================================
# LOGO
# ============================================================

addResourcePath(
  "logo",
  normalizePath("www")
)

.logo_radar <- tags$img(
  src = "logo/logo_radar_bonaerense_horizontal.png",
  class = "radar-logo",
  alt = "Radar Bonaerense"
)


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # CSS
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f5f6f8;
        font-family: -apple-system, BlinkMacSystemFont,
                     'Segoe UI', Roboto, Arial, sans-serif;
        color: #202124;
      }

      .container-fluid {
        max-width: 1500px;
        margin: 0 auto;
        padding-left: 25px;
        padding-right: 25px;
      }


      /* =====================================================
         ENCABEZADO
         ===================================================== */

      .radar-header {
        background: white;
        border-bottom: 1px solid #e2e5e8;
        padding: 18px 0;
        margin-bottom: 20px;
      }

      .radar-identidad {
        display: flex;
        align-items: center;
        gap: 22px;
      }

      .radar-logo {
        height: 72px;
        width: auto;
        display: block;
      }

      .radar-subtitulo {
        font-size: 18px;
        color: #5f6368;
        line-height: 1.35;
        font-weight: 400;
      }

      .radar-actualizacion {
        margin-left: auto;
        text-align: right;
        color: #6b7280;
        font-size: 13px;
        line-height: 1.4;
      }

      .radar-actualizacion strong {
        color: #374151;
        font-weight: 600;
      }


      /* =====================================================
         FILTROS
         ===================================================== */

      .panel-filtros {
        background: white;
        border: 1px solid #e1e5e9;
        border-radius: 10px;
        padding: 18px 20px;
        margin-bottom: 20px;
      }

      .panel-filtros .form-group {
        margin-bottom: 10px;
      }

      .panel-filtros label {
        font-size: 12px;
        font-weight: 600;
        color: #5f6368;
        margin-bottom: 5px;
      }

      .form-control {
        border-radius: 7px;
        border: 1px solid #d5d9dd;
        box-shadow: none;
        height: 38px;
      }

      .form-control:focus {
        border-color: #7a8ca5;
        box-shadow: 0 0 0 2px rgba(80, 100, 130, 0.08);
      }


      /* =====================================================
         PESTAÑAS
         ===================================================== */

      .nav-tabs {
        border-bottom: 1px solid #dfe3e7;
        margin-bottom: 20px;
      }

      .nav-tabs > li > a {
        color: #5f6368;
        font-weight: 500;
        border: none;
        padding: 10px 18px;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #202124;
        background: transparent;
        border: none;
        border-bottom: 3px solid #3f6f9f;
      }


      /* =====================================================
         MÉTRICAS
         ===================================================== */

      .metricas {
        display: flex;
        gap: 15px;
        margin-bottom: 20px;
        flex-wrap: wrap;
      }

      .metrica {
        background: white;
        border: 1px solid #e1e5e9;
        border-radius: 10px;
        padding: 15px 20px;
        min-width: 170px;
        flex: 1;
      }

      .metrica-label {
        font-size: 12px;
        color: #6b7280;
        margin-bottom: 5px;
      }

      .metrica-valor {
        font-size: 25px;
        font-weight: 650;
        color: #202124;
      }


      /* =====================================================
         TARJETAS DE NOTICIAS
         ===================================================== */

      .noticia {
        background: white;
        border: 1px solid #e1e5e9;
        border-radius: 10px;
        padding: 17px 20px;
        margin-bottom: 12px;
        transition: box-shadow 0.15s ease,
                    border-color 0.15s ease;
      }

      .noticia:hover {
        border-color: #cbd2d9;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      }

      .noticia-meta {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-wrap: wrap;
        margin-bottom: 7px;
      }

      .noticia-medio {
        font-size: 12px;
        font-weight: 650;
        color: #3f6f9f;
      }

      .noticia-separador {
        color: #c2c7cc;
      }

      .noticia-fecha {
        font-size: 12px;
        color: #7a8087;
      }

      .noticia-titulo {
        font-size: 17px;
        line-height: 1.4;
        font-weight: 600;
        margin-bottom: 7px;
      }

      .noticia-titulo a {
        color: #202124;
        text-decoration: none;
      }

      .noticia-titulo a:hover {
        color: #3f6f9f;
      }

      .noticia-descripcion {
        font-size: 13px;
        line-height: 1.5;
        color: #646b73;
      }

      .noticia-ubicacion {
        margin-top: 9px;
        font-size: 11px;
        color: #858b92;
      }


      /* =====================================================
         MENSAJES
         ===================================================== */

      .sin-resultados {
        background: white;
        border: 1px solid #e1e5e9;
        border-radius: 10px;
        padding: 35px;
        text-align: center;
        color: #6b7280;
      }


      /* =====================================================
         RESPONSIVE
         ===================================================== */

      @media (max-width: 768px) {

        .container-fluid {
          padding-left: 15px;
          padding-right: 15px;
        }

        .radar-header {
          padding: 14px 0;
        }

        .radar-identidad {
          gap: 12px;
          align-items: flex-start;
          flex-direction: column;
        }

        .radar-logo {
          height: 58px;
        }

        .radar-subtitulo {
          font-size: 15px;
        }

        .radar-actualizacion {
          margin-left: 0;
          text-align: left;
          margin-top: 5px;
        }

        .metricas {
          flex-direction: column;
        }

        .metrica {
          min-width: 100%;
        }

      }

    "))
  ),
  
  
  # ==========================================================
  # ENCABEZADO
  # ==========================================================
  
  div(
    class = "radar-header",
    
    div(
      class = "container-fluid",
      
      div(
        class = "radar-identidad",
        
        .logo_radar,
        
        div(
          class = "radar-subtitulo",
          "Detección y seguimiento de noticias publicadas por medios bonaerenses"
        ),
        
        div(
          class = "radar-actualizacion",
          
          tags$div(
            tags$strong("Última actualización")
          ),
          
          textOutput(
            "ultima_actualizacion",
            inline = TRUE
          )
        )
        
      )
    )
  ),
  
  
  # ==========================================================
  # CONTENIDO PRINCIPAL
  # ==========================================================
  
  div(
    class = "container-fluid",
    
    # --------------------------------------------------------
    # FILTROS
    # --------------------------------------------------------
    
    div(
      class = "panel-filtros",
      
      fluidRow(
        
        column(
          width = 3,
          
          selectInput(
            "seccion",
            "Sección",
            choices = NULL,
            selected = NULL
          )
        ),
        
        column(
          width = 3,
          
          selectInput(
            "partido",
            "Partido",
            choices = NULL,
            selected = NULL
          )
        ),
        
        column(
          width = 3,
          
          selectInput(
            "localidad",
            "Localidad",
            choices = NULL,
            selected = NULL
          )
        ),
        
        column(
          width = 3,
          
          selectInput(
            "medio",
            "Medio",
            choices = NULL,
            selected = NULL
          )
        )
        
      ),
      
      fluidRow(
        
        column(
          width = 12,
          
          textInput(
            "busqueda",
            "Buscar",
            placeholder = "Buscar en títulos y noticias..."
          )
          
        )
        
      )
      
    ),
    
    
    # --------------------------------------------------------
    # PESTAÑAS
    # --------------------------------------------------------
    
    tabsetPanel(
      
      id = "periodo",
      
      tabPanel(
        "Ahora",
        value = "ahora",
        
        uiOutput("metricas_ahora"),
        uiOutput("noticias_ahora")
      ),
      
      tabPanel(
        "Últimas 24 horas",
        value = "ultimas_24h",
        
        uiOutput("metricas_24h"),
        uiOutput("noticias_24h")
      ),
      
      tabPanel(
        "Últimos 7 días",
        value = "ultimos_7_dias",
        
        uiOutput("metricas_7d"),
        uiOutput("noticias_7d")
      )
      
    )
    
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # DATOS REACTIVOS
  # ==========================================================
  
  datos <- reactiveVal(vistas_iniciales)
  
  
  # ==========================================================
  # ACTUALIZACIÓN AUTOMÁTICA
  # ==========================================================
  
  observe({
    
    invalidateLater(
      INTERVALO_ACTUALIZACION,
      session
    )
    
    if (!file.exists(ARCHIVO_VISTAS)) {
      return()
    }
    
    vistas_nuevas <- tryCatch(
      readRDS(ARCHIVO_VISTAS),
      error = function(e) {
        NULL
      }
    )
    
    if (is.null(vistas_nuevas)) {
      return()
    }
    
    datos(vistas_nuevas)
    
  })
  
  
  # ==========================================================
  # ÚLTIMA ACTUALIZACIÓN
  # ==========================================================
  
  output$ultima_actualizacion <- renderText({
    
    vistas <- datos()
    
    if (
      is.null(vistas$generado) ||
      length(vistas$generado) == 0 ||
      is.na(vistas$generado)
    ) {
      return("")
    }
    
    format(
      vistas$generado,
      "%d/%m/%Y %H:%M"
    )
    
  })
  
  
  # ==========================================================
  # DATOS SEGÚN VISTA
  # ==========================================================
  
  datos_vista <- reactive({
    
    vistas <- datos()
    
    req(input$periodo)
    
    switch(
      input$periodo,
      
      ahora = vistas$ahora,
      
      ultimas_24h = vistas$ultimas_24h,
      
      ultimos_7_dias = vistas$ultimos_7_dias,
      
      vistas$ahora
    )
    
  })
  
  
  # ==========================================================
  # ACTUALIZAR FILTROS
  # ==========================================================
  
  observeEvent(
    datos(),
    {
      
      vistas <- datos()
      
      filtros <- vistas$filtros
      
      if (is.null(filtros)) {
        return()
      }
      
      updateSelectInput(
        session,
        "seccion",
        choices = c(
          "Todas" = "",
          filtros$seccion
        ),
        selected = ""
      )
      
      updateSelectInput(
        session,
        "partido",
        choices = c(
          "Todos" = "",
          filtros$partido
        ),
        selected = ""
      )
      
      updateSelectInput(
        session,
        "localidad",
        choices = c(
          "Todas" = "",
          filtros$localidad
        ),
        selected = ""
      )
      
      updateSelectInput(
        session,
        "medio",
        choices = c(
          "Todos" = "",
          filtros$medio
        ),
        selected = ""
      )
      
    },
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # FILTRO DE NOTICIAS
  # ==========================================================
  
  noticias_filtradas <- reactive({
    
    datos <- datos_vista()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(datos)
    }
    
    
    # --------------------------------------------------------
    # Sección
    # --------------------------------------------------------
    
    if (
      !is.null(input$seccion) &&
      input$seccion != ""
    ) {
      
      datos <- datos |>
        filter(
          seccion == input$seccion
        )
      
    }
    
    
    # --------------------------------------------------------
    # Partido
    # --------------------------------------------------------
    
    if (
      !is.null(input$partido) &&
      input$partido != ""
    ) {
      
      datos <- datos |>
        filter(
          partido == input$partido
        )
      
    }
    
    
    # --------------------------------------------------------
    # Localidad
    # --------------------------------------------------------
    
    if (
      !is.null(input$localidad) &&
      input$localidad != ""
    ) {
      
      datos <- datos |>
        filter(
          localidad == input$localidad
        )
      
    }
    
    
    # --------------------------------------------------------
    # Medio
    # --------------------------------------------------------
    
    if (
      !is.null(input$medio) &&
      input$medio != ""
    ) {
      
      datos <- datos |>
        filter(
          medio == input$medio
        )
      
    }
    
    
    # --------------------------------------------------------
    # Búsqueda
    # --------------------------------------------------------
    
    if (
      !is.null(input$busqueda) &&
      nzchar(trimws(input$busqueda))
    ) {
      
      patron <- trimws(input$busqueda)
      
      datos <- datos |>
        filter(
          str_detect(
            coalesce(texto_busqueda, ""),
            regex(
              patron,
              ignore_case = TRUE
            )
          )
        )
      
    }
    
    
    datos
    
  })
  
  
  # ==========================================================
  # FUNCIÓN PARA CREAR MÉTRICAS
  # ==========================================================
  
  crear_metricas <- function(datos) {
    
    if (
      is.null(datos) ||
      nrow(datos) == 0
    ) {
      
      return(
        div(
          class = "metricas",
          
          div(
            class = "metrica",
            
            div(
              class = "metrica-label",
              "Noticias"
            ),
            
            div(
              class = "metrica-valor",
              "0"
            )
            
          )
          
        )
      )
      
    }
    
    
    div(
      class = "metricas",
      
      div(
        class = "metrica",
        
        div(
          class = "metrica-label",
          "Noticias"
        ),
        
        div(
          class = "metrica-valor",
          format(
            nrow(datos),
            big.mark = "."
          )
        )
        
      ),
      
      div(
        class = "metrica",
        
        div(
          class = "metrica-label",
          "Medios"
        ),
        
        div(
          class = "metrica-valor",
          format(
            n_distinct(datos$medio),
            big.mark = "."
          )
        )
        
      ),
      
      div(
        class = "metrica",
        
        div(
          class = "metrica-label",
          "Partidos"
        ),
        
        div(
          class = "metrica-valor",
          format(
            n_distinct(datos$partido),
            big.mark = "."
          )
        )
        
      )
      
    )
    
  }
  
  
  # ==========================================================
  # MÉTRICAS
  # ==========================================================
  
  output$metricas_ahora <- renderUI({
    
    crear_metricas(
      noticias_filtradas()
    )
    
  })
  
  
  output$metricas_24h <- renderUI({
    
    crear_metricas(
      noticias_filtradas()
    )
    
  })
  
  
  output$metricas_7d <- renderUI({
    
    crear_metricas(
      noticias_filtradas()
    )
    
  })
  
  
  # ==========================================================
  # TARJETAS DE NOTICIAS
  # ==========================================================
  
  crear_noticias <- function(datos) {
    
    if (
      is.null(datos) ||
      nrow(datos) == 0
    ) {
      
      return(
        div(
          class = "sin-resultados",
          "No se encontraron noticias para los filtros seleccionados."
        )
      )
      
    }
    
    
    map(
      seq_len(nrow(datos)),
      function(i) {
        
        noticia <- datos[i, ]
        
        
        # ----------------------------------------------------
        # Descripción
        # ----------------------------------------------------
        
        descripcion <- noticia$descripcion
        
        if (
          is.null(descripcion) ||
          length(descripcion) == 0 ||
          is.na(descripcion) ||
          !nzchar(trimws(descripcion))
        ) {
          
          descripcion <- NULL
          
        }
        
        
        # ----------------------------------------------------
        # Ubicación
        # ----------------------------------------------------
        
        ubicacion <- c(
          if (
            !is.null(noticia$partido) &&
            !is.na(noticia$partido) &&
            nzchar(noticia$partido)
          ) {
            noticia$partido
          },
          
          if (
            !is.null(noticia$localidad) &&
            !is.na(noticia$localidad) &&
            nzchar(noticia$localidad)
          ) {
            noticia$localidad
          }
        )
        
        ubicacion <- paste(
          ubicacion,
          collapse = " · "
        )
        
        
        # ----------------------------------------------------
        # Fecha
        # ----------------------------------------------------
        
        fecha_texto <- ""
        
        if (
          "fecha_relativa" %in% names(noticia) &&
          !is.null(noticia$fecha_relativa) &&
          !is.na(noticia$fecha_relativa)
        ) {
          
          fecha_texto <- noticia$fecha_relativa
          
        } else if (
          "fecha" %in% names(noticia) &&
          !is.null(noticia$fecha) &&
          !is.na(noticia$fecha)
        ) {
          
          fecha_texto <- format(
            noticia$fecha,
            "%d/%m/%Y %H:%M"
          )
          
        }
        
        
        # ----------------------------------------------------
        # Tarjeta
        # ----------------------------------------------------
        
        div(
          class = "noticia",
          
          div(
            class = "noticia-meta",
            
            span(
              class = "noticia-medio",
              noticia$medio
            ),
            
            span(
              class = "noticia-separador",
              "·"
            ),
            
            span(
              class = "noticia-fecha",
              fecha_texto
            )
            
          ),
          
          div(
            class = "noticia-titulo",
            
            tags$a(
              href = noticia$url,
              target = "_blank",
              rel = "noopener noreferrer",
              noticia$titulo
            )
            
          ),
          
          if (!is.null(descripcion)) {
            
            div(
              class = "noticia-descripcion",
              descripcion
            )
            
          },
          
          if (
            nzchar(ubicacion)
          ) {
            
            div(
              class = "noticia-ubicacion",
              ubicacion
            )
            
          }
          
        )
        
      }
      
    ) |>
      tagList()
    
  }
  
  
  # ==========================================================
  # NOTICIAS
  # ==========================================================
  
  output$noticias_ahora <- renderUI({
    
    crear_noticias(
      noticias_filtradas()
    )
    
  })
  
  
  output$noticias_24h <- renderUI({
    
    crear_noticias(
      noticias_filtradas()
    )
    
  })
  
  
  output$noticias_7d <- renderUI({
    
    crear_noticias(
      noticias_filtradas()
    )
    
  })
  
}


# ============================================================
# EJECUTAR APLICACIÓN
# ============================================================

shinyApp(
  ui = ui,
  server = server
)