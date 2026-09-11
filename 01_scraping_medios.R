#install.packages("chromote")
library(chromote)

b <- ChromoteSession$new()

b$Page$navigate(
  "https://mapademedios.comunicacionpublica.gba.gob.ar/mapa"
)

b$Page$loadEventFired()

selector <- paste0(
  "body > div.LayoutShell_shell__gj8nR > div > ",
  "div > section.mapa_mapaSection__Yfdwl > ",
  "div.mapa_listaCard__nFRD4"
)

resultado <- b$Runtime$evaluate(
  sprintf(
    "document.querySelector(%s).innerText",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ),
  returnByValue = TRUE
)

resultado$result$value

resultado <- b$Runtime$evaluate(
  sprintf(
    "document.querySelector(%s).children.length",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ),
  returnByValue = TRUE
)

resultado$result$value

js <- sprintf(
  "JSON.stringify(
     [...document.querySelectorAll(%s + ' a')].map(a => ({
       texto: a.innerText,
       url: a.href
     }))
   )",
  jsonlite::toJSON(selector, auto_unbox = TRUE)
)

resultado <- b$Runtime$evaluate(
  js,
  returnByValue = TRUE
)

links <- jsonlite::fromJSON(
  resultado$result$value
)

links

resultado <- b$Runtime$evaluate(
  sprintf(
    "document.querySelector(%s).innerHTML",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ),
  returnByValue = TRUE
)

cat(resultado$result$value)

js <- sprintf(
  "document.querySelector(%s).querySelectorAll('button').length",
  jsonlite::toJSON(selector, auto_unbox = TRUE)
)

resultado <- b$Runtime$evaluate(
  js,
  returnByValue = TRUE
)

resultado$result$value

js <- sprintf(
  "JSON.stringify(
     [...document.querySelector(%s).querySelectorAll('button')]
       .map(x => ({
         texto: x.innerText,
         html: x.outerHTML
       }))
   )",
  jsonlite::toJSON(selector, auto_unbox = TRUE)
)

resultado <- b$Runtime$evaluate(
  js,
  returnByValue = TRUE
)

botones <- jsonlite::fromJSON(resultado$result$value)

botones

js <- sprintf(
  "
  JSON.stringify(
    [...document.querySelector(%s).querySelectorAll('button')]
      .map((x, i) => ({
        id: i,
        texto: x.innerText.trim(),
        aria: x.getAttribute('aria-label'),
        title: x.getAttribute('title'),
        clase: x.className
      }))
  )
  ",
  jsonlite::toJSON(selector, auto_unbox = TRUE)
)

resultado <- b$Runtime$evaluate(
  js,
  returnByValue = TRUE
)

botones <- jsonlite::fromJSON(
  resultado$result$value
)

botones

js <- sprintf(
  "
  document.querySelector(%s)
    .querySelectorAll('button')[0]
    .click()
  ",
  jsonlite::toJSON(selector, auto_unbox = TRUE)
)

b$Runtime$evaluate(
  js,
  returnByValue = TRUE
)
Sys.sleep(1)

resultado <- b$Runtime$evaluate(
  sprintf(
    "document.querySelector(%s).innerText",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ),
  returnByValue = TRUE
)

cat(resultado$result$value)

resultado <- b$Runtime$evaluate(
  sprintf(
    "document.querySelector(%s).innerHTML",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ),
  returnByValue = TRUE
)

cat(resultado$result$value)

botones

js <- "
JSON.stringify(
  [...document.querySelectorAll('*')]
    .filter(x => x.innerText && x.innerText.includes('25 de Mayo') && x.children.length < 5)
    .map(x => ({
      tag: x.tagName,
      clase: x.className,
      texto: x.innerText.substring(0, 1000),
      html: x.outerHTML.substring(0, 3000)
    }))
)
"

resultado <- b$Runtime$evaluate(
  js,
  returnByValue = TRUE
)

elementos <- jsonlite::fromJSON(
  resultado$result$value
)

View(elementos)

js <- "
JSON.stringify(
  [...document.querySelectorAll('div')]
    .map(x => ({
      clase: x.className,
      texto: x.innerText ? x.innerText.trim().substring(0, 500) : ''
    }))
    .filter(x =>
      x.texto.includes('Baires Centro') ||
      x.texto.includes('25 de Mayo')
    )
)
"

resultado <- b$Runtime$evaluate(
  js,
  returnByValue = TRUE
)

divs <- jsonlite::fromJSON(
  resultado$result$value
)

View(divs)

resultado <- b$Runtime$evaluate(
  "performance.getEntriesByType('resource').map(x => x.name)",
  returnByValue = TRUE
)

recursos <- resultado$result$value

recursos

recursos <- unlist(recursos)

recursos[
  grepl(
    "api|json|medio|mapa|media|location|local|data",
    recursos,
    ignore.case = TRUE
  )
]

recursos[
  grepl(
    "api|json|medio|mapa|media|location|local|data",
    recursos,
    ignore.case = TRUE
  )
]

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

url <- paste0(
  "https://mapademedios.comunicacionpublica.gba.gob.ar",
  "/api/medios/publico?pagina=1&resultados=50"
)

respuesta <- request(url) |>
  req_perform()

datos <- resp_body_json(respuesta)

str(datos, max.level = 3)

datos
names(datos)
length(datos)
medios <- datos$medios
names(medios[[1]])

datos$paginas
urls <- paste0(
  "https://mapademedios.comunicacionpublica.gba.gob.ar",
  "/api/medios/publico?pagina=",
  seq_len(datos$paginas),
  "&resultados=50"
)

urls

respuestas <- purrr::map(
  urls,
  \(url) {
    request(url) |>
      req_perform() |>
      resp_body_json()
  }
)

length(respuestas)
purrr::map_int(respuestas, \(x) length(x$medios))
medios_lista <- purrr::map(
  respuestas,
  "medios"
) |>
  purrr::flatten()

length(medios_lista)
medios_df <- tibble::tibble(
  medio = medios_lista
) |>
  tidyr::unnest_wider(medio)

library(dplyr)
library(writexl)

medios_excel <- medios_df |>
  mutate(
    tipo_gestion = as.character(tipo_gestion),
    tipo_formato = as.character(tipo_formato)
  ) |>
  select(-logo_base64)

write_xlsx(
  medios_excel,
  "medios_df.xlsx"
)
