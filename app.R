# =============================================================================
# geocoordR — app Shiny
#
# Converte e georreferencia vértices de memoriais descritivos:
#   UTM (E, N, zona, hemisfério) ou graus (decimal/GMS), em qualquer datum
#   brasileiro, para SIRGAS 2000 — com tabela, mapa, área, perímetro,
#   lados e exportação para outras ferramentas.
#
# Estrutura:
#   R/000-utils.R         utilidades e construtores de problemas
#   R/crs.R               elipsóides, data, EPSG, zonas
#   R/geo-math.R          projeção TM/UTM, ECEF e Helmert (R puro)
#   R/parse-numbers.R     números brasileiros, ângulos, GMS
#   R/parse-coordinates.R leitura de memoriais (linha por linha)
#   R/geometry.R          área, perímetro, lados, autointerseção
#   R/validate.R          validações com mensagens descritivas
#   R/convert.R           pipeline completo (gc_convert)
#   R/export.R            CSV, GeoJSON, KML, GeoPackage/Shapefile
#   R/info.R              textos de ajuda e fontes técnicas
#   R/ui-helpers.R        tema e componentes de interface
#   R/mod_*.R             módulos Shiny
#
# Testes: Rscript tests/testthat.R
# =============================================================================

# ---------------------------------------------------------------- carregador -
gc_source_dir <- function(dir = "R") {
  if (!dir.exists(dir)) {
    stop("Diretório '", dir, "' não encontrado. Rode o app a partir da raiz do projeto.",
         call. = FALSE)
  }
  arquivos <- sort(list.files(dir, pattern = "[.]R$", full.names = TRUE))
  for (f in arquivos) {
    source(f, encoding = "UTF-8", local = FALSE)
  }
  invisible(arquivos)
}

# Pacotes usados pela interface (os módulos usam shiny:: explicitamente,
# mas os componentes visuais e os update* ficam mais legíveis com o pacote
# carregado).
suppressPackageStartupMessages({
  library(shiny)
})

gc_source_dir("R")

options(
  digits = 15,
  encoding = "UTF-8",
  scipen = 999
)

# aquece o compilador JIT: a primeira conversão do usuário sai instantânea
invisible(try(
  gc_convert(gc_examples$utm, type = "utm", polygon = FALSE),
  silent = TRUE
))

# ---------------------------------------------------------------------- UI ---
ui <- bslib::page_navbar(
  title = htmltools::tagList(shiny::icon("map-location-dot"), " geocoordR"),
  lang = "pt-br",
  theme = gc_theme(),
  header = htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$title("geocoordR — coordenadas de memoriais descritivos"),
      htmltools::tags$meta(
        name = "description",
        content = paste("Conversão de coordenadas UTM e geográficas (graus decimais e GMS)",
                        "para SIRGAS 2000, com mapa, área e exportação.")
      )
    ),
    gc_css()
  ),
  bslib::nav_panel(
    "Converter",
    value = "converter",
    icon = shiny::icon("location-crosshairs"),
    htmltools::tags$div(
      class = "container-fluid py-3",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        mod_input_ui("input"),
        mod_results_ui("resultados")
      )
    )
  ),
  bslib::nav_panel(
    "Ajuda",
    icon = shiny::icon("circle-question"),
    pg_ajuda()
  ),
  bslib::nav_panel(
    "Sobre",
    icon = shiny::icon("circle-info"),
    pg_sobre()
  ),
  bslib::nav_spacer(),
  bslib::nav_item(
    htmltools::tags$a(
      href = "https://github.com/diegomsg/geocoordr",
      target = "_blank",
      rel = "noopener noreferrer",
      class = "nav-link",
      title = "Código fonte no GitHub",
      shiny::icon("github")
    )
  ),
  bslib::nav_item(
    bslib::input_dark_mode(id = "modo", mode = "light")
  )
)

# ------------------------------------------------------------------- server --
server <- function(input, output, session) {
  entrada <- mod_input_server("input")

  # contador que o botão "Converter agora" incrementa: força recalcular mesmo
  # quando os campos não mudaram (útil depois de corrigir uma configuração)
  recarregar <- shiny::reactiveVal(0)
  shiny::observeEvent(entrada$converter(), {
    recarregar(recarregar() + 1)
  }, ignoreInit = TRUE)

  resultado <- shiny::reactive({
    recarregar()
    cfg <- entrada$settings()
    texto <- cfg$text %||% ""
    if (!nzchar(gc_squish(texto))) {
      return(NULL)
    }
    gc_convert(
      text = texto,
      type = cfg$type,
      datum = cfg$datum,
      zone = cfg$zone,
      hemisphere = cfg$hemisphere,
      dec_sep = cfg$dec_sep,
      order = cfg$order,
      output_zone = "auto",
      target_datum = "SIRGAS2000",
      polygon = cfg$polygon
    )
  })

  mod_results_server("resultados", resultado)
}

shiny::shinyApp(ui = ui, server = server)
