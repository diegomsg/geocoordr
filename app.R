# App de conversão de coordenadas geográficas
# UTM -> graus decimais

# Pacotes -----
suppressWarnings(
  suppressPackageStartupMessages({
    library(shiny) # ferramenta principal
    library(bslib) # fornece tema e framework
    library(shinyWidgets) # habilita widgets
    library(thematic) # carrega tema bslib para plots
    library(tidyverse) # universo tidyverse, especialmente, dplyr, stringr, purrr
    library(concaveman) # algoritmo de mínima área côncava
    library(sf) # geometria espacial
    library(tmap) # visualização em mapas
    library(mapview) # exportar mapa com basemap
    library(DT) # visualização de tabelas
    library(plotKML) # exportar KML customizado
  })
)

# Parâmetros -----
Sys.setlocale(
  category = "LC_COLLATE",
  locale = "en-US"
) # Encoding do texto
options(digits = 15) # Dígitos para números

## Variáveis globais -----
pacotes <- c("shiny", "bslib", "shinyWidgets", "thematic", "tidyverse", "sf", "tmap", "DT")
last_updt <- "08/03/2023"

# Funções ----
citar <- function(pacote) { # citation em nav_item para nav conteiner
  nav_item(
    tagList(
      str_to_title(citation(pacote)[1]$title),
      span(paste0("(", citation(pacote)[1]$year, ")")),
      a(icon("link"), href = citation(pacote)[1]$url)
    )
  )
}

ler_coord <- function(txt,
                      sep_coord = ",",
                      sep_dec = ".",
                      format = "zh_xy") {
  # Valida entradas
  checkmate::assertChoice(sep_coord, c(",", ";", "/", "|", " "))
  checkmate::assertChoice(sep_dec, c(".", ","))
  checkmate::assert_true(sep_dec != sep_coord)
  checkmate::assertChoice(format, c("zh_xy", "xy", "zh_yx", "yx"))

  # Define máscara regex
  padrao_base <- if (str_starts(format, "z")) { # formatos "zh_xy" ou "zh_yx"
    "(\\d{1,2})\\s?([NS])\\s?([1-9]\\d*(?:\\{{sep_dec}}\\d*)?)\\s?[\\{{sep_coord}}\\;]?\\s?([1-9]\\d*(?:\\{{sep_dec}}\\d*)?)"
  } else {
    "([1-9]\\d*(?:\\{{sep_dec}}\\d*)?)\\s?[\\{{sep_coord}}\\;]?\\s?([1-9]\\d*(?:\\{{sep_dec}}\\d*)?)"
  }
  padrao <- padrao_base |>
    str_replace_all(fixed("{{sep_dec}}"), sep_dec) |>
    str_replace_all(fixed("{{sep_coord}}"), sep_coord)
  extracao <- str_squish(txt) |>
    str_to_upper() |>
    str_match_all(padrao) |>
    pluck(1) |>
    as_tibble(.name_repair = "minimal")
  switch(format,
    "zh_xy" = rename(extracao,
      original = 1,
      zona = 2,
      hemisferio = 3,
      x = 4,
      y = 5
    ),
    "xy" = rename(extracao,
      original = 1,
      x = 2,
      y = 3
    ),
    "zh_yx" = rename(extracao,
      original = 1,
      zona = 2,
      hemisferio = 3,
      y = 4,
      x = 5
    ),
    "yx" = rename(extracao,
      original = 1,
      y = 2,
      x = 3
    )
  ) |>
    select(!original) |>
    rownames_to_column(var = "Ponto") |>
    mutate(across(
      c(x, y),
      {
        \(x) str_replace(x, pattern = "\\,", replacement = "\\.")
      }
    )) |>
    type_convert()
}

georreferenciar <- function(base, format = "zh_xy", datum = "SIRGAS 2000", zone = NULL) {
  zone <- if (is_null(zone)) {
    checkmate::assert_names(names(base), must.include = c("zona", "hemisferio"))
    paste0(base$zona[1], base$hemisferio[1])
  } else {
    str_squish(zone) |>
      str_to_upper() |>
      str_replace_all("\\s", "")
  }
  # Valida entradas
  checkmate::assert_names(names(base), must.include = c("x", "y"))
  checkmate::assertChoice(format, c("zh_xy", "xy"))
  checkmate::assertChoice(datum, c("SAD 69", "Córrego Alegre 1961", "Córrego Alegre 1970", "WGS 84", "SIRGAS 2000"))
  if (format == "xy") {
    checkmate::assert_true(str_detect(zone, "\\d{1,2}.*[NnSs]"))
  }
  # Define strings a buscar no pacote wk
  proj_string <- switch(datum,
    "SAD 69" = "SAD69 / UTM zone",
    "Córrego Alegre 1961" = "Corrego Alegre 1961 / UTM zone",
    "Córrego Alegre 1970" = "Corrego Alegre 1970-72 / UTM zone",
    "WGS 84" = "WGS 84 / UTM zone",
    "SIRGAS 2000" = "SIRGAS 2000 / UTM zone"
  ) |>
    paste(zone)
  proj_crs <- wk::wk_proj_crs_view |>
    top_n(n = 1, str_detect(name, proj_string) &  deprecated == 0) |>
    select(code) |>
    pluck(1) %>%
    paste0("EPSG:", .)
  st_as_sf(
    x = unique(base),
    coords = c("x", "y"),
    crs = st_crs(proj_crs),
    dim = "XY"
  ) %>%
    mutate(
      X = st_coordinates(.)[, "X"],
      Y = st_coordinates(.)[, "Y"]
    )
}

# UI -----
## Temas -----
light <- bs_theme("flatly", version = 5) # tema claro
dark <- bs_theme("darkly", version = 5) # tema escuro

## Sidebar elements -----
input_coord_utm <- textAreaInput(
  "coordenadas_utm",
  "Coordenadas UTM",
  placeholder = "24S 713110.66, 8793023.16\n24S 713110,66; 8793023,16\n713110.66, 8793023.16\n713110,66; 8793023,16",
  rows = 6,
  resize = "vertical"
)
input_datum <- pickerInput(
  "datum",
  label = "Datum",
  choices = c("SAD 69", "Córrego Alegre 1961", "Córrego Alegre 1970", "WGS 84", "SIRGAS 2000"),
  selected = "SIRGAS 2000"
)
input_zona <- pickerInput(
  "zone",
  label = "Zona UTM",
  choices = list(
    "Nos dados" = c(""),
    Sul = outer(c(17:26), c("S"), FUN = str_c),
    Norte = outer(c(11:24), c("N"), FUN = str_c)
  ),
  selected = ""
)
input_format <- pickerInput(
  "formato",
  label = "Formato",
  choices = c(
    "zona X,Y" = "zh_xy",
    "X,Y" = "xy",
    "zona Y, X" = "zh_yx",
    "Y, X" = "yx"
  ),
  selected = NULL
)
input_sep_dec <- radioGroupButtons(
  "sep_dec",
  label = "Decimal",
  choices = c(".", ","),
  selected = "."
)
input_sep_coord <- radioGroupButtons(
  "sep_coord",
  label = "Coordenadas",
  choices = c(",", ";", "/", "|", " "),
  selected = ","
)
input_se_poligono <- switchInput(
  "poligono",
  label = "Fechar polígono",
  value = FALSE,
  onLabel = "Sim",
  offLabel = "Não"
)
bot_converter <- actionButton(
  "bot_converter",
  label = "Converter",
  icon = icon("cogs"),
  style = "fill"
)

## Páginas -----
pg_converter_result <-
  navs_tab_card(
    nav("Tabela",
      icon = icon("table"),
      card(
        DTOutput("resultado_tab"), 
        class = "min_height",
        full_screen = TRUE
      )
    ),
    nav("Mapa",
      icon = icon("map"),
      card(
        tmapOutput("resultado_map"),
        class = "min_height",
        full_screen = TRUE
      ),
      fluidRow(
        column(width = 3, offset = 0,
               downloadButton("download_png", label = "PNG")),
        column(width = 3, offset = 0,
               downloadButton("download_png_basemap", label = "Visualização")),
        column(width = 2, offset = 1,
               downloadButton("download_gpkg", label = "GKPG")),
        column(width = 2, offset = 1,
               downloadButton("download_kml", label = "KML"))
      )
    )
  )
pg_converter <-
  sidebarLayout(
    position = "left",
    fluid = TRUE,
    sidebarPanel(
      tags$h4("Converter"),
      input_datum,
      input_zona,
      input_format,
      input_coord_utm,
      card(
        card_header("Separador"),
        card_body(
          layout_column_wrap(width = "150px",
                            fixed_width = FALSE,
                            input_sep_dec,
                            input_sep_coord
         )
       )
      ),
      input_se_poligono,
      bot_converter
    ),
    mainPanel(pg_converter_result)
  )
pg_info <- navs_tab_card(
  nav(
    "Informações Técnicas",
    tags$ul(
      tags$li(
        "UTM: Universal Transverse Mercator é um sistema de coordenadas cartográficas que utiliza a projeção Mercator. É um sistema de coordenadas bidimensional que permite representar qualquer ponto na superfície terrestre através de um par de coordenadas numéricas: a distância em metros a partir de um ponto de origem e a distância em metros a partir do equador."
      ),
      tags$li(
        "Coordenadas geográficas: As coordenadas geográficas são utilizadas para representar posições na superfície terrestre em relação à linha do equador e ao meridiano de Greenwich. Elas são compostas por dois valores numéricos: a latitude e a longitude."
      ),
      tags$li(
        "Datum de referência: Um datum é um modelo matemático que descreve a forma e o tamanho da Terra. Ele é utilizado como referência para o cálculo das coordenadas geográficas ou UTM."
      ),
      tags$li(
        "Zonas UTM: A superfície terrestre é dividida em 60 zonas UTM, cada uma com 6 graus de longitude de largura. Cada zona é numerada de 1 a 60 e tem um ponto de origem específico."
      )
    )
  ),
  nav(
    "Uso dos Dados",
    p(
      "Os dados de saída representam as coordenadas em graus decimais e podem ser utilizados para visualização em mapas, cálculo de distâncias, cruzamento de informações com outras bases de dados, entre outras funções de geoprocessamento."
    )
  ),
  nav(
    "Vantagens",
    tags$ul(
      tags$li(
        "Precisão: O formato decimal permite que as coordenadas sejam expressas com maior precisão do que em outros formatos, como o grau-minuto-segundo."
      ),
      tags$li(
        "Facilidade de uso: As coordenadas decimais são fáceis de usar e interpretar, especialmente em sistemas de GPS e mapas digitais."
      ),
      tags$li(
        "Consistência global: As coordenadas geográficas em formato decimal são amplamente utilizadas em todo o mundo, garantindo consistência e padronização na localização de pontos geográficos."
      ),
      tags$li(
        "Compatibilidade: As coordenadas em formato decimal são facilmente convertidas em outros sistemas de coordenadas geográficas, como o UTM (Universal Transverse Mercator), que é amplamente utilizado em aplicações de cartografia."
      ),
      tags$li(
        "Flexibilidade: As coordenadas em formato decimal permitem que a localização de um ponto seja expressa com precisão, independentemente de sua posição na Terra, desde que sejam utilizadas as unidades de medida adequadas."
      )
    )
  )
)

pg_code <- navs_tab_card(
  nav("Ferramenta",
    icon = icon("terminal"),
    citar("base")
  ),
  nav("Pacotes utilizados",
    icon = icon("puzzle-piece"),
    map(pacotes, citar)
  )
  # nav("Código fonte", icon = icon("code"), conteudo)
)

## Define UI web para coleta de inputs e exibição de ouputs
## UI Page layout -----
ui <- page_navbar(
  header = tags$head(
              tags$style(
                HTML(".min_height{
                                  min-height: 60px;
                                 }")
              )
            ),
  title = "geocoordR",
  theme = light,
  lang = "pt-br",
  nav("Converter",
    icon = icon("map-location-dot"),
    pg_converter
  ),
  nav("Informações",
    icon = icon("globe"),
    pg_info
  ),
  nav_spacer(),
  nav_menu(
    "Mais",
    icon = icon("info"),
    align = "right",
    nav_item(a(icon("github"),
      href = "https://github.com/diegomsg/geocoordr"
    )),
    nav("Código fonte",
      icon = icon("code"),
      pg_code
    )
  ),
  nav_item(
    switchInput(
      inputId = "dark_mode",
      label = icon("circle-half-stroke"),
      value = FALSE,
      size = "small"
    )
  )
)

# Server -----
#devmode(TRUE)
server <- function(input, output, session) {
  
  ## Tema ----
  thematic_shiny(font = "auto") # Aplica tema do shiny aos plots
  observe(
    session$setCurrentTheme(if (isTRUE(input$dark_mode)) {
      dark
    } else {
      light
    })
  )
  
  ## Reatividade ----
  observeEvent(input$sep_dec, {
    if (input$sep_dec == ",") {
      updateRadioGroupButtons(session = getDefaultReactiveDomain(),
                              inputId = "sep_coord",
                              selected = ";")
    }
  }, ignoreInit = TRUE)
  
  ## Validação ----
  validar <- function() {
    validate(need(expr = input$coordenadas_utm != "", 
                  message = "Informe as coordenadas."))
    validate(need(expr = (str_starts(input$formato, "zh") & !str_detect(input$zone, pattern = "^[0-9]")) ||
                         (!str_starts(input$formato, "zh") & str_detect(input$zone, pattern = "^[0-9]")),
                  message = "Selecione a zona UTM ou selecione formato com a zona junto às coordenadas."))
  }
  output$resultado_tab <- renderDT(validar())
  output$resultado_map <- renderTmap(validar())
  
  ## Conversão ----
  observeEvent(input$bot_converter, {
    
    ### Inputs ----
    entrada <- ler_coord(
      txt = input$coordenadas_utm,
      sep_coord = input$sep_coord,
      sep_dec = input$sep_dec,
      format = input$formato
    )
    entrada_georref <- georreferenciar(
      base = entrada,
      format = switch(input$formato,
        "zh_yx" = "zh_xy",
        "yx" = "xy",
        NULL = "zh_xy",
        input$formato
      ),
      datum = input$datum,
      zone = input$zone
    )
    if (input$poligono) {
      entrada_poligono <- summarise(entrada_georref) |>
        concaveman(concavity = 1) %>%
        mutate(
          Área = st_area(.),
          Perímetro = st_boundary(.) |> st_length()
        )
    }
    coords_WGS <- st_transform(entrada_georref,
      crs = 4326
    ) |>
      st_coordinates()
    entrada_georref <- select(
      entrada_georref,
      c(Ponto, X, Y)
    ) |>
      mutate(
        X0 = X,
        Y0 = Y,
        X = coords_WGS[, 1],
        Y = coords_WGS[, 2]
      )
    map <- reactive({
      if (input$poligono) {
        tm_basemap(c(
          OpenStreetMap = "OpenStreetMap",
          Topográfico = "Esri.WorldTopoMap",
          Satélite = "Esri.WorldImagery"
        )) +
        tm_graticules(labels.cardinal = TRUE,
                      labels.show = TRUE,
                      col = "darkgrey") +
        tm_xlab("Longitude") +
        tm_ylab("Latitude") +
        tm_mouse_coordinates() +
        tm_shape(entrada_poligono,
                 name = "Polígono"
        ) +
        tm_polygons(
          id = "Polígono",
          col = "orangered",
          alpha = 0.3,
          border.col = "firebrick",
          popup.format = list(digits = 4, 
                              decimal.mark = ",", 
                              big.mark = ".")
        ) +
        tm_shape(entrada_georref,
                 name = "Pontos"
        ) +
        tm_dots(
          col = "darkorange1",
          size = 0.1,
          popup.format = list(digits = 6, 
                              decimal.mark = ".", 
                              big.mark = "")
        ) +
        tm_compass(size = 1,
                   position = c("right", "top")) +
        tm_scale_bar() +
        tm_credits(text = "geocoordR")
      } else {
        tm_basemap(c(
          OpenStreetMap = "OpenStreetMap",
          Topográfico = "Esri.WorldTopoMap",
          Satélite = "Esri.WorldImagery"
        )) +
        tm_graticules(labels.cardinal = TRUE,
                      labels.show = TRUE,
                      col = "darkgrey") +
        tm_xlab("Longitude") +
        tm_ylab("Latitude") +
        tm_mouse_coordinates() +
        tm_shape(entrada_georref,
                 name = "Pontos"
        ) +
        tm_dots(
          col = "darkorange1",
          size = 0.1,
          popup.format = list(digits = 4, 
                              decimal.mark = ",", 
                              big.mark = ".")
        ) +
        tm_compass(size = 1, 
                   position = c("right", "top")) +
        tm_scale_bar() +
        tm_credits(text = "geocoordR")
      }
    })
    map_lf <- tmap_leaflet(map())
    geo <- reactive({
      if (input$poligono) {
        entrada_poligono
      } else {
        entrada_georref
      }
    })
    
    ### Outputs ----
    
    #### Tabelas ----
    output$resultado_tab <- renderDT({
      validate(
        need(input$coordenadas_utm != "", "Informe as coordenadas.")
      )
      entrada_georref |>
        st_drop_geometry() |>
        select(c(Ponto, X0, Y0, X, Y))
      },
      extensions = 'Buttons', 
      options = list(
        searching = FALSE,
        dom = 'Bifrtlp',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
    
    #### Mapa ----
    tmap_mode("plot")
    output$resultado_map <- renderTmap({
      map()
    })
    
    #### Arquivos ----
    output$download_png_basemap <- downloadHandler(
      filename = paste0("geocoordR-", 
                        Sys.time() |> str_sub(end = 19) |> str_replace_all(":",""), 
                        ".png"),
      content = function(file) {
        mapshot(x = map_lf,
                vwidth = 1920, vheight = 1080,
                file = file)
      }
    )
    output$download_png <- downloadHandler(
      filename = paste0("geocoordR-",
                        Sys.time() |> str_sub(end = 19) |> str_replace_all(":",""),
                        ".png"),
      content = function(file) {
                                tmap_mode("view")
                                tmap_save(tm = map(),
                                         #  width = 1920, height = 1080,
                                          filename = file)
                                }
    )
    output$download_kml <- downloadHandler(
      filename = paste0("geocoordR-", 
                        Sys.time() |> str_sub(end = 19) |> str_replace_all(":",""), 
                        ".kml"),
      content = function(file) {
        if (input$poligono) {
                            kml(
                                obj = as_Spatial(st_zm(geo())),
                                labels = "geocoordR",
                                colour = "red",
                                alpha = 0.75,
                                file = file
                            )
                  } else {
                            kml(
                              obj = as_Spatial(geo()),
                              size = 1.5,
                              colour = "red",
                              alpha = 1,
                              file = file
                            )
                          }
      }
    )
    output$download_gpkg <- downloadHandler(
      filename = paste0("geocoordR-", 
                        Sys.time() |> str_sub(end = 19) |> str_replace_all(":",""), 
                        ".gpkg"),
      content = function(file) {
        st_write(obj = geo(),
                 dsn = file)
      }
    )
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
