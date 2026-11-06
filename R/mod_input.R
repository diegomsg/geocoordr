# =============================================================================
# geocoordR :: módulo de entrada de coordenadas
# =============================================================================

#' UI do painel de entrada
mod_input_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    class = "gc-input-card sticky-sidebar",
    bslib::card_header(
      htmltools::tags$div(
        class = "gc-card-title",
        shiny::icon("map-location-dot"),
        "1. Informe as coordenadas",
        info_badge("memoria")
      )
    ),
    bslib::card_body(
      gc_field(
        "O que você tem?",
        shinyWidgets::radioGroupButtons(
          inputId = ns("tipo"),
          label = NULL,
          choiceNames = list(
            htmltools::tagList(shiny::icon("ruler-combined"), " UTM (metros)"),
            htmltools::tagList(shiny::icon("globe"), " Graus (decimal ou GMS)"),
            htmltools::tagList(shiny::icon("wand-magic-sparkles"), " Não sei")
          ),
          choiceValues = c("utm", "geographic", "auto"),
          selected = "utm",
          justified = TRUE,
          size = "sm",
          status = "outline-secondary"
        ),
        info = "tipo_utm",
        help = "UTM: dois números grandes em metros (ex.: 713110.66 e 8793023.16). Graus: valores até 180, com °, vírgula decimal ou letras N/S/L/O."
      ),

      gc_field(
        "Cole aqui as coordenadas",
        shiny::textAreaInput(
          inputId = ns("coords"),
          label = NULL,
          rows = 9,
          resize = "vertical",
          placeholder = paste(
            "P1 24S 713110.66 8793023.16",
            "P2 24S 713550.20 8793023.16",
            "ou: -15.793889, -47.882778",
            "ou: 15°47'38\"S 47°52'58\"O",
            sep = "\n"
          )
        ),
        info = NULL,
        help = "Uma coordenada por linha. Rótulos como P1, V-01 ou Vértice 2 são reconhecidos automaticamente."
      ),

      gc_example_buttons(ns),
      htmltools::tags$div(class = "gc-counter", shiny::textOutput(ns("contador"))),

      bslib::layout_column_wrap(
        width = 1 / 2,
        gap = "0.75rem",
        gc_field(
          "Datum das coordenadas",
          shiny::selectInput(
            ns("datum"), label = NULL,
            choices = gc_datum_choices(),
            selected = "SIRGAS2000",
            width = "100%"
          ),
          info = "datum",
          help = "Está impresso no memorial ou na ART do levantamento."
        ),
        shiny::conditionalPanel(
          condition = "input.tipo === 'utm'",
          ns = ns,
          gc_field(
            "Zona UTM (fuso)",
            shiny::selectInput(
              ns("zona"), label = NULL,
              choices = c(
                "Detectar do texto / automática" = "",
                gc_brazil_zones()
              ),
              selected = "",
              width = "100%"
            ),
            info = "zona",
            help = "Só preencha aqui se o texto não trouxer a zona (ex.: 24S)."
          )
        )
      ),

      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          value = "avancado",
          title = htmltools::tagList(shiny::icon("sliders"), " Opções avançadas"),
          shiny::conditionalPanel(
            condition = "input.tipo === 'utm'",
            ns = ns,
            gc_field(
              "Ordem dos valores",
              shiny::selectInput(
                ns("ordem_utm"), label = NULL,
                choices = c(
                  "Detectar automaticamente" = "auto",
                  "E (leste) e depois N (norte)" = "EN",
                  "N (norte) e depois E (leste)" = "NE"
                ),
                width = "100%"
              ),
              info = "ordem"
            )
          ),
          shiny::conditionalPanel(
            condition = "input.tipo !== 'utm'",
            ns = ns,
            gc_field(
              "Ordem dos valores",
              shiny::selectInput(
                ns("ordem_geo"), label = NULL,
                choices = c(
                  "Detectar automaticamente" = "auto",
                  "Latitude e depois longitude" = "latlon",
                  "Longitude e depois latitude" = "lonlat"
                ),
                width = "100%"
              ),
              info = "ordem"
            )
          ),
          gc_field(
            "Separador decimal",
            shinyWidgets::radioGroupButtons(
              inputId = ns("dec_sep"),
              label = NULL,
              choices = c("Automático" = "auto", "Ponto (.)" = ".", "Vírgula (,)" = ","),
              selected = "auto",
              justified = TRUE,
              size = "sm",
              status = "outline-secondary"
            ),
            info = "separador"
          ),
          shiny::checkboxInput(
            ns("poligono"),
            label = htmltools::tagList("Fechar o polígono usando os pontos na ordem em que aparecem"),
            value = TRUE
          ),
          htmltools::tags$div(
            class = "gc-help",
            "Desligue para calcular apenas pontos (sem área e perímetro).",
            info_badge("poligono")
          )
        )
      ),

      htmltools::tags$div(
        class = "gc-actions",
        shiny::actionButton(
          ns("converter"),
          label = htmltools::tagList(shiny::icon("bolt"), " Converter agora"),
          class = "btn-primary"
        ),
        shiny::actionButton(
          ns("limpar"),
          label = htmltools::tagList(shiny::icon("eraser"), " Limpar"),
          class = "btn-outline-secondary"
        )
      ),
      htmltools::tags$div(
        class = "gc-help",
        "A conversão também acontece sozinha enquanto você digita."
      )
    )
  )
}

#' Servidor do painel de entrada
#'
#' @return lista de reactives com os valores já normalizados.
mod_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$contador <- shiny::renderText({
      txt <- input$coords %||% ""
      n <- nrow(gc_lines(txt))
      if (n == 0L) {
        return("Nenhuma linha com coordenadas ainda.")
      }
      paste0(n, if (n == 1L) " linha pronta para converter" else " linhas prontas para converter",
             " · ", nchar(txt), " caracteres")
    })

    # texto aplicado pelo próprio servidor (exemplos e "Limpar"): garante que a
    # conversão aconteça mesmo antes de o navegador devolver o valor do textarea
    texto_forcado <- shiny::reactiveVal(NULL)

    aplicar_exemplo <- function(id) {
      spec <- gc_example_spec(id)
      texto_forcado(spec$texto)
      shiny::updateTextAreaInput(session, "coords", value = spec$texto)
      shinyWidgets::updateRadioGroupButtons(session, "tipo", selected = spec$tipo)
      shiny::updateSelectInput(session, "datum", selected = spec$datum)
      shiny::updateSelectInput(session, "zona", selected = spec$zona)
      shinyWidgets::updateRadioGroupButtons(session, "dec_sep",
                                            selected = spec$dec_sep)
    }

    for (id in c("utm", "utm_br", "dd", "gms", "sad69")) {
      local({
        exemplo_id <- id
        shiny::observeEvent(input[[paste0("ex_", exemplo_id)]],
                            aplicar_exemplo(exemplo_id),
                            ignoreInit = TRUE)
      })
    }
    shiny::observeEvent(input$limpar, {
      texto_forcado("")
      shiny::updateTextAreaInput(session, "coords", value = "")
    }, ignoreInit = TRUE)

    # se o usuário editar o texto, o exemplo deixa de valer
    shiny::observeEvent(input$coords, {
      ov <- texto_forcado()
      if (!is.null(ov) && !identical(gc_squish(input$coords), gc_squish(ov))) {
        texto_forcado(NULL)
      }
    }, ignoreInit = TRUE)

    settings <- shiny::reactive({
      tipo <- input$tipo %||% "utm"
      z <- gc_parse_zone(input$zona)
      ordem <- switch(
        tipo,
        utm = input$ordem_utm %||% "auto",
        geographic = input$ordem_geo %||% "auto",
        auto = input$ordem_geo %||% "auto",
        "auto"
      )
      list(
        text = texto_forcado() %||% input$coords %||% "",
        type = tipo,
        datum = input$datum %||% "SIRGAS2000",
        zone = z$zone,
        hemisphere = z$hemisphere,
        dec_sep = input$dec_sep %||% "auto",
        order = ordem %||% "auto",
        polygon = isTRUE(input$poligono)
      )
    })

    list(
      settings = settings,
      converter = shiny::reactive(input$converter),
      text = shiny::reactive(input$coords %||% "")
    )
  })
}
