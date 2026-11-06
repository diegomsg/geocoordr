# =============================================================================
# geocoordR :: módulo de resultados (tabela, resumo, lados, verificações)
# =============================================================================

gc_dt_language <- list(
  emptyTable = "Nenhum dado disponível",
  info = "Mostrando _START_ a _END_ de _TOTAL_ linhas",
  infoEmpty = "Nenhuma linha",
  infoFiltered = "(filtrado de _MAX_ linhas)",
  lengthMenu = "Mostrar _MENU_ linhas",
  search = "Buscar:",
  zeroRecords = "Nenhum resultado encontrado",
  paginate = list(
    first = "Primeiro", last = "Último", `next` = "Próximo", previous = "Anterior"
  )
)

#' UI do painel de resultados
#'
#' @param selected aba aberta no início ("Tabela", "Mapa", "Resumo", "Lados",
#'   "Verificações", "Exportar" ou "Técnico").
mod_results_ui <- function(id, selected = "Tabela") {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      htmltools::tags$div(
        class = "gc-card-title",
        shiny::icon("chart-simple"),
        "2. Resultados",
        shiny::uiOutput(ns("badge"), inline = TRUE)
      )
    ),
    bslib::card_body(
      shiny::uiOutput(ns("faixa_resumo")),
      bslib::navset_card_tab(
        id = ns("abas"),
        selected = selected,
        bslib::nav_panel(
          title = "Tabela", icon = shiny::icon("table"),
          htmltools::tags$div(
            class = "gc-help", style = "margin: .5rem 0;",
            "Coordenadas de saída em SIRGAS 2000. Role para o lado para ver todas as colunas."
          ),
          DT::DTOutput(ns("tabela"))
        ),
        bslib::nav_panel(
          title = "Mapa", icon = shiny::icon("map"),
          mod_map_ui(ns("mapa"))
        ),
        bslib::nav_panel(
          title = "Resumo", icon = shiny::icon("calculator"),
          shiny::uiOutput(ns("resumo"))
        ),
        bslib::nav_panel(
          title = "Lados", icon = shiny::icon("ruler-combined"),
          htmltools::tags$div(
            class = "gc-help", style = "margin: .5rem 0;",
            "Distâncias e azimutes no plano UTM, na ordem em que os vértices foram informados."
          ),
          DT::DTOutput(ns("lados"))
        ),
        bslib::nav_panel(
          title = "Verificações", icon = shiny::icon("clipboard-check"),
          shiny::uiOutput(ns("verificacoes"))
        ),
        bslib::nav_panel(
          title = "Exportar", icon = shiny::icon("file-export"),
          mod_export_ui(ns("export"))
        ),
        bslib::nav_panel(
          title = "Técnico", icon = shiny::icon("gear"),
          shiny::uiOutput(ns("tecnico"))
        )
      )
    )
  )
}

gc_empty_dt <- function(msg) {
  DT::datatable(
    data.frame(` ` = msg, check.names = FALSE),
    rownames = FALSE,
    options = list(dom = "t", ordering = FALSE, columnDefs = list(
      list(className = "dt-center", targets = "_all")
    ))
  )
}

#' Servidor do painel de resultados
mod_results_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_map_server("mapa", result)
    mod_export_server("export", result)

    valido <- shiny::reactive({
      res <- result()
      !is.null(res) && nrow(res$points) > 0L
    })

    output$badge <- shiny::renderUI({
      res <- result()
      if (is.null(res) || !nrow(res$points)) {
        return(htmltools::tags$span(class = "gc-pill", "aguardando coordenadas"))
      }
      gc_issues_badge(res$issues)
    })

    # ------------------------------------------------------- faixa resumo ---
    output$faixa_resumo <- shiny::renderUI({
      res <- result()
      if (is.null(res) || !nrow(res$points)) {
        return(htmltools::tags$div(
          class = "gc-help", style = "margin-bottom: .5rem;",
          shiny::icon("arrow-left"),
          " Cole as coordenadas no painel ao lado para começar. ",
          "Se preferir, clique em um dos exemplos."
        ))
      }
      s <- res$summary
      htmltools::tags$div(
        class = "gc-stats",
        gc_stat("Pontos", as.character(s$n),
                paste0(if (s$n_utm > 0) paste0(s$n_utm, " UTM") else NULL,
                       if (s$n_utm > 0 && s$n_geo > 0) " + " else NULL,
                       if (s$n_geo > 0) paste0(s$n_geo, " em graus") else NULL)),
        gc_stat("Área",
                if (isTRUE(s$polygon)) paste0(gc_fmt_num(s$area_ha, 4), " ha") else "—",
                if (isTRUE(s$polygon)) paste0(gc_fmt_num(s$area_m2, 2), " m²") else "sem polígono",
                info = "area"),
        gc_stat("Perímetro",
                if (isTRUE(s$polygon)) paste0(gc_fmt_num(s$perimeter_m, 2), " m") else "—",
                if (isTRUE(s$polygon)) paste0(s$n_sides, " lados") else "sem polígono",
                info = "area"),
        gc_stat("Zona de saída",
                paste0(s$zona %||% "—", s$hemisferio %||% ""),
                if (!is.na(s$epsg %||% NA)) paste0("EPSG:", s$epsg) else "",
                info = "epsg")
      )
    })

    # ------------------------------------------------------------ tabela ----
    output$tabela <- DT::renderDT({
      res <- result()
      if (is.null(res) || !nrow(res$points)) {
        return(gc_empty_dt("Cole as coordenadas para ver a tabela."))
      }
      pts <- res$points
      dados <- data.frame(
        Ponto = pts$ponto,
        Latitude = round(pts$lat, 8),
        Longitude = round(pts$lon, 8),
        `Lat. (GMS)` = pts$lat_dms,
        `Long. (GMS)` = pts$lon_dms,
        `E (m)` = round(pts$e, 3),
        `N (m)` = round(pts$n, 3),
        Zona = paste0(ifelse(is.na(pts$zona), "", pts$zona),
                      ifelse(is.na(pts$hemisferio), "", pts$hemisferio)),
        Datum = gc_datum(pts$datum_entrada[1])$short,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        dados,
        rownames = FALSE,
        class = "compact stripe hover nowrap",
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = "Copiar"),
            list(extend = "csv", text = "CSV"),
            list(extend = "print", text = "Imprimir")
          ),
          language = gc_dt_language
        )
      ) |>
        DT::formatCurrency(columns = c("Latitude", "Longitude"), currency = "",
                           interval = 3, mark = ".", dec.mark = ",", digits = 8) |>
        DT::formatCurrency(columns = c("E (m)", "N (m)"), currency = "",
                           interval = 3, mark = ".", dec.mark = ",", digits = 3)
    }, server = FALSE)

    # -------------------------------------------------------------- lados ---
    output$lados <- DT::renderDT({
      res <- result()
      if (is.null(res) || !nrow(res$polygon$sides)) {
        return(gc_empty_dt("Sem lados: informe 3 ou mais pontos e feche o polígono."))
      }
      sides <- res$polygon$sides
      dados <- data.frame(
        De = sides$de,
        Para = sides$para,
        `Distância (m)` = round(sides$distancia, 3),
        `Azimute (°)` = round(sides$azimute, 4),
        check.names = FALSE
      )
      DT::datatable(
        dados,
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(
          pageLength = 15, dom = "frtip", scrollX = TRUE,
          language = gc_dt_language
        )
      ) |>
        DT::formatCurrency(columns = "Distância (m)", currency = "", interval = 3,
                           mark = ".", dec.mark = ",", digits = 3) |>
        DT::formatCurrency(columns = "Azimute (°)", currency = "", interval = 3,
                           mark = ".", dec.mark = ",", digits = 2)
    })

    # ------------------------------------------------------------- resumo ---
    output$resumo <- shiny::renderUI({
      res <- result()
      if (is.null(res) || !nrow(res$points)) {
        return(htmltools::tags$p(class = "gc-muted", "Sem dados para resumir."))
      }
      s <- res$summary
      linhas <- list(
        c("Pontos lidos", as.character(s$n)),
        c("Tipos de entrada", s$tipos),
        c("Datum de entrada", gc_datum(res$meta$datum_entrada)$label),
        c("Datum de saída", gc_datum(res$meta$datum_saida)$label),
        c("Zona UTM de saída", paste0(s$zona %||% "—", s$hemisferio %||% "")),
        c("Código EPSG", paste0("EPSG:", s$epsg %||% "—")),
        c("Centro da área",
          paste0(gc_fmt(s$centroid["lat"], 6, dec = ","), "°, ",
                 gc_fmt(s$centroid["lon"], 6, dec = ","), "°")),
        c("Caixa envolvente",
          paste0("lat ", gc_fmt(s$bbox$lat_min, 5, dec = ","), " a ",
                 gc_fmt(s$bbox$lat_max, 5, dec = ","),
                 " · lon ", gc_fmt(s$bbox$lon_min, 5, dec = ","), " a ",
                 gc_fmt(s$bbox$lon_max, 5, dec = ","))),
        c("Fator de escala UTM", gc_fmt(s$scale_factor, 6, dec = ",")),
        c("Área (plano UTM)", if (isTRUE(s$polygon)) gc_fmt_area(s$area_m2) else "—"),
        c("Perímetro", if (isTRUE(s$polygon)) paste0(gc_fmt_num(s$perimeter_m, 2), " m") else "—"),
        c("Maior lado", if (isTRUE(s$polygon)) paste0(gc_fmt_num(s$max_side_m, 2), " m") else "—"),
        c("Cruzamentos entre lados", as.character(s$self_intersections))
      )
      tabela <- htmltools::tags$table(
        class = "gc-kv",
        htmltools::tags$tbody(
          lapply(linhas, function(l) {
            htmltools::tags$tr(
              htmltools::tags$td(l[1]),
              htmltools::tags$td(l[2])
            )
          })
        )
      )
      htmltools::tagList(
        gc_section("Resumo do imóvel", NULL, icon = "calculator"),
        tabela,
        htmltools::tags$p(
          class = "gc-help",
          style = "margin-top: .75rem;",
          shiny::icon("circle-info"),
          " A área e o perímetro são planos (UTM). Para áreas muito maiores que ",
          "uma zona UTM, o ideal é o cálculo geodésico — fale com um profissional habilitado."
        )
      )
    })

    # ------------------------------------------------------- verificações ---
    output$verificacoes <- shiny::renderUI({
      res <- result()
      htmltools::tagList(
        gc_section(
          "O que foi conferido",
          paste0("Avisos não impedem o resultado: indicam pontos que merecem uma ",
                 "conferência antes de usar os dados em um documento oficial."),
          icon = "clipboard-check"
        ),
        if (is.null(res) || !nrow(res$points)) {
          htmltools::tags$p(class = "gc-muted", "Sem dados para verificar.")
        } else {
          gc_issues_ui(res$issues)
        }
      )
    })

    # ----------------------------------------------------------- técnico ----
    output$tecnico <- shiny::renderUI({
      res <- result()
      if (is.null(res) || !nrow(res$points)) {
        return(htmltools::tags$p(class = "gc-muted", "Sem dados técnicos ainda."))
      }
      s <- res$summary
      d_in <- gc_datum(res$meta$datum_entrada)
      d_out <- gc_datum(res$meta$datum_saida)
      linhas <- list(
        c("Entrada", paste0("Texto com ", s$n, " coordenada(s) no tipo ", s$tipos)),
        c("Datum de entrada",
          paste0(d_in$short, " (EPSG:", d_in$epsg, ")")),
        c("Caminho de cálculo",
          "texto -> leitura -> inversa da projeção (se UTM) -> Helmert para o WGS 84 -> datum de saída -> projeção UTM"),
        c("Datum de saída", paste0(d_out$short, " (EPSG:", d_out$epsg, ")")),
        c("CRS de saída", paste0("EPSG:", s$epsg %||% "—")),
        c("PROJ.4", s$proj4),
        c("Elipsóide", gc_ellipsoids[[d_out$ellipsoid]]$name),
        c("Precisão da projeção", "Erro inferior a 1 mm dentro da zona (validado contra o PROJ)"),
        c("Precisão do datum",
          if (identical(res$meta$datum_entrada, res$meta$datum_saida)) {
            "Sem transformação de datum"
          } else {
            paste0("Translação de 3 parâmetros; erro da ordem de ",
                   d_in$accuracy_m, " m")
          }),
        c("Fator de escala UTM (centro)", gc_fmt(s$scale_factor, 8, dec = ".")),
        c("Versão", GC_APP_VERSION)
      )
      htmltools::tags$table(
        class = "gc-kv",
        htmltools::tags$tbody(
          lapply(linhas, function(l) {
            htmltools::tags$tr(
              htmltools::tags$td(l[1]),
              htmltools::tags$td(l[2])
            )
          })
        )
      )
    })

    # as abas escondidas continuam sendo calculadas: ao clicar, o conteúdo já
    # está pronto (nada de tela em branco esperando o servidor)
    for (saida in c("faixa_resumo", "tabela", "resumo", "lados",
                    "verificacoes", "tecnico")) {
      shiny::outputOptions(output, saida, suspendWhenHidden = FALSE)
    }

    invisible(NULL)
  })
}
