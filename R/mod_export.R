# =============================================================================
# geocoordR :: módulo de exportação
# =============================================================================

gc_export_item <- function(ns_id, titulo, descricao, info = NULL,
                           icon = "file-arrow-down", ativo = TRUE) {
  botao <- if (isTRUE(ativo)) {
    shiny::downloadButton(
      ns_id,
      label = htmltools::tagList(shiny::icon("download"), " Baixar"),
      class = "btn-sm btn-outline-primary"
    )
  } else {
    htmltools::tags$span(
      class = "btn btn-sm btn-outline-secondary disabled",
      `aria-disabled` = "true",
      shiny::icon("ban"),
      " Indisponível"
    )
  }
  htmltools::tags$div(
    class = "gc-export-item",
    htmltools::tags$div(
      class = "gc-export-name",
      shiny::icon(icon),
      htmltools::tags$span(class = "gc-export-title", titulo),
      if (!is.null(info)) info_badge(info, placement = "bottom")
    ),
    htmltools::tags$div(class = "gc-export-desc", descricao),
    botao
  )
}

#' UI da aba de exportação
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    gc_section(
      "Levar os dados para outra ferramenta",
      paste0("Todos os arquivos saem em SIRGAS 2000 (EPSG:4674), com a zona UTM ",
             "usada no resultado."),
      icon = "file-export"
    ),
    htmltools::tags$div(
      class = "gc-export-grid",
      gc_export_item(
        ns("csv_br"),
        "Planilha (Excel BR)",
        "CSV com ponto e vírgula e vírgula decimal. Abre direto no Excel em português.",
        info = "csv", icon = "file-csv"
      ),
      gc_export_item(
        ns("csv_intl"),
        "CSV internacional",
        "Vírgula como separador e ponto decimal, para QGIS, Python, R e bancos de dados.",
        info = "csv", icon = "file-csv"
      ),
      gc_export_item(
        ns("geojson"),
        "GeoJSON",
        "Para Google My Maps, Leaflet, QGIS, SIGEF e aplicações web.",
        info = "geojson", icon = "file-code"
      ),
      gc_export_item(
        ns("kml"),
        "KML",
        "Abre no Google Earth: o imóvel aparece desenhado sobre a imagem de satélite.",
        info = "kml", icon = "earth-americas"
      ),
      gc_export_item(
        ns("sides_csv"),
        "Lados (confrontações)",
        "Distância e azimute de cada lado do polígono, prontos para a planilha.",
        info = "azimute", icon = "ruler"
      ),
      gc_export_item(
        ns("meta_txt"),
        "Relatório técnico",
        "Metadados: datum, zona, EPSG, área, perímetro e data da conversão.",
        info = "epsg", icon = "file-lines"
      )
    ),
    htmltools::tags$div(
      class = "gc-export-grid",
      style = "margin-top: .75rem;",
      gc_export_item(
        ns("gpkg"),
        "GeoPackage (.gpkg)",
        "Banco vetorial para QGIS/ArcGIS, com pontos e perímetro.",
        info = "gpkg", icon = "database", ativo = gc_sf_available()
      ),
      gc_export_item(
        ns("shp"),
        "Shapefile (.zip)",
        "Formato clássico de SIG (arquivos .shp/.shx/.dbf compactados).",
        info = "gpkg", icon = "layer-group", ativo = gc_sf_available()
      )
    ),
    shiny::uiOutput(ns("aviso_sf")),
    htmltools::tags$div(
      class = "gc-export-note",
      shiny::icon("circle-info"),
      paste0(" CSV, GeoJSON e KML são gerados diretamente pelo app e abrem em ",
             "qualquer ferramenta de mapas ou planilha.")
    )
  )
}

#' Servidor da aba de exportação
mod_export_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$aviso_sf <- shiny::renderUI({
      if (gc_sf_available()) {
        return(htmltools::tags$div(
          class = "gc-help",
          style = "margin-top: .5rem;",
          shiny::icon("circle-check"),
          " GeoPackage e Shapefile disponíveis (pacote 'sf' instalado)."
        ))
      }
      htmltools::tags$div(
        class = "gc-help",
        style = "margin-top: .5rem;",
        shiny::icon("triangle-exclamation"),
        " GeoPackage e Shapefile estão indisponíveis neste servidor ",
        "(pacote 'sf' não instalado). Use GeoJSON ou KML — eles abrem no QGIS."
      )
    })

    baixar_texto <- function(nome, conteudo_fn) {
      shiny::downloadHandler(
        filename = nome,
        content = function(file) {
          txt <- conteudo_fn()
          if (!nzchar(txt %||% "")) {
            writeLines("Nada para exportar.", file, useBytes = TRUE)
          } else {
            writeChar(txt, file, eos = NULL, useBytes = TRUE)
          }
        }
      )
    }

    saida <- shiny::reactive({
      res <- result()
      if (is.null(res) || !nrow(res$points) || gc_has_error(res$issues)) {
        return(NULL)
      }
      res
    })

    output$csv_br <- baixar_texto(gc_filename("csv"), function() {
      res <- saida()
      if (is.null(res)) "" else gc_csv_br(gc_export_table(res))
    })
    output$csv_intl <- baixar_texto(gc_filename("csv"), function() {
      res <- saida()
      if (is.null(res)) "" else gc_csv_intl(gc_export_table(res))
    })
    output$geojson <- baixar_texto(gc_filename("geojson"), function() {
      res <- saida()
      if (is.null(res)) "" else gc_geojson_string(res, polygon = TRUE)
    })
    output$kml <- baixar_texto(gc_filename("kml"), function() {
      res <- saida()
      if (is.null(res)) "" else gc_kml_string(res, polygon = TRUE)
    })
    output$sides_csv <- baixar_texto(gc_filename("lados.csv"), function() {
      res <- saida()
      if (is.null(res)) "" else gc_csv_br(gc_export_sides(res))
    })
    output$meta_txt <- baixar_texto(gc_filename("relatorio.txt"), function() {
      res <- saida()
      if (is.null(res)) {
        ""
      } else {
        meta <- gc_export_metadata(res)
        paste0(
          "geocoordR - relatorio tecnico\n",
          paste0(meta$item, ": ", meta$valor, collapse = "\n"),
          "\n\n", gc_result_text(res), "\n"
        )
      }
    })

    output$gpkg <- shiny::downloadHandler(
      filename = function() gc_filename("gpkg"),
      content = function(file) {
        res <- saida()
        if (is.null(res)) {
          writeLines("Nada para exportar.", file)
          return(invisible(NULL))
        }
        gc_write_sf(res, file, driver = "GPKG")
      }
    )

    output$shp <- shiny::downloadHandler(
      filename = function() gc_filename("zip"),
      content = function(file) {
        res <- saida()
        if (is.null(res)) {
          writeLines("Nada para exportar.", file)
          return(invisible(NULL))
        }
        tmp <- tempfile("gcsf")
        dir.create(tmp)
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        gc_write_sf(res, file.path(tmp, "geocoordr.shp"), driver = "ESRI Shapefile")
        arquivos <- list.files(tmp, full.names = TRUE)
        utils::zip(zipfile = file, files = arquivos, flags = "-j")
      }
    )
  })
}
