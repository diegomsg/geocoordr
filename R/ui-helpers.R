# =============================================================================
# geocoordR :: tema, componentes de UI e exemplos
# =============================================================================

#' Tema claro/escuro do app
gc_theme <- function(dark = FALSE) {
  fonte <- bslib::font_collection(
    "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto",
    "Helvetica Neue", "Arial", "sans-serif"
  )
  bslib::bs_theme(
    version = 5,
    preset = if (dark) "darkly" else "flatly",
    primary = if (dark) "#2dd4bf" else "#0f766e",
    secondary = "#64748b",
    success = "#15803d",
    danger = "#b91c1c",
    warning = "#b45309",
    base_font = fonte,
    heading_font = fonte,
    font_scale = 0.95
  )
}

#' CSS extra do app
gc_css <- function() {
  htmltools::tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "geocoordr.css"
  )
}

#' Rótulo de campo com ícone (i) e texto de apoio
gc_field <- function(label, input, info = NULL, help = NULL) {
  htmltools::tags$div(
    class = "gc-field",
    htmltools::tags$div(
      class = "gc-field-head",
      htmltools::tags$span(class = "gc-field-label", label),
      if (!is.null(info)) info_badge(info)
    ),
    input,
    if (!is.null(help)) {
      htmltools::tags$div(class = "gc-help", help)
    }
  )
}

#' Título de seção com subtítulo
gc_section <- function(title, subtitle = NULL, icon = NULL) {
  htmltools::tags$div(
    class = "gc-section",
    htmltools::tags$h5(
      class = "gc-section-title",
      if (!is.null(icon)) shiny::icon(icon),
      title
    ),
    if (!is.null(subtitle)) {
      htmltools::tags$p(class = "gc-section-sub", subtitle)
    }
  )
}

#' Bloco de destaque numérico (usado no resumo)
gc_stat <- function(label, value, sub = NULL, info = NULL) {
  htmltools::tags$div(
    class = "gc-stat",
    htmltools::tags$div(
      class = "gc-stat-label",
      label,
      if (!is.null(info)) info_badge(info, placement = "bottom")
    ),
    htmltools::tags$div(class = "gc-stat-value", value),
    if (!is.null(sub)) htmltools::tags$div(class = "gc-stat-sub", sub)
  )
}

# -----------------------------------------------------------------------------
# Problemas (erros/avisos) na interface
# -----------------------------------------------------------------------------
gc_issue_icon <- function(level) {
  switch(
    level,
    erro = shiny::icon("circle-xmark"),
    aviso = shiny::icon("triangle-exclamation"),
    dica = shiny::icon("circle-info"),
    shiny::icon("circle-info")
  )
}

#' Lista de problemas com ícones, linha do texto e sugestão
gc_issues_ui <- function(issues, max_show = 15) {
  if (is.null(issues) || !nrow(issues)) {
    return(htmltools::tags$div(
      class = "gc-issue gc-issue--ok",
      gc_issue_icon("dica"),
      htmltools::tags$span("Nenhum problema encontrado na leitura das coordenadas.")
    ))
  }
  ordenado <- issues[order(
    match(issues$level, c("erro", "aviso", "dica")),
    issues$line, na.last = TRUE
  ), , drop = FALSE]
  mostrar <- utils::head(ordenado, max_show)
  itens <- lapply(seq_len(nrow(mostrar)), function(i) {
    it <- mostrar[i, ]
    htmltools::tags$div(
      class = paste0("gc-issue gc-issue--", it$level),
      htmltools::tags$div(
        class = "gc-issue-head",
        gc_issue_icon(it$level),
        htmltools::tags$span(class = "gc-issue-title", it$title),
        if (!is.na(it$line)) {
          htmltools::tags$span(class = "gc-issue-line", paste0("linha ", it$line))
        }
      ),
      htmltools::tags$div(class = "gc-issue-message", it$message),
      if (!is.na(it$input)) {
        htmltools::tags$code(class = "gc-issue-input", it$input)
      },
      if (!is.na(it$hint)) {
        htmltools::tags$div(
          class = "gc-issue-hint",
          shiny::icon("lightbulb"),
          " ",
          it$hint
        )
      },
      if (!is.na(it$link)) {
        htmltools::tags$div(
          class = "gc-issue-link",
          htmltools::tags$a(
            href = it$link, target = "_blank", rel = "noopener noreferrer",
            "Documentação técnica", shiny::icon("arrow-up-right-from-square")
          )
        )
      }
    )
  })
  resto <- if (nrow(ordenado) > nrow(mostrar)) {
    htmltools::tags$div(
      class = "gc-help",
      paste0("E mais ", nrow(ordenado) - nrow(mostrar), " observação(ões).")
    )
  }
  htmltools::tagList(
    htmltools::tags$div(class = "gc-issues", itens),
    resto
  )
}

#' Cabeçalho compacto de contagem de problemas
gc_issues_badge <- function(issues) {
  n_erro <- gc_count(issues, "erro")
  n_aviso <- gc_count(issues, "aviso")
  n_dica <- gc_count(issues, "dica")
  cls <- if (n_erro > 0) "gc-pill gc-pill--erro" else if (n_aviso > 0) {
    "gc-pill gc-pill--aviso"
  } else {
    "gc-pill gc-pill--ok"
  }
  texto <- if (n_erro > 0) {
    paste0(n_erro, if (n_erro == 1) " erro" else " erros")
  } else if (n_aviso > 0) {
    paste0(n_aviso, if (n_aviso == 1) " aviso" else " avisos")
  } else {
    "sem problemas"
  }
  htmltools::tags$span(class = cls, gc_issue_icon(if (n_erro > 0) "erro" else if (n_aviso > 0) "aviso" else "dica"), texto)
}

# -----------------------------------------------------------------------------
# Exemplos
# -----------------------------------------------------------------------------
gc_examples <- list(
  utm = paste(
    "P1 24S 713110.66 8793023.16",
    "P2 24S 713550.20 8793023.16",
    "P3 24S 713550.20 8793423.16",
    "P4 24S 713110.66 8793423.16",
    sep = "\n"
  ),
  utm_br = paste(
    "P1: 24S, E 713.110,66, N 8.793.023,16",
    "P2: 24S, E 713.550,20, N 8.793.023,16",
    "P3: 24S, E 713.550,20, N 8.793.423,16",
    "P4: 24S, E 713.110,66, N 8.793.423,16",
    sep = "\n"
  ),
  dd = paste(
    "-15.793889, -47.882778",
    "-15.793889, -47.878333",
    "-15.790278, -47.878333",
    "-15.790278, -47.882778",
    sep = "\n"
  ),
  gms = paste(
    "15°47'38\"S 47°52'58\"O",
    "15°47'38\"S 47°52'42\"O",
    "15°47'25\"S 47°52'42\"O",
    "15°47'25\"S 47°52'58\"O",
    sep = "\n"
  ),
  sad69 = paste(
    "P1 23S 333287.92 7394588.32",
    "P2 23S 333650.10 7394588.32",
    "P3 23S 333650.10 7394950.50",
    "P4 23S 333287.92 7394950.50",
    sep = "\n"
  )
)

#' Especificação de cada exemplo: texto + opções que devem acompanhá-lo
gc_example_spec <- function(id) {
  specs <- list(
    utm = list(texto = gc_examples$utm, tipo = "utm", datum = "SIRGAS2000",
               zona = "", dec_sep = "auto"),
    utm_br = list(texto = gc_examples$utm_br, tipo = "utm", datum = "SIRGAS2000",
                  zona = "", dec_sep = "auto"),
    dd = list(texto = gc_examples$dd, tipo = "geographic", datum = "SIRGAS2000",
              zona = "", dec_sep = "auto"),
    gms = list(texto = gc_examples$gms, tipo = "geographic", datum = "SIRGAS2000",
               zona = "", dec_sep = "auto"),
    sad69 = list(texto = gc_examples$sad69, tipo = "utm", datum = "SAD69",
                 zona = "", dec_sep = "auto")
  )
  specs[[id]] %||% specs$utm
}

#' Botões de exemplo (preenchem o campo de texto)
gc_example_buttons <- function(ns) {
  htmltools::tags$div(
    class = "gc-examples",
    htmltools::tags$span(class = "gc-examples-label", "Ver exemplo:"),
    actionLink(ns("ex_utm"), "UTM", class = "gc-example-link"),
    actionLink(ns("ex_utm_br"), "UTM (padrão brasileiro)", class = "gc-example-link"),
    actionLink(ns("ex_dd"), "Graus decimais", class = "gc-example-link"),
    actionLink(ns("ex_gms"), "GMS", class = "gc-example-link"),
    actionLink(ns("ex_sad69"), "SAD 69", class = "gc-example-link")
  )
}
