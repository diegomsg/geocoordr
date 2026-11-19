# =============================================================================
# geocoordR :: utilitários internos
#
# Conteúdo:
#   * operadores e helpers genéricos;
#   * construtores de "problemas" (erros/avisos) usados por todo o app;
#   * formatação numérica em pt-BR.
#
# Este arquivo não depende de nenhum pacote além do R base, de propósito:
# toda a lógica de entrada/saída textual é testável sem Shiny, sem sf e sem
# conexão de rede (ver tests/testthat/).
# =============================================================================

# Versão exibida no app, nos relatórios e nos arquivos exportados
GC_APP_VERSION <- "2.0.0"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

#' Remove espaços repetidos e das pontas (inclusive NBSP)
gc_squish <- function(x) {
  x <- gsub("\u00a0|\u2007|\u202f", " ", x, useBytes = FALSE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

#' Normaliza texto para comparação (maiúsculas, sem acento)
gc_norm <- function(x) {
  x <- gc_squish(as.character(x))
  x <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  toupper(x %||% "")
}

#' Converte texto vazio/NA em `NA`
gc_blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & !nzchar(gc_squish(x))] <- NA_character_
  x
}

# -----------------------------------------------------------------------------
# Problemas (erros, avisos e dicas)
# -----------------------------------------------------------------------------
#
# Um "problema" é sempre uma linha de um data.frame com as mesmas colunas.
# Isso permite acumular vários problemas durante a leitura de um memorial
# (em vez de abortar no primeiro erro) e apresentá-los, todos, ao usuário.

GC_ISSUE_COLS <- c(
  "level", "code", "line", "input", "title", "message", "hint", "link"
)

gc_empty_issues <- function() {
  # constante: evita reconstruir o data.frame em cada chamada (rota quente)
  .gc_empty_issues
}

.gc_empty_issues <- data.frame(
  level   = character(),
  code    = character(),
  line    = integer(),
  input   = character(),
  title   = character(),
  message = character(),
  hint    = character(),
  link    = character(),
  stringsAsFactors = FALSE
)

#' Cria um problema
#'
#' @param level "erro" (impede o resultado), "aviso" (resultado sai, mas
#'   confira) ou "dica".
#' @param code identificador curto e estável (usado nos testes).
#' @param title manchete curta em linguagem simples.
#' @param message explicação do que aconteceu.
#' @param hint o que o usuário deve fazer.
#' @param line número da linha do texto de entrada (1-based) ou NA.
#' @param input trecho original que gerou o problema.
#' @param link URL de documentação técnica (opcional).
gc_issue <- function(level = c("erro", "aviso", "dica"),
                     code,
                     title,
                     message,
                     hint = NA_character_,
                     line = NA_integer_,
                     input = NA_character_,
                     link = NA_character_) {
  level <- match.arg(level)
  data.frame(
    level   = level,
    code    = as.character(code),
    line    = if (is.null(line)) NA_integer_ else as.integer(line),
    input   = gc_blank_to_na(input),
    title   = as.character(title),
    message = as.character(message),
    hint    = gc_blank_to_na(hint),
    link    = gc_blank_to_na(link),
    stringsAsFactors = FALSE
  )
}

#' Junta problemas em um único data.frame
#'
#' Aceita data.frames e listas de data.frames (resultado de lapply).
#' Construído sem rbind() de propósito: rbind.data.frame chama deparse() para
#' gerar nomes de linha, o que é caro quando há uma linha por ponto do memorial.
gc_issues <- function(...) {
  items <- list(...)
  flat <- list()
  for (it in items) {
    if (is.data.frame(it)) {
      flat[[length(flat) + 1L]] <- it
    } else if (is.list(it) && !is.null(it)) {
      for (sub in it) {
        if (is.data.frame(sub)) {
          flat[[length(flat) + 1L]] <- sub
        }
      }
    }
  }
  if (!length(flat)) {
    return(gc_empty_issues())
  }
  keep <- logical(length(flat))
  for (k in seq_along(flat)) {
    keep[k] <- nrow(flat[[k]]) > 0L
  }
  flat <- flat[keep]
  if (!length(flat)) {
    return(gc_empty_issues())
  }
  if (length(flat) == 1L) {
    out <- flat[[1]][, GC_ISSUE_COLS, drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }
  out <- vector("list", length(GC_ISSUE_COLS))
  names(out) <- GC_ISSUE_COLS
  for (col in GC_ISSUE_COLS) {
    out[[col]] <- unlist(lapply(flat, .subset2, col), use.names = FALSE)
  }
  class(out) <- "data.frame"
  attr(out, "row.names") <- .set_row_names(length(out$level))
  out
}

gc_error <- function(...) gc_issue(level = "erro", ...)
gc_warning <- function(...) gc_issue(level = "aviso", ...)
gc_tip <- function(...) gc_issue(level = "dica", ...)

gc_has_error <- function(issues) {
  if (is.null(issues) || !nrow(issues)) {
    return(FALSE)
  }
  any(issues$level == "erro")
}

gc_count <- function(issues, level) {
  if (is.null(issues) || !nrow(issues)) {
    return(0L)
  }
  sum(issues$level == level)
}

#' Texto plano multi-linha de um conjunto de problemas (usado em testes e logs)
gc_issues_text <- function(issues) {
  if (is.null(issues) || !nrow(issues)) {
    return("Nenhum problema encontrado.")
  }
  rotulo <- c(erro = "ERRO", aviso = "AVISO", dica = "DICA")
  linha <- ifelse(
    is.na(issues$line),
    "",
    paste0("linha ", issues$line, ": ")
  )
  out <- paste0(
    "[", rotulo[issues$level], "] ", linha, issues$title,
    ifelse(is.na(issues$message), "", paste0(" - ", issues$message))
  )
  paste(out, collapse = "\n")
}

# -----------------------------------------------------------------------------
# Formatação numérica (pt-BR)
# -----------------------------------------------------------------------------

#' Formata número com casas fixas e separador escolhido
gc_fmt <- function(x, digits = 2, dec = ",", big = ".", na = "—") {
  if (length(x) != 1L) {
    return(vapply(x, gc_fmt, character(1), digits = digits, dec = dec,
                  big = big, na = na))
  }
  if (is.null(x) || is.na(x) || !is.finite(x)) {
    return(na)
  }
  if (identical(dec, big)) {
    big <- ""
  }
  out <- formatC(round(as.numeric(x), digits), format = "f", digits = digits,
                 decimal.mark = dec, big.mark = big)
  out
}

#' Número com agrupamento de milhar e sem casas decimais desnecessárias
gc_fmt_num <- function(x, digits = 3, dec = ",", big = ".") {
  if (length(x) != 1L) {
    return(vapply(x, gc_fmt_num, character(1), digits = digits, dec = dec,
                  big = big))
  }
  if (is.null(x) || is.na(x) || !is.finite(x)) {
    return("—")
  }
  if (identical(dec, big)) {
    big <- ""
  }
  out <- formatC(round(as.numeric(x), digits), format = "f", digits = digits,
                 decimal.mark = dec, big.mark = big)
  out
}

#' Área em m² e hectares, pronta para exibição
gc_fmt_area <- function(area_m2) {
  if (length(area_m2) != 1L) {
    return(vapply(area_m2, gc_fmt_area, character(1)))
  }
  if (is.null(area_m2) || is.na(area_m2) || !is.finite(area_m2)) {
    return("—")
  }
  if (area_m2 >= 10000) {
    paste0(gc_fmt_num(area_m2 / 10000, 4), " ha (",
           gc_fmt_num(area_m2, 2), " m²)")
  } else {
    paste0(gc_fmt_num(area_m2, 2), " m²")
  }
}

# -----------------------------------------------------------------------------
# Diversos
# -----------------------------------------------------------------------------

#' Escapa texto para XML/KML/HTML
gc_xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

#' Timestamp para nomes de arquivo: 20240131-142530
gc_stamp <- function(time = Sys.time()) {
  format(time, "%Y%m%d-%H%M%S")
}

#' Converte texto em vetor de linhas não vazias, preservando o número da linha
gc_lines <- function(text) {
  if (is.null(text)) {
    return(data.frame(line = integer(), raw = character()))
  }
  text <- paste(as.character(text), collapse = "\n")
  text <- gsub("\r\n?", "\n", text)
  text <- gsub("\ufeff", "", text, fixed = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- sub("[[:space:]]+$", "", lines)
  keep <- nzchar(gc_squish(lines))
  data.frame(
    line = which(keep),
    raw = lines[keep],
    stringsAsFactors = FALSE
  )
}
