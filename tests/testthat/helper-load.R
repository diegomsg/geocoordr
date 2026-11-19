# =============================================================================
# Carrega a lógica do app (R/) para os testes.
# Funciona tanto com `Rscript tests/testthat.R` quanto com test_dir().
# =============================================================================

gc_test_root <- function() {
  inicio <- getwd()
  candidatos <- c(
    inicio,
    file.path(inicio, ".."),
    file.path(inicio, "..", ".."),
    file.path(inicio, "..", "..", "..")
  )
  for (cand in candidatos) {
    cand <- normalizePath(cand, mustWork = FALSE)
    if (file.exists(file.path(cand, "app.R")) && dir.exists(file.path(cand, "R"))) {
      return(cand)
    }
  }
  stop("Não foi possível localizar a raiz do projeto geocoordr.", call. = FALSE)
}

GC_ROOT <- gc_test_root()

suppressPackageStartupMessages({
  library(shiny)
})

arquivos_r <- sort(list.files(file.path(GC_ROOT, "R"), pattern = "[.]R$",
                              full.names = TRUE))
for (f in arquivos_r) {
  source(f, encoding = "UTF-8", local = FALSE)
}

# utilidades comuns dos testes ------------------------------------------------

#' Códigos de problema de um conjunto de issues
gc_codes <- function(issues) {
  if (is.null(issues) || !nrow(issues)) {
    return(character(0))
  }
  issues$code
}

#' Todos os textos de um conjunto de issues (para checar mensagens)
gc_texts <- function(issues) {
  if (is.null(issues) || !nrow(issues)) {
    return("")
  }
  paste(issues$title, issues$message,
        ifelse(is.na(issues$hint), "", issues$hint), collapse = " | ")
}

fixture <- function(nome) {
  utils::read.csv(file.path(GC_ROOT, "tests", "fixtures", nome),
                  stringsAsFactors = FALSE)
}
