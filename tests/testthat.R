#!/usr/bin/env Rscript
# =============================================================================
# geocoordR :: execução dos testes
#
#   Rscript tests/testthat.R
#
# Os testes cobrem a lógica pura (leitura, validação, conversão, exportação) e
# os módulos Shiny (via shiny::testServer). Não dependem de internet, de sf,
# de GDAL nem de PROJ — os valores de referência do PROJ estão congelados em
# tests/fixtures/proj-reference.csv (gerados por data-raw/gerar-fixtures-*.R).
# =============================================================================

arquivo <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit)) {
    sub("^--file=", "", hit[1])
  } else {
    file.path(getwd(), "tests", "testthat.R")
  }
})()

raiz <- normalizePath(file.path(dirname(arquivo), ".."), mustWork = FALSE)
if (!file.exists(file.path(raiz, "app.R"))) {
  raiz <- normalizePath(getwd(), mustWork = FALSE)
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Instale o pacote 'testthat' para rodar os testes.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(testthat)
})

cat("Projeto:", raiz, "\n")
cat("R:", R.version.string, "\n\n")

resultado <- testthat::test_dir(
  file.path(raiz, "tests", "testthat"),
  reporter = "summary",
  stop_on_failure = FALSE
)

df <- as.data.frame(resultado)
falhas <- sum(df$failed)
erros <- sum(df$error)
cat("\nResumo:", sum(df$passed), "asserções passaram;",
    falhas, "falharam;", erros, "erros.\n")

if (falhas > 0 || erros > 0) {
  quit(status = 1)
}
