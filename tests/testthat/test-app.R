# =============================================================================
# Teste de integração: o app completo (app.R) monta e converte
# =============================================================================

test_that("o app completo monta e devolve resultado", {
  skip_if_not(requireNamespace("DT", quietly = TRUE))

  pasta <- getwd()
  setwd(GC_ROOT)
  on.exit(setwd(pasta), add = TRUE)

  env <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(
    sys.source(file.path(GC_ROOT, "app.R"), envir = env)
  ))
  expect_s3_class(env$ui, "bslib_page")
  expect_true(is.function(env$server))
  expect_match(as.character(env$ui), "Informe as coordenadas")

  shiny::testServer(env$server, {
    session$setInputs(
      `input-tipo` = "utm",
      `input-coords` = paste(
        "P1 24S 713110.66 8793023.16",
        "P2 24S 713550.20 8793023.16",
        "P3 24S 713550.20 8793423.16",
        "P4 24S 713110.66 8793423.16",
        sep = "\n"
      ),
      `input-datum` = "SIRGAS2000",
      `input-zona` = "",
      `input-dec_sep` = "auto",
      `input-ordem_utm` = "auto",
      `input-ordem_geo` = "auto",
      `input-poligono` = TRUE
    )
    session$flushReact()

    tabela <- output$`resultados-tabela`
    expect_true(grepl("P1", tabela))
    expect_true(grepl("8793023", tabela))

    resumo <- paste(as.character(output$`resultados-resumo`), collapse = "")
    expect_match(resumo, "EPSG:31984")

    faixa <- paste(as.character(output$`resultados-faixa_resumo`), collapse = "")
    expect_match(faixa, "17,5816 ha|175.816")
  })
})
