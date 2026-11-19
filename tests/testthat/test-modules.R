# =============================================================================
# Testes: módulos Shiny (UI e servidores)
#
# testServer() executa o servidor do módulo sem navegador, o que permite
# testar a ligação entre widgets, reactives e saídas.
# =============================================================================

test_that("o tema e os componentes de UI são construídos sem erro", {
  tema <- gc_theme()
  expect_s3_class(tema, "bs_theme")

  campo <- gc_field("Datum", shiny::selectInput("x", NULL, c("a", "b")),
                    info = "datum", help = "ajuda")
  expect_s3_class(campo, "shiny.tag")

  expect_s3_class(gc_section("Título", "sub", icon = "gear"), "shiny.tag")
  expect_s3_class(gc_stat("Área", "1 ha", "10.000 m²", info = "area"), "shiny.tag")
  expect_s3_class(info_badge("utm"), "shiny.tag")
  expect_s3_class(gc_issues_ui(gc_empty_issues()), "shiny.tag")
  expect_s3_class(gc_issues_badge(gc_empty_issues()), "shiny.tag")
})

test_that("todo verbete de ajuda tem texto e fonte externa", {
  ids <- names(gc_info_registry)
  expect_gt(length(ids), 10)
  for (id in ids) {
    e <- gc_info(id)
    expect_true(nzchar(e$title), info = id)
    expect_true(nzchar(e$text), info = id)
    expect_match(e$link, "^https://", info = id)
    expect_true(nzchar(e$link_label), info = id)
  }
  # id inexistente não quebra
  expect_match(gc_info("inexistente")$link, "^https://")
})

test_that("UI do painel de entrada é montada", {
  ui <- mod_input_ui("teste")
  expect_s3_class(ui, "shiny.tag")
  html <- as.character(ui)
  expect_match(html, "teste-coords")
  expect_match(html, "teste-datum")
  expect_match(html, "teste-zona")
})

test_that("UI dos resultados e páginas estáticas são montadas", {
  expect_s3_class(mod_results_ui("res"), "shiny.tag")
  expect_s3_class(pg_ajuda(), "shiny.tag")
  expect_s3_class(pg_sobre(), "shiny.tag")
  expect_match(as.character(pg_ajuda()), "Formatos de coordenadas aceitos")
  expect_match(as.character(pg_ajuda()), "INCRA")
})

test_that("módulo de entrada traduz widgets em configuração", {
  shiny::testServer(mod_input_server, {
    session$setInputs(
      tipo = "utm", coords = "24S 713110.66 8793023.16",
      datum = "SAD69", zona = "24S", dec_sep = "auto",
      ordem_utm = "auto", ordem_geo = "auto", poligono = TRUE,
      converter = 0
    )
    cfg <- settings()
    expect_equal(cfg$type, "utm")
    expect_equal(cfg$datum, "SAD69")
    expect_equal(cfg$zone, 24L)
    expect_equal(cfg$hemisphere, "S")
    expect_equal(cfg$dec_sep, "auto")
    expect_true(cfg$polygon)
    expect_equal(cfg$text, "24S 713110.66 8793023.16")
  })
})

test_that("módulo de entrada respeita zona em branco e polígono desligado", {
  shiny::testServer(mod_input_server, {
    session$setInputs(
      tipo = "geographic", coords = "-15.79, -47.88",
      datum = "SIRGAS2000", zona = "", dec_sep = ",",
      ordem_utm = "auto", ordem_geo = "lonlat", poligono = FALSE
    )
    cfg <- settings()
    expect_true(is.na(cfg$zone))
    expect_equal(cfg$type, "geographic")
    expect_equal(cfg$dec_sep, ",")
    expect_equal(cfg$order, "lonlat")
    expect_false(cfg$polygon)
  })
})

test_that("exemplo aplicado vale mesmo antes do navegador devolver o texto", {
  shiny::testServer(mod_input_server, {
    session$setInputs(tipo = "utm", coords = "", datum = "SIRGAS2000",
                      zona = "", dec_sep = "auto", ordem_utm = "auto",
                      ordem_geo = "auto", poligono = TRUE)
    expect_equal(settings()$text, "")

    session$setInputs(ex_utm = 1)
    expect_equal(settings()$text, gc_examples$utm)
    expect_equal(settings()$type, "utm")

    # ao digitar, o exemplo é descartado
    session$setInputs(coords = "24S 713110.66 8793023.16")
    expect_equal(settings()$text, "24S 713110.66 8793023.16")

    # limpar zera tudo
    session$setInputs(ex_dd = 1)
    expect_equal(settings()$text, gc_examples$dd)
    session$setInputs(limpar = 1)
    expect_equal(settings()$text, "")
  })
})

test_that("cada exemplo tem texto e opções coerentes", {
  for (id in c("utm", "utm_br", "dd", "gms", "sad69")) {
    spec <- gc_example_spec(id)
    expect_true(nzchar(spec$texto), info = id)
    expect_true(spec$tipo %in% c("utm", "geographic"), info = id)
    r <- gc_convert(spec$texto, type = spec$tipo, datum = spec$datum)
    expect_true(r$ok, info = paste("exemplo", id, gc_issues_text(r$issues)))
    expect_gte(nrow(r$points), 4L)
  }
  expect_equal(gc_example_spec("gms")$tipo, "geographic")
  expect_equal(gc_example_spec("sad69")$datum, "SAD69")
  expect_equal(gc_example_spec("inexistente")$tipo, "utm")
})

test_that("módulo de resultados processa um resultado válido", {
  skip_if_not(requireNamespace("DT", quietly = TRUE))
  res <- gc_convert(
    paste("-15.7939, -47.8825", "-15.7939, -47.8783", "-15.7903, -47.8783",
          sep = "\n"),
    type = "geographic"
  )
  shiny::testServer(mod_results_server, args = list(result = shiny::reactive(res)), {
    expect_match(paste(as.character(output$badge), collapse = ""), "aviso|erro|sem problemas")
    html <- as.character(output$faixa_resumo)
    expect_match(paste(html, collapse = ""), "Área")
    expect_match(paste(as.character(output$resumo), collapse = ""), "Datum de saída")
    expect_match(paste(as.character(output$tecnico), collapse = ""), "EPSG")
  })
})

test_that("payload do mapa tem pontos e anel fechado", {
  res <- gc_convert(
    paste("-15.7939, -47.8825", "-15.7939, -47.8783", "-15.7903, -47.8783",
          sep = "\n"),
    type = "geographic"
  )
  p <- gc_map_payload(res)
  expect_equal(length(p$points), 3)
  expect_equal(p$points[[1]]$label, "P1")
  expect_true(is.numeric(p$points[[1]]$lat))
  expect_match(p$points[[1]]$popup, "Latitude")
  expect_true(p$has_polygon)
  expect_equal(length(p$polygon), 4)  # 3 pontos + fechamento
  expect_equal(p$polygon[[1]], p$polygon[[4]])
})

test_that("payload do mapa lida com resultado sem pontos", {
  vazio <- gc_convert("", type = "utm")
  p <- gc_map_payload(vazio)
  expect_equal(length(p$points), 0)
  expect_equal(length(p$polygon), 0)
})
