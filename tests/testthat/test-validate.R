# =============================================================================
# Testes: validações com mensagens descritivas
# =============================================================================

test_that("ponto fora do Brasil gera aviso apontando a linha", {
  pts <- gc_points_template()
  pts <- rbind(pts, data.frame(
    ponto = "P1", tipo = "GEO-DD", bruto = "48.85, 2.35", linha = 3L,
    lon = 2.35, lat = 48.85, e = NA_real_, n = NA_real_,
    zona = NA_integer_, hemisferio = "N", datum = "SIRGAS2000",
    stringsAsFactors = FALSE
  ))
  iss <- gc_validate_brazil(pts)
  expect_equal(gc_codes(iss), "fora_do_brasil")
  expect_equal(iss$line, 3L)
  expect_match(iss$hint, "hemisfério")
  expect_equal(iss$level, "aviso")
})

test_that("ponto dentro do Brasil não gera aviso", {
  pts <- gc_points_template()
  pts <- rbind(pts, data.frame(
    ponto = "P1", tipo = "GEO-DD", bruto = "-15.79, -47.88", linha = 1L,
    lon = -47.88, lat = -15.79, e = NA_real_, n = NA_real_,
    zona = NA_integer_, hemisferio = "S", datum = "SIRGAS2000",
    stringsAsFactors = FALSE
  ))
  expect_equal(nrow(gc_validate_brazil(pts)), 0L)
})

test_that("zona UTM divergente da longitude é avisada", {
  pts <- data.frame(
    ponto = "P1", linha = 1L, bruto = "22S 191170.91 8251746.35",
    lon = -47.8825, lat = -15.7939,
    zona_entrada = 22L, hemisferio_entrada = "S",
    stringsAsFactors = FALSE
  )
  iss <- gc_validate_utm_zone(pts)
  expect_equal(gc_codes(iss), "zona_divergente")
  expect_match(iss$message, "zona 23")
})

test_that("pontos repetidos são avisados", {
  pts <- data.frame(
    ponto = c("P1", "P2"), linha = c(1L, 2L),
    bruto = c("a", "b"), lon = c(-47.88, -47.88), lat = c(-15.79, -15.79),
    stringsAsFactors = FALSE
  )
  iss <- gc_validate_duplicates(pts)
  expect_equal(gc_codes(iss), "ponto_repetido")
  expect_equal(iss$line, 2L)
})

test_that("datum antigo gera aviso com a ordem de grandeza do erro", {
  iss <- gc_validate_datum("SAD69", "SIRGAS2000")
  expect_equal(gc_codes(iss), "datum_antigo")
  expect_match(iss$message, "5 m")
  expect_match(iss$hint, "SIRGAS 2000")

  iss2 <- gc_validate_datum("CORREGO_ALEGRE_1961", "SIRGAS2000")
  expect_match(iss2$message, "Córrego Alegre 1961")
  expect_match(iss2$message, "1970-72")

  expect_equal(nrow(gc_validate_datum("SIRGAS2000", "SIRGAS2000")), 0L)
})

test_that("WGS 84 recebe dica de equivalência com SIRGAS 2000", {
  iss <- gc_validate_datum("WGS84", "SIRGAS2000")
  expect_equal(gc_codes(iss), "wgs84_para_sirgas")
  expect_equal(iss$level, "dica")
})

test_that("zona de saída distante do meridiano central é avisada", {
  iss <- gc_validate_output_zone(-52, 23, "S")
  expect_equal(gc_codes(iss), "zona_saida_distorcida")
  expect_match(iss$message, "meridiano central")

  expect_equal(nrow(gc_validate_output_zone(-45, 23, "S")), 0L)
})

test_that("autointerseção do polígono é detectada", {
  # polígono em laço (bowtie)
  x <- c(0, 100, 0, 100)
  y <- c(0, 100, 100, 0)
  crosses <- gc_find_self_intersections(x, y)
  expect_gt(nrow(crosses), 0)

  iss <- gc_validate_polygon(x, y, c("P1", "P2", "P3", "P4"))
  expect_true("poligono_autointersecao" %in% gc_codes(iss))
  expect_match(gc_texts(iss), "se cruzam")
})

test_that("polígono simples não tem autointerseção", {
  x <- c(0, 100, 100, 0)
  y <- c(0, 0, 100, 100)
  expect_equal(nrow(gc_find_self_intersections(x, y)), 0L)
  expect_equal(nrow(gc_validate_polygon(x, y)), 0L)
})

test_that("polígono com menos de 3 pontos é avisado", {
  iss <- gc_validate_polygon(c(0, 1), c(0, 1), c("P1", "P2"))
  expect_equal(gc_codes(iss), "poligono_pontos_insuficientes")
  expect_match(iss$message, "3")
})

test_that("polígono com área nula é avisado", {
  iss <- gc_validate_polygon(c(0, 1, 2), c(0, 1, 2), c("P1", "P2", "P3"))
  expect_true("poligono_area_minima" %in% gc_codes(iss))
})

test_that("mensagens mantêm o texto original e o número da linha", {
  r <- gc_parse_coordinates("24S 713110.66\nlixo aqui", type = "utm")
  expect_equal(sort(unique(r$issues$line)), c(1L, 2L))
  expect_match(paste(r$issues$input, collapse = "|"), "lixo aqui")
  # toda mensagem tem um "o que fazer"
  expect_true(all(!is.na(r$issues$hint)))
})
