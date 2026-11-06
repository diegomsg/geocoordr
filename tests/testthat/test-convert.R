# =============================================================================
# Testes: pipeline completo (gc_convert)
# =============================================================================

quadrado_utm <- paste(
  "P1 24S 713110.66 8793023.16",
  "P2 24S 713550.20 8793023.16",
  "P3 24S 713550.20 8793423.16",
  "P4 24S 713110.66 8793423.16",
  sep = "\n"
)

test_that("área e perímetro de um retângulo UTM conhecido", {
  r <- gc_convert(quadrado_utm, type = "utm")
  expect_true(r$ok)
  expect_equal(nrow(r$points), 4)
  # 439,54 m x 400 m
  expect_equal(r$summary$perimeter_m, 2 * (439.54 + 400), tolerance = 0.02)
  expect_equal(r$summary$area_m2, 439.54 * 400, tolerance = 5)
  expect_equal(r$summary$area_ha, r$summary$area_m2 / 10000)
  expect_equal(r$summary$self_intersections, 0L)
  expect_equal(nrow(r$polygon$sides), 4)
  expect_true(r$polygon$closed)
})

test_that("lados trazem distância e azimute coerentes", {
  r <- gc_convert(quadrado_utm, type = "utm")
  sides <- r$polygon$sides
  expect_equal(sides$de, c("P1", "P2", "P3", "P4"))
  expect_equal(sides$para, c("P2", "P3", "P4", "P1"))
  expect_equal(sides$distancia[1], 439.54, tolerance = 0.01)
  expect_equal(sides$azimute[1], 90, tolerance = 1e-6)
  expect_equal(sides$azimute[2], 0, tolerance = 1e-6)
  expect_equal(sides$azimute[3], 270, tolerance = 1e-6)
  expect_equal(sides$azimute[4], 180, tolerance = 1e-6)
})

test_that("a saída é sempre em SIRGAS 2000, na zona de entrada", {
  r <- gc_convert(quadrado_utm, type = "utm")
  expect_equal(r$meta$datum_saida, "SIRGAS2000")
  expect_equal(r$summary$zona, 24L)
  expect_equal(r$summary$hemisferio, "S")
  expect_equal(r$summary$epsg, 31984L)
  expect_match(r$summary$proj4, "\\+zone=24")
  expect_true(all(r$points$zona == 24L))
})

test_that("entrada em SAD 69 é convertida para SIRGAS 2000", {
  r_sad <- gc_convert(quadrado_utm, type = "utm", datum = "SAD69")
  r_sig <- gc_convert(quadrado_utm, type = "utm", datum = "SIRGAS2000")
  expect_true(r_sad$ok)
  # as coordenadas geográficas diferem; a área permanece quase igual
  expect_gt(abs(r_sad$points$lat[1] - r_sig$points$lat[1]), 1e-5)
  expect_equal(r_sad$summary$area_m2, r_sig$summary$area_m2, tolerance = 1)
  expect_true("datum_antigo" %in% gc_codes(r_sad$issues))
  # o E/N de entrada é preservado
  expect_equal(r_sad$points$e_entrada[1], 713110.66)
})

test_that("GMS, DD e UTM descrevem o mesmo imóvel", {
  r_utm <- gc_convert(quadrado_utm, type = "utm")
  # mesmos vértices em graus decimais e em GMS
  dd <- paste0(
    format(r_utm$points$lat, digits = 12), ", ",
    format(r_utm$points$lon, digits = 12), collapse = "\n"
  )
  r_dd <- gc_convert(dd, type = "geographic")
  expect_equal(r_dd$points$lat, r_utm$points$lat, tolerance = 1e-6)
  expect_equal(r_dd$summary$area_m2, r_utm$summary$area_m2, tolerance = 1)

  gms <- paste(r_utm$points$lat_dms, r_utm$points$lon_dms, collapse = "\n")
  r_gms <- gc_convert(gms, type = "geographic")
  expect_equal(r_gms$points$lat, r_utm$points$lat, tolerance = 1e-6)
  expect_equal(r_gms$summary$area_m2, r_utm$summary$area_m2, tolerance = 1)
})

test_that("mistura de UTM e graus de zonas diferentes é avisada", {
  txt <- paste(
    "P1 24S 713500.20 8793023.16",
    "-15.793889, -47.882778",
    "15°47'38\"S 47°52'58\"O",
    sep = "\n"
  )
  r <- gc_convert(txt, type = "auto")
  expect_true(r$ok)
  expect_equal(r$summary$zona, 23L)       # zona da área
  expect_equal(r$points$lat[2], -15.793889, tolerance = 1e-6)
  expect_true("pontos_em_zonas_diferentes" %in% gc_codes(r$issues))
})

test_that("zona de saída automática segue o centro da área", {
  r <- gc_convert(paste(
    "-15.7939, -47.8825", "-15.7900, -47.8800", "-15.7880, -47.8850",
    sep = "\n"
  ), type = "geographic")
  expect_equal(r$summary$zona, 23L)
  expect_equal(r$summary$hemisferio, "S")
})

test_that("zona de saída pode ser forçada", {
  r <- gc_convert(quadrado_utm, type = "utm", output_zone = 23)
  expect_equal(r$summary$zona, 23L)
  expect_equal(r$summary$epsg, 31983L)
  expect_true("zona_saida_distorcida" %in% gc_codes(r$issues))
})

test_that("polígono desligado não calcula área", {
  r <- gc_convert(quadrado_utm, type = "utm", polygon = FALSE)
  expect_true(r$ok)
  expect_true(is.na(r$summary$area_m2))
  expect_false(isTRUE(r$summary$polygon))
})

test_that("erros impedem o resultado e viram mensagens claras", {
  r <- gc_convert("24S 713110.66\nabc", type = "utm")
  expect_false(r$ok)
  expect_true(gc_has_error(r$issues))
  expect_true(all(!is.na(r$issues$hint)))
  expect_equal(nrow(r$points), 0L)

  r2 <- gc_convert("", type = "utm")
  expect_false(r2$ok)
  expect_equal(gc_codes(r2$issues), "entrada_vazia")
})

test_that("linha com erro não derruba as linhas boas", {
  txt <- paste(
    "P1 24S 713110.66 8793023.16",
    "P2 24S lixo 8793023.16",
    "P3 24S 713550.20 8793423.16",
    sep = "\n"
  )
  r <- gc_convert(txt, type = "utm")
  expect_equal(nrow(r$points), 2)
  expect_false(r$ok)
  expect_true("utm_dois_valores" %in% gc_codes(r$issues))
})

test_that("aviso não impede o resultado", {
  r <- gc_convert("24S 950000.00 8793023.16\n24S 950500.00 8793023.16\n24S 950500.00 8793500.16",
                  type = "utm")
  expect_true(r$ok)
  expect_false(gc_has_error(r$issues))
  expect_gt(nrow(r$issues), 0)
})

test_that("resumo textual é legível", {
  r <- gc_convert(quadrado_utm, type = "utm")
  texto <- gc_result_text(r)
  expect_match(texto, "Pontos: 4")
  expect_match(texto, "SIRGAS 2000")
  expect_match(texto, "Zona de saída: 24S")
  expect_match(texto, "Área:")
})

test_that("a área é calculada no plano UTM e não explode com datum antigo", {
  r <- gc_convert(quadrado_utm, type = "utm", datum = "CORREGO_ALEGRE_1970")
  expect_true(r$ok)
  expect_equal(r$summary$area_m2, 175816, tolerance = 200)
})

test_that("coordenadas muito distantes avisam sobre a zona", {
  r <- gc_convert(paste(
    "-15.0, -47.0", "-15.0, -47.01", "-15.01, -47.01",
    sep = "\n"
  ), type = "geographic")
  expect_true(r$ok)
  expect_equal(nrow(r$polygon$sides), 3)
})

test_that("ponto isolado é aceito (sem polígono)", {
  r <- gc_convert("-15.793889, -47.882778", type = "geographic", polygon = TRUE)
  expect_true(r$ok)
  expect_true("poligono_pontos_insuficientes" %in% gc_codes(r$issues))
  expect_false(isTRUE(r$summary$polygon))
  expect_equal(nrow(r$points), 1)
})

test_that("pontos repetidos no fim do memorial não quebram o polígono", {
  txt <- paste(quadrado_utm, "P1 24S 713110.66 8793023.16", sep = "\n")
  r <- gc_convert(txt, type = "utm")
  expect_true(r$ok)
  expect_true("ponto_repetido" %in% gc_codes(r$issues))
  expect_equal(r$summary$area_m2, 175816, tolerance = 5)
})
