# =============================================================================
# Testes: leitura de memoriais (linha por linha)
# =============================================================================

parse_utm <- function(txt, ...) {
  gc_parse_coordinates(txt, type = "utm", ...)
}
parse_geo <- function(txt, ...) {
  gc_parse_coordinates(txt, type = "geographic", ...)
}

test_that("UTM simples com zona na linha", {
  r <- parse_utm("24S 713110.66 8793023.16\n24S 713500.20 8793500.10")
  expect_equal(nrow(r$points), 2)
  expect_false(gc_has_error(r$issues))
  expect_equal(r$points$e, c(713110.66, 713500.20))
  expect_equal(r$points$n, c(8793023.16, 8793500.10))
  expect_equal(r$points$zona, c(24L, 24L))
  expect_equal(r$points$hemisferio, c("S", "S"))
  expect_equal(r$points$ponto, c("P1", "P2"))
})

test_that("UTM aceita rótulos, separadores e padrão brasileiro", {
  casos <- c(
    "P1: 713110.66, 8793023.16",
    "P1 713110.66; 8793023.16",
    "P1 713.110,66 / 8.793.023,16",
    "P1 E 713110.66 N 8793023.16",
    "P1 713110,66 8793023,16"
  )
  for (txt in casos) {
    r <- parse_utm(txt, zone = 24, hemisphere = "S")
    expect_equal(nrow(r$points), 1L, info = txt)
    expect_equal(r$points$ponto, "P1", info = txt)
    expect_equal(r$points$e, 713110.66, tolerance = 1e-6, info = txt)
    expect_equal(r$points$n, 8793023.16, tolerance = 1e-6, info = txt)
  }
})

test_that("zona e hemisfério vêm do painel quando não estão no texto", {
  r <- parse_utm("P1 713110.66, 8793023.16", zone = 24, hemisphere = "S")
  expect_equal(nrow(r$points), 1)
  expect_equal(r$points$zona, 24L)
  expect_equal(r$points$hemisferio, "S")

  r2 <- parse_utm("P1 713110.66, 8793023.16", zone = "24S")
  expect_equal(r2$points$zona, 24L)
  expect_equal(r2$points$hemisferio, "S")
})

test_that("faltas de zona e hemisfério geram erros com instrução", {
  r <- parse_utm("713110.66, 8793023.16")
  expect_equal(nrow(r$points), 0)
  expect_equal(gc_codes(r$issues), "utm_sem_zona")
  expect_match(r$issues$hint[1], "zona")
  expect_match(gc_texts(r$issues), "24S")

  r2 <- parse_utm("24 713110.66 8793023.16")
  expect_equal(gc_codes(r2$issues), "utm_sem_hemisferio")
})

test_that("ordem N, E é detectada e avisada", {
  r <- parse_utm("N 8793023.16 E 713110.66", zone = 24, hemisphere = "S")
  expect_equal(r$points$e, 713110.66)
  expect_equal(r$points$n, 8793023.16)

  r2 <- parse_utm("8793023.16, 713110.66", zone = 24, hemisphere = "S")
  expect_equal(r2$points$e, 713110.66)
  expect_true("utm_ordem_detectada" %in% gc_codes(r2$issues))
})

test_that("ordem explícita é respeitada", {
  r <- parse_utm("8793023.16, 713110.66", zone = 24, hemisphere = "S", order = "NE")
  expect_equal(r$points$e, 713110.66)
  expect_equal(r$points$n, 8793023.16)

  # forçar ordem sem sentido gera erro de faixa, em vez de resultado errado
  r2 <- parse_utm("713110.66, 8793023.16", zone = 24, hemisphere = "S", order = "NE")
  expect_equal(nrow(r2$points), 0)
  expect_true("utm_e_fora_da_faixa" %in% gc_codes(r2$issues))
})

test_that("valores fora da faixa UTM são rejeitados", {
  r <- parse_utm("24S 1713110.66 8793023.16")
  expect_equal(gc_codes(r$issues), "utm_e_fora_da_faixa")
  expect_match(r$issues$hint[1], "ordem N, E")

  r2 <- parse_utm("24S 713110.66 18793023.16")
  expect_equal(gc_codes(r2$issues), "utm_n_fora_da_faixa")
})

test_that("valores incomuns geram aviso, não erro", {
  r <- parse_utm("24S 950000.00 8793023.16")
  expect_equal(nrow(r$points), 1)
  expect_true("utm_e_incomum" %in% gc_codes(r$issues))
})

test_that("linhas com quantidade errada de números explicam o problema", {
  r <- parse_utm("24S 713110.66")
  expect_equal(gc_codes(r$issues), "utm_dois_valores")
  expect_match(gc_texts(r$issues), "exatamente 2")

  r2 <- parse_utm("24S 713110.66 8793023.16 713500.00")
  expect_equal(gc_codes(r2$issues), "utm_dois_valores")
  expect_match(r2$issues$hint[1], "ponto e vírgula")
})

test_that("graus decimais com sinal", {
  r <- parse_geo("-15.793889, -47.882778\n-16.0 -48.0")
  expect_equal(nrow(r$points), 2)
  expect_equal(r$points$lat, c(-15.793889, -16))
  expect_equal(r$points$lon, c(-47.882778, -48))
  expect_false(gc_has_error(r$issues))
})

test_that("graus decimais com letras em qualquer ordem", {
  r <- parse_geo("15,793889 S / 47,882778 O")
  expect_equal(r$points$lat, -15.793889, tolerance = 1e-9)
  expect_equal(r$points$lon, -47.882778, tolerance = 1e-9)

  r2 <- parse_geo("47,882778 W 15,793889 S")
  expect_equal(r2$points$lat, -15.793889, tolerance = 1e-9)
  expect_equal(r2$points$lon, -47.882778, tolerance = 1e-9)
})

test_that("GMS com símbolos, espaços e dois-pontos", {
  alvo_lat <- -(15 + 47 / 60 + 38 / 3600)
  alvo_lon <- -(47 + 52 / 60 + 58 / 3600)
  casos <- c(
    "15°47'38\"S 47°52'58\"O",
    "15 47 38 S 47 52 58 O",
    "15:47:38 S 47:52:58 O",
    "15°47'38\"S, 47°52'58\"O",
    "S 15 47 38 W 47 52 58"
  )
  for (txt in casos) {
    r <- parse_geo(txt)
    expect_equal(nrow(r$points), 1L, info = txt)
    expect_equal(r$points$lat, alvo_lat, tolerance = 1e-9, info = txt)
    expect_equal(r$points$lon, alvo_lon, tolerance = 1e-9, info = txt)
    expect_match(r$points$tipo, "DMS", info = txt)
  }
})

test_that("sem letras, assume hemisfério sul/oeste do Brasil com aviso", {
  r <- parse_geo("15.7939, 47.8825")
  expect_equal(r$points$lat, -15.7939)
  expect_equal(r$points$lon, -47.8825)
  expect_true("geo_hemisferio_assumido" %in% gc_codes(r$issues))
  expect_true(all(r$issues$level == "aviso"))
})

test_that("longitude maior que 90 é reconhecida como longitude", {
  r <- parse_geo("47.8825, 15.7939")
  expect_equal(r$points$lat, -15.7939)
  expect_equal(r$points$lon, -47.8825)
})

test_that("erros de estrutura explicam como corrigir", {
  r <- parse_geo("abc def")
  expect_equal(gc_codes(r$issues), "geo_estrutura")
  expect_match(gc_texts(r$issues), "23°27'30")

  r2 <- parse_geo("15 S 47 S")
  expect_equal(gc_codes(r2$issues), "geo_mesmo_eixo")
})

test_that("latitude acima de 90 e longitude acima de 180 são rejeitadas", {
  r <- parse_geo("95.0, 47.0", order = "latlon")
  expect_true("geo_latitude_fora_da_faixa" %in% gc_codes(r$issues))

  r2 <- parse_geo("15.0, 190.0", order = "latlon")
  expect_true(any(grepl("fora_do_limite", gc_codes(r2$issues))))
})

test_that("detecção automática do tipo de entrada", {
  r <- gc_parse_coordinates("24S 713110.66 8793023.16", type = "auto")
  expect_equal(r$points$tipo, "UTM")

  r2 <- gc_parse_coordinates("-15.7939, -47.8825", type = "auto")
  expect_equal(nrow(r2$points), 1)
  expect_match(r2$points$tipo, "GEO")

  r3 <- gc_parse_coordinates("15°47'38\"S 47°52'58\"O", type = "auto")
  expect_match(r3$points$tipo, "GEO")
})

test_that("entrada vazia e datum inválido têm mensagens próprias", {
  r <- gc_parse_coordinates("", type = "utm")
  expect_equal(gc_codes(r$issues), "entrada_vazia")
  expect_match(r$issues$hint[1], "exemplo")

  r2 <- gc_parse_coordinates("24S 713110 8793023", type = "utm", datum = "XPTO")
  expect_equal(gc_codes(r2$issues), "datum_invalido")
})

test_that("pontos com rótulos repetidos são tornados únicos", {
  r <- parse_utm("P1 24S 713110.66 8793023.16\nP1 24S 713500.20 8793500.10")
  expect_equal(length(unique(r$points$ponto)), 2)
})

test_that("linha com dois pontos separados por ponto e vírgula é dividida", {
  r <- parse_utm("24S 713110.66 8793023.16 | 24S 713500.20 8793500.10")
  expect_equal(nrow(r$points), 2)
})

test_that("aceita vírgula e ponto como separadores de coordenadas", {
  for (sep in c(",", ";", "/", "|", " ")) {
    txt <- gsub(",", sep, "24S 713110.66,8793023.16")
    r <- parse_utm(txt)
    expect_equal(nrow(r$points), 1L, info = paste("separador", sep))
    expect_equal(r$points$e, 713110.66, tolerance = 1e-6)
    expect_equal(r$points$n, 8793023.16, tolerance = 1e-6)
  }
})

test_that("linhas vazias no meio do texto são ignoradas", {
  r <- parse_utm("24S 713110.66 8793023.16\n\n\n24S 713500.20 8793500.10\n")
  expect_equal(nrow(r$points), 2)
})
