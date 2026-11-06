# =============================================================================
# Testes: leitura de números e ângulos (incluindo GMS)
# =============================================================================

test_that("números com ponto decimal são lidos corretamente", {
  expect_equal(gc_parse_number("713110.66")$value, 713110.66)
  expect_equal(gc_parse_number("8793023.16")$value, 8793023.16)
  expect_equal(gc_parse_number("-47.8825")$value, -47.8825)
  expect_equal(gc_parse_number("+15.5")$value, 15.5)
  expect_equal(gc_parse_number("0.5")$value, 0.5)
})

test_that("números no padrão brasileiro (vírgula decimal) são lidos", {
  res <- gc_parse_number("713110,66")
  expect_true(res$ok)
  expect_equal(res$value, 713110.66)
  expect_equal(res$dec_sep, ",")

  # vírgula decimal com 3 casas: lê como decimal, mas avisa
  res3 <- gc_parse_number("1,234")
  expect_true(res3$ok)
  expect_equal(res3$value, 1.234)
  expect_true("num_milhar_suspeito" %in% gc_codes(res3$issues))
})

test_that("separador de milhar é interpretado e sinalizado", {
  mil <- gc_parse_number("713.110,66")
  expect_true(mil$ok)
  expect_equal(mil$value, 713110.66)
  expect_true(mil$thousands)

  mil2 <- gc_parse_number("8.793.023,16")
  expect_equal(mil2$value, 8793023.16)

  mil3 <- gc_parse_number("1,234,567.89")
  expect_equal(mil3$value, 1234567.89)

  mil4 <- gc_parse_number("1.234")
  expect_true("num_milhar_suspeito" %in% gc_codes(mil4$issues))
})

test_that("separador explícito resolve ambiguidade", {
  # usuário escolheu ponto como decimal: a vírgula passa a ser milhar
  res <- gc_parse_number("713,110", dec_sep = ".")
  expect_equal(res$value, 713110)
  expect_true("num_milhar_interpretado" %in% gc_codes(res$issues))

  # usuário escolheu vírgula: o ponto é milhar
  res2 <- gc_parse_number("713.110", dec_sep = ",")
  expect_equal(res2$value, 713110)
})

test_that("entradas numéricas inválidas geram erros descritivos", {
  vazio <- gc_parse_number("")
  expect_false(vazio$ok)
  expect_equal(gc_codes(vazio$issues), "num_vazio")

  letras <- gc_parse_number("713110m")
  expect_false(letras$ok)
  expect_equal(gc_codes(letras$issues), "num_caractere_invalido")
  expect_match(gc_texts(letras$issues), "números")

  ambiguo <- gc_parse_number("1.234,56.7")
  expect_false(ambiguo$ok)
  expect_equal(gc_codes(ambiguo$issues), "num_separadores_ambiguos")

  repetido <- gc_parse_number("1..234")
  expect_false(repetido$ok)

  gigante <- gc_parse_number("1234567890123456")
  expect_false(gigante$ok)
  expect_equal(gc_codes(gigante$issues), "num_muito_grande")
})

test_that("mensagens de erro citam o valor e sugerem o que fazer", {
  res <- gc_parse_number("71x.110")
  txt <- gc_texts(res$issues)
  expect_match(txt, "71x.110")
  expect_true(!is.na(res$issues$hint[1]))
})

test_that("hemisfério é extraído de várias notações", {
  expect_equal(gc_extract_hemisphere("15 S")$letters, "S")
  expect_equal(gc_extract_hemisphere("15S")$letters, "S")
  expect_equal(gc_extract_hemisphere("S 15")$letters, "S")
  expect_equal(gc_extract_hemisphere("47 O")$letters, "W")
  expect_equal(gc_extract_hemisphere("47 L")$letters, "E")
  expect_equal(gc_extract_hemisphere("47 W")$letters, "W")
  expect_equal(gc_extract_hemisphere("15 Sul")$letters, "S")
  expect_equal(gc_extract_hemisphere("47 Oeste")$letters, "W")
  expect_equal(gc_extract_hemisphere("47 Leste")$letters, "E")
})

test_that("hemisfério contraditório na mesma coordenada é erro", {
  res <- gc_extract_hemisphere("15 N S")
  expect_equal(gc_codes(res$issues), "hemi_conflito")
})

test_that("ângulos em graus decimais", {
  expect_equal(gc_parse_angle("-15.793889", "lat")$value, -15.793889)
  expect_equal(gc_parse_angle("15.793889 S", "lat")$value, -15.793889)
  expect_equal(gc_parse_angle("15,793889 S", "lat")$value, -15.793889)
  expect_equal(gc_parse_angle("47.882778 O", "lon")$value, -47.882778)
  expect_equal(gc_parse_angle("47.882778 E", "lon")$value, 47.882778)
})

test_that("ângulos em graus e minutos", {
  expect_equal(gc_parse_angle("15°47,5'S", "lat")$value, -(15 + 47.5 / 60))
  expect_equal(gc_parse_angle("47 52,5 O", "lon")$value, -(47 + 52.5 / 60))
})

test_that("ângulos em graus, minutos e segundos", {
  alvo <- -(15 + 47 / 60 + 38 / 3600)
  expect_equal(gc_parse_angle("15°47'38\"S", "lat")$value, alvo, tolerance = 1e-12)
  expect_equal(gc_parse_angle("15 47 38 S", "lat")$value, alvo, tolerance = 1e-12)
  expect_equal(gc_parse_angle("15:47:38 S", "lat")$value, alvo, tolerance = 1e-12)
  expect_equal(gc_parse_angle("15d47m38s S", "lat")$value, alvo, tolerance = 1e-12)
  expect_equal(gc_parse_angle("15 graus 47 minutos 38 segundos S", "lat")$value,
               alvo, tolerance = 1e-12)
  expect_equal(gc_parse_angle("15°47'38,5\"S", "lat")$value,
               -(15 + 47 / 60 + 38.5 / 3600), tolerance = 1e-12)
})

test_that("componentes de GMS inválidos são rejeitados com explicação", {
  min60 <- gc_parse_angle("15°60'00\"S", "lat")
  expect_false(min60$ok)
  expect_equal(gc_codes(min60$issues), "angulo_minuto_invalido")
  expect_match(gc_texts(min60$issues), "60 minutos = 1 grau")

  seg60 <- gc_parse_angle("15°47'60\"S", "lat")
  expect_false(seg60$ok)
  expect_equal(gc_codes(seg60$issues), "angulo_segundo_invalido")

  fora <- gc_parse_angle("95°00'00\"N", "lat")
  expect_false(fora$ok)
  expect_equal(gc_codes(fora$issues), "angulo_fora_do_limite")

  frac <- gc_parse_angle("15.5°47'00\"S", "lat")
  expect_false(frac$ok)
  expect_equal(gc_codes(frac$issues), "angulo_grau_fracionado")

  demais <- gc_parse_angle("15 47 38 12", "lat")
  expect_false(demais$ok)
  expect_equal(gc_codes(demais$issues), "angulo_componentes_demais")
})

test_that("formatação GMS é legível e reversível", {
  txt <- gc_format_dms_axis(-15.793889, "lat")
  expect_match(txt, "^15°47'38,00\"S$")

  lon <- gc_format_dms_axis(-47.882778, "lon")
  expect_match(lon, "W$")

  expect_equal(gc_parse_angle(txt, "lat")$value, -15.793889, tolerance = 1e-7)
  expect_equal(gc_parse_angle(lon, "lon")$value, -47.882778, tolerance = 1e-7)

  # zero e valores exatos
  expect_equal(gc_format_dms_axis(0, "lat"), "0°00'00,00\"N")
  expect_equal(gc_format_dms_axis(-0.5, "lat"), "0°30'00,00\"S")
  # arredondamento que estoura 60 segundos
  expect_equal(gc_format_dms_axis(-10.999999999, "lat"), "11°00'00,00\"S")
  expect_equal(gc_format_dms_axis(NA_real_, "lat"), "—")
})
