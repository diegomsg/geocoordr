# =============================================================================
# Testes: sistemas de referência (data, elipsóides, zonas, EPSG)
# =============================================================================

test_that("data brasileiros são reconhecidos por apelidos", {
  expect_equal(gc_datum_key("SIRGAS 2000"), "SIRGAS2000")
  expect_equal(gc_datum_key("sirgas2000"), "SIRGAS2000")
  expect_equal(gc_datum_key("WGS 84"), "WGS84")
  expect_equal(gc_datum_key("SAD 69"), "SAD69")
  expect_equal(gc_datum_key("SAD69(96)"), "SAD6996")
  expect_equal(gc_datum_key("Córrego Alegre 1970-72"), "CORREGO_ALEGRE_1970")
  expect_equal(gc_datum_key("Corrego Alegre 1961"), "CORREGO_ALEGRE_1961")
  expect_true(is.na(gc_datum_key("Datum Inventado")))
})

test_that("erro de datum desconhecido lista as opções válidas", {
  expect_error(gc_datum("XYZ"), "Opções válidas")
})

test_that("elipsóides têm os parâmetros oficiais", {
  grs80 <- gc_ellipsoid("SIRGAS2000")
  expect_equal(grs80$a, 6378137)
  expect_equal(grs80$rf, 298.257222101)
  expect_equal(grs80$e2, 0.0066943800229034, tolerance = 1e-13)

  sad69 <- gc_ellipsoid("SAD69")
  expect_equal(sad69$a, 6378160)
  expect_equal(sad69$rf, 298.25)

  intl <- gc_ellipsoid("CORREGO_ALEGRE_1970")
  expect_equal(intl$a, 6378388)
  expect_equal(intl$rf, 297)
})

test_that("códigos EPSG de UTM estão corretos", {
  expect_equal(gc_utm_epsg("SIRGAS2000", 23, "S"), 31983L)
  expect_equal(gc_utm_epsg("SIRGAS2000", 24, "S"), 31984L)
  expect_equal(gc_utm_epsg("SIRGAS2000", 17, "S"), 31977L)
  expect_equal(gc_utm_epsg("SIRGAS2000", 22, "N"), 31976L)
  expect_equal(gc_utm_epsg("SIRGAS2000", 24, "N"), 6211L)
  expect_equal(gc_utm_epsg("WGS84", 23, "S"), 32723L)
  expect_equal(gc_utm_epsg("WGS84", 23, "N"), 32623L)
  expect_equal(gc_utm_epsg("SAD69", 23, "S"), 29193L)
  expect_equal(gc_utm_epsg("SAD69", 18, "N"), 29168L)
  expect_equal(gc_utm_epsg("CORREGO_ALEGRE_1970", 23, "S"), 22523L)
  expect_equal(gc_utm_epsg("CORREGO_ALEGRE_1961", 23, "S"), 5538L)
  # combinação sem código oficial
  expect_true(is.na(gc_utm_epsg("CORREGO_ALEGRE_1961", 23, "N")))
})

test_that("EPSG geográfico corresponde ao datum", {
  expect_equal(gc_geographic_epsg("SIRGAS2000"), 4674)
  expect_equal(gc_geographic_epsg("WGS84"), 4326)
  expect_equal(gc_geographic_epsg("SAD69"), 4618)
  expect_equal(gc_geographic_epsg("CORREGO_ALEGRE_1970"), 4225)
})

test_that("zonas são derivadas da longitude", {
  expect_equal(gc_utm_zone_from_lon(-47.8825), 23L)
  expect_equal(gc_utm_zone_from_lon(-37.05), 24L)
  expect_equal(gc_utm_zone_from_lon(-60.02), 20L)
  expect_equal(gc_utm_zone_from_lon(-59.99), 21L)
  expect_equal(gc_utm_zone_from_lon(-70.5), 19L)
  expect_equal(gc_utm_zone_from_lon(-48), 23L)  # limite pertence à zona leste
  expect_equal(gc_utm_zone_from_lon(-51), 22L)
  expect_equal(gc_utm_zone_from_lon(180), 60L)
  expect_equal(gc_utm_zone_from_lon(-180), 1L)
})

test_that("meridiano central e hemisfério", {
  expect_equal(gc_utm_central_meridian(23), -45)
  expect_equal(gc_utm_central_meridian(24), -39)
  expect_equal(gc_hemisphere_from_lat(5), "N")
  expect_equal(gc_hemisphere_from_lat(-15), "S")
  expect_equal(gc_hemisphere_from_lat(0), "N")
})

test_that("zona informada como texto é interpretada", {
  z1 <- gc_parse_zone("24S")
  expect_equal(z1$zone, 24L)
  expect_equal(z1$hemisphere, "S")

  z2 <- gc_parse_zone("Zona 23 Sul")
  expect_equal(z2$zone, 23L)
  expect_equal(z2$hemisphere, "S")

  z3 <- gc_parse_zone("fuso 22 norte")
  expect_equal(z3$zone, 22L)
  expect_equal(z3$hemisphere, "N")

  z4 <- gc_parse_zone("")
  expect_true(is.na(z4$zone))

  z5 <- gc_parse_zone("99S")
  expect_true(is.na(z5$zone))
})

test_that("PROJ.4 é gerado com o datum e a zona corretos", {
  p <- gc_proj4("SIRGAS2000", 24, "S")
  expect_match(p, "\\+proj=utm")
  expect_match(p, "\\+zone=24")
  expect_match(p, "\\+south")
  expect_match(p, "\\+ellps=GRS80")
  expect_match(p, "\\+towgs84=0,0,0")

  sad <- gc_proj4("SAD69", 23, "S")
  expect_match(sad, "\\+ellps=aust_SA")
  expect_match(sad, "\\+towgs84=-67.35,3.88,-38.22")
})

test_that("seletores de datum trazem rótulos legíveis", {
  ch <- gc_datum_choices()
  expect_true("SIRGAS2000" %in% unname(ch))
  expect_match(names(ch)[ch == "SIRGAS2000"], "oficial no Brasil")
})
