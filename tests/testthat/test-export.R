# =============================================================================
# Testes: exportação (CSV, GeoJSON, KML, metadados)
# =============================================================================

res_teste <- function(datum = "SIRGAS2000") {
  gc_convert(
    paste(
      "P1 24S 713110.66 8793023.16",
      "P2 24S 713550.20 8793023.16",
      "P3 24S 713550.20 8793423.16",
      "P4 24S 713110.66 8793423.16",
      sep = "\n"
    ),
    type = "utm", datum = datum
  )
}

test_that("tabela de exportação tem as colunas esperadas", {
  r <- res_teste()
  tab <- gc_export_table(r)
  expect_equal(nrow(tab), 4)
  expect_equal(
    names(tab),
    c("Ponto", "Latitude", "Longitude", "Latitude (GMS)", "Longitude (GMS)",
      "E (m)", "N (m)", "Zona UTM", "EPSG", "Datum de entrada",
      "Texto de origem")
  )
  expect_equal(tab$Ponto, c("P1", "P2", "P3", "P4"))
  expect_equal(tab$`Zona UTM`, rep("24S", 4))
  expect_equal(tab$EPSG, rep(31984L, 4))
})

test_that("CSV brasileiro usa ponto e vírgula, vírgula decimal e BOM", {
  r <- res_teste()
  csv <- gc_csv_br(gc_export_table(r))
  expect_true(startsWith(csv, "\ufeff"))
  expect_match(sub("^\ufeff", "", csv), "^Ponto;Latitude;Longitude")
  expect_match(csv, "-10,91237", fixed = TRUE)
  expect_match(csv, "713110,66", fixed = TRUE)
  expect_equal(length(strsplit(csv, "\r\n")[[1]]), 5L)  # cabecalho + 4 linhas
})

test_that("CSV internacional usa vírgula e ponto decimal, sem BOM", {
  r <- res_teste()
  csv <- gc_csv_intl(gc_export_table(r))
  expect_false(startsWith(csv, "\ufeff"))
  expect_match(csv, "^Ponto,Latitude,Longitude")
  expect_match(csv, "-10.912", fixed = TRUE)
})

test_that("CSV escapa textos com o separador e aspas", {
  df <- data.frame(a = "nome, com vírgula", b = 'aspas "duplas"',
                   stringsAsFactors = FALSE)
  csv <- gc_csv_string(df, sep = ",", dec = ".", bom = FALSE)
  expect_match(csv, "\"nome, com vírgula\"", fixed = TRUE)
  expect_match(csv, "\"aspas \"\"duplas\"\"\"", fixed = TRUE)

  # com ponto e vírgula como separador, a vírgula não precisa de aspas
  csv2 <- gc_csv_string(df, sep = ";", dec = ",", bom = FALSE)
  expect_match(csv2, "nome, com vírgula;", fixed = TRUE)
})

test_that("CSV vazio devolve string vazia", {
  expect_equal(gc_csv_br(data.frame()), "")
  expect_equal(gc_csv_intl(data.frame()), "")
})

test_that("GeoJSON é válido, fechado e tem um feature por ponto + polígono", {
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  r <- res_teste()
  gj <- gc_geojson_string(r)
  obj <- jsonlite::fromJSON(gj, simplifyVector = FALSE)
  expect_equal(obj$type, "FeatureCollection")
  expect_equal(length(obj$features), 5L)
  expect_equal(obj$features[[1]]$geometry$type, "Point")
  expect_equal(obj$features[[1]]$properties$nome, "P1")
  expect_equal(length(obj$features[[1]]$geometry$coordinates), 2)
  # longitude, latitude
  expect_lt(obj$features[[1]]$geometry$coordinates[[1]], 0)
  expect_lt(obj$features[[1]]$geometry$coordinates[[2]], 0)

  pol <- obj$features[[5]]
  expect_equal(pol$geometry$type, "Polygon")
  anel <- pol$geometry$coordinates[[1]]
  expect_equal(length(anel), 5L)
  expect_equal(anel[[1]], anel[[5]])
  expect_equal(pol$properties$epsg, 31984)
})

test_that("GeoJSON de ponto isolado não inclui polígono", {
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  r <- gc_convert("-15.793889, -47.882778", type = "geographic")
  obj <- jsonlite::fromJSON(gc_geojson_string(r), simplifyVector = FALSE)
  expect_equal(length(obj$features), 1L)
})

test_that("KML contém placemarks, coordenadas e nome do datum", {
  r <- res_teste()
  kml <- gc_kml_string(r)
  expect_match(kml, "<kml xmlns=\"http://www.opengis.net/kml/2.2\">", fixed = TRUE)
  expect_match(kml, "<Placemark>", fixed = TRUE)
  expect_match(kml, "<coordinates>", fixed = TRUE)
  expect_equal(lengths(regmatches(kml, gregexpr("<Placemark>", kml))), 5L)
  expect_match(kml, "Perímetro")
  expect_match(kml, "SIRGAS 2000")
})

test_that("KML e GeoJSON escapam caracteres especiais", {
  r <- res_teste()
  r$points$ponto[1] <- "P1 & <V>"
  kml <- gc_kml_string(r)
  expect_match(kml, "P1 &amp; &lt;V&gt;", fixed = TRUE)
  expect_false(grepl("<V>", kml, fixed = TRUE))

  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  obj <- jsonlite::fromJSON(gc_geojson_string(r), simplifyVector = FALSE)
  expect_equal(obj$features[[1]]$properties$nome, "P1 & <V>")
})

test_that("lados e metadados são exportáveis", {
  r <- res_teste()
  lados <- gc_export_sides(r)
  expect_equal(names(lados), c("De", "Para", "Distância (m)", "Azimute (°)"))
  expect_equal(nrow(lados), 4)

  meta <- gc_export_metadata(r)
  expect_true(all(c("item", "valor") %in% names(meta)))
  expect_true(any(meta$item == "EPSG"))
  expect_equal(meta$valor[meta$item == "EPSG"], "31984")
  expect_equal(meta$valor[meta$item == "Datum de saída"], "SIRGAS 2000")
})

test_that("formato de arquivo é versionado e datado", {
  nome <- gc_filename("csv")
  expect_match(nome, "^geocoordR-[0-9]{8}-[0-9]{6}\\.csv$")
  expect_match(gc_filename("geojson", prefix = "x"), "^x-.*\\.geojson$")
})

test_that("sf é opcional e falha com mensagem amigável quando ausente", {
  skip_if(gc_sf_available(), "sf instalado neste ambiente")
  expect_error(gc_to_sf(res_teste()), "sf")
})
