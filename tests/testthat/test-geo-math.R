# =============================================================================
# Testes: projeção UTM, ECEF e transformação de datum
#
# Os valores de referência (<fixture>proj-reference.csv</fixture>) foram
# gerados com o PROJ 9.8 (cs2cs/cct) — ver data-raw/gerar-fixtures-*.R.
# =============================================================================

ref <- fixture("proj-reference.csv")

test_that("projeção direta (geográfico -> UTM) confere com o PROJ", {
  d <- ref[ref$op == "fwd", ]
  expect_gt(nrow(d), 10)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    out <- gc_geographic_to_utm(r$lon, r$lat, r$zone, r$hemi, "SIRGAS2000")
    expect_equal(out$easting, r$e, tolerance = 1e-3,
                 info = sprintf("ponto %d (%.4f, %.4f)", i, r$lon, r$lat))
    expect_equal(out$northing, r$n, tolerance = 1e-3)
  }
})

test_that("projeção inversa (UTM -> geográfico) confere com o PROJ", {
  d <- ref[ref$op == "inv", ]
  expect_gt(nrow(d), 10)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    out <- gc_utm_to_geographic(r$e, r$n, r$zone, r$hemi, "SIRGAS2000")
    expect_equal(out$lon, r$lon, tolerance = 1e-8)
    expect_equal(out$lat, r$lat, tolerance = 1e-8)
  }
})

test_that("transformação de datum confere com o pipeline do PROJ", {
  d <- ref[ref$op == "datum-fwd", ]
  expect_gt(nrow(d), 10)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    out <- gc_transform_datum(r$lon, r$lat, from = r$datum, to = "SIRGAS2000")
    expect_equal(out$lon, r$e, tolerance = 1e-8,
                 info = paste("datum", r$datum))
    expect_equal(out$lat, r$n, tolerance = 1e-8)
  }

  di <- ref[ref$op == "datum-inv", ]
  for (i in seq_len(nrow(di))) {
    r <- di[i, ]
    out <- gc_transform_datum(r$lon, r$lat, from = "SIRGAS2000", to = r$datum)
    expect_equal(out$lon, r$e, tolerance = 1e-8)
    expect_equal(out$lat, r$n, tolerance = 1e-8)
  }
})

test_that("ida e volta UTM -> geográfico é estável", {
  lon <- c(-74, -70, -60.5, -51.2, -47.8825, -39, -35, -28)
  lat <- c(-33.5, -23.5, -15.8, -3.1, 0.5, 4.5, -8, -30)
  for (i in seq_along(lon)) {
    zone <- gc_utm_zone_from_lon(lon[i])
    hemi <- gc_hemisphere_from_lat(lat[i])
    utm <- gc_geographic_to_utm(lon[i], lat[i], zone, hemi, "SIRGAS2000")
    back <- gc_utm_to_geographic(utm$easting, utm$northing, zone, hemi,
                                 "SIRGAS2000")
    expect_equal(back$lon, lon[i], tolerance = 1e-10)
    expect_equal(back$lat, lat[i], tolerance = 1e-10)
  }
})

test_that("false northing sul e norte estão corretos", {
  # ponto praticamente no equador, hemisfério sul -> N ~ 10.000.000
  sul <- gc_geographic_to_utm(-45, -0.000001, 23, "S", "SIRGAS2000")
  expect_equal(sul$northing, 10000000, tolerance = 0.5)
  # no hemisfério norte o equador vale 0
  norte <- gc_geographic_to_utm(-45, 0.000001, 23, "N", "SIRGAS2000")
  expect_equal(norte$northing, 0, tolerance = 0.5)
  # meridiano central: E = 500.000
  mc <- gc_geographic_to_utm(-45, -15, 23, "S", "SIRGAS2000")
  expect_equal(mc$easting, 500000, tolerance = 1e-6)
})

test_that("ECEF ida e volta é consistente", {
  ecef <- gc_geodetic_to_ecef(-47.8825, -15.7939, 1100)
  back <- gc_ecef_to_geodetic(ecef$x, ecef$y, ecef$z)
  expect_equal(back$lon, -47.8825, tolerance = 1e-10)
  expect_equal(back$lat, -15.7939, tolerance = 1e-10)
  expect_equal(back$h, 1100, tolerance = 1e-6)
})

test_that("Helmert reversível e sem efeito quando identidade", {
  p <- list(x = -67.35, y = 3.88, z = -38.22, rx = 0, ry = 0, rz = 0, s = 0)
  ida <- gc_helmert(1000000, 2000000, 3000000, p)
  volta <- gc_helmert(ida$x, ida$y, ida$z, p, inverse = TRUE)
  expect_equal(volta$x, 1000000, tolerance = 1e-6)
  expect_equal(volta$y, 2000000, tolerance = 1e-6)
  expect_equal(volta$z, 3000000, tolerance = 1e-6)
})

test_that("SIRGAS 2000 -> SIRGAS 2000 não altera nada", {
  out <- gc_transform_datum(-47.8825, -15.7939, "SIRGAS2000", "SIRGAS2000")
  expect_equal(out$lon, -47.8825)
  expect_equal(out$lat, -15.7939)
})

test_that("SAD 69 difere do SIRGAS 2000 na ordem de dezenas de metros", {
  out <- gc_transform_datum(-47.8825, -15.7939, "SAD69", "SIRGAS2000")
  # ordem de grandeza esperada (~70 m no Brasil central)
  expect_gt(abs(out$lat + 15.7939) * 110574, 20)
  expect_lt(abs(out$lat + 15.7939) * 110574, 200)
})

test_that("fator de escala UTM é coerente", {
  k_mc <- gc_utm_scale_factor(-45, -15, 23, "S", "SIRGAS2000")
  expect_equal(k_mc, 0.9996, tolerance = 1e-4)

  k_borda <- gc_utm_scale_factor(-42, -15, 23, "S", "SIRGAS2000")
  expect_gt(k_borda, k_mc)
  expect_lt(k_borda, 1.0010)
})

test_that("longitudes no limite da zona não explodem", {
  # diferença de longitude > 180 (zona do outro lado do antimeridiano)
  out <- gc_tm_forward(lon = 179, lat = 0, lon0 = -179, k0 = 0.9996,
                       fe = 500000, fn = 0, ellps = gc_ellipsoid("SIRGAS2000"))
  expect_true(is.finite(out$x))
  expect_true(is.finite(out$y))
})
