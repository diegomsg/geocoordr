# =============================================================================
# geocoordR :: matemática geodésica
#
# Implementação em R puro (sem sf/GDAL/PROJ) de:
#   * projeção Transversa de Mercator (UTM), séries de Snyder (USGS PP 1395);
#   * conversão geodésico <-> geocêntrico (ECEF);
#   * transformação de datum por Helmert (7 parâmetros, translação no caso
#     dos datums brasileiros).
#
# A motivação de não depender de sf/PROJ aqui é dupla: (1) o app funciona em
# qualquer máquina com R, inclusive shinyapps.io sem GDAL; (2) a matemática
# fica testável de forma determinística, comparada contra o PROJ nos testes
# (tests/fixtures/proj-reference.csv).
#
# Precisão esperada: erro < 1 mm na projeção dentro da zona UTM; dezenas de
# centímetros na transformação de datum (a incerteza real é a do datum antigo,
# ~5 m, informada ao usuário).
# =============================================================================

# -----------------------------------------------------------------------------
# Constantes UTM
# -----------------------------------------------------------------------------
GC_UTM_K0 <- 0.9996
GC_UTM_FALSE_EASTING <- 500000
GC_UTM_FALSE_NORTHING_SOUTH <- 10000000

#' Zona UTM (1-60) a partir da longitude
gc_utm_zone_from_lon <- function(lon) {
  zone <- floor((lon + 180) / 6) + 1
  zone <- pmin(pmax(zone, 1), 60)
  as.integer(zone)
}

#' Meridiano central de uma zona UTM
gc_utm_central_meridian <- function(zone) {
  -183 + 6 * as.numeric(zone)
}

#' Hemisfério a partir da latitude
gc_hemisphere_from_lat <- function(lat) {
  ifelse(lat >= 0, "N", "S")
}

#' Parâmetros de projeção de uma zona UTM
gc_utm_params <- function(zone, hemisphere) {
  hemi <- ifelse(toupper(substr(hemisphere, 1, 1)) == "S", "S", "N")
  list(
    lon0 = gc_utm_central_meridian(zone),
    k0 = GC_UTM_K0,
    fe = GC_UTM_FALSE_EASTING,
    fn = ifelse(hemi == "S", GC_UTM_FALSE_NORTHING_SOUTH, 0)
  )
}

# -----------------------------------------------------------------------------
# Transversa de Mercator (Snyder, USGS Professional Paper 1395, eq. 8-9 a 8-17)
# -----------------------------------------------------------------------------

#' Meridiano arco (distância ao equador) em metros
gc_tm_meridian_arc <- function(phi, ellps) {
  e2 <- ellps$e2
  e4 <- e2^2
  e6 <- e2^3
  a <- ellps$a
  a * (
    (1 - e2 / 4 - 3 * e4 / 64 - 5 * e6 / 256) * phi -
      (3 * e2 / 8 + 3 * e4 / 32 + 45 * e6 / 1024) * sin(2 * phi) +
      (15 * e4 / 256 + 45 * e6 / 1024) * sin(4 * phi) -
      (35 * e6 / 3072) * sin(6 * phi)
  )
}

#' Geodésico -> Transversa de Mercator
#'
#' @return data.frame(x, y) em metros.
gc_tm_forward <- function(lon, lat, lon0, k0 = GC_UTM_K0,
                          fe = GC_UTM_FALSE_EASTING, fn = 0,
                          ellps = gc_ellipsoid("SIRGAS2000")) {
  e2 <- ellps$e2
  ep2 <- e2 / (1 - e2)
  phi <- lat * pi / 180
  # diferença de longitude normalizada em [-180, 180)
  dlam <- ((lon - lon0 + 180) %% 360) - 180
  lam <- dlam * pi / 180

  sin_phi <- sin(phi)
  cos_phi <- cos(phi)
  tan_phi <- tan(phi)

  N <- ellps$a / sqrt(1 - e2 * sin_phi^2)
  T <- tan_phi^2
  C <- ep2 * cos_phi^2
  A <- lam * cos_phi
  M <- gc_tm_meridian_arc(phi, ellps)

  x <- k0 * N * (
    A +
      (1 - T + C) * A^3 / 6 +
      (5 - 18 * T + T^2 + 72 * C - 58 * ep2) * A^5 / 120
  )
  y <- k0 * (
    M + N * tan_phi * (
      A^2 / 2 +
        (5 - T + 9 * C + 4 * C^2) * A^4 / 24 +
        (61 - 58 * T + T^2 + 600 * C - 330 * ep2) * A^6 / 720
    )
  )

  data.frame(x = fe + x, y = fn + y)
}

#' Transversa de Mercator -> geodésico
#'
#' @return data.frame(lon, lat) em graus decimais.
gc_tm_inverse <- function(x, y, lon0, k0 = GC_UTM_K0,
                          fe = GC_UTM_FALSE_EASTING, fn = 0,
                          ellps = gc_ellipsoid("SIRGAS2000")) {
  e2 <- ellps$e2
  ep2 <- e2 / (1 - e2)
  a <- ellps$a

  M <- (y - fn) / k0
  mu <- M / (a * (1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256))
  e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))

  phi1 <- mu +
    (3 * e1 / 2 - 27 * e1^3 / 32) * sin(2 * mu) +
    (21 * e1^2 / 16 - 55 * e1^4 / 32) * sin(4 * mu) +
    (151 * e1^3 / 96) * sin(6 * mu) +
    (1097 * e1^4 / 512) * sin(8 * mu)

  sin_phi1 <- sin(phi1)
  cos_phi1 <- cos(phi1)
  tan_phi1 <- tan(phi1)

  C1 <- ep2 * cos_phi1^2
  T1 <- tan_phi1^2
  N1 <- a / sqrt(1 - e2 * sin_phi1^2)
  R1 <- a * (1 - e2) / (1 - e2 * sin_phi1^2)^(3 / 2)
  D <- (x - fe) / (N1 * k0)

  phi <- phi1 - (N1 * tan_phi1 / R1) * (
    D^2 / 2 -
      (5 + 3 * T1 + 10 * C1 - 4 * C1^2 - 9 * ep2) * D^4 / 24 +
      (61 + 90 * T1 + 298 * C1 + 45 * T1^2 - 252 * ep2 - 3 * C1^2) * D^6 / 720
  )

  lam <- (
    D -
      (1 + 2 * T1 + C1) * D^3 / 6 +
      (5 - 2 * C1 + 28 * T1 - 3 * C1^2 + 8 * ep2 + 24 * T1^2) * D^5 / 120
  ) / cos_phi1

  data.frame(
    lon = lon0 + lam * 180 / pi,
    lat = phi * 180 / pi
  )
}

# -----------------------------------------------------------------------------
# UTM (atalhos com zona/hemisfério)
# -----------------------------------------------------------------------------

#' UTM -> geográfico
gc_utm_to_geographic <- function(easting, northing, zone, hemisphere,
                                 datum = "SIRGAS2000") {
  n <- max(length(easting), length(northing), length(zone), length(hemisphere))
  if (n == 0L) {
    return(data.frame(lon = numeric(), lat = numeric()))
  }
  easting <- rep_len(as.numeric(easting), n)
  northing <- rep_len(as.numeric(northing), n)
  zone <- rep_len(as.integer(zone), n)
  hemisphere <- rep_len(as.character(hemisphere), n)
  ellps <- gc_ellipsoid(datum)

  p <- gc_utm_params(zone, hemisphere)
  out <- gc_tm_inverse(
    x = easting, y = northing,
    lon0 = p$lon0, k0 = p$k0, fe = p$fe, fn = p$fn,
    ellps = ellps
  )
  data.frame(
    lon = ((out$lon + 180) %% 360) - 180,
    lat = out$lat
  )
}

#' Geográfico -> UTM
gc_geographic_to_utm <- function(lon, lat, zone, hemisphere,
                                 datum = "SIRGAS2000") {
  n <- max(length(lon), length(lat), length(zone), length(hemisphere))
  if (n == 0L) {
    return(data.frame(easting = numeric(), northing = numeric()))
  }
  lon <- rep_len(as.numeric(lon), n)
  lat <- rep_len(as.numeric(lat), n)
  zone <- rep_len(as.integer(zone), n)
  hemi_in <- rep_len(as.character(hemisphere), n)
  hemi_in[is.na(hemi_in)] <- ""
  hemisphere <- ifelse(
    nzchar(hemi_in),
    toupper(substr(hemi_in, 1, 1)),
    gc_hemisphere_from_lat(lat)
  )
  ellps <- gc_ellipsoid(datum)

  p <- gc_utm_params(zone, hemisphere)
  out <- gc_tm_forward(
    lon = lon, lat = lat,
    lon0 = p$lon0, k0 = p$k0, fe = p$fe, fn = p$fn,
    ellps = ellps
  )
  data.frame(easting = out$x, northing = out$y)
}

# -----------------------------------------------------------------------------
# Geodésico <-> ECEF e transformação de datum
# -----------------------------------------------------------------------------

#' Geodésico (graus, metros) -> ECEF
gc_geodetic_to_ecef <- function(lon, lat, h = 0,
                                ellps = gc_ellipsoid("SIRGAS2000")) {
  phi <- lat * pi / 180
  lam <- lon * pi / 180
  e2 <- ellps$e2
  N <- ellps$a / sqrt(1 - e2 * sin(phi)^2)
  data.frame(
    x = (N + h) * cos(phi) * cos(lam),
    y = (N + h) * cos(phi) * sin(lam),
    z = (N * (1 - e2) + h) * sin(phi)
  )
}

#' ECEF -> geodésico (iterativo; converge em poucas iterações)
gc_ecef_to_geodetic <- function(x, y, z,
                                ellps = gc_ellipsoid("SIRGAS2000")) {
  a <- ellps$a
  e2 <- ellps$e2
  lon <- atan2(y, x)
  p <- sqrt(x^2 + y^2)
  # latitude inicial (Bowring)
  lat <- atan2(z, p * (1 - e2))
  for (i in 1:10) {
    N <- a / sqrt(1 - e2 * sin(lat)^2)
    h <- p / cos(lat) - N
    lat_new <- atan2(z, p * (1 - e2 * N / (N + h)))
    if (all(abs(lat_new - lat) < 1e-12, na.rm = TRUE)) {
      lat <- lat_new
      break
    }
    lat <- lat_new
  }
  N <- a / sqrt(1 - e2 * sin(lat)^2)
  h <- p / cos(lat) - N
  data.frame(lon = lon * 180 / pi, lat = lat * 180 / pi, h = h)
}

#' Helmert de 7 parâmetros (convenção "position vector")
#'
#' @param p lista com x, y, z (m), rx, ry, rz (segundos de arco) e s (ppm).
gc_helmert <- function(x, y, z, p, inverse = FALSE) {
  p <- as.list(p)
  tx <- p$x %||% 0
  ty <- p$y %||% 0
  tz <- p$z %||% 0
  rx <- (p$rx %||% 0) * pi / (180 * 3600)
  ry <- (p$ry %||% 0) * pi / (180 * 3600)
  rz <- (p$rz %||% 0) * pi / (180 * 3600)
  s <- 1 + (p$s %||% 0) * 1e-6
  if (inverse) {
    tx <- -tx
    ty <- -ty
    tz <- -tz
    rx <- -rx
    ry <- -ry
    rz <- -rz
    s <- 1 / s
  }
  data.frame(
    x = tx + s * (x - rz * y + ry * z),
    y = ty + s * (rz * x + y - rx * z),
    z = tz + s * (-ry * x + rx * y + z)
  )
}

#' Transforma coordenadas geográficas entre data
#'
#' A transformação passa pelo WGS 84 (pivô), usando os parâmetros de
#' translação publicados para cada datum brasileiro.
gc_transform_datum <- function(lon, lat, from = "SIRGAS2000", to = "SIRGAS2000",
                               h = 0) {
  from_key <- gc_datum_key(from)
  to_key <- gc_datum_key(to)
  if (is.na(from_key)) stop(gc_datum_error(from), call. = FALSE)
  if (is.na(to_key)) stop(gc_datum_error(to), call. = FALSE)

  n <- max(length(lon), length(lat))
  if (n == 0L) {
    return(data.frame(lon = numeric(), lat = numeric()))
  }
  lon <- rep_len(as.numeric(lon), n)
  lat <- rep_len(as.numeric(lat), n)
  h <- rep_len(as.numeric(h), n)

  if (from_key == to_key) {
    return(data.frame(lon = lon, lat = lat))
  }

  ecef <- gc_geodetic_to_ecef(lon, lat, h, gc_ellipsoid(from_key))
  # datum de origem -> WGS 84
  ecef <- gc_helmert(ecef$x, ecef$y, ecef$z, gc_helmert_params(gc_datums[[from_key]]))
  # WGS 84 -> datum de destino
  ecef <- gc_helmert(ecef$x, ecef$y, ecef$z,
                     gc_helmert_params(gc_datums[[to_key]]), inverse = TRUE)
  out <- gc_ecef_to_geodetic(ecef$x, ecef$y, ecef$z, gc_ellipsoid(to_key))
  data.frame(lon = out$lon, lat = out$lat)
}

#' Fator de escala local (k) e fator de elevação, para avisos de área
gc_utm_scale_factor <- function(lon, lat, zone, hemisphere,
                                datum = "SIRGAS2000") {
  ellps <- gc_ellipsoid(datum)
  p <- gc_utm_params(zone, hemisphere)
  e2 <- ellps$e2
  phi <- lat * pi / 180
  dlam <- (((lon - p$lon0 + 180) %% 360) - 180) * pi / 180
  ep2 <- e2 / (1 - e2)
  A <- dlam * cos(phi)
  T <- tan(phi)^2
  C <- ep2 * cos(phi)^2
  GC_UTM_K0 * (1 + (1 + C) * A^2 / 2 +
                  (5 - 4 * T + 42 * C + 13 * C^2 - 28 * ep2) * A^4 / 24)
}
