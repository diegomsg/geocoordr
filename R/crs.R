# =============================================================================
# geocoordR :: sistemas de referência
#
# Todo o conhecimento sobre data e elipsóides vive aqui. Nada de números
# mágicos espalhados pelo código.
#
# Referências técnicas (usadas também nos tooltips do app):
#   * IBGE - Sistemas de Referência: https://www.ibge.gov.br/geociencias/geodesia
#   * EPSG Registry (SIRGAS 2000 / UTM): https://epsg.io/31983
#   * PROJ - transformações SAD69 -> SIRGAS 2000: https://epsg.io/15485
#   * INDE / Perfil de Metadados Geoespaciais do Brasil (CONCAR)
# =============================================================================

# -----------------------------------------------------------------------------
# Elipsóides
# -----------------------------------------------------------------------------
gc_ellipsoids <- list(
  GRS80 = list(
    name = "GRS 80",
    a = 6378137,
    rf = 298.257222101,
    info = "Elipsóide do SIRGAS 2000. Praticamente idêntico ao WGS 84."
  ),
  WGS84 = list(
    name = "WGS 84",
    a = 6378137,
    rf = 298.257223563,
    info = "Elipsóide do sistema GPS."
  ),
  SOUTH_AMERICAN_1969 = list(
    name = "South American 1969 (SAD69)",
    a = 6378160,
    rf = 298.25,
    info = "Elipsóide do SAD 69, usado no Brasil até 2005."
  ),
  INTERNATIONAL_1924 = list(
    name = "International 1924 (Hayford)",
    a = 6378388,
    rf = 297,
    info = "Elipsóide usado no Córrego Alegre."
  )
)

# -----------------------------------------------------------------------------
# Datums
# -----------------------------------------------------------------------------
#
# `helmert` são as translações (metros) do datum para o WGS 84, no sentido
# datum -> WGS 84 (convenção "geocentric translations", EPSG 9603).
# `accuracy_m` documenta a ordem de grandeza do erro da transformação.
gc_datums <- list(
  SIRGAS2000 = list(
    key = "SIRGAS2000",
    label = "SIRGAS 2000 — oficial no Brasil",
    short = "SIRGAS 2000",
    epsg = 4674,
    ellipsoid = "GRS80",
    helmert = c(x = 0, y = 0, z = 0),
    accuracy_m = 0,
    legacy = FALSE,
    info = "sirgas"
  ),
  WGS84 = list(
    key = "WGS84",
    label = "WGS 84 — padrão do GPS",
    short = "WGS 84",
    epsg = 4326,
    ellipsoid = "WGS84",
    helmert = c(x = 0, y = 0, z = 0),
    accuracy_m = 1,
    legacy = FALSE,
    info = "wgs84"
  ),
  SAD69 = list(
    key = "SAD69",
    label = "SAD 69 — antigo, comum em documentos rurais",
    short = "SAD 69",
    epsg = 4618,
    ellipsoid = "SOUTH_AMERICAN_1969",
    helmert = c(x = -67.35, y = 3.88, z = -38.22),
    accuracy_m = 5,
    legacy = TRUE,
    info = "sad69"
  ),
  SAD6996 = list(
    key = "SAD6996",
    label = "SAD 69(96) — variante ajustada do SAD 69",
    short = "SAD 69(96)",
    epsg = 5527,
    ellipsoid = "SOUTH_AMERICAN_1969",
    helmert = c(x = -67.35, y = 3.88, z = -38.22),
    accuracy_m = 5,
    legacy = TRUE,
    info = "sad69"
  ),
  CORREGO_ALEGRE_1970 = list(
    key = "CORREGO_ALEGRE_1970",
    label = "Córrego Alegre 1970-72 — antigo",
    short = "Córrego Alegre 1970-72",
    epsg = 4225,
    ellipsoid = "INTERNATIONAL_1924",
    helmert = c(x = -206.05, y = 168.28, z = -3.82),
    accuracy_m = 5,
    legacy = TRUE,
    info = "corrego"
  ),
  CORREGO_ALEGRE_1961 = list(
    key = "CORREGO_ALEGRE_1961",
    label = "Córrego Alegre 1961 — antigo",
    short = "Córrego Alegre 1961",
    epsg = 5524,
    ellipsoid = "INTERNATIONAL_1924",
    # Não existe transformação oficial de 3 parâmetros publicada pelo EPSG
    # para o Córrego Alegre 1961 (apenas grade oficial do IBGE). Usamos os
    # mesmos parâmetros aproximados do Córrego Alegre 1970-72.
    helmert = c(x = -206.05, y = 168.28, z = -3.82),
    accuracy_m = 10,
    legacy = TRUE,
    approx = TRUE,
    info = "corrego"
  )
)

gc_datum_aliases <- c(
  "SIRGAS2000" = "SIRGAS2000",
  "SIRGAS 2000" = "SIRGAS2000",
  "SIRGAS" = "SIRGAS2000",
  "SIRGAS2000 (BRASIL)" = "SIRGAS2000",
  "WGS84" = "WGS84",
  "WGS 84" = "WGS84",
  "WGS" = "WGS84",
  "SAD69" = "SAD69",
  "SAD 69" = "SAD69",
  "SAD-69" = "SAD69",
  "SAD69 (96)" = "SAD6996",
  "SAD 69(96)" = "SAD6996",
  "SAD6996" = "SAD6996",
  "SAD 69 96" = "SAD6996",
  "CORREGO ALEGRE" = "CORREGO_ALEGRE_1970",
  "CORREGO ALEGRE 1970" = "CORREGO_ALEGRE_1970",
  "CORREGO ALEGRE 1970-72" = "CORREGO_ALEGRE_1970",
  "CORREGO ALEGRE 1970/72" = "CORREGO_ALEGRE_1970",
  "CORREGO ALEGRE 1961" = "CORREGO_ALEGRE_1961"
)

#' Resolve a chave canônica de um datum (aceita apelidos e acentos)
gc_datum_key <- function(datum = "SIRGAS2000") {
  datum <- datum[1]
  if (is.null(datum) || !length(datum) || is.na(datum) || !nzchar(datum)) {
    return("SIRGAS2000")
  }
  key <- gc_norm(datum)[1]
  if (key %in% names(gc_datums)) {
    return(key)
  }
  if (key %in% names(gc_datum_aliases)) {
    return(unname(gc_datum_aliases[key]))
  }
  # tenta casar ignorando espaços/hífens
  squashed <- gsub("[^A-Z0-9]", "", key)
  for (alias in names(gc_datum_aliases)) {
    if (gsub("[^A-Z0-9]", "", alias) == squashed) {
      return(unname(gc_datum_aliases[alias]))
    }
  }
  NA_character_
}

#' Retorna a lista completa do datum (ou erro amigável)
gc_datum <- function(datum = "SIRGAS2000") {
  key <- gc_datum_key(datum)
  if (is.na(key)) {
    stop(gc_datum_error(datum), call. = FALSE)
  }
  gc_datums[[key]]
}

gc_datum_error <- function(datum) {
  paste0(
    "Datum '", paste(datum, collapse = ", "), "' não reconhecido. ",
    "Opções válidas: ", paste(vapply(gc_datums, `[[`, "", "short"),
                              collapse = ", "), "."
  )
}

#' Elipsóide de um datum
gc_ellipsoid <- function(datum = "SIRGAS2000") {
  d <- gc_datum(datum)
  e <- gc_ellipsoids[[d$ellipsoid]]
  e$e2 <- 2 * (1 / e$rf) - (1 / e$rf)^2
  e
}

#' Escolhas para os seletores do app (label amigável -> chave)
gc_datum_choices <- function(include_legacy = TRUE) {
  keys <- names(gc_datums)
  if (!include_legacy) {
    keys <- keys[!vapply(gc_datums[keys], function(d) isTRUE(d$legacy), logical(1))]
  }
  stats::setNames(keys, vapply(gc_datums[keys], `[[`, "", "label"))
}

#' EPSG do CRS geográfico
gc_geographic_epsg <- function(datum = "SIRGAS2000") {
  gc_datum(datum)$epsg
}

# -----------------------------------------------------------------------------
# EPSG de UTM (tabela verificada contra o registro EPSG/PROJ 9.x)
# -----------------------------------------------------------------------------
gc_utm_epsg_table <- list(
  SIRGAS2000 = list(
    N = c(
      "11" = 31965, "12" = 31966, "13" = 31967, "14" = 31968,
      "15" = 31969, "16" = 31970, "17" = 31971, "18" = 31972,
      "19" = 31973, "20" = 31974, "21" = 31975, "22" = 31976,
      "23" = 6210, "24" = 6211
    ),
    S = c(
      "17" = 31977, "18" = 31978, "19" = 31979, "20" = 31980,
      "21" = 31981, "22" = 31982, "23" = 31983, "24" = 31984,
      "25" = 31985, "26" = 5396
    )
  ),
  WGS84 = list(
    N = stats::setNames(32600 + 1:60, as.character(1:60)),
    S = stats::setNames(32700 + 1:60, as.character(1:60))
  ),
  SAD69 = list(
    N = c("17" = 5463, "18" = 29168, "19" = 29169, "20" = 29170,
          "21" = 29171, "22" = 29172),
    S = c("17" = 29187, "18" = 29188, "19" = 29189, "20" = 29190,
          "21" = 29191, "22" = 29192, "23" = 29193, "24" = 29194,
          "25" = 29195)
  ),
  SAD6996 = list(
    N = c(),
    S = c("18" = 5875, "19" = 5876, "20" = 5877, "21" = 5531,
          "22" = 5858, "23" = 5533, "24" = 5534, "25" = 5535)
  ),
  CORREGO_ALEGRE_1970 = list(
    N = c(),
    S = c("21" = 22521, "22" = 22522, "23" = 22523, "24" = 22524,
          "25" = 22525)
  ),
  CORREGO_ALEGRE_1961 = list(
    N = c(),
    S = c("21" = 5536, "22" = 5537, "23" = 5538, "24" = 5539)
  )
)

#' Código EPSG de uma combinação datum + zona + hemisfério (NA se não existir)
gc_utm_epsg <- function(datum, zone, hemisphere) {
  key <- gc_datum_key(datum)
  if (is.na(key)) {
    return(NA_integer_)
  }
  hemi <- gc_hemisphere(hemisphere)
  tab <- gc_utm_epsg_table[[key]][[hemi]]
  if (is.null(tab)) {
    return(NA_integer_)
  }
  code <- tab[as.character(as.integer(zone))]
  if (!length(code) || is.na(code)) {
    return(NA_integer_)
  }
  as.integer(code)
}

#' String PROJ.4 do CRS (útil em exportações e no painel técnico)
gc_proj4 <- function(datum = "SIRGAS2000", zone = NULL, hemisphere = NULL) {
  d <- gc_datum(datum)
  ellps <- switch(
    d$ellipsoid,
    GRS80 = "GRS80",
    WGS84 = "WGS84",
    SOUTH_AMERICAN_1969 = "aust_SA",
    INTERNATIONAL_1924 = "intl"
  )
  towgs84 <- paste(gc_helmert_params(d), collapse = ",")
  base <- paste0("+ellps=", ellps, " +towgs84=", towgs84, " +units=m +no_defs")
  if (is.null(zone)) {
    return(paste("+proj=longlat", base))
  }
  hemi <- gc_hemisphere(hemisphere)
  paste0(
    "+proj=utm +zone=", as.integer(zone),
    if (hemi == "S") " +south" else "",
    " ", base
  )
}

#' Parâmetros Helmert completos (7 parâmetros; rotações e escala zeradas)
gc_helmert_params <- function(d) {
  h <- d$helmert
  c(
    x = unname(h["x"]), y = unname(h["y"]), z = unname(h["z"]),
    rx = 0, ry = 0, rz = 0, s = 0
  )
}

# -----------------------------------------------------------------------------
# Hemisférios e zonas
# -----------------------------------------------------------------------------
gc_hemisphere <- function(x) {
  if (is.null(x) || !length(x) || all(is.na(x))) {
    return(NA_character_)
  }
  n <- gc_norm(x)
  out <- ifelse(
    n %in% c("S", "SUL", "SOUTH", "-", "S-"),
    "S",
    ifelse(n %in% c("N", "NORTE", "NORTH", "+"), "N", NA_character_)
  )
  out[1]
}

#' Normaliza uma zona UTM informada como texto ("24S", "zona 24 sul", "24")
gc_parse_zone <- function(x) {
  if (is.null(x) || !length(x) || all(is.na(x))) {
    return(list(zone = NA_integer_, hemisphere = NA_character_, raw = NA_character_))
  }
  raw <- as.character(x)[1]
  n <- gc_norm(raw)
  n <- gsub("^(ZONA|Z|FUSO|UTM)[[:space:]:.-]*", "", n)
  n <- gc_squish(n)
  hemi_from_text <- if (grepl("SUL|SOUTH", n)) {
    "S"
  } else if (grepl("NORTE|NORTH", n)) {
    "N"
  } else {
    NA_character_
  }
  num <- regmatches(n, regexpr("[0-9]{1,2}", n))
  zone <- if (length(num) && nzchar(num)) as.integer(num) else NA_integer_
  hemi_letter <- regmatches(n, regexpr("(^|[^A-Z])[NS]([^A-Z]|$)", n))
  hemi_letter <- if (length(hemi_letter) && nzchar(hemi_letter)) {
    gc_hemisphere(gsub("[^NS]", "", hemi_letter))
  } else {
    NA_character_
  }
  hemi <- hemi_from_text %||% hemi_letter
  list(
    zone = if (is.na(zone) || zone < 1L || zone > 60L) NA_integer_ else zone,
    hemisphere = hemi,
    raw = raw
  )
}

#' Lista de zonas UTM usadas no Brasil, para os seletores
gc_brazil_zones <- function() {
  list(
    "Sul (hemisfério sul)" = stats::setNames(as.character(17:26), paste0(17:26, "S")),
    "Norte (hemisfério norte)" = stats::setNames(as.character(11:24), paste0(11:24, "N"))
  )
}

# Limites aproximados do Brasil (para avisos de sanidade)
gc_brazil_bbox <- list(
  lon_min = -74.5, lon_max = -27.0,
  lat_min = -34.5, lat_max = 6.0
)
