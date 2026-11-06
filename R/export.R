# =============================================================================
# geocoordR :: exportação
#
# CSV (padrão brasileiro e internacional), GeoJSON (RFC 7946), KML (OGC) e,
# quando o pacote sf estiver disponível, GeoPackage e Shapefile.
# Tudo em R puro, exceto GPKG/SHP.
# =============================================================================

#' Tabela de saída com nomes em português
gc_export_table <- function(result, digits = 8) {
  pts <- result$points
  if (!nrow(pts)) {
    return(data.frame())
  }
  data.frame(
    Ponto = pts$ponto,
    Latitude = signif(pts$lat, 12),
    Longitude = signif(pts$lon, 12),
    `Latitude (GMS)` = pts$lat_dms,
    `Longitude (GMS)` = pts$lon_dms,
    `E (m)` = round(pts$e, 3),
    `N (m)` = round(pts$n, 3),
    `Zona UTM` = paste0(ifelse(is.na(pts$zona), "", pts$zona),
                        ifelse(is.na(pts$hemisferio), "", pts$hemisferio)),
    EPSG = pts$epsg,
    `Datum de entrada` = gc_datum(pts$datum_entrada[1])$short,
    `Texto de origem` = pts$bruto,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Lados do polígono com distância e azimute
gc_export_sides <- function(result) {
  sides <- result$polygon$sides
  if (is.null(sides) || !nrow(sides)) {
    return(data.frame())
  }
  data.frame(
    De = sides$de,
    Para = sides$para,
    `Distância (m)` = round(sides$distancia, 3),
    `Azimute (°)` = round(sides$azimute, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Cabecalho de metadados com o CRS usado
gc_export_metadata <- function(result) {
  s <- result$summary
  data.frame(
    item = c("Aplicativo", "Pontos", "Datum de entrada", "Datum de saída",
             "Zona UTM de saída", "EPSG", "PROJ.4", "Área (m²)", "Área (ha)",
             "Perímetro (m)", "Gerado em"),
    valor = c(
      paste0("geocoordR ", GC_APP_VERSION),
      as.character(s$n %||% 0L),
      gc_datum(result$meta$datum_entrada)$short,
      gc_datum(result$meta$datum_saida)$short,
      paste0(s$zona %||% "", s$hemisferio %||% ""),
      as.character(s$epsg %||% ""),
      s$proj4 %||% "",
      gc_fmt(s$area_m2 %||% NA_real_, 2, dec = "."),
      gc_fmt(s$area_ha %||% NA_real_, 4, dec = "."),
      gc_fmt(s$perimeter_m %||% NA_real_, 3, dec = "."),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# CSV
# -----------------------------------------------------------------------------

#' Formata um campo CSV (aspas apenas quando necessário)
gc_csv_field <- function(x, sep = ";", dec = ".") {
  if (is.numeric(x)) {
    out <- format(x, scientific = FALSE, trim = TRUE, digits = 15)
    out[is.na(x)] <- ""
    if (!identical(dec, ".")) {
      out <- sub(".", dec, out, fixed = TRUE)
    }
    return(out)
  }
  out <- as.character(x)
  out[is.na(out)] <- ""
  precisa <- grepl(sep, out, fixed = TRUE) |
    grepl("\"", out, fixed = TRUE) |
    grepl("[\r\n]", out)
  out[precisa] <- paste0("\"", gsub("\"", "\"\"", out[precisa], fixed = TRUE), "\"")
  out
}

#' Gera o conteúdo CSV como texto
gc_csv_string <- function(df, sep = ";", dec = ",", bom = TRUE) {
  if (is.null(df) || !nrow(df)) {
    return("")
  }
  cabecalho <- vapply(names(df), gc_csv_field, character(1), sep = sep, dec = dec)
  linhas <- lapply(seq_len(nrow(df)), function(i) {
    campos <- lapply(df, function(col) gc_csv_field(col[i], sep = sep, dec = dec))
    paste(unlist(campos, use.names = FALSE), collapse = sep)
  })
  body <- c(paste(cabecalho, collapse = sep), linhas)
  paste0(if (bom) "\ufeff" else "", paste(body, collapse = "\r\n"), "\r\n")
}

#' CSV no padrão brasileiro (Excel: ponto e vírgula + vírgula decimal + BOM)
gc_csv_br <- function(df) {
  gc_csv_string(df, sep = ";", dec = ",", bom = TRUE)
}

#' CSV internacional (vírgula + ponto decimal)
gc_csv_intl <- function(df) {
  gc_csv_string(df, sep = ",", dec = ".", bom = FALSE)
}

# -----------------------------------------------------------------------------
# GeoJSON (RFC 7946 — sempre WGS 84 / lon,lat)
# -----------------------------------------------------------------------------

gc_geojson_string <- function(result, polygon = TRUE) {
  pts <- result$points
  if (!nrow(pts)) {
    return("")
  }
  ok <- is.finite(pts$lon) & is.finite(pts$lat)
  pts <- pts[ok, , drop = FALSE]

  features <- lapply(seq_len(nrow(pts)), function(i) {
    list(
      type = "Feature",
      geometry = list(type = "Point", coordinates = c(pts$lon[i], pts$lat[i])),
      properties = list(
        nome = pts$ponto[i],
        latitude = pts$lat[i],
        longitude = pts$lon[i],
        latitude_gms = pts$lat_dms[i],
        longitude_gms = pts$lon_dms[i],
        e_utm = if (is.finite(pts$e[i])) round(pts$e[i], 3) else NULL,
        n_utm = if (is.finite(pts$n[i])) round(pts$n[i], 3) else NULL,
        zona_utm = if (!is.na(pts$zona[i])) paste0(pts$zona[i], pts$hemisferio[i]) else NULL,
        epsg = if (!is.na(pts$epsg[i])) pts$epsg[i] else NULL,
        datum_entrada = gc_datum(pts$datum_entrada[i])$short
      )
    )
  })

  if (isTRUE(polygon) && length(result$polygon$lon) >= 4L) {
    ring <- lapply(seq_along(result$polygon$lon), function(i) {
      c(result$polygon$lon[i], result$polygon$lat[i])
    })
    features <- c(features, list(list(
      type = "Feature",
      geometry = list(type = "Polygon", coordinates = list(ring)),
      properties = list(
        nome = "Perímetro",
        area_m2 = round(result$polygon$area_m2, 3),
        area_ha = round(result$polygon$area_m2 / 10000, 6),
        perimetro_m = round(result$polygon$perimeter_m, 3),
        datum = gc_datum(result$meta$datum_saida)$short,
        epsg = result$summary$epsg
      )
    )))
  }

  fc <- list(
    type = "FeatureCollection",
    name = "geocoordR",
    features = features
  )
  jsonlite::toJSON(fc, auto_unbox = TRUE, digits = NA, pretty = TRUE, null = "null")
}

# -----------------------------------------------------------------------------
# KML (OGC)
# -----------------------------------------------------------------------------

gc_kml_string <- function(result, polygon = TRUE) {
  pts <- result$points
  if (!nrow(pts)) {
    return("")
  }
  ok <- is.finite(pts$lon) & is.finite(pts$lat)
  pts <- pts[ok, , drop = FALSE]

  doc_name <- paste0("geocoordR — ", gc_datum(result$meta$datum_saida)$short,
                     " UTM ", result$summary$zona %||% "", result$summary$hemisferio %||% "")
  marks <- vapply(seq_len(nrow(pts)), function(i) {
    paste0(
      "    <Placemark>\n",
      "      <name>", gc_xml_escape(pts$ponto[i]), "</name>\n",
      "      <description>", gc_xml_escape(paste0(
        "Lat: ", gc_fmt(pts$lat[i], 8, dec = "."),
        " Lon: ", gc_fmt(pts$lon[i], 8, dec = "."),
        " | E: ", gc_fmt(pts$e[i], 3, dec = "."),
        " N: ", gc_fmt(pts$n[i], 3, dec = ".")
      )), "</description>\n",
      "      <styleUrl>#ponto</styleUrl>\n",
      "      <Point><coordinates>", gc_fmt(pts$lon[i], 8, dec = "."), ",",
      gc_fmt(pts$lat[i], 8, dec = "."), ",0</coordinates></Point>\n",
      "    </Placemark>"
    )
  }, character(1))

  poly_block <- ""
  if (isTRUE(polygon) && length(result$polygon$lon) >= 4L) {
    coords <- paste(
      paste0(gc_fmt(result$polygon$lon, 8, dec = "."), ",",
             gc_fmt(result$polygon$lat, 8, dec = "."), ",0"),
      collapse = " "
    )
    poly_block <- paste0(
      "    <Placemark>\n",
      "      <name>Perímetro</name>\n",
      "      <styleUrl>#poligono</styleUrl>\n",
      "      <Polygon><outerBoundaryIs><LinearRing><coordinates>", coords,
      "</coordinates></LinearRing></outerBoundaryIs></Polygon>\n",
      "    </Placemark>\n"
    )
  }

  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<kml xmlns="http://www.opengis.net/kml/2.2">\n',
    "  <Document>\n",
    "    <name>", gc_xml_escape(doc_name), "</name>\n",
    "    <description>", gc_xml_escape(paste0(
      "Gerado por geocoordR ", GC_APP_VERSION, ". Datum ",
      gc_datum(result$meta$datum_saida)$short, ". ",
      "Área: ", gc_fmt_area(result$polygon$area_m2)
    )), "</description>\n",
    '    <Style id="ponto"><IconStyle><color>ff0f6ee8</color><scale>1.1</scale></IconStyle></Style>\n',
    '    <Style id="poligono"><LineStyle><color>ff0f6ee8</color><width>2</width></LineStyle>' ,
    "<PolyStyle><color>4d0f6ee8</color></PolyStyle></Style>\n",
    paste(marks, collapse = "\n"), "\n",
    poly_block,
    "  </Document>\n",
    "</kml>\n"
  )
}

# -----------------------------------------------------------------------------
# sf (opcional): GeoPackage e Shapefile
# -----------------------------------------------------------------------------

gc_sf_available <- function() {
  requireNamespace("sf", quietly = TRUE)
}

gc_to_sf <- function(result) {
  if (!gc_sf_available()) {
    stop("O pacote 'sf' não está instalado. Use CSV, GeoJSON ou KML.", call. = FALSE)
  }
  pts <- result$points
  ok <- is.finite(pts$lon) & is.finite(pts$lat)
  pts <- pts[ok, , drop = FALSE]
  dados <- data.frame(
    ponto = pts$ponto,
    lat = pts$lat,
    lon = pts$lon,
    e_utm = pts$e,
    n_utm = pts$n,
    zona = pts$zona,
    hemisferio = pts$hemisferio,
    epsg = pts$epsg,
    stringsAsFactors = FALSE
  )
  pontos <- sf::st_as_sf(dados, coords = c("lon", "lat"),
                         crs = sf::st_crs(4674), dim = "XY")
  poligono <- NULL
  if (length(result$polygon$lon) >= 4L) {
    ring <- cbind(result$polygon$lon, result$polygon$lat)
    poligono <- sf::st_sf(
      nome = "Perímetro",
      area_m2 = round(result$polygon$area_m2, 3),
      geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4674)
    )
  }
  list(pontos = pontos, poligono = poligono)
}

gc_write_sf <- function(result, file, driver = NULL) {
  obj <- gc_to_sf(result)
  if (is.null(driver)) {
    driver <- if (grepl("\\.gpkg$", file, ignore.case = TRUE)) "GPKG" else "ESRI Shapefile"
  }
  if (identical(driver, "GPKG")) {
    sf::st_write(obj$pontos, file, layer = "pontos", delete_dsn = TRUE, quiet = TRUE)
    if (!is.null(obj$poligono)) {
      sf::st_write(obj$poligono, file, layer = "perimetro", append = TRUE, quiet = TRUE)
    }
  } else {
    dir <- dirname(file)
    base <- sub("\\.shp$", "", basename(file), ignore.case = TRUE)
    sf::st_write(obj$pontos, file.path(dir, paste0(base, "_pontos.shp")),
                 delete_dsn = TRUE, quiet = TRUE)
    if (!is.null(obj$poligono)) {
      sf::st_write(obj$poligono, file.path(dir, paste0(base, "_perimetro.shp")),
                   delete_dsn = TRUE, quiet = TRUE)
    }
  }
  invisible(file)
}

#' Nome de arquivo padronizado
gc_filename <- function(ext, prefix = "geocoordR") {
  paste0(prefix, "-", gc_stamp(), ".", ext)
}
