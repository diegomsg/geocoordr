# =============================================================================
# geocoordR :: conversão completa (texto -> pontos -> SIRGAS 2000 -> UTM)
#
# gc_convert() é a única porta de entrada usada pela interface: recebe o texto
# do memorial + opções e devolve um objeto com pontos, polígono, resumo,
# metadados e a lista de problemas encontrados.
# =============================================================================

#' Resultado vazio (usado quando nada pôde ser lido)
gc_empty_result <- function(issues, datum, target_datum, t0 = Sys.time()) {
  list(
    ok = FALSE,
    points = gc_points_template(),
    issues = issues,
    polygon = list(x = numeric(), y = numeric(), lon = numeric(), lat = numeric(),
                   closed = FALSE, area_m2 = NA_real_, perimeter_m = NA_real_,
                   sides = data.frame(), self_intersections = data.frame()),
    summary = list(n = 0L),
    meta = list(datum_entrada = datum, datum_saida = target_datum,
                elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  )
}

#' Converte o texto de entrada em coordenadas prontas para uso
#'
#' @param text texto colado pelo usuário (uma coordenada por linha).
#' @param type "utm", "geographic" ou "auto".
#' @param datum datum das coordenadas de entrada.
#' @param zone zona UTM padrão (quando o texto não traz a zona).
#' @param hemisphere hemisfério padrão.
#' @param dec_sep separador decimal ("auto", "." ou ",").
#' @param order ordem dos eixos ("auto", "EN"/"NE", "latlon"/"lonlat").
#' @param output_zone "auto", "same" (mesma da entrada) ou número da zona.
#' @param target_datum datum de saída (padrão SIRGAS 2000).
#' @param polygon fechar polígono?
#' @param digits casas decimais nas coordenadas de saída.
gc_convert <- function(text,
                       type = c("utm", "geographic", "auto"),
                       datum = "SIRGAS2000",
                       zone = NA,
                       hemisphere = NA,
                       dec_sep = "auto",
                       order = "auto",
                       output_zone = "auto",
                       target_datum = "SIRGAS2000",
                       polygon = TRUE,
                       digits = 6) {
  type <- match.arg(type)
  t0 <- Sys.time()

  parsed <- gc_parse_coordinates(
    text = text, type = type, datum = datum, zone = zone,
    hemisphere = hemisphere, dec_sep = dec_sep, order = order
  )
  issues <- parsed$issues
  pts <- parsed$points

  if (!nrow(pts)) {
    if (!nrow(issues)) {
      issues <- gc_error(
        code = "sem_pontos", title = "Nenhum ponto lido",
        message = "Não foi possível interpretar nenhuma coordenada no texto informado.",
        hint = "Confira o tipo de coordenada escolhido e use 'Usar exemplo' para ver o formato aceito."
      )
    }
    return(gc_empty_result(issues, datum, target_datum, t0))
  }

  is_utm <- pts$tipo == "UTM"

  # ---------------------------------------------------------------- entrada --
  pts$e_entrada <- ifelse(is_utm, pts$e, NA_real_)
  pts$n_entrada <- ifelse(is_utm, pts$n, NA_real_)
  pts$zona_entrada <- ifelse(is_utm, pts$zona, NA_integer_)
  pts$hemisferio_entrada <- ifelse(is_utm, pts$hemisferio, NA_character_)

  # UTM -> geográfico (no datum de entrada)
  if (any(is_utm)) {
    geo_in <- gc_utm_to_geographic(
      easting = pts$e[is_utm], northing = pts$n[is_utm],
      zone = pts$zona[is_utm], hemisphere = pts$hemisferio[is_utm],
      datum = datum
    )
    pts$lon[is_utm] <- geo_in$lon
    pts$lat[is_utm] <- geo_in$lat
  }
  pts$lon_entrada <- pts$lon
  pts$lat_entrada <- pts$lat

  # ------------------------------------------------------- validações base --
  issues <- gc_issues(
    issues,
    gc_validate_brazil(pts, "lon_entrada", "lat_entrada"),
    gc_validate_utm_zone(pts),
    gc_validate_duplicates(pts),
    gc_validate_datum(datum, target_datum)
  )

  # --------------------------------------------------- mudança de datum ----
  sig <- gc_transform_datum(pts$lon_entrada, pts$lat_entrada,
                            from = datum, to = target_datum)
  pts$lon <- sig$lon
  pts$lat <- sig$lat
  issues <- gc_issues(issues, gc_validate_brazil(pts, "lon", "lat"))

  # ------------------------------------------------- zona de saída ----------
  centroid <- gc_centroid(pts$lon, pts$lat)
  zone_centro <- if (is.finite(centroid[["lon"]])) {
    unname(gc_utm_zone_from_lon(centroid[["lon"]]))
  } else {
    NA_integer_
  }
  zone_out <- NA_integer_
  if (is.character(output_zone) && output_zone %in% c("auto", "same")) {
    zones_in <- unique(stats::na.omit(pts$zona_entrada))
    # mantém a zona do documento apenas quando todos os pontos são UTM e
    # compartilham a mesma zona; caso contrário usa a zona da área
    if (all(is_utm) && length(zones_in) == 1L) {
      zone_out <- as.integer(zones_in)
    } else {
      zone_out <- zone_centro
    }
  } else if (is.numeric(output_zone) && length(output_zone) == 1L &&
             is.finite(output_zone)) {
    zone_out <- as.integer(output_zone)
  }
  if (is.na(zone_out)) {
    issues <- gc_issues(issues, gc_error(
      code = "zona_saida_ausente", title = "Não foi possível definir a zona de saída",
      message = "Não há zona UTM na entrada e a zona automática não pôde ser calculada.",
      hint = "Escolha a zona de saída no painel de resultados."
    ))
    zone_out <- NA_integer_
  }
  hemi_out <- if (is.finite(centroid[["lat"]])) {
    unname(gc_hemisphere_from_lat(centroid[["lat"]]))
  } else {
    NA_character_
  }

  if (!is.na(zone_out)) {
    issues <- gc_issues(issues, gc_validate_output_zone(pts$lon, zone_out, hemi_out))

    # pontos em zonas diferentes não cabem numa única planta UTM
    zonas_naturais <- unique(gc_utm_zone_from_lon(pts$lon[is.finite(pts$lon)]))
    if (length(zonas_naturais) > 1L) {
      issues <- gc_issues(issues, gc_warning(
        code = "pontos_em_zonas_diferentes",
        title = "Pontos em zonas UTM diferentes",
        message = paste0("As longitudes lidas ocupam as zonas ",
                         paste(sort(zonas_naturais), collapse = ", "),
                         ". Uma única planta UTM só é exata dentro da sua zona; os ",
                         "pontos mais afastados ficam com E/N fora da faixa usual."),
        hint = paste0("Para áreas que cruzam o limite de zonas, use as coordenadas ",
                      "geográficas (graus decimais ou GMS), que não têm essa limitação."),
        link = "https://epsg.io/31983"
      ))
    }

    utm_out <- gc_geographic_to_utm(pts$lon, pts$lat, zone_out, hemi_out,
                                    target_datum)

    pts$e <- utm_out$easting
    pts$n <- utm_out$northing
    pts$zona <- zone_out
    pts$hemisferio <- hemi_out
  } else {
    pts$e <- NA_real_
    pts$n <- NA_real_
    pts$zona <- NA_integer_
    pts$hemisferio <- NA_character_
  }

  # ---------------------------------------------------------- formatação ----
  pts$lat_dms <- vapply(pts$lat, gc_format_dms_axis, character(1), axis = "lat")
  pts$lon_dms <- vapply(pts$lon, gc_format_dms_axis, character(1), axis = "lon")
  pts$datum_entrada <- datum
  pts$epsg <- if (is.na(zone_out)) NA_integer_ else {
    gc_utm_epsg(target_datum, zone_out, hemi_out)
  }

  # ----------------------------------------------------------- polígono -----
  poly <- list(
    x = numeric(), y = numeric(), lon = numeric(), lat = numeric(),
    closed = FALSE, area_m2 = NA_real_, perimeter_m = NA_real_,
    sides = data.frame(), self_intersections = data.frame()
  )
  if (isTRUE(polygon)) {
    good <- is.finite(pts$e) & is.finite(pts$n)
    x <- pts$e[good]
    y <- pts$n[good]
    labels <- pts$ponto[good]
    ring <- gc_close_ring(x, y)
    poly$closed <- ring$closed
    poly$lon <- c(pts$lon[good], if (ring$closed) pts$lon[good][1] else numeric(0))
    poly$lat <- c(pts$lat[good], if (ring$closed) pts$lat[good][1] else numeric(0))
    poly$x <- ring$x
    poly$y <- ring$y
    if (length(x) >= 3L) {
      poly$area_m2 <- gc_polygon_area(x, y)
      poly$perimeter_m <- gc_polygon_perimeter(x, y)
      poly$sides <- gc_sides(x, y, labels)
      poly$self_intersections <- gc_find_self_intersections(x, y)
    }
    if (ring$added) {
      issues <- gc_issues(issues, gc_tip(
        code = "poligono_fechado",
        title = "Polígono fechado automaticamente",
        message = "O último vértice foi ligado ao primeiro para fechar o polígono.",
        hint = "Se o memorial já termina repetindo o primeiro vértice, nada a fazer."
      ))
    }
    issues <- gc_issues(issues, gc_validate_polygon(x, y, labels))
  }

  # ------------------------------------------------------------- resumo -----
  bb <- gc_bbox(pts$lon, pts$lat)
  k <- if (!is.na(zone_out) && is.finite(centroid[["lon"]])) {
    gc_utm_scale_factor(centroid[["lon"]], centroid[["lat"]], zone_out, hemi_out,
                        target_datum)
  } else {
    NA_real_
  }
  sides <- poly$sides
  if (!is.null(k)) k <- unname(k)
  summary <- list(
    n = nrow(pts),
    n_utm = sum(is_utm),
    n_geo = sum(!is_utm),
    tipos = paste(sort(unique(pts$tipo)), collapse = ", "),
    datum_entrada = datum,
    datum_saida = target_datum,
    zona = zone_out,
    hemisferio = hemi_out,
    epsg = if (is.na(zone_out)) NA_integer_ else gc_utm_epsg(target_datum, zone_out, hemi_out),
    epsg_geografico = gc_geographic_epsg(target_datum),
    proj4 = gc_proj4(target_datum, if (is.na(zone_out)) NULL else zone_out,
                     hemi_out),
    centroid = centroid,
    bbox = bb,
    area_m2 = poly$area_m2,
    area_ha = poly$area_m2 / 10000,
    perimeter_m = poly$perimeter_m,
    scale_factor = k,
    n_sides = if (nrow(sides)) nrow(sides) else 0L,
    max_side_m = if (nrow(sides)) max(sides$distancia, na.rm = TRUE) else NA_real_,
    self_intersections = nrow(poly$self_intersections),
    polygon = isTRUE(polygon) && nrow(pts) >= 3L,
    elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )

  list(
    ok = !gc_has_error(issues),
    points = pts,
    issues = issues,
    polygon = poly,
    summary = summary,
    meta = list(
      datum_entrada = datum,
      datum_saida = target_datum,
      zona = zone_out,
      hemisferio = hemi_out,
      epsg = summary$epsg,
      proj4 = summary$proj4,
      type = type,
      dec_sep = dec_sep,
      order = order,
      digits = digits,
      elapsed = summary$elapsed
    )
  )
}

#' Texto de conferência (usado em testes e no botão "copiar resumo")
gc_result_text <- function(result) {
  s <- result$summary
  if (!is.null(s$n) && s$n == 0L) {
    return("Nenhum ponto lido.")
  }
  out <- c(
    paste0("Pontos: ", s$n, " (", s$tipos, ")"),
    paste0("Datum de entrada: ", gc_datum(result$meta$datum_entrada)$short,
           " | Datum de saída: ", gc_datum(result$meta$datum_saida)$short),
    paste0("Zona de saída: ", s$zona, s$hemisferio,
           if (!is.na(s$epsg)) paste0(" (EPSG:", s$epsg, ")") else ""),
    paste0("Centro: ", gc_fmt(s$centroid["lat"], 6, dec = "."), "°, ",
           gc_fmt(s$centroid["lon"], 6, dec = "."), "°"),
    if (isTRUE(s$polygon)) paste0("Área: ", gc_fmt_area(s$area_m2)),
    if (isTRUE(s$polygon)) paste0("Perímetro: ", gc_fmt_num(s$perimeter_m, 2), " m")
  )
  paste(out[!vapply(out, is.null, logical(1))], collapse = "\n")
}
