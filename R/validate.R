# =============================================================================
# geocoordr :: validações com mensagens descritivas
#
# Cada função devolve um data.frame de problemas (ver gc_issue()).
# Níveis: "erro" impede o resultado; "aviso" deixa passar mas pede conferência.
# =============================================================================

# -----------------------------------------------------------------------------
# Sanidade geográfica
# -----------------------------------------------------------------------------

#' Avisa quando um ponto cai fora dos limites do Brasil
gc_validate_brazil <- function(points, lon_col = "lon", lat_col = "lat") {
  bb <- gc_brazil_bbox
  lon <- points[[lon_col]]
  lat <- points[[lat_col]]
  out <- gc_empty_issues()
  for (i in seq_len(nrow(points))) {
    if (!is.finite(lon[i]) || !is.finite(lat[i])) {
      next
    }
    if (lon[i] < bb$lon_min || lon[i] > bb$lon_max ||
        lat[i] < bb$lat_min || lat[i] > bb$lat_max) {
      out <- gc_issues(out, gc_warning(
        code = "fora_do_brasil",
        title = "Ponto fora dos limites do Brasil",
        message = paste0("O ponto ", points$ponto[i], " resultou em ",
                         gc_fmt(lat[i], 5, dec = "."), "°, ",
                         gc_fmt(lon[i], 5, dec = "."), "°, fora da área do Brasil ",
                         "(lat 6°N a 34°S, lon 28°W a 74°W)."),
        hint = paste0("Confira o datum, o hemisfério e a zona UTM, e se os eixos ",
                      "E/N não estão invertidos."),
        line = points$linha[i] %||% NA_integer_,
        input = points$bruto[i] %||% NA_character_
      ))
    }
  }
  out
}

#' Confere se a zona UTM informada corresponde à longitude do ponto
gc_validate_utm_zone <- function(points) {
  out <- gc_empty_issues()
  sel <- !is.na(points$zona_entrada) & is.finite(points$lon)
  if (!any(sel)) {
    return(out)
  }
  for (i in which(sel)) {
    lon <- points$lon[i]
    zone_natural <- gc_utm_zone_from_lon(lon)
    if (!identical(as.integer(zone_natural), as.integer(points$zona_entrada[i]))) {
      out <- gc_issues(out, gc_warning(
        code = "zona_divergente",
        title = "Zona UTM não corresponde à longitude",
        message = paste0("O ponto ", points$ponto[i], " foi informado na zona ",
                         points$zona_entrada[i], points$hemisferio_entrada[i],
                         ", mas a longitude calculada (",
                         gc_fmt(lon, 6, dec = "."), "°) pertence à zona ",
                         zone_natural, "."),
        hint = paste0("Isso costuma acontecer com pontos próximos da divisa entre ",
                      "zonas (a distorção cresce longe do meridiano central) ou com a zona trocada."),
        line = points$linha[i] %||% NA_integer_,
        input = points$bruto[i] %||% NA_character_
      ))
    }
  }
  out
}

#' Avisa sobre a transformação de datum aplicada
gc_validate_datum <- function(datum, target = "SIRGAS2000") {
  d <- gc_datum(datum)
  out <- gc_empty_issues()
  if (identical(gc_datum_key(datum), gc_datum_key(target))) {
    return(out)
  }
  if (isTRUE(d$legacy)) {
    out <- gc_issues(out, gc_warning(
      code = "datum_antigo",
      title = paste0("Conversão de ", d$short, " para ", gc_datum(target)$short),
      message = paste0(
        "O datum de entrada é o ", d$short,
        ". A conversão para ", gc_datum(target)$short,
        " usa translação de 3 parâmetros publicada e tem erro da ordem de ",
        d$accuracy_m, " m (a incerteza do datum antigo é maior que a da projeção).",
        if (isTRUE(d$approx)) " Para o Córrego Alegre 1961 não há parâmetros oficiais de 3 parâmetros: usamos os do Córrego Alegre 1970-72, com erro ainda maior." else ""
      ),
      hint = paste0("Para fins de georreferenciamento oficial, obtenha as coordenadas ",
                    "já referidas ao SIRGAS 2000 ou use a grade oficial do IBGE."),
      link = "https://epsg.io/4618"
    ))
  } else if (gc_datum_key(datum) %in% c("WGS84")) {
    out <- gc_issues(out, gc_tip(
      code = "wgs84_para_sirgas",
      title = "WGS 84 e SIRGAS 2000 são equivalentes na prática",
      message = paste0("A diferença entre WGS 84 e SIRGAS 2000 no Brasil é ",
                       "inferior a 1 m para a maioria das aplicações."),
      link = "https://www.ibge.gov.br/geociencias/geodesia"
    ))
  }
  out
}

#' Avisa sobre a fuga da zona de saída
gc_validate_output_zone <- function(lon, zone, hemisphere) {
  if (!length(lon) || any(!is.finite(lon))) {
    return(gc_empty_issues())
  }
  lon0 <- gc_utm_central_meridian(zone)
  dlam <- abs(((lon - lon0 + 180) %% 360) - 180)
  worst <- max(dlam, na.rm = TRUE)
  if (worst <= 3.0001) {
    return(gc_empty_issues())
  }
  gc_warning(
    code = "zona_saida_distorcida",
    title = "Pontos longe do meridiano central da zona",
    message = paste0("A zona ", zone, hemisphere, " tem meridiano central ", lon0,
                     "°. Há pontos a ", gc_fmt(worst, 2, dec = "."),
                     "° dele, onde a distorção da UTM aumenta."),
    hint = paste0("Para áreas que cruzam o limite de zonas, use a zona que contém ",
                  "a maior parte da área ou trabalhe em coordenadas geográficas.")
  )
}

#' Procura pontos repetidos
gc_validate_duplicates <- function(points) {
  out <- gc_empty_issues()
  if (nrow(points) < 2L) {
    return(out)
  }
  key <- paste(round(points$lon, 10), round(points$lat, 10), sep = "|")
  dup <- duplicated(key)
  if (any(dup)) {
    for (i in which(dup)) {
      out <- gc_issues(out, gc_warning(
        code = "ponto_repetido",
        title = "Ponto repetido",
        message = paste0("O ponto ", points$ponto[i], " (linha ", points$linha[i],
                         ") tem as mesmas coordenadas de um ponto anterior."),
        hint = "Se faz parte do polígono, confira se o memorial repete o vértice inicial.",
        line = points$linha[i] %||% NA_integer_,
        input = points$bruto[i] %||% NA_character_
      ))
    }
  }
  out
}

# -----------------------------------------------------------------------------
# Sanidade do polígono
# -----------------------------------------------------------------------------

gc_validate_polygon <- function(x, y, labels = NULL) {
  out <- gc_empty_issues()
  n <- length(x)
  if (n < 3L) {
    return(gc_warning(
      code = "poligono_pontos_insuficientes",
      title = "Poucos pontos para fechar o polígono",
      message = paste0("Foram lidos ", n, " ponto(s); são necessários ao menos 3 ",
                       "para formar um polígono."),
      hint = "Desligue 'fechar polígono' para ver apenas os pontos."
    ))
  }
  crosses <- gc_find_self_intersections(x, y)
  if (nrow(crosses)) {
    if (is.null(labels)) labels <- paste0("P", seq_len(n))
    pares <- paste0(labels[crosses$i], "–", labels[crosses$j], collapse = ", ")
    out <- gc_issues(out, gc_warning(
      code = "poligono_autointersecao",
      title = "O polígono se cruza",
      message = paste0("Os lados ", pares, " se cruzam. Um polígono válido não ",
                       "pode ter lados que se cruzam."),
      hint = "Confira a ordem dos vértices no memorial: eles devem seguir o contorno do imóvel."
    ))
  }
  area <- gc_polygon_area(x, y)
  if (is.finite(area) && area < 1) {
    out <- gc_issues(out, gc_warning(
      code = "poligono_area_minima",
      title = "Área praticamente nula",
      message = paste0("A área calculada foi ", gc_fmt(area, 3),
                       " m², o que sugere pontos alinhados ou repetidos."),
      hint = "Confira se todos os vértices foram informados."
    ))
  }
  out
}
