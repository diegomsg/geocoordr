# =============================================================================
# geocoordR :: leitura de memoriais descritivos
#
# Cada linha do texto pode descrever um vértice. O leitor aceita, por linha:
#
#   UTM:
#     24S 713110.66 8793023.16
#     Zona 24 Sul, E 713110,66, N 8793023,16
#     P1: 713.110,66; 8.793.023,16  (zona no painel)
#     P-01 N 8793023.16 E 713110.66
#
#   Geográfico:
#     -15.7939, -47.8825
#     15,7939 S / 47,8825 O
#     15°47'38" S 47°52'57" O
#     15 47 38.0 S 47 52 57.0 O
#
# O retorno é sempre list(points, issues): `points` contém só o que pôde ser
# lido e `issues` explica, linha por linha, o que não pôde e por quê.
# =============================================================================

gc_points_template <- function() {
  data.frame(
    ponto = character(),
    tipo = character(),
    bruto = character(),
    linha = integer(),
    lon = numeric(),
    lat = numeric(),
    e = numeric(),
    n = numeric(),
    zona = integer(),
    hemisferio = character(),
    datum = character(),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Detecção de zona UTM
# -----------------------------------------------------------------------------

#' Extrai uma indicação de zona UTM de uma linha
#'
#' Formas aceitas: "24S", "24 S", "Zona 24 Sul", "fuso 24s", "24/S", "24-S".
gc_extract_utm_zone <- function(text) {
  keyed <- "(?i)\\b(?:ZONA|FUSO|UTM|Z)\\s*[:.]?\\s*([0-9]{1,2})(?:\\s*[-/]?\\s*)?(NORTE|SUL|NORTH|SOUTH|N|S)?\\b"
  at_start <- "(?i)^\\s*([0-9]{1,2})\\s*[-/]?\\s*(NORTE|SUL|NORTH|SOUTH|N|S)\\b"
  at_end <- "(?i)\\b([0-9]{1,2})\\s*[-/]?\\s*(NORTE|SUL|NORTH|SOUTH|N|S)\\s*$"

  get_part <- function(pat) {
    m <- regexpr(pat, text, perl = TRUE)
    if (m[1] == -1L) {
      return(NULL)
    }
    regmatches(text, m)
  }

  for (pat in list(keyed, at_start, at_end)) {
    hit <- get_part(pat)
    if (!is.null(hit)) {
      zone <- as.integer(regmatches(hit, regexpr("[0-9]{1,2}", hit)))
      hemi <- gc_norm(regmatches(hit, regexpr("(?i)(NORTE|SUL|NORTH|SOUTH|N|S)\\s*$", hit, perl = TRUE)))
      hemi <- gc_hemisphere(hemi)
      return(list(
        zone = if (!is.na(zone) && zone >= 1 && zone <= 60) zone else NA_integer_,
        hemisphere = hemi,
        matched = hit
      ))
    }
  }
  list(zone = NA_integer_, hemisphere = NA_character_, matched = NA_character_)
}

#' Remove a primeira ocorrência de um trecho
gc_drop_first <- function(text, pattern, fixed = FALSE) {
  if (is.na(pattern) || !nzchar(pattern)) {
    return(text)
  }
  sub(pattern, " ", text, fixed = fixed, perl = !fixed)
}

# -----------------------------------------------------------------------------
# Rótulo do vértice ("P1", "V-01", "Vértice 1:" ...)
# -----------------------------------------------------------------------------
gc_extract_label <- function(text) {
  patterns <- c(
    "(?i)^\\s*([A-Za-z\u00c0-\u00ff][A-Za-z0-9\u00c0-\u00ff _.\\-]{0,20}?)\\s*[:=]\\s*(?=[-+]?[0-9])",
    "(?i)^\\s*([A-Za-z\u00c0-\u00ff]{1,8}[-_.]?[0-9]{1,4})\\s+(?=[-+]?[0-9])"
  )
  for (pat in patterns) {
    m <- regexpr(pat, text, perl = TRUE)
    if (m[1] != -1L) {
      hit <- regmatches(text, m)
      label <- gc_squish(sub("[:=]\\s*$", "", hit))
      if (nzchar(label) && !gc_is_utm_keyword(label)) {
        return(list(label = label, rest = sub(pat, "", text, perl = TRUE)))
      }
    }
  }
  list(label = NA_character_, rest = text)
}

gc_is_utm_keyword <- function(x) {
  gc_norm(x) %in% c("ZONA", "FUSO", "UTM", "Z", "E", "N", "X", "Y", "E N", "N E")
}

# -----------------------------------------------------------------------------
# Letras de eixo (E/N/X/Y) na frente dos números
# -----------------------------------------------------------------------------
gc_extract_utm_axes <- function(text) {
  pat <- "(?i)(?<![A-Za-z])([ENXY])\\s*[:=]?\\s*(?=[-+]?[0-9])"
  found <- regmatches(text, gregexpr(pat, text, perl = TRUE))[[1]]
  letters <- toupper(gsub("[^A-Za-z]", "", found))
  rest <- gsub(pat, " ", text, perl = TRUE)
  # normaliza: E/X = leste, N/Y = norte
  letters <- ifelse(letters %in% c("E", "X"), "E", "N")
  list(letters = letters, rest = rest)
}

#' Checa se um texto parece UTM (para o modo "não sei")
gc_looks_like_utm <- function(text) {
  if (grepl("(?i)\\b(ZONA|FUSO|UTM)\\b", text, perl = TRUE)) {
    return(TRUE)
  }
  numbers <- gc_extract_numbers(text)$token
  if (!length(numbers)) {
    return(FALSE)
  }
  values <- suppressWarnings(vapply(numbers, function(tk) {
    res <- gc_parse_number(tk, dec_sep = "auto")
    res$value
  }, numeric(1)))
  sum(abs(values) >= 1000, na.rm = TRUE) >= 2L
}

# -----------------------------------------------------------------------------
# Parser UTM de uma linha
# -----------------------------------------------------------------------------
gc_parse_utm_line <- function(text, line = NA_integer_, datum = "SIRGAS2000",
                              zone = NA_integer_, hemisphere = NA_character_,
                              dec_sep = "auto", order = "auto") {
  issues <- gc_empty_issues()
  raw_line <- text
  x <- gc_squish(text)

  # 1. rótulo do vértice (P1, V-01, Vértice 2: ...)
  lab <- gc_extract_label(x)
  x <- lab$rest

  # 2. zona UTM na própria linha (depois do rótulo, em qualquer posição)
  z <- gc_extract_utm_zone(x)
  if (!is.na(z$zone) || !is.na(z$hemisphere)) {
    x <- gc_drop_first(x, z$matched, fixed = TRUE)
    if (!is.na(z$zone)) zone <- z$zone
    if (!is.na(z$hemisphere)) hemisphere <- z$hemisphere
  }

  # 3. letras de eixo (E/N/X/Y antes dos números)
  axes <- gc_extract_utm_axes(x)
  x <- axes$rest
  axis_order <- NA_character_
  if (length(axes$letters) >= 2L) {
    axis_order <- paste(axes$letters[1:2], collapse = "")
    if (!axis_order %in% c("EN", "NE")) axis_order <- NA_character_
  }

  # 4. números
  nums <- gc_extract_numbers(x)

  # 4b. zona escrita sem hemisfério ("24 713110.66 8793023.16")
  if (nrow(nums) == 3L) {
    primeiro <- gc_parse_number(nums$token[1], dec_sep = dec_sep)
    outros <- vapply(nums$token[2:3], gc_parse_number_value, numeric(1),
                     dec_sep = dec_sep)
    if (isTRUE(primeiro$ok) && primeiro$value == round(primeiro$value) &&
        primeiro$value >= 1 && primeiro$value <= 60 &&
        all(abs(outros) >= 1000)) {
      if (is.na(zone)) {
        zone <- as.integer(primeiro$value)
      }
      nums <- nums[2:3, , drop = FALSE]
    }
  }
  if (nrow(nums) != 2L) {
    return(list(
      row = NULL,
      issues = gc_error(
        code = "utm_dois_valores",
        title = if (nrow(nums) < 2L) "Faltam valores na linha" else "Números demais na linha",
        message = paste0("No modo UTM esperamos exatamente 2 números (E e N), ",
                         "mas encontramos ", nrow(nums), " em '", gc_squish(raw_line), "'."),
        hint = paste0("Use um ponto por linha, por exemplo: 24S 713110.66 8793023.16. ",
                      "Se houver mais de um ponto na mesma linha, separe-os por ponto e vírgula."),
        line = line, input = raw_line
      )
    ))
  }

  parsed <- lapply(nums$token, gc_parse_number_coord,
                   dec_sep = dec_sep, line = line, raw = raw_line)
  issues <- gc_issues(issues, lapply(parsed, `[[`, "issues"))
  if (!all(vapply(parsed, `[[`, logical(1), "ok"))) {
    return(list(row = NULL, issues = issues))
  }
  v1 <- parsed[[1]]$value
  v2 <- parsed[[2]]$value

  # 5. ordem E/N
  guessed <- NA_character_
  if (is.na(axis_order)) {
    if (order %in% c("EN", "NE")) {
      axis_order <- order
    } else if (abs(v1) > 5e6 && abs(v2) <= 1e6) {
      axis_order <- "NE"
      guessed <- "NE"
    } else if (abs(v2) > 5e6 && abs(v1) <= 1e6) {
      axis_order <- "EN"
    } else {
      axis_order <- "EN"
    }
  }
  if (!is.na(guessed)) {
    issues <- gc_issues(issues, gc_warning(
      code = "utm_ordem_detectada",
      title = "Ordem dos valores detectada automaticamente",
      message = paste0("A linha '", gc_squish(raw_line), "' foi lida como ",
                       if (guessed == "EN") "E (leste), depois N (norte)" else "N (norte), depois E (leste)",
                       "."),
      hint = "Se estiver errado, escolha a ordem dos eixos nas opções avançadas.",
      line = line, input = raw_line
    ))
  }
  e <- if (axis_order == "EN") v1 else v2
  n <- if (axis_order == "EN") v2 else v1

  # 6. zona e hemisfério obrigatórios
  if (is.na(zone)) {
    issues <- gc_issues(issues, gc_error(
      code = "utm_sem_zona",
      title = "Falta a zona UTM",
      message = paste0("A linha '", gc_squish(raw_line),
                       "' tem as coordenadas, mas não diz a zona UTM (fuso)."),
      hint = paste0("Escreva a zona junto do ponto (ex.: 24S 713110.66 8793023.16) ",
                    "ou escolha a zona no painel ao lado."),
      line = line, input = raw_line
    ))
    return(list(row = NULL, issues = issues))
  }
  if (is.na(hemisphere)) {
    issues <- gc_issues(issues, gc_error(
      code = "utm_sem_hemisferio",
      title = "Falta o hemisfério",
      message = paste0("Não sabemos se a zona ", zone, " é Norte ou Sul."),
      hint = "Escreva a zona como 24S (sul) ou 22N (norte).",
      line = line, input = raw_line
    ))
    return(list(row = NULL, issues = issues))
  }

  # 7. faixas plausíveis
  e_issue <- gc_check_easting(e, zone, hemisphere, line, raw_line)
  n_issue <- gc_check_northing(n, hemisphere, line, raw_line)
  issues <- gc_issues(issues, e_issue, n_issue)
  if (gc_has_error(e_issue) || gc_has_error(n_issue)) {
    return(list(row = NULL, issues = issues))
  }

  row <- list(
    ponto = lab$label %||% NA_character_,
    tipo = "UTM",
    bruto = gc_squish(raw_line),
    linha = as.integer(line),
    lon = NA_real_,
    lat = NA_real_,
    e = as.numeric(e),
    n = as.numeric(n),
    zona = as.integer(zone),
    hemisferio = gc_hemisphere(hemisphere),
    datum = datum
  )
  list(row = row, issues = issues)
}

#' Número lido como coordenada UTM (usado em lapply, sem closure por chamada)
gc_parse_number_coord <- function(tk, dec_sep, line, raw) {
  gc_parse_number(tk, dec_sep = dec_sep, line = line, raw = raw,
                  what = "coordenada")
}

gc_check_easting <- function(e, zone, hemisphere, line, raw) {
  if (is.na(e)) {
    return(gc_empty_issues())
  }
  if (e < 0 || e > 1000000) {
    return(gc_error(
      code = "utm_e_fora_da_faixa",
      title = "Valor de E (leste) fora do intervalo",
      message = paste0("E = ", gc_fmt(e, 2),
                       " m. Em UTM, o E vai de 0 a 1.000.000 m (normalmente entre ",
                       "100.000 e 900.000 m)."),
      hint = "Confira se os valores não estão na ordem N, E ou se há separador decimal trocado.",
      line = line, input = raw
    ))
  }
  if (e < 100000 || e > 900000) {
    return(gc_warning(
      code = "utm_e_incomum",
      title = "E (leste) fora do usual",
      message = paste0("E = ", gc_fmt(e, 2), " m está fora da faixa usual de uma zona UTM ",
                       "(100.000 a 900.000 m)."),
      hint = "O ponto pode estar muito longe do meridiano central da zona.",
      line = line, input = raw
    ))
  }
  gc_empty_issues()
}

gc_check_northing <- function(n, hemisphere, line, raw) {
  if (is.na(n)) {
    return(gc_empty_issues())
  }
  if (n < 0 || n > 10000000) {
    return(gc_error(
      code = "utm_n_fora_da_faixa",
      title = "Valor de N (norte) fora do intervalo",
      message = paste0("N = ", gc_fmt(n, 2),
                       " m. Em UTM, o N vai de 0 a 10.000.000 m ",
                       "(no hemisfério sul, o equador vale 10.000.000 m)."),
      hint = "Confira o hemisfério e se os valores não estão na ordem N, E.",
      line = line, input = raw
    ))
  }
  hemi <- gc_hemisphere(hemisphere)
  if (identical(hemi, "N") && n > 9500000) {
    return(gc_warning(
      code = "utm_n_incomum_norte",
      title = "N (norte) incomum para o hemisfério norte",
      message = paste0("N = ", gc_fmt(n, 2),
                       " m com hemisfério Norte fica muito próximo do equador."),
      hint = "Se o ponto está no Brasil ao sul do equador, o hemisfério é S.",
      line = line, input = raw
    ))
  }
  if (identical(hemi, "S") && n < 1000000) {
    return(gc_warning(
      code = "utm_n_incomum_sul",
      title = "N (norte) incomum para o hemisfério sul",
      message = paste0("N = ", gc_fmt(n, 2),
                       " m com hemisfério Sul indica um ponto muito próximo do polo sul."),
      hint = "Se o ponto está no Brasil, confira o hemisfério e a ordem dos eixos.",
      line = line, input = raw
    ))
  }
  gc_empty_issues()
}

# -----------------------------------------------------------------------------
# Parser geográfico de uma linha
# -----------------------------------------------------------------------------

#' Separa a linha em eixos usando as letras N/S/L/O/E/W
gc_split_by_hemisphere <- function(x) {
  xw <- x
  for (w in names(GC_HEMI_WORDS)) {
    pat <- paste0("(?i)(?<![A-Za-z])", w, "(?![A-Za-z])")
    xw <- gsub(pat, GC_HEMI_WORDS[[w]], xw, perl = TRUE)
  }
  marked <- gsub("(?<![A-Za-z])([NSEWLO])(?![A-Za-z])", "|\u00a7\\1\u00a7|", xw, perl = TRUE)
  parts <- trimws(strsplit(marked, "|", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]

  groups <- list()
  cur <- ""
  pending <- NA_character_
  for (p in parts) {
    if (grepl("^\u00a7[A-Z]\u00a7$", p)) {
      letter <- gsub("\u00a7", "", p)
      # L = Leste / O = Oeste (notação brasileira) -> E / W
      letter <- if (letter == "L") "E" else if (letter == "O") "W" else letter
      if (nzchar(gc_squish(cur))) {
        groups[[length(groups) + 1L]] <- list(text = gc_squish(cur), hemi = letter)
        cur <- ""
      } else {
        pending <- letter
      }
    } else {
      cur <- paste(cur, p)
      if (!is.na(pending)) {
        groups[[length(groups) + 1L]] <- list(text = gc_squish(cur), hemi = pending)
        cur <- ""
        pending <- NA_character_
      }
    }
  }
  if (nzchar(gc_squish(cur)) || !is.na(pending)) {
    groups[[length(groups) + 1L]] <- list(text = gc_squish(cur),
                                          hemi = pending %||% NA_character_)
  }
  groups
}

gc_parse_geographic_line <- function(text, line = NA_integer_, datum = "SIRGAS2000",
                                     dec_sep = "auto", order = "auto") {
  issues <- gc_empty_issues()
  raw_line <- text

  lab <- gc_extract_label(gc_squish(text))
  x <- lab$rest

  groups <- gc_split_by_hemisphere(x)
  axes <- list()

  if (length(groups) == 2L) {
    for (g in groups) {
      parsed <- gc_parse_angle(g$text, axis = "auto", dec_sep = dec_sep,
                               line = line, raw = raw_line)
      value <- parsed$value
      if (isTRUE(parsed$ok) && !is.na(g$hemi) && g$hemi %in% c("S", "W")) {
        value <- -abs(value)
      }
      axes[[length(axes) + 1L]] <- list(
        value = value, ok = parsed$ok, issues = parsed$issues,
        hemi = g$hemi, format = parsed$format, text = g$text
      )
    }
  } else {
    # sem letras (ou com estrutura atípica): usa a contagem de números
    nums <- gc_extract_numbers(x)
    n <- nrow(nums)
    if (n %in% c(2L, 4L, 6L)) {
      half <- n / 2L
      t1 <- paste(nums$token[seq_len(half)], collapse = " ")
      t2 <- paste(nums$token[(half + 1L):n], collapse = " ")
      for (t in c(t1, t2)) {
        parsed <- gc_parse_angle(t, axis = "auto", dec_sep = dec_sep,
                                 line = line, raw = raw_line)
        axes[[length(axes) + 1L]] <- list(
          value = parsed$value, ok = parsed$ok, issues = parsed$issues,
          hemi = NA_character_, format = parsed$format, text = t
        )
      }
    } else {
      return(list(
        row = NULL,
        issues = gc_issues(issues, gc_error(
          code = "geo_estrutura",
          title = "Não foi possível separar latitude e longitude",
          message = paste0("Encontramos ", n, " números em '", gc_squish(raw_line),
                           "' e não conseguimos identificar dois ângulos."),
          hint = paste0("Separe latitude e longitude com N/S e L/O (ex.: ",
                        "23°27'30\"S, 47°12'10\"O) ou digite -23.4583, -47.2028."),
          line = line, input = raw_line
        ))
      ))
    }
  }

  issues <- gc_issues(issues, lapply(axes, `[[`, "issues"))
  if (!all(vapply(axes, `[[`, logical(1), "ok"))) {
    return(list(row = NULL, issues = issues))
  }

  # descobre qual eixo é latitude
  hemi1 <- axes[[1]]$hemi
  hemi2 <- axes[[2]]$hemi
  axis1 <- if (!is.na(hemi1)) gc_hemi_axis(hemi1) else NA_character_
  axis2 <- if (!is.na(hemi2)) gc_hemi_axis(hemi2) else NA_character_

  if (!is.na(axis1) && !is.na(axis2) && axis1 == axis2) {
    return(list(row = NULL, issues = gc_issues(issues, gc_error(
      code = "geo_mesmo_eixo",
      title = "Os dois valores são do mesmo eixo",
      message = paste0("'", gc_squish(raw_line), "' tem duas indicações de ",
                       if (axis1 == "lat") "latitude (N/S)" else "longitude (L/O)"),
      hint = "Informe uma latitude (N/S) e uma longitude (L/O).",
      line = line, input = raw_line
    ))))
  }

  if (is.na(axis1) && !is.na(axis2)) {
    axis1 <- setdiff(c("lat", "lon"), axis2)
  } else if (!is.na(axis1) && is.na(axis2)) {
    axis2 <- setdiff(c("lat", "lon"), axis1)
  } else if (is.na(axis1) && is.na(axis2)) {
    # sem letras: usa a ordem escolhida ou decide pela plausibilidade
    ord <- order
    if (ord == "auto") {
      a <- abs(axes[[1]]$value)
      b <- abs(axes[[2]]$value)
      if (a > 90 && b <= 90) {
        ord <- "lonlat"
      } else if (b > 90 && a <= 90) {
        ord <- "latlon"
      } else if (a >= 28 && a <= 74 && b <= 34) {
        ord <- "lonlat"
      } else if (a <= 34 && b >= 28 && b <= 74) {
        ord <- "latlon"
      } else {
        ord <- "latlon"
        issues <- gc_issues(issues, gc_warning(
          code = "geo_sem_hemisferio",
          title = "Sem indicação de hemisfério",
          message = paste0("'", gc_squish(raw_line), "' não traz N/S nem L/O. ",
                           "Assumimos o padrão de memoriais: latitude primeiro, longitude depois."),
          hint = "Se os valores estiverem invertidos, escolha a ordem nas opções avançadas.",
          line = line, input = raw_line
        ))
      }
    }
    if (ord == "latlon") {
      axis1 <- "lat"; axis2 <- "lon"
    } else {
      axis1 <- "lon"; axis2 <- "lat"
    }
  }

  lat_i <- if (axis1 == "lat") 1L else 2L
  lon_i <- if (axis1 == "lon") 1L else 2L

  lat <- axes[[lat_i]]$value
  lon <- axes[[lon_i]]$value

  if (abs(lat) > 90 + 1e-9) {
    return(list(row = NULL, issues = gc_issues(issues, gc_error(
      code = "geo_latitude_fora_da_faixa",
      title = "Latitude fora do intervalo",
      message = paste0("A latitude calculada foi ", gc_fmt(lat, 6),
                       "°; o máximo é 90°."),
      hint = "Confira se os eixos não estão trocados (a longitude pode ir até 180°).",
      line = line, input = raw_line
    ))))
  }
  if (abs(lon) > 180 + 1e-9) {
    return(list(row = NULL, issues = gc_issues(issues, gc_error(
      code = "geo_longitude_fora_da_faixa",
      title = "Longitude fora do intervalo",
      message = paste0("A longitude calculada foi ", gc_fmt(lon, 6),
                       "°; o máximo é 180°."),
      hint = "Confira a ordem dos eixos e os valores digitados.",
      line = line, input = raw_line
    ))))
  }

  # valores positivos sem letra e sem sinal: assume Brasil (S/W)
  if (is.na(axes[[lat_i]]$hemi) && lat > 0) {
    lat <- -lat
    issues <- gc_issues(issues, gc_warning(
      code = "geo_hemisferio_assumido",
      title = "Hemisfério assumido: Sul",
      message = paste0("A latitude ", gc_fmt(abs(axes[[lat_i]]$value), 6),
                       "° não tem N/S nem sinal negativo. Assumimos latitude Sul ",
                       "(hemisfério do Brasil)."),
      hint = "Se for hemisfério Norte, escreva o valor com N (ex.: 3,5 N).",
      line = line, input = raw_line
    ))
  }
  if (is.na(axes[[lon_i]]$hemi) && lon > 0) {
    lon <- -lon
    issues <- gc_issues(issues, gc_warning(
      code = "geo_hemisferio_assumido",
      title = "Hemisfério assumido: Oeste",
      message = paste0("A longitude ", gc_fmt(abs(axes[[lon_i]]$value), 6),
                       "° não tem L/O nem sinal negativo. Assumimos longitude Oeste ",
                       "(hemisfério do Brasil)."),
      hint = "Se for longitude Leste, escreva o valor com E/L (ex.: 47,2 L).",
      line = line, input = raw_line
    ))
  }

  row <- list(
    ponto = lab$label %||% NA_character_,
    tipo = paste0("GEO-", axes[[lat_i]]$format %||% "DD"),
    bruto = gc_squish(raw_line),
    linha = as.integer(line),
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    e = NA_real_,
    n = NA_real_,
    zona = NA_integer_,
    hemisferio = gc_hemisphere_from_lat(lat),
    datum = datum
  )
  list(row = row, issues = issues)
}

gc_parse_number_value <- function(tk, dec_sep) {
  gc_parse_number(tk, dec_sep = dec_sep)$value
}

# -----------------------------------------------------------------------------
# Parser principal
# -----------------------------------------------------------------------------

#' Lê o texto de entrada e devolve pontos + problemas
#'
#' @param text texto colado pelo usuário.
#' @param type "utm", "geographic" ou "auto".
#' @param datum datum das coordenadas de entrada.
#' @param zone zona UTM padrão (usada quando a linha não traz zona).
#' @param hemisphere hemisfério padrão.
#' @param dec_sep separador decimal ("auto", "." ou ",").
#' @param order ordem dos eixos ("auto", "EN"/"NE" para UTM,
#'   "latlon"/"lonlat" para geográfico).
gc_parse_coordinates <- function(text,
                                 type = c("utm", "geographic", "auto"),
                                 datum = "SIRGAS2000",
                                 zone = NA_integer_,
                                 hemisphere = NA_character_,
                                 dec_sep = "auto",
                                 order = "auto") {
  type <- match.arg(type)
  key <- gc_datum_key(datum)
  if (is.na(key)) {
    return(list(points = gc_points_template(), issues = gc_error(
      code = "datum_invalido", title = "Datum não reconhecido",
      message = gc_datum_error(datum),
      hint = "Escolha o datum na lista do painel de opções."
    )))
  }
  datum <- key

  if (is.character(zone) || is.factor(zone)) {
    zinfo <- gc_parse_zone(zone)
    zone <- zinfo$zone
    if (is.na(hemisphere) && !is.na(zinfo$hemisphere)) {
      hemisphere <- zinfo$hemisphere
    }
  }
  if (!is.null(hemisphere) && length(hemisphere) && !is.na(hemisphere)) {
    hemisphere <- gc_hemisphere(hemisphere)
  } else {
    hemisphere <- NA_character_
  }
  if (!is.null(zone) && length(zone) && !is.na(zone)) {
    zone <- as.integer(zone)
  } else {
    zone <- NA_integer_
  }

  lines <- gc_lines(text)
  if (!nrow(lines)) {
    return(list(points = gc_points_template(), issues = gc_error(
      code = "entrada_vazia",
      title = "Nenhuma coordenada informada",
      message = "O campo de coordenadas está vazio.",
      hint = "Cole as coordenadas (uma por linha) ou clique em 'Usar exemplo'."
    )))
  }

  # separa pontos na mesma linha quando o separador é ';' ou '|',
  # mas só quando cada pedaço faz sentido sozinho
  parts <- list()
  for (i in seq_len(nrow(lines))) {
    raw <- lines$raw[i]
    for (ch in gc_split_line_chunks(raw, type)) {
      parts[[length(parts) + 1L]] <- list(line = lines$line[i], text = ch)
    }
  }

  rows <- list()
  issues <- gc_empty_issues()
  for (p in parts) {
    this_type <- type
    if (type == "auto") {
      this_type <- if (gc_looks_like_utm(p$text)) "utm" else "geographic"
    }
    res <- if (this_type == "utm") {
      gc_parse_utm_line(p$text, line = p$line, datum = datum,
                        zone = zone, hemisphere = hemisphere,
                        dec_sep = dec_sep, order = order)
    } else {
      gc_parse_geographic_line(p$text, line = p$line, datum = datum,
                               dec_sep = dec_sep, order = order)
    }
    issues <- gc_issues(issues, res$issues)
    if (!is.null(res$row)) {
      rows[[length(rows) + 1L]] <- res$row
    }
  }

  points <- gc_rows_to_points(rows)

  if (nrow(points)) {
    labels <- points$ponto
    autos <- is.na(labels) | !nzchar(labels)
    labels[autos] <- paste0("P", seq_len(sum(autos)))
    points$ponto <- make.unique(labels)
  }

  list(points = points, issues = issues)
}

# -----------------------------------------------------------------------------
# Divisão de linhas com mais de um ponto
# -----------------------------------------------------------------------------

#' Um pedaço de linha é uma coordenada completa?
gc_chunk_is_coordinate <- function(ch, type = "auto") {
  if (type == "auto") {
    tipo <- if (gc_looks_like_utm(ch)) "utm" else "geographic"
  } else {
    tipo <- type
  }
  n <- nrow(gc_extract_numbers(ch))
  if (tipo == "utm") {
    return(n >= 2L)
  }
  grupos <- gc_split_by_hemisphere(ch)
  if (length(grupos) == 2L) {
    return(TRUE)
  }
  n %in% c(2L, 4L, 6L)
}

#' Divide uma linha em vários pontos quando separados por ';' ou '|'
#'
#' Só divide quando cada pedaço é, por si só, uma coordenada plausível —
#' evitando quebrar "713110.66|8793023.16" ou "15°47'38\"S;47°52'58\"O".
gc_split_line_chunks <- function(raw, type = c("utm", "geographic", "auto")) {
  type <- match.arg(type)
  if (!grepl("[;|]", raw)) {
    return(raw)
  }
  chunks <- trimws(strsplit(raw, "[;|]")[[1]])
  chunks <- chunks[nzchar(chunks)]
  if (length(chunks) < 2L) {
    return(raw)
  }
  validos <- vapply(chunks, gc_chunk_is_coordinate, logical(1), type = type)
  if (all(validos)) chunks else raw
}

# -----------------------------------------------------------------------------
# Montagem da tabela de pontos (colunar, sem data.frame por linha)
# -----------------------------------------------------------------------------

gc_rows_to_points <- function(rows) {
  n <- length(rows)
  if (!n) {
    return(gc_points_template())
  }
  modelo <- gc_points_template()
  out <- vector("list", length(modelo))
  names(out) <- names(modelo)
  for (nm in names(modelo)) {
    col <- unlist(lapply(rows, .subset2, nm), use.names = FALSE)
    if (is.null(col)) {
      col <- modelo[[nm]][0]
    }
    out[[nm]] <- col
  }
  class(out) <- "data.frame"
  attr(out, "row.names") <- .set_row_names(n)
  out
}
