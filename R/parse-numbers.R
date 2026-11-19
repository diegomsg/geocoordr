# =============================================================================
# geocoordR :: leitura de números, ângulos, graus-minutos-segundos
#
# Regras de projeto:
#  * nunca "chutar" em silêncio: toda decisão automática (separador decimal,
#    separador de milhar, hemisfério, ordem dos eixos) gera um aviso explícito;
#  * todo erro vem com "o que fazer" (hint), em português simples;
#  * o texto original e a linha são sempre preservados na mensagem.
# =============================================================================

# Número com separador decimal e/ou de milhar:
#   713110.66 | 713.110,66 | 713110,66 | 1.234.567 | 23,5 | 8793023.16
GC_NUM_REGEX <- "[-+]?(?:[0-9]{1,3}(?:[.,][0-9]{3}(?![0-9]))+(?:[.,][0-9]+)?|[0-9]+(?:[.,][0-9]+)?)"

# Marcadores de ângulo que aparecem nos memoriais descritivos
gc_normalize_angle_text <- function(x) {
  x <- gc_squish(x)
  # minus unicode
  x <- gsub("\u2212|\u2013|\u2014", "-", x)
  # sinais de grau
  x <- gsub("[\u00b0\u00ba\u02da\u030a\u02dc]", " ", x)
  # minutos (linha e apóstrofos tipográficos)
  x <- gsub("[\u2032\u2035\u2019\u2018\u00b4`]", " ", x)
  # segundos
  x <- gsub("[\u2033\u2036\u201c\u201d\u00b4\u00b4]", " ", x)
  # dois apóstrofos e duas aspas retas viram espaço
  x <- gsub("''", " ", x, fixed = TRUE)
  x <- gsub("\"", " ", x, fixed = TRUE)
  x <- gsub("'", " ", x, fixed = TRUE)
  # dois-pontos e barra como separador de componentes (23:27:30)
  x <- gsub(":", " ", x, fixed = TRUE)
  # palavras de unidade
  x <- gsub("(?i)(?<![A-Za-z])(graus?|g|gr)(?![A-Za-z])", " ", x, perl = TRUE)
  x <- gsub("(?i)(?<![A-Za-z])(minutos?|mins?|m)(?![A-Za-z])", " ", x, perl = TRUE)
  x <- gsub("(?i)(?<![A-Za-z])(segundos?|segs?|s)(?![A-Za-z])", " ", x, perl = TRUE)
  # marcadores d/m/s colados ao número: 23d27m30s
  x <- gsub("([0-9])[dDgGmMsS](?![A-Za-z])", "\\1 ", x, perl = TRUE)
  gc_squish(x)
}

#' Extrai números de um texto, preservando a posição de cada um
gc_extract_numbers <- function(text, regex = GC_NUM_REGEX) {
  if (is.null(text) || is.na(text) || !nzchar(text)) {
    return(data.frame(token = character(), start = integer(), end = integer(),
                      stringsAsFactors = FALSE))
  }
  m <- gregexpr(regex, text, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) {
    return(data.frame(token = character(), start = integer(), end = integer(),
                      stringsAsFactors = FALSE))
  }
  tokens <- regmatches(text, list(m))[[1]]
  data.frame(
    token = tokens,
    start = as.integer(m),
    end = as.integer(m + attr(m, "match.length") - 1L),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Números decimais com separadores brasileiros
# -----------------------------------------------------------------------------

#' Interpreta um token numérico
#'
#' @param token texto do número.
#' @param dec_sep "." , "," ou "auto".
#' @return lista(ok, value, issues, dec_sep, thousands)
# Resultado de falha na leitura de um número (mantido fora da função para não
# recriar closures a cada chamada — esta é a rota mais quente do app)
gc_num_fail <- function(code, title, message, hint = NA_character_,
                        line = NA_integer_, raw = NULL, token = NULL) {
  list(
    ok = FALSE, value = NA_real_, dec_sep = NA_character_, thousands = FALSE,
    issues = gc_error(code = code, title = title, message = message,
                      hint = hint, line = line, input = raw %||% token)
  )
}

gc_num_note <- function(code, title, message, hint = NA_character_,
                        line = NA_integer_, raw = NULL, token = NULL) {
  gc_warning(code = code, title = title, message = message, hint = hint,
             line = line, input = raw %||% token)
}

gc_parse_number <- function(token, dec_sep = "auto",
                            line = NA_integer_, raw = NA_character_,
                            what = "valor") {
  dec_sep <- match.arg(as.character(dec_sep)[1], c("auto", ".", ","))

  tk <- gc_squish(token)
  tk <- gsub("\u2212|\u2013|\u2014", "-", tk)
  if (!nzchar(tk)) {
    return(gc_num_fail("num_vazio", "Número vazio",
                paste0("O ", what, " está em branco."),
                "Preencha o valor ou remova o separador sobrando.", line = line, raw = raw, token = token))
  }
  # milhar separado por espaço: 713 110,66
  if (grepl("^[+-]?[0-9]{1,3}( [0-9]{3})+(,[0-9]+)?$", tk)) {
    tk <- gsub(" ", "", tk)
  }
  neg <- grepl("^-", tk)
  tk <- sub("^[+-]", "", tk)
  if (!grepl("^[0-9.,]+$", tk)) {
    return(gc_num_fail(
      "num_caractere_invalido",
      "Caractere inesperado no número",
      paste0("'", token, "' contém caracteres que não são dígitos, vírgula ",
             "ou ponto. Valores devem ser apenas números."),
      "Remova símbolos, letras ou unidades (m, km, °) do valor numérico."
    , line = line, raw = raw, token = token))
  }
  if (!grepl("[0-9]", tk)) {
    return(gc_num_fail("num_sem_digito", "Número incompleto",
                paste0("'", token, "' não tem nenhum dígito."),
                "Digite o número completo, por exemplo 713110.66.", line = line, raw = raw, token = token))
  }

  has_dot <- grepl(".", tk, fixed = TRUE)
  has_comma <- grepl(",", tk, fixed = TRUE)

  decimal_mark <- NA_character_
  thousands <- FALSE
  extra_issue <- gc_empty_issues()

  if (has_dot && has_comma) {
    if (grepl("^[0-9]{1,3}(\\.[0-9]{3})+(,[0-9]+)?$", tk)) {
      # 713.110,66  -> ponto de milhar, vírgula decimal
      decimal_mark <- ","
      thousands <- TRUE
    } else if (grepl("^[0-9]{1,3}(,[0-9]{3})+(\\.\\d+)?$", tk)) {
      # 713,110.66  -> vírgula de milhar, ponto decimal
      decimal_mark <- "."
      thousands <- TRUE
    } else {
      return(gc_num_fail(
        "num_separadores_ambiguos",
        "Dois separadores diferentes no mesmo número",
        paste0("Não foi possível saber o que é separador de milhar e o que é ",
               "separador decimal em '", token, "'."),
        paste0("Escreva sem separador de milhar (713110.66 ou 713110,66) ou ",
               "use o formato 713.110,66.")
      , line = line, raw = raw, token = token))
    }
  } else if (has_dot || has_comma) {
    mark <- if (has_dot) "." else ","
    n_mark <- lengths(regmatches(tk, gregexpr(mark, tk, fixed = TRUE)))
    n_digits_after <- nchar(sub(paste0("^.*", if (mark == ".") "\\." else ","), "", tk))
    # só pode ser separador de milhar se o que vem antes tiver 1 a 3 dígitos
    int_digits <- nchar(sub(paste0(mark, "[0-9]+$"), "", tk))

    if (n_mark > 1L) {
      if (grepl("^[0-9]{1,3}([.,][0-9]{3})+$", tk)) {
        decimal_mark <- NA_character_
        thousands <- TRUE
      } else {
        return(gc_num_fail(
          "num_separador_repetido",
          "Separador repetido no número",
          paste0("'", token, "' tem vários '", mark, "' seguidos."),
          "Use um único separador decimal (713110.66) ou o padrão de milhar (713.110,66)."
        , line = line, raw = raw, token = token))
      }
    } else if (dec_sep != "auto" && mark == dec_sep) {
      decimal_mark <- mark
    } else if (dec_sep != "auto" && mark != dec_sep) {
      # separador diferente do escolhido pelo usuário
      if (n_digits_after == 3L && int_digits <= 3L) {
        decimal_mark <- NA_character_
        thousands <- TRUE
        extra_issue <- gc_num_note(
          "num_milhar_interpretado",
          "Separador de milhar interpretado",
          paste0("'", token, "' foi lido como ", "número com separador de milhar",
                 " porque você escolheu '", dec_sep, "' como separador decimal."),
          paste0("Se a intenção era ", gc_fmt(as.numeric(gsub(mark, ".", tk, fixed = TRUE)), 6),
                 ", ajuste o separador decimal nas opções avançadas."),
          line = line, raw = raw, token = token
        )
      } else {
        decimal_mark <- mark
        extra_issue <- gc_num_note(
          "num_separador_divergente",
          "Separador decimal diferente do escolhido",
          paste0("Você escolheu '", dec_sep, "' como separador decimal, mas '",
                 token, "' usa '", mark, "'. Interpretamos como decimal."),
          "Padronize o separador decimal nas opções avançadas.",
          line = line, raw = raw, token = token
        )
      }
    } else {
      # modo auto
      decimal_mark <- mark
      if (n_digits_after == 3L && int_digits <= 3L) {
        extra_issue <- gc_num_note(
          "num_milhar_suspeito",
          "Confira o separador de milhar",
          paste0("Lemos '", token, "' com '", mark, "' como separador decimal (",
                 gc_fmt(as.numeric(gsub(",", ".", tk)), digits = 3), "). Se o '",
                 mark, "' for separador de milhar, o valor correto seria ",
                 gc_fmt(as.numeric(gsub("[.,]", "", tk)), digits = 0), "."),
          paste0("Para evitar dúvida, escreva números grandes sem separador de ",
                 "milhar (ex.: 713110.66)."),
          line = line, raw = raw, token = token
        )
      }
    }
  }

  normalized <- tk
  if (thousands) {
    if (!is.na(decimal_mark)) {
      # há parte decimal: tudo antes do separador decimal é milhar
      dec_pattern <- paste0("\\", decimal_mark, "[0-9]+$")
      pos <- regexpr(dec_pattern, tk)
      int_part <- gsub("[.,]", "", substr(tk, 1L, pos - 1L))
      dec_part <- substring(tk, pos + 1L)
      normalized <- paste0(int_part, ".", dec_part)
    } else {
      normalized <- gsub("[.,]", "", tk)
    }
  } else if (!is.na(decimal_mark)) {
    normalized <- gsub(decimal_mark, ".", tk, fixed = TRUE)
  }

  value <- suppressWarnings(as.numeric(normalized))
  if (is.na(value) || !is.finite(value)) {
    return(gc_num_fail("num_invalido", "Número inválido",
                paste0("Não foi possível interpretar '", token, "' como número."),
                "Exemplos válidos: 713110.66 | 713.110,66 | -23,5", line = line, raw = raw, token = token))
  }
  if (abs(value) >= 1e12) {
    return(gc_num_fail("num_muito_grande", "Número fora de escala",
                paste0("'", token, "' resultou em ", gc_fmt(value, 0),
                       ", um valor muito grande para coordenadas."),
                "Verifique se não há dígitos extras ou separadores trocados.", line = line, raw = raw, token = token))
  }
  if (neg) {
    value <- -abs(value)
  }

  list(
    ok = TRUE,
    value = value,
    dec_sep = decimal_mark,
    thousands = thousands,
    issues = extra_issue
  )
}

# -----------------------------------------------------------------------------
# Hemisfério / quadrante
# -----------------------------------------------------------------------------

GC_HEMI_WORDS <- c(
  NORTE = "N", NORTH = "N", SUL = "S", SOUTH = "S",
  LESTE = "E", EAST = "E", OESTE = "W", WEST = "W"
)

gc_hemi_axis <- function(letter) {
  ifelse(letter %in% c("N", "S"), "lat", "lon")
}

#' Extrai o hemisfério/quadrante de um texto de ângulo
#'
#' Aceita N/S/E/W e também L (Leste) e O (Oeste), usados em documentos
#' brasileiros, em qualquer posição (prefixo ou sufixo).
gc_extract_hemisphere <- function(x) {
  xi <- gc_squish(x)
  letters <- character(0)
  issues <- gc_empty_issues()

  # palavras por extenso
  for (word in names(GC_HEMI_WORDS)) {
    pat <- paste0("(?i)(?<![A-Za-z])", word, "(?![A-Za-z])")
    if (grepl(pat, xi, perl = TRUE)) {
      letters <- c(letters, GC_HEMI_WORDS[[word]])
      xi <- gsub(pat, " ", xi, perl = TRUE)
    }
  }
  # letras soltas (não podem estar coladas em outras letras)
  m <- gregexpr("(?<![A-Za-z])([NSEWLO])(?![A-Za-z])", xi, perl = TRUE)[[1]]
  if (!(length(m) == 1L && m[1] == -1L)) {
    found <- regmatches(xi, list(m))[[1]]
    letters <- c(letters, found)
    # remove as letras encontradas, mantendo o resto
    xi <- gsub("(?<![A-Za-z])([NSEWLO])(?![A-Za-z])", " ", xi, perl = TRUE)
  }
  letters <- toupper(letters)
  letters[letters == "L"] <- "E"
  letters[letters == "O"] <- "W"
  letters <- unique(letters)

  axes <- gc_hemi_axis(letters)
  if (length(letters) > 2L || (length(axes) != length(unique(axes)))) {
    issues <- gc_error(
      code = "hemi_conflito",
      title = "Hemisfério repetido",
      message = paste0("Encontramos as indicações ", paste(letters, collapse = " / "),
                       " na mesma coordenada; só é possível uma para latitude e ",
                       "uma para longitude."),
      hint = "Use, por exemplo, 23°27'30\"S para latitude e 47°12'10\"O para longitude.",
      input = x
    )
  }
  list(
    text = gc_squish(xi),
    letters = letters,
    axes = axes,
    issues = issues
  )
}

# -----------------------------------------------------------------------------
# Ângulos: graus decimais, graus + minutos e GMS
# -----------------------------------------------------------------------------

#' Versões com argumentos fixos, para uso em lapply/vapply (evita recriar
#' closures a cada chamada, que é o gargalo de desempenho do R com JIT)
gc_parse_number_angle <- function(tk, dec_sep, line, raw) {
  gc_parse_number(tk, dec_sep = dec_sep, line = line, raw = raw,
                  what = "ângulo")
}

#' Interpreta um ângulo (coordenada de um eixo)
#'
#' @param token texto do ângulo, já sem o outro eixo.
#' @param axis "lat", "lon" ou "auto".
#' @param dec_sep separador decimal (ver gc_parse_number).
#' @return lista(ok, value, axis, format, issues)
gc_parse_angle <- function(token, axis = "auto", dec_sep = "auto",
                           line = NA_integer_, raw = NA_character_) {
  issues <- gc_empty_issues()
  hemi <- gc_extract_hemisphere(token)
  issues <- gc_issues(issues, hemi$issues)

  if (length(hemi$axes) == 1L) {
    axis <- hemi$axes
  } else if (length(hemi$axes) > 1L) {
    axis <- "auto"
  }

  txt <- gc_normalize_angle_text(hemi$text)
  nums <- gc_extract_numbers(txt)

  if (!nrow(nums)) {
    return(list(
      ok = FALSE, value = NA_real_, axis = axis, format = NA_character_,
      issues = gc_issues(issues, gc_error(
        code = "angulo_sem_numero",
        title = "Nenhum número encontrado",
        message = paste0("Não encontramos um ângulo em '", gc_squish(token), "'."),
        hint = "Exemplos aceitos: -23.5505 | 23,5505 | 23°27'30,5\"S | 23 27 30,5 S",
        line = line, input = raw %||% token
      ))
    ))
  }
  if (nrow(nums) > 3L) {
    return(list(
      ok = FALSE, value = NA_real_, axis = axis, format = NA_character_,
      issues = gc_issues(issues, gc_error(
        code = "angulo_componentes_demais",
        title = "Ângulo com componentes demais",
        message = paste0("'", gc_squish(token), "' tem ", nrow(nums),
                         " números; um ângulo tem no máximo graus, minutos e segundos."),
        hint = "Confira se a latitude e a longitude não ficaram no mesmo campo.",
        line = line, input = raw %||% token
      ))
    ))
  }

  parsed <- lapply(nums$token, gc_parse_number_angle,
                   dec_sep = dec_sep, line = line, raw = raw)
  issues <- gc_issues(issues, lapply(parsed, `[[`, "issues"))
  if (!all(vapply(parsed, `[[`, logical(1), "ok"))) {
    return(list(
      ok = FALSE, value = NA_real_, axis = axis, format = NA_character_,
      issues = issues
    ))
  }
  values <- abs(vapply(parsed, `[[`, numeric(1), "value"))
  n <- length(values)
  fmt <- c("DD", "DM", "DMS")[n]

  deg <- values[1]
  minutes <- if (n >= 2L) values[2] else 0
  seconds <- if (n >= 3L) values[3] else 0

  if (n >= 2L && abs(deg - round(deg)) > 1e-9) {
    return(list(
      ok = FALSE, value = NA_real_, axis = axis, format = fmt,
      issues = gc_issues(issues, gc_error(
        code = "angulo_grau_fracionado",
        title = "Graus com casas decimais e minutos",
        message = paste0("Em '", gc_squish(token), "' os graus aparecem com ",
                         "casas decimais e também há minutos/segundos."),
        hint = "Use só graus decimais (23,45847) ou graus inteiros com minutos (23°27,5').",
        line = line, input = raw %||% token
      ))
    ))
  }
  if (minutes >= 60) {
    return(list(
      ok = FALSE, value = NA_real_, axis = axis, format = fmt,
      issues = gc_issues(issues, gc_error(
        code = "angulo_minuto_invalido",
        title = "Minuto inválido",
        message = paste0("Encontramos ", gc_fmt(minutes, 4),
                         " minutos; o máximo é 59,9999."),
        hint = "Converta o excedente em graus (60 minutos = 1 grau).",
        line = line, input = raw %||% token
      ))
    ))
  }
  if (seconds >= 60) {
    return(list(
      ok = FALSE, value = NA_real_, axis = axis, format = fmt,
      issues = gc_issues(issues, gc_error(
        code = "angulo_segundo_invalido",
        title = "Segundo inválido",
        message = paste0("Encontramos ", gc_fmt(seconds, 4),
                         " segundos; o máximo é 59,9999."),
        hint = "Converta o excedente em minutos (60 segundos = 1 minuto).",
        line = line, input = raw %||% token
      ))
    ))
  }

  # sinal explícito no primeiro número
  sign_value <- parsed[[1]]$value
  value <- abs(sign_value) + minutes / 60 + seconds / 3600
  negative <- FALSE
  if (sign_value < 0) {
    negative <- TRUE
  }
  hemi_letter <- hemi$letters[1]
  if (!is.na(hemi_letter)) {
    hemi_negative <- hemi_letter %in% c("S", "W")
    if (negative && !hemi_negative) {
      issues <- gc_issues(issues, gc_warning(
        code = "angulo_sinal_conflita",
        title = "Sinal e hemisfério em conflito",
        message = paste0("'", gc_squish(token), "' tem sinal negativo e indicação '",
                         hemi_letter, "' (positiva). Mantivemos o sinal negativo."),
        hint = "Use apenas uma forma: -23.5505 ou 23.5505 S.",
        line = line, input = raw %||% token
      ))
    }
    negative <- negative || hemi_negative
  }
  # sem sinal e sem hemisfério: decide depois (Brasil -> negativo)
  if (negative) {
    value <- -value
  }

  limit <- switch(
    axis,
    lat = 90,
    lon = 180,
    180
  )
  if (abs(value) > limit + 1e-9) {
    return(list(
      ok = FALSE, value = NA_real_, axis = axis, format = fmt,
      issues = gc_issues(issues, gc_error(
        code = "angulo_fora_do_limite",
        title = paste0("Ângulo fora do limite de ", limit, "°"),
        message = paste0("O valor calculado foi ", gc_fmt(value, 6),
                         "°, mas ", if (axis == "lat") "a latitude" else "a longitude",
                         " vai de -", limit, "° a ", limit, "°."),
        hint = "Verifique graus, minutos e segundos (ou se os eixos não estão trocados).",
        line = line, input = raw %||% token
      ))
    ))
  }

  list(
    ok = TRUE,
    value = value,
    axis = axis,
    format = fmt,
    hemisphere = hemi_letter %||% NA_character_,
    issues = issues
  )
}

#' Converte graus decimais em texto GMS
gc_format_dms <- function(x, digits = 2) {
  if (length(x) != 1L) {
    return(vapply(x, gc_format_dms, character(1), digits = digits))
  }
  if (is.null(x) || is.na(x) || !is.finite(x)) {
    return("—")
  }
  dir <- if (x >= 0) "N" else "S"
  total <- abs(x)
  d <- floor(total)
  min_full <- (total - d) * 60
  m <- floor(min_full)
  s <- (min_full - m) * 60
  # arredondamento que pode estourar 60 s
  s <- round(s, digits)
  if (s >= 60) {
    s <- s - 60
    m <- m + 1
  }
  if (m >= 60) {
    m <- m - 60
    d <- d + 1
  }
  s_txt <- formatC(s, format = "f", digits = digits, decimal.mark = ",", big.mark = "")
  need <- (digits + 3L) - nchar(s_txt, type = "chars")
  if (need > 0) {
    s_txt <- paste0(strrep("0", need), s_txt)
  }
  sprintf("%d°%02d'%s\"%s", d, m, s_txt, dir)
}

#' Igual a \code{gc_format_dms}, mas usa N/S para latitude e E/W para longitude
gc_format_dms_axis <- function(x, axis = c("lat", "lon"), digits = 2) {
  axis <- match.arg(axis)
  if (length(x) != 1L) {
    return(vapply(x, gc_format_dms_axis, character(1), axis = axis, digits = digits))
  }
  out <- gc_format_dms(x, digits = digits)
  if (axis == "lon" && !is.na(x)) {
    out <- sub("N$", "E", out)
    out <- sub("S$", "W", out)
  }
  out
}
