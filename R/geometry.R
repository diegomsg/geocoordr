# =============================================================================
# geocoordR :: geometria plana (área, perímetro, lados, autointerseção)
#
# Tudo é calculado no plano UTM SIRGAS 2000 de saída, que é como os memoriais
# descritivos brasileiros expressam área e confrontações (área plana).
# =============================================================================

#' Área de polígono pela fórmula de Gauss (shoelace)
gc_polygon_area <- function(x, y) {
  n <- length(x)
  if (n < 3L) {
    return(NA_real_)
  }
  if (all(is.na(x)) || all(is.na(y))) {
    return(NA_real_)
  }
  x1 <- c(x, x[1])
  y1 <- c(y, y[1])
  abs(sum(x1[-length(x1)] * y1[-1] - x1[-1] * y1[-length(y1)])) / 2
}

#' Perímetro (assumindo polígono fechado)
gc_polygon_perimeter <- function(x, y) {
  if (length(x) < 2L) {
    return(NA_real_)
  }
  sum(gc_segment_lengths(x, y))
}

#' Comprimento de cada lado, fechando o polígono
gc_segment_lengths <- function(x, y) {
  n <- length(x)
  x1 <- c(x, if (n >= 3L) x[1] else numeric(0))
  y1 <- c(y, if (n >= 3L) y[1] else numeric(0))
  sqrt(diff(x1)^2 + diff(y1)^2)
}

#' Distância plana e azimute de cada lado do polígono
gc_sides <- function(x, y, labels = NULL) {
  n <- length(x)
  if (n < 2L) {
    return(data.frame(
      de = character(), para = character(), distancia = numeric(),
      azimute = numeric(), stringsAsFactors = FALSE
    ))
  }
  to <- c(seq_len(n)[-1], if (n >= 3L) 1L else integer(0))
  from <- seq_len(length(to))
  dx <- x[to] - x[from]
  dy <- y[to] - y[from]
  if (is.null(labels)) {
    labels <- paste0("P", seq_len(n))
  }
  az <- (atan2(dx, dy) * 180 / pi) %% 360
  az[az >= 359.999999] <- 0
  data.frame(
    de = labels[from],
    para = labels[to],
    distancia = sqrt(dx^2 + dy^2),
    azimute = az,
    stringsAsFactors = FALSE
  )
}

#' Fecha o anel do polígono (acrescenta o primeiro ponto, se necessário)
gc_close_ring <- function(x, y) {
  n <- length(x)
  if (n < 3L) {
    return(list(x = x, y = y, closed = FALSE, added = FALSE))
  }
  same <- isTRUE(all.equal(c(x[n], y[n]), c(x[1], y[1]), tolerance = 1e-9))
  if (same) {
    return(list(x = x, y = y, closed = TRUE, added = FALSE))
  }
  list(x = c(x, x[1]), y = c(y, y[1]), closed = TRUE, added = TRUE)
}

#' Testa autointerseção de um polígono simples
#'
#' @return data.frame com os pares de lados que se cruzam (vazio se não houver).
gc_find_self_intersections <- function(x, y) {
  n <- length(x)
  empty <- data.frame(i = integer(), j = integer(), stringsAsFactors = FALSE)
  if (n < 4L) {
    return(empty)
  }
  # arestas i -> i+1 (com fechamento do anel)
  x1 <- x
  y1 <- y
  x2 <- c(x[-1], x[1])
  y2 <- c(y[-1], y[1])
  minx <- pmin(x1, x2)
  maxx <- pmax(x1, x2)
  miny <- pmin(y1, y2)
  maxy <- pmax(y1, y2)

  hits_i <- integer(0)
  hits_j <- integer(0)
  for (i in seq_len(n - 1L)) {
    j <- seq.int(i + 1L, n)
    # arestas vizinhas compartilham vértice: ignorar
    if (length(j)) {
      j <- j[!(j == i + 1L | (i == 1L & j == n))]
    }
    if (!length(j)) {
      next
    }
    # pré-filtro por caixa envolvente (descarta a maioria dos pares)
    cand <- maxx[i] >= minx[j] & minx[i] <= maxx[j] &
      maxy[i] >= miny[j] & miny[i] <= maxy[j]
    j <- j[cand]
    if (!length(j)) {
      next
    }
    d1 <- (x2[j] - x1[j]) * (y1[i] - y1[j]) - (y2[j] - y1[j]) * (x1[i] - x1[j])
    d2 <- (x2[j] - x1[j]) * (y2[i] - y1[j]) - (y2[j] - y1[j]) * (x2[i] - x1[j])
    d3 <- (x2[i] - x1[i]) * (y1[j] - y1[i]) - (y2[i] - y1[i]) * (x1[j] - x1[i])
    d4 <- (x2[i] - x1[i]) * (y2[j] - y1[i]) - (y2[i] - y1[i]) * (x2[j] - x1[i])
    cruz <- ((d1 > 0 & d2 < 0) | (d1 < 0 & d2 > 0)) &
      ((d3 > 0 & d4 < 0) | (d3 < 0 & d4 > 0))
    if (any(cruz)) {
      hits_i <- c(hits_i, rep.int(i, sum(cruz)))
      hits_j <- c(hits_j, j[cruz])
    }
  }
  if (!length(hits_i)) {
    return(empty)
  }
  data.frame(i = hits_i, j = hits_j, stringsAsFactors = FALSE)
}

gc_orient <- function(ax, ay, bx, by, cx, cy) {
  (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
}

gc_segments_cross <- function(p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y) {
  d1 <- gc_orient(p3x, p3y, p4x, p4y, p1x, p1y)
  d2 <- gc_orient(p3x, p3y, p4x, p4y, p2x, p2y)
  d3 <- gc_orient(p1x, p1y, p2x, p2y, p3x, p3y)
  d4 <- gc_orient(p1x, p1y, p2x, p2y, p4x, p4y)
  ((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) &&
    ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))
}

#' Centroide dos vértices (média simples, suficiente para enquadrar o mapa)
gc_centroid <- function(lon, lat) {
  ok <- is.finite(lon) & is.finite(lat)
  if (!any(ok)) {
    return(c(lon = NA_real_, lat = NA_real_))
  }
  c(lon = mean(lon[ok]), lat = mean(lat[ok]))
}

#' Caixa envolvente
gc_bbox <- function(lon, lat) {
  ok <- is.finite(lon) & is.finite(lat)
  if (!any(ok)) {
    return(list(lon_min = NA_real_, lon_max = NA_real_,
                lat_min = NA_real_, lat_max = NA_real_))
  }
  list(
    lon_min = min(lon[ok]), lon_max = max(lon[ok]),
    lat_min = min(lat[ok]), lat_max = max(lat[ok])
  )
}
