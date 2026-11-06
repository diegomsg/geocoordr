# =============================================================================
# Gera tests/fixtures/proj-reference.csv com valores de referência do PROJ.
#
# Requer o PROJ (cs2cs/cct) instalado. Rode a partir da raiz do projeto:
#
#   Rscript data-raw/gerar-fixtures-proj.R
#
# O arquivo gerado contém:
#   * op = "fwd":   geográfico -> UTM (SIRGAS 2000 / GRS80), vários fusos;
#   * op = "inv":   UTM -> geográfico;
#   * op = "datum-fwd"/"datum-inv": transformação SAD69 <-> SIRGAS 2000 e
#     Córrego Alegre 1970-72 <-> SIRGAS 2000, com os mesmos parâmetros de
#     Helmert usados pelo app (EPSG 15485 e 6193).
#
# Os testes (tests/testthat/test-geo-math.R) comparam a implementação em R
# puro com estes valores (tolerância de 1 mm na projeção e 1e-8° no datum).
# =============================================================================

suppressWarnings({
  raiz <- getwd()
  if (!file.exists(file.path(raiz, "app.R"))) {
    stop("Rode a partir da raiz do projeto.")
  }

  pontos <- data.frame(
    lon = c(-47.8825, -46.6333, -60.0217, -38.5014, -51.2300, -70.5000,
            -34.8631, -48.5482, -56.0967, -62.8000, -45.0000, -57.6000,
            -73.0000, -35.2000, -52.2000, -44.3000, -66.8000, -63.1000,
            -49.2000, -40.3000),
    lat = c(-15.7939, -23.5505, -3.1019, -12.9714, -30.0346, -9.9747,
            -8.0539, -27.5969, -2.5307, -13.0000, -21.0000, -3.1000,
            -7.2000, -5.8000, -12.9000, -19.9000, -9.9000, -16.6000,
            -25.5000, -20.3000)
  )
  pontos$zone <- floor((pontos$lon + 180) / 6) + 1
  pontos$hemi <- ifelse(pontos$lat >= 0, "N", "S")

  cs2cs <- function(entrada, args, formato) {
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    writeLines(entrada, f)
    out <- system2("cs2cs", c("-f", formato, args, f), stdout = TRUE, stderr = "")
    as.numeric(strsplit(trimws(out[1]), "[[:space:]]+")[[1]])
  }

  cct <- function(entrada, pipeline, digitos = 10) {
    out <- system2("cct", c("-d", as.character(digitos), pipeline),
                   input = entrada, stdout = TRUE, stderr = "")
    v <- suppressWarnings(as.numeric(strsplit(trimws(out[1]), "[[:space:]]+")[[1]][1:2]))
    stopifnot(length(v) == 2, all(is.finite(v)))
    v
  }

  # 1) geográfico -> UTM
  fwd <- do.call(rbind, lapply(seq_len(nrow(pontos)), function(i) {
    r <- pontos[i, ]
    xy <- cs2cs(
      sprintf("%.8f %.8f", r$lon, r$lat),
      c("+proj=longlat", "+ellps=GRS80", "+to",
        sprintf("+proj=utm +zone=%d +%s +ellps=GRS80", r$zone,
                if (r$hemi == "S") "south" else "north")),
      "%.4f"
    )
    data.frame(lon = r$lon, lat = r$lat, zone = r$zone, hemi = r$hemi,
               e = xy[1], n = xy[2], datum = "SIRGAS2000", op = "fwd")
  }))

  # 2) UTM -> geográfico
  inv <- do.call(rbind, lapply(seq_len(nrow(fwd)), function(i) {
    r <- fwd[i, ]
    ll <- cs2cs(
      sprintf("%.3f %.3f", r$e, r$n),
      c(sprintf("+proj=utm +zone=%d +%s +ellps=GRS80", r$zone,
                if (r$hemi == "S") "south" else "north"),
        "+to", "+proj=longlat +ellps=GRS80"),
      "%.10f"
    )
    data.frame(lon = ll[1], lat = ll[2], zone = r$zone, hemi = r$hemi,
               e = r$e, n = r$n, datum = "SIRGAS2000", op = "inv")
  }))

  # 3) transformação de datum (mesmos parâmetros do app)
  casos <- list(
    list(key = "SAD69", ellps = "aust_SA", x = -67.35, y = 3.88, z = -38.22),
    list(key = "CORREGO_ALEGRE_1970", ellps = "intl",
         x = -206.05, y = 168.28, z = -3.82)
  )
  pipeline <- function(cs, inverso = FALSE) {
    helm <- sprintf("+step +proj=helmert +x=%s +y=%s +z=%s",
                    if (inverso) -cs$x else cs$x,
                    if (inverso) -cs$y else cs$y,
                    if (inverso) -cs$z else cs$z)
    if (!inverso) {
      c("+proj=pipeline",
        "+step +proj=unitconvert +xy_in=deg +xy_out=rad",
        paste0("+step +proj=cart +ellps=", cs$ellps), helm,
        "+step +inv +proj=cart +ellps=GRS80",
        "+step +proj=unitconvert +xy_in=rad +xy_out=deg")
    } else {
      c("+proj=pipeline",
        "+step +proj=unitconvert +xy_in=deg +xy_out=rad",
        "+step +proj=cart +ellps=GRS80", helm,
        paste0("+step +inv +proj=cart +ellps=", cs$ellps),
        "+step +proj=unitconvert +xy_in=rad +xy_out=deg")
    }
  }
  dt <- do.call(rbind, lapply(casos, function(cs) {
    do.call(rbind, lapply(c("fwd", "inv"), function(dir) {
      p <- pipeline(cs, inverso = dir == "inv")
      do.call(rbind, lapply(seq_len(nrow(pontos)), function(i) {
        ll <- cct(sprintf("%.10f %.10f 0", pontos$lon[i], pontos$lat[i]), p)
        data.frame(lon = pontos$lon[i], lat = pontos$lat[i], zone = NA_real_,
                   hemi = NA_character_, e = ll[1], n = ll[2],
                   datum = cs$key, op = paste0("datum-", dir))
      }))
    }))
  }))

  ref <- rbind(fwd, inv, dt)[, c("lon", "lat", "zone", "hemi", "e", "n",
                                 "datum", "op")]
  saida <- file.path(raiz, "tests", "fixtures", "proj-reference.csv")
  utils::write.csv(ref, saida, row.names = FALSE)
  cat("Gerado:", saida, "com", nrow(ref), "linhas\n")
})
