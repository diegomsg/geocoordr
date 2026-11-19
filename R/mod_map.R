# =============================================================================
# geocoordR :: módulo de mapa
#
# Usa Leaflet direto do CDN via htmlDependency: nenhuma dependência pesada
# (leaflet/tmap/sf) é necessária para ver o imóvel no mapa.
# =============================================================================

#' Monta o payload enviado ao mapa
gc_map_payload <- function(result) {
  pts <- result$points
  if (is.null(pts) || !nrow(pts)) {
    return(list(points = list(), polygon = list()))
  }
  ok <- is.finite(pts$lon) & is.finite(pts$lat)
  idx <- which(ok)
  points <- lapply(idx, function(i) {
    popup <- paste0(
      "<strong>", htmltools::htmlEscape(pts$ponto[i]), "</strong><br/>",
      "Latitude: ", gc_fmt(pts$lat[i], 8, dec = "."), "<br/>",
      "Longitude: ", gc_fmt(pts$lon[i], 8, dec = "."), "<br/>",
      if (is.finite(pts$e[i])) {
        paste0("E: ", gc_fmt(pts$e[i], 3, dec = "."), " m<br/>",
               "N: ", gc_fmt(pts$n[i], 3, dec = "."), " m<br/>",
               "Zona: ", pts$zona[i], pts$hemisferio[i], "<br/>")
      } else "",
      "GMS: ", pts$lat_dms[i], " / ", pts$lon_dms[i]
    )
    list(label = pts$ponto[i], lat = pts$lat[i], lon = pts$lon[i], popup = popup)
  })

  polygon <- list()
  if (length(result$polygon$lon) >= 4L) {
    polygon <- lapply(seq_along(result$polygon$lon), function(i) {
      c(result$polygon$lon[i], result$polygon$lat[i])
    })
  }
  list(
    points = points,
    polygon = polygon,
    has_polygon = length(result$polygon$lon) >= 4L,
    area = if (is.finite(result$polygon$area_m2 %||% NA_real_)) {
      gc_fmt_area(result$polygon$area_m2)
    } else {
      NULL
    }
  )
}

gc_map_dependency <- function() {
  htmltools::htmlDependency(
    name = "leaflet",
    version = "1.9.4",
    src = c(href = "https://unpkg.com/leaflet@1.9.4/dist"),
    script = "leaflet.js",
    stylesheet = "leaflet.css"
  )
}

gc_map_script <- function() {
  htmltools::tags$script(htmltools::HTML("
(function () {
  if (window.gcMapReady) return;
  window.gcMapReady = true;
  window.gcMaps = window.gcMaps || {};

  window.gcMapRender = function (el, msg) {
    var map = el._gcMap;
    if (!map) {
      map = L.map(el, { zoomControl: true });
      var osm = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19, attribution: '&copy; OpenStreetMap'
      });
      var sat = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        maxZoom: 19, attribution: 'Esri, Maxar, Earthstar Geographics'
      });
      var topo = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}', {
        maxZoom: 19, attribution: 'Esri'
      });
      osm.addTo(map);
      L.control.layers({ 'Mapa': osm, 'Satélite': sat, 'Topográfico': topo }, null,
                       { collapsed: true, position: 'topright' }).addTo(map);
      L.control.scale({ imperial: false, metric: true, position: 'bottomleft' }).addTo(map);
      el._gcMap = map;
      el._gcLayer = L.layerGroup().addTo(map);
      window.gcMaps[el.id] = el;
    }
    var layer = el._gcLayer;
    layer.clearLayers();
    var latlngs = [];
    (msg.points || []).forEach(function (p) {
      var ll = [p.lat, p.lon];
      latlngs.push(ll);
      L.marker(ll, { title: p.label })
        .bindTooltip(p.label, { permanent: true, direction: 'top', offset: [0, -10] })
        .bindPopup(p.popup)
        .addTo(layer);
    });
    if (msg.polygon && msg.polygon.length > 2) {
      L.polygon(msg.polygon.map(function (c) { return [c[1], c[0]]; }), {
        color: '#0f766e', weight: 2, fillColor: '#0f766e', fillOpacity: 0.12
      }).addTo(layer);
    }
    el._gcBounds = latlngs.length > 1 ? L.latLngBounds(latlngs) : null;
    el._gcSingle = latlngs.length === 1 ? latlngs[0] : null;
    setTimeout(function () { window.gcMapFit(el); }, 120);
    setTimeout(function () { window.gcMapFit(el); }, 600);
  };

  // enquadra os pontos; chamado também quando a aba do mapa aparece
  window.gcMapFit = function (el) {
    if (!el || !el._gcMap) return;
    el._gcMap.invalidateSize();
    if (el._gcSingle) {
      el._gcMap.setView(el._gcSingle, 17);
    } else if (el._gcBounds) {
      el._gcMap.fitBounds(el._gcBounds.pad(0.3));
    }
  };

  if (window.Shiny) {
    Shiny.addCustomMessageHandler('gc-map', function (msg) {
      var el = document.getElementById(msg.id);
      if (el) window.gcMapRender(el, msg);
    });
    Shiny.addCustomMessageHandler('gc-map-clear', function (msg) {
      var el = document.getElementById(msg.id);
      if (el && el._gcLayer) el._gcLayer.clearLayers();
    });
  }

  document.addEventListener('shown.bs.tab', function () {
    Object.keys(window.gcMaps).forEach(function (k) {
      try { window.gcMapFit(window.gcMaps[k]); } catch (e) {}
    });
  });
})();
"))
}

#' UI do mapa
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    gc_map_dependency(),
    htmltools::tags$div(id = ns("map"), class = "gc-map"),
    gc_map_script(),
    htmltools::tags$div(
      class = "gc-help",
      style = "margin-top: .5rem;",
      shiny::icon("circle-info"),
      " Arraste e use a roda do mouse para ampliar. No canto superior direito",
      " você alterna entre mapa, satélite e topográfico."
    )
  )
}

#' Servidor do mapa
mod_map_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    enviar <- function(res) {
      if (is.null(res) || !nrow(res$points)) {
        session$sendCustomMessage("gc-map-clear", list(id = ns("map")))
        return(invisible(NULL))
      }
      payload <- gc_map_payload(res)
      payload$id <- ns("map")
      session$sendCustomMessage("gc-map", payload)
      invisible(NULL)
    }

    shiny::observeEvent(result(), enviar(result()), ignoreNULL = FALSE)
    invisible(NULL)
  })
}
