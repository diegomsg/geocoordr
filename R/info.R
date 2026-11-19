# =============================================================================
# geocoordR :: textos de ajuda e fontes técnicas
#
# Cada campo do app tem um ícone (i). O conteúdo vem daqui, sempre com uma
# fonte externa de referência (IBGE, INCRA, EPSG, PROJ, OGC, IETF).
# =============================================================================

gc_info_registry <- list(
  memoria = list(
    title = "O que é um memorial descritivo?",
    text = paste0(
      "É o documento que descreve os limites de um imóvel: lista os vértices ",
      "com suas coordenadas e as confrontações de cada lado. Aqui você pode ",
      "colar apenas as coordenadas, uma por linha."
    ),
    link = "https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/Manual_Tecnico_de_Georreferenciamento_2_Edicao.pdf",
    link_label = "Manual Técnico para Georreferenciamento (INCRA, 2022)"
  ),
  tipo_utm = list(
    title = "Coordenadas UTM",
    text = paste0(
      "São dois números em metros: E (leste, distância ao meridiano central da ",
      "zona) e N (norte, distância ao equador). Precisam vir acompanhados da ",
      "zona UTM (fuso) e do hemisfério."
    ),
    link = "https://proj.org/en/stable/operations/projections/utm.html",
    link_label = "PROJ — projeção UTM"
  ),
  tipo_dd = list(
    title = "Graus decimais",
    text = paste0(
      "Latitude e longitude escritas como um único número decimal, por exemplo ",
      "-15,793889. Valores negativos indicam Sul e Oeste. É o formato usado por ",
      "GPS e planilhas."
    ),
    link = "https://epsg.io/4674",
    link_label = "EPSG:4674 — SIRGAS 2000 (coordenadas geográficas)"
  ),
  tipo_gms = list(
    title = "Graus, minutos e segundos",
    text = paste0(
      "Formato tradicional dos memoriais: 15°47'38\"S. Cada grau tem 60 minutos ",
      "e cada minuto 60 segundos. Aceitamos também 15 47 38 S ou 15:47:38 S."
    ),
    link = "https://epsg.io/4674",
    link_label = "EPSG:4674 — SIRGAS 2000"
  ),
  zona = list(
    title = "Zona UTM (fuso)",
    text = paste0(
      "O Brasil usa as zonas 17 a 26 no hemisfério Sul (e 11 a 24 no Norte). ",
      "Cada zona tem 6° de longitude. Usar a zona correta evita distorção."
    ),
    link = "https://epsg.io/31983",
    link_label = "EPSG:31983 — exemplo de zona UTM no Brasil"
  ),
  hemisferio = list(
    title = "Hemisfério",
    text = paste0(
      "No hemisfério Sul o equador recebe o valor 10.000.000 m e o N cresce ",
      "para o sul. No hemisfério Norte o equador vale 0 m."
    ),
    link = "https://proj.org/en/stable/operations/projections/utm.html",
    link_label = "PROJ — UTM"
  ),
  datum = list(
    title = "Datum",
    text = paste0(
      "É o modelo matemático que dá origem às coordenadas. O mesmo ponto tem ",
      "coordenadas diferentes em cada datum — por isso escolher o datum errado ",
      "desloca o imóvel em dezenas ou centenas de metros."
    ),
    link = "https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/sirgas.html",
    link_label = "IBGE — SIRGAS 2000 (sistema oficial do Brasil)"
  ),
  sirgas = list(
    title = "SIRGAS 2000",
    text = paste0(
      "Sistema Geodésico de Referência oficial do Brasil desde 2005, obrigatório ",
      "desde 25/02/2015. Praticamente idêntico ao WGS 84 do GPS."
    ),
    link = "https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/sirgas.html",
    link_label = "IBGE — SIRGAS 2000"
  ),
  sad69 = list(
    title = "SAD 69 (datum antigo)",
    text = paste0(
      "Sistema de Referência Datum Sul-Americano de 1969, usado no Brasil até ",
      "a adoção do SIRGAS 2000. Documentos antigos frequentemente estão em SAD 69."
    ),
    link = "https://epsg.io/4618",
    link_label = "EPSG:4618 — SAD69"
  ),
  corrego = list(
    title = "Córrego Alegre (datum antigo)",
    text = paste0(
      "Datum usado até a década de 1970 em levantamentos do IBGE. Aparece em ",
      "documentos muito antigos; a conversão tem erro maior que a do SAD 69."
    ),
    link = "https://epsg.io/4225",
    link_label = "EPSG:4225 — Córrego Alegre 1970-72"
  ),
  wgs84 = list(
    title = "WGS 84",
    text = paste0(
      "Sistema usado pelo GPS. No Brasil, a diferença em relação ao SIRGAS 2000 ",
      "é de menos de 1 metro para a maioria dos usos."
    ),
    link = "https://epsg.io/4326",
    link_label = "EPSG:4326 — WGS 84"
  ),
  ordem = list(
    title = "Ordem dos eixos",
    text = paste0(
      "Alguns documentos trazem N antes de E (ou longitude antes da latitude). ",
      "No modo automático o app detecta pela magnitude dos números; se errar, ",
      "escolha a ordem manualmente."
    ),
    link = "https://epsg.io/31983",
    link_label = "EPSG — convenção de eixos (E, N)"
  ),
  separador = list(
    title = "Separador decimal",
    text = paste0(
      "No Brasil usa-se vírgula (713110,66). Em sistemas e planilhas, ponto ",
      "(713110.66). O app entende os dois, inclusive 713.110,66 (ponto de milhar)."
    ),
    link = "https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/sirgas.html",
    link_label = "IBGE — Geodésia"
  ),
  area = list(
    title = "Área e perímetro",
    text = paste0(
      "Calculados no plano UTM do SIRGAS 2000 (área plana), como nos memoriais ",
      "descritivos. A distorção da projeção UTM é de poucos centímetros por ",
      "hectare dentro da zona."
    ),
    link = "https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/Manual_Tecnico_de_Georreferenciamento_2_Edicao.pdf",
    link_label = "INCRA — Manual Técnico (cálculo de área e vértices)"
  ),
  azimute = list(
    title = "Azimute e distância",
    text = paste0(
      "O azimute é o ângulo medido a partir do Norte, no sentido horário ",
      "(0° a 360°). A distância é a do lado no plano UTM (em metros)."
    ),
    link = "https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/Manual_Tecnico_de_Georreferenciamento_2_Edicao.pdf",
    link_label = "INCRA — Manual Técnico"
  ),
  poligono = list(
    title = "Fechar polígono",
    text = paste0(
      "Liga o último vértice ao primeiro, formando a área do imóvel. Se a ",
      "ordem dos vértices estiver errada, o polígono pode se cruzar — o app avisa."
    ),
    link = "https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/Manual_Tecnico_de_Georreferenciamento_2_Edicao.pdf",
    link_label = "INCRA — Manual Técnico"
  ),
  epsg = list(
    title = "Código EPSG",
    text = paste0(
      "Identificador internacional do sistema de coordenadas. Ex.: 31984 é o ",
      "SIRGAS 2000 / UTM zona 24S. Serve para configurar outros softwares."
    ),
    link = "https://epsg.org/",
    link_label = "EPSG Registry (IOGP)"
  ),
  precisao = list(
    title = "Sobre a precisão",
    text = paste0(
      "A conversão de UTM para graus (e vice-versa) é exata ao milímetro. ",
      "A conversão entre data antigos e o SIRGAS 2000 carrega a incerteza do ",
      "próprio datum (cerca de 5 m para SAD 69 e Córrego Alegre)."
    ),
    link = "https://epsg.io/15485",
    link_label = "EPSG:15485 — SAD69 para SIRGAS 2000"
  ),
  geojson = list(
    title = "GeoJSON",
    text = paste0(
      "Formato aberto (RFC 7946) para mapas web, QGIS, Google My Maps e ",
      "bibliotecas como Leaflet. Sempre em WGS 84 / SIRGAS 2000."
    ),
    link = "https://datatracker.ietf.org/doc/html/rfc7946",
    link_label = "RFC 7946 — GeoJSON"
  ),
  kml = list(
    title = "KML",
    text = paste0(
      "Formato do Google Earth e do Google Maps. Abra o arquivo e o imóvel ",
      "aparece desenhado sobre a imagem de satélite."
    ),
    link = "https://www.ogc.org/standards/kml/",
    link_label = "OGC — KML"
  ),
  csv = list(
    title = "CSV / planilha",
    text = paste0(
      "O formato 'Excel brasileiro' usa ponto e vírgula e vírgula decimal, ",
      "abrindo corretamente no Excel em português. O formato internacional usa ",
      "vírgula e ponto."
    ),
    link = "https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/Manual_Tecnico_de_Georreferenciamento_2_Edicao.pdf",
    link_label = "INCRA — Manual Técnico"
  ),
  gpkg = list(
    title = "GeoPackage / Shapefile",
    text = paste0(
      "Formatos usados por SIG (QGIS, ArcGIS). Exigem o pacote R 'sf' instalado ",
      "no servidor. Se não estiver disponível, use GeoJSON ou KML."
    ),
    link = "https://www.ogc.org/standards/gpkg/",
    link_label = "OGC — GeoPackage"
  ),
  bdg = list(
    title = "Conferir com dados oficiais",
    text = paste0(
      "O IBGE mantém o Banco de Dados Geodésicos (BDG) com coordenadas de ",
      "estações oficiais — útil para validar suas conversões."
    ),
    link = "https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/servicos-para-posicionamento-geodesico/16327-banco-de-dados-geodesicos.html",
    link_label = "IBGE — Banco de Dados Geodésicos"
  )
)

#' Recupera um verbete de ajuda
gc_info <- function(id) {
  entry <- gc_info_registry[[id]]
  if (is.null(entry)) {
    list(
      title = "Informação",
      text = "Sem conteúdo de ajuda para este campo.",
      link = "https://github.com/diegomsg/geocoordr",
      link_label = "Documentação do geocoordR"
    )
  } else {
    entry
  }
}

#' Ícone (i) com tooltip e link para a fonte técnica
info_badge <- function(id, placement = "top") {
  entry <- gc_info(id)
  bslib::tooltip(
    trigger = htmltools::tags$a(
      href = entry$link,
      target = "_blank",
      rel = "noopener noreferrer",
      class = "gc-info-badge",
      `aria-label` = paste0("Ajuda sobre ", entry$title),
      shiny::icon("circle-info")
    ),
    htmltools::tags$strong(entry$title),
    htmltools::tags$br(),
    entry$text,
    htmltools::tags$br(),
    htmltools::tags$span(
      class = "gc-info-source",
      shiny::icon("arrow-up-right-from-square"),
      " ",
      entry$link_label
    ),
    placement = placement
  )
}
