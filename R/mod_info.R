# =============================================================================
# geocoordR :: páginas de ajuda e sobre
# =============================================================================

gc_link <- function(texto, url) {
  htmltools::tags$a(href = url, target = "_blank", rel = "noopener noreferrer", texto)
}

#' Página de ajuda (conteúdo estático)
pg_ajuda <- function() {
  htmltools::tags$div(
    class = "container-fluid py-3",
    htmltools::tags$div(
      class = "gc-help-page",
      bslib::card(
        bslib::card_header(
          htmltools::tags$div(
            class = "gc-card-title",
            shiny::icon("circle-question"),
            "Como usar o geocoordR"
          )
        ),
        bslib::card_body(
          htmltools::tags$h4("Em três passos"),
          htmltools::tags$ol(
            htmltools::tags$li(
              htmltools::tags$strong("Diga o que você tem: "),
              "coordenadas UTM (metros) ou geográficas (graus decimais ou GMS).",
              "Se não souber, escolha ", htmltools::tags$em("Não sei"), " e o app detecta."
            ),
            htmltools::tags$li(
              htmltools::tags$strong("Cole as coordenadas, uma por linha. "),
              "Rótulos como P1, V-01 ou Vértice 3 são reconhecidos e reaparecem na tabela."
            ),
            htmltools::tags$li(
              htmltools::tags$strong("Escolha o datum. "),
              "Está escrito no memorial ou na ART. Em dúvida, SIRGAS 2000 é o padrão oficial brasileiro."
            )
          ),
          htmltools::tags$p(
            class = "gc-muted",
            "O resultado aparece na hora. Se algo não puder ser lido, você verá a linha do texto, ",
            "o motivo e o que fazer — nada é corrigido escondido."
          ),

          htmltools::tags$h4("Formatos de coordenadas aceitos"),
          htmltools::tags$p("UTM (metros), com a zona em qualquer lugar da linha:"),
          htmltools::tags$pre(
            class = "gc-format-list",
            paste(
              "P1 24S 713110.66 8793023.16",
              "P2: Zona 24 Sul, E 713.550,20, N 8.793.023,16",
              "P3 713550.20; 8793423.16   (zona escolhida no painel)",
              "P4 N 8793423,16 E 713.110,66",
              sep = "\n"
            )
          ),
          htmltools::tags$p("Graus decimais:"),
          htmltools::tags$pre(
            class = "gc-format-list",
            paste(
              "-15.793889, -47.882778",
              "-15,793889 -47,882778",
              "15,793889 S / 47,882778 O",
              "15.793889S 47.882778W",
              sep = "\n"
            )
          ),
          htmltools::tags$p("Graus, minutos e segundos (GMS):"),
          htmltools::tags$pre(
            class = "gc-format-list",
            paste(
              "15°47'38\"S 47°52'58\"O",
              "15 47 38,0 S 47 52 58,0 O",
              "15:47:38 S 47:52:58 O",
              "15°47'38\"S, 47°52'58\"O",
              sep = "\n"
            )
          ),

          htmltools::tags$h4("O que acontece com os dados"),
          htmltools::tags$ol(
            htmltools::tags$li("O texto é lido linha por linha, com validação de cada valor."),
            htmltools::tags$li("Coordenadas UTM são convertidas para latitude/longitude no datum de origem."),
            htmltools::tags$li("Se o datum não for SIRGAS 2000, aplicamos a transformação oficial de 3 parâmetros."),
            htmltools::tags$li("As coordenadas são projetadas novamente em UTM SIRGAS 2000, na zona da área."),
            htmltools::tags$li("Área, perímetro, lados e azimutes são calculados no plano UTM.")
          ),

          htmltools::tags$h4("Sobre a precisão"),
          htmltools::tags$ul(
            htmltools::tags$li(
              "Conversão entre UTM e graus: erro inferior a 1 mm, validado contra a ",
              gc_link("biblioteca PROJ", "https://proj.org/"), " no ambiente de testes."
            ),
            htmltools::tags$li(
              "Transformação de datum: erro da ordem de 5 m para SAD 69 e Córrego Alegre ",
              "(limite da própria informação contida nesses data antigos)."
            ),
            htmltools::tags$li(
              "Área: calculada no plano UTM. A distorção da projeção é de poucos cm por hectare ",
              "dentro da zona; para áreas com dezenas de milhares de hectares, o cálculo deve ser ",
              "geodésico."
            ),
            htmltools::tags$li(
              "Este app é uma ferramenta de apoio. Peças técnicas oficiais exigem responsável ",
              "técnico habilitado e conferência no ",
              gc_link("SIGEF/INCRA", "https://sigef.incra.gov.br/"), "."
            )
          ),

          htmltools::tags$h4("Fontes técnicas"),
          htmltools::tags$ul(
            htmltools::tags$li(gc_link(
              "IBGE — SIRGAS 2000, o sistema de referência oficial do Brasil",
              "https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/sirgas.html"
            )),
            htmltools::tags$li(gc_link(
              "IBGE — Banco de Dados Geodésicos (BDG)",
              "https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/servicos-para-posicionamento-geodesico/16327-banco-de-dados-geodesicos.html"
            )),
            htmltools::tags$li(gc_link(
              "INCRA — Manual Técnico para Georreferenciamento de Imóveis Rurais (2022)",
              "https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/Manual_Tecnico_de_Georreferenciamento_2_Edicao.pdf"
            )),
            htmltools::tags$li(gc_link(
              "INCRA — Norma Técnica para Georreferenciamento de Imóveis Rurais",
              "https://sigef.incra.gov.br/static/documentos/norma_tecnica_georreferenciamento_imoveis_rurais_3ed.pdf"
            )),
            htmltools::tags$li(gc_link(
              "EPSG Registry — definições de SIRGAS 2000, SAD 69, Córrego Alegre e zonas UTM",
              "https://epsg.org/"
            )),
            htmltools::tags$li(gc_link(
              "PROJ — biblioteca de transformação de coordenadas",
              "https://proj.org/"
            )),
            htmltools::tags$li(gc_link(
              "Snyder, J. P. (1987). Map Projections — A Working Manual (USGS PP 1395)",
              "https://pubs.usgs.gov/publication/pp1395"
            )),
            htmltools::tags$li(gc_link(
              "RFC 7946 — The GeoJSON Format",
              "https://datatracker.ietf.org/doc/html/rfc7946"
            )),
            htmltools::tags$li(gc_link(
              "OGC — KML e GeoPackage",
              "https://www.ogc.org/standards/kml/"
            ))
          )
        )
      )
    )
  )
}

#' Página "Sobre"
pg_sobre <- function() {
  htmltools::tags$div(
    class = "container-fluid py-3",
    htmltools::tags$div(
      class = "gc-help-page",
      bslib::card(
        bslib::card_header(
          htmltools::tags$div(
            class = "gc-card-title",
            shiny::icon("seedling"),
            "Sobre o geocoordR"
          )
        ),
        bslib::card_body(
          htmltools::tags$p(
            "O geocoordR lê coordenadas de memoriais descritivos de imóveis e devolve ",
            "pontos e polígonos em SIRGAS 2000, prontos para mapa, planilha e SIG."
          ),
          htmltools::tags$p(
            "Esta versão (", GC_APP_VERSION, ") reescreveu o app em módulos, sem dependência ",
            "de GDAL/PROJ no servidor: a projeção e a transformação de datum são calculadas ",
            "em R puro e testadas contra o PROJ."
          ),
          htmltools::tags$h4("Tecnologia"),
          htmltools::tags$ul(
            htmltools::tags$li(gc_link("Shiny", "https://shiny.posit.co/"), " — servidor web"),
            htmltools::tags$li(gc_link("bslib", "https://rstudio.github.io/bslib/"), " — tema Bootstrap 5"),
            htmltools::tags$li(gc_link("DT", "https://rstudio.github.io/DT/"), " — tabelas"),
            htmltools::tags$li(gc_link("jsonlite", "https://jeroen.r-universe.dev/jsonlite"), " — GeoJSON"),
            htmltools::tags$li(gc_link("leaflet", "https://leafletjs.com/"), " (via CDN) — mapa"),
            htmltools::tags$li(gc_link("testthat", "https://testthat.r-lib.org/"), " — testes automatizados")
          ),
          htmltools::tags$h4("Licença"),
          htmltools::tags$p(
            "MIT. O código está em ",
            gc_link("github.com/diegomsg/geocoordr", "https://github.com/diegomsg/geocoordr"),
            "."
          ),
          htmltools::tags$h4("Limitações conhecidas"),
          htmltools::tags$ul(
            htmltools::tags$li(
              "A transformação para data antigos usa translação de 3 parâmetros. ",
              "Não aplicamos as grades oficiais do IBGE (mais precisas, porém pesadas)."
            ),
            htmltools::tags$li(
              "O mapa usa imagens de satélite públicas (OpenStreetMap/Esri) e depende de internet."
            ),
            htmltools::tags$li(
              "Não há verificação de sobreposição com imóveis vizinhos nem integração com o SIGEF."
            )
          )
        )
      )
    )
  )
}
