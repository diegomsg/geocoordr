# geocoordR

Aplicativo Shiny para **ler, converter e georreferenciar coordenadas de memoriais
descritivos** — pontos ou polígonos — com exportação para planilha, mapa e SIG.

Feito para quem não é especialista: a interface pergunta o mínimo, explica cada
campo com um ícone **(i)** (com link para a fonte técnica oficial), nunca corrige
nada em silêncio e devolve mensagens de erro que dizem **onde** está o problema,
**por que** ele aconteceu e **o que fazer**.

- Demonstração: <https://geocoordr.shinyapps.io/geocoordr/>
- Fonte: <https://github.com/diegomsg/geocoordr>

---

## O que o app faz

| Etapa | Descrição |
|---|---|
| **Entrada** | UTM (E, N + zona + hemisfério) ou geográficas em graus decimais ou graus/minutos/segundos, em vários formatos de escrita |
| **Datum** | SIRGAS 2000, WGS 84, SAD 69, SAD 69(96), Córrego Alegre 1970-72 e Córrego Alegre 1961 |
| **Cálculo** | UTM → geográfico → transformação de datum → UTM SIRGAS 2000 |
| **Saída** | Tabela, mapa, resumo (área, perímetro, centro, zona, EPSG), lados (distância e azimute) e verificações |
| **Exportação** | CSV (Excel brasileiro e internacional), GeoJSON, KML, lados, relatório técnico, GeoPackage e Shapefile (com `sf`) |

### Formatos de entrada aceitos

UTM — a zona pode estar na linha, no início, no fim ou ser escolhida no painel:

```
P1 24S 713110.66 8793023.16
P2: Zona 24 Sul, E 713.550,20, N 8.793.023,16
P3 713550.20; 8793423.16          (zona escolhida no painel)
P4 N 8793423,16 E 713.110,66
```

Graus decimais:

```
-15.793889, -47.882778
-15,793889 -47,882778
15,793889 S / 47,882778 O
15.793889S 47.882778W
```

Graus, minutos e segundos:

```
15°47'38"S 47°52'58"O
15 47 38,0 S 47 52 58,0 O
15:47:38 S 47:52:58 O
15d47m38s S 47d52m58s O
15 graus 47 minutos 38 segundos S 47 graus 52 minutos 58 segundos O
```

Rótulos (`P1`, `V-01`, `Vértice 3:`, `M1`) são reconhecidos e preservados.
Separadores decimais: ponto, vírgula e ponto de milhar (`713.110,66`) — sempre com
aviso quando há ambiguidade.

## Instalação

```r
# dependências obrigatórias
install.packages(c("shiny", "bslib", "shinyWidgets", "DT", "jsonlite"))

# opcional: GeoPackage / Shapefile
install.packages("sf")

# opcional: testes
install.packages("testthat")
```

Não é necessário GDAL ou PROJ no servidor: a projeção UTM e a transformação de
datum são implementadas em R puro (ver *Precisão*, abaixo).

## Executar

```r
shiny::runApp()          # a partir da raiz do projeto
# ou
shiny::runApp("caminho/para/geocoordr")
```

Publicação no shinyapps.io:

```r
rsconnect::deployApp()
```

## Testes

```bash
Rscript tests/testthat.R
```

851 asserções cobrem:

- leitura de números (vírgula/ponto/milhar), ângulos e GMS;
- leitura de memoriais linha por linha, com todos os formatos e todos os erros;
- projeção UTM e transformação de datum **comparadas com o PROJ** (valores
  congelados em `tests/fixtures/proj-reference.csv`, gerados por
  `data-raw/gerar-fixtures-proj.R`);
- validações (limites do Brasil, zona × longitude, pontos repetidos,
  autointerseção do polígono, área nula);
- pipeline completo e exportações (CSV, GeoJSON, KML);
- módulos Shiny (`shiny::testServer`) e o app completo.

## Precisão

| Operação | Erro |
|---|---|
| UTM ↔ geográfico (Snyder/PP 1395) | < 1 mm dentro da zona — validado contra o PROJ 9.x |
| SIRGAS 2000 ↔ WGS 84 | < 1 m (equivalentes na prática) |
| SAD 69 → SIRGAS 2000 (3 parâmetros, EPSG 15485) | ~5 m |
| Córrego Alegre 1970-72 → SIRGAS 2000 (EPSG 6193) | ~5 m |
| Córrego Alegre 1961 → SIRGAS 2000 | ~10 m (não há transformação oficial de 3 parâmetros; usamos os parâmetros do CA 1970-72) |
| Área / perímetro | plano UTM (como nos memoriais); distorção de poucos cm por hectare |

O app avisa sempre que a conversão envolve datum antigo. Para peças oficiais
(SIGEF/INCRA), a conferência por responsável técnico continua obrigatória.

## Arquitetura

```
app.R                      # entrada fina: tema, layout, server, shinyApp()
R/000-utils.R              # utilitários, problemas (erros/avisos) e formatação
R/crs.R                    # elipsóides, data, EPSG, zonas UTM
R/geo-math.R               # Transversa de Mercator, ECEF, Helmert (R puro)
R/parse-numbers.R          # números pt-BR, ângulos, GMS
R/parse-coordinates.R      # leitura de memoriais (linha por linha)
R/geometry.R               # área, perímetro, lados, autointerseção
R/validate.R               # validações com mensagens descritivas
R/convert.R                # pipeline completo: gc_convert()
R/export.R                 # CSV, GeoJSON, KML, GeoPackage/Shapefile
R/info.R                   # textos de ajuda + fontes externas
R/ui-helpers.R             # tema, componentes de UI, exemplos
R/mod_input.R              # módulo: entrada de coordenadas
R/mod_results.R            # módulo: tabelas, resumo, verificações, técnico
R/mod_map.R                # módulo: mapa (Leaflet via CDN, sem sf/tmap)
R/mod_export.R             # módulo: downloads
R/mod_info.R               # páginas de ajuda e sobre
www/geocoordr.css          # estilo
tests/testthat/            # testes
data-raw/                  # geração das fixtures do PROJ
```

Princípios adotados:

1. **Núcleo puro e testável.** Toda a matemática e a leitura de texto vivem em
   funções sem Shiny, sem `sf` e sem rede. Os módulos apenas conectam widgets a
   essas funções.
2. **Problemas como dados.** Erros e avisos são um `data.frame` (`level`, `code`,
   `line`, `input`, `title`, `message`, `hint`, `link`), acumulados em vez de
   abortar na primeira falha. A UI só renderiza.
3. **Nada de decisão silenciosa.** Separador de milhar, hemisfério assumido,
   ordem dos eixos detectada, polígono fechado automaticamente: tudo vira aviso.
4. **Ajuda no lugar da dúvida.** Cada campo tem um ícone (i) com explicação em
   linguagem simples e link para a fonte oficial (IBGE, INCRA, EPSG, PROJ, OGC,
   IETF).

## Fontes técnicas

- IBGE — SIRGAS 2000: <https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/sirgas.html>
- IBGE — Banco de Dados Geodésicos: <https://www.ibge.gov.br/geociencias/informacoes-sobre-posicionamento-geodesico/servicos-para-posicionamento-geodesico/16327-banco-de-dados-geodesicos.html>
- INCRA — Manual Técnico para Georreferenciamento (2022): <https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/Manual_Tecnico_de_Georreferenciamento_2_Edicao.pdf>
- INCRA — Norma Técnica: <https://sigef.incra.gov.br/static/documentos/norma_tecnica_georreferenciamento_imoveis_rurais_3ed.pdf>
- EPSG Registry: <https://epsg.org/> (SIRGAS 2000/UTM 24S: <https://epsg.io/31984>)
- PROJ: <https://proj.org/>
- Snyder, J. P. (1987), *Map Projections — A Working Manual*, USGS PP 1395
- RFC 7946 (GeoJSON), OGC KML

## Licença

MIT — veja [LICENSE](LICENSE).
