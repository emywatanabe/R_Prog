# =====================================================================================
# ui.R -- interface (usa objetos definidos em global.R)
# =====================================================================================

header <- dashboardHeader(title = "Agua Mineral (2010-2025)")

sidebar <- dashboardSidebar(
  div(
    class = "sidebar-sobre",
    tags$strong("Sobre"),
    "Painel exploratorio da producao comercializada de agua mineral no Brasil (2010\u20132025), com base no Anuario Mineral Brasileiro (ANM). Os indicadores consideram agua embalada (5 formatos) em valores nominais."
  ),
  sidebarMenu(
    id = "abas",
    menuItem("Contexto Historico", tabName = "contexto", icon = icon("clock-rotate-left"), selected = TRUE),
    menuItem("Embalagens", tabName = "embalagens", icon = icon("bottle-water")),
    menuItem("Estados e Regioes", tabName = "estados", icon = icon("map"))
  ),
  hr(),
  uiOutput("ui_anos"),
  uiOutput("ui_regiao"),
  uiOutput("ui_uf"),
  selectInput(
    "embalagem",
    "Tipo de embalagem",
    choices = c("Todas" = "todas", emb_cols_all),
    selected = "todas"
  )
)

estilos_css <- "
  .content-wrapper, .right-side { background-color: #fbfdfc !important; }
  .main-header .logo { background-color: #163a42 !important; color: #ffffff !important; font-weight: 600; }
  .main-header .logo:hover { background-color: #123037 !important; }
  .main-header .navbar { background-color: #d8efe9 !important; }
  .main-header .navbar .sidebar-toggle { color: #163a42 !important; }
  .main-header .navbar .sidebar-toggle:hover { background-color: #c5e6de !important; }
  .main-sidebar, .left-side { background-color: #1c444d !important; }
  .sidebar-menu > li > a { color: #d7e8e4 !important; }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover {
    background-color: #265965 !important; color: #ffffff !important; border-left-color: #10b981 !important;
  }
  .main-sidebar hr { border-top-color: rgba(255, 255, 255, 0.14); }

  /* Bloco Sobre no topo da barra lateral */
  .sidebar-sobre {
    margin: 12px 15px 8px 15px; padding-bottom: 12px;
    border-bottom: 1px solid rgba(255, 255, 255, 0.14);
    font-size: 14px; line-height: 1.45; color: #cfe0db; white-space: normal;
  }
  .sidebar-sobre strong {
    display: block; color: #ffffff; font-size: 14px; text-transform: uppercase;
    letter-spacing: 0.4px; margin-bottom: 4px;
  }

  /* Barra lateral sempre visivel no desktop: sem o botao de menu (no celular ele continua, senao a barra nao abre) */
  @media (min-width: 768px) {
    .main-header .sidebar-toggle { display: none !important; }
  }

  /* Tufte: sem molduras, sombras ou fundos decorativos nos paineis */
  .box {
    background-color: transparent !important; border: none !important;
    border-top: 1px solid #cfe0db !important; border-radius: 0 !important; box-shadow: none !important;
  }
  .box-header .box-title { color: #163a42 !important; font-weight: 600; font-size: 18px; }

  /* Indicadores como texto (numero grande + rotulo), sem caixa colorida */
  .kpi-row { display: flex; flex-wrap: wrap; gap: 8px 28px; padding: 6px 4px 14px 4px; border-bottom: 1px solid #cfe0db; margin-bottom: 14px; }
  .kpi { min-width: 155px; }
  .kpi-label { font-size: 12px; text-transform: uppercase; letter-spacing: 0.4px; color: #475569; }
  .kpi-valor { font-size: 26px; font-weight: 700; color: #0f172a; line-height: 1.15; }
  .kpi-detalhe { font-size: 12px; color: #475569; }

  .controles-mapa { display: flex; flex-wrap: wrap; gap: 12px 24px; align-items: flex-end; margin: 0 4px 8px 4px; }
  .controles-mapa .form-group { margin-bottom: 4px; }

  /* Cards de R$/L por embalagem no padrao Tufte */
  .emb-kpi-grid { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 14px; padding: 4px 0; }
  @media (max-width: 992px) { .emb-kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
  .emb-kpi-card { border-top: 2px solid #163a42; padding: 8px 6px 4px 6px; }
  .emb-kpi-nome { font-size: 12px; font-weight: 600; color: #163a42; text-transform: uppercase; letter-spacing: 0.3px; }
  .emb-kpi-preco { font-size: 22px; font-weight: 700; color: #0f172a; margin: 2px 0; }
  .emb-kpi-sub { font-size: 12px; color: #475569; }

  /* Notas sob os graficos (substituem subtitulo/legenda, que o plotly nao exibe) */
  .nota { font-size: 12px; color: #475569; margin: 4px 6px 0 6px; }

  .rodape-fonte { margin-top: 6px; padding: 8px 12px; font-size: 12px; color: #334155; border-top: 1px solid #cde0da; }
  .rodape-fonte a { color: #163a42; font-weight: 600; text-decoration: underline; }
"

body <- dashboardBody(
  tags$head(
    tags$style(HTML(estilos_css)),
    # Layout "fixed" do AdminLTE: cabecalho e barra lateral ficam fixos ao rolar a pagina
    tags$script(HTML("$(function(){ $('body').addClass('fixed'); });"))
  ),
  tabItems(
    # ABA 1: Contexto Historico
    tabItem(
      tabName = "contexto",
      fluidRow(column(width = 12, uiOutput("kpis_ui"))),
      fluidRow(
        box(
          title = "Producao ao longo do tempo: Volume (Bi L) x Valor (R$ Bi)",
          width = 12,
          plotlyOutput("plot_vol_val_tempo", height = "360px"),
          uiOutput("nota_vol_val_tempo")
        )
      ),
      fluidRow(
        box(
          title = "Preco medio por litro (R$/L) por ano e por UF (dispersao bruta da base)",
          width = 12,
          plotlyOutput("dotplot_preco_ano_uf", height = "430px"),
          uiOutput("nota_dotplot_preco_ano_uf")
        )
      )
    ),

    # ABA 2: Embalagens
    tabItem(
      tabName = "embalagens",
      fluidRow(
        box(
          title = "Resumo do preco por litro por embalagem (R$/L)",
          width = 12,
          uiOutput("cards_preco_emb_ui"),
          div(class = "nota", "Preco medio ponderado (Valor / Volume) e variacao percentual (Δ%) entre o ano inicial e final do filtro.")
        )
      ),
      fluidRow(
        box(
          title = "Dispersao volume x R$/litro por UF (decisao por mercado e embalagem)",
          width = 12,
          plotlyOutput("scatter_uf", height = "430px"),
          uiOutput("nota_scatter")
        )
      ),
      fluidRow(
        box(
          title = "Evolucao do preco por litro por embalagem (R$/L) e variacao percentual (Δ%)",
          width = 12,
          plotlyOutput("plot_preco_emb_ano", height = "300px"),
          uiOutput("nota_preco_emb_ano")
        )
      )
    ),

    # ABA 3: Estados e Regioes (ano e metrica valem para o mapa e para o ranking)
    tabItem(
      tabName = "estados",
      fluidRow(
        column(
          width = 12,
          div(
            class = "controles-mapa",
            uiOutput("ui_ano_mapa"),
            radioButtons(
              "metrica_mapa",
              "Exibir no mapa:",
              choices = c(
                "Volume (Bi L)" = "litros",
                "Valor (R$ Bi)" = "valor",
                "Preco medio (R$/L)" = "preco"
              ),
              selected = "litros",
              inline = TRUE
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Mapa do Brasil por UF",
          width = 6,
          plotlyOutput("mapa_estatico", height = "500px"),
          uiOutput("nota_mapa")
        ),
        box(
          title = "Ranking das 10 maiores UFs e concentracao acumulada",
          width = 6,
          plotOutput("plot_top10_uf", height = "500px"),
          uiOutput("nota_top10_uf")
        )
      )
    )
  ),
  div(
    class = "rodape-fonte",
    HTML(
      'Fonte: Dados Abertos (<a href="https://dados.gov.br/dados/conjuntos-dados/anuario-mineral-brasileiro-amb" target="_blank">Anuario Mineral Brasileiro / ANM</a>). Valores nominais; agua embalada (sem produtos industrializados).'
    )
  )
)

ui <- dashboardPage(skin = "blue", header = header, sidebar = sidebar, body = body)
