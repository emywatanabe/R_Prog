# =====================================================================================
# ui.R -- interface (usa objetos definidos em global.R)
# =====================================================================================

header <- dashboardHeader(
  title = "",
  tags$li(
    class = "dropdown nav-abas",
    actionButton("nav_contexto", tagList(icon("clock-rotate-left"), " Contexto Histórico"), class = "top-tab-btn active"),
    actionButton("nav_embalagens", tagList(icon("bottle-water"), " Embalagens"), class = "top-tab-btn"),
    actionButton("nav_estados", tagList(icon("map"), " Estados e Regiões"), class = "top-tab-btn")
  )
)

sidebar <- dashboardSidebar(
  div(
    class = "sidebar-sobre",
    tags$div(class = "sidebar-autor", "Emily Yamagutti Watanabe"),
    tags$strong("Sobre"),
  ),
  sidebarMenu(
    id = "abas",
    menuItem("Contexto Histórico", tabName = "contexto", icon = icon("clock-rotate-left"), selected = TRUE),
    menuItem("Embalagens", tabName = "embalagens", icon = icon("bottle-water")),
    menuItem("Estados e Regiões", tabName = "estados", icon = icon("map"))
  ),
  # Abas 1 e 2: período. Aba 3: um único ano (mapa e ranking)
  conditionalPanel("input.abas != 'estados'", uiOutput("ui_anos")),
  conditionalPanel(
    "input.abas == 'estados'",
    selectInput(
      "ano_mapa", "Ano (mapa e ranking)",
      choices = sort(unique(agua_clean$ano), decreasing = TRUE),
      selected = max(agua_clean$ano, na.rm = TRUE)
    )
  ),
  uiOutput("ui_regiao"),
  # A UF só se aplica às abas 1 e 2 (mapa e ranking mostram todas as UFs)
  conditionalPanel("input.abas != 'estados'", uiOutput("ui_uf")),
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

  /* Abas na barra superior; o logo some e a barra ocupa toda a largura */
  .main-header .logo { display: none !important; }
  .main-header .navbar { margin-left: 0 !important; }
  .main-header .navbar-custom-menu { float: left !important; }
  .main-header .navbar-custom-menu > .navbar-nav > li { float: left; }
  .top-tab-btn { background: transparent !important; border: none !important; border-radius: 0 !important; box-shadow: none !important;
    height: 50px; padding: 0 20px; font-size: 16px; font-weight: 600; color: #163a42 !important; border-bottom: 3px solid transparent !important; }
  .top-tab-btn:hover { background-color: #c5e6de !important; }
  .top-tab-btn.active { border-bottom-color: #10b981 !important; background-color: #c5e6de !important; }
  .sidebar-menu { display: none; }   /* menu lateral oculto: so serve de controle das abas */

  /* Titulo e autor na barra lateral */
  .sidebar-titulo { margin: 0 0 4px 0; font-size: 18px; font-weight: 700; color: #ffffff; line-height: 1.25; }
  .sidebar-autor { margin-bottom: 10px; font-size: 13px; color: #a9c9c2; }

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

  /* Barra lateral fixa com rolagem propria: todos os filtros ficam acessiveis em telas baixas */
  .main-sidebar { overflow-y: auto !important; overflow-x: hidden; }
  .main-sidebar .sidebar { height: auto !important; max-height: none !important; overflow: visible !important; padding-bottom: 40px; }
  .main-sidebar .slimScrollDiv { height: auto !important; overflow: visible !important; }
  .main-sidebar .slimScrollBar, .main-sidebar .slimScrollRail { display: none !important; }
  .main-sidebar .form-group { margin-bottom: 8px; }
  .main-sidebar .selectize-dropdown { z-index: 2000; }

  /* Barra lateral sempre visivel no desktop: sem o botao de menu (no celular ele continua, senao a barra não abre) */
  @média (min-width: 768px) {
    .main-header .sidebar-toggle { display: none !important; }
  }

  /* Título de cada aba */
  .titulo-aba { margin: 0 4px 12px 4px; padding-bottom: 8px; border-bottom: 1px solid #cfe0db; }
  .titulo-aba h3 { margin: 0 0 4px 0; font-size: 24px; font-weight: 700; color: #163a42; }
  .titulo-aba p { margin: 0; font-size: 14px; color: #475569; }

  /* Tufte: sem molduras, sombras ou fundos decorativos nos paineis */
  .box {
    background-color: transparent !important; border: none !important;
    border-top: 1px solid #cfe0db !important; border-radius: 0 !important; box-shadow: none !important;
  }
  .box-header .box-title { color: #163a42 !important; font-weight: 600; font-size: 18px; }

  /* Indicadores como texto (número grande + rotulo), sem caixa colorida */
  .kpi-row { display: flex; flex-wrap: wrap; gap: 8px 28px; padding: 6px 4px 14px 4px; border-bottom: 1px solid #cfe0db; margin-bottom: 14px; }
  .kpi { min-width: 155px; }
  .kpi-label { font-size: 12px; text-transform: uppercase; letter-spacing: 0.4px; color: #475569; }
  .kpi-valor { font-size: 26px; font-weight: 700; color: #0f172a; line-height: 1.15; }
  .kpi-detalhe { font-size: 12px; color: #475569; }

  .controles-mapa { display: flex; flex-wrap: wrap; gap: 12px 24px; align-items: flex-end; margin: 0 4px 8px 4px; }
  .controles-mapa .form-group { margin-bottom: 4px; }

  /* Cards de R$/L por embalagem no padrao Tufte */
  .emb-kpi-grid { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 14px; padding: 4px 0; }
  @média (max-width: 992px) { .emb-kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
  .emb-kpi-card { border-top: 2px solid #163a42; padding: 8px 6px 4px 6px; }
  .emb-kpi-nome { font-size: 12px; font-weight: 600; color: #163a42; text-transform: uppercase; letter-spacing: 0.3px; }
  .emb-kpi-preço { font-size: 22px; font-weight: 700; color: #0f172a; margin: 2px 0; }
  .emb-kpi-sub { font-size: 12px; color: #475569; }

  /* Notas sob os graficos (substituem subtitulo/legenda, que o plotly não exibe) */
  .nota { font-size: 12px; color: #475569; margin: 4px 6px 0 6px; }

  .rodape-fonte { margin-top: 6px; padding: 8px 12px; font-size: 12px; color: #334155; border-top: 1px solid #cde0da; }
  .rodape-fonte a { color: #163a42; font-weight: 600; text-decoration: underline; }
"

body <- dashboardBody(
  tags$head(
    tags$style(HTML(estilos_css)),
    # Layout "fixed" do AdminLTE: cabecalho e barra lateral ficam fixos ao rolar a pagina
    tags$script(HTML("
      $(function(){ $('body').addClass('fixed'); });
      $(document).on('click', '.top-tab-btn', function() {
        $('.top-tab-btn').removeClass('active');
        $(this).addClass('active');
      });
    "))
  ),
  tabItems(
    # ABA 1: Contexto Historico
    tabItem(
      tabName = "contexto",
      div(class = "titulo-aba", tags$h3("Contexto Histórico: volume, valor e preço do litro (2010–2025)"), tags$p("O valor cresceu mais que o volume? Os indicadores e gráficos respeitam o período, a região e a UF escolhidos na barra lateral.")),
      fluidRow(column(width = 12, uiOutput("kpis_ui"))),
      fluidRow(
        box(
          title = "Produção ao longo do tempo: Volume (Bi L) x Valor (R$ Bi)",
          width = 12,
          plotlyOutput("plot_vol_val_tempo", height = "360px"),
          uiOutput("nota_vol_val_tempo")
        )
      ),
      fluidRow(
        box(
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
          title = "Resumo do preço por litro por embalagem (R$/L)",
          width = 12,
          uiOutput("cards_preco_emb_ui"),
          div(class = "nota", "Preço médio ponderado (Valor / Volume) e variação percentual (Δ%) entre o ano inicial e final do filtro.")
        )
      ),
      fluidRow(
        box(
          width = 6,
          plotlyOutput("scatter_uf", height = "430px"),
          uiOutput("nota_scatter")
        ),
        box(
          title = "Distribuição do R$/L entre UFs por embalagem (violino e boxplot)",
          width = 6,
          plotlyOutput("violin_preco_emb", height = "430px"),
          uiOutput("nota_violin_preco_emb")
        )
      ),
      fluidRow(
        box(
          width = 12,
          plotlyOutput("plot_preco_emb_ano", height = "300px"),
          uiOutput("nota_preco_emb_ano")
        )
      )
    ),

    # ABA 3: Estados e Regioes (ano e metrica valem para o mapa e para o ranking)
    tabItem(
      tabName = "estados",
      div(class = "titulo-aba", tags$h3("Estados e Regiões: onde se produz e quanto vale cada litro"), tags$p("Escolha o ano e a métrica para ver o mapa e o ranking das maiores UFs, com a concentração acumulada.")),
      fluidRow(
        column(
          width = 12,
          div(
            class = "controles-mapa",
            radioButtons(
              "metrica_mapa",
              "Exibir no mapa:",
              choices = c(
                "Volume (Bi L)" = "litros",
                "Valor (R$ Bi)" = "valor",
                "Preço médio (R$/L)" = "preco"
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
          title = "Ranking das 10 maiores UFs e concentração acumulada",
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
