library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(geobr)
library(sf)

# =====================================================================================
# Estrutura do dashboard (2 abas):
#   1. Estados e Regioes -> Onde e quanto? (KPIs 2025 + base 100 + R$/L por ano/UF + mapa interativo + top 10 UFs)
#   2. Embalagens        -> O que mudou?   (R$/L por embalagem + dispersao UF + pictograma 5 embalagens + R$/L por ano)
# Graficos interativos com plotly (ggplot2 -> ggplotly): hover com detalhes, zoom e pan.
# Escopo dos numeros: agua embalada (5 embalagens), sem "produtos industrializados".
# Valores nominais (sem deflacao).
# =====================================================================================

# ---------- 1. Interface (UI) ----------
header <- dashboardHeader(title = "Agua Mineral (2010-2025)")

emb_cols_all <- c(
  "Garrafao" = "garrafao_l",
  "Garrafa Plastica" = "plastica_l",
  "Garrafa de Vidro" = "vidro_l",
  "Copo" = "copo_l",
  "Outros" = "outras_l",
  "Comp. Prod. Industrializados" = "comp_ind_l"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "abas",
    menuItem("Estados e Regioes", tabName = "estados", icon = icon("map"), selected = TRUE),
    menuItem("Embalagens", tabName = "embalagens", icon = icon("bottle-water"))
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
  ),
  radioButtons(
    "metrica",
    "Metrica de referencia",
    choices = c(
      "Volume em embalagens (Bi L)" = "litros",
      "Valor em embalagens (R$ Bi)" = "valor"
    ),
    selected = "litros"
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
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

      /* Tufte: sem molduras, sombras ou fundos decorativos nos paineis */
      .box {
        background-color: transparent !important; border: none !important;
        border-top: 1px solid #cfe0db !important; border-radius: 0 !important; box-shadow: none !important;
      }
      .box-header .box-title { color: #163a42 !important; font-weight: 600; font-size: 15px; }

      /* Indicadores como texto (numero grande + rotulo), sem caixa colorida */
      .kpi-row { display: flex; flex-wrap: wrap; gap: 8px 28px; padding: 6px 4px 14px 4px; border-bottom: 1px solid #cfe0db; margin-bottom: 14px; }
      .kpi { min-width: 155px; }
      .kpi-label { font-size: 12px; text-transform: uppercase; letter-spacing: 0.4px; color: #475569; }
      .kpi-valor { font-size: 26px; font-weight: 700; color: #0f172a; line-height: 1.15; }
      .kpi-detalhe { font-size: 12px; color: #475569; }

      .controles-mapa { display: flex; flex-wrap: wrap; gap: 12px 24px; align-items: flex-end; margin-bottom: 8px; }
      .controles-mapa .form-group { margin-bottom: 4px; }

      /* Pictograma Tufte: 5 colunas em linha, sem caixa pesada nem cores decorativas */
      .picto-grid {
        display: grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 12px;
        margin-top: 4px;
        padding: 4px 0;
      }
      @media (max-width: 992px) {
        .picto-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
      .picto-card {
        background: transparent;
        border: none;
        border-top: 1px solid #e2ece9;
        padding: 12px 6px 8px 6px;
        text-align: center;
      }

      /* Notas sob os graficos (substituem subtitulo/legenda, que o plotly nao exibe) */
      .nota { font-size: 12px; color: #475569; margin: 4px 6px 0 6px; }

      .rodape-fonte { margin-top: 6px; padding: 8px 12px; font-size: 12px; color: #334155; border-top: 1px solid #cde0da; }
      .rodape-fonte a { color: #163a42; font-weight: 600; text-decoration: underline; }
    "))
  ),
  tabItems(
    # ABA 1: Estados e Regioes
    tabItem(
      tabName = "estados",
      fluidRow(column(width = 12, uiOutput("resumo_mapa_ui"))),
      fluidRow(
        box(
          title = "Comparacao anual: Valor x Volume (indice base 100)",
          width = 6,
          plotlyOutput("plot_indice_vol_val", height = "370px"),
          uiOutput("nota_indice")
        ),
        box(
          title = "Preco medio por litro (R$/L) por ano e por UF",
          width = 6,
          plotlyOutput("plot_preco_ano_uf", height = "370px"),
          uiOutput("nota_preco_ano_uf")
        )
      ),
      fluidRow(
        box(
          title = "Mapa do Brasil por UF",
          width = 6,
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
          ),
          plotlyOutput("mapa_estatico", height = "480px"),
          uiOutput("nota_mapa")
        ),
        box(
          title = "Ranking das 10 maiores UFs e concentracao acumulada",
          width = 6,
          plotlyOutput("plot_top10_uf", height = "525px"),
          uiOutput("nota_top10_uf")
        )
      )
    ),
    
    # ABA 2: Embalagens -- o que mudou
    tabItem(
      tabName = "embalagens",
      fluidRow(
        box(
          title = "Preco por litro de cada embalagem (R$/L)",
          width = 6,
          plotlyOutput("plot_preco_emb", height = "360px"),
          uiOutput("nota_preco_emb")
        ),
        box(
          title = "Dispersao volume x R$/litro por UF",
          width = 6,
          plotlyOutput("scatter_uf", height = "360px"),
          uiOutput("nota_scatter")
        )
      ),
      fluidRow(
        box(
          title = "Pictograma: participacao em Volume (%) e Valor (%)",
          width = 12,
          uiOutput("cards_embalagens_html"),
          div(class = "nota", "Preenchimento proporcional ao volume; negrito indica a metrica de referencia ativa.")
        )
      ),
      fluidRow(
        box(
          title = "Evolucao do preco por litro, por embalagem (mesma escala)",
          width = 12,
          plotlyOutput("plot_preco_emb_ano", height = "280px"),
          div(class = "nota", "Mesma escala em todos os paineis. Valores nominais.")
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

# ---------- 2. Leitura e limpeza dos dados ----------
parse_br_num <- function(x) {
  as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))
}

caminho_csv <- "https://raw.githubusercontent.com/emywatanabe/R_Prog/main/CE303%20-%20visualiza%C3%A7%C3%A3o/Agua_Mineral_Producao.csv"

agua_raw <- read.csv(
  caminho_csv,
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
stopifnot(ncol(agua_raw) >= 16)

cols_l <- c("garrafao_l", "plastica_l", "vidro_l", "copo_l", "outras_l", "comp_ind_l")
cols_v <- c("garrafao_v", "plastica_v", "vidro_v", "copo_v", "outras_v", "comp_ind_v")
nomes <- names(agua_raw)
nomes[c(1, 2, 5:9, 10:14, 15, 16)] <- c(
  "ano", "UF",
  "garrafao_l", "plastica_l", "vidro_l", "copo_l", "outras_l",
  "garrafao_v", "plastica_v", "vidro_v", "copo_v", "outras_v",
  "comp_ind_l", "comp_ind_v"
)
names(agua_raw) <- nomes

agua_clean <- agua_raw |>
  mutate(
    ano = as.integer(ano),
    regiao = case_when(
      UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      UF %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      UF %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      UF %in% c("PR", "RS", "SC") ~ "Sul",
      .default = NA_character_
    ),
    across(all_of(c(cols_l, cols_v)), parse_br_num)
  ) |>
  mutate(
    total_litros_emb = rowSums(across(c(garrafao_l, plastica_l, vidro_l, copo_l, outras_l)), na.rm = TRUE),
    valor_emb_tot    = rowSums(across(c(garrafao_v, plastica_v, vidro_v, copo_v, outras_v)), na.rm = TRUE)
  )

prepara_base <- function(df, embalagem) {
  df <- df |>
    filter(!is.na(UF), !is.na(ano)) |>
    mutate(across(all_of(c(cols_l, cols_v)), ~ replace_na(as.numeric(.x), 0)))
  if (identical(embalagem, "todas")) {
    df |> mutate(vol_sel = total_litros_emb, val_sel = valor_emb_tot)
  } else {
    col_v <- sub("_l$", "_v", embalagem)
    df |> mutate(vol_sel = .data[[embalagem]], val_sel = .data[[col_v]])
  }
}

emb_meta <- tibble(
  rotulo = c("Garrafao", "Garrafa plastica", "Garrafa de vidro", "Copo", "Outros"),
  col_l  = c("garrafao_l", "plastica_l", "vidro_l", "copo_l", "outras_l"),
  col_v  = c("garrafao_v", "plastica_v", "vidro_v", "copo_v", "outras_v")
)

mapa_br <- geobr::read_state(year = 2020, simplified = TRUE, showProgress = FALSE) |>
  sf::st_transform(4326)

coords_mapa <- sf::st_coordinates(sf::st_point_on_surface(mapa_br))
centroides_mapa <- mapa_br |>
  sf::st_drop_geometry() |>
  mutate(lon = coords_mapa[, 1], lat = coords_mapa[, 2]) |>
  select(UF = abbrev_state, lon, lat)

mapa_poly <- local({
  m <- sf::st_cast(mapa_br, "MULTIPOLYGON")
  co <- as.data.frame(sf::st_coordinates(m))
  stopifnot(ncol(co) >= 5)
  names(co)[1:5] <- c("lon", "lat", "anel", "poligono", "feicao")
  co$UF <- m$abbrev_state[co$feicao]
  co$grupo <- paste(co$UF, co$poligono, co$anel, sep = "_")
  co[co$anel == 1, c("lon", "lat", "UF", "grupo")]
})

# ---------- Formatadores, pictograma, tema e helper do plotly ----------
fmt_bi_l <- label_number(scale = 1e-9, accuracy = 0.01, decimal.mark = ",", big.mark = " ", suffix = " Bi L")
fmt_bi_brl <- label_number(scale = 1e-9, accuracy = 0.01, decimal.mark = ",", big.mark = " ", prefix = "R$ ", suffix = " Bi")
fmt_num <- function(x, acc = 0.01) number(x, accuracy = acc, decimal.mark = ",", big.mark = " ")
fmt_pct <- function(x, acc = 0.1) percent(x, accuracy = acc, decimal.mark = ",")

rotulo_emb <- function(chave) {
  if (is.null(chave) || identical(chave, "todas")) return("todas (5 embalagens)")
  nm <- names(emb_cols_all)[match(chave, emb_cols_all)]
  if (is.na(nm)) "todas (5 embalagens)" else nm
}

kpi_item <- function(rotulo, valor, detalhe = "") {
  div(
    class = "kpi",
    div(class = "kpi-label", rotulo),
    div(class = "kpi-valor", valor),
    div(class = "kpi-detalhe", detalhe)
  )
}

# Pictograma no padrao Tufte: preenchimento monocromatico sobrio (#163a42) sobre cinza neutro (#cbd5e1)
card_embalagem_html <- function(nome, icone, tamanho_px, pct_vol, pct_val, metrica = "litros") {
  fill_pct <- min(max(pct_vol * 100, 5), 100)
  top_cut  <- 100 - fill_pct
  cor_tufte <- "#163a42"
  
  peso_vol <- if (identical(metrica, "litros")) "700" else "400"
  peso_val <- if (identical(metrica, "valor"))  "700" else "400"
  cor_vol  <- if (identical(metrica, "litros")) "#0f172a" else "#64748b"
  cor_val  <- if (identical(metrica, "valor"))  "#0f172a" else "#64748b"
  
  HTML(sprintf(
    '
    <div class="picto-card">
      <div style="font-weight:600; color:#163a42; font-size:12px; margin-bottom:8px; min-height:28px; display:flex; align-items:center; justify-content:center;">%s</div>

      <div style="position:relative; display:inline-block; font-size:%dpx; line-height:1; margin:6px 0 10px 0;">
        <i class="%s" style="color:#cbd5e1;"></i>
        <i class="%s" style="color:%s; position:absolute; left:0; top:0; clip-path: inset(%s%% 0 0 0);"></i>
      </div>

      <div style="font-size:11px; font-weight:%s; color:%s;">Vol: %s</div>
      <div style="font-size:11px; font-weight:%s; color:%s;">Val: %s</div>
    </div>
    ',
    nome,
    tamanho_px,
    icone,
    icone,
    cor_tufte,
    format(round(top_cut, 1), nsmall = 1),
    peso_vol,
    cor_vol,
    scales::percent(pct_vol, accuracy = 0.1, decimal.mark = ","),
    peso_val,
    cor_val,
    scales::percent(pct_val, accuracy = 0.1, decimal.mark = ",")
  ))
}

cor_valor  <- "#163a42"
cor_volume <- "#ef5350"
cor_neutra <- "#64748b"

tema_base <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#e8efed", linewidth = 0.3),
      axis.title = element_text(colour = "#475569", size = 10),
      axis.text = element_text(colour = "#334155"),
      legend.position = "none"
    )
}

para_plotly <- function(p) {
  ggplotly(p, tooltip = "text") |>
    layout(showlegend = FALSE, hoverlabel = list(align = "left")) |>
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"))
}

# ---------- 3. Servidor ----------
server <- function(input, output, session) {
  
  output$ui_anos <- renderUI({
    ano_min <- as.integer(min(agua_clean$ano, na.rm = TRUE))
    ano_max <- as.integer(max(agua_clean$ano, na.rm = TRUE))
    sliderInput(
      "anos", "Periodo (Ano base)",
      min = ano_min, max = ano_max,
      value = c(ano_min, ano_max),
      step = 1L, round = TRUE, sep = ""
    )
  })
  
  output$ui_regiao <- renderUI({
    selectInput("regiao", "Regiao",
                choices = c("Todas", sort(unique(na.omit(agua_clean$regiao)))),
                selected = "Todas")
  })
  
  output$ui_uf <- renderUI({
    req(input$anos, input$regiao)
    ufs <- agua_clean |>
      filter(ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
             input$regiao == "Todas" | regiao == input$regiao) |>
      pull(UF) |> unique() |> sort()
    selectInput("uf", "UF", choices = c("Todas", ufs), selected = "Todas")
  })
  
  output$ui_ano_mapa <- renderUI({
    req(input$anos)
    anos_disp <- sort(unique(as.integer(dados_filtrados()$ano)), decreasing = TRUE)
    if (length(anos_disp) == 0) anos_disp <- seq(as.integer(input$anos[2]), as.integer(input$anos[1]), by = -1L)
    padrao <- if (2025L %in% anos_disp) 2025L else anos_disp[1]
    selectInput("ano_mapa", "Ano do mapa", choices = anos_disp, selected = padrao, width = "115px")
  })
  
  dados_filtrados <- reactive({
    req(input$anos, input$regiao, input$uf)
    agua_clean |>
      filter(ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
             input$regiao == "Todas" | regiao == input$regiao,
             input$uf == "Todas" | UF == input$uf)
  })
  
  # =====================  ABA 1: ESTADOS E REGIOES  =====================
  
  output$resumo_mapa_ui <- renderUI({
    req(input$regiao, input$uf, input$embalagem)
    df_recorte <- prepara_base(dados_filtrados(), input$embalagem)
    validate(need(nrow(df_recorte) > 0, "Sem dados para os filtros selecionados."))
    
    ano_ref <- if (2025L %in% df_recorte$ano) 2025L else max(df_recorte$ano, na.rm = TRUE)
    df_2025 <- df_recorte |> filter(ano == ano_ref)
    df_2010 <- df_recorte |> filter(ano == 2010L)
    
    vol_25 <- sum(df_2025$vol_sel, na.rm = TRUE)
    val_25 <- sum(df_2025$val_sel, na.rm = TRUE)
    preco_25 <- if (vol_25 > 0) val_25 / vol_25 else NA_real_
    
    vol_10 <- sum(df_2010$vol_sel, na.rm = TRUE)
    val_10 <- sum(df_2010$val_sel, na.rm = TRUE)
    preco_10 <- if (vol_10 > 0) val_10 / vol_10 else NA_real_
    
    det_vol <- if (nrow(df_2010) > 0 && vol_10 > 0) {
      d_v <- (vol_25 / vol_10) - 1
      paste0(if_else(d_v >= 0, "+", ""), fmt_pct(d_v), " vs 2010")
    } else {
      paste0("Recorte: ", input$regiao, " | UF: ", input$uf)
    }
    
    det_val <- if (nrow(df_2010) > 0 && val_10 > 0) {
      d_p <- (val_25 / val_10) - 1
      paste0(if_else(d_p >= 0, "+", ""), fmt_pct(d_p), " vs 2010")
    } else {
      "Valores nominais"
    }
    
    det_preco <- if (nrow(df_2010) > 0 && !is.na(preco_10) && preco_10 > 0 && !is.na(preco_25)) {
      paste0("2010: R$ ", fmt_num(preco_10), " / L")
    } else {
      "Valor / litros em embalagens"
    }
    
    por_uf <- df_2025 |>
      group_by(UF) |>
      summarise(vol_u = sum(vol_sel, na.rm = TRUE), val_u = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(vol_u))
    uf_top <- if (nrow(por_uf) > 0 && por_uf$vol_u[1] > 0) por_uf$UF[1] else "N/D"
    uf_pct <- if (nrow(por_uf) > 0 && vol_25 > 0) por_uf$vol_u[1] / vol_25 else NA_real_
    
    emb_cols <- if (identical(input$embalagem, "todas")) {
      emb_cols_all[emb_cols_all != "comp_ind_l"]
    } else {
      emb_cols_all[emb_cols_all == input$embalagem]
    }
    por_emb <- bind_rows(lapply(names(emb_cols), function(nome_emb) {
      col_l <- emb_cols[[nome_emb]]
      tibble(embalagem = nome_emb, vol_e = sum(df_2025[[col_l]], na.rm = TRUE))
    })) |>
      arrange(desc(vol_e))
    emb_top <- if (nrow(por_emb) > 0 && por_emb$vol_e[1] > 0) por_emb$embalagem[1] else "N/D"
    emb_pct <- if (nrow(por_emb) > 0 && vol_25 > 0) por_emb$vol_e[1] / vol_25 else NA_real_
    
    div(
      class = "kpi-row",
      kpi_item(paste0("Volume ", ano_ref), fmt_bi_l(vol_25), det_vol),
      kpi_item(paste0("Valor ", ano_ref), fmt_bi_brl(val_25), det_val),
      kpi_item(
        paste0("Preco medio ", ano_ref),
        if (is.na(preco_25)) "N/D" else paste0("R$ ", fmt_num(preco_25), " / L"),
        det_preco
      ),
      kpi_item(
        paste0("UF lider (", ano_ref, ")"),
        uf_top,
        if (!is.na(uf_pct)) paste0(fmt_pct(uf_pct), " do volume") else ""
      ),
      kpi_item(
        paste0("Embalagem lider (", ano_ref, ")"),
        emb_top,
        if (!is.na(emb_pct)) paste0(fmt_pct(emb_pct), " do volume") else ""
      )
    )
  })
  
  serie_ano <- reactive({
    req(input$embalagem)
    prepara_base(dados_filtrados(), input$embalagem) |>
      group_by(ano) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      arrange(ano) |>
      mutate(preco = if_else(vol > 0, val / vol, NA_real_))
  })
  
  output$nota_indice <- renderUI({
    req(input$embalagem)
    s <- serie_ano()
    ano_ini <- if (nrow(s) > 0) min(s$ano) else 2010L
    ano_fim <- if (nrow(s) > 0) max(s$ano) else 2025L
    resumo_cmp <- if (nrow(s) >= 2 && s$vol[1] > 0 && s$val[1] > 0) {
      d_vol <- (s$vol[nrow(s)] / s$vol[1]) - 1
      d_val <- (s$val[nrow(s)] / s$val[1]) - 1
      paste0(
        "Comparacao ", ano_fim, " vs ", ano_ini, ": Valor ",
        if_else(d_val >= 0, "+", ""), fmt_pct(d_val, 0.1),
        " vs Volume ", if_else(d_vol >= 0, "+", ""), fmt_pct(d_vol, 0.1), ". "
      )
    } else {
      ""
    }
    extra <- if (2024L %in% s$ano) " O volume de 2024 destoa dos anos vizinhos: interpretar com cautela." else ""
    div(class = "nota", paste0(resumo_cmp, "Embalagem: ", rotulo_emb(input$embalagem), ". Valores nominais.", extra))
  })
  
  output$plot_indice_vol_val <- renderPlotly({
    s <- serie_ano()
    validate(
      need(nrow(s) >= 2, "Selecione pelo menos 2 anos para comparar a evolucao."),
      need(s$vol[1] > 0 && s$val[1] > 0, "Ano inicial sem volume ou valor positivo para base 100 neste recorte.")
    )
    
    d <- s |>
      mutate(
        idx_vol = (vol / vol[1]) * 100,
        idx_val = (val / val[1]) * 100,
        var_vol = (vol / vol[1]) - 1,
        var_val = (val / val[1]) - 1
      )
    
    ano_ini <- as.integer(min(d$ano))
    ano_fim <- as.integer(max(d$ano))
    
    long <- d |>
      select(ano, idx_vol, idx_val, var_vol, var_val) |>
      pivot_longer(
        cols = c(idx_vol, idx_val),
        names_to = "serie",
        values_to = "indice"
      ) |>
      mutate(
        var_pct = if_else(serie == "idx_vol", var_vol, var_val),
        serie = recode(serie, idx_vol = "Volume", idx_val = "Valor"),
        sinal = if_else(var_pct >= 0, "+", ""),
        tip = paste0(
          serie, " em ", ano, ": ", fmt_num(indice, 0.1),
          " (", sinal, fmt_pct(var_pct, 0.1), " vs ", ano_ini, ")"
        )
      )
    
    fim <- long |> filter(ano == ano_fim)
    
    p <- ggplot() +
      geom_hline(yintercept = 100, linetype = "dotted", colour = "#94a3b8") +
      geom_ribbon(
        data = d,
        aes(x = ano, ymin = pmin(idx_vol, idx_val), ymax = pmax(idx_vol, idx_val)),
        fill = "#10b981",
        alpha = 0.12
      ) +
      geom_line(data = long, aes(x = ano, y = indice, colour = serie, group = serie), linewidth = 1.2) +
      geom_point(data = long, aes(x = ano, y = indice, colour = serie, text = tip), size = 1.8) +
      geom_point(data = fim, aes(x = ano, y = indice, colour = serie, text = tip), size = 3.2) +
      geom_text(
        data = fim,
        aes(
          x = ano, y = indice, colour = serie,
          label = paste0(
            serie, " ", fmt_num(indice, 0.1),
            " (", if_else(var_pct >= 0, "+", ""), fmt_pct(var_pct, 0.1), " vs ", ano_ini, ")"
          )
        ),
        hjust = 0, nudge_x = 0.35, fontface = "bold", size = 3.4
      ) +
      scale_colour_manual(values = c(Volume = cor_volume, Valor = cor_valor)) +
      scale_x_continuous(
        breaks = seq(ano_ini, ano_fim, by = 1L),
        labels = function(x) sprintf("%d", as.integer(x)),
        expand = expansion(mult = c(0.02, 0.35))
      ) +
      scale_y_continuous(labels = label_number(accuracy = 1, big.mark = " ")) +
      labs(x = NULL, y = paste0("Indice (", ano_ini, " = 100)")) +
      tema_base() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    para_plotly(p)
  })
  
  dados_preco_ano_uf <- reactive({
    req(input$anos, input$regiao, input$uf, input$embalagem)
    df_uf_ano <- agua_clean |>
      filter(ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
             input$regiao == "Todas" | regiao == input$regiao) |>
      prepara_base(input$embalagem) |>
      group_by(ano, UF, regiao) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      filter(vol > 0, val > 0) |>
      mutate(preco = val / vol)
    
    if (!identical(input$uf, "Todas")) {
      df_uf_ano <- df_uf_ano |> filter(UF == input$uf)
    } else {
      top_ufs <- df_uf_ano |>
        group_by(UF) |>
        summarise(vol_tot = sum(vol, na.rm = TRUE), .groups = "drop") |>
        arrange(desc(vol_tot)) |>
        slice_head(n = 8) |>
        pull(UF)
      df_uf_ano <- df_uf_ano |> filter(UF %in% top_ufs)
    }
    df_uf_ano
  })
  
  output$nota_preco_ano_uf <- renderUI({
    txt <- if (!identical(input$uf, "Todas")) {
      paste0("Exibindo UF selecionada: ", input$uf, ".")
    } else {
      "Exibindo as 8 UFs de maior volume no recorte (passe o mouse para ver cada estado)."
    }
    div(class = "nota", paste(txt, "Valores nominais (R$/L)."))
  })
  
  output$plot_preco_ano_uf <- renderPlotly({
    d <- dados_preco_ano_uf()
    validate(need(nrow(d) >= 2, "Sem dados suficientes para exibir o preco por ano e UF."))
    
    ano_min <- as.integer(min(d$ano))
    ano_max <- as.integer(max(d$ano))
    
    d <- d |>
      mutate(tip = paste0(UF, " (", ano, "): R$ ", fmt_num(preco), "/L"))
    
    pal_uf <- c(
      "#163a42", "#ef5350", "#10b981", "#2c7fb8",
      "#d97706", "#7c3aed", "#0d9488", "#db2777"
    )
    
    p <- ggplot(d, aes(x = ano, y = preco, colour = UF, group = UF)) +
      geom_line(linewidth = 1.05, alpha = 0.9) +
      geom_point(aes(text = tip), size = 1.8) +
      scale_colour_manual(values = pal_uf) +
      scale_x_continuous(
        breaks = seq(ano_min, ano_max, by = 1L),
        labels = function(x) sprintf("%d", as.integer(x))
      ) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12)),
                         labels = label_number(prefix = "R$ ", accuracy = 0.1, decimal.mark = ",")) +
      labs(x = NULL, y = "Preco medio (R$/L)") +
      tema_base() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    para_plotly(p)
  })
  
  dados_mapa_ano <- reactive({
    req(input$embalagem, input$regiao, input$uf)
    ano_sel <- if (!is.null(input$ano_mapa) && !is.na(as.integer(input$ano_mapa))) {
      as.integer(input$ano_mapa)
    } else if (2025L %in% agua_clean$ano) {
      2025L
    } else {
      max(agua_clean$ano, na.rm = TRUE)
    }
    
    agua_clean |>
      filter(
        ano == ano_sel,
        input$regiao == "Todas" | regiao == input$regiao,
        input$uf == "Todas" | UF == input$uf
      ) |>
      prepara_base(input$embalagem) |>
      group_by(UF) |>
      summarise(
        vol = sum(vol_sel, na.rm = TRUE),
        val = sum(val_sel, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        vol_bi = vol / 1e9,
        val_bi = val / 1e9,
        preco  = if_else(vol > 0, val / vol, NA_real_)
      )
  })
  
  output$nota_mapa <- renderUI({
    req(input$metrica_mapa, input$embalagem)
    ano_txt <- if (!is.null(input$ano_mapa)) input$ano_mapa else "2025"
    met_txt <- switch(
      input$metrica_mapa,
      litros = "volume (Bi L)",
      valor  = "valor (R$ Bi)",
      preco  = "preco medio (R$/L)"
    )
    div(class = "nota", paste0(
      "Ano: ", ano_txt, " | Metrica: ", met_txt,
      " | Embalagem: ", rotulo_emb(input$embalagem),
      ". Passe o mouse sobre a UF para detalhes."
    ))
  })
  
  output$mapa_estatico <- renderPlotly({
    req(input$metrica_mapa, input$regiao, input$uf)
    df_uf <- dados_mapa_ano()
    reg_uf <- agua_clean |> select(UF, regiao) |> distinct()
    
    base_uf <- tibble(UF = mapa_br$abbrev_state) |>
      left_join(reg_uf, by = "UF") |>
      left_join(df_uf, by = "UF") |>
      left_join(centroides_mapa, by = "UF")
    
    if (!identical(input$regiao, "Todas")) base_uf <- base_uf |> filter(regiao == input$regiao)
    if (!identical(input$uf, "Todas")) base_uf <- base_uf |> filter(UF == input$uf)
    validate(need(nrow(base_uf) > 0, "Sem estados para os filtros selecionados."))
    
    base_uf <- base_uf |>
      mutate(
        valor_mapa = case_when(
          identical(input$metrica_mapa, "litros") ~ vol_bi,
          identical(input$metrica_mapa, "valor")  ~ val_bi,
          .default = preco
        ),
        hover = if_else(
          is.na(vol) | vol <= 0,
          paste0(UF, ": sem dados no ano"),
          paste0(UF, " (", regiao, ")",
                 "<br>Volume: ", fmt_num(vol_bi), " Bi L",
                 "<br>Valor: R$ ", fmt_num(val_bi), " Bi",
                 "<br>Preco medio: R$ ", fmt_num(preco), "/L")
        )
      )
    
    lim_med <- median(base_uf$valor_mapa, na.rm = TRUE)
    df_lab <- base_uf |>
      mutate(cor_txt = if_else(!is.na(valor_mapa) & valor_mapa > lim_med, "claro", "escuro"))
    
    poligonos <- mapa_poly |>
      inner_join(base_uf |> select(UF, valor_mapa, hover), by = "UF")
    
    leg_cfg <- switch(
      input$metrica_mapa,
      litros = list(nome = "Volume (Bi L)", lbl = label_number(accuracy = 0.1, decimal.mark = ",", suffix = " Bi L")),
      valor  = list(nome = "Valor (R$ Bi)",  lbl = label_number(accuracy = 0.1, decimal.mark = ",", prefix = "R$ ", suffix = " Bi")),
      list(nome = "Preco (R$/L)", lbl = label_number(accuracy = 0.1, decimal.mark = ",", prefix = "R$ "))
    )
    
    p <- ggplot() +
      geom_polygon(data = poligonos,
                   aes(x = lon, y = lat, group = grupo, fill = valor_mapa, text = hover),
                   colour = "white", linewidth = 0.3) +
      geom_text(data = df_lab, aes(x = lon, y = lat, label = UF, colour = cor_txt),
                size = 2.9, fontface = "bold") +
      scale_fill_gradient(
        low = "#d8efe9", high = "#163a42", na.value = "#e2e8f0",
        name = leg_cfg$nome, labels = leg_cfg$lbl
      ) +
      scale_colour_manual(values = c(claro = "#ffffff", escuro = "#0f172a"), guide = "none") +
      coord_fixed(ratio = 1) +
      theme_void(base_size = 11)
    
    para_plotly(p)
  })
  
  dados_top10_uf <- reactive({
    req(input$anos, input$regiao, input$embalagem, input$metrica)
    usa_litros <- identical(input$metrica, "litros")
    
    df_tot <- agua_clean |>
      filter(ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
             input$regiao == "Todas" | regiao == input$regiao) |>
      prepara_base(input$embalagem) |>
      group_by(UF, regiao) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      mutate(
        medida = if (usa_litros) vol else val,
        preco  = if_else(vol > 0, val / vol, NA_real_)
      ) |>
      filter(medida > 0) |>
      arrange(desc(medida))
    
    total_recorte <- sum(df_tot$medida, na.rm = TRUE)
    
    df_tot |>
      mutate(
        part = medida / total_recorte,
        part_acum = cumsum(part)
      ) |>
      slice_head(n = 10) |>
      mutate(
        medida_b = medida / 1e9,
        UF_ord = factor(UF, levels = rev(UF)),
        rotulo_barra = paste0(
          if (usa_litros) paste0(fmt_num(medida_b), " Bi L") else paste0("R$ ", fmt_num(medida_b), " Bi"),
          " (", fmt_pct(part), " | acum. ", fmt_pct(part_acum), ")"
        ),
        tip = paste0(
          UF, " (", regiao, ")",
          "<br>Volume: ", fmt_num(vol / 1e9), " Bi L",
          "<br>Valor: R$ ", fmt_num(val / 1e9), " Bi",
          "<br>Preco medio: R$ ", fmt_num(preco), "/L",
          "<br>Participacao: ", fmt_pct(part),
          "<br>Participacao acumulada: ", fmt_pct(part_acum)
        )
      )
  })
  
  output$nota_top10_uf <- renderUI({
    d <- dados_top10_uf()
    req(nrow(d) > 0)
    top5_txt <- if (nrow(d) >= 5) paste0("Top 5 = ", fmt_pct(d$part_acum[5]), " | ") else ""
    top10_txt <- paste0("Top ", nrow(d), " = ", fmt_pct(d$part_acum[nrow(d)]))
    div(class = "nota", paste0(
      "Concentracao acumulada no recorte: ", top5_txt, top10_txt,
      ". Respeita periodo, regiao e embalagem."
    ))
  })
  
  output$plot_top10_uf <- renderPlotly({
    d <- dados_top10_uf()
    validate(need(nrow(d) > 0, "Sem UFs com dados para o recorte selecionado."))
    
    p <- ggplot(d, aes(x = medida_b, y = UF_ord)) +
      geom_col(aes(text = tip), fill = "#1c444d", width = 0.68) +
      geom_text(aes(label = rotulo_barra), hjust = 0, nudge_x = 0.015 * max(d$medida_b),
                size = 3.2, fontface = "bold", colour = "#0f172a") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.55))) +
      labs(x = NULL, y = NULL) +
      tema_base() +
      theme(panel.grid = element_blank(), axis.text.x = element_blank())
    
    para_plotly(p)
  })
  
  # =====================  ABA 2: EMBALAGENS  =====================
  resumo_emb <- reactive({
    df <- prepara_base(dados_filtrados(), "todas")
    bind_rows(lapply(seq_len(nrow(emb_meta)), function(i) {
      tibble(
        rotulo = emb_meta$rotulo[i],
        vol = sum(df[[emb_meta$col_l[i]]], na.rm = TRUE),
        val = sum(df[[emb_meta$col_v[i]]], na.rm = TRUE)
      )
    })) |>
      mutate(
        preco = if_else(vol > 0, val / vol, NA_real_),
        pct_vol = if (sum(vol) > 0) vol / sum(vol) else 0,
        pct_val = if (sum(val) > 0) val / sum(val) else 0
      )
  })
  
  output$nota_preco_emb <- renderUI({
    r <- resumo_emb() |> filter(vol > 0, val > 0)
    req(nrow(r) > 0)
    preco_ref <- sum(r$val) / sum(r$vol)
    div(class = "nota", paste0("Linha tracejada: media das embalagens (R$ ", fmt_num(preco_ref), "/L)."))
  })
  
  output$plot_preco_emb <- renderPlotly({
    r <- resumo_emb() |> filter(vol > 0, val > 0)
    validate(need(nrow(r) > 0, "Sem dados de embalagem para os filtros atuais."))
    preco_ref <- sum(r$val) / sum(r$vol)
    r <- r |>
      mutate(
        tip = paste0(rotulo, ": R$ ", fmt_num(preco), "/L<br>",
                     fmt_pct(pct_vol), " do volume | ", fmt_pct(pct_val), " do valor"),
        lab = paste0("R$ ", fmt_num(preco), "/L (", fmt_pct(pct_vol, 0), " do volume)")
      )
    
    p <- ggplot(r, aes(x = preco, y = reorder(rotulo, preco))) +
      geom_col(aes(text = tip), fill = "#1c444d", width = 0.65) +
      geom_vline(xintercept = preco_ref, linetype = "dashed", colour = cor_neutra) +
      geom_text(aes(label = lab), hjust = 0, nudge_x = 0.02 * max(r$preco), size = 3.5,
                fontface = "bold", colour = "#0f172a") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.75))) +
      labs(x = NULL, y = NULL) +
      tema_base() +
      theme(panel.grid = element_blank(), axis.text.x = element_blank())
    
    para_plotly(p)
  })
  
  dados_scatter_emb <- reactive({
    req(input$anos, input$regiao, input$embalagem)
    agua_clean |>
      filter(ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
             input$regiao == "Todas" | regiao == input$regiao) |>
      prepara_base(input$embalagem) |>
      group_by(UF, regiao) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      filter(vol > 0, val > 0) |>
      mutate(vol_bi = vol / 1e9, preco = val / vol)
  })
  
  output$nota_scatter <- renderUI({
    df <- dados_scatter_emb()
    req(nrow(df) >= 3)
    preco_ref <- sum(df$val) / sum(df$vol)
    div(class = "nota", paste0(
      "Tracejado: preco medio (R$ ", fmt_num(preco_ref), "/L) e volume mediano. ",
      "Em vermelho: maior volume, maior R$/L e menor R$/L."
    ))
  })
  
  output$scatter_uf <- renderPlotly({
    df <- dados_scatter_emb()
    validate(need(nrow(df) >= 3, "Poucas UFs com dados para este recorte."))
    
    preco_ref <- sum(df$val) / sum(df$vol)
    vol_ref <- median(df$vol_bi)
    df <- df |>
      mutate(
        destaque = UF %in% c(UF[which.max(preco)], UF[which.min(preco)], UF[which.max(vol_bi)]),
        hover = paste0(UF, " (", regiao, ")",
                       "<br>Volume: ", fmt_num(vol_bi), " Bi L",
                       "<br>Valor: R$ ", fmt_num(val / 1e9), " Bi",
                       "<br>Preco medio: R$ ", fmt_num(preco), "/L")
      )
    
    xmax <- max(df$vol_bi); xmin <- min(df$vol_bi)
    ymax <- max(df$preco);  ymin <- min(df$preco)
    desloc_y <- 0.04 * (ymax - ymin)
    
    p <- ggplot(df, aes(x = vol_bi, y = preco)) +
      geom_vline(xintercept = vol_ref, linetype = "dashed", colour = "#94a3b8") +
      geom_hline(yintercept = preco_ref, linetype = "dashed", colour = "#94a3b8") +
      annotate("text", x = xmax, y = ymax * 1.05, label = "Grande e caro",    hjust = 1, size = 3.2, colour = "#94a3b8", fontface = "italic") +
      annotate("text", x = xmax, y = ymin * 0.95, label = "Grande e barato",  hjust = 1, size = 3.2, colour = "#94a3b8", fontface = "italic") +
      annotate("text", x = xmin, y = ymax * 1.05, label = "Pequeno e caro",   hjust = 0, size = 3.2, colour = "#94a3b8", fontface = "italic") +
      annotate("text", x = xmin, y = ymin * 0.95, label = "Pequeno e barato", hjust = 0, size = 3.2, colour = "#94a3b8", fontface = "italic") +
      geom_point(aes(fill = destaque, text = hover), shape = 21, size = 4, colour = "white", stroke = 0.6) +
      geom_text(aes(label = UF, colour = destaque), size = 3.1, fontface = "bold", nudge_y = desloc_y) +
      scale_fill_manual(values = c("FALSE" = "#94a3b8", "TRUE" = "#c0392b"), guide = "none") +
      scale_colour_manual(values = c("FALSE" = "#334155", "TRUE" = "#c0392b"), guide = "none") +
      scale_x_log10(labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
      scale_y_continuous(labels = label_number(prefix = "R$ ", accuracy = 0.01, decimal.mark = ",")) +
      labs(x = "Volume produzido (Bi L, escala log)", y = "Preco medio (R$/L)") +
      tema_base() +
      theme(panel.grid.major.x = element_line(colour = "#e8efed", linewidth = 0.3))
    
    para_plotly(p)
  })
  
  output$cards_embalagens_html <- renderUI({
    req(input$metrica)
    r <- resumo_emb()
    validate(need(sum(r$vol) > 0 && sum(r$val) > 0, "Sem dados positivos para os filtros selecionados."))
    
    cfg_picto <- tibble(
      rotulo = c("Garrafao", "Garrafa plastica", "Garrafa de vidro", "Copo", "Outros"),
      nome_card = c("Garrafao", "Garrafa Plastica", "Garrafa de Vidro", "Copo", "Outros"),
      icone = c(
        "fa-solid fa-bottle-water",
        "fa-solid fa-bottle-water",
        "fa-solid fa-bottle-droplet",
        "fa-solid fa-glass-water",
        "fa-solid fa-box-open"
      ),
      px = c(56L, 50L, 48L, 48L, 46L)
    )
    
    tab <- cfg_picto |>
      left_join(r, by = "rotulo") |>
      mutate(
        pct_vol = replace_na(pct_vol, 0),
        pct_val = replace_na(pct_val, 0)
      )
    
    div(
      class = "picto-grid",
      lapply(seq_len(nrow(tab)), function(i) {
        card_embalagem_html(
          nome       = tab$nome_card[i],
          icone      = tab$icone[i],
          tamanho_px = tab$px[i],
          pct_vol    = tab$pct_vol[i],
          pct_val    = tab$pct_val[i],
          metrica    = input$metrica
        )
      })
    )
  })
  
  output$plot_preco_emb_ano <- renderPlotly({
    df <- prepara_base(dados_filtrados(), "todas")
    serie <- bind_rows(lapply(seq_len(nrow(emb_meta)), function(i) {
      df |>
        group_by(ano) |>
        summarise(vol = sum(.data[[emb_meta$col_l[i]]], na.rm = TRUE),
                  val = sum(.data[[emb_meta$col_v[i]]], na.rm = TRUE), .groups = "drop") |>
        mutate(rotulo = emb_meta$rotulo[i])
    })) |>
      filter(vol > 0, val > 0) |>
      mutate(preco = val / vol,
             rotulo = factor(rotulo, levels = emb_meta$rotulo),
             tip = paste0(rotulo, " ", ano, ": R$ ", fmt_num(preco), "/L"))
    
    validate(need(n_distinct(serie$ano) >= 2, "Selecione pelo menos 2 anos para ver a evolucao."))
    ult <- serie |> group_by(rotulo) |> filter(ano == max(ano)) |> ungroup()
    desloc <- 0.1 * max(serie$preco)
    
    p <- ggplot(serie, aes(x = ano, y = preco)) +
      geom_line(colour = cor_valor, linewidth = 1) +
      geom_point(aes(text = tip), colour = cor_valor, size = 1.6) +
      geom_point(data = ult, aes(text = tip), colour = cor_valor, size = 2.8) +
      geom_text(data = ult, aes(label = paste0("R$ ", fmt_num(preco))), nudge_y = desloc, size = 3.3, fontface = "bold") +
      facet_wrap(~rotulo, nrow = 1) +
      scale_x_continuous(breaks = breaks_pretty(3), labels = label_number(accuracy = 1, big.mark = ""),
                         expand = expansion(mult = c(0.05, 0.12))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.2)),
                         labels = label_number(prefix = "R$ ", accuracy = 0.1, decimal.mark = ",")) +
      labs(x = NULL, y = NULL) +
      tema_base() +
      theme(strip.text = element_text(face = "bold", hjust = 0, colour = "#163a42"))
    
    para_plotly(p)
  })
}

shinyApp(ui, server)