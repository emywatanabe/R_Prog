library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(geobr)
library(sf)
library(DT)

# 1. Interface (UI) no topo
header <- dashboardHeader(
  title = "Agua Mineral (2010-2025)"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "abas",
    menuItem("Visao Geral", tabName = "visao_geral", icon = icon("chart-line")),
    menuItem("Mapa do Brasil", tabName = "mapa_brasil", icon = icon("map"))
  ),
  hr(),
  uiOutput("ui_anos"),
  uiOutput("ui_regiao"),
  uiOutput("ui_uf"),
  selectInput(
    "embalagem",
    "Tipo de embalagem",
    choices = c(
      "Todas" = "todas",
      "Garrafao" = "garrafao_l",
      "Garrafa Plastica" = "plastica_l",
      "Garrafa de Vidro" = "vidro_l",
      "Copo" = "copo_l",
      "Outras Embalagens" = "outras_l",
      "Comp. Prod. Industrializados" = "comp_ind_l"
    ),
    selected = "todas"
  ),
  radioButtons(
    "metrica",
    "Metrica de referencia",
    choices = c(
      "Volume em embalagens (Bi L)" = "litros",
      "Valor total declarado (R$ Bi)" = "valor"
    ),
    selected = "litros"
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #e6f4f0 !important;
      }
      .main-header .logo {
        background-color: #163a42 !important;
        color: #ffffff !important;
        font-weight: 600;
      }
      .main-header .logo:hover {
        background-color: #123037 !important;
      }
      .main-header .navbar {
        background-color: #d8efe9 !important;
      }
      .main-header .navbar .sidebar-toggle {
        color: #163a42 !important;
      }
      .main-header .navbar .sidebar-toggle:hover {
        background-color: #c5e6de !important;
      }
      .main-sidebar, .left-side {
        background-color: #1c444d !important;
      }
      .sidebar-menu > li > a {
        color: #d7e8e4 !important;
      }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li > a:hover {
        background-color: #265965 !important;
        color: #ffffff !important;
        border-left-color: #10b981 !important;
      }
      .main-sidebar hr {
        border-top-color: rgba(255, 255, 255, 0.14);
      }
      .small-box {
        background-color: #ffffff !important;
        color: #1e293b !important;
        border: 1px solid #d5e5e0 !important;
        border-radius: 6px !important;
        box-shadow: 0 1px 2px rgba(16, 58, 66, 0.04) !important;
      }
      .small-box h3 {
        color: #0f172a !important;
        font-weight: 700;
      }
      .small-box p {
        color: #475569 !important;
      }
      .small-box .icon-large, .small-box .fa, .small-box .glyphicon {
        color: #10b981 !important;
        opacity: 0.85;
      }
      .box {
        background-color: #ffffff !important;
        border: 1px solid #d5e5e0 !important;
        border-top: 3px solid #1c444d !important;
        border-radius: 6px !important;
        box-shadow: 0 1px 2px rgba(16, 58, 66, 0.04) !important;
      }
      .box-header .box-title {
        color: #163a42 !important;
        font-weight: 600;
      }
      .resumo-card {
        background-color: #f8fcfa;
        border: 1px solid #d5e5e0;
        border-left: 4px solid #10b981;
        border-radius: 5px;
        padding: 10px 14px;
        margin-bottom: 10px;
      }
      .resumo-label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.4px;
        color: #475569;
        margin-bottom: 3px;
      }
      .resumo-valor {
        font-size: 19px;
        font-weight: 700;
        color: #0f172a;
      }
      .resumo-detalhe {
        font-size: 13px;
        color: #334155;
        margin-top: 2px;
      }
      .rodape-fonte {
        margin-top: 6px;
        padding: 8px 12px;
        font-size: 12px;
        color: #334155;
        border-top: 1px solid #cde0da;
      }
      .rodape-fonte a {
        color: #163a42;
        font-weight: 600;
        text-decoration: underline;
      }
    "))
  ),
  tabItems(
    # ABA 1: Visao Geral
    tabItem(
      tabName = "visao_geral",
      fluidRow(
        valueBoxOutput("kpi_vol_acum", width = 3),
        valueBoxOutput("kpi_val_acum", width = 3),
        valueBoxOutput("kpi_preco_medio", width = 3),
        valueBoxOutput("kpi_var_periodo", width = 3)
      ),
      fluidRow(
        box(
          title = "Evolucao relativa: Valor vs. Volume ao longo do tempo (Base 100)",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          plotOutput("plot_indice_vol_val", height = "440px")
        )
      ),
      fluidRow(
        box(
          title = "Embalagens: Participacao em Volume (%) e Valor (%)",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          uiOutput("cards_embalagens_html")
        )
      )
    ),
    
    # ABA 2: Mapa Estatico por Regiao e UF + Resumo do Recorte + Tabela DT
    tabItem(
      tabName = "mapa_brasil",
      fluidRow(
        box(
          title = "Mapa por regiao e UF (valores em bilhoes)",
          status = "primary",
          solidHeader = FALSE,
          width = 7,
          plotOutput("mapa_estatico", height = "620px")
        ),
        box(
          title = "Resumo do recorte selecionado",
          status = "primary",
          solidHeader = FALSE,
          width = 5,
          uiOutput("resumo_mapa_ui")
        )
      ),
      fluidRow(
        box(
          title = "Detalhamento do recorte (valores em bilhoes)",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          div(
            style = "margin-bottom: 10px;",
            radioButtons(
              "modo_tabela_mapa",
              "Agrupar tabela por:",
              choices = c("UF" = "uf", "Tipo de embalagem" = "embalagem"),
              selected = "uf",
              inline = TRUE
            )
          ),
          DTOutput("tabela_detalhe_mapa")
        )
      )
    )
  ),
  div(
    class = "rodape-fonte",
    HTML(
      'Fonte: Dados Abertos (<a href="https://dados.gov.br/dados/conjuntos-dados/anuario-mineral-brasileiro-amb" target="_blank">Anuario Mineral Brasileiro / ANM</a>)'
    )
  )
)

ui <- dashboardPage(
  skin = "blue",
  header = header,
  sidebar = sidebar,
  body = body
)

# 2. Leitura e limpeza dos dados
parse_br_num <- function(x) {
  as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))
}

agua_raw <- read.csv(
  "CE303 - visualização/Agua_Mineral_Producao.csv",
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

agua_clean <- agua_raw |>
  mutate(
    ano = as.integer(`Ano base`),
    regiao = case_when(
      UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      UF %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      UF %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      UF %in% c("PR", "RS", "SC") ~ "Sul",
      .default = NA_character_
    ),
    garrafao_l = parse_br_num(`Quantidade (litros) Garrafăo`),
    plastica_l = parse_br_num(`Quantidade (litros) Garrafa Plástica`),
    vidro_l    = parse_br_num(`Quantidade (litros) Garrafa de Vidro`),
    copo_l     = parse_br_num(`Quantidade (litros) Copo`),
    outras_l   = parse_br_num(`Quantidade (litros) Outras Embalagens`),
    comp_ind_l = parse_br_num(`Quantidade (litros) Composiçăo Produtos Industrializados`),
    volume_tot = parse_br_num(volume),
    valor_tot  = parse_br_num(valor),
    total_litros_emb = rowSums(
      across(c(garrafao_l, plastica_l, vidro_l, copo_l, outras_l)),
      na.rm = TRUE
    )
  )

mapa_br <- geobr::read_state(year = 2020, simplified = TRUE, showProgress = FALSE) |>
  sf::st_transform(4326)

coords_mapa <- sf::st_coordinates(sf::st_point_on_surface(mapa_br))
centroides_mapa <- mapa_br |>
  sf::st_drop_geometry() |>
  mutate(
    lon = coords_mapa[, 1],
    lat = coords_mapa[, 2]
  ) |>
  select(UF = abbrev_state, lon, lat)

fmt_bi_l <- label_number(
  scale = 1e-9,
  accuracy = 0.01,
  decimal.mark = ",",
  big.mark = " ",
  suffix = " Bi L"
)

fmt_bi_brl <- label_number(
  scale = 1e-9,
  accuracy = 0.01,
  decimal.mark = ",",
  big.mark = " ",
  prefix = "R$ ",
  suffix = " Bi"
)

fmt_b_dinamico <- function(x_b, usa_litros) {
  if (is.null(x_b) || is.na(x_b)) return("N/D")
  txt_num <- number(x_b, accuracy = 0.01, decimal.mark = ",", big.mark = " ")
  if (usa_litros) paste0(txt_num, " Bi L") else paste0("R$ ", txt_num, " Bi")
}

card_embalagem_html <- function(nome, icone, cor, tamanho_px, pct_vol, pct_val, metrica = "litros") {
  fill_pct <- min(max(pct_vol * 100, 5), 100)
  top_cut  <- 100 - fill_pct
  
  peso_vol <- if (identical(metrica, "litros")) "700" else "400"
  peso_val <- if (identical(metrica, "valor"))  "700" else "400"
  cor_vol  <- if (identical(metrica, "litros")) "#0f172a" else "#475569"
  cor_val  <- if (identical(metrica, "valor"))  "#0f172a" else "#475569"
  
  HTML(sprintf(
    '
    <div style="background:#f8fcfa; border:1px solid #d5e5e0; border-radius:6px; padding:16px 10px; text-align:center; margin-bottom:10px;">
      <div style="font-weight:700; color:#163a42; font-size:14px; margin-bottom:10px;">%s</div>

      <div style="position:relative; display:inline-block; font-size:%dpx; line-height:1; margin:8px 0 12px 0;">
        <i class="%s" style="color:#cbd5e1;"></i>
        <i class="%s" style="color:%s; position:absolute; left:0; top:0; clip-path: inset(%s%% 0 0 0);"></i>
      </div>

      <div style="font-size:13px; font-weight:%s; color:%s;">Volume: %s</div>
      <div style="font-size:13px; font-weight:%s; color:%s;">Valor: %s</div>
    </div>
    ',
    nome,
    tamanho_px,
    icone,
    icone,
    cor,
    format(round(top_cut, 1), nsmall = 1),
    peso_vol,
    cor_vol,
    scales::percent(pct_vol, accuracy = 0.1, decimal.mark = ","),
    peso_val,
    cor_val,
    scales::percent(pct_val, accuracy = 0.1, decimal.mark = ",")
  ))
}

# 3. Servidor (Server)
server <- function(input, output, session) {
  output$ui_anos <- renderUI({
    ano_min <- min(agua_clean$ano, na.rm = TRUE)
    ano_max <- max(agua_clean$ano, na.rm = TRUE)
    sliderInput(
      "anos",
      "Periodo (Ano base)",
      min = ano_min,
      max = ano_max,
      value = c(ano_min, ano_max),
      sep = ""
    )
  })
  
  output$ui_regiao <- renderUI({
    selectInput(
      "regiao",
      "Regiao",
      choices = c("Todas", sort(unique(na.omit(agua_clean$regiao)))),
      selected = "Todas"
    )
  })
  
  output$ui_uf <- renderUI({
    req(input$anos, input$regiao)
    ufs <- agua_clean |>
      filter(
        ano >= input$anos[1],
        ano <= input$anos[2],
        input$regiao == "Todas" | regiao == input$regiao
      ) |>
      pull(UF) |>
      unique() |>
      sort()
    
    selectInput("uf", "UF", choices = c("Todas", ufs), selected = "Todas")
  })
  
  dados_filtrados <- reactive({
    req(input$anos, input$regiao, input$uf)
    agua_clean |>
      filter(
        ano >= input$anos[1],
        ano <= input$anos[2],
        input$regiao == "Todas" | regiao == input$regiao,
        input$uf == "Todas" | UF == input$uf
      )
  })
  
  var_periodo_txt <- function(atual, base, ano_ini) {
    if (is.null(base) || is.na(base) || base == 0 || is.na(atual)) {
      return(paste0("sem base em ", ano_ini))
    }
    sinal <- if_else(atual >= base, "▲ ", "▼ ")
    paste0(
      sinal,
      percent(abs(atual / base - 1), accuracy = 0.1, decimal.mark = ","),
      " vs ", ano_ini
    )
  }
  
  kpis_dinamicos <- reactive({
    req(input$anos)
    df <- dados_filtrados()
    
    vol_acum <- sum(df$total_litros_emb, na.rm = TRUE)
    vol_tot_acum <- sum(df$volume_tot, na.rm = TRUE)
    val_acum <- sum(df$valor_tot, na.rm = TRUE)
    denom_acum <- if (vol_tot_acum > 0) vol_tot_acum else vol_acum
    preco_acum <- if (denom_acum > 0) val_acum / denom_acum else NA_real_
    
    ano_ini <- input$anos[1]
    ano_fim <- input$anos[2]
    
    resumo_ano <- function(a) {
      d <- df |> filter(ano == a)
      vol_emb <- sum(d$total_litros_emb, na.rm = TRUE)
      vol_tot <- sum(d$volume_tot, na.rm = TRUE)
      val_tot <- sum(d$valor_tot, na.rm = TRUE)
      denom   <- if (vol_tot > 0) vol_tot else vol_emb
      preco_l <- if (denom > 0) val_tot / denom else NA_real_
      
      list(
        vol_emb = vol_emb,
        val_tot = val_tot,
        preco_l = preco_l
      )
    }
    
    list(
      vol_acum   = vol_acum,
      val_acum   = val_acum,
      preco_acum = preco_acum,
      ano_ini    = ano_ini,
      ano_fim    = ano_fim,
      k_ini      = resumo_ano(ano_ini),
      k_fim      = resumo_ano(ano_fim)
    )
  })
  
  output$kpi_vol_acum <- renderValueBox({
    res <- kpis_dinamicos()
    valueBox(
      value = fmt_bi_l(res$vol_acum),
      subtitle = paste0("Qtde acumulada (", res$ano_ini, "-", res$ano_fim, ")"),
      icon = icon("bottle-water")
    )
  })
  
  output$kpi_val_acum <- renderValueBox({
    res <- kpis_dinamicos()
    valueBox(
      value = fmt_bi_brl(res$val_acum),
      subtitle = paste0("Valor acumulado (", res$ano_ini, "-", res$ano_fim, ")"),
      icon = icon("coins")
    )
  })
  
  output$kpi_preco_medio <- renderValueBox({
    res <- kpis_dinamicos()
    preco <- res$preco_acum
    txt_preco <- if (is.na(preco)) {
      "N/D"
    } else {
      paste0("R$ ", number(preco, accuracy = 0.01, decimal.mark = ","), " / L")
    }
    
    sub_txt <- if (res$ano_fim > res$ano_ini) {
      paste0("R$/litro medio, ", var_periodo_txt(res$k_fim$preco_l, res$k_ini$preco_l, res$ano_ini))
    } else {
      paste0("R$/litro medio em ", res$ano_fim)
    }
    
    valueBox(
      value = txt_preco,
      subtitle = sub_txt,
      icon = icon("tag")
    )
  })
  
  output$kpi_var_periodo <- renderValueBox({
    req(input$metrica)
    res <- kpis_dinamicos()
    usa_litros <- identical(input$metrica, "litros")
    atual_m <- if (usa_litros) res$k_fim$vol_emb else res$k_fim$val_tot
    base_m  <- if (usa_litros) res$k_ini$vol_emb else res$k_ini$val_tot
    rotulo_m <- if (usa_litros) "Volume" else "Valor"
    
    var_num <- if (!is.na(base_m) && base_m > 0 && !is.na(atual_m)) {
      (atual_m / base_m) - 1
    } else {
      NA_real_
    }
    
    txt_var <- if (is.na(var_num)) {
      "N/D"
    } else {
      sinal <- if_else(var_num >= 0, "+", "")
      paste0(sinal, percent(var_num, accuracy = 0.1, decimal.mark = ","))
    }
    
    valueBox(
      value = txt_var,
      subtitle = paste0("Variacao ", rotulo_m, " (", res$ano_fim, " vs ", res$ano_ini, ")"),
      icon = icon("chart-line")
    )
  })
  
  # Grafico 1 da Aba 1: Valor vs. Volume ao longo do tempo (Base 100)
  output$plot_indice_vol_val <- renderPlot({
    df_serie <- dados_filtrados() |>
      filter(!is.na(ano)) |>
      group_by(ano) |>
      summarise(
        vol = sum(total_litros_emb, na.rm = TRUE),
        val = sum(valor_tot, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(ano)
    
    validate(
      need(nrow(df_serie) >= 2, "Selecione pelo menos 2 anos para comparar a evolucao.")
    )
    
    base_row <- df_serie |> slice(1)
    validate(
      need(base_row$vol > 0 && base_row$val > 0, "Ano inicial sem volume ou valor positivo para base 100.")
    )
    
    df_idx <- df_serie |>
      mutate(
        idx_vol = (vol / base_row$vol) * 100,
        idx_val = (val / base_row$val) * 100
      )
    
    df_long <- df_idx |>
      select(ano, idx_vol, idx_val) |>
      pivot_longer(
        cols = c(idx_val, idx_vol),
        names_to = "serie",
        values_to = "indice"
      ) |>
      mutate(
        serie = recode(
          serie,
          idx_val = "Valor declarado (Base 100)",
          idx_vol = "Volume em embalagens (Base 100)"
        )
      )
    
    ano_ini <- min(df_idx$ano)
    ano_fim <- max(df_idx$ano)
    ult <- df_idx |> slice(n())
    
    ggplot() +
      geom_ribbon(
        data = df_idx,
        aes(x = ano, ymin = pmin(idx_vol, idx_val), ymax = pmax(idx_vol, idx_val)),
        fill = "#10b981",
        alpha = 0.16
      ) +
      geom_line(
        data = df_long,
        aes(x = ano, y = indice, colour = serie),
        linewidth = 1.25
      ) +
      geom_point(
        data = df_long,
        aes(x = ano, y = indice, colour = serie),
        size = 2.4
      ) +
      scale_colour_manual(
        values = c(
          "Valor declarado (Base 100)" = "#163a42",
          "Volume em embalagens (Base 100)" = "#ef5350"
        ),
        name = NULL
      ) +
      scale_x_continuous(
        breaks = sort(unique(df_idx$ano)),
        expand = expansion(mult = c(0.02, 0.04))
      ) +
      scale_y_continuous(
        labels = number_format(accuracy = 1, decimal.mark = ",")
      ) +
      labs(
        title = paste0("Crescimento acumulado desde ", ano_ini, " (", ano_ini, " = 100)"),
        subtitle = paste0(
          "Em ", ano_fim, ": Valor = ", number(ult$idx_val, accuracy = 0.1, decimal.mark = ","),
          " | Volume = ", number(ult$idx_vol, accuracy = 0.1, decimal.mark = ",")
        ),
        x = "Ano base",
        y = paste0("Indice (", ano_ini, " = 100)")
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 10, colour = "#334155")
      )
  })
  
    # Cartoes HTML da Aba 1: Copo, Garrafao, Garrafa Plastica e Garrafa de Vidro
    output$cards_embalagens_html <- renderUI({
      req(input$anos, input$regiao, input$uf, input$metrica)
      
      df_base <- dados_filtrados() |>
        mutate(
          across(
            c(garrafao_l, plastica_l, vidro_l, copo_l, outras_l, comp_ind_l),
            ~ replace_na(as.numeric(.x), 0)
          ),
          total_linha_l = garrafao_l + plastica_l + vidro_l + copo_l + outras_l + comp_ind_l,
          valor_clean   = replace_na(as.numeric(valor_tot), 0)
        )
      
      emb_map <- list(
        list(chave = "garrafao_l", rotulo = "Garrafao",         icone = "fa-solid fa-bottle-water",   cor = "#163a42", px = 64),
        list(chave = "plastica_l", rotulo = "Garrafa Plastica", icone = "fa-solid fa-bottle-water",   cor = "#ef5350", px = 54),
        list(chave = "vidro_l",    rotulo = "Garrafa de Vidro", icone = "fa-solid fa-bottle-droplet", cor = "#10b981", px = 52),
        list(chave = "copo_l",     rotulo = "Copo",             icone = "fa-solid fa-glass-water",    cor = "#2c7fb8", px = 52)
      )
      
      resumo <- bind_rows(lapply(emb_map, function(item) {
        vol_emb <- sum(df_base[[item$chave]], na.rm = TRUE)
        prop_v  <- ifelse(df_base$total_linha_l > 0, df_base[[item$chave]] / df_base$total_linha_l, 0)
        val_emb <- sum(df_base$valor_clean * prop_v, na.rm = TRUE)
        tibble(
          rotulo  = item$rotulo,
          icone   = item$icone,
          cor     = item$cor,
          px      = item$px,
          vol_emb = vol_emb,
          val_emb = val_emb
        )
      }))
      
      tot_vol <- sum(resumo$vol_emb, na.rm = TRUE)
      tot_val <- sum(resumo$val_emb, na.rm = TRUE)
      
      validate(
        need(tot_vol > 0 && tot_val > 0, "Sem dados positivos para os filtros selecionados.")
      )
      
      tab <- resumo |>
        mutate(
          pct_vol = vol_emb / tot_vol,
          pct_val = val_emb / tot_val
        )
      
      fluidRow(
        lapply(seq_len(nrow(tab)), function(i) {
          column(
            width = 3,
            card_embalagem_html(
              nome       = tab$rotulo[i],
              icone      = tab$icone[i],
              cor        = tab$cor[i],
              tamanho_px = tab$px[i],
              pct_vol    = tab$pct_vol[i],
              pct_val    = tab$pct_val[i],
              metrica    = input$metrica
            )
          )
        })
      )
    })
  
  # Base reativa da Aba 2 respeitando embalagem e metrica de referencia
  dados_mapa_base <- reactive({
    req(input$embalagem, input$metrica)
    usa_litros <- identical(input$metrica, "litros")
    
    df_base <- dados_filtrados() |>
      filter(!is.na(UF), !is.na(ano)) |>
      mutate(
        across(
          c(garrafao_l, plastica_l, vidro_l, copo_l, outras_l, comp_ind_l),
          ~ replace_na(.x, 0)
        ),
        total_linha_l = garrafao_l + plastica_l + vidro_l + copo_l + outras_l + comp_ind_l,
        valor_clean   = replace_na(valor_tot, 0)
      )
    
    if (identical(input$embalagem, "todas")) {
      df_base <- df_base |>
        mutate(
          vol_sel = replace_na(total_litros_emb, 0),
          val_sel = valor_clean
        )
    } else {
      col_emb <- df_base[[input$embalagem]]
      df_base <- df_base |>
        mutate(
          vol_sel  = col_emb,
          prop_emb = if_else(total_linha_l > 0, col_emb / total_linha_l, 0),
          val_sel  = valor_clean * prop_emb
        )
    }
    
    df_base |>
      mutate(
        medida_bruta = if_else(rep(usa_litros, n()), vol_sel, val_sel),
        medida_b     = medida_bruta / 1e9
      )
  })
  
  output$mapa_estatico <- renderPlot({
    req(input$metrica, input$anos, input$regiao, input$uf)
    usa_litros <- identical(input$metrica, "litros")
    
    df_uf <- dados_mapa_base() |>
      group_by(UF, regiao) |>
      summarise(
        valor_b = sum(medida_b, na.rm = TRUE),
        .groups = "drop"
      )
    
    mapa_regiao <- mapa_br |>
      sf::st_drop_geometry() |>
      select(UF = abbrev_state) |>
      distinct() |>
      left_join(
        agua_clean |> select(UF, regiao) |> distinct(),
        by = "UF"
      )
    
    mapa_sub <- mapa_br |>
      left_join(mapa_regiao, by = c("abbrev_state" = "UF")) |>
      left_join(df_uf |> select(UF, valor_b), by = c("abbrev_state" = "UF")) |>
      left_join(centroides_mapa, by = c("abbrev_state" = "UF"))
    
    if (!identical(input$regiao, "Todas")) {
      mapa_sub <- mapa_sub |> filter(regiao == input$regiao)
    }
    if (!identical(input$uf, "Todas")) {
      mapa_sub <- mapa_sub |> filter(abbrev_state == input$uf)
    }
    
    validate(
      need(nrow(mapa_sub) > 0, "Sem estados para os filtros selecionados.")
    )
    
    pal_regiao <- c(
      "Norte"        = "#26a69a",
      "Nordeste"     = "#ef5350",
      "Centro-Oeste" = "#f59e0b",
      "Sudeste"      = "#1c444d",
      "Sul"          = "#3b82f6"
    )
    
    df_pts <- mapa_sub |>
      sf::st_drop_geometry() |>
      filter(!is.na(valor_b), valor_b > 0)
    
    leg_tam <- if (usa_litros) {
      label_number(accuracy = 0.01, decimal.mark = ",", suffix = " Bi L")
    } else {
      label_number(accuracy = 0.01, decimal.mark = ",", prefix = "R$ ", suffix = " Bi")
    }
    
    ggplot() +
      geom_sf(
        data = mapa_sub,
        aes(fill = regiao),
        alpha = 0.18,
        colour = "#64748b",
        linewidth = 0.35
      ) +
      geom_point(
        data = df_pts,
        aes(x = lon, y = lat, size = valor_b, fill = regiao),
        shape = 21,
        colour = "#0f172a",
        stroke = 0.65,
        alpha = 0.88
      ) +
      geom_text(
        data = df_pts,
        aes(x = lon, y = lat, label = abbrev_state),
        size = 2.8,
        fontface = "bold",
        colour = "#0f172a"
      ) +
      scale_fill_manual(
        values = pal_regiao,
        na.value = "#e2e8f0",
        name = "Regiao"
      ) +
      scale_size_area(
        max_size = 19,
        name = if (usa_litros) "Volume (Bi L)" else "Valor (R$ Bi)",
        labels = leg_tam
      ) +
      guides(
        fill = guide_legend(
          override.aes = list(shape = 21, size = 5, alpha = 0.88, colour = "#0f172a")
        )
      ) +
      labs(
        subtitle = paste0(
          "Periodo: ", input$anos[1], " a ", input$anos[2],
          " | Metrica: ", if (usa_litros) "Volume (Bi L)" else "Valor declarado (R$ Bi)"
        )
      ) +
      theme_void(base_size = 11) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.subtitle = element_text(hjust = 0.5, colour = "#334155", margin = margin(b = 6))
      )
  })
  
  output$resumo_mapa_ui <- renderUI({
    req(input$metrica, input$anos, input$regiao, input$uf)
    usa_litros <- identical(input$metrica, "litros")
    df_base <- dados_mapa_base()
    
    total_b   <- sum(df_base$medida_b, na.rm = TRUE)
    tot_vol_l <- sum(df_base$vol_sel, na.rm = TRUE)
    tot_val_r <- sum(df_base$val_sel, na.rm = TRUE)
    preco_medio_l <- if (tot_vol_l > 0) tot_val_r / tot_vol_l else NA_real_
    
    txt_preco_medio <- if (is.na(preco_medio_l)) {
      "N/D"
    } else {
      paste0("R$ ", number(preco_medio_l, accuracy = 0.01, decimal.mark = ","), " / L")
    }
    
    por_uf <- df_base |>
      group_by(UF) |>
      summarise(total_uf_b = sum(medida_b, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_uf_b))
    
    uf_top   <- if (nrow(por_uf) > 0 && por_uf$total_uf_b[1] > 0) por_uf$UF[1] else "N/D"
    uf_val_b <- if (nrow(por_uf) > 0) por_uf$total_uf_b[1] else NA_real_
    uf_pct   <- if (!is.na(uf_val_b) && total_b > 0) uf_val_b / total_b else NA_real_
    
    emb_cols <- c(
      "Garrafao" = "garrafao_l",
      "Garrafa Plastica" = "plastica_l",
      "Garrafa de Vidro" = "vidro_l",
      "Copo" = "copo_l",
      "Outras Embalagens" = "outras_l",
      "Comp. Prod. Industrializados" = "comp_ind_l"
    )
    
    por_emb <- bind_rows(lapply(names(emb_cols), function(nome_emb) {
      col_l <- emb_cols[[nome_emb]]
      d_e <- df_base |>
        mutate(
          prop_e = if_else(total_linha_l > 0, .data[[col_l]] / total_linha_l, 0),
          med_e_b = if_else(
            rep(usa_litros, n()),
            .data[[col_l]] / 1e9,
            (valor_clean * prop_e) / 1e9
          )
        )
      tibble(embalagem = nome_emb, total_emb_b = sum(d_e$med_e_b, na.rm = TRUE))
    })) |>
      arrange(desc(total_emb_b))
    
    emb_top   <- if (nrow(por_emb) > 0 && por_emb$total_emb_b[1] > 0) por_emb$embalagem[1] else "N/D"
    emb_val_b <- if (nrow(por_emb) > 0) por_emb$total_emb_b[1] else NA_real_
    emb_pct   <- if (!is.na(emb_val_b) && total_b > 0) emb_val_b / total_b else NA_real_
    
    por_ano <- df_base |>
      group_by(ano) |>
      summarise(total_ano_b = sum(medida_b, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_ano_b))
    
    ano_top   <- if (nrow(por_ano) > 0 && por_ano$total_ano_b[1] > 0) por_ano$ano[1] else NA_integer_
    ano_val_b <- if (nrow(por_ano) > 0) por_ano$total_ano_b[1] else NA_real_
    
    tagList(
      div(
        class = "resumo-card",
        div(class = "resumo-label", paste0("Recorte: ", input$regiao, " | UF: ", input$uf)),
        div(class = "resumo-valor", fmt_b_dinamico(total_b, usa_litros)),
        div(
          class = "resumo-detalhe",
          paste0("Periodo: ", input$anos[1], " a ", input$anos[2])
        )
      ),
      div(
        class = "resumo-card",
        div(class = "resumo-label", "Preco medio no recorte"),
        div(class = "resumo-valor", txt_preco_medio),
        div(
          class = "resumo-detalhe",
          paste0(
            "Valor: ", fmt_bi_brl(tot_val_r),
            " | Volume: ", fmt_bi_l(tot_vol_l)
          )
        )
      ),
      div(
        class = "resumo-card",
        div(class = "resumo-label", "Estado lider no recorte"),
        div(class = "resumo-valor", uf_top),
        div(
          class = "resumo-detalhe",
          paste0(
            fmt_b_dinamico(uf_val_b, usa_litros),
            if (!is.na(uf_pct)) paste0(" (", percent(uf_pct, accuracy = 0.1, decimal.mark = ","), " do recorte)") else ""
          )
        )
      ),
      div(
        class = "resumo-card",
        div(class = "resumo-label", "Tipo de embalagem mais produzido"),
        div(class = "resumo-valor", emb_top),
        div(
          class = "resumo-detalhe",
          paste0(
            fmt_b_dinamico(emb_val_b, usa_litros),
            if (!is.na(emb_pct)) paste0(" (", percent(emb_pct, accuracy = 0.1, decimal.mark = ","), " do recorte)") else ""
          )
        )
      ),
      div(
        class = "resumo-card",
        div(class = "resumo-label", "Ano de maior producao / valor"),
        div(class = "resumo-valor", if (is.na(ano_top)) "N/D" else as.character(ano_top)),
        div(
          class = "resumo-detalhe",
          fmt_b_dinamico(ano_val_b, usa_litros)
        )
      )
    )
  })
  
  output$tabela_detalhe_mapa <- renderDT({
    req(input$modo_tabela_mapa, input$metrica)
    df_base <- dados_mapa_base()
    
    validate(
      need(nrow(df_base) > 0, "Sem dados para exibir na tabela com os filtros atuais.")
    )
    
    if (identical(input$modo_tabela_mapa, "uf")) {
      tab_uf <- df_base |>
        group_by(Regiao = regiao, UF) |>
        summarise(
          vol_bi    = sum(vol_sel, na.rm = TRUE) / 1e9,
          val_bi    = sum(val_sel, na.rm = TRUE) / 1e9,
          vol_bruto = sum(vol_sel, na.rm = TRUE),
          val_bruto = sum(val_sel, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(
          preco_l = if_else(vol_bruto > 0, val_bruto / vol_bruto, NA_real_)
        ) |>
        arrange(desc(if (identical(input$metrica, "litros")) vol_bi else val_bi)) |>
        transmute(
          Regiao = Regiao,
          UF = UF,
          `Volume (Bi L)` = round(vol_bi, 2),
          `Valor (R$ Bi)` = round(val_bi, 2),
          `Preco medio (R$/L)` = round(preco_l, 2)
        )
      
      col_barra <- if (identical(input$metrica, "litros")) "Volume (Bi L)" else "Valor (R$ Bi)"
      cor_barra <- if (identical(input$metrica, "litros")) "#c8e6df" else "#d1fae5"
      
      datatable(
        tab_uf,
        rownames = FALSE,
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
            infoEmpty = "Nenhum registro encontrado",
            paginate = list(previous = "Anterior", `next` = "Proximo")
          )
        )
      ) |>
        formatRound(c("Volume (Bi L)", "Valor (R$ Bi)", "Preco medio (R$/L)"), digits = 2, mark = " ", dec.mark = ",") |>
        formatStyle(
          col_barra,
          background = styleColorBar(range(tab_uf[[col_barra]], na.rm = TRUE), cor_barra),
          backgroundSize = "96% 72%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    } else {
      emb_cols <- c(
        "Garrafao" = "garrafao_l",
        "Garrafa Plastica" = "plastica_l",
        "Garrafa de Vidro" = "vidro_l",
        "Copo" = "copo_l",
        "Outras Embalagens" = "outras_l",
        "Comp. Prod. Industrializados" = "comp_ind_l"
      )
      
      tab_emb <- bind_rows(lapply(names(emb_cols), function(nome_emb) {
        col_l <- emb_cols[[nome_emb]]
        d_e <- df_base |>
          mutate(
            prop_e = if_else(total_linha_l > 0, .data[[col_l]] / total_linha_l, 0),
            val_e  = valor_clean * prop_e
          )
        tibble(
          Embalagem = nome_emb,
          vol_bi = sum(d_e[[col_l]], na.rm = TRUE) / 1e9,
          val_bi = sum(d_e$val_e, na.rm = TRUE) / 1e9
        )
      }))
      
      tot_m <- sum(if (identical(input$metrica, "litros")) tab_emb$vol_bi else tab_emb$val_bi, na.rm = TRUE)
      
      tab_emb <- tab_emb |>
        mutate(
          part_pct = if (tot_m > 0) {
            100 * (if (identical(input$metrica, "litros")) vol_bi else val_bi) / tot_m
          } else {
            0
          }
        ) |>
        arrange(desc(if (identical(input$metrica, "litros")) vol_bi else val_bi)) |>
        transmute(
          Embalagem = Embalagem,
          `Volume (Bi L)` = round(vol_bi, 2),
          `Valor (R$ Bi)` = round(val_bi, 2),
          `Participacao (%)` = round(part_pct, 1)
        )
      
      col_barra <- if (identical(input$metrica, "litros")) "Volume (Bi L)" else "Valor (R$ Bi)"
      cor_barra <- if (identical(input$metrica, "litros")) "#c8e6df" else "#d1fae5"
      
      datatable(
        tab_emb,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          scrollX = TRUE,
          language = list(
            search = "Buscar:",
            infoEmpty = "Nenhum registro encontrado"
          )
        )
      ) |>
        formatRound(c("Volume (Bi L)", "Valor (R$ Bi)"), digits = 2, mark = " ", dec.mark = ",") |>
        formatRound("Participacao (%)", digits = 1, mark = " ", dec.mark = ",") |>
        formatStyle(
          col_barra,
          background = styleColorBar(range(tab_emb[[col_barra]], na.rm = TRUE), cor_barra),
          backgroundSize = "96% 72%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    }
  })
}

shinyApp(ui, server)
  