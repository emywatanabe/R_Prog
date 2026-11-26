# =====================================================================================
# server.R -- logica reativa e graficos (usa objetos definidos em global.R)
# =====================================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Filtros da barra lateral
  # ---------------------------------------------------------------------------
  output$ui_anos <- renderUI({
    ano_min <- as.integer(min(agua_clean$ano, na.rm = TRUE))
    ano_max <- as.integer(max(agua_clean$ano, na.rm = TRUE))
    sliderInput(
      "anos", "Período (Ano base)",
      min = ano_min, max = ano_max,
      value = c(ano_min, ano_max),
      step = 1L, round = TRUE, sep = ""
    )
  })

  output$ui_regiao <- renderUI({
    selectInput("regiao", "Região",
                choices = c("Todas", sort(unique(na.omit(agua_clean$regiao)))),
                selected = "Todas")
  })

  # A UF escolhida e mantida quando o periodo muda (so volta para "Todas" se deixar de existir na regiao)
  output$ui_uf <- renderUI({
    req(input$anos, input$regiao)
    ufs <- agua_clean |>
      filter(ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
             input$regiao == "Todas" | regiao == input$regiao) |>
      pull(UF) |> unique() |> sort()
    atual <- isolate(input$uf)
    selecionada <- if (!is.null(atual) && atual %in% ufs) atual else "Todas"
    selectInput("uf", "UF", choices = c("Todas", ufs), selected = selecionada)
  })

  # O ano do mapa/ranking so depende do periodo; a escolha do usuario e mantida ao mudar regiao/UF
  output$ui_ano_mapa <- renderUI({
    req(input$anos)
    anos_disp <- seq(as.integer(input$anos[2]), as.integer(input$anos[1]), by = -1L)
    atual <- isolate(input$ano_mapa)
    selecionado <- if (!is.null(atual) && as.character(atual) %in% as.character(anos_disp)) {
      as.character(atual)
    } else {
      as.character(anos_disp[1])
    }
    selectInput("ano_mapa", "Ano do mapa e do ranking", choices = anos_disp,
                selected = selecionado, width = "190px")
  })

  ano_mapa_sel <- reactive({
    a <- suppressWarnings(as.integer(input$ano_mapa))
    if (length(a) == 1 && !is.na(a)) a else max(agua_clean$ano, na.rm = TRUE)
  })

  dados_filtrados <- reactive({
    req(input$anos, input$regiao, input$uf)
    agua_clean |>
      filter(ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
             input$regiao == "Todas" | regiao == input$regiao,
             input$uf == "Todas" | UF == input$uf)
  })

  # ===========================================================================
  # ABA 1: CONTEXTO HISTORICO
  # ===========================================================================

  # Cartoes: ultimo ano do periodo, comparado com o primeiro ano do periodo
  output$kpis_ui <- renderUI({
    req(input$regiao, input$uf, input$embalagem)
    df_recorte <- prepara_base(dados_filtrados(), input$embalagem)
    validate(need(nrow(df_recorte) > 0, "Sem dados para os filtros selecionados."))

    ano_ref  <- max(df_recorte$ano, na.rm = TRUE)
    ano_base <- min(df_recorte$ano, na.rm = TRUE)
    tem_base <- ano_ref > ano_base

    df_ref  <- df_recorte |> filter(ano == ano_ref)
    df_base <- df_recorte |> filter(ano == ano_base)

    vol_ref <- sum(df_ref$vol_sel, na.rm = TRUE)
    val_ref <- sum(df_ref$val_sel, na.rm = TRUE)
    preco_ref <- if (vol_ref > 0) val_ref / vol_ref else NA_real_

    vol_base <- sum(df_base$vol_sel, na.rm = TRUE)
    val_base <- sum(df_base$val_sel, na.rm = TRUE)
    preco_base <- if (vol_base > 0) val_base / vol_base else NA_real_

    var_vs_base <- function(atual, base, padrao) {
      if (!tem_base || is.na(base) || base <= 0 || is.na(atual)) return(padrao)
      d <- (atual / base) - 1
      paste0(if (d >= 0) "+" else "", fmt_pct(d), " vs ", ano_base)
    }

    det_vol <- var_vs_base(vol_ref, vol_base, paste0("Recorte: ", input$regiao, " | UF: ", input$uf))
    det_val <- var_vs_base(val_ref, val_base, "Valores nominais")
    det_preco <- if (tem_base && !is.na(preco_base) && preco_base > 0) {
      paste0(ano_base, ": R$ ", fmt_num(preco_base), " / L")
    } else {
      "Valor / litros em embalagens"
    }

    por_uf <- df_ref |>
      group_by(UF) |>
      summarise(vol_u = sum(vol_sel, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(vol_u))
    uf_top <- if (nrow(por_uf) > 0 && por_uf$vol_u[1] > 0) por_uf$UF[1] else "N/D"
    uf_pct <- if (nrow(por_uf) > 0 && vol_ref > 0) por_uf$vol_u[1] / vol_ref else NA_real_

    emb_cols <- if (identical(input$embalagem, "todas")) {
      emb_cols_all[emb_cols_all != "comp_ind_l"]
    } else {
      emb_cols_all[emb_cols_all == input$embalagem]
    }
    por_emb <- bind_rows(lapply(names(emb_cols), function(nome_emb) {
      tibble(embalagem = nome_emb, vol_e = sum(df_ref[[emb_cols[[nome_emb]]]], na.rm = TRUE))
    })) |>
      arrange(desc(vol_e))
    emb_top <- if (nrow(por_emb) > 0 && por_emb$vol_e[1] > 0) por_emb$embalagem[1] else "N/D"
    emb_pct <- if (nrow(por_emb) > 0 && vol_ref > 0) por_emb$vol_e[1] / vol_ref else NA_real_

    div(
      class = "kpi-row",
      kpi_item(paste0("Volume ", ano_ref), fmt_bi_l(vol_ref), det_vol),
      kpi_item(paste0("Valor ", ano_ref), fmt_bi_brl(val_ref), det_val),
      kpi_item(
        paste0("Preço médio ", ano_ref),
        if (is.na(preco_ref)) "N/D" else paste0("R$ ", fmt_num(preco_ref), " / L"),
        det_preco
      ),
      kpi_item(
        paste0("UF líder (", ano_ref, ")"),
        uf_top,
        if (!is.na(uf_pct)) paste0(fmt_pct(uf_pct), " do volume") else ""
      ),
      kpi_item(
        paste0("Embalagem líder (", ano_ref, ")"),
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
      mutate(
        vol_bi = vol / 1e9,
        val_bi = val / 1e9,
        preco  = if_else(vol > 0, val / vol, NA_real_)
      )
  })

  output$nota_vol_val_tempo <- renderUI({
    s <- serie_ano()
    req(nrow(s) >= 2)
    ano_ini <- min(s$ano)
    ano_fim <- max(s$ano)
    d_vol <- if (s$vol[1] > 0) (s$vol[nrow(s)] / s$vol[1]) - 1 else NA_real_
    d_val <- if (s$val[1] > 0) (s$val[nrow(s)] / s$val[1]) - 1 else NA_real_
    txt_var <- if (!is.na(d_vol) && !is.na(d_val)) {
      paste0(
        "Variação ", ano_fim, " vs ", ano_ini, ": Valor ",
        if (d_val >= 0) "+" else "", fmt_pct(d_val, 0.1),
        " vs Volume ", if (d_vol >= 0) "+" else "", fmt_pct(d_vol, 0.1), ". "
      )
    } else {
      ""
    }
    div(class = "nota", paste0(
      txt_var,
      "Linhas verticais marcam 2010, 2015, 2020 e 2025. Embalagem: ", rotulo_emb(input$embalagem),
      ". Valores nominais."
    ))
  })

  output$plot_vol_val_tempo <- renderPlotly({
    s <- serie_ano()
    validate(need(nrow(s) >= 2, "Selecione pelo menos 2 anos para ver a evolução temporal."))

    long <- s |>
      select(ano, vol_bi, val_bi, preco) |>
      pivot_longer(cols = c(vol_bi, val_bi), names_to = "serie", values_to = "valor_bi") |>
      mutate(
        serie_lbl = recode(serie, vol_bi = "Volume (Bi L)", val_bi = "Valor (R$ Bi)"),
        tip = paste0(
          serie_lbl, " em ", ano, ": ",
          if_else(serie == "val_bi", paste0("R$ ", fmt_num(valor_bi), " Bi"), paste0(fmt_num(valor_bi), " Bi L")),
          "<br>Preço médio: R$ ", fmt_num(preco), "/L"
        )
      )

    anos_marco <- intersect(c(2010L, 2015L, 2020L, 2025L), unique(as.integer(s$ano)))
    y_top <- max(long$valor_bi, na.rm = TRUE) * 1.12

    p <- ggplot() +
      { if (length(anos_marco) > 0) geom_vline(xintercept = anos_marco, linetype = "dashed", colour = "#94a3b8", linewidth = 0.5) } +
      { if (length(anos_marco) > 0) annotate("text", x = anos_marco, y = y_top, label = as.character(anos_marco),
                                             vjust = 1, hjust = -0.15, size = 3.1, colour = "#475569", fontface = "bold") } +
      geom_line(data = long, aes(x = ano, y = valor_bi, colour = serie_lbl, group = serie_lbl), linewidth = 1.25) +
      geom_point(data = long, aes(x = ano, y = valor_bi, colour = serie_lbl, text = tip), size = 2.1) +
      scale_colour_manual(values = c("Volume (Bi L)" = cor_volume, "Valor (R$ Bi)" = cor_valor)) +
      scale_x_continuous(
        breaks = seq(min(s$ano), max(s$ano), by = 1L),
        labels = function(x) sprintf("%d", as.integer(x))
      ) +
      scale_y_continuous(
        limits = c(0, y_top * 1.05),
        labels = label_number(accuracy = 0.1, decimal.mark = ",", big.mark = " ")
      ) +
      labs(x = NULL, y = "Bi L  /  R$ Bi") +
      tema_base() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    para_plotly(p)
  })

  # Dispersao bruta: cada ponto e uma UF em um ano (todas as embalagens, sem filtro de embalagem)
  dados_dotplot_bruto <- reactive({
    req(input$anos, input$regiao, input$uf)
    agua_clean |>
      filter(
        ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
        input$regiao == "Todas" | regiao == input$regiao,
        input$uf == "Todas" | UF == input$uf
      ) |>
      prepara_base("todas") |>
      group_by(ano, UF, regiao) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      filter(vol > 0, val > 0) |>
      mutate(
        preco = val / vol,
        tip = paste0(
          UF, " (", regiao, ") - ", ano,
          "<br>Preço médio bruto: R$ ", fmt_num(preco), "/L",
          "<br>Volume: ", fmt_num(vol / 1e9), " Bi L",
          "<br>Valor: R$ ", fmt_num(val / 1e9), " Bi"
        )
      )
  })

  output$nota_dotplot_preco_ano_uf <- renderUI({
    d <- dados_dotplot_bruto()
    req(nrow(d) > 0)
    div(class = "nota", paste0(
      "Cada ponto cinza é uma UF em um ano; a linha é o preço médio ponderado do recorte. ",
      "Todas as embalagens somadas (o filtro de embalagem não se aplica). ",
      "Faixa observada: R$ ", fmt_num(min(d$preco)), "/L a R$ ", fmt_num(max(d$preco)), "/L."
    ))
  })

  output$dotplot_preco_ano_uf <- renderPlotly({
    d <- dados_dotplot_bruto()
    validate(need(nrow(d) > 0, "Sem dados para exibir no recorte selecionado."))

    media_ano <- d |>
      group_by(ano) |>
      summarise(preco = sum(val) / sum(vol), .groups = "drop") |>
      mutate(tip = paste0("Média do recorte em ", ano, ": R$ ", fmt_num(preco), "/L"))
    fim <- media_ano[nrow(media_ano), ]

    anos_marco <- intersect(c(2010L, 2015L, 2020L, 2025L), unique(as.integer(d$ano)))
    y_max <- max(d$preco, na.rm = TRUE) * 1.08

    p <- ggplot(d, aes(x = ano, y = preco)) +
      { if (length(anos_marco) > 0) geom_vline(xintercept = anos_marco, linetype = "dashed", colour = "#cbd5e1", linewidth = 0.5) } +
      geom_point(aes(text = tip), colour = "#94a3b8", alpha = 0.75, size = 2.3) +
      geom_line(data = media_ano, colour = cor_valor, linewidth = 1.2) +
      geom_point(data = media_ano, aes(text = tip), colour = cor_valor, size = 2.2) +
      geom_text(data = fim, aes(label = paste0("Média R$ ", fmt_num(preco))),
                hjust = 0, nudge_x = 0.3, fontface = "bold", size = 3.4, colour = cor_valor) +
      scale_x_continuous(
        breaks = seq(min(d$ano), max(d$ano), by = 1L),
        labels = function(x) sprintf("%d", as.integer(x)),
        expand = expansion(mult = c(0.02, 0.14))
      ) +
      scale_y_continuous(
        limits = c(0, y_max),
        labels = label_number(prefix = "R$ ", accuracy = 0.05, decimal.mark = ",")
      ) +
      labs(x = NULL, y = "Preço médio bruto (R$/L)") +
      tema_base() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    para_plotly(p)
  })

  # ===========================================================================
  # ABA 2: EMBALAGENS
  # ===========================================================================
  resumo_emb <- reactive({
    df <- prepara_base(dados_filtrados(), "todas")
    anos_df <- sort(unique(df$ano))
    ano_ini <- if (length(anos_df) > 0) min(anos_df) else NA_integer_
    ano_fim <- if (length(anos_df) > 0) max(anos_df) else NA_integer_

    preco_no_ano <- function(col_l, col_v, ano_alvo) {
      if (is.na(ano_alvo)) return(NA_real_)
      d <- df |> filter(ano == ano_alvo)
      v <- sum(d[[col_l]], na.rm = TRUE)
      m <- sum(d[[col_v]], na.rm = TRUE)
      if (v > 0 && m > 0) m / v else NA_real_
    }

    bind_rows(lapply(seq_len(nrow(emb_meta)), function(i) {
      col_l <- emb_meta$col_l[i]
      col_v <- emb_meta$col_v[i]
      vol_tot <- sum(df[[col_l]], na.rm = TRUE)
      val_tot <- sum(df[[col_v]], na.rm = TRUE)
      p_ini <- preco_no_ano(col_l, col_v, ano_ini)
      p_fim <- preco_no_ano(col_l, col_v, ano_fim)
      delta_pct <- if (!is.na(p_ini) && p_ini > 0 && !is.na(p_fim)) (p_fim / p_ini) - 1 else NA_real_

      tibble(
        rotulo = emb_meta$rotulo[i],
        vol = vol_tot,
        val = val_tot,
        preco = if_else(vol_tot > 0, val_tot / vol_tot, NA_real_),
        delta_pct = delta_pct,
        ano_ini = ano_ini,
        ano_fim = ano_fim
      )
    })) |>
      mutate(
        pct_vol = if (sum(vol) > 0) vol / sum(vol) else 0,
        pct_val = if (sum(val) > 0) val / sum(val) else 0
      )
  })

  output$cards_preco_emb_ui <- renderUI({
    r <- resumo_emb()
    validate(need(nrow(r) > 0 && sum(r$vol) > 0, "Sem dados de embalagem para o recorte atual."))

    div(
      class = "emb-kpi-grid",
      lapply(seq_len(nrow(r)), function(i) {
        preco_txt <- if (is.na(r$preco[i])) "N/D" else paste0("R$ ", fmt_num(r$preco[i]), " / L")
        delta_txt <- if (is.na(r$delta_pct[i])) {
          "Variação N/D"
        } else {
          sinal <- if (r$delta_pct[i] >= 0) "+" else ""
          paste0("Δ ", sinal, fmt_pct(r$delta_pct[i], 0.1), " (", r$ano_ini[i], "–", r$ano_fim[i], ")")
        }
        cor_delta <- if (!is.na(r$delta_pct[i]) && r$delta_pct[i] >= 0) "#163a42" else "#ef5350"

        div(
          class = "emb-kpi-card",
          div(class = "emb-kpi-nome", r$rotulo[i]),
          div(class = "emb-kpi-preço", preco_txt),
          div(class = "emb-kpi-sub", style = paste0("color:", cor_delta, "; font-weight:600;"), delta_txt),
          div(class = "emb-kpi-sub", paste0("Vol: ", fmt_pct(r$pct_vol[i]), " | Val: ", fmt_pct(r$pct_val[i])))
        )
      })
    )
  })

  # Dispersao por UF: ignora o filtro de UF (senao seria um ponto so)
  dados_scatter_emb <- reactive({
    req(input$anos, input$regiao, input$embalagem)
    agua_clean |>
      filter(
        ano >= as.integer(input$anos[1]), ano <= as.integer(input$anos[2]),
        input$regiao == "Todas" | regiao == input$regiao
      ) |>
      prepara_base(input$embalagem) |>
      group_by(UF, regiao) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      filter(vol > 0, val > 0) |>
      mutate(vol_bi = vol / 1e9, preco = val / vol)
  })

  output$nota_scatter <- renderUI({
    df <- dados_scatter_emb()
    req(nrow(df) >= 3)
    uf_caro <- df$UF[which.max(df$preco)]
    uf_barato <- df$UF[which.min(df$preco)]
    uf_maior_vol <- df$UF[which.max(df$vol_bi)]

    div(class = "nota", paste0(
      "Embalagem ativa: ", rotulo_emb(input$embalagem),
      " | Maior volume: ", uf_maior_vol, " (", fmt_num(max(df$vol_bi)), " Bi L)",
      " | Maior R$/L: ", uf_caro, " (R$ ", fmt_num(max(df$preco)), "/L)",
      " | Menor R$/L: ", uf_barato, " (R$ ", fmt_num(min(df$preco)), "/L). ",
      "Linhas tracejadas: volume mediano e preço médio ponderado. Não muda com o filtro de UF."
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
        hover = paste0(
          UF, " (", regiao, ")",
          "<br>Embalagem: ", rotulo_emb(input$embalagem),
          "<br>Volume: ", fmt_num(vol_bi), " Bi L",
          "<br>Valor: R$ ", fmt_num(val / 1e9), " Bi",
          "<br>Preço médio: R$ ", fmt_num(preco), "/L"
        )
      )

    xmax <- max(df$vol_bi); xmin <- min(df$vol_bi)
    ymax <- max(df$preco);  ymin <- min(df$preco)
    desloc_y <- 0.04 * (ymax - ymin)

    p <- ggplot(df, aes(x = vol_bi, y = preco)) +
      geom_vline(xintercept = vol_ref, linetype = "dashed", colour = "#94a3b8") +
      geom_hline(yintercept = preco_ref, linetype = "dashed", colour = "#94a3b8") +
      annotate("text", x = xmax, y = ymax * 1.05, label = "Maior volume e mais caro",     hjust = 1, size = 3.1, colour = "#64748b", fontface = "italic") +
      annotate("text", x = xmax, y = ymin * 0.95, label = "Maior volume e mais barato",   hjust = 1, size = 3.1, colour = "#64748b", fontface = "italic") +
      annotate("text", x = xmin, y = ymax * 1.05, label = "Menor volume e mais caro",     hjust = 0, size = 3.1, colour = "#64748b", fontface = "italic") +
      annotate("text", x = xmin, y = ymin * 0.95, label = "Menor volume e mais barato",   hjust = 0, size = 3.1, colour = "#64748b", fontface = "italic") +
      geom_point(aes(fill = destaque, text = hover), shape = 21, size = 4.2, colour = "white", stroke = 0.6) +
      geom_text(aes(label = UF, colour = destaque), size = 3.1, fontface = "bold", nudge_y = desloc_y) +
      scale_fill_manual(values = c("FALSE" = "#94a3b8", "TRUE" = "#c0392b"), guide = "none") +
      scale_colour_manual(values = c("FALSE" = "#334155", "TRUE" = "#c0392b"), guide = "none") +
      scale_x_log10(labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
      scale_y_continuous(labels = label_number(prefix = "R$ ", accuracy = 0.01, decimal.mark = ",")) +
      labs(x = "Volume produzido (Bi L, escala log)", y = "Preço médio (R$/L)") +
      tema_base() +
      theme(panel.grid.major.x = element_line(colour = "#e8efed", linewidth = 0.3))

    para_plotly(p)
  })

  # Mesmo criterio de variacao dos cartoes: preco no primeiro e no ultimo ano do periodo
  serie_preco_emb <- reactive({
    df <- prepara_base(dados_filtrados(), "todas")
    req(nrow(df) > 0)
    ano_ini <- min(df$ano)
    ano_fim <- max(df$ano)

    serie_raw <- bind_rows(lapply(seq_len(nrow(emb_meta)), function(i) {
      df |>
        group_by(ano) |>
        summarise(
          vol = sum(.data[[emb_meta$col_l[i]]], na.rm = TRUE),
          val = sum(.data[[emb_meta$col_v[i]]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(rotulo = emb_meta$rotulo[i])
    })) |>
      filter(vol > 0, val > 0) |>
      mutate(preco = val / vol)

    deltas <- serie_raw |>
      group_by(rotulo) |>
      summarise(
        p_ini = preco[ano == ano_ini][1],
        p_fim = preco[ano == ano_fim][1],
        .groups = "drop"
      ) |>
      mutate(
        ano_ini = ano_ini,
        ano_fim = ano_fim,
        delta_pct = if_else(!is.na(p_ini) & p_ini > 0 & !is.na(p_fim), (p_fim / p_ini) - 1, NA_real_),
        rotulo_delta = if_else(
          is.na(delta_pct),
          rotulo,
          paste0(rotulo, " (", if_else(delta_pct >= 0, "+", ""), fmt_pct(delta_pct, 0.1), ")")
        )
      )

    niveis <- deltas$rotulo_delta[match(emb_meta$rotulo, deltas$rotulo)]
    niveis <- niveis[!is.na(niveis)]

    serie_raw |>
      left_join(deltas |> select(rotulo, delta_pct, rotulo_delta, ano_ini, ano_fim), by = "rotulo") |>
      mutate(
        rotulo_delta = factor(rotulo_delta, levels = niveis),
        tip = paste0(
          rotulo, " (", ano, "): R$ ", fmt_num(preco), "/L",
          if_else(!is.na(delta_pct),
                  paste0("<br>Variação ", ano_ini, "–", ano_fim, ": ",
                         if_else(delta_pct >= 0, "+", ""), fmt_pct(delta_pct, 0.1)),
                  "")
        )
      )
  })

  output$nota_preco_emb_ano <- renderUI({
    s <- serie_preco_emb()
    req(nrow(s) > 0)
    d_res <- s |> distinct(rotulo, delta_pct) |> filter(!is.na(delta_pct)) |> arrange(desc(delta_pct))
    destaque <- if (nrow(d_res) > 0) {
      paste0("Maior alta nominal: ", d_res$rotulo[1], " (",
             if (d_res$delta_pct[1] >= 0) "+" else "", fmt_pct(d_res$delta_pct[1], 0.1), "). ")
    } else ""
    div(class = "nota", paste0(destaque, "Mesma escala vertical em todos os paineis. Valores nominais."))
  })

  output$plot_preco_emb_ano <- renderPlotly({
    serie <- serie_preco_emb()
    validate(need(n_distinct(serie$ano) >= 2, "Selecione pelo menos 2 anos para ver a evolução."))

    ult <- serie |> group_by(rotulo_delta) |> filter(ano == max(ano)) |> ungroup()
    desloc <- 0.1 * max(serie$preco, na.rm = TRUE)

    p <- ggplot(serie, aes(x = ano, y = preco)) +
      geom_line(colour = cor_valor, linewidth = 1) +
      geom_point(aes(text = tip), colour = cor_valor, size = 1.6) +
      geom_point(data = ult, aes(text = tip), colour = cor_valor, size = 2.8) +
      geom_text(data = ult, aes(label = paste0("R$ ", fmt_num(preco))), nudge_y = desloc, size = 3.2, fontface = "bold") +
      facet_wrap(~rotulo_delta, nrow = 1) +
      scale_x_continuous(
        breaks = breaks_pretty(3),
        labels = label_number(accuracy = 1, big.mark = ""),
        expand = expansion(mult = c(0.05, 0.12))
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.2)),
        labels = label_number(prefix = "R$ ", accuracy = 0.1, decimal.mark = ",")
      ) +
      labs(x = NULL, y = NULL) +
      tema_base() +
      theme(strip.text = element_text(face = "bold", hjust = 0, colour = "#163a42", size = 9.5))

    para_plotly(p)
  })

  # ===========================================================================
  # ABA 3: ESTADOS E REGIOES (mapa e ranking usam o mesmo ano e a mesma metrica)
  # ===========================================================================
  dados_mapa_ano <- reactive({
    req(input$embalagem, input$regiao, input$uf)
    agua_clean |>
      filter(
        ano == ano_mapa_sel(),
        input$regiao == "Todas" | regiao == input$regiao,
        input$uf == "Todas" | UF == input$uf
      ) |>
      prepara_base(input$embalagem) |>
      group_by(UF) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      mutate(
        vol_bi = vol / 1e9,
        val_bi = val / 1e9,
        preco  = if_else(vol > 0, val / vol, NA_real_)
      )
  })

  output$nota_mapa <- renderUI({
    req(input$metrica_mapa, input$embalagem)
    met_txt <- switch(
      input$metrica_mapa,
      litros = "volume (Bi L)",
      valor  = "valor (R$ Bi)",
      preco  = "preço médio (R$/L)"
    )
    div(class = "nota", paste0(
      "Ano: ", ano_mapa_sel(), " | Métrica: ", met_txt,
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

    base_uf$valor_mapa <- switch(
      input$metrica_mapa,
      litros = base_uf$vol_bi,
      valor  = base_uf$val_bi,
      base_uf$preco
    )

    base_uf <- base_uf |>
      mutate(
        hover = if_else(
          is.na(vol) | vol <= 0,
          paste0(UF, ": sem dados no ano"),
          paste0(UF, " (", regiao, ")",
                 "<br>Volume: ", fmt_num(vol_bi), " Bi L",
                 "<br>Valor: R$ ", fmt_num(val_bi), " Bi",
                 "<br>Preço médio: R$ ", fmt_num(preco), "/L")
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
      list(nome = "Preço (R$/L)", lbl = label_number(accuracy = 0.1, decimal.mark = ",", prefix = "R$ "))
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

  # Ranking: mesmo ano do mapa; ordena por valor se a metrica for valor, senao por volume
  # (o preco medio nao mede tamanho, entao nesse caso o ranking continua por volume)
  dados_top10_uf <- reactive({
    req(input$regiao, input$embalagem, input$metrica_mapa)
    usa_valor <- identical(input$metrica_mapa, "valor")

    df_tot <- agua_clean |>
      filter(ano == ano_mapa_sel(),
             input$regiao == "Todas" | regiao == input$regiao) |>
      prepara_base(input$embalagem) |>
      group_by(UF, regiao) |>
      summarise(vol = sum(vol_sel, na.rm = TRUE), val = sum(val_sel, na.rm = TRUE), .groups = "drop") |>
      mutate(
        medida = if (usa_valor) val else vol,
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
        grupo_top = if_else(row_number() <= 5, "Top 5", "6º ao 10º"),
        rotulo_barra = paste0(
          if (usa_valor) paste0("R$ ", fmt_num(medida_b), " Bi") else paste0(fmt_num(medida_b), " Bi L"),
          " (", fmt_pct(part), " | acum. ", fmt_pct(part_acum), ")"
        )
      )
  })

  output$nota_top10_uf <- renderUI({
    d <- dados_top10_uf()
    req(nrow(d) > 0)
    base_txt <- if (identical(input$metrica_mapa, "valor")) "valor" else "volume"
    top5_txt <- if (nrow(d) >= 5) paste0("Top 5 = ", fmt_pct(d$part_acum[5]), " | ") else ""
    top10_txt <- paste0("Top ", nrow(d), " = ", fmt_pct(d$part_acum[nrow(d)]))
    div(class = "nota", paste0(
      "Ano: ", ano_mapa_sel(), " | Ranking por ", base_txt, " | Concentração acumulada: ", top5_txt, top10_txt,
      ". Respeita região e embalagem; não muda com o filtro de UF."
    ))
  })

  # Ranking estatico (sem hover): fontes grandes para leitura na apresentacao
  output$plot_top10_uf <- renderPlot({
    d <- dados_top10_uf()
    validate(need(nrow(d) > 0, "Sem UFs com dados para o recorte selecionado."))

    ggplot(d, aes(x = medida_b, y = UF_ord, fill = grupo_top)) +
      geom_col(width = 0.68) +
      geom_text(
        aes(label = rotulo_barra),
        hjust = 0,
        nudge_x = 0.015 * max(d$medida_b),
        size = 5,
        fontface = "bold",
        colour = "#0f172a"
      ) +
      scale_fill_manual(values = c("Top 5" = "#1c444d", "6º ao 10º" = "#8fb8b2"), guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.58))) +
      labs(x = NULL, y = NULL) +
      tema_base() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 14, colour = "#0f172a")
      )
  }, bg = "transparent")
}
