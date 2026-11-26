# =====================================================================================
# global.R -- pacotes, dados e funcoes auxiliares (carregado por app.R antes de ui.R e server.R)
#
# Estrutura do dashboard (3 abas):
#   1. Contexto Historico  -> KPIs + Volume vs Valor ao longo do tempo + dispersao de R$/L por ano e UF
#   2. Embalagens          -> Cards de R$/L e variacao por embalagem + dispersao por UF + evolucao por embalagem
#   3. Estados e Regioes   -> Mapa por UF (ano e metrica a escolha) + ranking Top 10 e concentracao acumulada
# Graficos interativos com plotly (ggplot2 -> ggplotly): hover com detalhes, zoom e pan.
# Escopo dos numeros: agua embalada (5 embalagens), sem "produtos industrializados"
# (exceto quando o usuario escolhe essa categoria no filtro de embalagem). Valores nominais (sem deflacao).
# =====================================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(geobr)
library(sf)

# ---------- Constantes ----------
emb_cols_all <- c(
  "Garrafao" = "garrafao_l",
  "Garrafa Plastica" = "plastica_l",
  "Garrafa de Vidro" = "vidro_l",
  "Copo" = "copo_l",
  "Outros" = "outras_l",
  "Comp. Prod. Industrializados" = "comp_ind_l"
)

cols_l <- c("garrafao_l", "plastica_l", "vidro_l", "copo_l", "outras_l", "comp_ind_l")
cols_v <- c("garrafao_v", "plastica_v", "vidro_v", "copo_v", "outras_v", "comp_ind_v")

emb_meta <- tibble(
  rotulo = c("Garrafao", "Garrafa plastica", "Garrafa de vidro", "Copo", "Outros"),
  col_l  = c("garrafao_l", "plastica_l", "vidro_l", "copo_l", "outras_l"),
  col_v  = c("garrafao_v", "plastica_v", "vidro_v", "copo_v", "outras_v")
)

cor_valor  <- "#163a42"
cor_volume <- "#ef5350"

# ---------- Leitura e limpeza dos dados ----------
parse_br_num <- function(x) {
  as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))
}

# Prefere o arquivo local (mais rapido e funciona offline); se nao houver, le do GitHub
url_csv <- "https://raw.githubusercontent.com/emywatanabe/R_Prog/main/CE303%20-%20visualiza%C3%A7%C3%A3o/Agua_Mineral_Producao.csv"
candidatos_csv <- c(
  "Agua_Mineral_Producao.csv",
  "data/Agua_Mineral_Producao.csv",
  "CE303 - visualização/Agua_Mineral_Producao.csv"
)
csv_local <- candidatos_csv[file.exists(candidatos_csv)][1]
caminho_csv <- if (!is.na(csv_local)) csv_local else url_csv

agua_raw <- read.csv(
  caminho_csv,
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
stopifnot(ncol(agua_raw) >= 16)

# Renomeia por POSICAO (evita problemas com acentos quebrados nos cabecalhos):
#  1 = ano, 2 = UF, 5-9 = litros por embalagem, 10-14 = valor (R$) por embalagem,
#  15-16 = produtos industrializados (litros, R$)
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
    # Agua embalada = 5 embalagens (sem industrializados): litros e R$ na mesma base
    total_litros_emb = rowSums(across(c(garrafao_l, plastica_l, vidro_l, copo_l, outras_l)), na.rm = TRUE),
    valor_emb_tot    = rowSums(across(c(garrafao_v, plastica_v, vidro_v, copo_v, outras_v)), na.rm = TRUE)
  )

# Define vol_sel / val_sel para "todas" (5 embalagens) ou para uma embalagem especifica,
# usando o valor REAL de cada embalagem (nao ha rateio proporcional).
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

# ---------- Mapa ----------
mapa_br <- geobr::read_state(year = 2020, simplified = TRUE, showProgress = FALSE) |>
  sf::st_transform(4326)

coords_mapa <- sf::st_coordinates(sf::st_point_on_surface(mapa_br))
centroides_mapa <- mapa_br |>
  sf::st_drop_geometry() |>
  mutate(lon = coords_mapa[, 1], lat = coords_mapa[, 2]) |>
  select(UF = abbrev_state, lon, lat)

# Poligonos das UFs como data.frame comum (o plotly nao converte geom_sf com geometrias mistas)
mapa_poly <- local({
  m <- sf::st_cast(mapa_br, "MULTIPOLYGON")
  co <- as.data.frame(sf::st_coordinates(m))
  stopifnot(ncol(co) >= 5)
  names(co)[1:5] <- c("lon", "lat", "anel", "poligono", "feicao")
  co$UF <- m$abbrev_state[co$feicao]
  co$grupo <- paste(co$UF, co$poligono, co$anel, sep = "_")
  co[co$anel == 1, c("lon", "lat", "UF", "grupo")]   # so contornos externos (sem buracos)
})

# ---------- Formatadores, tema e helper do plotly ----------
fmt_bi_l   <- label_number(scale = 1e-9, accuracy = 0.01, decimal.mark = ",", big.mark = " ", suffix = " Bi L")
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

# Converte um ggplot em grafico interativo: o hover mostra so o texto da aesthetic `text`
para_plotly <- function(p) {
  ggplotly(p, tooltip = "text") |>
    layout(showlegend = FALSE, hoverlabel = list(align = "left")) |>
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"))
}

