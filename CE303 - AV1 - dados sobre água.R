#CE303 - AV1 - dados sobre água

# Libraries
library(dplyr)
library(ggplot2)


dados_agua <- read.csv("~/Documentos/Disciplinas/2026-2/Visualização/Agua_Mineral_Producao.csv", 
                  fileEncoding = "latin1",
                  check.names = FALSE)

# ==========================================
# 1. PREPARAÇÃO DOS DADOS (Agrupamento)
# ==========================================
dados_serie <- dados_agua %>%
  mutate(
    regiao = case_when(
      UF %in% c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO') ~ 'Norte',
      UF %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE') ~ 'Nordeste',
      UF %in% c('DF', 'GO', 'MT', 'MS') ~ 'Centro-Oeste',
      UF %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste',
      UF %in% c('PR', 'RS', 'SC') ~ 'Sul',
      ),
    
    total_litros = (as.numeric(gsub(",", ".", `Quantidade (litros) Garrafão`)) + 
      as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa Plástica`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa de Vidro`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Copo`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Outras Embalagens`)))
  ) %>%
  group_by(`Ano base`, regiao) %>%
  summarise(total_produzido = sum(total_litros, na.rm = TRUE), .groups = 'drop')


# 1. Calcular os dados empilhados por região e o total acumulado por ano
dados_grafico <- dados_serie %>%
  group_by(`Ano base`) %>%
  mutate(
    total_acumulado = sum(total_produzido, na.rm = TRUE),
    # Calcula a posição vertical para centralizar o texto na barra ou colocar no topo
    posicao_label = cumsum(total_produzido) - (total_produzido / 2)
  )
# 2. Criar tabela auxiliar apenas com o topo de cada barra para o rótulo total
dados_totais <- dados_grafico %>%
  summarise(total_geral = max(total_acumulado, na.rm = TRUE), .groups = 'drop')


# ==========================================
# 2. GRÁFICO 1 - BARRAS EMPILHADAS
# ==========================================
ggplot(dados_serie, aes(x = as.factor(`Ano base`), y = total_produzido, fill = regiao)) +
  geom_col(position = "stack") +
  # Adiciona o rótulo com o valor total acumulado no topo de cada barra
  geom_text(
    data = dados_totais,
    aes(x = as.factor(`Ano base`), y = total_geral, 
        label = scales::label_number(scale_cut = scales::cut_short_scale(),accuracy = 0.1)(total_geral)),
    inherit.aes = FALSE,
    vjust = -0.5,           # Posiciona logo acima da barra
    size = 3.5,             # Tamanho da fonte do rótulo
    fontface = "bold"
  ) +
  labs(
    #title = "Produção de Água Mineral por Região (2010-2025)",
    #subtitle = "Comparação anual e acumulada",
    x = "Ano Base",
    y = "Quantidade Total (Litros)",
    fill = "Região"
  ) +
  theme_classic() +
  scale_fill_manual(values = c(
    "Centro-Oeste" = "#C6DBEF", 
    "Nordeste"     = "#6BAED6", 
    "Norte"        = "#2171B5", 
    "Sudeste"      = "#08306B", 
    "Sul"          = "#03102E"
  )) +
  scale_y_continuous(
      expand = expansion(mult = c(0, 0.12)),
      labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 12),
    legend.position = c(0.12, 0.72),
    legend.background = element_rect(fill = alpha("white", 0.6)),
    legend.text = element_text(size = 12)
    #plot.subtitle = element_text(size = 12)
  )


# ==========================================
# GRÁFICO 2: VALOR ($) POR ANO (2 GRAFICOS)
# ==========================================

library(dplyr)
library(ggplot2)

# ==========================================
# 1. PREPARAÇÃO DOS DADOS DE FATURAMENTO POR REGIÃO
# ==========================================
dados_faturamento_regiao <- dados_agua %>%
  mutate(
    regiao = case_when(
      UF %in% c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO') ~ 'Norte',
      UF %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE') ~ 'Nordeste',
      UF %in% c('DF', 'GO', 'MT', 'MS') ~ 'Centro-Oeste',
      UF %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste',
      UF %in% c('PR', 'RS', 'SC') ~ 'Sul',
      TRUE ~ 'Outros'
    ),
    
    # Converte e soma todos os valores em R$ de todas as embalagens
    total_valor = as.numeric(gsub(",", ".", `Valor (R$) Garrafão`)) + 
      as.numeric(gsub(",", ".", `Valor (R$) Garrafa Plástica`)) +
      as.numeric(gsub(",", ".", `Valor (R$) Garrafa de Vidro`)) +
      as.numeric(gsub(",", ".", `Valor (R$) Copo`)) +
      as.numeric(gsub(",", ".", `Valor (R$) Outras Embalagens`))
  ) %>%
  group_by(`Ano base`, regiao) %>%
  summarise(faturamento_total = sum(total_valor, na.rm = TRUE), .groups = 'drop')

# 2. Tabela auxiliar com o total acumulado do faturamento por ano (para os rótulos do topo)
dados_totais_fat <- dados_faturamento_regiao %>%
  group_by(`Ano base`) %>%
  summarise(total_geral = sum(faturamento_total, na.rm = TRUE), .groups = 'drop')


# ==========================================
# 3. GRÁFICO DE BARRAS EMPILHADAS DE FATURAMENTO
# ==========================================
ggplot(dados_faturamento_regiao, aes(x = as.factor(`Ano base`), y = faturamento_total, fill = regiao)) +
  geom_col(position = "stack") +
  
  # Adiciona o rótulo com o valor total acumulado no topo de cada barra
  geom_text(
    data = dados_totais_fat,
    aes(x = as.factor(`Ano base`), y = total_geral, 
        label = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)(total_geral)),
    inherit.aes = FALSE,
    vjust = -0.5,            # Posiciona logo acima da barra
    size = 3.5,              # Tamanho da fonte do rótulo
    fontface = "bold"
  ) +
  
  labs(
    #title = "Faturamento Total de Água Mineral por Região (2010-2025)",
    #subtitle = "Comparação anual e acumulada (R$)",
    x = "Ano Base",
    y = "Faturamento Total (R$)",
    fill = "Região"
  ) +
  theme_classic() +
  scale_fill_manual(values = c(
    "Centro-Oeste" = "#C6DBEF", 
    "Nordeste"     = "#6BAED6", 
    "Norte"        = "#2171B5", 
    "Sudeste"      = "#08306B", 
    "Sul"          = "#03102E"
  )) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1),
    expand = expansion(mult = c(0, 0.1)) # Dá uma folga em cima para os rótulos não cortarem
  ) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = c(0.12, 0.72),
    legend.background = element_rect(fill = alpha("white", 0.6)),
    legend.text = element_text(size = 12)
  )


#=================================
#DADOS DO BRASIL
#=================================

dados_serie <- dados_agua %>%
  mutate (pais = case_when(
    UF %in% c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO', 'AL', 'BA', 'CE', 'MA',
              'PB', 'PE', 'PI', 'RN', 'SE', 'DF', 'GO', 'MT', 'MS', 'ES', 'MG', 
              'RJ', 'SP', 'PR', 'RS', 'SC') ~ 'Brasil'
),

total_litros = as.numeric(gsub(",", ".", `Quantidade (litros) Garrafão`)) + 
  as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa Plástica`)) +
  as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa de Vidro`)) +
  as.numeric(gsub(",", ".", `Quantidade (litros) Copo`)) +
  as.numeric(gsub(",", ".", `Quantidade (litros) Outras Embalagens`))
  ) %>%
  group_by(`Ano base`, pais) %>%
  summarise(total_produzido = sum(total_litros, na.rm = TRUE), .groups = 'drop')

#plot serie

ggplot(dados_serie, aes(x=`Ano base`, y=total_produzido, color = pais)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  labs(
    title = "Série temporal da produção de Água Mineral",
    x = "Ano Base",
    y = "Quantidade Total / litros",
    color = "Região"
  ) +
  theme_minimal(
    base_size = 12, 
    
  ) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))


#=================================
#grafico de rosa  - valor de embalagem 
#=================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ==========================================
# 1. PREPARAÇÃO DOS DADOS: CONSOLIDAÇÃO POR EMBALAGEM
# ==========================================
dados_rose_embalagem <- dados_agua %>%
  # Converte as colunas de Valor para numérico de forma segura
  mutate(across(starts_with("Valor"), ~ as.numeric(gsub(",", ".", .x)))) %>%
  # Soma o total de R$ para cada tipo de embalagem
  summarise(
    Garrafão = sum(`Valor (R$) Garrafão`, na.rm = TRUE),
    `Garrafa Plástica` = sum(`Valor (R$) Garrafa Plástica`, na.rm = TRUE),
    `Garrafa de Vidro` = sum(`Valor (R$) Garrafa de Vidro`, na.rm = TRUE),
    Copo = sum(`Valor (R$) Copo`, na.rm = TRUE),
    `Outras Embalagens` = sum(`Valor (R$) Outras Embalagens`, na.rm = TRUE)
  ) %>%
  # Transforma para o formato longo (uma coluna para Categoria, uma para Valor)
  pivot_longer(everything(), names_to = "Embalagem", values_to = "Faturamento_Total")

# ==========================================
# 2. PLOTAR O ROSE CHART (Gráfico de Nightingale)
# ==========================================

ggplot(dados_rose_embalagem, aes(x = Embalagem, y = Faturamento_Total, fill = Embalagem)) +
  # geom_col com width = 1 para garantir fatias sem espaçamento nas bordas
  geom_col(width = 1, color = "white", alpha = 0.85, show.legend = FALSE) +
  # A conversão para coordenadas polares cria o efeito de roseta
  coord_polar(start = 0, clip = "off") +
  labs(
    title = "Distribuição do Faturamento de Água Mineral por Embalagem",
    subtitle = "Valor Total Acumulado (R$) por Tipo de Recipiente",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Garrafão"          = "#cfd8dc", 
    "Garrafa Plástica"  = "#90a4ae", 
    "Garrafa de Vidro"  = "#607d8b", 
    "Copo"              = "#455a64", 
    "Outras Embalagens" = "#263238"
  )) +
  # Formata os rótulos radiais para exibirem valores monetários (ex: R$ 1,5B)
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale(), prefix = "R$ ", accuracy = 0.1),
    expand = expansion(mult = c(0.05, 0.1)) # Dá respiro no centro e nas bordas
  ) +
  theme(
    # Remove o grid secundário para poluir menos o gráfico
    panel.grid = element_line(color = "white"),
    # Remove as marcas e números do eixo y (que fica implícito pelo tamanho das fatias)
    axis.text.y = element_blank(),
    # Estiliza os nomes das embalagens ao redor do círculo
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    # Títulos do gráfico
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    # Margens para garantir que os textos não sejam cortados nas bordas
    plot.margin = unit(c(1.5, 2, 1.5, 2), "cm")
  )


#===================================
# TERCEIRO GRAFICO

#===================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

library(tidyverse)

# ==========================================
# 1. PREPARAÇÃO DOS DADOS POR TIPO DE EMBALAGEM
# ==========================================
# Transforma as colunas de quantidade e valor de cada embalagem em linhas
dados_volume_emb <- dados_agua %>%
  mutate(across(starts_with("Quantidade"), ~ as.numeric(gsub(",", ".", .x)))) %>%
  group_by(`Ano base`) %>%
  summarise(
    Garrafão           = sum(`Quantidade (litros) Garrafão`, na.rm = TRUE),
    `Garrafa Plástica` = sum(`Quantidade (litros) Garrafa Plástica`, na.rm = TRUE),
    `Garrafa de Vidro` = sum(`Quantidade (litros) Garrafa de Vidro`, na.rm = TRUE),
    Copo               = sum(`Quantidade (litros) Copo`, na.rm = TRUE),
    `Outras Embalagens` = sum(`Quantidade (litros) Outras Embalagens`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(-`Ano base`, names_to = "Embalagem", values_to = "Volume_Litros")

dados_valor_emb <- dados_agua %>%
  mutate(across(starts_with("Valor"), ~ as.numeric(gsub(",", ".", .x)))) %>%
  group_by(`Ano base`) %>%
  summarise(
    Garrafão           = sum(`Valor (R$) Garrafão`, na.rm = TRUE),
    `Garrafa Plástica` = sum(`Valor (R$) Garrafa Plástica`, na.rm = TRUE),
    `Garrafa de Vidro` = sum(`Valor (R$) Garrafa de Vidro`, na.rm = TRUE),
    Copo               = sum(`Valor (R$) Copo`, na.rm = TRUE),
    `Outras Embalagens` = sum(`Valor (R$) Outras Embalagens`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(-`Ano base`, names_to = "Embalagem", values_to = "Faturamento_Reais")

# Junta o volume e o faturamento por ano e embalagem
dados_bubble_embalagem <- inner_join(dados_volume_emb, dados_valor_emb, by = c("Ano base", "Embalagem"))

# ==========================================
# 2. PLOTAR O BUBBLE PLOT POR EMBALAGEM
# ==========================================
ggplot(dados_bubble_embalagem, aes(x = Volume_Litros, y = Faturamento_Reais, size = Faturamento_Reais, color = Embalagem)) +
  # geom_point com transparência para melhor visualização
  geom_point(alpha = 0.75) +
  
  # Controla o tamanho das bolhas
  scale_size_area(max_size = 10, guide = "none") +
  
  # Correção: Alterado de scale_fill_manual para scale_color_manual
  scale_color_manual(values = c(
    "Garrafão"          = "#C6DBEF", 
    "Garrafa Plástica"  = "#6BAED6", 
    "Garrafa de Vidro"  = "#2171B5", 
    "Copo"              = "#08306B", 
    "Outras Embalagens" = "#03102E"
  )) +
  
  labs(
    title = "Volume vs. Faturamento",
    x = "Volume Total Produzido (Litros)",
    y = "Faturamento Total (R$)",
    color = "Embalagem"
  ) +
  theme_bw() +
  
  # Formatação dos eixos com abreviações e 1 casa decimal
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.1)),
    labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1, prefix = "R$ ")) +
  
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    legend.position = c(0.82, 0.8),
    legend.background = element_rect(fill = alpha("white", 0)),
    legend.text = element_text(size = 12)
  )

#==================
#ÚLTIMO GRAFICO
#==================
library(dplyr)
library(ggplot2)
library(scales)

# ==========================================
# 1. PREPARAÇÃO DOS DADOS (Volume Total por UF)
# ==========================================
dados_rose <- dados_agua %>%
  mutate(
    total_litros = as.numeric(gsub(",", ".", `Quantidade (litros) Garrafão`)) + 
      as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa Plástica`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa de Vidro`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Copo`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Outras Embalagens`))
  ) %>%
  group_by(UF) %>%
  summarise(Volume_Total = sum(total_litros, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(Volume_Total) & Volume_Total > 0) %>%
  slice_max(order_by = Volume_Total, n = 10)

# ==========================================
# 2. DEFINIÇÃO DA PALETA DE AZUIS
# ==========================================
paleta_azuis <- c(
  "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", 
  "#2171B5", "#08519C", "#08306B", "#05204A", "#03102E"
)

# ==========================================
# 3. PLOTAR O NIGHTINGALE ROSE CHART COM A PALETA em azul
# ==========================================
ggplot(dados_rose, aes(x = reorder(UF, Volume_Total), y = Volume_Total, fill = UF)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.5) +
  coord_polar(start = 0) +
  # Aplicação da paleta de cores personalizada em tons de azul
  scale_fill_manual(values = paleta_azuis) +
  labs(
    title = "Top 10: Produção de Água Mineral por estado",
    x = NULL,
    y = NULL,
    fill = "UF"
  ) +
  theme_light(base_size = 13) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  theme(
    axis.text.x = element_text(face = "bold", size = 18, color = "black"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none"
  )


#===============================
#grafico de bolhas
#==============================

library(tidyverse)
library(packcircles)

# 1. Carregando os dados da tabela
dados <- data.frame(
  Embalagem = c("Garrafão", "Garrafa Plástica", "Garrafa de Vidro", "Copo", "Outras emb.", "Composição Ind."),
  Valor_Agregado_Pct = c(25.11, 62.93, 0.77, 2.41, 0.77, 8.00),
  Volume_Litros_Pct = c(57.30, 20.21, 0.12, 0.70, 0.44, 21.23)
)

# 2. Cálculo do layout progressivo rigorosamente proporcional ao volume real
circle.layout <- circleProgressiveLayout(dados$Volume_Litros_Pct, sizetype = "area")

# Une os dados calculados ao dataframe original
dados_plot <- bind_cols(dados, circle.layout)

# Cria os vértices para desenhar as bolhas
circle.vertices <- circleLayoutVertices(circle.layout, npoints = 50)
circle.vertices$Embalagem <- dados_plot$Embalagem[circle.vertices$id]

# 3. Separar categorias grandes (texto dentro) e pequenas (texto na legenda)
# Definimos um limiar de corte (ex: volume menor que 2% fica sem texto interno)
dados_plot <- dados_plot %>%
  mutate(Texto_Interno = ifelse(Volume_Litros_Pct >= 2.0, paste0(Embalagem, "\n", Volume_Litros_Pct, "%"), ""))

# 4. Construção do Gráfico com ggplot2
ggplot() + 
  # Desenha os círculos preenchidos e com transparência
  geom_polygon(data = circle.vertices,
               aes(x = x, y = y, group = id, fill = Embalagem),
               colour = "white",
               linewidth = 0.8,
               alpha = 0.85) +
  
  # Adiciona texto apenas nas bolhas grandes onde há espaço adequado
  geom_text(data = filter(dados_plot, Volume_Litros_Pct >= 2.0),
            aes(x = x, y = y, label = Texto_Interno),
            fontface = "bold", size = 6, color = "#222222", lineheight = 0.9) +
  
  # Paleta de cores personalizada
  scale_fill_manual(values = c(
    "Garrafão"           = "#C6DBEF", 
    "Garrafa Plástica"   = "#6BAED6", 
    "Garrafa de Vidro"   = "#2171B5", 
    "Copo"               = "#08306B", 
    "Outras emb."        = "#4292C6",
    "Composição Ind."    = "#08519C"
  )) +
  
  # Mantém a proporção geométrica correta dos círculos
  coord_equal() +
  
  # Remove eixos e limpa o fundo
  theme_void() +
  
  # Configurações de título e legenda informativa
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15))
  ) +
  labs(
    title = "Distribuição de Volume (%) por Embalagem/Uso",
    # subtitle = "Categorias menores identificadas via legenda lateral de alta precisão",
    fill = "Embalagem"
  )

#====================

#=========================

library(tidyverse)
library(packcircles)

# 1. Carregando os dados da tabela
dados <- data.frame(
  Embalagem = c("Garrafão", "Garrafa Plástica", "Garrafa de Vidro", "Copo", "Outras emb.", "Composição Ind."),
  Valor_Agregado_Pct = c(25.11, 62.93, 0.77, 2.41, 0.77, 8.00),
  Volume_Litros_Pct = c(57.30, 20.21, 0.12, 0.70, 0.44, 21.23)
)

# 2. Cálculo do layout progressivo rigorosamente proporcional ao volume real
circle.layout <- circleProgressiveLayout(dados$Valor_Agregado_Pct, sizetype = "area")

# Une os dados calculados ao dataframe original
dados_plot <- bind_cols(dados, circle.layout)

# Cria os vértices para desenhar as bolhas
circle.vertices <- circleLayoutVertices(circle.layout, npoints = 50)
circle.vertices$Embalagem <- dados_plot$Embalagem[circle.vertices$id]

# 3. Separar categorias grandes (texto dentro) e pequenas (texto na legenda)
# Definimos um limiar de corte (ex: volume menor que 2% fica sem texto interno)
dados_plot <- dados_plot %>%
  mutate(Texto_Interno = ifelse(Valor_Agregado_Pct >= 2.0, paste0(Embalagem, "\n", Valor_Agregado_Pct, "%"), ""))

# 4. Construção do Gráfico com ggplot2
ggplot() + 
  # Desenha os círculos preenchidos e com transparência
  geom_polygon(data = circle.vertices,
               aes(x = x, y = y, group = id, fill = Embalagem),
               colour = "white",
               linewidth = 0.8,
               alpha = 0.85) +
  
  # Adiciona texto apenas nas bolhas grandes onde há espaço adequado
  geom_text(data = filter(dados_plot, Valor_Agregado_Pct >= 2.0),
            aes(x = x, y = y, label = Texto_Interno),
            fontface = "bold", size = 6, color = "#222222", lineheight = 0.9) +
  
  # Paleta de cores personalizada
  scale_fill_manual(values = c(
    "Garrafão"           = "#C6DBEF", 
    "Garrafa Plástica"   = "#6BAED6", 
    "Garrafa de Vidro"   = "#2171B5", 
    "Copo"               = "#08306B", 
    "Outras emb."        = "#4292C6",
    "Composição Ind."    = "#08519C"
  )) +
  
  # Mantém a proporção geométrica correta dos círculos
  coord_equal() +
  
  # Remove eixos e limpa o fundo
  theme_void() +
  
  # Configurações de título e legenda informativa
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15))
  ) +
  labs(
    title = "Distribuição de Valor (%) por Embalagem/Uso",
    # subtitle = "Categorias menores identificadas via legenda lateral de alta precisão",
    fill = "Embalagem"
  )

#========================================================
#Ranking das UF
#========================================================

library(tidyverse)

# 1. Carregando os dados da imagem
dados_top10 <- data.frame(
  UF = c("SP", "PE", "BA", "RS", "PA", "CE", "RJ", "MG", "MT", "RN"),
  Volume = c(40289212788.89, 25940186231.32, 25350095651.24, 13566071808.89, 
             11941393989.24, 9538020367.54, 9425755343.21, 8842869480.27, 
             6868405983.03, 6862106822.61)
)

# 2. Construção do Gráfico de Barras Horizontais Limpo
ggplot(dados_top10, aes(x = reorder(UF, Volume), y = Volume)) +
  
  geom_col(fill = "#DEEBF7") +
  
  # Inverte os eixos para o formato horizontal e ordena corretamente
  coord_flip() +
  
  geom_text(aes(label = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)(Volume)),
            hjust = -0.15, size = 5, fontface = "bold", color = "#333333") +
  geom_te

scale_y_continuous(
  expand = expansion(mult = c(0, 0.15)),
  labels = scales::label_number(scale_cut = scales::cut_short_scale())
) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(), # Remove linhas horizontais de fundo
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E5E5", linetype = "dashed"),
    axis.title.y = element_blank(),     
    axis.text.y = element_text(face = "bold", size = 12, color = "#222222"),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 15)),
    
    labs(
      title = "Top 10 Estados: Produção de Água Mineral",
      x = NULL,
      y = "Volume Total (Litros)"
    ))

