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
    
    total_litros = as.numeric(gsub(",", ".", `Quantidade (litros) Garrafão`)) + 
      as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa Plástica`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Garrafa de Vidro`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Copo`)) +
      as.numeric(gsub(",", ".", `Quantidade (litros) Outras Embalagens`))
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
    title = "Volume de Produção de Água Mineral por Região",
    subtitle = "Comparação anual e acumulada",
    x = "Ano Base",
    y = "Quantidade Total (Litros)",
    fill = "Região"
  ) +
  theme_classic() +
  scale_fill_manual(values = c(
    "Centro-Oeste" = "#cfd8dc", 
    "Nordeste"     = "#90a4ae", 
    "Norte"        = "#607d8b", 
    "Sudeste"      = "#455a64", 
    "Sul"          = "#263238"
  )) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
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
    title = "Faturamento Total de Água Mineral por Região",
    subtitle = "Comparação anual e acumulada (R$)",
    x = "Ano Base",
    y = "Faturamento Total (R$)",
    fill = "Região"
  ) +
  theme_classic() +
  scale_fill_manual(values = c(
    "Centro-Oeste" = "#cfd8dc", 
    "Nordeste"     = "#90a4ae", 
    "Norte"        = "#607d8b", 
    "Sudeste"      = "#455a64", 
    "Sul"          = "#263238"
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
    plot.subtitle = element_text(size = 12)
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
    panel.grid = element_line(color = "gray90"),
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

# ==========================================
# 1. PREPARAÇÃO DOS DADOS POR TIPO DE EMBALAGEM
# ==========================================
# Transforma as colunas de quantidade e valor de cada embalagem em linhas
dados_volume_emb <- dados_agua %>%
  mutate(across(starts_with("Quantidade"), ~ as.numeric(gsub(",", ".", .x)))) %>%
  group_by(`Ano base`) %>%
  summarise(
    Garrafão          = sum(`Quantidade (litros) Garrafão`, na.rm = TRUE),
    `Garrafa Plástica` = sum(`Quantidade (litros) Garrafa Plástica`, na.rm = TRUE),
    `Garrafa de Vidro` = sum(`Quantidade (litros) Garrafa de Vidro`, na.rm = TRUE),
    Copo              = sum(`Quantidade (litros) Copo`, na.rm = TRUE),
    `Outras Embalagens` = sum(`Quantidade (litros) Outras Embalagens`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(-`Ano base`, names_to = "Embalagem", values_to = "Volume_Litros")

dados_valor_emb <- dados_agua %>%
  mutate(across(starts_with("Valor"), ~ as.numeric(gsub(",", ".", .x)))) %>%
  group_by(`Ano base`) %>%
  summarise(
    Garrafão          = sum(`Valor (R$) Garrafão`, na.rm = TRUE),
    `Garrafa Plástica` = sum(`Valor (R$) Garrafa Plástica`, na.rm = TRUE),
    `Garrafa de Vidro` = sum(`Valor (R$) Garrafa de Vidro`, na.rm = TRUE),
    Copo              = sum(`Valor (R$) Copo`, na.rm = TRUE),
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
  scale_size_area(max_size = 12, guide = "none") +
  
  labs(
    title = "Bubble Plot: Volume vs. Faturamento por Tipo de Embalagem",
    subtitle = "Comparação entre os recipientes de uso (Garrafão, Plástica, Vidro, Copo e Outros)",
    x = "Volume Total Produzido (Litros)",
    y = "Faturamento Total (R$)",
    color = "Embalagem"
  ) +
  theme_classic() +
  
  # Formatação dos eixos com abreviações e 1 casa decimal
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1, prefix = "R$ ")) +
  
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

#==================
#ÚLTIMO GRAFICO
#==================
