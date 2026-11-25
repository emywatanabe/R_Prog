library(tidyverse)
library(scales)
library(ggplot2)
library(shiny)

agua_mineral <- read_csv("CE303 - visualização/Agua_Mineral_Producao.csv")
agua_mineral <- agua_mineral |>
  rename(`Quantidade (litros) Garrafão` = `Quantidade (litros) Garrafăo`)  #acento estranho

agua_regiao <- agua_mineral |> 
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
  ) |> 
  group_by(`Ano base`, regiao) |> 
  summarise(total_produzido = sum(total_litros, na.rm = TRUE),
            total_bi_numero = total_produzido / 1e9,
            total_bi_formatado = label_number(
              scale = 1e-9,
              accuracy = 0.01,
              decimal.mark = ",",
              big.mark = " ",
              suffix = " B"
            )(total_produzido),
            .groups = "drop"
  )


