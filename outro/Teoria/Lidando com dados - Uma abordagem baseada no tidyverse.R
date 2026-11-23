### Módulo · Lidando com dados · Uma abordagem baseada no tidyverse ------------
### Prof. Wagner Hugo Bonat ----------------------------------------------------

## Instalando o pacote tidyverse
install.packages("tidyverse")

## Carregando o pacote tidyverse
library(tidyverse)


################################################################################
## Importação de dados #########################################################
################################################################################

## Importando arquivo do tipo .txt
url <- "http://leg.ufpr.br/~wagner/scientificR/anovareg.txt"
dados <- read_tsv(url, col_names = TRUE)
head(dados)

## Importando dados do tipo .csv
url <- "http://leg.ufpr.br/~wagner/scientificR/reglinear.csv"
dados <- read_table(url, col_names = TRUE)
head(dados)

## Baixando e importando uma planilha eletrônica
# Se precisar instalar o readxl ou o httr descomente os códigos abaixo
# install.packages("readxl")
# install.packages("httr")

library(readxl)
library(httr)
url <- "http://leg.ufpr.br/~wagner/scientificR/meus_dados.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
tb <- read_excel(tf, sheet = "mtcars")
View(tb)

## Conexão com banco de dados relacionais
## O banco usado para exemplo é público e pode sair do ar.

## Se não tiver instalado os pacotes DBI e RMySQL descomente as linhas abaixo
# install.packages("DBI")
# install.packages("RMySQL")

library(DBI)
library(RMySQL)

# Criando a conexão.
db <- dbConnect(
  RMySQL::MySQL(),
  user = "rfamro", password = "",
  port = 4497, dbname = "Rfam",
  host = "mysql-rfam-public.ebi.ac.uk")

# Lista as tabelas do BD.
dbListTables(db)

# Listas as colunas em uma tabela.
dbListFields(db, "keywords")

# Importanto a tabela.
tb <- RMySQL::dbFetch(
  RMySQL::dbSendQuery(
    db, "SELECT * FROM keywords"))
str(tb)
View(tb)
# Desconecta
dbDisconnect(db)

################################################################################
## Manipulação de dados ########################################################
################################################################################

## Criando um tibble
# Tabela com alunos do curso de Matemática e de Estatística.
df1 <- tibble(
  mat = c(256, 487, 965,
          125, 458, 874, 963),
  nome = c("João", "Vanessa", "Tiago",
           "Luana", "Gisele", "Pedro",
           "André"),
  curso = c("Mat", "Mat", "Est", "Est",
            "Est", "Mat", "Est"),
  prova1 = c(80, 75, 95, 70, 45, 55, 30),
  prova2 = c(90, 75, 80, 85, 50, 75, NA),
  prova3 = c(80, 75, 75, 50, NA, 90, 30),
  faltas = c(4, 4, 0, 8, 16, 0, 20))

# Informações de cadastro dos alunos em outra base de dados.
df_extra <- tribble(
  ~mat,     ~nome, ~idade, ~bolsista,
  256,  'João'  ,     18,       "S",
  965,  'Tiago' ,     18,       "N",
  285,  'Tiago' ,     22,       "N",
  125,  'Luana' ,     21,       "S",
  874,  'Pedro' ,     19,       "N",
  321,  'Mia'   ,     18,       "N",
  669,  'Luana' ,     19,       "S",
  967,  'André' ,     20,       "N",
)

################################################################################
## Combinação de dados #########################################################
################################################################################

# Concatenação na vertical (pilha).
bind_rows(df1[1:3, c(1, 3, 5)],
          df1[5:7, c(1, 3, 5, 4)],
          df1[4,   c(1,    5, 4)])

# Concatenação na horizontal (fila).
bind_cols(df1[, c(1:3)],
          df1[, c(6:7)])

# Full join = união.
full_join(df1, df_extra,
          by = c("mat" = "mat", "nome"))

# Inner join = intersecção.
inner_join(df1,
           df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Todos os que estão na 1º tabela
left_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

# Todos os que estão na 2º tabela
right_join(df1, df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Os da 2º que não aparecem na 1º.
anti_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

################################################################################
## Exportação de dados #########################################################
################################################################################

## Texto pleno
write_csv(df1, 
          file = "Nome_do_arquivo.csv")

## Binário padrão R
save(df1, file = "Nome_do_arquivo.RData")

## Carregando arquivo .RData
load("Nome_do_arquivo.RData")

## Planilha eletrônica
library(writexl)
write_xlsx(df1, "Nome_do_arquivo.xlsx")

### FIM ########################################################################