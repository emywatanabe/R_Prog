require(tidyverse)

dados <- readr::read_csv("data/Mental Health Dataset.csv")
## Para vermos os dados, podemos utilizar a função head()
head(dados, 2)
glimpse(dados)
x <- c(-2:2)
x

# Operador Pipe %>%
# Opção 1 - Sem identação
sort(cos(unique(x)), decreasing = TRUE)

# Opção 2 - Com identação
sort(
  cos(
    unique(
      x
    )
  ), 
  decreasing = TRUE)


# Opção 3 - Utilizando o operador %>%
require(magrittr)

x %>% 
  unique() %>% 
  cos() %>%
  sort(decreasing = TRUE)

y = x %>% 
  unique() %>% 
  cos() %>%
  sort(decreasing = TRUE)
x
y

x = x %>% 
  unique() %>% 
  cos() %>%
  sort(decreasing = TRUE)
x

## Pipe de atribuição %<>%
x <- 1:10
x 
x %<>% log()
x

# Manipulação de dados com dplyr
## Criar Colunas com Mutate
dados <- dados %>% 
  mutate(mercosul = ifelse(Country %in%
                             c("Argentina", "Brazil", "Paraguay", "Uruguay"),
                           "Mercosul", "Não Mercosul"))
glimpse(dados)
table(dados$mercosul)
## Selecionar colunas com select()
dados2 <- dados %>% 
  select(Country, Timestamp, Days_Indoors, mercosul)
glimpse(dados2)

dados3 <- dados %>% 
  select(3:5)
glimpse(dados3)

dados4 <- dados %>% 
  select(treatment:Changes_Habits)
glimpse(dados4)

dados6 <- dados %>% 
  select(starts_with("t"))
glimpse(dados6)

dados7 <- dados %>% 
  select(ends_with("s"))

dados %>% 
  select(ends_with("ss")) %>% 
  glimpse()

dados8 <- dados %>% 
  select(contains("ing"))
glimpse(dados8)

dados9 <- dados %>% 
  select(matches("[tT]"))
glimpse(dados9)

dados10 <- dados %>% 
  select(-Country, -Timestamp, -Days_Indoors, -mercosul)
glimpse(dados10)

dados11 <- dados %>% 
  select(-c(Country, Timestamp, Days_Indoors, mercosul))
glimpse(dados11)

dados12 <- dados %>% 
  select_if(is.character)
glimpse(dados12)

dados %>% 
  select_if(is.logical)

dados %>% 
  select_if(is.numeric)

variaveis <- c("Country", "Timestamp", "Days_Indoors", "mercosul")
dados13 <- dados %>% 
  select(all_of(variaveis))
glimpse(dados13)

variaveis <- c("Country", "Timestamp", "Days_Indoors", "mercosul", "Loss_of_Smell")
dados14 <- dados %>% 
  select(any_of(variaveis))
glimpse(dados14)


dados15 <- dados %>% 
  filter(mercosul == "Mercosul")

table(dados15$mercosul)
# Carregando dados comprimidos (.gz)
library(data.table)
car_crash <- fread("data/Brazil Total highway crashes 2010 - 2023.csv.gz")

glimpse(car_crash)

# Filtrando linhas com filter()
car_crash2 <- car_crash %>% 
  filter(tipo_de_ocorrencia == "sem vítima")
glimpse(car_crash2)

## Filtros com múltiplas condições
car_crash3 <- car_crash %>% 
  filter(tipo_de_ocorrencia == "sem vítima" & automovel >= 3)
glimpse(car_crash3)

car_crash4 <- car_crash %>% 
  filter(between(automovel, 3, 5))

tipos <- c("sem vítima", "com vítima")
tipos
car_crash5 <- car_crash %>% 
  filter(tipo_de_ocorrencia %in% tipos)
car_crash5

car_crash6 <- car_crash %>% 
  filter(!tipo_de_ocorrencia %in% c("sem vítima", "com vítima"))
glimpse(car_crash6)

## Usando o operador %ni%
`%ni%` <- Negate(`%in%`)
car_crash7 <- car_crash %>%
  filter(tipo_de_ocorrencia %ni% c("sem vítima", "com vítima"))
glimpse(car_crash7)
## Operador like %like%
car_crash8 <- car_crash %>% 
  filter(tipo_de_ocorrencia %like% "vítima")

glimpse(car_crash8)

car_crash9 =  car_crash %>% 
  filter(grepl("ilesa|fatal", tipo_de_ocorrencia))

glimpse(car_crash9)

## Ordenando linhas com arrange()
car_crash10 = car_crash %>% 
  arrange(desc(automovel))
glimpse(car_crash10)
car_crash11 = car_crash %>% 
  arrange(desc(automovel), mortos) %>%
  select(automovel, mortos) %>% 
  na.exclude()
head(car_crash11)

car_crash_slice1 = car_crash %>% 
  select(1:5) %>%
  slice(3:5)
car_crash_slice1

car_crash_slice2 = car_crash %>% 
  select(1:5) %>%
  slice_head(n = 3)
car_crash_slice2


car_crash_slice3 = car_crash %>% 
  select(1:5) %>%
  slice_tail(n = 3)
car_crash_slice3

## Exercícios
# 1.
storms %>% 
  filter(status == "tropical depression") %>% 
  nrow()
# 2.
storms %>%
  filter(status == "tropical depression" & wind >= 40) %>%
  nrow()
# 3.
storms %>%
  select_if(is.numeric) %>%
  arrange(pressure)

## Slices
car_crash_slice1 = car_crash %>% 
  select(1:5) %>%
  slice(3:5)
car_crash_slice1

car_crash_slice2 = car_crash %>% 
  select(1:5) %>%
  slice_head(n = 3)
car_crash_slice2

car_crash_slice3 = car_crash %>% 
  select(1:5) %>%
  slice_tail(n = 3)
car_crash_slice3

# Rename
car_carsh12 = car_crash %>% 
  rename(numero_automoveis = automovel)
glimpse(car_carsh12)

# Relocate
car_crash_relocate = car_crash %>% 
  relocate(automovel, .before = 1)
glimpse(car_crash_relocate)

car_crash_relocate2 = car_crash %>% 
  relocate(mortos, .after = last_col())
glimpse(car_crash_relocate2)

# Transmute
car_crash_transmute <- car_crash %>% 
  transmute(automovel_10 = automovel / 10)
glimpse(car_crash_transmute)

# NA Replace
car_crash_replace_na <- car_crash %>%
  mutate(mortos = replace_na(mortos, 0))
glimpse(car_crash_replace_na)

# Cut
car_crash_cut <- car_crash %>%
  mutate(automovel = replace_na(automovel, 0)) %>%
  mutate(automovel_cat = cut(automovel,
                             breaks = c(-Inf, 0, 3, Inf),
                             labels = c("sem automóveis",
                                        "entre 1 e 3 automóveis",
                                        "mais do que três")))
glimpse(car_crash_cut)
table( car_crash_cut$automovel, 
       car_crash_cut$automovel_cat)

## Summarise
car_crash13 = car_crash %>% 
  summarise(total_automoveis = sum(automovel, na.rm = TRUE))
car_crash13
sum(car_crash$automovel, na.rm = T)

car_crash14 = car_crash %>% 
  summarise(total_automoveis = sum(automovel, na.rm = TRUE),
            total_mortos = sum(mortos, na.rm = TRUE), 
            n = n(), 
            media_mortos = mean(mortos, na.rm = TRUE))
car_crash14
sum(car_crash$mortos, na.rm = T)
nrow(car_crash)
mean(car_crash$mortos, na.rm = T)

require(lubridate)
## Agrupamento com group_by()
car_crash15 = car_crash %>% 
  mutate(ano = year(dmy(data))) %>%
  group_by(ano)
glimpse(car_crash15)

car_crash16 = car_crash %>% 
  mutate(ano = year(dmy(data))) %>%
  group_by(ano) %>%
  summarise(total_automoveis = sum(automovel, na.rm = TRUE),
            total_mortos = sum(mortos, na.rm = TRUE))
head(car_crash16)

car_crash17 = car_crash %>% 
  filter(tipo_de_ocorrencia == "com vítima") %>%
  summarise(total_automoveis = sum(automovel, na.rm = TRUE),
            total_mortos = sum(mortos, na.rm = TRUE))
car_crash17

## Exercícios 
# 1.
starwars %>%
  summarise(n_especies = n_distinct(species))

# 2.
starwars %>%
  group_by(species) %>%
  summarise(freq_especies = n()) %>%
  arrange(desc(freq_especies))

# 3.
starwars %>% 
  filter(sex %in% c("female", "male")) %>% 
  group_by(sex) %>%
  summarise(media_altura = mean(height, na.rm = TRUE))

# 4.
starwars %>% 
  filter(sex == "male") %>%
  group_by(species) %>%
  summarise(media_peso = mean(mass, na.rm = TRUE))
# 5.
starwars %>% 
  group_by(species) %>%
  filter(mass == max(mass, na.rm = TRUE)) %>%
  select(species, name, mass)


## Trabalhando com datas - lubridate
car_crash %>% 
  mutate(data = dmy(data)) %>%
  select(data) %>%
  glimpse()

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(ano = year(data),
         mes = month(data),
         dia = day(data)) %>%
  select(data, ano, mes, dia) %>%
  glimpse()

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(dias_desde_acidente = difftime(Sys.Date(), data, units = "days")) %>%
  select(data, dias_desde_acidente) %>%
  head()

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(data_mais_10_dias = data + lubridate::days(10)) %>%
  select(data, data_mais_10_dias) %>%
  head()

## Extraindo componentes de data e hora
data <- ymd_hms("2023-08-21 15:30:45")
ano <- year(data)
mes <- month(data)
dia <- day(data)
hora <- hour(data)
minuto <- minute(data)
segundo <- second(data)

print(ano)
print(mes)
print(dia)
print(hora)
print(minuto)
print(segundo)

# Data original no fuso horário de Nova Iorque
data_ny <- ymd_hms("2025-10-21 12:00:00", tz = "America/New_York")

# Converter para o fuso horário de Londres
data_london <- with_tz(data_ny, tz = "Europe/London")

print(data_ny)
print(data_london)

## Exercícios com datas
car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(ano = year(data),
         mes = month(data)) %>%
  select(data, ano, mes, mortos) %>%
  filter(mortos > 0) %>% 
  group_by(mes) %>%
  summarise(total_mortos = sum(mortos)) %>% 
  arrange(desc(total_mortos))

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(dia_semana = lubridate::wday(data, label = T, abbr = F)) %>%
  select(dia_semana, mortos) %>%
  filter(mortos > 0) %>% 
  group_by(dia_semana) %>%
  summarise(total_mortos_dia = sum(mortos)) %>% 
  arrange(desc(total_mortos_dia))


# Tidy data

# Carregar pacotes necessários
library(tidyverse)

# Visualizar o dataset table1
table1

# Transformar dados long -> wide usando pivot_wider()
table1 %>% 
  select(-population) %>%
  pivot_wider(names_from = year, 
              values_from = cases)

# Pivotar com mais de uma variável
table1 %>% 
  pivot_wider(names_from = year, 
              values_from = c(cases, population))

# Transformar dados wide -> long usando pivot_longer()
table1 %>% 
  pivot_longer(cols = c(cases, population), 
               names_to = "variable", 
               values_to = "total")


# Separando e juntando observações


# Visualizar o dataset table3
table3

# Separar coluna 'rate' em 'cases' e 'population'
table3 %>% 
  separate(rate, into = c("cases", "population"))

# Juntar colunas 'cases' e 'population' em uma única coluna 'rate'
table1 %>% 
  unite(rate, cases, population, sep = "/")


# Exercício: número de voos entre aeroportos


# Utilizando os dados do pacote nycflights13
require(nycflights13)
flights %>% 
  count(origin, dest) %>% 
  pivot_wider(names_from = origin, 
              values_from = n, 
              values_fill = 0)


# Manipulação de Strings em R

library(stringr)
texto <- "Olá, Mundo!"

# Comprimento da string
str_length(texto)

# Converter para minúsculas
str_to_lower(texto)

# Converter para maiúsculas
str_to_upper(texto)

str_to_title(texto)

str_to_sentence(texto)

# Extrair substring
str_sub(texto, 1, 3)

# Substituir parte da string
str_replace(texto, "Mundo", "R")

# Verificar se a string contém um padrão
str_detect(texto, "Mundo")
str_detect(texto, "R!")

# Expressões Regulares (Regex)

# Corresponder qualquer caractere
str_detect("abc", "a.c") # TRUE

# Início da string
str_detect("abc", "^a")   # TRUE

# Fim da string
str_detect("abc", "c$")   # TRUE

# Zero ou mais ocorrências
str_detect("aaab", "a*b") # TRUE

# Uma ou mais ocorrências
str_detect("aaab", "a+b") # TRUE

# Conjunto de caracteres: corresponde a 'a', 'b' ou 'c'
str_detect("abc", "[abc]") # TRUE

# Operador "ou": corresponde a 'cat' ou 'dog'
str_detect("I have a cat", "cat|dog") # TRUE
