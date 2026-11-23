library(tidytuesdayR)
library(tidyverse)

# Dataset de Doctor Who
directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-11-23/imdb.csv')


# O gabarito conta apenas EPISÓDIOS (type == "episode"), não especiais
episodios <- episodes |>
  filter(type == "episode")

#A dupla de diretor e escritor formada por Hettie MacDonald e Steven Moffat é responsável por quantos episódios? 
dupla <- episodios |>
  inner_join(writers,   by = "story_number") |>
  inner_join(directors, by = "story_number") |>
  filter(writer == "Steven Moffat", director == "Hettie MacDonald")

nrow(dupla)                     

# ---- 2. Steven Moffat em 2011: quantos episódios? ----
moffat_2011 <- episodios |>
  inner_join(writers, by = "story_number") |>
  filter(writer == "Steven Moffat", year(first_aired) == 2011)

nrow(moffat_2011)               

# ---- 3. Toby Whithouse : quantos anos trabalhou na série? ----
whithouse <- episodes |>
  filter(type == "episode") |>
  inner_join(writers, by = "story_number") |>
  filter(writer == "Toby Whithouse") |>
  mutate(ano = lubridate::year(first_aired))

nrow(whithouse)                       # esperado: 7 (também é a resposta da pergunta 4)
max(whithouse$ano) - min(whithouse$ano) 

# ---- 4. Toby Whithouse: quantos roteiros? ----
nrow(whithouse)                 

# ---- 5. Hettie MacDonald: duração média dos episódios (minutos) ----
duracao_hettie <- episodios |>
  inner_join(directors, by = "story_number") |>
  filter(director == "Hettie MacDonald") |>
  summarise(media_duracao = round(mean(duration, na.rm = TRUE), 2))

duracao_hettie                    
