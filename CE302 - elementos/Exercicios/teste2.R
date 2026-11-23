library(tidyverse)

directors <- read_csv("arquivo_csv/directors.csv")
episodes <- read_csv("arquivo_csv/episodes.csv")
imdb <- read_csv("arquivo_csv/imdb.csv")
writers <- read_csv("arquivo_csv/writers.csv")

#quais são as colunas?
names(directors)
names(episodes)
names(imdb)
names(writers)
#em comum seria story_number (excluindo imdb, que não tem)
#juntar com a tabela episodes (o que tem mais) com writers and directors
dw <- episodes |> 
  filter(type == "episode") |> 
  inner_join(writers, by = "story_number") |> 
  inner_join(directors, by = "story_number")

#P1: diretor e escritor Euros Lyn e Steven Moffat é responsável por quantos episódios? 
dw |> 
  filter(writer == "Steven Moffat", director == "Euros Lyn") |> 
  nrow() #3


#P2: ano: 2015; numero total de episodios
dw |> filter(writer == "Chris Chibnall", year(first_aired) == 2018) |> 
  nrow() #6


#P3> writer: Jamie Mathiesons; quantos anos?
anos <- dw |> 
  filter(writer == "Jamie Mathieson") |> 
  mutate(ano = year(first_aired))
         
anos_JM <- max(anos$ano) - min(anos$ano)
anos_JM


#P4: writer: Jamie Mathieson; quantos roteiros?
dw |> filter(writer == "Jamie Mathieson") |> 
  summarise(roteiros = n_distinct(story_number))

#P5: O diretor Emma Sullivan dirigiu episódios com duração média de quantos minutos?
dw |> 
  filter(director == "Emma Sullivan") |> 
  summarise(media_ep = round(mean(duration, na.rm = TRUE),2))





