library(tidyverse)

# Caminho do arquivo (ajuste se necessário)
michelin <- read_csv("arquivo_csv/michelin.csv")

glimpse(michelin)

# ---- Função Haversine ----
haversine_distance <- function(lat1_deg, lon1_deg, lat2_deg, lon2_deg) {
  # converter graus para radianos
  lat1 <- lat1_deg * pi / 180
  lon1 <- lon1_deg * pi / 180
  lat2 <- lat2_deg * pi / 180
  lon2 <- lon2_deg * pi / 180
  
  r <- 6371  # raio da Terra em km
  
  a <- sin((lat2 - lat1) / 2)^2 +
    cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2
  
  2 * r * asin(sqrt(a))
}

#Restaurante inicial: Sensi em Amalfi, Italy ----
restaurante_inicial <- michelin |>
  filter(str_detect(Name, "Sensi"))

stopifnot(nrow(restaurante_inicial) == 1)  # garante que achou só um

lat_ref    <- restaurante_inicial$Latitude
lon_ref    <- restaurante_inicial$Longitude
cidade_ref <- restaurante_inicial$Location

# Base com a distância até o restaurante inicial.
# O restaurante inicial CONTINUA na base (dist_km = 0); cada pergunta
# decide se ele entra ou não.
michelin_dist <- michelin |>
  mutate(dist_km = haversine_distance(lat_ref, lon_ref, Latitude, Longitude))

# Base fora da cidade inicial (perguntas 1, 3 e 4)
michelin_fora <- michelin_dist |>
  filter(Location != cidade_ref)

# Padrão para estrelas Michelin de verdade (1, 2 ou 3 Stars).
# Não usar "Star" solto: pegaria "Green Star" e "Bib Gourmand,Green Star",
# que não são estrelas Michelin.
padrao_estrelas <- "^[123] Stars?"

# 1. Próximo restaurante com 1 Star (fora da cidade inicial) ----
proximo_1_estrela <- michelin_fora |>
  filter(str_detect(Award, "^1 Star")) |>
  arrange(dist_km) |>
  slice(1)

proximo_1_estrela |> select(Name, Location, Award, dist_km)
round(proximo_1_estrela$dist_km, 2)  

# 2. Restaurantes com 1, 2 ou 3 estrelas a até 3000 km ----
# Usa michelin_dist (inclui o próprio restaurante inicial, 1 Star, dist = 0)
restaurantes_500km <- michelin_dist |>
  filter(
    dist_km <= 500,
    str_detect(Award, padrao_estrelas)
  )

nrow(restaurantes_500km)             

# 3. Aniversário: >= 1 estrela, até 3 símbolos de preço,
#         no máximo 1500 km, fora da cidade inicial ----
aniversario_opcoes <- michelin_fora |>
  filter(
    dist_km <= 3000,
    str_detect(Award, padrao_estrelas),
    !is.na(Price), nchar(Price) <= 2
  )

nrow(aniversario_opcoes)             

# 4. Creative: menor distância (fora da cidade inicial) ----
creative_mais_proximo <- michelin_fora |>
  filter(str_detect(Cuisine, "Creative")) |>
  arrange(dist_km) |>
  slice(1)

creative_mais_proximo |> select(Name, Location, Cuisine, dist_km)
round(creative_mais_proximo$dist_km, 2) 
