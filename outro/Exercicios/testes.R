library(tidyverse)

michelin <- read_csv("Downloads/michelin.csv")
michelin
glimpse(michelin)
view(michelin)

ggplot(data = michelin,
       mapping = aes(x = Latitude, y = Longitude, color = Award)
       )+
      geom_point() +
      labs(title = "Localização dos restaurantes",
           subtitle = "de 1 a 3 estrelas",
           x = "Latitude / graus",
           y = "Longitude / graus",
           color = "Award",
           shape = "Award",
           ?legend = "Os dados são provenientes do pacote dados") + 
      scale_color_hue()
nrow(michelin)
ncol(michelin)           

#===========
library(tidyverse)

michelin <- read_csv("arquivo_csv/michelin.csv")

michelin |>  
  filter(Location %in% c("Florence, Italy", "Osaka, Japan"))

michelin |> 
  arrange(desc(Latitude))

michelin |> 
  distinct(Latitude, Longitude, .keep_all = TRUE)

michelin |> 
  count(Latitude, Longitude, sort = TRUE)
#=====================

#Filter
library(dplyr)
library(stringr)
michelin |> 
  filter(str_detect(Award, "3 Stars"))

michelin |>
    filter(Cuisine %in% c("Italian", "Contemporary", "French")) 

michelin |> 
  filter(str_detect(FacilitiesAndServices, "Air conditioning"), 
         str_detect(FacilitiesAndServices, "Terrace")) |> nrow()

michelin |> 
  filter(str_detect(Location, "Shanghai"))

#Arrange

michelin |> 
  arrange(desc(Latitude))

michelin |> 
  summarize(
    max_lat = max(Latitude),
    min_lat = min(Latitude)
  )
#mutate

michelin |> 
  mutate(tamanho_descricao = nchar(Description)) |> 
  arrange(desc(tamanho_descricao))

michelin |> 
  filter(Price %in% c("€€€€", "¥¥¥¥", "$$$$")) |> nrow()

michelin |> 
  filter(str_detect(Award, "Green Star"))


#=================
library(tidyverse)
michelin <- read_csv("Downloads/michelin.csv")

#equação de Haversine
haversine_equation <- function(lat1, lon1, lat2, lon2){
  r <- 6371.0
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon1 <- lon1 * pi / 180
  lon2 <- lon2 * pi / 180
  
  a <- (sin((lat2 - lat1)/2)^2+cos(lat1)*cos(lat2)*sin((lon2 - lon1)/2)^2)
 
  2*r*asin(sqrt(a))
  }

#qual o restaurante inicial?
restaurante_inicial = michelin |> filter(str_detect(Name, "Pipero Roma"))
stopifnot(nrow(restaurante_inicial) == 1)   # confere se achou só um

#onde fica o restaurante inicial?
lat_ref <- restaurante_inicial$Latitude
lon_ref <- restaurante_inicial$Longitude
cidade_ref <- restaurante_inicial$Location

#P1: quantos km ele precisa viajar para restaurante "1 Star"?
#distancia de todos até o restaurante inicial
michelin_dist <-  michelin |> 
  mutate(quantos_km = haversine_equation(lat_ref, lon_ref, Latitude, Longitude))

#michelin_fora é a base sem a cidade referencia
michelin_fora <- michelin_dist |> 
  filter(Location != cidade_ref)

#estrelas
proximo_1_Star <- michelin_fora |> 
  filter(str_detect(Award, "^1 Star")) |> 
  arrange(quantos_km) |> 
  slice(1)

proximo_1_Star |> select(Name, Location, Award, quantos_km)

#P2.quantos restaurantes 1 2 ou 3 estrelas a uma distancia de 500 km do restaurante inicial
restaurante_estrelas <- michelin_dist |> 
  filter(
   str_detect(Award, "^[123] Stars?"),
    quantos_km <= 500)
nrow(restaurante_estrelas)  

#P3. >= 1 Star; <= $$$, quantos_km = max(1500) fora do inicial
aniversario <- michelin_fora |> 
  filter(
    str_detect(Award, "^[123] Stars?"),
    !is.na(Price), nchar(Price) <= 3,     #!is.na(Price) descarta os restaurantes sem preço informado
    quantos_km <= 1500
         )
nrow(aniversario)

#P4. Culinaria Mediterranean, quantos_km?
culinaria <- michelin_fora |> 
  filter(str_detect(Cuisine, "Mediterranean Cuisine")) |> 
  arrange(quantos_km) |> 
  slice_head()

culinaria |> select(Name, Location, Cuisine, quantos_km)
round(culinaria$quantos_km, 2)


