library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
library(shiny)


# 1. Carregar os dados do TidyTuesday
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-05-12/volcano.csv')

# 2. Filtrar dados
faixas <- volcano |> 
  mutate(last_eruption_year = suppressWarnings(as.numeric(last_eruption_year))) %>%
  filter(!is.na(last_eruption_year), last_eruption_year >= 1700) %>%
  mutate(faixa = cut(last_eruption_year,
                     breaks = c(seq(1700, 2000, 100), Inf),
                     right = FALSE,
                     labels = c("1700-1799", "1800-1899", "1900–1999",
                                "2000+"))) %>%
  count(faixa) %>%
  mutate(ordem = row_number())

k <- nrow(faixas)

# Uma linha por faixa e por etapa; cada faixa cresce quando chega a sua vez
anim_dados <- crossing(faixas, etapa = seq(0, k, by = 0.05)) %>%
  mutate(n_atual = n * pmin(1, pmax(0, etapa - (ordem - 1))))

grafico <- ggplot(anim_dados, aes(x = n_atual, y = faixa)) +
  geom_segment(aes(x = 0, 
                   xend = n_atual, 
                   yend = faixa),
               color = "gray", 
               linewidth = 1.2) +
  geom_point(color = "darkorange", 
             size = 6) +
  geom_text(aes(label = round(n_atual)), 
            hjust = -0.7, 
            size = 5) +
  coord_cartesian(xlim = c(0, max(faixas$n) * 1.15)) +   
  labs(
    title = "Última Erupção dos Vulcões a cada 100 anos desde 1700",
    x = "Quantidade de Vulcões", y = "faixa"
  ) +
  theme_minimal(base_size = 16) +
  transition_time(etapa) +
  ease_aes("cubic-out")

#animate(grafico, nframes = length(unique(anim_dados$etapa)), fps = 20,
#       width = 650, height = 420, end_pause = 20,
#        renderer = gifski_renderer())


ui <- fluidPage(
  titlePanel("Última erupção dos vulcões a cada 100 anos desde 1700"),
  imageOutput("animacao", height = "430px")
)

server <- function(input, output, session) {
  output$animacao <- renderImage({
    outfile <- tempfile(fileext = ".gif")
    
    withProgress(message = "Renderizando animação...", value = 0.5, {
      anim <- animate(
        grafico,
        nframes = length(unique(anim_dados$etapa)), fps = 20,
        width = 650, height = 420, end_pause = 20,
        renderer = gifski_renderer()
      )
      anim_save(outfile, anim)
    })
    
    list(src = outfile, contentType = "image/gif", width = 650, height = 420)
  }, deleteFile = TRUE)
}

shinyApp(ui, server)
