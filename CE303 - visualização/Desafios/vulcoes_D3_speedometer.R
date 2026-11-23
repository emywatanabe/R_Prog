library(tidyverse)
library(gganimate)
library(gifski)
library(shiny)


volcano <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-05-12/volcano.csv"
)

# 1. Dados: proporção de cada faixa
faixas <- volcano |>
  mutate(last_eruption_year = as.numeric(last_eruption_year)) |>
  filter(!is.na(last_eruption_year), last_eruption_year >= 1700) |>
  mutate(faixa = cut(last_eruption_year,
                     breaks = c(seq(1700, 2000, 100), Inf),
                     right = FALSE,
                     labels = c("1700-1799", "1800-1899", "1900–1999", "2000+"))) |>
  count(faixa) |>
  mutate(
    ordem = row_number(),
    fract = n / sum(n),
    ymax  = cumsum(fract),
    ymin  = lag(ymax, default = 0),
    rotulo = scales::percent(fract, accuracy = 1)
  )

k <- nrow(faixas)

# 2. Uma linha por faixa e por etapa; cada arco se enche quando chega a sua vez
anim_dados <- crossing(faixas, etapa = seq(0, k, by = 0.05)) |>
  mutate(
    prog       = pmin(1, pmax(0, etapa - (ordem - 1))),
    ymax_atual = ymin + (ymax - ymin) * prog
  )

# 3. Gráfico
grafico <- ggplot(
  anim_dados,
  aes(fill = faixa, ymin = ymin, ymax = ymax_atual, xmin = 1, xmax = 2)
) +
  geom_rect(color = "white") +
  geom_text(
    aes(x = 1.5, y = (ymin + ymax) / 2,
        label = rotulo,                                 # texto sempre igual
        alpha = ifelse(prog >= 1, 1, 0)),               # aparece só ao completar
    color = "white", fontface = "bold", size = 6,
    show.legend = FALSE
  ) +
  scale_alpha_identity() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_viridis_d(option = "magma", end = 0.85) +
  coord_radial(
    theta = "y",
    start = -pi / 2,
    end = pi / 2,
    expand = FALSE,
    inner.radius = .5
  ) +
  labs(
    title = "Última Erupção dos Vulcões a cada 100 anos desde 1700",
    fill = "Faixa"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    line = element_blank()
  ) +
  transition_time(etapa) +
  ease_aes("cubic-out")

#animate(grafico, nframes = length(unique(anim_dados$etapa)), fps = 20,
#        width = 650, height = 420, end_pause = 20,
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
