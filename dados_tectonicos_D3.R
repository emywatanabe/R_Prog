library(tidyverse)
library(stringr)
library(gganimate)
library(gifski)
library(shiny)

# Carregar os dados do TidyTuesday
volcano <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-05-12/volcano.csv",
  show_col_types = FALSE)

# filtar os dados para o gráfico animado
dados_tectonicos <- volcano |>
  filter(!is.na(tectonic_settings), tectonic_settings != "Unknown") |>
  count(tectonic_settings, sort = TRUE) |>
  mutate(
    ordem = row_number(),
    tectonic_settings = str_replace(tectonic_settings, " / ", " /\n"),
    tectonic_settings = factor(
      tectonic_settings,
      levels = rev(unique(tectonic_settings))
    )
  )

k <- nrow(dados_tectonicos)

# Execução de uma categoria por vez
anim_dados <- crossing(
  dados_tectonicos,
  etapa = seq(0, k, by = 0.5)
) |>
  mutate(
    n_atual = n * pmin(1, pmax(0, etapa - (ordem - 1)))
  )

# 3. Construir o ggplot + gganimate - feito com o esquisse e ajustado ao codigo principal
grafico <- ggplot(anim_dados) +
  aes(x = n_atual, y = tectonic_settings) +
  geom_col(fill = "red") +
  geom_text(
    aes(label = ifelse(n_atual > 0, round(n_atual), "")),
    hjust = -0.3,
    fontface = "bold",
    size = 4.5
  ) +
  coord_cartesian(xlim = c(0, max(dados_tectonicos$n) * 1.15)) +
  labs(
    title = "Contexto Tectônico dos vulcões ",
    x = "Contexto tectônico",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L, face = "bold.italic"),
    axis.title.y = element_text(size = 13L, face = "bold"),
    axis.title.x = element_text(size = 13L, face = "bold"),
    axis.text.y = element_text(face = "bold", size = 13L),
    axis.text.x = element_text(face = "bold", size = 12L, vjust = 3L),
    panel.grid.major.y = element_blank()
  ) +
  transition_time(etapa) +
  ease_aes("cubic-out")
grafico

# 4. Aplicativo Shiny
ui <- fluidPage(
  titlePanel("Configurações Tectônicas dos Vulcões"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "fps",
        label = "Velocidade da animação (FPS):",
        min = 5,
        max = 40,
        value = 20,
        step = 5
      ),
      actionButton(
        inputId = "gerar",
        label = "Atualizar Animação",
        class = "btn-primary"
      )
    ),
    mainPanel(
      imageOutput("animacao", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  gif_path <- eventReactive(input$gerar, {
    outfile <- tempfile(fileext = ".gif")
    
    withProgress(message = "Renderizando animação...", value = 0.5, {
      anim <- animate(
        grafico,
        nframes = length(unique(anim_dados$etapa)) * 4,
        fps = input$fps,
        width = 650,
        height = 500,
        end_pause = 20,
        renderer = gifski_renderer()
      )
      incProgress(0.4)
      anim_save(outfile, animation = anim)
    })
    
    outfile
  }, ignoreNULL = FALSE)
  
  output$animacao <- renderImage({
    req(gif_path())
    list(
      src = gif_path(),
      contentType = "image/gif",
      width = 650,
      height = 500,
      alt = "Gráfico de barras animado das configurações tectônicas dos vulcões"
    )
  }, deleteFile = TRUE)
}

shinyApp(ui, server)
