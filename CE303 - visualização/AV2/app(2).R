# =====================================================================================
# app.R -- ponto de entrada. Reune global.R (dados e funcoes), ui.R e server.R.
#
# Estrutura da pasta:
#   app.R          (este arquivo)
#   global.R       pacotes, leitura/limpeza dos dados, mapa, formatadores
#   ui.R           interface (objeto `ui`)
#   server.R       logica reativa e graficos (funcao `server`)
#   Agua_Mineral_Producao.csv   (opcional: se estiver na pasta, e usado no lugar da URL do GitHub)
#
# Para rodar: abra app.R no RStudio e clique em "Run App", ou  shiny::runApp("pasta_do_app")
# =====================================================================================

source("global.R", local = TRUE, encoding = "UTF-8")
source("ui.R", local = TRUE, encoding = "UTF-8")
source("server.R", local = TRUE, encoding = "UTF-8")

shinyApp(ui = ui, server = server)
