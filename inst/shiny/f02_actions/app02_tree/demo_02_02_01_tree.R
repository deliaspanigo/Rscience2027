library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all() # Descomenta si estás en modo desarrollo

ui <- fluidPage(
  # ESTILO CRÍTICO: Convierte la App en un contenedor que no se desborda
  style = "display: flex; flex-direction: column; height: 100vh; width: 100vw; margin: 0; padding: 0; overflow: hidden;",

  tags$div(
    #style = "padding: 10px 20px; background: #111; color: white;",
    h1("TITULO DE PRUEBA", style = "margin: 0; font-size: 24px;")
  ),

  # El módulo absorberá el 100% del espacio restante
  mod_02_02_01_tree_ui("tree_module"),

  "AVERR"
)

server <- function(input, output, session) {
  seleccion <- mod_02_02_01_tree_server("tree_module", show_debug = T)
}

shinyApp(ui, server)
