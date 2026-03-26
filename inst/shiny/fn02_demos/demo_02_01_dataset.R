# inst/app/demo_importar.R
# App de demostración del módulo de importación
devtools::load_all()

library(shiny)
library(DT)
library(Rscience2027)   # tu paquete
library(bslib)
library(listviewer)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  titlePanel("Demo: Módulo de Importación de Datos"),

  mod_02_01_dataset_ui("demo_dataset"),


)

server <- function(input, output, session) {

  # Llamamos al módulo
  seleccion <- mod_02_01_dataset_server(id = "demo_dataset", show_debug = T)


}

shinyApp(ui, server)
