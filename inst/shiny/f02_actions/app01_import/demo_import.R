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

  mod_import_ui("demo_import"),


)

server <- function(input, output, session) {

  # Llamamos al módulo
  seleccion <- mod_import_server(id = "demo_import", show_debug = T)


}

shinyApp(ui, server)
