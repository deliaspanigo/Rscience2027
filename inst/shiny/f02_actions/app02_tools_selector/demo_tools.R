# inst/app/demo_importar.R
# App de demostración del módulo de importación
devtools::load_all()

library(shiny)
library(DT)
library(Rscience2027)   # tu paquete

ui <- fluidPage(
  titlePanel("Demo: Módulo de Importación de Datos"),

  module_treeApp_UI("demo_tools"),


)

server <- function(input, output, session) {

  # Llamamos al módulo
  seleccion <- module_treeApp_Server(id = "demo_tools")


}

# Función para lanzar la app desde cualquier lugar
#' Lanzar demo del módulo de importación
#'
#' @export
run_importar_demo <- function() {
  shinyApp(ui = ui, server = server)
}


run_importar_demo()
