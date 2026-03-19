# inst/app/demo_importar.R
# App de demostración del módulo de importación

library(shiny)
library(DT)
library(Rscience2027)   # tu paquete

ui <- fluidPage(
  titlePanel("Demo: Módulo de Importación de Datos"),

  mod_import_ui("demo_import"),


)

server <- function(input, output, session) {

  # Llamamos al módulo
  seleccion <- mod_import_server("demo_import")


}

# Función para lanzar la app desde cualquier lugar
#' Lanzar demo del módulo de importación
#'
#' @export
run_importar_demo <- function() {
  shinyApp(ui = ui, server = server)
}


run_importar_demo()
