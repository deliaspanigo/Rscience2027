library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

devtools::load_all() # Descomenta si estás en modo desarrollo

source(file = "mod_special_bibliography.R")



# Ejecución limpia
ui <- page_fluid(
  #theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_special_bibliography_ui("my_ns_special_bibliography"),
  mod_special_bibliography_DEBUG_ui("my_ns_special_bibliography")# Llamamos a la UI
)

#ui <-
server <- function(input, output, session) {

  mod_special_bibliography_server(id = "my_ns_special_bibliography")
}

shinyApp(ui, server)
