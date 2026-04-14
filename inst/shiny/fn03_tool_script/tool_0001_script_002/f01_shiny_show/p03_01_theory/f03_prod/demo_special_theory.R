library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

devtools::load_all() # Descomenta si estás en modo desarrollo

source(file = "mod_special_theory.R")



# Ejecución limpia
ui <- page_fluid(
  #theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_special_theory_ui("my_ns_special_theory"),
  mod_special_theory_DEBUG_ui("my_ns_special_theory")# Llamamos a la UI
)

#ui <-
server <- function(input, output, session) {

  mod_special_theory_server(id = "my_ns_special_theory")
}

shinyApp(ui, server)
