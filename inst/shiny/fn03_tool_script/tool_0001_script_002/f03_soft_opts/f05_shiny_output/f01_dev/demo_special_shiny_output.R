library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

devtools::load_all() # Descomenta si estás en modo desarrollo

source(file = "mod_special_shiny_output.R")



# Ejecución limpia
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_special_shiny_output_ui("my_ns_special_shiny_output") # Llamamos a la UI
)

#ui <-
server <- function(input, output, session) {
  mod_special_shiny_output_server(id = "my_ns_special_shiny_output")
}

shinyApp(ui, server)
