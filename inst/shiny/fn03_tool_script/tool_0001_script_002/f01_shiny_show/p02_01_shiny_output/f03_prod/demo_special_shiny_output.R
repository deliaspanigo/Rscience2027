library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

devtools::load_all() # Descomenta si estás en modo desarrollo

source(file = "mod_special_shiny_output.R")



# Ejecución limpia
ui <- page_fluid(
  #theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_special_shiny_output_ui("my_ns_special_cite"),
  #mod_special_cite_DEBUG_ui("my_ns_special_cite")# Llamamos a la UI
)

#ui <-
file_path <- system.file("test_shiny_output", "f05_shiny_output", "tab01_control.html", package = "Rscience2027")
server <- function(input, output, session) {

  mod_special_shiny_output_server(id = "my_ns_special_cite", file_path = reactive(file_path))
}

shinyApp(ui, server)
