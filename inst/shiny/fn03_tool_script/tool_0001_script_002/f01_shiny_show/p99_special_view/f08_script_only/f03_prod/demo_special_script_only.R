library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

devtools::load_all() # Descomenta si estás en modo desarrollo

source(file = "mod_special_script_only.R")



# Ejecución limpia
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_special_script_only_ui("my_ns_special_script_only") # Llamamos a la UI
)

#ui <-
server <- function(input, output, session) {
  mod_special_script_only_server(id = "my_ns_special_script_only")
}

shinyApp(ui, server)
