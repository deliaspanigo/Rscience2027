

library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

devtools::load_all() # Descomenta si estás en modo desarrollo


# Ejecución limpia
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_02_03_00_script_ui("root") # Llamamos a la UI
)

#ui <-
server <- function(input, output, session) {
  mod_02_03_00_script_server("root")
}

shinyApp(ui, server)
