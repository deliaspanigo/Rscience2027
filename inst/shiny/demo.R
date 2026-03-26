

library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

# Ejecución limpia
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_main_explorer_ui("root") # Llamamos a la UI
)

#ui <-
server <- function(input, output, session) {
  mod_main_explorer_server("root")
}

shinyApp(ui, server)
