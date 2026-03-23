library(shiny)
library(bslib)

# Cargamos el archivo de módulos si estuviera separado
# source("modules.R")

source(file = "../sm02_levels.R")


ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  div(class = "container-fluid", style = "padding: 30px;", SUB_mod_levels_ui("f_config"))
)
server <- function(input, output, session) { SUB_mod_levels_server("f_config", show_debug = TRUE) }
shinyApp(ui, server)
