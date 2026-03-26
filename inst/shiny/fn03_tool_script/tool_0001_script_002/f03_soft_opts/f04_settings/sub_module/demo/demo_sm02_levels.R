library(shiny)
library(bslib)
library(colourpicker) # Importante cargarlas aquí también para el demo
library(plotly)
library(DT)
library(shinyjs)

# 1. Cargar el módulo (Asegúrate que la ruta sea correcta)
source(file = "../sm02_levels.R")

# 2. UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(), # Necesario para los bloqueos de JS del módulo
  div(class = "container-fluid", style = "padding: 30px;",
      SUB_mod_levels_ui("f_config")
  )
)

# 3. SERVER
server <- function(input, output, session) {

  # Invocamos el módulo
  # Nota: var_rv y var_factor no pueden ser NULL para que el módulo despierte
  SUB_mod_levels_server(
    id = "f_config",
    df_input   = reactive({ mtcars }),
    var_rv     = reactive({ NULL }),   # Cambiado de NULL a "mpg" para probar
    var_factor = reactive({ NULL }),   # Cambiado de NULL a "cyl" para probar
    show_debug = TRUE
  )
}

# 4. RUN
shinyApp(ui, server)
