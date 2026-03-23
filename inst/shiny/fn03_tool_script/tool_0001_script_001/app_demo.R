library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(DT)
library(plotly)
library(colourpicker)

# Cargamos los componentes
source("sub_module/sm01_var_selection.R")
source("sub_module/sm02_levels.R")
source("pack_module.R")

# Usamos page_fillable para que el contenido use todo el espacio sin headers
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # Inyectamos shinyjs para las funciones de bloqueo (JS)
  useShinyjs(),

  # Contenedor principal con un poco de margen para que no toque los bordes
  div(style = "padding: 15px;",
      PACK_mod_main_ui("mi_app")
  )
)

server <- function(input, output, session) {

  # Ejecutamos el pack
  # show_debug = TRUE activará la pestaña de inspección dentro del card del pack
  resultados <- PACK_mod_main_server(
    id = "mi_app",
    df_input = reactive(mtcars),
    show_debug = TRUE
  )

}

shinyApp(ui, server)
