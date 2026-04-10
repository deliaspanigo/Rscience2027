library(shiny)
library(shinyjs)
library(bslib)


source(file = "sm01_var_selection.R")

# ==============================================================================
# MAIN APP
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),
  div(class = "container", style = "margin-top: 30px; max-width: 900px;",
      SUB_mod_var_selection_ui("mi_selector")
  )
)

server <- function(input, output, session) {
  seleccion_obj <- SUB_mod_var_selection_server("mi_selector", df_input = iris, show_debug = T)
}

shinyApp(ui, server)
