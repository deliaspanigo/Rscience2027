library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all()


# ==========================================
# 3. APP PRINCIPAL
# ==========================================

# --- APP ---
ui <- fluidPage(
  mod_tree_ui("tree_module")
)

server <- function(input, output, session) {
  seleccion <- mod_tree_server("tree_module", show_debug = T)
}

shinyApp(ui, server)

