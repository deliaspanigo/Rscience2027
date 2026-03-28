

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
  mod_02_03_00_script_server(id = "root",
                             vector_str_folder_tool_script = c("tool_0001_script_001", "tool_0001_script_002"),
                             show_debug = TRUE)
}

shinyApp(ui, server)
