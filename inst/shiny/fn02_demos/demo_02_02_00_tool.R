library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all()


# ==========================================
# 3. APP PRINCIPAL
# ==========================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  mod_02_02_00_tool_ui("my_tool")
)

server <- function(input, output, session) {
  # Resultado final de todo el flujo
  resultado_final <- mod_02_02_00_tool_server(id = "my_tool", show_debug = T)

  observe({
    req(resultado_final()$confirmado)
    print("--- SELECCIÓN FINAL RECIBIDA EN APP ---")
    print(resultado_final()$datos$path)
  })
}

shinyApp(ui, server)

