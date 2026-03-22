library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all()


# ==========================================
# 3. APP PRINCIPAL
# ==========================================
ui <- fluidPage(
  mod_tools_ui("my_tool")
)

server <- function(input, output, session) {
  # Resultado final de todo el flujo
  resultado_final <- mod_tools_server("my_tool")

  observe({
    req(resultado_final()$confirmado)
    print("--- SELECCIÓN FINAL RECIBIDA EN APP ---")
    print(resultado_final()$datos$path)
  })
}

shinyApp(ui, server)

