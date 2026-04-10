library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all()

# 1. CARGAR RECURSOS (Esto es lo que faltaba)
# Registro de recursos CSS
# Registro de recursos CSS
css_folder <- system.file("www", "css", package = "Rscience2027")
if (css_folder == "") css_folder <- "www/css"
try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)


# ==========================================
# 3. APP PRINCIPAL
# ==========================================
ui <- bslib::page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

  tags$head(
    useShinyjs(),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      # RS-STYLES debe ser el nombre que registraste en addResourcePath
      href = paste0("RS-STYLES/style_000.css?v=", as.numeric(Sys.time()))
    )
  ),

  mod_02_02_00_tool_ui("my_tool"),
  mod_02_02_00_tool_DEBUG_ui("my_tool")
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

