library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all()

# # 1. CARGAR RECURSOS (Esto es lo que faltaba)
# # Buscamos la carpeta www del paquete
# lib_www_path <- system.file("www", "css", package = "Rscience2027")
#
# # Si estás en desarrollo local (sin el paquete instalado aún)
# if (lib_www_path == "") lib_www_path <- "www"
#
# # Si existe la carpeta, creamos la ruta y el path al CSS
# if (dir.exists(lib_www_path)) {
#   addResourcePath("lib_www", normalizePath(lib_www_path))
#   path_to_css <- file.path(lib_www_path, "style_000.css")
# } else {
#   path_to_css <- NULL
# }


# ==========================================
# 3. APP PRINCIPAL
# ==========================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

  tags$head(
    useShinyjs(),
    if (!is.null(path_to_css)) {
      tags$link(rel = "stylesheet", type = "text/css", href = "lib_www/style_000.css")
    }
  ),

  mod_02_02_00_tool_ui("my_tool")
)

server <- function(input, output, session) {
  # Resultado final de todo el flujo
  resultado_final <- mod_02_02_00_tool_server(id = "my_tool", show_debug = F)

  observe({
    req(resultado_final()$confirmado)
    print("--- SELECCIÓN FINAL RECIBIDA EN APP ---")
    print(resultado_final()$datos$path)
  })
}

shinyApp(ui, server)

