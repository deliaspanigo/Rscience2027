library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all()

# Registro de recursos CSS
css_folder <- system.file("www", "css", package = "Rscience2027")
if (css_folder == "") css_folder <- "www/css"
try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)


ui <- fluidPage(
  # ESTILO CRÍTICO: Convierte la App en un contenedor que no se desborda
  #style = "display: flex; flex-direction: column; height: 100vh; width: 100vw; margin: 0; padding: 0; overflow: hidden;",

  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

  # 2. Configurar el HEAD
  tags$head(
    useShinyjs(),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      # RS-STYLES debe ser el nombre que registraste en addResourcePath
      href = paste0("RS-STYLES/style_000.css?v=", as.numeric(Sys.time()))
    )
  ),
  tags$div(
    #style = "padding: 10px 20px; background: #111; color: white;",
    h1("TITULO DE PRUEBA", style = "margin: 0; font-size: 24px;")
  ),

  # El módulo absorberá el 100% del espacio restante
  mod_02_02_01_tree_ui("tree_module"),
  mod_02_02_00_tool_DEBUG_ui("tree_module")
)

server <- function(input, output, session) {
  seleccion <- mod_02_02_01_tree_server("tree_module", show_debug = T)
}

shinyApp(ui, server)
