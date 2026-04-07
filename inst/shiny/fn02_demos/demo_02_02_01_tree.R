library(shiny)
library(jsonlite)
library(dplyr)

devtools::load_all()

# 1. CARGAR RECURSOS (Esto es lo que faltaba)
# Buscamos la carpeta www del paquete
lib_www_path <- system.file("www", "css", package = "Rscience2027")

# Si estás en desarrollo local (sin el paquete instalado aún)
if (lib_www_path == "") lib_www_path <- "www"

# Si existe la carpeta, creamos la ruta y el path al CSS
if (dir.exists(lib_www_path)) {
  addResourcePath("lib_www", normalizePath(lib_www_path))
  path_to_css <- file.path(lib_www_path, "style_000.css")
} else {
  path_to_css <- NULL
}


ui <- fluidPage(
  # ESTILO CRÍTICO: Convierte la App en un contenedor que no se desborda
  #style = "display: flex; flex-direction: column; height: 100vh; width: 100vw; margin: 0; padding: 0; overflow: hidden;",

  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

  tags$head(
    useShinyjs(),
    if (!is.null(path_to_css)) {
      tags$link(rel = "stylesheet", type = "text/css", href = "lib_www/style_000.css")
    }
  ),
  tags$div(
    #style = "padding: 10px 20px; background: #111; color: white;",
    h1("TITULO DE PRUEBA", style = "margin: 0; font-size: 24px;")
  ),

  # El módulo absorberá el 100% del espacio restante
  mod_02_02_01_tree_ui("tree_module"),

  "AVERR"
)

server <- function(input, output, session) {
  seleccion <- mod_02_02_01_tree_server("tree_module", show_debug = T)
}

shinyApp(ui, server)
