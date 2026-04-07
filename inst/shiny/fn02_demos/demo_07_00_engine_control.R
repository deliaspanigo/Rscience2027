library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)

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

# 2. UI
# 2. UI
ui <- page_fixed(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#fff", primary = "#00d4ff"),

  # tags$head(
  #   useShinyjs(),
  #
  #   # IMPORTANTE: No usamos includeCSS.
  #   # Usamos tags$link apuntando al recurso que definiste con addResourcePath
  #   if (!is.null(path_to_css)) {
  #     tags$link(rel = "stylesheet", type = "text/css", href = "lib_www/style_000.css")
  #   }
  # ),

  mod_07_00_engine_control_ui("main_switch")
)

# 3. SERVER
server <- function(input, output, session) {
  modo_activo <- mod_07_00_engine_control_server("main_switch", show_debug = T)

  observe({
    print(paste("El motor está en modo:", modo_activo()))
  })
}

shinyApp(ui, server)
