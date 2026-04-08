library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)

devtools::load_all()


# Registro de recursos CSS
css_folder <- system.file("www", "css", package = "Rscience2027")
if (css_folder == "") css_folder <- "www/css"
try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)

# 2. UI
# 2. UI
ui <- page_fixed(
  # 1. Definir el tema primero
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#fff", primary = "#00d4ff"),

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

  # 3. El contenido de la UI
  div(class = "container-fluid",
      mod_07_00_engine_control_ui("main_switch"),
      mod_07_00_engine_control_DEBUG_ui("main_switch")
  )
)

# 3. SERVER
server <- function(input, output, session) {
  modo_activo <- mod_07_00_engine_control_server("main_switch", show_debug = T)

}

shinyApp(ui, server)
