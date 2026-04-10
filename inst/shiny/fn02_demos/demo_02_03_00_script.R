library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)


devtools::load_all()

# Registro de recursos CSS
css_folder <- system.file("www", "css", package = "Rscience2027")
if (css_folder == "") css_folder <- "www/css"
try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)


# UI con disposición vertical (Interactividad -> Debug)
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
  # Contenedor principal
  div(style = "max-width: 1200px; margin: 20px auto;",

      # 1. Bloque de Interactividad (Script Engine)
      card(
        full_screen = TRUE,
        card_header(span(icon("code"), " Script Explorer Engine")),
        mod_02_03_00_script_ui("test_id")
      ),

      br(), # Espaciador visual

      # 2. Bloque de Debug (Aparece debajo)
      card(
        style = "border-color: #005555;", # Un borde sutil de color para indicar zona de debug
        card_header(span(icon("bug"), " Console & Metadata Debugger")),
        mod_02_03_00_script_DEBUG_ui("test_id")
      )
  )
)

server <- function(input, output, session) {
  # Llamada al server del módulo
  # El retorno 'datos_out' es el que fluye hacia el Collector 01 después
  datos_out <- mod_02_03_00_script_server(
    id = "test_id",
    vector_str_folder_tool_script = reactive(c("tool_0001_script_001", "tool_0001_script_002")),
    show_debug = TRUE
  )
}

shinyApp(ui, server)
