library(shiny)
library(bslib)
library(shinyjs)
library(tidyverse)
library(listviewer)

# 1. Cargar el paquete y módulos
devtools::load_all()

# 2. Configuración de rutas de recursos
path_www <- system.file("www", package = "Rscience2027")
if (path_www == "") path_www <- "inst/www"
addResourcePath("RS-STYLES", normalizePath(path_www))

# 3. UI
ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  shinyjs::useShinyjs(),

  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("RS-STYLES/css/style_000.css?v=", as.numeric(Sys.time()))
    )
  ),

  div(style = "padding: 20px;",
      h2("Pipeline de Visualización ANOVA", style = "color: #00d4ff; font-weight: 300;"),
      hr(style = "border-color: #333;"),
      mod_11_A_script_and_comments_ui("pipeline_1")
  )
)

# 4. SERVER
server <- function(input, output, session) {

  # --- RUTAS DE CONFIGURACIÓN ---

  # A. Ruta del archivo .R del módulo HIJO
  # (Corregido el nombre de la variable para que sea consistente)
  flat_child_path <- system.file(
    "shiny", "fn03_tool_script", "tool_0001_script_002",
    "f01_shiny_show", "p02_01_script_and_comments", "f03_prod",
    "mod_special_script_and_comments.R",
    package = "Rscience2027"
  )

  # B. Carpeta de trabajo
  flat_temp_folder <- system.file(
    "shiny", "fn03_tool_script", "tool_0001_script_002",
    package = "Rscience2027"
  )

  # --- VALIDACIÓN DE RUTAS EN CONSOLA ---
  observe({
    if(flat_child_path == "") {
      warning("!!! ERROR: No se encontró el archivo .R del módulo hijo.")
    } else {
      message(">>> Módulo hijo localizado en: ", flat_child_path)
    }

    if(flat_temp_folder == "") {
      warning("!!! ERROR: No se encontró la carpeta de trabajo.")
    }
  })

  # --- EJECUCIÓN DEL MÓDULO PADRE ---
  mod_11_A_script_and_comments_server(
    id = "pipeline_1",
    module_script_and_comments_file_path = flat_child_path,
    temp_folder_tool_script              = flat_temp_folder,
    # local_folder_tool_script           = flat_temp_folder,  <-- ELIMINA O COMENTA ESTA LÍNEA
    show_file                            = TRUE,
    show_debug                           = TRUE
  )
}

shinyApp(ui, server)
