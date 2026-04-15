library(shiny)
library(bslib)
library(shinyjs)
library(tidyverse)

# 1. Cargar el paquete
devtools::load_all()

source(file = "mod_special_proccessing.R")


# 2. Configuración de rutas robusta
# Buscamos la carpeta inst/www/css que es el estándar de paquetes R
path_www <- system.file("www", package = "Rscience2027")
if (path_www == "") path_www <- "inst/www" # Fallback para desarrollo con devtools

addResourcePath("RS-STYLES", normalizePath(path_www))

# 3. Definir la ruta de test de archivos QMD
# Asegúrate de que esta ruta realmente devuelva algo
path_test <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002", package = "Rscience2027")
if (path_test == "") path_test <- "inst/shiny/fn03_tool_script/tool_0001_script_002"

ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("RS-STYLES/css/style_000.css?v=", as.numeric(Sys.time())) # Nota el /css/ adicional si registraste 'www'
    )
  ),
  mod_special_proccessing_ui("pipeline_1"),
  mod_special_proccessing_DEBUG_ui("pipeline_1")

)

server <- function(input, output, session) {
  mod_special_proccessing_server(
    id = "pipeline_1",
    local_folder_tool_script = path_test,
    temp_folder_tool_script = path_test,
    list_settings = NULL
  )
}

shinyApp(ui, server)
