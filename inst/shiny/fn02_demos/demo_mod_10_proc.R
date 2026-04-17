library(shiny)
library(bslib)
library(shinyjs)
library(tidyverse)

# 1. Cargar el paquete
devtools::load_all()

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
  mod_10_00_proccessing_ui("pipeline_1")
)

server <- function(input, output, session) {

  flat_module_proccessing_file_path <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002",
                                                   "f01_shiny_show", "p01_05_proccessing", "f03_prod", "mod_special_proccessing.R" ,
                                                   package = "Rscience2027")

  flat_local_folder_tool_script <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002", package = "Rscience2027")
  flat_temp_folder_tool_script  <- flat_local_folder_tool_script

  # the_folder_package <- system.file(package = "Rscience2027")
  # the_folder_relative <- file.path(the_folder_package, "shiny", "fn03_tool_script", "tool_0001_script_002")
  # the_folder_absolute <- normalizePath(the_folder_relative, mustWork = T)

  mod_10_00_proccessing_server(
    id = "pipeline_1",
    module_proccessing_file_path = flat_module_proccessing_file_path,
    local_folder_tool_script = flat_local_folder_tool_script,
    temp_folder_tool_script  = flat_temp_folder_tool_script,
    list_settings = NULL
  )
}

shinyApp(ui, server)
