library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

# Asegúrate de que el nombre del archivo coincida
source(file = "mod_special_script_and_comments.R")

# --- SIMULACIÓN DE DATOS PARA TEST ---
# Buscamos un archivo real en tu instalación de R para que el iframe tenga algo que mostrar
# Si no encuentra el de Rscience2027, usará uno de bslib como respaldo
path_test <- system.file("test_shiny_output", "f05_shiny_output", "tab01_control.html", package = "Rscience2027")
if (path_test == "") {
  path_test <- system.file("LICENSE", package = "bslib") # Fallback para ver 'algo'
}

ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  shinyjs::useShinyjs(),

  layout_column_wrap(
    width = 1,
    card(
      card_header("Test de Visualizador de Vectores"),
      mod_special_script_and_comments_ui("my_test_visualizer")
    )
  )
)

server <- function(input, output, session) {

  # Solo pasamos la carpeta base donde están (o estarán) los archivos
  # El hijo buscará dentro de /f02_quarto_proc/f02_quarto_mod/ automáticamente
  the_folder <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002",package = "Rscience2027")
  mod_special_script_and_comments_server(
    id = "my_test_visualizer",
    temp_folder_tool_script = the_folder,
    show_file = TRUE,
    show_debug = TRUE
  )
}

shinyApp(ui, server)
