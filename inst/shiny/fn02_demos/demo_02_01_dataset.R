library(shiny)
library(DT)
library(bslib)
library(listviewer)
library(shinyjs)
library(shinyWidgets) # Necesario para el radioGroupButtons

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

# 2. UI DE LA APP
ui <- fluidPage(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

  # Estilo crítico para que la página no tenga scroll propio
  #tags$style("body, html { overflow: hidden; height: 100%; margin: 0; padding: 0; }"),

  tags$head(
    useShinyjs(),
    if (!is.null(path_to_css)) {
      tags$link(rel = "stylesheet", type = "text/css", href = "lib_www/style_000.css")
    }
  ),

  # El h2 y el módulo
  #div(style = "height: 100vh; display: flex; flex-direction: column;",
      div(style = "padding: 10px 0; flex-shrink: 0;",
          h2("RScience Engine v.0.7.2", style="color: #00d4ff; text-align: center; margin:0;")
      ),
      mod_02_01_dataset_ui("demo_dataset")
  #)
)

# 3. SERVER DE LA APP
server <- function(input, output, session) {

  # Llamamos al server del módulo
  # Guardamos el retorno en un reactivo por si queremos usar los datos en la app
  datos_importados <- mod_02_01_dataset_server(id = "demo_dataset", show_debug = F)

  # Ejemplo de cómo acceder a los datos desde fuera del módulo
  observe({
    req(datos_importados()$is_done)
    message("App principal detectó carga de: ", datos_importados()$metadata$name_mod)
  })
}

shinyApp(ui, server)
