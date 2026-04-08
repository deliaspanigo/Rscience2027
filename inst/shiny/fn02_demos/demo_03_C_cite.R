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

  tagList(
      mod_03_C_cite_ui("test"),
      mod_03_C_cite_DEBUG_ui("test"),
  )
)

# 3. SERVER
server <- function(input, output, session) {

  the_folder_package <- system.file(package = "Rscience2027")
  the_folder_relative <- file.path(the_folder_package, "shiny", "fn03_tool_script", "tool_0001_script_002")
  the_folder_absolute <- normalizePath(the_folder_relative, mustWork = T)

  modo_activo <- mod_03_C_cite_server("test", folder_path_tool_script = the_folder_absolute, show_debug = T)

  # --- DENTRO DEL SERVER DE TU DEMO ---

  observe({
    # 1. Obtenemos el retorno del módulo (que es un reactive)
    # modo_activo es un reactive({ rv$sub_data })
    res_modulo <- modo_activo()

    # 2. Validamos que no sea NULL antes de imprimir
    req(res_modulo)

    # 3. Accedemos al contenido.
    # Como el submódulo devuelve 'cite_meta' (otro reactive),
    # tenemos que ejecutarlo con () para ver la lista de metadatos.
    datos_internos <- res_modulo()

    # Ahora sí, extraemos el string que queremos
    mensaje <- paste("El motor está en modo:", datos_internos$status)

    print(mensaje)
  })
}

shinyApp(ui, server)
