library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)

devtools::load_all()

# Registro de recursos CSS
css_folder <- system.file("www", "css", package = "Rscience2027")
if (css_folder == "") css_folder <- "www/css"
try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)


# UI con disposición vertical (Interactividad -> Debug)
ui <- bslib::page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#fff", primary = "#00d4ff"),

  tags$head(
    useShinyjs(),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      # RS-STYLES debe ser el nombre que registraste en addResourcePath
      href = paste0("RS-STYLES/style_000.css?v=", as.numeric(Sys.time()))
    )
  ),

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
