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
    mod_04_00_settings_ui("test"),
    mod_04_00_settings_DEBUG_ui("test"),
  )
)

# 3. SERVER
server <- function(input, output, session) {

  the_folder_package <- system.file(package = "Rscience2027")
  the_folder_relative <- file.path(the_folder_package, "shiny", "fn03_tool_script", "tool_0001_script_002")
  the_folder_absolute <- normalizePath(the_folder_relative, mustWork = TRUE)

  df_input <- reactive({ mtcars })

  # modo_activo es un reactive que devuelve el resultado del orquestador
  modo_activo <- mod_04_00_settings_server(
    id = "test",
    df_input = df_input,
    folder_path_tool_script = reactive(the_folder_absolute),
    show_debug = TRUE
  )

  observe({
    # 1. Obtenemos el objeto que emite el orquestador
    # Esto ya nos devuelve la LISTA final (is_locked, metadata, etc.)
    res_final <- modo_activo()

    # 2. Validamos existencia
    req(res_final)

    # 3. Accedemos a los datos directamente (YA NO SON REACTIVOS AQUÍ)
    # Según tu mod_special_settings_server, la lista tiene 'is_locked' y 'metadata'

    estado_bloqueo <- if(res_final$is_locked) "BLOQUEADO" else "PENDIENTE"

    # Si quieres imprimir la selección de variables:
    vars_seleccionadas <- res_final$metadata$selection

    cat("\n--- [DEMO MONITOR] ---")
    cat("\nEstado General:", estado_bloqueo)
    cat("\nVariables:", paste(vars_seleccionadas, collapse = ", "))
    cat("\n----------------------\n")
  })
}

shinyApp(ui, server)
