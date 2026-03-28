library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================

mod_04_00_collector02_settings_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_settings"))
}



# ==============================================================================
# MÓDULO SERVER: CARGA LOCAL Y RENDERIZADO
# ==============================================================================

# ==============================================================================
# MÓDULO SERVER: CARGA LOCAL CON SALIDA REACTIVA
# ==============================================================================

mod_04_00_collector02_settings_server <- function(id, df_input, folder_path_tool_script, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Creamos un reactiveVal INTERNO para guardar la instancia del servidor cargado
    # Lo inicializamos como una función vacía que devuelve NULL para evitar errores al inicio
    current_sub_server_out <- reactiveVal(function() NULL)

    # Entorno local aislado
    local_env <- new.env(parent = environment())

    # Manejo de la ruta
    path_r <- reactive({
      if (is.function(folder_path_tool_script)) folder_path_tool_script() else folder_path_tool_script
    })

    # 2. El OBSERVE se encarga de la "fontanería" (source y ejecución)
    observe({
      req(path_r())
      base_p <- path_r()

      # Al cambiar de carpeta, reseteamos la salida actual para que el padre sepa
      # que la herramienta anterior ya no es válida
      current_sub_server_out(function() NULL)

      path_settings <- file.path(base_p, "f03_soft_opts", "f04_settings", "f03_prod", "mod_special_settings.R")

      if (file.exists(path_settings)) {
        tryCatch({
          # Source local
          source(file = path_settings, local = local_env)

          # Render UI
          output$placeholder_settings <- renderUI({
            req(local_env$mod_special_settings_ui)
            local_env$mod_special_settings_ui(ns("sub_settings"))
          })

          # Ejecución del servidor y captura de su retorno
          if (!is.null(local_env$mod_special_settings_server)) {
            res <- local_env$mod_special_settings_server(id = "sub_settings", df_input = df_input, folder_path_tool_script = folder_path_tool_script)
            # Guardamos el objeto reactivo que devuelve el sub-módulo
            current_sub_server_out(res)
          }

        }, error = function(e) {
          warning("Error en carga dinámica: ", e$message)
        })
      }
    })

    # 3. LA SALIDA REACTIVA FINAL
    # Este es el objeto que el padre consumirá.
    # Al ser un reactive(), se invalidará automáticamente cuando current_sub_server_out cambie.
    the_output <- reactive({
      # Llamamos al reactivo que capturamos del sub-módulo
      current_sub_server_out()()
    })

    # 4. RETORNO
    return(the_output)
  })
}
