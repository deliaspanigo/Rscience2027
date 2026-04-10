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

    # 1. Usamos un reactiveVal para guardar el RETORNO del submódulo
    # No guardamos la función, guardamos el objeto reactivo mismo.
    sub_module_data_r <- reactiveVal(NULL)

    # Entorno local
    local_env <- new.env(parent = environment())

    path_r <- reactive({
      val <- if (is.function(folder_path_tool_script)) folder_path_tool_script() else folder_path_tool_script
      req(val)
      val
    })

    # 2. Monitor de cambio de ruta
    observeEvent(path_r(), {
      base_p <- path_r()
      path_settings <- file.path(base_p, "f01_shiny_show", "p02_settings", "f03_prod", "mod_special_settings.R")

      req(file.exists(path_settings))

      tryCatch({
        # Limpiamos el entorno para evitar basura de la herramienta anterior
        local_env <- new.env(parent = environment())
        source(file = path_settings, local = local_env)

        # Renderizar la UI primero
        output$placeholder_settings <- renderUI({
          req(local_env$mod_special_settings_ui)
          # Usamos un ID fijo pero dentro de nuestro namespace
          local_env$mod_special_settings_ui(ns("sub_settings"))
        })

        # EJECUCIÓN CLAVE:
        # Ejecutamos el servidor y guardamos el objeto reactivo que devuelve.
        if (!is.null(local_env$mod_special_settings_server)) {
          # IMPORTANTE: El ID aquí debe coincidir EXACTAMENTE con el de la UI arriba
          res <- local_env$mod_special_settings_server(
            id = "sub_settings",
            df_input = df_input,
            folder_path_tool_script = path_r # Pasamos el reactivo de la ruta
          )

          # Guardamos el objeto reactivo (la lista de inputs/datos)
          sub_module_data_r(res)
        }

      }, error = function(e) {
        warning("Error en carga dinámica RScience: ", e$message)
      })
    })

    # 3. Retorno simplificado
    # Esto devolverá el contenido del reactivo del submódulo.
    return(reactive({
      req(sub_module_data_r())
      # Ejecutamos el reactivo guardado para obtener los valores actuales
      sub_module_data_r()()
    }))
  })
}
