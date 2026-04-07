library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================

mod_03_A_theory_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder"))
}


# ==============================================================================
# MÓDULO SERVER: CARGA LOCAL Y RENDERIZADO
# ==============================================================================

mod_03_A_theory_server <- function(id, folder_path_tool_script) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Entorno local para cargar los scripts sourceados
    # Lo manejamos como un reactiveVal para poder "resetearlo" si cambia la carpeta
    local_env <- reactiveVal(new.env(parent = .GlobalEnv))

    # 2. Manejo de la ruta (Reactiva)
    path_r <- reactive({
      # Si es función (reactivo del padre), lo ejecutamos; si no, lo usamos tal cual
      p <- if (is.function(folder_path_tool_script)) folder_path_tool_script() else folder_path_tool_script
      req(p, p != "") # Validamos que la ruta exista y no sea vacía
      return(p)
    })

    # 3. OBSERVER MAESTRO: Carga el archivo cuando cambia la ruta
    observe({
      base_p <- path_r()
      # Construcción de la ruta específica al archivo de teoría
      target_file <- file.path(base_p, "f03_soft_opts", "f01_theory", "f03_prod", "mod_special_theory.R")

      if (file.exists(target_file)) {
        message("--- [Collector] Cargando teoría desde: ", target_file)

        # Creamos un entorno nuevo para esta carga específica
        new_env <- new.env(parent = .GlobalEnv)

        tryCatch({
          source(target_file, local = new_env)

          # Si el source fue exitoso, actualizamos nuestro reactiveVal
          local_env(new_env)

          # Inicializamos el servidor del sub-módulo cargado
          if (!is.null(new_env$mod_special_theory_server)) {
            new_env$mod_special_theory_server("sub_theory")
            message("--- [Collector] Server 'sub_theory' inicializado.")
          }

        }, error = function(e) {
          warning("--- [Collector] Error al cargar el script: ", e$message)
        })
      } else {
        warning("--- [Collector] El archivo no existe: ", target_file)
      }
    })

    # 4. Renderizado de la UI
    # Este renderUI se disparará automáticamente cuando local_env() cambie
    output$placeholder <- renderUI({
      env <- local_env()
      req(env$mod_special_theory_ui) # Solo renderiza si la función existe en el entorno

      tagList(
        env$mod_special_theory_ui(ns("sub_theory"))
      )
    })

    message("--- [RScience] Módulo Collector listo para recibir rutas en ID: ", id)
  })
}
