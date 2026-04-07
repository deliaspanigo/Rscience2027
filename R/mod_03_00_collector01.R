library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================

mod_03_00_collector01_ui_01_theory <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_theory"))
}

mod_03_00_collector01_ui_02_bibliography <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_bibliography"))
}

mod_03_00_collector01_ui_03_cite <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_cite"))
}

# ==============================================================================
# MÓDULO SERVER: CARGA LOCAL Y RENDERIZADO
# ==============================================================================

mod_03_00_collector01_server_UNIVERSAL <- function(id, folder_path_tool_script) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Entorno local aislado para evitar colisiones en el Global Environment
    local_env <- new.env(parent = environment())

    # Manejo flexible de la ruta (acepta String o Reactive)
    path_r <- reactive({
      if (is.function(folder_path_tool_script)) folder_path_tool_script() else folder_path_tool_script
    })

    observe({
      # 1. Validación de entrada
      req(path_r())
      base_p <- path_r()

      print(base_p)
      # Definición de rutas internas según estructura RScience
      paths <- list(
        theory       = file.path(base_p, "f03_soft_opts", "f01_theory",       "f03_prod", "mod_special_theory.R"),
        bibliography = file.path(base_p, "f03_soft_opts", "f02_bibliography", "f03_prod", "mod_special_bibliography.R"),
        cite         = file.path(base_p, "f03_soft_opts", "f03_cite",         "f03_prod", "mod_special_cite.R")
      )

      # 2. Carga local de los scripts (Source)
      # Se cargan las funciones mod_special_..._ui y mod_special_..._server en local_env
      lapply(names(paths), function(name) {
        p <- paths[[name]]
        if (file.exists(p)) {
          tryCatch({
            source(file = p, local = local_env)
          }, error = function(e) {
            warning("Error al cargar script ", name, ": ", e$message)
          })
        } else {
          warning("Archivo no encontrado: ", p)
        }
      })

      # 3. Renderizado de las UIs (se ejecuta después del source)
      # Teoría
      output$placeholder_theory <- renderUI({
        req(local_env$mod_special_theory_ui)
        local_env$mod_special_theory_ui(ns("sub_theory"))
      })

      # Bibliografía
      output$placeholder_bibliography <- renderUI({
        req(local_env$mod_special_bibliography_ui)
        local_env$mod_special_bibliography_ui(ns("sub_bibliography"))
      })

      # Citas
      output$placeholder_cite <- renderUI({
        req(local_env$mod_special_cite_ui)
        local_env$mod_special_cite_ui(ns("sub_cite"))
      })

      # 4. Inicialización de Servidores de los sub-módulos
      # Llamamos a los servidores usando el entorno donde se cargaron
      try({
        if (!is.null(local_env$mod_special_theory_server))
          local_env$mod_special_theory_server("sub_theory")

        if (!is.null(local_env$mod_special_bibliography_server))
          local_env$mod_special_bibliography_server("sub_bibliography")

        if (!is.null(local_env$mod_special_cite_server))
          local_env$mod_special_cite_server("sub_cite")
      }, silent = FALSE)

      message("--- [RScience] Módulos Collector inicializados correctamente en: ", id)
    })

  })
}
