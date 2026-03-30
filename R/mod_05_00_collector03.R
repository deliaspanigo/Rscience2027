library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================

mod_05_00_collector03_ui_01_shiny_output <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_shiny_output"))
}

mod_05_00_collector03_ui_02_asa <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_asa"))
}

mod_05_00_collector03_ui_03_script_comment <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_script_comment"))
}

mod_05_00_collector03_ui_04_script_only <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_script_only"))
}

mod_05_00_collector03_ui_05_reporting <- function(id) {
  ns <- NS(id)
  uiOutput(ns("placeholder_reporting"))
}

# ==============================================================================
# MÓDULO SERVER: CARGA LOCAL Y RENDERIZADO
# ==============================================================================

mod_05_00_collector03_server_UNIVERSAL <- function(id, folder_path_tool_script) {
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

      # Definición de rutas internas según estructura RScience
      paths <- list(
        shiny_output   = file.path(base_p, "f03_soft_opts", "f05_shiny_output",  "f03_prod", "mod_special_shiny_output.R"),
        asa            = file.path(base_p, "f03_soft_opts", "f06_asa",           "f03_prod", "mod_special_asa.R"),
        script_comment = file.path(base_p, "f03_soft_opts", "f07_script_comment","f03_prod", "mod_special_script_comment.R"),
        script_only    = file.path(base_p, "f03_soft_opts", "f08_script_only",   "f03_prod", "mod_special_script_only.R"),
        reporting      = file.path(base_p, "f03_soft_opts", "f09_reporting",     "f03_prod", "mod_special_reporting.R")

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
      # 3.01. shiny_output
      output$placeholder_shiny_output <- renderUI({
        req(local_env$mod_special_shiny_output_ui)
        local_env$mod_special_shiny_output_ui(ns("sub_shiny_output"))
      })

      # 3.02. asa
      output$placeholder_asa <- renderUI({
        req(local_env$mod_special_asa_ui)
        local_env$mod_special_asa_ui(ns("sub_asa"))
      })



      # 3.03. script_comment
      output$placeholder_script_comment <- renderUI({
        req(local_env$mod_special_script_comment_ui)
        local_env$mod_special_script_comment_ui(ns("sub_script_comment"))
      })


      # 3.04. script_only
      output$placeholder_script_only <- renderUI({
        req(local_env$mod_special_script_only_ui)
        local_env$mod_special_script_only_ui(ns("sub_script_only"))
      })

      # 3.05. reporting
      output$placeholder_reporting <- renderUI({
        req(local_env$mod_special_reporting_ui)
        local_env$mod_special_reporting_ui(ns("sub_reporting"))
      })

      # 4. Inicialización de Servidores de los sub-módulos
      # Llamamos a los servidores usando el entorno donde se cargaron
      try({
        if (!is.null(local_env$mod_special_shiny_output_server))
          local_env$mod_special_shiny_output_server("sub_shiny_output")

        if (!is.null(local_env$mod_special_asa_server))
          local_env$mod_special_asa_server("sub_asa")

        if (!is.null(local_env$mod_special_script_comment_server))
          local_env$mod_special_script_comment_server("sub_script_comment")

        if (!is.null(local_env$mod_special_script_only_server))
          local_env$mod_special_script_only_server("sub_script_only")

        if (!is.null(local_env$mod_special_reporting_server))
          local_env$mod_special_reporting_server("sub_reporting")

      }, silent = FALSE)

      message("--- [RScience] Módulos Collector03 inicializados correctamente en: ", id)
    })

  })
}
