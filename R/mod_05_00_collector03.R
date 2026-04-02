library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

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

# NUEVO: MÓDULO UI PARA DEBUG
mod_05_00_collector03_ui_debug <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "p-3 border rounded bg-light",
        tags$h5(icon("bug"), " RScience Collector Debug"),
        hr(),
        uiOutput(ns("debug_status_ui")), # Resumen visual rápido
        br(),
        listviewer::jsoneditOutput(ns("debug_json"), height = "400px") # Explorador completo
    )
  )
}

# ==============================================================================
# MÓDULO SERVER: CARGA LOCAL Y RENDERIZADO
# ==============================================================================

mod_05_00_collector03_server_UNIVERSAL <- function(id, folder_path_tool_script) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Entorno local aislado
    local_env <- new.env(parent = environment())

    # Manejo flexible de la ruta
    path_r <- reactive({
      if (is.function(folder_path_tool_script)) folder_path_tool_script() else folder_path_tool_script
    })

    # --- 1. LÓGICA DE COPIA TEMPORAL Y OBJETO DE RETORNO ---
    # --- 1. OBJETO REACTIVO PRINCIPAL (Estructura Local vs Temporal) ---
    res_bundle <- reactive({
      orig_p <- path_r()

      # Verificación de la carpeta LOCAL
      local_exists <- !is.null(orig_p) && dir.exists(orig_p)

      # Setup de rutas TEMPORALES (solo si local existe)
      ts <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
      folder_name <- if(local_exists) basename(orig_p) else "unknown"
      tmp_base <- file.path(tempdir(), paste0("Rscience_", ts))
      tmp_final <- file.path(tmp_base, folder_name)

      success_copy <- FALSE
      if (local_exists) {
        if (!dir.exists(tmp_base)) dir.create(tmp_base, recursive = TRUE)
        success_copy <- file.copy(from = orig_p, to = tmp_base, recursive = TRUE)
      }

      # Verificación física de la carpeta TEMPORAL
      temp_exists <- dir.exists(tmp_final)

      # OBJETO DE RETORNO ESTRUCTURADO
      list(
        module_id = id,
        timestamp = ts,
        # Info de la carpeta de ORIGEN
        local = list(
          path   = orig_p,
          exists = local_exists,
          name   = folder_name
        ),
        # Info de la carpeta de DESTINO
        temporal = list(
          path_root  = tmp_base,
          path_final = tmp_final,
          exists     = temp_exists,
          copy_ok    = success_copy
        ),
        # Estado global para validaciones rápidas
        is_ready = (local_exists && temp_exists)
      )
    })


    folder_temp_path <- reactive({
      req(res_bundle())
      res <- res_bundle()
      res$temporal$path_final

    })

    # --- 2. RENDERIZADO DEL DEBUG UI ---
    output$debug_status_ui <- renderUI({
      res <- res_bundle()
      div(
        span(tags$b("Status: "),
             span(style = if(res$is_ready) "color:green" else "color:red", res$status)),
        br(),
        span(tags$b("Target: "), code(res$folder_name)),
        br(),
        span(tags$b("Temp Path: "), code(res$path_temp))
      )
    })

    output$debug_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(res_bundle(), mode = "view", name = paste0("Bundle_", id))
    })

    # --- 3. CARGA DINÁMICA DE SUB-MÓDULOS ---
    observe({
      req(path_r())
      base_p <- path_r()

      paths <- list(
        shiny_output   = file.path(base_p, "f03_soft_opts", "f05_shiny_output",  "f03_prod", "mod_special_shiny_output.R"),
        asa            = file.path(base_p, "f03_soft_opts", "f06_asa",           "f03_prod", "mod_special_asa.R"),
        script_comment = file.path(base_p, "f03_soft_opts", "f07_script_comment","f03_prod", "mod_special_script_comment.R"),
        script_only    = file.path(base_p, "f03_soft_opts", "f08_script_only",   "f03_prod", "mod_special_script_only.R"),
        reporting      = file.path(base_p, "f03_soft_opts", "f09_reporting",     "f03_prod", "mod_special_reporting.R")
      )

      lapply(names(paths), function(name) {
        p <- paths[[name]]
        if (file.exists(p)) {
          tryCatch({ source(file = p, local = local_env) },
                   error = function(e) { warning("Error en script ", name, ": ", e$message) })
        }
      })

      # Renders de placeholders
      output$placeholder_shiny_output <- renderUI({ req(local_env$mod_special_shiny_output_ui); local_env$mod_special_shiny_output_ui(ns("sub_shiny_output")) })
      output$placeholder_asa <- renderUI({ req(local_env$mod_special_asa_ui); local_env$mod_special_asa_ui(ns("sub_asa")) })

      output$placeholder_script_comment <- renderUI({ req(local_env$mod_special_script_comment_ui); local_env$mod_special_script_comment_ui(ns("sub_script_comment")) })

      output$placeholder_script_only <- renderUI({ req(local_env$mod_special_script_only_ui); local_env$mod_special_script_only_ui(ns("sub_script_only")) })
      output$placeholder_reporting <- renderUI({ req(local_env$mod_special_reporting_ui); local_env$mod_special_reporting_ui(ns("sub_reporting")) })

      # Inicialización de servidores
      try({
        if (!is.null(local_env$mod_special_shiny_output_server)) local_env$mod_special_shiny_output_server("sub_shiny_output")
        if (!is.null(local_env$mod_special_asa_server)) local_env$mod_special_asa_server("sub_asa")
        if (!is.null(local_env$mod_special_script_comment_server)) local_env$mod_special_script_comment_server(id = "sub_script_comment", folder_temp_path = folder_temp_path)
        if (!is.null(local_env$mod_special_script_only_server)) local_env$mod_special_script_only_server(id = "sub_script_only", folder_temp_path = folder_temp_path)
        if (!is.null(local_env$mod_special_reporting_server)) local_env$mod_special_reporting_server("sub_reporting")
      }, silent = FALSE)

      message("--- [RScience] Módulos Collector03 cargados en temporal para: ", id)
    })

    # RETORNO DEL OBJETO COMPLETO
    return(res_bundle)
  })
}
