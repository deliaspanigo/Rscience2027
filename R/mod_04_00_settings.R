library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================

mod_04_00_settings_DEBUG_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("debug_externo"))
}

mod_04_00_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_hidden(
      id = ns("theory_switcher"),
      nav_panel_hidden(
        value = "state_waiting",
        uiOutput(ns("ui_waiting_state"))
      ),
      nav_panel_hidden(
        value = "state_loading",
        div(style = "padding: 80px; text-align: center;",
            icon("sync", class = "fa-spin fa-3x", style = "color: #00d4ff;"),
            h4("Sincronizando Componentes...", style = "color: #00d4ff;"))
      ),
      nav_panel_hidden(
        value = "state_ready",
        uiOutput(ns("placeholder_dinamico"))
      )
    ),
    uiOutput(ns("debug_internal"))
  )
}

# ==============================================================================
# LÓGICA DE APOYO: DEBUG LAYOUT
# ==============================================================================



# ==============================================================================
# MÓDULO SERVER: COLECTOR Y ORQUESTADOR RECURSIVO
# ==============================================================================
library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

# ==============================================================================
# MÓDULO SERVER: COLECTOR Y ORQUESTADOR RECURSIVO
# ==============================================================================

mod_04_00_settings_server <- function(id, df_input, folder_path_tool_script, show_debug = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)


    # Lógica de layout para el debug interno
    ui_debug_layout <- function(ns, prefix = "") {
      id_colector  <- ns(paste0(prefix, "render_json_colector"))
      id_submodulo <- ns(paste0(prefix, "render_json_submodulo"))
      div(style = "margin-top: 20px; padding: 15px; background: #0b1218; border-radius: 8px; border: 1px solid #1a262f;",
          h6(icon("terminal"), "RScience Debug Console", style = "color: #00bc8c;"),
          fluidRow(
            column(6, tags$small("Colector Meta", style="color:#00d4ff;"), listviewer::jsoneditOutput(id_colector, height = "250px")),
            column(6, tags$small("Sub-Módulo Return", style="color:#00bc8c;"), listviewer::jsoneditOutput(id_submodulo, height = "250px"))
          )
      )
    }

    # --- 1. ESTADOS ---
    local_env <- reactiveVal(NULL)
    rv <- reactiveValues(ready = FALSE, sub_data = NULL)

    # --- 2. GESTIÓN DE RUTA ---
    internal_meta <- reactive({
      p <- if (is.function(folder_path_tool_script)) folder_path_tool_script() else folder_path_tool_script
      if (is.null(p) || p == "") return(list(status = "WAITING_PATH", exists = FALSE))

      target <- file.path(p, "f01_shiny_show", "p02_settings", "f03_prod", "mod_special_settings.R")
      list(
        status      = "PATH_PROVIDED",
        base_path   = p,
        target_file = target,
        exists      = file.exists(target),
        timestamp   = Sys.time()
      )
    })

    # --- 3. CARGA DINÁMICA RECURSIVA ---
    observeEvent(internal_meta(), {
      info <- internal_meta()
      if (info$status == "PATH_PROVIDED" && isTRUE(info$exists)) {
        rv$ready <- FALSE

        # IMPORTANTE: El entorno padre debe ser .GlobalEnv para que Selectize
        # encuentre las funciones de ayuda y librerías.
        new_env <- new.env(parent = .GlobalEnv)

        tryCatch({
          special_folder <- dirname(info$target_file)
          all_r_files <- list.files(path = special_folder, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
          all_r_files <- all_r_files[!grepl("^demo_", basename(all_r_files))]

          # Carga de archivos (dependencias primero)
          target_norm <- normalizePath(info$target_file, mustWork = FALSE)
          others <- all_r_files[normalizePath(all_r_files, mustWork = FALSE) != target_norm]

          lapply(others, function(f) source(f, local = new_env))
          source(info$target_file, local = new_env)

          local_env(new_env)

          # Ejecución del servidor hijo dentro de su entorno
          if (exists("mod_special_settings_server", envir = new_env)) {
            func_server <- get("mod_special_settings_server", envir = new_env)

            # Pasamos df_input tal cual (como reactivo)
            rv$sub_data <- func_server(
              id = "sub_theory",
              df_input = df_input,
              folder_path_tool_script = folder_path_tool_script
            )
            rv$ready <- TRUE
          }
        }, error = function(e) {
          rv$ready <- FALSE
          message("Error en carga dinámica: ", e$message)
        })
      }
    })

    # --- 4. RENDER UI ---
    output$placeholder_dinamico <- renderUI({
      req(rv$ready)
      env <- local_env()
      if(exists("mod_special_settings_ui", envir = env)) {
        func_ui <- get("mod_special_settings_ui", envir = env)
        func_ui(ns("sub_theory"))
      }
    })

    # --- 5. SWITCHER DE VISTAS ---
    observe({
      info <- internal_meta()
      if (info$status == "WAITING_PATH") {
        nav_select("theory_switcher", "state_waiting")
      } else if (!isTRUE(rv$ready)) {
        nav_select("theory_switcher", "state_loading")
      } else {
        nav_select("theory_switcher", "state_ready")
      }
    })

    # --- 6. DEBUG LOGIC ---
    get_debug_data <- reactive({
      req(rv$ready)
      if(is.reactive(rv$sub_data)) rv$sub_data() else rv$sub_data
    })

    output$render_json_colector <- listviewer::renderJsonedit({ listviewer::jsonedit(internal_meta()) })
    output$render_json_submodulo <- listviewer::renderJsonedit({ listviewer::jsonedit(get_debug_data()) })
    output$debug_internal <- renderUI({ req(internal_show_debug); ui_debug_layout(ns) })


    ##########

    output$render_json_colector_externo <- listviewer::renderJsonedit({ listviewer::jsonedit(internal_meta()) })
    output$render_json_submodulo_externo <- listviewer::renderJsonedit({ listviewer::jsonedit(get_debug_data()) })
    output$debug_externo <- renderUI({  ui_debug_layout(ns) })


    return(reactive({
      req(rv$ready)
      if(is.reactive(rv$sub_data)) rv$sub_data() else rv$sub_data
    }))
  })
}
