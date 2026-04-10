library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================

mod_04_00_settings_DEBUG_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("panel_debug_externo"))
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

mod_04_00_settings_server <- function(id, df_input, folder_path_tool_script, show_debug = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ui_debug_layout <- function(ns, prefix = "") {
      id_colector  <- ns(paste0(prefix, "render_json_colector"))
      id_submodulo <- ns(paste0(prefix, "render_json_submodulo"))

      div(style = "margin-top: 20px; padding: 15px; background: #1a1a1a; border-radius: 8px; border: 1px solid #333;",
          h4(icon("terminal"), "RScience Debug Console", style = "color: #00bc8c; font-family: monospace;"),
          fluidRow(
            column(6, tags$b("Colector Meta", style="color:#00d4ff;"), listviewer::jsoneditOutput(id_colector, height = "300px")),
            column(6, tags$b("Sub-Módulo Return", style="color:#00bc8c;"), listviewer::jsoneditOutput(id_submodulo, height = "300px"))
          )
      )
    }

    # Registro de recursos para logos y assets
    www_folder <- system.file("www", package = "Rscience2027")
    if (www_folder == "") www_folder <- "www"
    try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

    # --- 1. ESTADOS ---
    local_env <- reactiveVal(NULL)
    rv <- reactiveValues(ready = FALSE, sub_data = NULL)

    # --- 2. GESTIÓN DE RUTA E IDENTIFICACIÓN DEL TARGET ---
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

        # Aislamos las funciones en un entorno hijo del actual
        new_env <- new.env(parent = environment())

        tryCatch({
          # Definimos la carpeta de búsqueda (f03_prod)
          special_folder <- dirname(info$target_file)

          # Buscamos todos los .R (incluyendo sub_module/)
          all_r_files <- list.files(
            path = special_folder,
            pattern = "\\.R$",
            full.names = TRUE,
            recursive = TRUE
          )
          all_r_files <- all_r_files[!grepl("^demo_", basename(all_r_files))]

          print(all_r_files)
          # Normalizamos para no cargar dos veces el principal
          target_norm <- normalizePath(info$target_file, mustWork = FALSE)
          others <- all_r_files[normalizePath(all_r_files, mustWork = FALSE) != target_norm]

          # Carga de dependencias
          lapply(others, function(f) {
            source(f, local = new_env)
          })

          # Carga del Orquestador Hijo
          source(info$target_file, local = new_env)
          local_env(new_env)

          # Ejecución del servidor hijo
          if (exists("mod_special_settings_server", envir = new_env)) {
            func_server <- get("mod_special_settings_server", envir = new_env)

            rv$sub_data <- func_server(
              id = "sub_theory",
              df_input = df_input,
              folder_path_tool_script = folder_path_tool_script
            )
            rv$ready <- TRUE
          } else {
            stop("No se encontró la función mod_special_settings_server.")
          }

        }, error = function(e) {
          rv$ready <- FALSE
          warning("--- [RScience Collector] Error Crítico: ", e$message)
        })
      } else {
        rv$ready <- FALSE
        rv$sub_data <- NULL
      }
    }, ignoreInit = FALSE)

    # --- 4. RENDER UI ---
    output$placeholder_dinamico <- renderUI({
      req(rv$ready)
      env <- local_env()
      if(exists("mod_special_settings_ui", envir = env)) {
        func_ui <- get("mod_special_settings_ui", envir = env)
        func_ui(ns("sub_theory"))
      } else {
        div(class="alert alert-danger", "Error: mod_special_settings_ui no encontrada.")
      }
    })

    # --- 5. LÓGICA DE ESPERA ---
    output$ui_waiting_state <- renderUI({
      tagList(
        tags$style(HTML("@keyframes bounceIn {
          0% { opacity: 0; transform: scale3d(.3, .3, .3); }
          50% { opacity: 1; transform: scale3d(1.05, 1.05, 1.05); }
          100% { transform: scale3d(1, 1, 1); }
        } .rs-logo-animated { animation: bounceIn 0.8s ease-out; }")),
        div(style = "padding: 60px 20px; text-align: center; border: 2px dashed #333; border-radius: 20px; background: #0f171e;",
            div(class = "rs-logo-animated", img(src = "WWW-FOLDER/Rscience_logo_sticker.png", style = "width: 120px;")),
            h3("RScience Framework", style = "color: #00bc8c; margin-top: 20px; font-weight: 700;"),
            p("Waiting for directory selection...", style = "color: #888;")
        )
      )
    })

    # --- 6. COORDINADOR DE VISTAS (SWITCHER) ---
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

    # --- 7. DEBUG LOGIC ---
    get_debug_data <- function() {
      req(rv$ready)
      res_val <- if(is.reactive(rv$sub_data)) rv$sub_data() else rv$sub_data
      list(
        env_objects = ls(local_env()),
        sub_return_type = if(is.reactive(rv$sub_data)) "Reactive" else "Static",
        sub_content = res_val
      )
    }

    output$render_json_colector <- listviewer::renderJsonedit({ listviewer::jsonedit(internal_meta()) })
    output$render_json_submodulo <- listviewer::renderJsonedit({ listviewer::jsonedit(get_debug_data()) })
    output$ext_render_json_colector <- listviewer::renderJsonedit({ listviewer::jsonedit(internal_meta()) })
    output$ext_render_json_submodulo <- listviewer::renderJsonedit({ listviewer::jsonedit(get_debug_data()) })

    output$debug_internal <- renderUI({ req(show_debug); ui_debug_layout(ns) })
    output$panel_debug_externo <- renderUI({
      div(style = "border: 1px solid #00d4ff; padding: 10px; border-radius: 10px; background: #0b1218;",
          h5(icon("external-link-alt"), "EXTERNAL SYSTEM MONITOR", style="color: #00d4ff; font-size: 0.8rem;"),
          ui_debug_layout(ns, prefix = "ext_"))
    })

    # --- 8. RETORNO UNIFICADO ---
    return(reactive({
      req(rv$ready)
      if(is.reactive(rv$sub_data)) rv$sub_data() else rv$sub_data
    }))
  })
}
