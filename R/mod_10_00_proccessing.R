library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI
# ==============================================================================

mod_10_00_proccessing_DEBUG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("debug_external"))
  )
}

mod_10_00_proccessing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_hidden(
      id = ns("proccessing_switcher"),
      nav_panel_hidden(
        value = "state_waiting",
        uiOutput(ns("ui_waiting_state"))
      ),
      nav_panel_hidden(
        value = "state_loading",
        div(style = "padding: 80px; text-align: center;",
            icon("sync", class = "fa-spin fa-3x", style = "color: #00d4ff;"),
            h4("Sincronizando...", style = "color: #00d4ff;"))
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
# MÓDULO SERVER
# ==============================================================================

library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI
# ==============================================================================

mod_10_00_proccessing_DEBUG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("debug_external"))
  )
}

mod_10_00_proccessing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_hidden(
      id = ns("proccessing_switcher"),
      nav_panel_hidden(
        value = "state_waiting",
        uiOutput(ns("ui_waiting_state"))
      ),
      nav_panel_hidden(
        value = "state_loading",
        div(style = "padding: 80px; text-align: center;",
            icon("sync", class = "fa-spin fa-3x", style = "color: #00d4ff;"),
            h4("Sincronizando...", style = "color: #00d4ff;"))
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
# MÓDULO SERVER (CORREGIDO)
# ==============================================================================

mod_10_00_proccessing_server <- function(id,
                                         module_proccessing_file_path,
                                         local_folder_tool_script,
                                         temp_folder_tool_script,
                                         list_quarto_replacement,
                                         show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message(">>> [PADRE] Módulo inicializado en ", Sys.time())

    # --- 1. CONFIGURACIÓN DE RECURSOS ---
    www_folder <- system.file("www", package = "Rscience2027")
    if (www_folder == "") www_folder <- "www"
    try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

    # --- 2. REACTIVOS DE ENTRADA Y ESTADOS ---
    internal_path  <- reactive({
      if (is.function(module_proccessing_file_path)) module_proccessing_file_path() else module_proccessing_file_path
    })
    internal_local <- reactive({
      if (is.function(local_folder_tool_script)) local_folder_tool_script() else local_folder_tool_script
    })
    internal_temp  <- reactive({
      if (is.function(temp_folder_tool_script)) temp_folder_tool_script() else temp_folder_tool_script
    })
    internal_list_quarto_replacement <- reactive({
      if (is.function(list_quarto_replacement)) list_quarto_replacement() else list_quarto_replacement
    })

    local_env  <- reactiveVal(NULL)
    data_store <- reactiveValues(
      details = "*** RScience - Module Proccessing ***",
      is_done = FALSE,
      error_msg = NULL
    )

    rv <- reactiveValues(
      ready = FALSE,
      special_module = NULL,
      module_created = FALSE  # Flag para crear submódulo solo una vez
    )

    # --- 3. METADATOS Y VALIDACIÓN ---
    internal_meta <- reactive({
      p <- internal_path()
      if (is.null(p) || is.na(p) || p == "") {
        return(list(status = "WAITING_PATH", exists = FALSE))
      }
      list(status = "PATH_PROVIDED", target_file = p, exists = file.exists(p))
    })

    # --- 4. CARGA DEL ARCHIVO (SOURCE) - SOLO UNA VEZ ---
    observeEvent(internal_meta(), {
      info <- internal_meta()
      message(">>> [PADRE] internal_meta: ", info$status)

      if (info$status == "PATH_PROVIDED" && isTRUE(info$exists)) {
        new_env <- new.env(parent = .GlobalEnv)
        tryCatch({
          message(">>> [PADRE] Cargando source desde: ", info$target_file)
          source(info$target_file, local = new_env)
          local_env(new_env)
          rv$ready <- TRUE
          message(">>> [PADRE] ✅ Source cargado correctamente")
        }, error = function(e) {
          rv$ready <- FALSE
          message(">>> [PADRE] ❌ Error en source: ", e$message)
          warning("--- [Collector] Error en source: ", e$message)
        })
      } else {
        rv$ready <- FALSE
        message(">>> [PADRE] ⚠️ Path no disponible o archivo no existe")
      }
    }, ignoreInit = FALSE)

    # --- 5. EJECUCIÓN DEL SUBMÓDULO - SOLO UNA VEZ ---
    observeEvent(rv$ready, {
      if (rv$ready && !rv$module_created) {
        env <- local_env()
        if (!is.null(env) && !is.null(env$mod_special_proccessing_server)) {
          message(">>> [PADRE] 🚀 Creando submódulo (SOLO UNA VEZ)")

          rv$special_module <- env$mod_special_proccessing_server(
            id = "sub_proc",
            local_folder_tool_script = internal_local,
            temp_folder_tool_script = internal_temp,
            list_quarto_replacement = internal_list_quarto_replacement,
            show_debug = show_debug
          )

          rv$module_created <- TRUE
          message(">>> [PADRE] ✅ Submódulo creado exitosamente")
        } else {
          message(">>> [PADRE] ⚠️ No se encontró mod_special_proccessing_server en el entorno")
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    # --- 6. AGREGADOR DE SALIDA (EL CORAZÓN) ---
    the_output <- reactive({
      out <- reactiveValuesToList(data_store)

      # Si el submódulo está cargado y tiene un valor
      if (rv$module_created && !is.null(rv$special_module)) {
        # Evaluamos el reactivo del hijo
        child_data <- if(is.function(rv$special_module)) rv$special_module() else rv$special_module

        # Guardamos el valor
        out$module_special_is_done <- child_data

        # Sincronizamos is_done principal con el del hijo
        if (isTRUE(child_data)) {
          out$is_done <- TRUE
          message(">>> [PADRE] 🎉 Pipeline hijo completado")
        }
      }

      return(out)
    })

    # --- 7. RENDERS UI ---

    # Estado Espera con Logo Animado
    output$ui_waiting_state <- renderUI({
      v_css_animations <- tags$style(HTML("
        @keyframes bounceIn {
          0%, 20%, 40%, 60%, 80%, 100% { transition-timing-function: cubic-bezier(0.215, 0.610, 0.355, 1.000); }
          0% { opacity: 0; transform: scale3d(.3, .3, .3); }
          20% { transform: scale3d(1.1, 1.1, 1.1); }
          40% { transform: scale3d(.9, .9, .9); }
          60% { opacity: 1; transform: scale3d(1.03, 1.03, 1.03); }
          80% { transform: scale3d(.97, .97, .97); }
          100% { opacity: 1; transform: scale3d(1, 1, 1); }
        }
        @keyframes slowRotate { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }
        .rs-logo-animated {
          animation: bounceIn 0.8s ease-out, slowRotate 10s linear infinite 0.8s;
          transform-origin: center;
        }
      "))

      tagList(
        v_css_animations,
        div(style = "padding: 80px 20px; text-align: center; border: 2px dashed #444; border-radius: 20px; background: #1a1a1a; overflow: hidden;",
            icon("toolbox", style = "font-size: 3rem; color: #375a7f; margin-bottom: 10px; opacity: 0.5;"),
            div(class = "text-center rs-logo-animated", style = "padding: 10px 0 10px 0;",
                img(src = "WWW-FOLDER/Rscience_logo_sticker.png", style = "width: 150px; filter: drop-shadow(0 0 10px rgba(0,212,255,0.3));")
            ),
            h3("Action Required", style = "color: #00bc8c; margin-top: 15px; font-weight: 600;"),
            hr(style = "width: 30%; margin: 15px auto; border-color: #333;"),
            p("Complete the selection in the", tags$b("'Tools'", style="color: #00d4ff;"),
              "section to unlock the", tags$b("Theory", style="color: #ffffff;"), "content.",
              style = "color: #aaaaaa; font-size: 1.05rem; max-width: 450px; margin: 0 auto;")
        )
      )
    })

    # Marcador de posición para UI del hijo
    output$placeholder_dinamico <- renderUI({
      req(rv$module_created)  # Cambiado: esperar a que el módulo esté creado, no solo ready
      env <- local_env()
      req(env, env$mod_special_proccessing_ui)
      message(">>> [PADRE] Renderizando UI del submódulo")
      env$mod_special_proccessing_ui(ns("sub_proc"))
    })

    # --- 8. SISTEMA DE DEBUG ---

    # Switcher de navegación
    observe({
      info <- internal_meta()
      state <- if (info$status == "WAITING_PATH") {
        "state_waiting"
      } else if (!rv$ready) {
        "state_loading"
      } else {
        "state_ready"
      }
      nav_select("proccessing_switcher", state)
    })

    # Render JSON Interno y Externo
    output$json_int <- listviewer::renderJsonedit({
      listviewer::jsonedit(the_output())
    })
    output$json_ext <- listviewer::renderJsonedit({
      listviewer::jsonedit(the_output())
    })

    output$debug_internal <- renderUI({
      req(show_debug)
      div(style = "margin-top: 20px; padding: 15px; background: #1a1a1a; border-radius: 8px; border: 1px solid #333;",
          h4(icon("terminal"), "Internal Debug", style = "color: #00bc8c; font-size: 0.9rem;"),
          listviewer::jsoneditOutput(ns("json_int"), height = "250px")
      )
    })

    output$debug_external <- renderUI({
      req(show_debug)
      div(style = "margin-top: 20px; padding: 15px; background: #1a1a1a; border-radius: 8px; border: 1px solid #333;",
          h4(icon("terminal"), "External Debug", style = "color: #00bc8c; font-size: 0.9rem;"),
          listviewer::jsoneditOutput(ns("json_ext"), height = "250px")
      )
    })

    return(the_output)
  })
}
