library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================

mod_03_C_cite_DEBUG_ui <- function(id) {
  ns <- NS(id)
  # Este uiOutput debe coincidir con el ID del server
  uiOutput(ns("panel_debug_externo"))
}

mod_03_C_cite_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_hidden(
      id = ns("cite_switcher"),

      # PANEL 1: Ahora es un render dinámico para facilitar cambios estéticos
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
# MÓDULO SERVER: CARGA LOCAL Y RENDERIZADO
# ==============================================================================

# ==============================================================================
# MÓDULO SERVER: COLECTOR Y ORQUESTADOR DE TEORÍA
# ==============================================================================


# Colócala fuera del mod_03_C_cite_server
ui_debug_layout_cite <- function(ns, prefix = "") {
  # Creamos IDs únicos basados en el prefijo (ej: "ext_render_json_colector")
  id_colector <- ns(paste0(prefix, "render_json_colector"))
  id_submodulo <- ns(paste0(prefix, "render_json_submodulo"))

  div(style = "margin-top: 20px; padding: 15px; background: #1a1a1a; border-radius: 8px;",
      h4(icon("terminal"), "RScience Debug Console", style = "color: #00bc8c;"),
      fluidRow(
        column(6, tags$b("Colector"), listviewer::jsoneditOutput(id_colector, height = "300px")),
        column(6, tags$b("Sub-Módulo"), listviewer::jsoneditOutput(id_submodulo, height = "300px"))
      )
  )
}

# Función auxiliar para estandarizar la vista de Debug
# ==============================================================================
# MÓDULO SERVER: COLECTOR Y ORQUESTADOR DE TEORÍA
# ==============================================================================

mod_03_C_cite_server <- function(id, folder_path_tool_script, show_debug = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    www_folder <- system.file("www", package = "Rscience2027")
    if (www_folder == "") www_folder <- "www"
    try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)


    # --- 1. ESTADOS Y ENTORNOS (FALTABA ESTO) ---
    local_env <- reactiveVal(new.env(parent = .GlobalEnv))
    rv <- reactiveValues(ready = FALSE, sub_data = NULL)

    # --- 2. METADATOS (Manejo de Path Vacío) ---
    internal_meta <- reactive({
      p <- if (is.function(folder_path_tool_script)) folder_path_tool_script() else folder_path_tool_script

      if (is.null(p) || p == "") {
        return(list(status = "WAITING_PATH", exists = FALSE))
      }

      target <- file.path(p, "f01_shiny_show", "p01_03_cite", "f03_prod", "mod_special_cite.R")
      list(
        status      = "PATH_PROVIDED",
        base_path   = p,
        target_file = target,
        exists      = file.exists(target),
        timestamp   = Sys.time()
      )
    })

    # --- 3. LÓGICA DE CARGA DINÁMICA ---
    observeEvent(internal_meta(), {
      info <- internal_meta()

      if (info$status == "PATH_PROVIDED" && isTRUE(info$exists)) {
        rv$ready <- FALSE
        new_env <- new.env(parent = .GlobalEnv)

        tryCatch({
          source(info$target_file, local = new_env)
          local_env(new_env)

          if (!is.null(new_env$mod_special_cite_server)) {
            rv$sub_data <- new_env$mod_special_cite_server(id = "sub_cite")
          }
          rv$ready <- TRUE

        }, error = function(e) {
          rv$ready <- FALSE
          warning("--- [Collector] Error en source: ", e$message)
        })
      } else {
        rv$ready <- FALSE
        rv$sub_data <- NULL
      }
    }, ignoreInit = FALSE)

    # --- 4. RENDER: ESTADO DE ESPERA (WAITING) ---
    # --- RENDER: ESTADO DE ESPERA (WAITING) EN INGLÉS ---
    # --- RENDER: ESTADO DE ESPERA CON ANIMACIÓN DIVERTIDA ---
    output$ui_waiting_state <- renderUI({

      # 1. DEFINICIÓN DE LAS ANIMACIONES CSS (Keyframes)
      # Colocamos esto aquí para que solo se cargue cuando sea necesario.
      # 'bounceIn': Salto de entrada.
      # 'slowRotate': Giro infinito suave.
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

        @keyframes slowRotate {
          from { transform: rotate(0deg); }
          to { transform: rotate(360deg); }
        }

        .rs-logo-animated {
          /* Primero salta (0.8s), luego gira siempre (10s) */
          animation: bounceIn 0.8s ease-out, slowRotate 10s linear infinite 0.8s;
          transform-origin: center; /* Gira sobre su eje */
        }
      "))

      # 2. ESTRUCTURA HTML DE LA CORTESÍA
      tagList(
        v_css_animations, # Inyectamos los estilos

        div(style = "padding: 80px 20px; text-align: center; border: 2px dashed #444; border-radius: 20px; background: #1a1a1a; transition: all 0.5s ease; overflow: hidden;",

            # Icono superior
            icon("toolbox", style = "font-size: 3rem; color: #375a7f; margin-bottom: 10px; opacity: 0.5;"),

            # --- EL CONTENEDOR DEL LOGO ANIMADO ---
            div(class = "text-center rs-logo-animated",
                style = "padding: 10px 0 10px 0; will-change: transform;",
                # Asegúrate de que la ruta a la imagen sea correcta en tu www folder
                img(src = "WWW-FOLDER/Rscience_logo_sticker.png",
                    style = "width: 150px; filter: drop-shadow(0 0 10px rgba(0,212,255,0.3));")
            ),
            # --------------------------------------

            h3("Action Required",
               style = "color: #00bc8c; margin-top: 15px; font-family: 'Segoe UI'; font-weight: 600; letter-spacing: 1px;"),

            hr(style = "width: 30%; margin: 15px auto; border-color: #333;"),

            p("Complete the selection in the",
              tags$b("'Tools'", style="color: #00d4ff;"),
              "section to unlock the",
              tags$b("cite", style="color: #ffffff;"),
              "content.",
              style = "color: #aaaaaa; font-size: 1.05rem; line-height: 1.5; max-width: 450px; margin: 0 auto;")
        )
      )
    })

    # --- 5. RENDER: MÓDULO LISTO ---
    output$placeholder_dinamico <- renderUI({
      req(rv$ready)
      env <- local_env()
      req(env$mod_special_cite_ui)
      env$mod_special_cite_ui(ns("sub_cite"))
    })

    # --- 6. CONTROL DE NAVEGACIÓN (SWITCHER) ---
    observe({
      info <- internal_meta()
      if (info$status == "WAITING_PATH") {
        nav_select("cite_switcher", "state_waiting")
      } else if (!isTRUE(rv$ready)) {
        nav_select("cite_switcher", "state_loading")
      } else {
        nav_select("cite_switcher", "state_ready")
      }
    })

    # --- 7. SISTEMA DE DEBUG (DUPLICADO PARA INTERNO/EXTERNO) ---

    # Función de ayuda para la data de debug
    get_debug_data <- function() {
      req(rv$ready)
      res_val <- if(is.function(rv$sub_data)) rv$sub_data() else rv$sub_data
      list(
        env_objects = ls(local_env()),
        sub_return = if(is.function(res_val)) res_val() else res_val
      )
    }

    # Renders para debug interno
    output$render_json_colector <- listviewer::renderJsonedit({ listviewer::jsonedit(internal_meta()) })
    output$render_json_submodulo <- listviewer::renderJsonedit({ listviewer::jsonedit(get_debug_data()) })

    # Renders para debug externo
    output$ext_render_json_colector <- listviewer::renderJsonedit({ listviewer::jsonedit(internal_meta()) })
    output$ext_render_json_submodulo <- listviewer::renderJsonedit({ listviewer::jsonedit(get_debug_data()) })

    output$debug_internal <- renderUI({
      req(show_debug)
      ui_debug_layout_cite(ns, prefix = "")
    })

    output$panel_debug_externo <- renderUI({
      req(internal_meta())
      div(style = "border: 2px solid #00d4ff; padding: 10px; border-radius: 10px;",
          h5("MODO EXTERNO ACTIVADO", style="color: #00d4ff;"),
          ui_debug_layout_cite(ns, prefix = "ext_")
      )
    })

    # --- 8. RETORNO ---
    return(reactive({ rv$sub_data }))
  })
}
