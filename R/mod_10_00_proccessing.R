library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI: PLACEHOLDERS DINÁMICOS
# ==============================================================================



mod_10_00_proccessing_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_hidden(
      id = ns("proccessing_switcher"),

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




# Función auxiliar para estandarizar la vista de Debug
# ==============================================================================
# MÓDULO SERVER: COLECTOR Y ORQUESTADOR DE TEORÍA
# ==============================================================================

mod_10_00_proccessing_server <- function(id, module_proccessing_file_path, local_folder_tool_script, temp_folder_tool_script, list_settings, show_debug = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_module_proccessing_file_path <- reactive({ if (is.function(module_proccessing_file_path)) module_proccessing_file_path() else module_proccessing_file_path })
    internal_local_folder_tool_script <- reactive({ if (is.function(local_folder_tool_script)) local_folder_tool_script() else local_folder_tool_script })
    internal_temp_folder_tool_script <- reactive({ if (is.function(temp_folder_tool_script)) temp_folder_tool_script() else temp_folder_tool_script })

    # Colócala fuera del mod_10_00_proccessing_server
    ui_debug_layout_theory <- function(ns, prefix = "") {
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

    www_folder <- system.file("www", package = "Rscience2027")
    if (www_folder == "") www_folder <- "www"
    try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)


    # --- 1. ESTADOS Y ENTORNOS (FALTABA ESTO) ---
    local_env <- reactiveVal(new.env(parent = .GlobalEnv))
    rv <- reactiveValues(ready = FALSE, sub_data = NULL)

    # --- 2. METADATOS (Manejo de Path Vacío) ---
    # --- 2. METADATOS (Manejo de Path Vacío) ---
    internal_meta <- reactive({
      # Aquí está el problema: internal es un reactive()
      p <- if (is.function(internal_module_proccessing_file_path)) internal_module_proccessing_file_path() else internal_module_proccessing_file_path

      if (is.null(p) || is.na(p) || p == "") { # Agregué is.na(p) por seguridad
        return(list(status = "WAITING_PATH", exists = FALSE))
      }

      list(
        status      = "PATH_PROVIDED",
        target_file = p, # ¡Usa 'p', no 'module_proccessing_file_path'!
        exists      = file.exists(p), # Aquí fallaba porque le pasabas la función entera
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

          if (!is.null(new_env$mod_special_proccessing_server)) {
            rv$sub_data <- new_env$mod_special_proccessing_server(id = "sub_proc",
                                                                  local_folder_tool_script = internal_local_folder_tool_script(),
                                                                  temp_folder_tool_script = internal_temp_folder_tool_script())
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
              tags$b("Theory", style="color: #ffffff;"),
              "content.",
              style = "color: #aaaaaa; font-size: 1.05rem; line-height: 1.5; max-width: 450px; margin: 0 auto;")
        )
      )
    })

    # --- 5. RENDER: MÓDULO LISTO ---
    output$placeholder_dinamico <- renderUI({
      req(rv$ready)
      env <- local_env()
      req(env$mod_special_proccessing_ui)
      env$mod_special_proccessing_ui(ns("sub_proc"))
    })

    # --- 6. CONTROL DE NAVEGACIÓN (SWITCHER) ---
    observe({
      info <- internal_meta()
      if (info$status == "WAITING_PATH") {
        nav_select("proccessing_switcher", "state_waiting")
      } else if (!isTRUE(rv$ready)) {
        nav_select("proccessing_switcher", "state_loading")
      } else {
        nav_select("proccessing_switcher", "state_ready")
      }
    })

    # --- 7. SISTEMA DE DEBUG (DUPLICADO PARA INTERNO/EXTERNO) ---

    # --- 7. SISTEMA DE DEBUG SIMPLIFICADO ---

    # Solo mostramos metadatos básicos para evitar errores de serialización JSON
    debug_payload <- reactive({
      list(
        status = internal_meta(),
        sub_module_ready = rv$ready,
        files = list(
          local = internal_local_folder_tool_script(),
          temp =  internal_temp_folder_tool_script()
        )
      )
    })

    # Render Interno
    output$debug_internal <- renderUI({
      req(show_debug)
      div(style = "margin-top: 20px; padding: 15px; background: #1a1a1a; border-radius: 8px; border: 1px solid #333;",
          h4(icon("terminal"), "Internal Debug", style = "color: #00bc8c; font-size: 0.9rem;"),
          listviewer::jsoneditOutput(ns("json_int"), height = "250px")
      )
    })
    output$json_int <- listviewer::renderJsonedit({
      listviewer::jsonedit(debug_payload())
    })

    # Render Externo
    output$panel_debug_externo <- renderUI({
      div(style = "border: 2px solid #00d4ff; padding: 15px; border-radius: 10px; background: #0b1218;",
          h5(icon("bug"), "EXTERNAL DEBUG MODE", style="color: #00d4ff;"),
          listviewer::jsoneditOutput(ns("json_ext"), height = "250px")
      )
    })
    output$json_ext <- listviewer::renderJsonedit({
      listviewer::jsonedit(debug_payload())
    })

    # --- 8. RETORNO ---
    return(reactive({ rv$sub_data }))
  })
}
