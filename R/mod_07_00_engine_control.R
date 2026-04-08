# ==========================================================
# MOD_07_00_ENGINE_CONTROL.R
# ==========================================================

# UI del Módulo
mod_07_00_engine_control_ui <- function(id) {
  ns <- NS(id)

  # Carga de CSS
  lib_www_path <- system.file("www", "css", package = "Rscience2027")
  if (lib_www_path == "") lib_www_path <- "www"
  if (dir.exists(lib_www_path)) {
    addResourcePath("lib_www", normalizePath(lib_www_path))
    path_to_css <- "lib_www/style_000.css" # Cambia al nombre de tu CSS de motor
  } else {
    path_to_css <- NULL
  }

  tagList(
    tags$head(
      useShinyjs(),
      if (!is.null(path_to_css)) tags$link(rel = "stylesheet", type = "text/css", href = path_to_css)
    ),

    div(class = "rs-control-card",
        div(class = "rs-btn-group-container rs-engine-selector",
            shinyWidgets::radioGroupButtons(
              inputId = ns("engine_mode"),
              label = NULL,
              choiceNames = list(
                HTML("<i class='fas fa-lock-open'></i>&nbsp; UNLOCK"),
                HTML("<i class='fas fa-lock'></i>&nbsp; LOCK"),
                HTML("<i class='fas fa-sync-alt'></i>&nbsp; RESET")
              ),
              choiceValues = list("unlock", "lock", "reset"),
              selected = "unlock",
              justified = FALSE, # IMPORTANTE: FALSE para permitir el gap de las cápsulas
              status = "engine-custom"
            )
        ),
        uiOutput(ns("internal_status_ui")),
        br(),
        listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
    )
  )
}

mod_07_00_engine_control_DEBUG_ui <-  function(id) {

  ns <- NS(id)

  tagList(
    uiOutput(ns("show_debug_external"))
  )

}



# Server del Módulo
# Server del Módulo - v.0.7.1 (Safe Mode)
# Server del Módulo - v.0.7.1 (Safe Mode + Modal Confirmation)
# Server del Módulo - v.0.7.1 (Modal solo para RESET)
# Server del Módulo - v.0.7.1 (Modal como Bloqueo de Interfaz)
mod_07_00_engine_control_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    www_folder <- system.file("www", package = "Rscience2027")
    if (www_folder == "") www_folder <- "www"
    try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

    # --- ALMACÉN DE ESTADO ---
    engine_status <- reactiveValues(
      mode = "unlock",
      last_update = format(Sys.time(), "%H:%M:%S"),
      try_lock = FALSE
    )

    # --- LÓGICA DE RESET CON BLOQUEO TOTAL ---
    observeEvent(input$engine_mode, {


      req(input$engine_mode == "reset")

      # 1. Disparamos un modal que NO se puede cerrar (Bloqueo de UI)
      # 1. Disparamos un modal que NO se puede cerrar (Bloqueo de UI)
      showModal(modalDialog(
        title = NULL,
        footer = NULL,
        size = "l",
        easyClose = FALSE,
        div(class = "text-center", style = "padding: 40px; background-color: #0b1218; border-radius: 15px;",

            # --- CONTENEDOR DE IMAGEN ---
            div(style = "margin-bottom: 30px;",
                img(src = "WWW-FOLDER/Rscience_logo_sticker.png",
                    style = "width: 250px; filter: drop-shadow(0 0 10px #00d4ff66);")
            ),

            # --- ICONO DE PROCESO ---
            div(style = "margin-bottom: 20px;",
                icon("sync-alt", class = "fa-spin",
                     style = "font-size: 4.5rem; color: #ffc107; text-shadow: 0 0 20px rgba(255, 193, 7, 0.4);")
            ),

            # --- TEXTO DE ESTADO ---
            h3("RScience Engine", style = "color: #00d4ff; font-weight: 800; letter-spacing: 2px;"),
            p("Resetting system & clearing cache...",
              style = "color: #aaaaaa; font-style: italic; font-size: 1.1rem; margin-top: 10px;")
        )
      ))

      # 2. Actualizamos el estado interno
      engine_status$mode <- "reset"
      engine_status$last_update <- format(Sys.time(), "%H:%M:%S")
      engine_status$try_lock <- FALSE

      # 3. Simulamos/Ejecutamos el proceso y liberamos la UI
      shinyjs::delay(2000, {
        # Volvemos el selector a unlock
        shinyWidgets::updateRadioGroupButtons(session, "engine_mode", selected = "unlock")

        # Actualizamos estado
        engine_status$mode <- "unlock"
        engine_status$last_update <- format(Sys.time(), "%H:%M:%S")

        # QUITAMOS EL MODAL (Libera la App)
        removeModal()

        showNotification("Engine Ready", type = "message")
      })
    })

    # --- ACTUALIZACIÓN DE ESTADO (LOCK / UNLOCK) ---
    observeEvent(input$engine_mode, {
      req(input$engine_mode %in% c("unlock", "lock"))
      engine_status$mode <- input$engine_mode
      engine_status$last_update <- format(Sys.time(), "%H:%M:%S")

      if(input$engine_mode == "lock")  engine_status$try_lock <- TRUE else engine_status$try_lock <- TRUE
    })

    # --- RENDERS VISUALES ---
    output$internal_status_ui <- renderUI({
      mode <- engine_status$mode
      config <- switch(mode,
                       "unlock" = list(txt = "ENGINE UNLOCKED",  cl = "status-unlock"),
                       "lock"   = list(txt = "ENGINE LOCKED",    cl = "status-lock"),
                       "reset"  = list(txt = "RESETTING...",     cl = "status-reset"),
                       list(txt = "WAITING...", cl = "status-waiting")
      )
      div(class = paste("rs-status-display", config$cl), span(config$txt))
    })

    # --- LÓGICA DE DEBUG (PROTEGIDA) ---
    render_debug_json <- function() {
      listviewer::jsonedit(
        listdata = reactiveValuesToList(engine_status),
        mode = "text"
      )
    }

    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      render_debug_json()
    })

    output$show_debug_external <- renderUI({
      req(show_debug)
      div(style = "background: #1a1a1a; padding: 15px; border-radius: 8px; border: 1px solid #333;",
          div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              icon("microchip", style="color: #00bc8c;"),
              h5("DEBUG - Engine Control System", style="color: #00bc8c; margin: 0;")
          ),
          listviewer::jsoneditOutput(ns("debug_json_ext"), height = "300px")
      )
    })

    output$debug_json_ext <- listviewer::renderJsonedit({
      req(show_debug)
      render_debug_json()
    })

    # --- RETORNO ---
    return(reactive({ reactiveValuesToList(engine_status) }))
  })
}



