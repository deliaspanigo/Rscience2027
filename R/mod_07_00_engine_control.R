# UI del Módulo
mod_07_00_engine_control_ui <- function(id) {
  ns <- NS(id)

  # 1. CARGAR RECURSOS (Esto es lo que faltaba)
  # Buscamos la carpeta www del paquete
  lib_www_path <- system.file("www", "css", package = "Rscience2027")

  # Si estás en desarrollo local (sin el paquete instalado aún)
  if (lib_www_path == "") lib_www_path <- "www"

  # Si existe la carpeta, creamos la ruta y el path al CSS
  if (dir.exists(lib_www_path)) {
    addResourcePath("lib_www", normalizePath(lib_www_path))
    path_to_css <- file.path(lib_www_path, "style_000.css")
  } else {
    path_to_css <- NULL
  }

  tagList(
    tags$head(
      useShinyjs(),

      # IMPORTANTE: No usamos includeCSS.
      # Usamos tags$link apuntando al recurso que definiste con addResourcePath
      if (!is.null(path_to_css)) {
        tags$link(rel = "stylesheet", type = "text/css", href = "lib_www/style_000.css")
      }
    ),
    # --- TRUCO: Forzar carga de Font Awesome ---
    tags$div(style = "display:none;", icon("cogs")),

    div(class = "rs-control-card",
        div(class = "rs-btn-group-container rs-engine-selector",
            shinyWidgets::radioGroupButtons(
              inputId = ns("engine_mode"),
              label = NULL,
              choiceNames = list(
                HTML("<i class='fas fa-unlock-alt'></i>&nbsp; UNLOCK"),
                HTML("<i class='fas fa-lock'></i>&nbsp; LOCK"),
                HTML("<i class='fas fa-sync-alt'></i>&nbsp; RESET")
              ),
              choiceValues = list("unlock", "lock", "reset"),
              selected = "unlock",
              justified = TRUE,
              status = "engine-custom"
            )
        ),
        uiOutput(ns("internal_status_ui")),
        br(),
        listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
    )
  )
}

# Server del Módulo
# Server del Módulo - v.0.7.1 (Safe Mode)
mod_07_00_engine_control_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- ALMACÉN DE ESTADO ---
    # Aquí guardamos lo que el server "emite" y lo que el debug muestra
    engine_status <- reactiveValues(
      mode = "unlock",
      last_update = format(Sys.time(), "%H:%M:%S")
    )

    # Actualizar el almacén cuando cambie el input
    observeEvent(input$engine_mode, {
      req(input$engine_mode)
      engine_status$mode <- input$engine_mode
      engine_status$last_update <- format(Sys.time(), "%H:%M:%S")

      # Lógica especial de Reset
      if(input$engine_mode == "reset") {
        showNotification("Flushing RScience Engine...", type = "warning")
        shinyjs::delay(1000, {
          shinyWidgets::updateRadioGroupButtons(session, "engine_mode", selected = "unlock")
        })
      }
    })

    # --- RENDER ESTATUS (Visual) ---
    # --- RENDER ESTATUS (Visual) ---
    output$internal_status_ui <- renderUI({
      mode <- engine_status$mode

      # Solo definimos el texto y la clase CSS
      config <- switch(mode,
                       "unlock" = list(txt = "ENGINE UNLOCKED",  cl = "status-unlock"),
                       "lock"   = list(txt = "ENGINE LOCKED",       cl = "status-lock"),
                       "reset"  = list(txt = "RESETTING...",        cl = "status-reset"),
                       list(txt = "WAITING...", cl = "status-waiting")
      )

      div(class = paste("rs-status-display", config$cl),
          span(config$txt)
      )
    })

    # --- RENDER DEBUG ---
    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      # Muestra exactamente lo que el servidor está procesando/retornando
      listviewer::jsonedit(
        listdata = reactiveValuesToList(engine_status),
        mode = "text"
      )
    })

    # --- RETORNO ---
    # Retornamos la lista completa (modo y hora)
    return(reactive({ reactiveValuesToList(engine_status) }))
  })
}



