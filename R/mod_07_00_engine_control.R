# UI del Módulo
mod_07_00_engine_control_ui <- function(id) {
  ns <- NS(id)

  tagList(
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



