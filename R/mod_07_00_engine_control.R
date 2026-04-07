# UI del Módulo
mod_07_00_engine_control_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "rs-control-card",
        #h4("RScience Engine", style="text-align:center; color:#00d4ff; margin-bottom:20px;"),

        # 1. Agregamos la clase 'rs-engine-selector' para que el CSS sepa dónde buscar
        div(class = "rs-btn-group-container rs-engine-selector",
            shinyWidgets::radioGroupButtons(
              inputId = ns("engine_mode"),
              label = NULL,
              choices = c(
                " <i class='fa fa-unlock-alt'></i>&nbsp; UNLOCK" = "unlock",
                " <i class='fa fa-lock'></i>&nbsp; LOCKED"      = "lock",
                " <i class='fa fa-sync'></i>&nbsp; RESET"       = "reset"
              ),
              selected = "unlock",
              justified = TRUE,
              individual = FALSE,
              # 2. IMPORTANTE: Cambiamos a 'engine-custom' para evitar conflictos con Bootstrap
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
                       "unlock" = list(txt = "CONFIGURATION MODE",  cl = "status-unlock"),
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



