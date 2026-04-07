# UI del Módulo
mod_07_00_engine_control_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "rs-control-card",
        #h4("RScience Engine", style="text-align:center; color:#00d4ff; margin-bottom:20px;"),

        # 1. Agregamos la clase 'rs-engine-selector' para que el CSS sepa dónde buscar
        div(class = "rs-btn-group-container rs-engine-selector",
            radioGroupButtons(
              inputId = ns("engine_mode"),
              label = NULL,
              choices = c(
                " <i class='fa fa-unlock-alt'></i> UNLOCK" = "unlock",
                " <i class='fa fa-lock'></i> LOCKED" = "lock",
                " <i class='fa fa-sync'></i> RESET" = "reset"
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
          updateRadioGroupButtons(session, "engine_mode", selected = "unlock")
        })
      }
    })

    # --- RENDER ESTATUS (Visual) ---
    output$internal_status_ui <- renderUI({
      mode <- engine_status$mode

      config <- switch(mode,
                       "unlock" = list(txt = "CONFIGURATION MODE",  col = "#00d4ff", bg = "rgba(0, 212, 255, 0.05)"),
                       "lock"   = list(txt = "ENGINE LOCKED",       col = "#28a745", bg = "rgba(40, 167, 69, 0.08)"),
                       "reset"  = list(txt = "RESETTING...",        col = "#f39c12", bg = "rgba(255, 71, 87, 0.08)"),
                       list(txt = "WAITING...", col = "#aaa", bg = "transparent")
      )

      div(class = "rs-status-display",
          style = paste0("border: 1px solid ", config$col, "; color: ", config$col, "; background: ", config$bg),
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



