library(shiny)
library(shinyjs)
library(listviewer)

# ==============================================================================
# MOD_07_00_ENGINE_CONTROL.R - v.0.1.7 (FULL ORANGE REFRESH)
# ==============================================================================

# --- 1. SUB-COMPONENTES "INTELIGENTES" ---

.engine_wrapper <- function(id, content) {
  ns <- NS(id)
  div(
    id = ns("isolate_scope"),
    mod_07_00_inject_css(id),
    content
  )
}

mod_07_00_label_ui <- function(id) {
  ns <- NS(id)
  .engine_wrapper(id, uiOutput(ns("status_display")))
}

mod_07_00_refresh_ui <- function(id) {
  ns <- NS(id)
  .engine_wrapper(id, div(id = ns("btn_refresh"), class = "refresh-trigger", icon("sync-alt")))
}

mod_07_00_toggle_ui <- function(id) {
  ns <- NS(id)
  .engine_wrapper(id,
                  div(id = ns("toggle_engine"), class = "custom-toggle-rail",
                      icon("lock-open", style = "color: rgba(0, 229, 255, 0.2); font-size: 18px;"),
                      icon("lock", style = "color: rgba(0, 255, 136, 0.2); font-size: 18px;"),
                      div(id = ns("knob"), class = "toggle-knob", uiOutput(ns("knob_icon")))
                  )
  )
}

mod_07_00_unlock_ghost_ui <- function(id) {
  ns <- NS(id)
  .engine_wrapper(id,
                  shinyjs::hidden(
                    actionButton(ns("btn_unlock_ghost"), "FORCE UNLOCK (GHOST)",
                                 style = "margin-top: 10px; background: rgba(255,255,255,0.05); color: #555;
                            border: 1px dashed #333; font-size: 0.65rem; padding: 2px 8px; border-radius: 4px;")
                  )
  )
}

# --- 2. EL INYECTOR DE CSS (ESTADOS AMPLIADOS) ---

mod_07_00_inject_css <- function(id) {
  ns <- NS(id)
  scope_id <- paste0("#", ns("isolate_scope"))

  tags$head(
    tags$style(HTML(paste0("
      ", scope_id, " .engine-hub {
        background: rgba(22, 27, 34, 0.98) !important; padding: 15px 30px; border-radius: 100px;
        border: 3px solid rgba(255, 255, 255, 0.15) !important; display: inline-flex; align-items: center; gap: 25px;
      }
      ", scope_id, " .hub-label {
        font-family: 'Segoe UI', sans-serif !important; font-weight: 900 !important; font-size: 1.1rem !important;
        letter-spacing: 2.5px; text-transform: uppercase; text-align: center;
      }
      ", scope_id, " .text-unlock  { color: #00e5ff !important; text-shadow: 0 0 15px rgba(0, 229, 255, 0.6); }
      ", scope_id, " .text-lock    { color: #00ff88 !important; text-shadow: 0 0 15px rgba(0, 255, 136, 0.6); }
      ", scope_id, " .text-refresh { color: #ff9100 !important; text-shadow: 0 0 15px rgba(255, 145, 0, 0.6); }

      ", scope_id, " .refresh-trigger {
        color: #ff9100 !important; font-size: 1.8rem; cursor: pointer;
        width: 55px; height: 55px; display: flex; align-items: center; justify-content: center;
      }

      ", scope_id, " .custom-toggle-rail {
        width: 130px; height: 50px; background: #05070a !important; border-radius: 40px;
        position: relative; cursor: pointer; border: 3px solid #30363d !important;
        display: flex; align-items: center; justify-content: space-between; padding: 0 18px;
        transition: border-color 0.4s ease;
      }

      ", scope_id, " .toggle-knob {
        width: 40px; height: 40px; border-radius: 50%; position: absolute;
        transition: all 0.4s cubic-bezier(0.68, -0.55, 0.265, 1.55);
        display: flex; align-items: center; justify-content: center; color: #0d1117 !important;
      }

      /* ESTADOS DINÁMICOS */
      ", scope_id, " .knob-unlock  { left: 5px; background: #00e5ff !important; box-shadow: 0 0 20px #00e5ff; }
      ", scope_id, " .rail-unlock  { border-color: #00e5ff !important; }

      ", scope_id, " .knob-lock    { left: 80px; background: #00ff88 !important; box-shadow: 0 0 20px #00ff88; }
      ", scope_id, " .rail-lock    { border-color: #00ff88 !important; }

      ", scope_id, " .knob-refresh { left: 42.5px; background: #ff9100 !important; box-shadow: 0 0 20px #ff9100; }
      ", scope_id, " .rail-refresh { border-color: #ff9100 !important; box-shadow: 0 0 20px rgba(255, 145, 0, 0.3); }

      ", scope_id, " .refreshing-anim { animation: ", ns("spin"), " 0.8s infinite linear; }
      @keyframes ", ns("spin"), " { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }

      ", scope_id, " .hub-refreshing { border-color: #ff9100 !important; pointer-events: none; opacity: 0.8; }
    ")))
  )
}

# --- 3. UI COMPLETA ---

mod_07_00_engine_control_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "engine-hub-container",
    mod_07_00_unlock_ghost_ui(id),
    mod_07_00_label_ui(id),
    div(id = ns("hub_body"), class = "engine-hub",
        mod_07_00_refresh_ui(id),
        div(style = "width: 3px; height: 30px; background: rgba(255,255,255,0.1);"),
        mod_07_00_toggle_ui(id)
    ),
    uiOutput(ns("show_debug_internal"))
  )
}

# --- 4. SERVER (LÓGICA CON REFRESH SINCRONIZADO) ---

mod_07_00_engine_control_server <- function(id,
                                            show_debug = reactive({FALSE}),
                                            show_ghost = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_store <- reactiveValues(mode = "unlock", sys_status = "READY", click_count = 0)

    # Helper para actualizar visuales (ahora incluye modo refresh)
    update_visuals <- function(target_mode) {
      shinyjs::removeClass("toggle_engine", "rail-unlock rail-lock rail-refresh")
      shinyjs::removeClass("knob", "knob-unlock knob-lock knob-refresh")

      if(target_mode == "unlock") {
        shinyjs::addClass("toggle_engine", "rail-unlock"); shinyjs::addClass("knob", "knob-unlock")
      } else if(target_mode == "lock") {
        shinyjs::addClass("toggle_engine", "rail-lock"); shinyjs::addClass("knob", "knob-lock")
      } else if(target_mode == "refresh") {
        shinyjs::addClass("toggle_engine", "rail-refresh"); shinyjs::addClass("knob", "knob-refresh")
      }
    }

    output$status_display <- renderUI({
      cl <- switch(data_store$mode, "unlock"="text-unlock", "lock"="text-lock", "refresh"="text-refresh")
      div(class = paste("hub-label", cl), data_store$sys_status)
    })

    output$knob_icon <- renderUI({
      if(data_store$mode == "lock") icon("lock")
      else if(data_store$mode == "refresh") icon("sync-alt")
      else icon("lock-open")
    })

    observe({ update_visuals(data_store$mode) })

    # Evento Ghost
    observeEvent(input$btn_unlock_ghost, {
      data_store$mode <- "unlock"
      data_store$sys_status <- "SYSTEM UNLOCKED"
      update_visuals("unlock")
    })

    # Toggle Manual
    shinyjs::onclick("toggle_engine", {
      if(data_store$mode != "refresh") {
        data_store$mode <- if(data_store$mode == "unlock") "lock" else "unlock"
        data_store$sys_status <- if(data_store$mode == "lock") "SYSTEM SECURED" else "SYSTEM UNLOCKED"
        data_store$click_count <- data_store$click_count + 1
      }
    })

    # Botón Refresh (AQUÍ OCURRE LA MAGIA)
    shinyjs::onclick("btn_refresh", {
      if(data_store$mode != "refresh") {
        data_store$mode <- "refresh"
        data_store$sys_status <- "RESETTING..."

        # Sincronizamos el toggle a naranja
        update_visuals("refresh")

        shinyjs::addClass("btn_refresh", "refreshing-anim")
        shinyjs::addClass("hub_body", "hub-refreshing")

        shinyjs::delay(1500, {
          data_store$mode <- "unlock"
          data_store$sys_status <- "SYSTEM UNLOCKED"

          shinyjs::removeClass("btn_refresh", "refreshing-anim")
          shinyjs::removeClass("hub_body", "hub-refreshing")

          # Devolvemos el toggle a su estado base
          update_visuals("unlock")
        })
      }
    })

    observe({

      the_mode <- data_store$mode
      is_locked <- the_mode == "lock"
      data_store$is_locked <- is_locked
    })

    output$debug_internal_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(reactiveValuesToList(data_store))
    })

    return(reactive({ reactiveValuesToList(data_store) }))
  })
}
