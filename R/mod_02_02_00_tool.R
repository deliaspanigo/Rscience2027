# ==============================================================================
# MÓDULO: EXPLORADOR DE HERRAMIENTAS RScience - v.0.0.1
# ==============================================================================
library(jsonlite)
library(listviewer)
library(shiny)
library(shinyjs)



mod_02_02_00_tool_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # CSS para asegurar que el scroll sea fluido y no se corte
    # Cambia esto en mod_02_02_00_tool_ui:
    tags$style(HTML(paste0("
        #", ns("total_explorer_container"), " {
          height: auto;
          /* Elimina o reduce el min-height */
          min-height: 200px;
          overflow-y: visible;
          padding-bottom: 20px;
        }
        .debug-section {
          /* Un min-height de 400px es mucho si sumas el árbol arriba */
          min-height: 200px;
          margin-bottom: 20px;
        }
      "))),

    div(id = ns("total_explorer_container"), class = "container-fluid",

        # --- CABECERA ---
        div(id = ns("the_control"),
            style = "display: flex; align-items: center; justify-content: flex-start; gap: 20px; margin-bottom: 25px;",
            div(class = "section-label",
                style = "margin: 0; white-space: nowrap; font-size: 1.2rem; font-weight: 700; color: #00d4ff;",
                icon("screwdriver-wrench"), " TOOL" # Actualizado el icono a uno más técnico
            ),
            mod_07_00_toggle_ui(ns("main_switch")),
            div(style = "flex-grow: 1;"),
            mod_07_00_label_ui(ns("main_switch")),
            mod_07_00_refresh_ui(ns("main_switch")),
            mod_07_00_unlock_ghost_ui(ns("main_switch"))
        ),

        # --- PATH CHIPS ---
        div(style = "width: 100%; margin-bottom: 10px;",
            div(id = ns("info_section"),
                class = "path-display-area",
                uiOutput(ns("path_chips_ui"))
            )
        ),

        # --- EL ÁRBOL ---
        div(class = "map-section",
            div(id = ns("tree_wrapper"), class = "map-wrapper",
                mod_02_02_01_tree_ui(ns("inner_tree"))
            )
        ),

        # --- BANNER DE INFORMACIÓN ---
        uiOutput(ns("scripts_info_banner")),

        # --- DEBUG PANEL (CORREGIDO) ---
        # Eliminamos el max-height y el overflow-y:auto de aquí
        div(style = "margin-top: 4px;",
            uiOutput(ns("show_debug"))
        )
    )
  )
}

mod_02_02_00_tool_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)


    # 1. Componentes e Invocaciones
    rlist_control_btn <- mod_07_00_engine_control_server("main_switch",
                                                         show_debug = internal_show_debug,
                                                         show_ghost = FALSE)
    rlist_tree   <- mod_02_02_01_tree_server("inner_tree")

    is_done      <- reactiveVal(FALSE)
    is_locked    <- reactiveVal(FALSE)

    # ANTI-FLASH: Debounce para el path de chips
    path_estable <- reactive({
      req(rlist_tree()$path_mod)
      rlist_tree()$path_mod
    }) %>% shiny::debounce(150)

    # --- Special functions ---
    get_default_data <- function() {
      list(
        "details" = "*** RScience - Tool ***",
        "my_sys_time" = Sys.time(),
        "click_count" = 0, # Corregido el nombre si era click_count
        "mode" = NULL,
        "is_done" = FALSE,
        "is_locked" = FALSE,
        "error_msg" = NULL,
        "metadata_control" = list(),
        "metadata_tree" = list()
      )
    }
    reset_data_store <- function() {
      defaults <- get_default_data()

      # mapply recorre los nombres y valores de la lista de defaults
      # y los asigna uno a uno al objeto reactiveValues
      mapply(function(val, name) {
        data_store[[name]] <- val
      }, defaults, names(defaults))
    }

    data_store <- do.call(reactiveValues, get_default_data())


    # --- OBSERVER DEL MOTOR ---
    # --- OBSERVER DEL MOTOR (v.0.1.2) ---
    # ==========================================================================
    # OBSERVADOR DE ESTADO DEL MOTOR (LOCK / UNLOCK / RESET)
    # ==========================================================================
    observeEvent(rlist_control_btn(), {

      # 1. CAPTURA DE ESTADO DISPARADOR
      # Solo reaccionamos al cambio de modo del switch
      flat_rlist_control_btn <- rlist_control_btn()
      state <- flat_rlist_control_btn$mode

      # 2. AISLAMIENTO DEL CONTEXTO (Snapshot)
      # Usamos isolate para tomar la 'foto' del árbol sin crear dependencia reactiva.
      # Esto evita que el observer se dispare solo porque el usuario navega en el árbol.
      tree_snapshot <- isolate(rlist_tree())
      current_node  <- tree_snapshot$selected_node_name

      # IDs que recibirán el tratamiento visual de congelamiento
      sections_to_freeze <- c(
        "header_section",
        "info_section",
        "tree_wrapper",
        "path_chips_ui"
      )

      if (state == "lock") {
        # --- LÓGICA DE BLOQUEO (LOCK) ---

        # Verificamos validez de la selección antes de cerrar el candado
        if(!is.null(current_node) && current_node != "Rscience") {

          # A. FEEDBACK VISUAL INMEDIATO
          # Aplicamos clase que deshabilita puntero (pointer-events: none)
          lapply(sections_to_freeze, function(x) {
            shinyjs::addClass(id = x, class = "locked-disabled")
          })

          # Aplicamos estética de 'Sistema Operativo' (Verde/Neon)
          shinyjs::addClass(id = "tree_wrapper", class = "locked-tree-mode")

          # B. ACTUALIZACIÓN DE ESTADO INTERNO (AISLADO)
          isolate({
            is_locked(TRUE)
            is_done(TRUE)

            # Sincronizamos con el data_store principal
            data_store$is_done   <- is_done()
            data_store$is_locked <- is_locked()
            data_store$metadata_control <- flat_rlist_control_btn
            data_store$metadata_tree      <- tree_snapshot # Guardamos la versión fija del árbol
            data_store$mode      <- "lock"
            data_store$click_count <- data_store$click_count + 1
          })

          showNotification(paste("Tool confirmed:", current_node), type = "message")

        } else {
          # --- LÓGICA DE REBOTE (FALLO DE VALIDACIÓN) ---
          showNotification("Please select a specific tool before locking.", type = "warning")

          # Forzamos el switch a volver a 'unlock' por software
          shinyjs::delay(200, {
            shinyWidgets::updateRadioGroupButtons(
              session  = session,
              inputId  = "main_switch-engine_mode",
              selected = "unlock"
            )
          })
        }

      } else if (state == "unlock") {
        # --- LÓGICA DE LIBERACIÓN (UNLOCK) ---

        # A. RESTAURACIÓN VISUAL (Retorno a Cian/Activo)
        lapply(sections_to_freeze, function(x) {
          shinyjs::removeClass(id = x, class = "locked-disabled")
        })

        shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree-mode")

        # B. ACTUALIZACIÓN DE ESTADO (AISLADO)
        isolate({
          is_locked(FALSE)
          is_done(FALSE)

          save_count <- data_store$click_count
          new_count <- save_count + 1
          reset_data_store()
          data_store$mode <- "unlock"
          data_store$click_count <- new_count

          # No reseteamos todo el store aquí si queremos mantener la selección previa
        })

      } else if (state == "reset") {
        # --- LÓGICA DE LIMPIEZA TOTAL (RESET) ---

        # Limpiamos clases de bloqueo por si acaso
        lapply(sections_to_freeze, function(x) {
          shinyjs::removeClass(id = x, class = "locked-disabled")
        })
        shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree-mode")

        # Reset total de la lógica
        isolate({
          is_locked(FALSE)
          is_done(FALSE)

          save_count <- data_store$click_count
          new_count <- save_count + 1
          reset_data_store()
          data_store$mode <- "reset"
          data_store$click_count <- new_count

          #reset_data_store() # Esta función debe resetear data_store

        })

        # Pequeña pausa estética y volvemos a unlock automáticamente
        shinyjs::delay(1000, {
          shinyWidgets::updateRadioGroupButtons(
            session  = session,
            inputId  = "main_switch-engine_mode",
            selected = "unlock"
          )
        })
      }
    }, ignoreInit = TRUE)

    # --- RENDERS ---


    output$path_chips_ui <- renderUI({
      partes <- unlist(strsplit(path_estable(), " / "))
      tagList(lapply(partes, function(p) { span(class = "path-chip", p) }))
    })

    output$scripts_info_banner <- renderUI({
      tree_data <- rlist_tree()
      req(tree_data$real_total_tools > 0)
      status_color <- if(is_done()) "#28a745" else "#ff9100"

      div(class = "info-banner-blue",
          style = "line-height: 1.8; padding: 15px; border-radius: 8px; font-size: 0.95rem;",
          div(icon("sitemap", style = "color: #1890ff; width: 25px;"),
              span("Selected Node - Tools: ", style = "font-weight: 800; color: #ffffff;"),
              span(sprintf("There are %d tools available in this branch.", tree_data$n_tools))),
          div(style = paste0("margin-top: 5px; border-top: 1px solid rgba(24, 144, 255, 0.2); color:", status_color, ";"),
              icon("microchip", style = paste0("color:", status_color, "; width: 25px;")),
              span("Selected Node - Script: ", style = "font-weight: 800;"),
              span(sprintf("%d scripts for '%s'.", tree_data$n_script, tree_data$selected_node_name_mod)))
      )
    })


    # # # DEBUG
    output$debug_tool <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(listdata = reactiveValuesToList(data_store), mode = "text")
    })

    output$debug_control <- listviewer::renderJsonedit({
      req(internal_show_debug())
      req(rlist_control_btn())
      flat_rlist_control_btn <- rlist_control_btn()

      listviewer::jsonedit(listdata = flat_rlist_control_btn, mode = "text")
    })

    output$show_debug <- renderUI({
      req(internal_show_debug())

      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 15px; margin-top: 20px;",

          # Título de la sección
          div(class = "section-label",
              style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 15px; color: #ffab70;",
              icon("bug"), " Internal Debug - Dataset"),

          # Contenedor de dos columnas
          div(style = "display: flex; gap: 20px; align-items: flex-start;",

              # Columna 1: Tool
              div(style = "flex: 1; min-width: 0;",
                  div(style = "font-size: 0.7rem; color: #8b949e; margin-bottom: 5px;", "TOOL STATE"),
                  listviewer::jsoneditOutput(ns("debug_tool"), height = "auto")
              ),

              # Columna 2: Control
              div(style = "flex: 1; min-width: 0;",
                  div(style = "font-size: 0.7rem; color: #8b949e; margin-bottom: 5px;", "CONTROL ENGINE"),
                  listviewer::jsoneditOutput(ns("debug_control"), height = "auto")
              )
          )
      )
    })




    return(reactive({ reactiveValuesToList(data_store) }))
  })
}
