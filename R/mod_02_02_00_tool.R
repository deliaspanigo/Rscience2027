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
    # Contenedor principal con ID para control de estado (Cian/Verde)
    div(id = ns("total_explorer_container"), class = paste("container-fluid", ns("tools-container")),

        # 1. Cabecera Dinámica
        div(class = "flex-shrink-0", uiOutput(ns("tools_header"))),

        br(),

        # Separador Superior
        div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 35px 0;"),

        # 4. El Árbol (Motor de Selección)
        div(class = "map-section",
            div(id = ns("tree_wrapper"), class = "map-wrapper",
                mod_02_02_01_tree_ui(ns("inner_tree"))
            )
        ),

        # Separador Inferior
        div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 35px 0;"),

        # 2. Tree Path and Buttons
        div(class = "row g-3 align-items-center flex-shrink-0", style="margin-top:5px;",
            div(class = "col-md-7",
                div(class = "path-display-area", uiOutput(ns("path_chips_ui")))
            ),
            div(class = "col-md-5",
                div(class = "action-row-right",
                    mod_07_00_engine_control_ui(ns("main_switch"))
                )
            )
        ),

        # 3. Banner de Información
        uiOutput(ns("scripts_info_banner")),

        # 5. Debug Panel
        div(style = "margin-top: 10px; max-height: 200px; overflow-y: auto;",
            listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
        )
    )
  )
}

mod_02_02_00_tool_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Componentes e Invocaciones
    engine_state <- mod_07_00_engine_control_server("main_switch", show_debug = show_debug)
    rlist_tree   <- mod_02_02_01_tree_server("inner_tree")

    is_done      <- reactiveVal(FALSE)
    is_locked    <- reactiveVal(FALSE)

    # ANTI-FLASH: Debounce para el path de chips
    path_estable <- reactive({
      req(rlist_tree()$path_mod)
      rlist_tree()$path_mod
    }) %>% debounce(150)

    # --- OBSERVER DEL MOTOR ---
    observeEvent(engine_state(), {
      state        <- engine_state()$mode
      current_node <- rlist_tree()$selected_node_name
      main_id      <- "total_explorer_container"

      if (state == "lock") {
        if(!is.null(current_node) && current_node != "Rscience") {
          # CAMBIO A VERDE Y BLOQUEO
          shinyjs::addClass(id = main_id, class = "locked-disabled")
          shinyjs::addClass(id = "tree_wrapper", class = "locked-tree-mode")
          is_locked(TRUE)
          is_done(TRUE)
        } else {
          showNotification("Please select a specific tool before locking.", type = "warning")
          shinyjs::delay(200, {
            shinyWidgets::updateRadioGroupButtons(session, "main_switch-engine_mode", selected = "unlock")
          })
        }
      } else {
        # RETORNO A CIAN (Unlock / Reset)
        shinyjs::removeClass(id = main_id, class = "locked-disabled")
        shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree-mode")
        is_locked(FALSE)
        is_done(FALSE)
      }
    })

    # --- RENDERS ---
    output$tools_header <- renderUI({
      tree_data <- rlist_tree()
      current_name <- if(!is.null(tree_data$selected_node_name_mod) && tree_data$selected_node_name != "Rscience") {
        tree_data$selected_node_name_mod
      } else { NULL }

      if (is_locked()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" TOOL CONFIRMED:", current_name)),
            span(class = "header-id", "STATUS: LOCK"))
      } else if (!is.null(current_name)) {
        div(class = "selection-header active-selection",
            span(icon("lock-open"), paste(" SELECTED TOOL:", current_name)),
            span(class = "header-id", "STATUS: UNLOCK"))
      } else {
        div(class = "selection-header waiting-mode",
            span(icon("bolt"), " Waiting for tool selection..."),
            span(class = "header-id", "STATUS: WAITING"))
      }
    })

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
              span("RScience System: ", style = "font-weight: 800; color: #1a202c;"),
              span(sprintf("There are %d tools available in this branch.", tree_data$n_tools))),
          div(style = paste0("margin-top: 5px; border-top: 1px solid rgba(24, 144, 255, 0.2); color:", status_color, ";"),
              icon("microchip", style = paste0("color:", status_color, "; width: 25px;")),
              span("Current Selection: ", style = "font-weight: 800;"),
              span(sprintf("%d scripts for '%s'.", tree_data$n_script, tree_data$selected_node_name_mod)))
      )
    })

    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      listviewer::jsonedit(listdata = list(is_done=is_done(), is_locked=is_locked(), tree=rlist_tree()), mode = "text")
    })

    return(reactive({ list(is_done = is_done(), is_locked = is_locked(), tree = rlist_tree()) }))
  })
}
