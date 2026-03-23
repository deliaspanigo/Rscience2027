mod_tools_ui <- function(id) {
  ns <- NS(id)
  root_sel <- paste0(".", ns("tools-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        ", root_sel, " {
            display: flex; flex-direction: column;
            width: 100%; height: calc(100vh - 80px);
            padding: 20px !important; background: #f8f9fa;
            overflow: hidden;
        }
        ", root_sel, " .map-section {
            display: flex; flex-direction: column; flex: 1;
            margin-top: 15px; position: relative;
        }
        ", root_sel, " .map-wrapper {
            background: #000; border-radius: 20px;
            border: 4px solid #1a202c; position: relative;
            flex: 1; width: 100%; overflow: hidden;
            transition: all 0.3s ease;
        }

        /* --- CAMBIOS SOLICITADOS: BLOQUEO VERDE A LA IZQUIERDA --- */
        .locked-tree {
            pointer-events: none !important;
            opacity: 0.85;
            position: relative;
            border: 4px solid #28a745 !important; /* Borde verde al confirmar */
            box-shadow: 0 0 20px rgba(40, 167, 69, 0.3);
        }

        .locked-tree::after {
            content: '🔒 SELECCIÓN CONFIRMADA';
            position: absolute;
            top: 20px;
            left: 20px;   /* Movido a la IZQUIERDA */
            right: auto;  /* Reseteamos el right por si acaso */
            background: #28a745; /* Fondo Verde */
            color: #ffffff;
            padding: 10px 18px;
            border-radius: 8px;
            font-weight: 800;
            font-size: 1.2rem;
            z-index: 2000;
            box-shadow: 0 0 15px rgba(40, 167, 69, 0.5);
        }
        /* ------------------------------------------------------- */

        .path-display-area {
            background: #1a202c; border-radius: 12px; padding: 10px;
            min-height: 50px; display: flex; align-items: center; gap: 8px;
        }
        .path-chip {
            background: #00FFFF; color: #000; padding: 4px 12px;
            border-radius: 50px; font-weight: 800; font-size: 0.8rem;
        }
        .info-banner-blue {
            background: #e6f7ff; border-left: 5px solid #1890ff;
            padding: 10px 20px; margin: 10px 0; border-radius: 4px;
        }
      ")))
    ),

    div(class = paste("container-fluid", ns("tools-container")),
        div(class = "flex-shrink-0", uiOutput(ns("tools_header"))),

        div(class = "row g-3 align-items-center flex-shrink-0", style="margin-top:5px;",
            div(class = "col-md-7",
                div(class = "path-display-area", uiOutput(ns("path_chips_ui")))
            ),
            div(class = "col-md-5 text-end",
                actionButton(ns("btn_confirm"), "Confirm", class = "btn btn-success"),
                actionButton(ns("btn_edit"), "Edit", class = "btn btn-warning"),
                actionButton(ns("btn_reset"), "Reset", class = "btn btn-primary")
            )
        ),

        uiOutput(ns("scripts_info_banner")),

        div(class = "map-section",
            div(id = ns("tree_wrapper"), class = "map-wrapper",
                mod_tree_ui(ns("inner_tree"))
            )
        )
    )
  )
}

mod_tools_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    arbol_res <- mod_tree_server("inner_tree")
    is_locked <- reactiveVal(FALSE)

    observeEvent(input$btn_confirm, {
      req(arbol_res()$node_name)
      if(arbol_res()$node_name != "Rscience") {
        shinyjs::addClass(id = "tree_wrapper", class = "locked-tree")
        shinyjs::disable("btn_confirm")
        is_locked(TRUE)
      }
    })

    observeEvent(input$btn_edit, {
      shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree")
      shinyjs::enable("btn_confirm")
      is_locked(FALSE)
    })

    observeEvent(input$btn_reset, {
      shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree")
      shinyjs::enable("btn_confirm")
      is_locked(FALSE)
    })

    output$tools_header <- renderUI({
      data <- arbol_res()
      color <- if(is_locked()) "#28a745" else "#ffc107"
      tags$h2(paste("Herramienta:", data$node_name),
              style = paste0("color:", color, "; font-weight:900;"))
    })

    output$path_chips_ui <- renderUI({
      req(arbol_res()$path)
      partes <- unlist(strsplit(arbol_res()$path, " / "))
      lapply(partes, function(p) {
        span(class = "path-chip", p)
      })
    })

    output$scripts_info_banner <- renderUI({
      req(arbol_res()$scripts)
      n_scripts <- length(arbol_res()$scripts)
      div(class = "info-banner-blue",
          icon("info-circle"),
          span(sprintf(" Se han detectado %d scripts disponibles en esta rama del árbol.", n_scripts))
      )
    })

    return(reactive({
      list(confirmado = is_locked(), datos = arbol_res())
    }))
  })
}
