

#devtools::load_all()

mod_tools_ui <- function(id) {
  ns <- NS(id)
  root_sel <- paste0(".", ns("tools-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        /* --- BLOQUEO CON OVERLAY (ADN IMPORTACIÓN) --- */
        ", root_sel, " .lock-wrapper { position: relative; transition: all 0.3s ease; }
        ", root_sel, " .locked-disabled::after {
            content: ''; position: absolute; top: -2px; left: -5px; right: -5px; bottom: -5px;
            background: rgba(240, 240, 240, 0.4); backdrop-filter: blur(2px);
            z-index: 100; border-radius: 15px; border: 1px solid rgba(0,0,0,0.05); cursor: not-allowed !important;
        }
        ", root_sel, " .locked-disabled { pointer-events: none !important; user-select: none; filter: grayscale(0.3); }

        /* --- SELECTION HEADER (MARQUESINA) --- */
        ", root_sel, " .selection-header {
            display: flex; justify-content: space-between; align-items: center;
            padding: 15px 25px; border-radius: 12px; transition: all 0.4s ease;
            box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        }
        ", root_sel, " .selection-header.waiting-mode { background: #f0fdff; border: 1px solid #00cfd4; color: #008184; }
        ", root_sel, " .selection-header.active-selection { background: #fff9f0; border: 1px solid #ff9100; color: #b36600; }
        ", root_sel, " .selection-header.confirmed { background: #f6fff8; border: 1px solid #28a745; color: #1e7e34; }
        ", root_sel, " .header-id {
            font-family: 'Monaco', 'Courier New', monospace; font-weight: 700;
            font-size: 0.85rem; background: rgba(0,0,0,0.08); padding: 4px 15px; border-radius: 20px;
        }

        /* --- BOTONES PILDORA XL --- */
        ", root_sel, " .btn.btn-pill-xl {
            border-radius: 50px !important; padding: 15px 35px !important;
            font-weight: 800 !important; font-size: 1.1rem !important;
            text-transform: uppercase !important; letter-spacing: 1px !important;
            display: inline-flex !important; align-items: center !important;
            justify-content: center !important; gap: 10px !important;
            transition: all 0.3s ease !important;
        }
        ", root_sel, " .btn.btn-pill-xl:hover { transform: translateY(-2px) !important; filter: brightness(1.1); }
        ", root_sel, " .action-row-right { display: flex; flex-direction: row; justify-content: flex-end; align-items: center; gap: 12px; height: 100%; }

        /* --- LABELS Y PATH --- */
        ", root_sel, " .section-label { font-weight: 800; font-size: 1.1rem !important; color: #1a1a1a !important; text-transform: uppercase; margin-bottom: 10px; letter-spacing: 1px; }
        ", root_sel, " .path-display-area { background: #1a202c; border-radius: 12px; padding: 15px; min-height: 60px; display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
        ", root_sel, " .path-chip { background: #28a745; color: white !important; padding: 6px 15px; border-radius: 50px; font-weight: 800; border: 1px solid #1e7e34; font-size: 0.85rem; }

        /* --- BANNER DE INFO (Scripts) --- */
        ", root_sel, " .info-banner-blue { background: rgba(0, 212, 255, 0.05); border-left: 5px solid #00d4ff; padding: 15px; margin-bottom: 20px; border-radius: 0 8px 8px 0; }

        /* --- CONTENEDOR MAPA --- */
        ", root_sel, " .map-wrapper { background: #000; border-radius: 20px; overflow: hidden; border: 4px solid #1a202c; height: 650px; position: relative; }
      ")))
    ),

    div(class = paste("container-fluid", ns("tools-container")),
        # FILA 0: HEADER
        div(class = "row", div(class = "col-12", uiOutput(ns("tools_header")))),
        br(),

        # FILA 1: PANEL DE CONTROL
        div(class = "row g-4 align-items-start",
            div(class = "col-md-7",
                div(id = ns("main_tools_col"), class = "lock-wrapper",
                    div(class = "section-label", icon("route"), " Current Tool Path"),
                    div(class = "path-display-area", uiOutput(ns("path_chips_ui")))
                )
            ),
            div(class = "col-md-1"),
            div(class = "col-md-4",
                div(class = "action-row-right",
                    actionButton(ns("btn_confirm"), span(icon("check"), "Confirm"), class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_edit"),    span(icon("edit"), "Edit"),     class = "btn-warning btn-pill-xl"),
                    actionButton(ns("btn_reset"),   span(icon("sync"), "Reset"),    class = "btn-primary btn-pill-xl")
                )
            )
        ),

        div(style = "border-top: 4px solid #00d4ff; margin: 35px 0;"),

        # FILA 2: INFO BANNER
        uiOutput(ns("scripts_info_banner")),

        # FILA 3: EL MAPA
        div(class = "row",
            div(class = "col-12",
                div(class = "section-label mb-3", icon("sitemap"), " Tool Selection Tree"),
                div(class = "map-wrapper", mod_tree_ui(ns("inner_tree")))
            )
        )
    )
  )
}


mod_tools_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Motor D3 (Hijo)
    arbol_res <- mod_tree_server("inner_tree", show_debug = FALSE)
    is_locked <- reactiveVal(FALSE)

    # Lógica de bloqueo UI
    toggle_ui <- function(lock) {
      if(lock) {
        shinyjs::disable("btn_confirm")
        shinyjs::addClass(id = "main_tools_col", class = "locked-disabled")
      } else {
        shinyjs::enable("btn_confirm")
        shinyjs::removeClass(id = "main_tools_col", class = "locked-disabled")
      }
      is_locked(lock)
    }

    # Header Dinámico (Estilo Import)
    output$tools_header <- renderUI({
      res <- arbol_res()
      if (is_locked()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" TOOL LOCKED:", res$node_name)),
            span(class = "header-id", "STATUS: READY"))
      } else if (res$node_name != "Rscience") {
        div(class = "selection-header active-selection",
            span(icon("file-import"), paste(" SELECTED:", res$node_name)),
            span(class = "header-id", "STATUS: PENDING"))
      } else {
        div(class = "selection-header waiting-mode",
            span(icon("bolt"), " Waiting for tool selection..."),
            span(class = "header-id", "STATUS: WAITING"))
      }
    })

    # Chips de Path
    output$path_chips_ui <- renderUI({
      path_string <- arbol_res()$path
      parts <- unlist(strsplit(path_string, " / "))
      if(length(parts) <= 1) return(span("Navigate tree to select...", style="color:#718096; italic;"))
      lapply(parts, function(p) span(class = "path-chip", p))
    })

    # Banner de Scripts (Estilo File Info)
    output$scripts_info_banner <- renderUI({
      res <- arbol_res()
      req(length(res$scripts) > 0)
      div(class = "info-banner-blue", fluidRow(
        column(6, span(style="font-weight:900; color:#00d4ff; font-size:1.1rem;", "TOOL: "), span(style="font-size:1.1rem; font-weight:600;", res$node_name)),
        column(3, span(style="font-weight:900; color:#00d4ff;", "SCRIPTS: "), span(length(res$scripts))),
        column(3, span(style="font-weight:900; color:#00d4ff;", "READY: "), span(if(is_locked()) "YES" else "NO"))
      ))
    })

    # Eventos de Botones
    observeEvent(input$btn_confirm, {
      if(arbol_res()$node_name != "Rscience") toggle_ui(TRUE)
    })

    observeEvent(input$btn_edit, toggle_ui(FALSE))

    observeEvent(input$btn_reset, {
      toggle_ui(FALSE)
      # Aquí podrías añadir un mensaje a mod_tree para resetear el zoom
    })

    return(reactive({
      list(confirmado = is_locked(), datos = arbol_res())
    }))
  })
}


# ui <- fluidPage(
#   mod_tools_ui("my_selector")
# )
#
# server <- function(input, output, session) {
#   # Resultado final de todo el flujo
#   resultado_final <- mod_tools_server("my_selector")
#
#   observe({
#     req(resultado_final()$confirmado)
#     print("--- SELECCIÓN FINAL RECIBIDA EN APP ---")
#     print(resultado_final()$datos$path)
#   })
# }
#
# shinyApp(ui, server)
