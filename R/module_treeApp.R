# ==============================================================================
# TOOLS SELECTOR MODULE - v.0.1.0 (UNIFIED ESTHETIC)
# ==============================================================================
library("bslib")
library("shiny")
library("shinyjs")

# ==============================================================================
# TOOLS SELECTOR MODULE - v.0.1.2 (REORDERED ROWS: HEADER -> FILTER -> MAP)
# ==============================================================================

module_treeApp_UI <- function(id) {
  ns <- NS(id)
  root_id <- paste0("#", ns("tree-container"))
  root_sel <- paste0(".", ns("tree-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(paste0("
      /* --- HEADER Y BOTONES (Consistentes con v.0.1.1) --- */
      ", root_sel, " .selection-header { display: flex; justify-content: space-between; align-items: center; padding: 15px 25px; border-radius: 12px; transition: all 0.4s ease; box-shadow: 0 4px 12px rgba(0,0,0,0.05); margin-bottom: 20px; }
      ", root_sel, " .selection-header.waiting-mode { background: #f0fdff; border: 1px solid #00cfd4; color: #008184; }
      ", root_sel, " .selection-header.active-selection { background: #fff9f0; border: 1px solid #ff9100; color: #b36600; }
      ", root_sel, " .selection-header.confirmed { background: #f6fff8; border: 1px solid #28a745; color: #1e7e34; }

      ", root_sel, " .btn.btn-pill-xl { border-radius: 50px !important; padding: 12px 25px !important; font-weight: 800 !important; font-size: 0.9rem !important; text-transform: uppercase !important; display: inline-flex !important; align-items: center !important; justify-content: center !important; gap: 8px !important; transition: all 0.3s ease !important; }

      /* --- FILA DE FILTROS (Chips horizontales) --- */
      ", root_sel, " .filter-row-container { background: white; border-radius: 15px; padding: 20px; border: 1px solid #eee; margin-bottom: 20px; display: flex; align-items: center; gap: 15px; flex-wrap: wrap; }
      ", root_sel, " .path-chip { background: #f0fdf4; color: #28a745; padding: 6px 15px; border-radius: 50px; font-weight: 800; border: 1px solid #bbf7d0; font-size: 0.85rem; }

      /* --- MAPA Y BLOQUEO --- */
      ", root_sel, " .map-wrapper { background: white; border-radius: 25px; padding: 20px; border: 1px solid #eee; position: relative; height: 750px; overflow: hidden; }
      #", ns("map_lock"), " { position:absolute; top:0; left:0; width:100%; height:100%; background: rgba(240, 240, 240, 0.4); backdrop-filter: blur(2px); z-index:100; cursor:not-allowed; border-radius:25px; display:none; }

      /* --- D3 HIGHLIGHTS --- */
      ", root_id, " .nodo-ruta-activa circle { fill: #ff9100 !important; stroke: #b36600 !important; stroke-width: 4px !important; }
      ", root_id, " .link-ruta-activa { stroke: #ff9100 !important; stroke-width: 5px !important; }
    ")))),

    div(id = ns("tree-container"), class = paste("container-fluid", ns("tree-container")),

        # FILA 1: HEADER (STATUS)
        div(class = "row", div(class = "col-12", uiOutput(ns("selection_header")))),

        # FILA 2: NAVIGATION FILTER & ACTIONS
        div(class = "row align-items-center",
            div(class = "col-md-8",
                div(class = "filter-row-container",
                    span(style="font-weight:900; color:#cbd5e0; text-transform:uppercase; font-size:0.8rem;", "Path:"),
                    uiOutput(ns("tree_selection_ui"))
                )
            ),
            div(class = "col-md-4 text-end",
                div(class = "d-flex gap-2 justify-content-end",
                    actionButton(ns("btn_select"), span(icon("check"), "Confirm"), class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),    class = "btn-warning btn-pill-xl"),
                    actionButton(ns("btn_reset_all"), span(icon("sync"), "Reset"), class = "btn-primary btn-pill-xl")
                )
            )
        ),

        # FILA 3: MAPA DE NODOS (FULL WIDTH)
        div(class = "row",
            div(class = "col-12",
                div(class = "map-wrapper",
                    div(id = ns("map_lock")),
                    collapsibleTree::collapsibleTreeOutput(ns("stat_tree"), height = "700px")
                )
            )
        ),

        # FILA EXTRA: IDENTIFIER (DEBUG)
        div(class = "row mt-3",
            div(class = "col-12",
                div(style="background: #ffffff; padding: 10px 20px; border-radius: 10px; border: 1px solid #eee; display: flex; align-items: center; gap: 20px;",
                    span(style="font-weight:800; color:#555; font-size:0.8rem;", "TARGET SCRIPT:"),
                    uiOutput(ns("debug_ui"))
                )
            )
        )
    ),

    # --- SCRIPT D3 (Recuperado de v.0.1.1) ---
    tags$script(HTML(paste0("
      (function() {
        var lastSelectedNode = null;
        var activePathNames = ['Rscience'];
        var treeId = '", ns("stat_tree"), "';
        var containerId = '", ns("tree-container"), "';

        function applyVisuals() {
          var container = d3.select('#' + containerId);
          var allNodes = container.select('#' + treeId).selectAll('g.node');
          allNodes.classed('nodo-ruta-activa', function(d) { return activePathNames.includes(d.data.name); });
          allNodes.classed('nodo-faded', function(d) {
            if (activePathNames.length <= 1) return false;
            return !activePathNames.includes(d.data.name);
          });
          container.select('#' + treeId).selectAll('path.link')
            .classed('link-ruta-activa', function(d) { return activePathNames.includes(d.source.data.name) && activePathNames.includes(d.target.data.name); });
        }

        $(document).on('shiny:visualchange', function(event) {
          if (event.target.id === treeId) {
            setTimeout(applyVisuals, 200);
            d3.select('#' + treeId).selectAll('circle.node').on('click.rscience', function(d) {
              activePathNames = [];
              lastSelectedNode = d.data.name;
              var current = d;
              while (current) { activePathNames.push(current.data.name); current = current.parent; }
              setTimeout(applyVisuals, 50);
            });
          }
        });
      })();
    ")))
  )
}

module_treeApp_Server <- function(id, data = tree_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_confirmed <- reactiveVal(FALSE)

    # --- RESET LOGIC ---
    observeEvent(input$btn_reset_all, {
      is_confirmed(FALSE)
      shinyjs::hide("map_lock")
      shinyjs::enable("btn_select")
      # Aquí podrías añadir lógica para resetear el input$node_click si fuera posible,
      # pero collapsibleTree es de solo lectura desde Shiny mayormente.
    })

    output$stat_tree <- collapsibleTree::renderCollapsibleTree({
      collapsibleTree::collapsibleTree(data, hierarchy = c("nivel1", "nivel2", "nivel3", "nivel4", "nivel5"),
                                       root = "Rscience", inputId = ns("node_click"), linkLength = 150,
                                       fill = "#ffffff", fillClosed = "#28a745", nodeSize = "size", zoomable = TRUE)
    })

    current_script_id <- reactive({
      path <- input$node_click; if (is.null(path) || length(path) == 0) return("---")
      p <- rev(path); res <- data
      try({
        if(length(p) >= 1) res <- res %>% dplyr::filter(nivel1 == p[1])
        if(length(p) >= 2) res <- res %>% dplyr::filter(nivel2 == p[2])
        if(length(p) >= 3) res <- res %>% dplyr::filter(nivel3 == p[3])
        if(length(p) >= 4) res <- res %>% dplyr::filter(nivel4 == p[4])
        if(length(p) >= 5) res <- res %>% dplyr::filter(nivel5 == p[5])
      }, silent = TRUE)
      if(nrow(res) > 0) res$script_id[1] else "..."
    })

    output$selection_header <- renderUI({
      path <- input$node_click
      if (is_confirmed()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" TOOL READY:", paste(rev(path), collapse = " > "))),
            span(class = "header-id", paste("SCRIPT:", current_script_id())))
      } else if (!is.null(path) && length(path) > 0) {
        div(class = "selection-header active-selection",
            span(icon("bolt"), paste(" PENDING CONFIRMATION:", paste(rev(path), collapse = " > "))),
            span(class = "header-id", paste("ID:", current_script_id())))
      } else {
        div(class = "selection-header waiting-mode",
            span(icon("mouse-pointer"), " Waiting for tool selection..."),
            span(class = "header-id", "STATUS: IDLE"))
      }
    })

    output$tree_selection_ui <- renderUI({
      path <- input$node_click; if (is.null(path) || length(path) == 0) return(p("Start navigation in the tree..."))
      lapply(seq_along(rev(path)), function(i) div(class = "path-chip", span(style="color:#aaa; font-size:0.7rem; margin-right:10px;", paste0("LVL ", i)), rev(path)[i]))
    })

    output$debug_ui <- renderUI({
      path <- input$node_click; if (is.null(path) || length(path) == 0) return(div(class="debug-box", "STANDBY"))
      div(class="debug-box", div(">> TARGET SCRIPT:"), span(class="script-val", current_script_id()))
    })

    observeEvent(input$btn_select, { if(!is.null(input$node_click)) { is_confirmed(TRUE); shinyjs::show("map_lock"); shinyjs::disable("btn_select") } })
    observeEvent(input$btn_edit, { is_confirmed(FALSE); shinyjs::hide("map_lock"); shinyjs::enable("btn_select") })

  })
}
