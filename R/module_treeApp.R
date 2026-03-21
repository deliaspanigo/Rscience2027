library("bslib")
library("shiny")
library("shinyjs")

#' Módulo UI de Rscience Tree
#' @param id Identificador del módulo
#' @export
module_treeApp_UI <- function(id) {
  ns <- NS(id)

  # Selector raíz para el blindaje CSS
  root_id <- paste0("#", ns("tree-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(paste0("
      /* --- BLINDAJE CSS PARA RSCIENCE TREE --- */

      ", root_id, " .map-container { background: white; border-radius: 25px; padding: 20px; border: 1px solid #eee; position: relative; height: 750px; overflow: hidden; }
      ", root_id, " .map-controls { position: absolute; top: 20px; right: 20px; z-index: 101; display: flex; flex-direction: column; gap: 10px; }

      ", root_id, " .btn-map-tool { width: 45px; height: 45px; border-radius: 50% !important; border: none; display: flex; align-items: center; justify-content: center; box-shadow: 0 4px 10px rgba(0,0,0,0.1); color: white; }
      ", root_id, " .btn-center-view { background-color: #00d4ff; }
      ", root_id, " .btn-reset-map { background-color: #ffc107; }

      ", root_id, " .side-panel { background: white; border-radius: 25px; padding: 30px; border: 1px solid #eee; min-height: 750px; }
      ", root_id, " .path-chip { background: #f0fdf4; color: #28a745; padding: 12px; border-radius: 12px; margin-bottom: 8px; font-weight: 800; border: 1px solid #bbf7d0; display: block; }

      ", root_id, " .debug-box { background: #121212; color: #00ff00; padding: 20px; border-radius: 12px; font-family: monospace; margin-top: 20px; border-left: 6px solid #00d4ff; }
      ", root_id, " .script-val { color: #ffffff; font-size: 1.4rem; font-weight: 900; display: block; margin-top: 10px; }

      ", root_id, " .selection-header { min-height: 60px; background: #f8f9fa; color: #718096; padding: 15px 25px; border-radius: 15px; margin-bottom: 15px; font-weight: 700; display: flex; justify-content: space-between; align-items: center; border: 2px dashed #cbd5e0; transition: all 0.3s ease; }
      ", root_id, " .selection-header.active-selection { background: #fff7ed; color: #ea580c; border: 2px solid #ff9800; }
      ", root_id, " .selection-header.confirmed { background: #00d4ff; color: white; border: 2px solid #00d4ff; box-shadow: 0 4px 15px rgba(0,212,255,0.2); }

      ", root_id, " .header-id { background: #edf2f7; color: #718096; padding: 5px 15px; border-radius: 50px; font-size: 0.9rem; font-weight: 800; }
      ", root_id, " .confirmed .header-id { background: white; color: #00d4ff; }

      /* Estilos específicos del SVG del árbol (D3) */
      ", root_id, " .nodo-ruta-activa circle { fill: #ff9800 !important; stroke: #e65100 !important; stroke-width: 4px !important; opacity: 1 !important; }
      ", root_id, " .nodo-faded { opacity: 0.1 !important; pointer-events: none; transition: opacity 0.4s ease; }
      ", root_id, " .link-ruta-activa { stroke: #ff9800 !important; stroke-width: 5px !important; opacity: 1 !important; }
      ", root_id, " .link-faded { opacity: 0.05 !important; transition: opacity 0.4s ease; }

      #", ns("map_lock"), " { position:absolute; top:0; left:0; width:100%; height:100%; background: rgba(44, 62, 80, 0.4); backdrop-filter: blur(2px); z-index:100; cursor:not-allowed; border-radius:25px; display:none; }
    ")))),

    # CONTENEDOR RAÍZ CON ID ÚNICO
    div(id = ns("tree-container"), class = "container-fluid",
        fluidRow(
          column(8,
                 uiOutput(ns("selection_header")),
                 div(class = "map-container",
                     div(class = "map-controls",
                         actionButton(ns("btn_center"), icon("compress-arrows-alt"), class = "btn-map-tool btn-center-view"),
                         actionButton(ns("btn_clear_map"), icon("sync-alt"), class = "btn-map-tool btn-reset-map")
                     ),
                     div(id = ns("map_lock")),
                     collapsibleTree::collapsibleTreeOutput(ns("stat_tree"), height = "700px")
                 )
          ),
          column(4,
                 div(class = "side-panel",
                     h5("FILTRO DE NAVEGACIÓN", style="font-weight:900; color:#cbd5e0; margin-bottom:20px;"),
                     uiOutput(ns("tree_selection_ui")),
                     hr(),
                     h6("IDENTIFICADOR RSCIENCE", style="font-weight:800; color:#555;"),
                     uiOutput(ns("debug_ui")),
                     div(style="margin-top:30px;",
                         actionButton(ns("btn_select"), "Confirmar Script", class = "btn-success", style="width:100%; border-radius:50px; font-weight:800; padding: 15px; margin-bottom:10px;"),
                         actionButton(ns("btn_edit"), "Editar / Desbloquear", class = "btn-warning", style="width:100%; border-radius:50px; font-weight:800; padding: 15px; margin-bottom:10px;"),
                         actionButton(ns("btn_reset_all"), "Reiniciar App", class = "btn-danger", style="width:100%; border-radius:50px; font-weight:800; padding: 15px;")
                     )
                 )
          )
        )
    ),

    # Script de D3.js adaptado para trabajar solo dentro de este módulo
    tags$script(HTML(paste0("
      (function() {
        var lastSelectedNode = null;
        var activePathNames = ['Rscience'];
        var treeId = '", ns("stat_tree"), "';
        var containerId = '", ns("tree-container"), "';

        function applyVisuals() {
          var container = d3.select('#' + containerId);
          function isDescendant(parentName, child) {
            var node = child;
            while (node) { if (node.data.name === parentName) return true; node = node.parent; }
            return false;
          }
          var allNodes = container.select('#' + treeId).selectAll('g.node');
          allNodes.classed('nodo-ruta-activa', function(d) { return activePathNames.includes(d.data.name); });
          allNodes.classed('nodo-faded', function(d) {
            if (!lastSelectedNode) return false;
            return !activePathNames.includes(d.data.name) && !isDescendant(lastSelectedNode, d);
          });
          container.select('#' + treeId).selectAll('path.link')
            .classed('link-ruta-activa', function(d) { return activePathNames.includes(d.source.data.name) && activePathNames.includes(d.target.data.name); })
            .classed('link-faded', function(d) {
               if (!lastSelectedNode) return false;
               return !activePathNames.includes(d.target.data.name) && !isDescendant(lastSelectedNode, d.target);
            });
        }

        $(document).on('shiny:visualchange', function(event) {
          if (event.target.id === treeId) {
            setTimeout(applyVisuals, 150);
            d3.select('#' + treeId).selectAll('circle.node').on('click.rscience', function(d) {
              activePathNames = [];
              lastSelectedNode = d.data.name;
              var current = d;
              while (current) { activePathNames.push(current.data.name); current = current.parent; }
              setTimeout(applyVisuals, 50);
            });
            var targetNode = document.getElementById(treeId);
            if(targetNode) {
              var observer = new MutationObserver(function() { window.requestAnimationFrame(applyVisuals); });
              observer.observe(targetNode, { childList: true, subtree: true });
            }
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
            span(icon("lock"), paste(" FINALIZADO:", paste(rev(path), collapse = " > "))),
            span(class = "header-id", paste("SCRIPT:", current_script_id())))
      } else if (!is.null(path) && length(path) > 0) {
        div(class = "selection-header active-selection",
            span(icon("location-arrow"), paste(" SELECCIÓN:", paste(rev(path), collapse = " > "))),
            span(class = "header-id", paste("ID:", current_script_id())))
      } else {
        div(class = "selection-header",
            span(icon("search"), " Navegando... Seleccione un nodo para enfocar ruta"),
            span(class = "header-id", "ID: ---"))
      }
    })

    output$tree_selection_ui <- renderUI({
      path <- input$node_click; if (is.null(path) || length(path) == 0) return(p("Inicie la navegación..."))
      lapply(seq_along(rev(path)), function(i) div(class = "path-chip", span(style="color:#aaa; font-size:0.7rem; margin-right:10px;", paste0("LVL ", i)), rev(path)[i]))
    })

    output$debug_ui <- renderUI({
      path <- input$node_click; if (is.null(path) || length(path) == 0) return(div(class="debug-box", "STANDBY"))
      div(class="debug-box", div(">> TARGET: ", rev(path)[length(path)]), span(class="script-val", current_script_id()))
    })

    observeEvent(input$btn_select, { if(!is.null(input$node_click)) { is_confirmed(TRUE); shinyjs::show("map_lock"); shinyjs::disable("btn_select") } })
    observeEvent(input$btn_edit, { is_confirmed(FALSE); shinyjs::hide("map_lock"); shinyjs::enable("btn_select") })
    observeEvent(input$btn_reset_all, { session$reload() })
    observeEvent(input$btn_center, { session$reload() })
  })
}
