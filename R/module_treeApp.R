# ==============================================================================
# TOOLS SELECTOR MODULE UI - v.0.3.3 (SYNC ENGINE REINFORCED & TARGET PURGE)
# ==============================================================================
library("bslib")
library("shiny")
library("shinyjs")
library("dplyr")
library("collapsibleTree")

# ==============================================================================
# TOOLS SELECTOR MODULE UI - v.0.4.6 (FULL INTERFACE RESTORED)
# ==============================================================================
# ==============================================================================
# TOOLS SELECTOR MODULE UI - v.0.4.7 (ISOLATE PATH CHECKBOX)
# ==============================================================================
# ==============================================================================
# TOOLS SELECTOR MODULE UI - v.0.4.8 (STRICT LINK LOGIC & DUAL CONTROLS)
# ==============================================================================
module_treeApp_UI <- function(id) {
  ns <- NS(id)
  root_id <- paste0("#", ns("tree-container"))
  root_sel <- paste0(".", ns("tree-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(paste0("
      /* --- FILA 1: HEADER --- */
      ", root_sel, " .selection-header { display: flex; justify-content: space-between; align-items: center; padding: 15px 25px; border-radius: 12px; margin-bottom: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); }
      ", root_sel, " .selection-header.active-selection { background: #fff9f0; border: 1px solid #ff9100; color: #b36600; }

      /* --- FILA 2: CONTROLES (DARK) --- */
      ", root_sel, " .filter-row-container { background: #1a202c; border-radius: 15px; padding: 15px 25px; border: 1px solid #2d3748; display: flex; align-items: center; gap: 12px; min-height: 70px; }
      ", root_sel, " .path-chip { background: #28a745; color: white !important; padding: 6px 15px; border-radius: 50px; font-weight: 800; border: 1px solid #1e7e34; }

      /* Controles de Checkbox */
      ", root_sel, " .isolate-control { background: #2d3748; padding: 5px 12px; border-radius: 50px; color: #a0aec0; font-size: 0.75rem; font-weight: 700; display: flex; align-items: center; border: 1px solid #4a5568; white-space: nowrap; }
      ", root_sel, " .isolate-control input { margin-right: 5px; }

      /* --- FILA 3: LISTA DE SCRIPTS --- */
      ", root_sel, " .scripts-container { background: #f8f9fa; border-radius: 15px; border: 1px dashed #cbd5e0; padding: 15px; margin-bottom: 20px; min-height: 50px; }

      /* --- FILA 4: MAPA Y D3 VISUALS --- */
      ", root_sel, " .map-wrapper { background: white; border-radius: 25px; padding: 20px; border: 1px solid #eee; height: 750px; position: relative; }

      ", root_id, " path.link { stroke: #000000 !important; stroke-width: 4px !important; opacity: 0.8 !important; fill: none !important; }
      ", root_id, " g.node.nodo-ruta-activa circle { fill: #ff9100 !important; stroke: #b36600 !important; stroke-width: 3px !important; }

      /* LÓGICA DE VISIBILIDAD DINÁMICA */
      ", root_id, ".isolate-active g.node:not(.nodo-ruta-activa) { opacity: 0 !important; pointer-events: none !important; }
      ", root_id, ".strict-active path.link:not(.link-path-elegido) { opacity: 0 !important; pointer-events: none !important; }
    ")))),

    div(id = ns("tree-container"), class = paste("container-fluid", ns("tree-container")),
        # 1. Header
        div(class = "row", div(class = "col-12", uiOutput(ns("selection_header")))),

        # 2. Path e Interruptores
        div(class = "row align-items-center mb-3",
            div(class = "col-md-6",
                div(class = "filter-row-container",
                    span(style="color:#cbd5e0; font-weight:900; font-size:0.7rem;", "PATH:"),
                    uiOutput(ns("tree_selection_ui"))
                )
            ),
            div(class = "col-md-6 text-end",
                div(class = "d-flex gap-2 justify-content-end align-items-center",
                    div(class = "isolate-control", checkboxInput(ns("cb_isolate"), "ISOLATE NODES", value = FALSE)),
                    div(class = "isolate-control", checkboxInput(ns("cb_strict"), "STRICT LINKS", value = FALSE)),
                    actionButton(ns("btn_select"), "Confirm", class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_reset_all"), "Reset", class = "btn-primary btn-pill-xl")
                )
            )
        ),

        # 3. Scripts
        div(class = "row", div(class = "col-12", div(class = "scripts-container", uiOutput(ns("full_script_list_ui"))))),

        # 4. Mapa
        div(class = "row", div(class = "col-12", div(class = "map-wrapper", collapsibleTree::collapsibleTreeOutput(ns("stat_tree"), height = "700px"))))
    ),

    tags$script(HTML(paste0("
      (function() {
        var activeNames = ['Rscience'];
        var treeId = '", ns("stat_tree"), "';
        var containerId = '", ns("tree-container"), "';

        function refreshStyles() {
          var svg = d3.select('#' + treeId);
          var isIsolate = $('#", ns("cb_isolate"), "').is(':checked');
          var isStrict = $('#", ns("cb_strict"), "').is(':checked');

          $('#' + containerId).toggleClass('isolate-active', isIsolate);
          $('#' + containerId).toggleClass('strict-active', isStrict);

          // 1. Nodos
          svg.selectAll('g.node').each(function(d) {
            var isActive = d.data && activeNames.indexOf(d.data.name) !== -1;
            d3.select(this).classed('nodo-ruta-activa', isActive);
          });

          // 2. Conectores con Regla de Doble Extremo
          svg.selectAll('path.link').each(function(d) {
             var sourceActive = d.source && d.source.data && activeNames.indexOf(d.source.data.name) !== -1;
             var targetActive = d.target && d.target.data && activeNames.indexOf(d.target.data.name) !== -1;

             // Un link es de 'path elegido' solo si AMBOS extremos están en el path
             var isPathLink = sourceActive && targetActive;

             if (isPathLink) {
                d3.select(this)
                  .classed('link-path-elegido', true)
                  .style('stroke', '#ff9100', 'important').style('stroke-width', '10px', 'important').style('opacity', '1', 'important');
             } else {
                d3.select(this)
                  .classed('link-path-elegido', false)
                  .style('stroke', '#000000', 'important').style('stroke-width', '4px', 'important').style('opacity', '0.8', 'important');
             }
          });
        }

        $(document).on('shiny:visualchange', function(e) {
          if (e.target.id === treeId) {
            var loop = setInterval(refreshStyles, 60);
            setTimeout(function() { clearInterval(loop); }, 1800);

            d3.select('#' + treeId).selectAll('circle').on('click.pathFix', function(d) {
              activeNames = [];
              var curr = d;
              while (curr) {
                if (curr.data) activeNames.push(curr.data.name);
                curr = curr.parent;
              }
              refreshStyles();
            });
          }
        });

        $(document).on('change', '#", ns("cb_isolate"), ", #", ns("cb_strict"), "', refreshStyles);
      })();
    ")))
  )
}



module_treeApp_Server <- function(id, data = tree_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_confirmed <- reactiveVal(FALSE)

    filtered_data <- reactive({
      path <- input$node_click
      if (is.null(path) || length(path) == 0) return(NULL)
      p <- rev(path); res <- data
      try({
        if(length(p) >= 1) res <- res %>% filter(nivel1 == p[1])
        if(length(p) >= 2) res <- res %>% filter(nivel2 == p[2])
        if(length(p) >= 3) res <- res %>% filter(nivel3 == p[3])
        if(length(p) >= 4) res <- res %>% filter(nivel4 == p[4])
        if(length(p) >= 5) res <- res %>% filter(nivel5 == p[5])
      }, silent = TRUE)
      res
    })

    output$stat_tree <- collapsibleTree::renderCollapsibleTree({
      collapsibleTree::collapsibleTree(data, hierarchy = c("nivel1", "nivel2", "nivel3", "nivel4", "nivel5"),
                                       root = "Rscience", inputId = ns("node_click"), linkLength = 150,
                                       fill = "#ffffff", fillClosed = "#28a745", nodeSize = "size",
                                       zoomable = TRUE, fontSize = 16)
    })

    output$selection_header <- renderUI({
      path <- input$node_click
      if (is_confirmed()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" TOOL LOCKED:", paste(rev(path), collapse = " > "))),
            span(class = "header-id", "STATUS: READY"))
      } else if (!is.null(path) && length(path) > 0) {
        div(class = "selection-header active-selection",
            span(icon("file-import"), paste(" SELECTED:", paste(rev(path), collapse = " > "))),
            span(class = "header-id", "STATUS: PENDING"))
      } else {
        div(class = "selection-header waiting-mode",
            span(icon("bolt"), " Waiting for tool selection..."),
            span(class = "header-id", "STATUS: WAITING"))
      }
    })

    output$tree_selection_ui <- renderUI({
      path <- input$node_click
      if (is.null(path) || length(path) == 0) return(span("Start navigating...", style="color:#4a5568;"))
      lapply(seq_along(rev(path)), function(i) div(class = "path-chip", rev(path)[i]))
    })

    output$full_script_list_ui <- renderUI({
      df <- filtered_data()
      if (is.null(df)) return(span("No selection yet.", style="color:#aaa; font-style:italic;"))
      ids <- unique(df$script_id)
      div(style="display:flex; flex-wrap:wrap; gap:8px;",
          lapply(ids, function(id) span(id, style="background:#fff; color:#2d3748; border:1px solid #dee2e6; font-family:monospace; font-size:0.75rem; padding:3px 10px; border-radius:5px; font-weight:bold;")))
    })

    observeEvent(input$cb_isolate, { session$sendCustomMessage("refresh_visuals", list()) })
    observeEvent(input$btn_reset_all, {
      is_confirmed(FALSE)
      shinyjs::hide("map_lock")
      updateCheckboxInput(session, "cb_isolate", value = FALSE)
      session$sendCustomMessage("reset_tree_visuals", list())
    })
    observeEvent(input$btn_select, { if(!is.null(input$node_click)) { is_confirmed(TRUE); shinyjs::show("map_lock") } })
    observeEvent(input$btn_edit, { is_confirmed(FALSE); shinyjs::hide("map_lock") })
  })
}
