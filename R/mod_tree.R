# ==========================================
# MÓDULO: EXPLORADOR DE ÁRBOL RScience
# ==========================================
library(shiny)
library(dplyr)
library(jsonlite)

# ==========================================
# MÓDULO: EXPLORADOR DE ÁRBOL RScience (UI)
# ==========================================

mod_tree_ui <- function(id) {
  ns <- NS(id)
  module_id <- ns("module-wrapper")

  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/d3@6/dist/d3.min.js"),
      tags$style(HTML(paste0("
        /* Contenedor principal: FLEXBOX vertical */
        #", module_id, " {
          height: 100%; /* CAMBIO: De 100vh a 100% para independencia total */
          width: 100%;
          background-color: #000;
          margin: 0;
          padding: 0;
          display: flex;
          flex-direction: column;
          overflow: hidden;
          font-family: 'Segoe UI', sans-serif;
        }

        /* Consola de estado */
        #", module_id, " .status-console {
          background: #0a1f2a;
          color: #ADFF2F;
          border-bottom: 2px solid #00aaff;
          margin: 0;
          padding: 10px 20px;
          font-family: 'Courier New', monospace;
          font-size: 13px;
          flex-shrink: 0;
          z-index: 1002;
        }

        #", module_id, " .status-console pre {
          background: transparent;
          border: none;
          color: inherit;
          margin: 0;
          padding: 0;
        }

        /* Área de visualización */
        #", module_id, " .viewport-area {
          flex-grow: 1;
          position: relative;
          overflow: hidden;
          width: 100%;
          height: 100%;
        }

        #", module_id, " #tree-container {
          position: absolute;
          inset: 0;
          height: 100%;
          width: 100%;
        }

        /* Paneles de Control */
        #", module_id, " .panel-ctrl {
          position: absolute;
          z-index: 1000;
          background: rgba(10, 20, 25, 0.92);
          border: 2px solid #00FFFF;
          border-radius: 12px;
          color: #00FFFF;
          padding: 16px;
          box-shadow: 0 0 25px rgba(0, 255, 255, 0.25);
          transition: all 0.5s cubic-bezier(0.4, 0, 0.2, 1);
        }

        #", module_id, " .panel-left  { top: 20px; left: 20px;  width: 220px; }
        #", module_id, " .panel-right { top: 20px; right: 20px; width: 260px; }

        /* Estilos de Nodos y Links */
        #", module_id, " .node text {
          pointer-events: none;
          fill: #fff;
          font-size: 20px !important;
          font-weight: 900;
          text-shadow: 3px 3px 8px #000;
        }
        #", module_id, " .link { fill: none; }

        /* Botones de Comando */
        #", module_id, " .btn-cmd {
          background: transparent;
          color: #00FFFF;
          border: 1px solid #00FFFF;
          padding: 10px;
          width: 100%;
          font-weight: bold;
          cursor: pointer;
          margin-bottom: 10px;
          border-radius: 6px;
          text-transform: uppercase;
          transition: all 0.3s;
        }
        #", module_id, " .btn-cmd:hover {
          background: #00FFFF;
          color: #000;
          box-shadow: 0 0 15px #00FFFF;
        }

        /* Títulos de Paneles */
        .panel-title {
          font-weight: bold;
          margin-bottom: 15px;
          border-bottom: 1px solid rgba(0, 255, 255, 0.3);
          padding-bottom: 5px;
          letter-spacing: 1px;
        }

        #", module_id, " .collapsed { opacity: 0.1; pointer-events: none; }
      ")))
    ),

    div(id = module_id,
        # 1. Consola (Sección independiente arriba)
        uiOutput(ns("debug_ui_wrapper")),

        # 2. Área de Visualización
        div(class = "viewport-area",
            div(id = "tree-container"),

            # Panel izquierdo: ACTIONS
            div(id = "panel-left", class = "panel-ctrl panel-left",
                tags$button(id = "btn-toggle-left",
                            style="position:absolute; top:5px; right:5px; background:none; border:none; color:#0ff; cursor:pointer;",
                            "✖", onclick = "togglePanel('left')"),
                div(class = "panel-title", "⚡ ACTIONS"),
                tags$button("↺ Reset View", class = "btn-cmd", onclick = "resetMap()"),
                tags$button("🎯 Focus Branch", class = "btn-cmd", style="border-color:#ADFF2F; color:#ADFF2F;", onclick = "runCollapseOthers()"),
                tags$button("⇱ Expand All", class = "btn-cmd", onclick = "fullExpand()"),

                # BOTÓN DE CAPTURA
                tags$button("📸 Capture SVG", class = "btn-cmd",
                            style="border-color:#FF00FF; color:#FF00FF; margin-top: 10px;",
                            onclick = "captureTree()")
            ),

            # Panel derecho: VIEW MODES
            div(id = "panel-right", class = "panel-ctrl panel-right",
                tags$button(id = "btn-toggle-right",
                            style="position:absolute; top:5px; left:5px; background:none; border:none; color:#0ff; cursor:pointer;",
                            "✖", onclick = "togglePanel('right')"),
                div(class = "panel-title", "🛠️ VIEW MODES"),
                div(style="margin-bottom:15px; display:flex; justify-content:space-between; align-items:center;",
                    "GHOST MODE",
                    tags$input(type="checkbox", onclick="toggleGhost()", style="cursor:pointer; width:18px; height:18px;")),
                div(style="display:flex; justify-content:space-between; align-items:center;",
                    "TOP ALIGN",
                    tags$input(type="checkbox", onclick="toggleTopAlign()", style="cursor:pointer; width:18px; height:18px;"))
            )
        )
    ),
    # Inyector de lógica JS
    uiOutput(ns("js_injector"))
  )
}

mod_tree_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Carga de datos
    data_full <- tryCatch({
      Rscience2027::tree_data
    }, error = function(e) {
      data.frame(nivel1 = "Error", nivel2 = "No Data", script_id = 0)
    })

    # 1. OBJETO REACTIVO PARA LA APP PRINCIPAL
    info_nodo <- reactive({
      node_name <- if (is.null(input$selected_node) ||
                       input$selected_node == "" ||
                       input$selected_node == "Rscience") {
        "Rscience"
      } else {
        input$selected_node
      }

      if (node_name == "Rscience") {
        path_final <- "Rscience (Root)"
        scripts <- unique(data_full$script_id)
      } else {
        row_match <- data_full %>% filter(if_any(starts_with("nivel"), ~ .x == node_name))
        if (nrow(row_match) > 0) {
          sample_row <- row_match %>% slice(1)
          niveles_all <- sample_row %>% select(starts_with("nivel")) %>% as.character()
          niveles_clean <- niveles_all[!is.na(niveles_all) & niveles_all != "" & niveles_all != "NA"]
          idx_actual <- which(niveles_clean == node_name)
          path_parts <- niveles_clean[1:idx_actual]
          path_final <- paste(c("Rscience", path_parts), collapse = " / ")

          query_data <- data_full
          for (i in seq_along(path_parts)) {
            col_name <- paste0("nivel", i)
            if (col_name %in% names(query_data)) {
              query_data <- query_data %>% filter(!!sym(col_name) == path_parts[i])
            }
          }
          scripts <- query_data %>% pull(script_id) %>% unique()
        } else {
          path_final <- "Rscience (Root)"
          scripts <- unique(data_full$script_id)
        }
      }
      list(node_name = node_name, path = path_final, scripts = scripts, time = format(Sys.time(), "%H:%M:%S"))
    })

    # 2. CONSOLA DE ESTADO
    output$status_info <- renderPrint({
      req(show_debug)
      res <- info_nodo()
      cat("PATH          :", res$path, "\n")
      cat("TOTAL SCRIPTS :", length(res$scripts), "\n")
      cat("SYSTEM TIME   :", res$time)
    })

    output$debug_ui_wrapper <- renderUI({
      if (show_debug) {
        div(class = "status-console",
            verbatimTextOutput(ns("status_info"))
        )
      } else {
        NULL
      }
    })

    # 3. HELPER JERARQUÍA
    df_a_jerarquia <- function(data, name = "Rscience") {
      node <- list(name = as.character(name))
      if (ncol(data) > 0) {
        level_col <- names(data)[1]
        hijos <- unique(data[[level_col]])
        hijos <- hijos[!is.na(hijos) & hijos != "" & hijos != "NA"]
        if (length(hijos) > 0) {
          node$children <- lapply(hijos, function(h) {
            sub_data <- data[data[[level_col]] == h, -1, drop = FALSE]
            df_a_jerarquia(sub_data, h)
          })
        }
      }
      node
    }

    # 4. INYECTOR JS
    output$js_injector <- renderUI({
      req(data_full)
      df_tree <- data_full %>% select(starts_with("nivel"))
      tree_json <- jsonlite::toJSON(df_a_jerarquia(df_tree), auto_unbox = TRUE)

      tags$script(HTML(paste0(
        sprintf("var treeData = %s;", tree_json),
        "
        var root, svg, g, treemap, zoomHandler, i = 0, duration = 600;
        var activeNode = null, ghostMode = false, topAlignMode = false;

        function initTree() {
          const container = d3.select('#tree-container');
          if (container.empty()) return;
          container.selectAll('*').remove();

          // CORRECCIÓN: Usar Rect para centro relativo al contenedor real
          const rect = container.node().getBoundingClientRect();
          const width = rect.width;
          const height = rect.height;

          svg = container.append('svg').attr('width', '100%').attr('height', '100%');
          g = svg.append('g');

          zoomHandler = d3.zoom().scaleExtent([0.01, 8])
            .on('zoom', (event) => g.attr('transform', event.transform));
          svg.call(zoomHandler);

          treemap = d3.tree().nodeSize([180, 850]);

          root = d3.hierarchy(treeData, d => d.children);
          root.descendants().forEach(d => { d.id = ++i; });

          // CENTRADO VERTICAL INDEPENDIENTE
          root.x0 = height / 2;
          root.y0 = 0;
          activeNode = root;

          Shiny.setInputValue('", ns("selected_node"), "', 'Rscience');
          if (root.children) root.children.forEach(collapse);

          update(root, 0);
        }

        function update(source, customDuration) {
          const currentDuration = (customDuration !== undefined) ? customDuration : duration;

          if (activeNode) {
            reorderPathToTop(activeNode, !topAlignMode);
          }

          const nodes = treemap(root).descendants();
          const links = nodes.slice(1);

          nodes.forEach(d => d.y = d.depth * 850);
          fitToViewport(nodes, currentDuration);

          const node = g.selectAll('g.node').data(nodes, d => d.id);

          const nodeEnter = node.enter().append('g').attr('class', 'node')
            .attr('transform', d => (currentDuration === 0) ? `translate(${d.y},${d.x})` : `translate(${source.y0 || 0},${source.x0 || 0})`)
            .on('click', (event, d) => {
              activeNode = d;
              Shiny.setInputValue('", ns("selected_node"), "', d.data.name, {priority: 'event'});
              if (d.children) { d._children = d.children; d.children = null; }
              else { d.children = d._children; d._children = null; }
              update(d);
            });

          nodeEnter.append('circle').attr('r', 28);
          nodeEnter.append('text').attr('dy', '.35em')
            .attr('x', d => d.children || d._children ? -45 : 45)
            .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
            .text(d => d.data.name);

          const nodeUpdate = nodeEnter.merge(node);
          if (currentDuration === 0) {
             nodeUpdate.attr('transform', d => `translate(${d.y},${d.x})`);
          } else {
             nodeUpdate.transition().duration(currentDuration).attr('transform', d => `translate(${d.y},${d.x})`);
          }

          nodeUpdate.select('circle')
            .style('fill', d => {
              if (!d.children && !d._children) return '#00FF00';
              if (d === activeNode || isAncestor(d)) return '#ff9100';
              return '#00FFFF';
            })
            .style('stroke', d => (d === activeNode) ? '#ff9100' : '#fff')
            .style('stroke-width', '6px')
            .style('opacity', d => (ghostMode && d !== activeNode && !isAncestor(d)) ? 0.15 : 1);

          if (currentDuration === 0) { node.exit().remove(); }
          else { node.exit().transition().duration(currentDuration).attr('transform', d => `translate(${source.y},${source.x})`).remove(); }

          const link = g.selectAll('path.link').data(links, d => d.id);
          const linkEnter = link.enter().insert('path', 'g').attr('class', 'link')
            .attr('d', d => {
               if (currentDuration === 0) return diagonal(d, d.parent);
               const o = {y: source.y0 || 0, x: source.x0 || 0};
               return diagonal(o, o);
            });

          const linkUpdate = linkEnter.merge(link);
          if (currentDuration === 0) { linkUpdate.attr('d', d => diagonal(d, d.parent)); }
          else { linkUpdate.transition().duration(currentDuration).attr('d', d => diagonal(d, d.parent)); }

          linkUpdate
            .style('stroke', d => (d === activeNode || isAncestor(d)) ? '#ff9100' : '#00FFFF')
            .style('stroke-width', d => (d === activeNode || isAncestor(d)) ? '25px' : '12px')
            .style('opacity', d => (ghostMode && !isAncestor(d) && d !== activeNode) ? 0.08 : 0.6);

          if (currentDuration === 0) { link.exit().remove(); }
          else { link.exit().transition().duration(currentDuration).attr('d', d => { const o = {y: source.y, x: source.x}; return diagonal(o, o); }).remove(); }

          nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });
        }

        function fitToViewport(targetNodes, customDuration) {
          const container = document.getElementById('tree-container');
          if (!container || !targetNodes || targetNodes.length === 0) return;
          const rect = container.getBoundingClientRect();
          const width = rect.width, height = rect.height;

          const minX = d3.min(targetNodes, d => d.x), maxX = d3.max(targetNodes, d => d.x);
          const minY = d3.min(targetNodes, d => d.y), maxY = d3.max(targetNodes, d => d.y);
          const contentWidth = (maxY - minY) || 1, contentHeight = (maxX - minX) || 1;

          const padding = 160;
          let scale = Math.min(width / (contentWidth + padding * 6), height / (contentHeight + padding * 2));
          scale = Math.min(Math.max(scale, 0.05), 0.8);

          const transform = d3.zoomIdentity.translate(width / 2, height / 2).scale(scale).translate(-(minY + maxY) / 2, -(minX + maxX) / 2);

          if (customDuration === 0) { svg.call(zoomHandler.transform, transform); }
          else { svg.transition().duration(customDuration).ease(d3.easeCubicInOut).call(zoomHandler.transform, transform); }
        }

        function reorderPathToTop(d, revert = false) {
          let curr = d;
          while (curr && curr.parent) {
            let p = curr.parent;
            let children = p.children || p._children;
            if (children) {
              if (revert) { children.sort((a, b) => a.id - b.id); }
              else {
                let idx = children.findIndex(c => c.id === curr.id);
                if (idx > -1) { children.unshift(children.splice(idx, 1)[0]); }
              }
            }
            curr = p;
          }
        }

        window.captureTree = function() {
          const svgNode = document.querySelector('#tree-container svg');
          const rect = svgNode.getBoundingClientRect();
          const width = rect.width, height = rect.height;

          const clonedSvg = svgNode.cloneNode(true);
          d3.select(clonedSvg).selectAll('text')
            .style('fill', '#ffffff')
            .style('font-family', 'Segoe UI, sans-serif')
            .style('text-shadow', '2px 2px 4px #000000');

          d3.select(clonedSvg).selectAll('.link')
            .style('fill', 'none')
            .style('stroke-opacity', '0.6');

          const serializer = new XMLSerializer();
          let source = serializer.serializeToString(clonedSvg);
          if(!source.match(/^<svg[^>]+xmlns=\"http\\:\\/\\/www\\.w3\\.org\\/2000\\/svg\"/)){
              source = source.replace(/^<svg/, '<svg xmlns=\"http://www.w3.org/2000/svg\"');
          }

          const img = new Image();
          const svgBlob = new Blob([source], {type: 'image/svg+xml;charset=utf-8'});
          const url = URL.createObjectURL(svgBlob);

          img.onload = function() {
            const canvas = document.createElement('canvas');
            const scaleFactor = 2;
            canvas.width = width * scaleFactor;
            canvas.height = height * scaleFactor;
            const ctx = canvas.getContext('2d');
            ctx.fillStyle = '#000000';
            ctx.fillRect(0, 0, canvas.width, canvas.height);
            ctx.scale(scaleFactor, scaleFactor);
            ctx.drawImage(img, 0, 0);

            const pngUrl = canvas.toDataURL('image/png');
            const link = document.createElement('a');
            link.href = pngUrl;
            link.download = 'RScience_Capture_' + new Date().getTime() + '.png';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            URL.revokeObjectURL(url);
          };
          img.src = url;
        };

        function runCollapseOthers() {
          if (!activeNode || activeNode === root) return;
          var ancestors = [];
          var curr = activeNode;
          while (curr) { ancestors.push(curr.id); curr = curr.parent; }
          root.descendants().forEach(d => {
            if (ancestors.indexOf(d.id) === -1) {
              if (d.children) { d._children = d.children; d.children = null; }
            }
          });
          update(activeNode);
        }

        function isAncestor(d) { let curr = activeNode ? activeNode.parent : null; while(curr) { if(curr === d) return true; curr = curr.parent; } return false; }
        function diagonal(s, d) { return `M ${s.y} ${s.x} C ${(s.y+d.y)/2} ${s.x}, ${(s.y+d.y)/2} ${d.x}, ${d.y} ${d.x}`; }
        function collapse(d) { if(d.children) { d._children = d.children; d._children.forEach(collapse); d.children = null; } }
        function expand(d) { if(d._children) { d.children = d._children; d._children = null; } if(d.children) d.children.forEach(expand); }
        function toggleGhost() { ghostMode = !ghostMode; update(activeNode); }
        function toggleTopAlign() { topAlignMode = !topAlignMode; update(activeNode); }
        function togglePanel(side) {
          const panel = document.getElementById('panel-' + side);
          const btn = document.getElementById('btn-toggle-' + side);
          panel.classList.toggle('collapsed');
          btn.innerHTML = panel.classList.contains('collapsed') ? '☰' : '✖';
        }
        function resetMap() { activeNode = root; if(root.children) root.children.forEach(collapse); update(root); }
        function fullExpand() { expand(root); update(root); }

        setTimeout(initTree, 300);
        window.addEventListener('resize', () => {
           if(treemap && root) fitToViewport(treemap(root).descendants(), 0);
        });
        "
      )))
    })

    return(info_nodo)
  })
}

