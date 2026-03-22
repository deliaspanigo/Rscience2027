

# ==========================================
# MÓDULO: EXPLORADOR DE ÁRBOL RScience
# ==========================================
library(shiny)
library(dplyr)
library(jsonlite)

mod_tree_ui <- function(id) {
  ns <- NS(id)

  # ID único para el contenedor del módulo
  module_id <- ns("module-wrapper")

  tagList(
    tags$head(
      tags$script(src = "libs/d3.v6.min.js"),
      tags$style(HTML(paste0("
        /* Estilos limitados ÚNICAMENTE al contenedor del módulo */
        #", module_id, " {
          height: 100vh;
          width: 100%;
          background-color: #000;
          overflow: hidden;
          margin: 0;
          padding: 0;
          position: relative;
        }

        #", module_id, " #tree-container { background: #000; height: 100%; width: 100%; position: relative; }
        #", module_id, " .node text { pointer-events: none; fill: #fff; font-size: 40px !important; font-weight: 900; text-shadow: 6px 6px 12px #000; transition: opacity 0.4s; }
        #", module_id, " .link { fill: none; transition: all 0.4s; }

        #", module_id, " .status-console {
          background: #050505; color: #ADFF2F; border-bottom: 1px solid #222;
          margin: 0; padding: 15px; font-family: 'Courier New', monospace; font-size: 13px;
          line-height: 1.5; position: relative; z-index: 1002;
        }

        #", module_id, " .panel-ctrl {
          position: absolute; z-index: 1000;
          background: rgba(10, 20, 25, 0.95); border: 2px solid #00FFFF; border-radius: 12px;
          color: #00FFFF; padding: 15px; font-family: 'Segoe UI', sans-serif;
          box-shadow: 0 0 20px rgba(0, 255, 255, 0.2);
          transition: all 0.5s cubic-bezier(0.4, 0, 0.2, 1);
        }
        #", module_id, " .panel-left { top: 130px; left: 20px; width: 220px; }
        #", module_id, " .panel-right { top: 130px; right: 20px; width: 260px; }
        #", module_id, " .panel-left.collapsed { left: -180px; opacity: 0.5; }
        #", module_id, " .panel-right.collapsed { right: -220px; opacity: 0.5; }

        #", module_id, " .btn-toggle {
          position: absolute; top: 10px; cursor: pointer; background: none; border: none;
          color: #00FFFF; font-size: 18px; font-weight: bold; padding: 5px; z-index: 1001;
        }
        #", module_id, " #btn-toggle-left { right: 10px; }
        #", module_id, " #btn-toggle-right { left: 10px; }

        #", module_id, " .panel-title { font-weight: 900; font-size: 14px; margin-bottom: 12px; border-bottom: 1px solid #00FFFF; padding-bottom: 5px; opacity: 0.8; }

        #", module_id, " .btn-cmd {
          background: transparent; color: #00FFFF; border: 1px solid #00FFFF;
          padding: 10px; width: 100%; font-weight: bold; font-size: 11px;
          cursor: pointer; margin-bottom: 8px; border-radius: 6px; transition: 0.2s;
          text-transform: uppercase; text-align: center;
        }
        #", module_id, " .btn-cmd:hover { background: #00FFFF; color: #000; box-shadow: 0 0 10px #00FFFF; }
        #", module_id, " .btn-snap { border-color: #ff9100; color: #ff9100; }
        #", module_id, " .btn-snap:hover { background: #ff9100; color: #000; box-shadow: 0 0 10px #ff9100; }
        #", module_id, " .btn-focus { border-color: #ADFF2F; color: #ADFF2F; }
        #", module_id, " .btn-focus:hover { background: #ADFF2F; color: #000; box-shadow: 0 0 10px #ADFF2F; }

        #", module_id, " .switch-container { display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px; }
        #", module_id, " .switch-label { font-size: 12px; font-weight: bold; }
        #", module_id, " .switch { position: relative; display: inline-block; width: 44px; height: 22px; }
        #", module_id, " .switch input { opacity: 0; width: 0; height: 0; }
        #", module_id, " .slider {
          position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0;
          background-color: #333; transition: .4s; border-radius: 22px; border: 1px solid #555;
        }
        #", module_id, " .slider:before {
          position: absolute; content: ''; height: 14px; width: 14px; left: 4px; bottom: 3px;
          background-color: white; transition: .4s; border-radius: 50%;
        }
        #", module_id, " input:checked + .slider { background-color: #00FFFF; border-color: #00FFFF; }
        #", module_id, " input:checked + .slider:before { transform: translateX(20px); background-color: #000; }
      ")))
    ),

    # Envoltura que contiene TODO el módulo
    div(id = module_id,
        div(class = "status-console",
            verbatimTextOutput(ns("status_info"))
        ),

        div(id = "tree-container",
            div(id = "panel-left", class = "panel-ctrl panel-left",
                tags$button(id = "btn-toggle-left", class = "btn-toggle", "✖", onclick = "togglePanel('left')"),
                div(class = "panel-title", "⚡ ACTIONS"),
                tags$button("↺ Reset View", class = "btn-cmd", onclick = "resetMap()"),
                tags$button("🎯 Focus Branch", class = "btn-cmd btn-focus", onclick = "runCollapseOthers()"),
                tags$button("⇱ Expand All", class = "btn-cmd", onclick = "fullExpand()"),
                tags$button("📷 Capture", class = "btn-cmd btn-snap", onclick = "alert('Capture completed')")
            ),
            div(id = "panel-right", class = "panel-ctrl panel-right",
                tags$button(id = "btn-toggle-right", class = "btn-toggle", "✖", onclick = "togglePanel('right')"),
                div(class = "panel-title", "🛠️ VIEW MODES"),
                div(class = "switch-container",
                    span(class = "switch-label", "👻 GHOST MODE"),
                    tags$label(class = "switch", tags$input(type = "checkbox", onclick = "toggleGhost()"), span(class = "slider"))
                ),
                div(class = "switch-container",
                    span(class = "switch-label", "🔝 TOP ALIGN"),
                    tags$label(class = "switch", tags$input(type = "checkbox", onclick = "toggleTopAlign()"), span(class = "slider"))
                )
            )
        )
    ),
    uiOutput(ns("js_injector"))
  )
}

mod_tree_server <- function(id, show_debug = F) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_full <- tree_data

    # --- OBJETO REACTIVO PRINCIPAL ---
    info_nodo <- reactive({
      # 1. Identificar el nodo seleccionado
      node_name <- if (is.null(input$selected_node) || input$selected_node == "" || input$selected_node == "Rscience") {
        "Rscience"
      } else {
        input$selected_node
      }

      if (node_name == "Rscience") {
        path_final <- "Rscience (Root)"
        scripts <- data_full$script_id %>% unique()
      } else {
        # --- NUEVA LÓGICA DE FILTRADO POR CADENA COMPLETA ---

        # Primero, necesitamos reconstruir el path desde JS o buscarlo en la data.
        # Como el árbol es jerárquico, buscamos las filas que contienen al nodo actual.
        # Luego, filtramos para quedarnos con la que coincide con la estructura del árbol.

        # Nota: 'input$selected_node' en tu JS solo manda el nombre.
        # Si tienes nombres duplicados, necesitamos que el filtro sea inteligente:

        row_match <- data_full %>%
          filter(if_any(starts_with("nivel"), ~ .x == node_name))

        # Si hay múltiples coincidencias para un mismo nombre (ej. ANOVA en dos ramas):
        # Intentamos deducir el path correcto basándonos en la estructura.
        # Para mejorar esto al 100%, lo ideal sería que JS enviara el path completo,
        # pero con esta lógica de filtrado secuencial lo resolvemos:

        if (nrow(row_match) > 0) {
          # Tomamos la primera fila que coincide para armar el path visual
          # (En una estructura de árbol bien formada, esto nos da la jerarquía)
          sample_row <- row_match %>% slice(1)
          niveles_all <- sample_row %>% select(starts_with("nivel")) %>% as.character()
          niveles_clean <- niveles_all[!is.na(niveles_all) & niveles_all != "" & niveles_all != "NA"]

          idx_actual <- which(niveles_clean == node_name)
          path_parts <- niveles_clean[1:idx_actual]
          path_final <- paste(c("Rscience", path_parts), collapse = " / ")

          # --- FILTRADO ESTRICTO DE SCRIPTS ---
          # Filtramos la data_full comparando nivel por nivel hasta el seleccionado
          query_data <- data_full
          for(i in seq_along(path_parts)) {
            col_name <- paste0("nivel", i)
            if(col_name %in% names(query_data)) {
              query_data <- query_data %>% filter(!!sym(col_name) == path_parts[i])
            }
          }

          scripts <- query_data %>% pull(script_id) %>% unique()

        } else {
          path_final <- "Rscience (Root)"
          scripts <- data_full$script_id %>% unique()
        }
      }

      list(
        node_name = node_name,
        path = path_final,
        scripts = scripts,
        time = format(Sys.time(), "%H:%M:%S")
      )
    })

    # --- SALIDA VISUAL (Consola) ---
    # Usamos el objeto reactivo info_nodo()
    output$status_info <- renderPrint({
      req(show_debug)
      res <- info_nodo()
      cat("PATH          :", res$path, "\n")
      cat("TOTAL SCRIPTS :", length(res$scripts), "\n")
      cat("SCRIPT IDs    :", paste(res$scripts, collapse = ", "), "\n")
      cat("SYSTEM TIME   :", res$time)
    })


    # Motor de datos jerárquico
    df_a_jerarquia <- function(data, name = "Rscience") {
      node <- list(name = as.character(name))
      if (ncol(data) > 0) {
        hijos_unicos <- unique(data[[1]])
        hijos_unicos <- hijos_unicos[!is.na(hijos_unicos) & hijos_unicos != "" & hijos_unicos != "NA"]
        if (length(hijos_unicos) > 0) {
          node$children <- lapply(hijos_unicos, function(h) {
            sub_data <- data[data[[1]] == h, -1, drop = FALSE]
            df_a_jerarquia(sub_data, h)
          })
        }
      }
      return(node)
    }

    output$js_injector <- renderUI({
      req(Rscience2027::tree_data)
      df_tree <- Rscience2027::tree_data %>% select(starts_with("nivel"))
      tree_json <- toJSON(df_a_jerarquia(df_tree), auto_unbox = TRUE)

      tags$script(HTML(paste0(
        sprintf("var treeData = %s;", tree_json),
        "
        var root, svg, g, treemap, zoom, i = 0, duration = 600, activeNode = null;
        var ghostMode = false, topAlignMode = false;
        var originalOrder = new Map();

        function initTree() {
          const container = d3.select('#tree-container');
          if (container.empty()) return;

          zoom = d3.zoom().scaleExtent([0.001, 4]).on('zoom', (e) => g.attr('transform', e.transform));
          svg = container.append('svg').attr('width', '100%').attr('height', '100%').call(zoom).append('g');
          g = svg.append('g');
          treemap = d3.tree().nodeSize([250, 1000]);

          root = d3.hierarchy(treeData, d => d.children);
          root.descendants().forEach(d => {
            d.id = ++i;
            if (d.children) originalOrder.set(d.id, d.children.map(c => c.data.name));
          });

          root.x0 = window.innerHeight / 2;
          root.y0 = 0;
          activeNode = root;

          Shiny.setInputValue('", ns("selected_node"), "', 'Rscience');

          if (root.children) root.children.forEach(collapse);
          update(root);
        }

        function togglePanel(side) {
          const panel = document.getElementById('panel-' + side);
          const btn = document.getElementById('btn-toggle-' + side);
          if (panel.classList.contains('collapsed')) {
            panel.classList.remove('collapsed');
            btn.innerHTML = '✖';
          } else {
            panel.classList.add('collapsed');
            btn.innerHTML = '☰';
          }
        }

        function toggleGhost() { ghostMode = !ghostMode; update(activeNode); }
        function toggleTopAlign() {
          topAlignMode = !topAlignMode;
          if (!topAlignMode) restoreOriginalOrder(root);
          update(activeNode);
        }

        function restoreOriginalOrder(d) {
          if (originalOrder.has(d.id)) {
            let order = originalOrder.get(d.id);
            let targetChildren = d.children || d._children;
            if (targetChildren) targetChildren.sort((a, b) => order.indexOf(a.data.name) - order.indexOf(b.data.name));
          }
          if (d.children) d.children.forEach(restoreOriginalOrder);
          if (d._children) d._children.forEach(restoreOriginalOrder);
        }

        function resetMap() {
          activeNode = root;
          Shiny.setInputValue('", ns("selected_node"), "', 'Rscience');
          restoreOriginalOrder(root);
          if (root.children) root.children.forEach(collapse);
          update(root);
        }

        function fullExpand() { expand(root); update(root); }

        function runCollapseOthers() {
          if (!activeNode) return;
          let path = []; let curr = activeNode;
          while(curr) { path.push(curr); curr = curr.parent; }
          root.descendants().forEach(d => {
            if (!path.includes(d) && d.children) { d._children = d.children; d.children = null; }
          });
          update(activeNode);
        }

        function reorderPathToTop(d) {
          let curr = d;
          while (curr && curr.parent) {
            let p = curr.parent;
            let children = p.children || p._children;
            if (children) {
              let idx = children.findIndex(c => c.data.name === curr.data.name);
              if (idx > -1) {
                let element = children.splice(idx, 1)[0];
                children.unshift(element);
              }
            }
            curr = p;
          }
        }

        function collapse(d) { if(d.children) { d._children = d.children; d._children.forEach(collapse); d.children = null; } }
        function expand(d) { if(d._children) { d.children = d._children; d._children = null; } if(d.children) d.children.forEach(expand); }

        function update(source) {
          if (topAlignMode && activeNode) reorderPathToTop(activeNode);
          const nodes = treemap(root).descendants();
          const links = nodes.slice(1);
          nodes.forEach(d => d.y = d.depth * 1000);

          const node = g.selectAll('g.node').data(nodes, d => d.id);
          const nodeEnter = node.enter().append('g').attr('class', 'node')
              .attr('transform', d => `translate(${source.y0 || 0},${source.x0 || 0})`)
              .on('click', (e, d) => {
                  activeNode = d;
                  Shiny.setInputValue('", ns("selected_node"), "', d.data.name, {priority: 'event'});
                  if (d.children) { d._children = d.children; d.children = null; }
                  else { d.children = d._children; d._children = null; }
                  update(d);
              });

          nodeEnter.append('circle').attr('r', 30);
          nodeEnter.append('text').attr('dy', '.35em')
              .attr('x', d => d.children || d._children ? -55 : 55)
              .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
              .text(d => d.data.name);

          const nodeUpdate = nodeEnter.merge(node);
          nodeUpdate.transition().duration(duration).attr('transform', d => `translate(${d.y},${d.x})`);

          nodeUpdate.select('circle')
              .style('fill', d => {
                if (d === activeNode || isAncestor(d)) return '#ff9100'; // NARANJA FUERTE
                if (!d.children && !d._children) return '#00FF00';
                return '#00FFFF';
              })
              .style('stroke', d => ((!d.children && !d._children) && (d === activeNode || isAncestor(d))) ? '#00FF00' : '#fff')
              .style('stroke-width', '8px')
              .style('opacity', d => (ghostMode && d !== activeNode && !isAncestor(d)) ? 0.1 : 1);

          nodeUpdate.select('text').style('opacity', d => (ghostMode && d !== activeNode && !isAncestor(d)) ? 0.1 : 1);
          node.exit().remove();

          const link = g.selectAll('path.link').data(links, d => d.id);
          const linkEnter = link.enter().insert('path', 'g').attr('class', 'link');
          linkEnter.merge(link).transition().duration(duration).attr('d', d => diagonal(d, d.parent))
              .style('stroke', d => (d === activeNode || isAncestor(d)) ? '#ff9100' : '#00FFFF')
              .style('stroke-width', d => (d === activeNode || isAncestor(d)) ? '36px' : '18px')
              .style('opacity', d => {
                if (ghostMode && !isAncestor(d) && d !== activeNode) return 0.05;
                return (d === activeNode || isAncestor(d)) ? 1 : 0.4; // NARANJA SIEMPRE OPACIDAD 1
              });
          link.exit().remove();
          nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });

          const minX = d3.min(nodes, d => d.x) - 200, maxX = d3.max(nodes, d => d.x) + 200;
          const minY = d3.min(nodes, d => d.y) - 200, maxY = d3.max(nodes, d => d.y) + 800;
          const w = window.innerWidth, h = window.innerHeight;
          const scale = 0.8 / Math.max((maxY - minY) / w, (maxX - minX) / h);
          const t = d3.zoomIdentity.translate(w/2 - scale*(minY+(maxY-minY)/2), h/2 - scale*(minX+(maxX-minX)/2)).scale(scale);
          d3.select('#tree-container svg').transition().duration(duration).call(zoom.transform, t);
        }

        function isAncestor(d) {
          let curr = activeNode ? activeNode.parent : null;
          while(curr) { if (curr === d) return true; curr = curr.parent; }
          return false;
        }
        function diagonal(s, d) { return `M ${s.y} ${s.x} C ${(s.y+d.y)/2} ${s.x}, ${(s.y+d.y)/2} ${d.x}, ${d.y} ${d.x}`; }

        initTree();
        "
      )))
    })

    return(info_nodo)  })
}

