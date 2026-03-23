

# ==========================================
# MÓDULO: EXPLORADOR DE ÁRBOL RScience
# ==========================================
library(shiny)
library(dplyr)
library(jsonlite)

mod_tree_ui <- function(id) {
  ns <- NS(id)
  module_id <- ns("module-wrapper")   # ID único para scoping de estilos

  tagList(
    # ────────────────────────────────────────────────────────────────
    # Dependencias y estilos CSS (scoped al módulo)
    # ────────────────────────────────────────────────────────────────
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/d3@6/dist/d3.min.js"),

      tags$style(HTML(paste0("
        #", module_id, " {
          height: 100vh;
          width: 100%;
          background-color: #000;
          overflow: hidden;
          margin: 0;
          padding: 0;
          position: relative;
          font-family: 'Segoe UI', sans-serif;
        }

        #", module_id, " #tree-container {
          position: absolute;
          inset: 0;
          height: 100%;
          width: 100%;
          background: #000;
        }

        #", module_id, " .status-console {
          background: #0a1f2a;               /* Fondo diferente al negro del árbol */
          color: #ADFF2F;
          border-bottom: 1px solid #00aaff;
          margin: 0;
          padding: 12px 20px;
          font-family: 'Courier New', monospace;
          font-size: 13px;
          line-height: 1.5;
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          z-index: 1002;
          pointer-events: none;
        }

        #", module_id, " .node text {
          pointer-events: none;
          fill: #fff;
          font-size: 20px !important;
          font-weight: 900;
          text-shadow: 3px 3px 8px #000;
          transition: opacity 0.4s;
        }

        #", module_id, " .link {
          fill: none;
          transition: all 0.4s;
        }

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

        #", module_id, " .panel-left  { top: 90px; left: 20px;  width: 220px; }
        #", module_id, " .panel-right { top: 90px; right: 20px; width: 260px; }

        #", module_id, " .panel-left.collapsed  { left: -200px;  opacity: 0.6; }
        #", module_id, " .panel-right.collapsed { right: -240px; opacity: 0.6; }

        #", module_id, " .btn-toggle {
          position: absolute;
          top: 12px;
          cursor: pointer;
          background: none;
          border: none;
          color: #00FFFF;
          font-size: 20px;
          font-weight: bold;
          padding: 6px;
          z-index: 1001;
        }

        #", module_id, " #btn-toggle-left  { right: 12px; }
        #", module_id, " #btn-toggle-right { left: 12px; }

        #", module_id, " .panel-title {
          font-weight: 900;
          font-size: 15px;
          margin-bottom: 14px;
          border-bottom: 1px solid #00FFFF;
          padding-bottom: 6px;
          opacity: 0.85;
        }

        #", module_id, " .btn-cmd {
          background: transparent;
          color: #00FFFF;
          border: 1px solid #00FFFF;
          padding: 10px;
          width: 100%;
          font-weight: bold;
          font-size: 12px;
          cursor: pointer;
          margin-bottom: 10px;
          border-radius: 6px;
          transition: 0.25s;
          text-transform: uppercase;
        }

        #", module_id, " .btn-cmd:hover {
          background: #00FFFF;
          color: #000;
          box-shadow: 0 0 12px #00FFFF;
        }

        #", module_id, " .btn-snap  { border-color: #ff9100; color: #ff9100; }
        #", module_id, " .btn-snap:hover  { background: #ff9100; color: #000; box-shadow: 0 0 12px #ff9100; }

        #", module_id, " .btn-focus { border-color: #ADFF2F; color: #ADFF2F; }
        #", module_id, " .btn-focus:hover { background: #ADFF2F; color: #000; box-shadow: 0 0 12px #ADFF2F; }

        #", module_id, " .switch-container {
          display: flex;
          align-items: center;
          justify-content: space-between;
          margin-bottom: 16px;
          font-size: 13px;
        }

        #", module_id, " .switch {
          position: relative;
          display: inline-block;
          width: 48px;
          height: 24px;
        }

        #", module_id, " .switch input { opacity: 0; width: 0; height: 0; }

        #", module_id, " .slider {
          position: relative;
          cursor: pointer;
          top: 0; left: 0; right: 0; bottom: 0;
          background-color: #444;
          transition: .4s;
          border-radius: 24px;
          border: 1px solid #666;
        }

        #", module_id, " .slider:before {
          position: absolute;
          content: '';
          height: 18px;
          width: 18px;
          left: 4px;
          bottom: 3px;
          background-color: white;
          transition: .4s;
          border-radius: 50%;
        }

        #", module_id, " input:checked + .slider { background-color: #00FFFF; border-color: #00FFFF; }
        #", module_id, " input:checked + .slider:before { transform: translateX(24px); background-color: #000; }
      ")))
    ),

    # ────────────────────────────────────────────────────────────────
    # Contenido principal
    # ────────────────────────────────────────────────────────────────
    div(id = module_id,

        # Consola de estado (fondo diferente)
        div(class = "status-console",
            verbatimTextOutput(ns("status_info"))
        ),

        # Contenedor del árbol (ocupa todo el espacio)
        div(id = "tree-container"),

        # Panel izquierdo - Acciones
        div(id = "panel-left", class = "panel-ctrl panel-left",
            tags$button(id = "btn-toggle-left", class = "btn-toggle", "✖",
                        onclick = "togglePanel('left')"),
            div(class = "panel-title", "⚡ ACTIONS"),
            tags$button("↺ Reset View", class = "btn-cmd", onclick = "resetMap()"),
            tags$button("🎯 Focus Branch", class = "btn-cmd btn-focus", onclick = "runCollapseOthers()"),
            tags$button("⇱ Expand All", class = "btn-cmd", onclick = "fullExpand()"),
            tags$button("📷 Capture", class = "btn-cmd btn-snap", onclick = "alert('Capture completed')")
        ),

        # Panel derecho - Modos de vista
        div(id = "panel-right", class = "panel-ctrl panel-right",
            tags$button(id = "btn-toggle-right", class = "btn-toggle", "✖",
                        onclick = "togglePanel('right')"),
            div(class = "panel-title", "🛠️ VIEW MODES"),

            div(class = "switch-container",
                span(class = "switch-label", "👻 GHOST MODE"),
                tags$label(class = "switch",
                           tags$input(type = "checkbox", onclick = "toggleGhost()"),
                           span(class = "slider")
                )
            ),

            div(class = "switch-container",
                span(class = "switch-label", "🔝 TOP ALIGN"),
                tags$label(class = "switch",
                           tags$input(type = "checkbox", onclick = "toggleTopAlign()"),
                           span(class = "slider")
                )
            )
        )
    ),

    # Inyección del JavaScript (definido en el server)
    uiOutput(ns("js_injector"))
  )
}

mod_tree_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Datos de respaldo por seguridad
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

    # 3. HELPER: CONVERSIÓN DE DATAFRAME A LISTA JERÁRQUICA
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

    # 4. INYECTOR JAVASCRIPT (LÓGICA DE ANIMACIÓN SECUENCIAL)
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

          const width = container.node().clientWidth;
          const height = container.node().clientHeight;

          svg = container.append('svg').attr('width', '100%').attr('height', '100%');
          g = svg.append('g');

          zoomHandler = d3.zoom().scaleExtent([0.01, 8])
            .on('zoom', (event) => g.attr('transform', event.transform));
          svg.call(zoomHandler);

          treemap = d3.tree().nodeSize([180, 850]);

          root = d3.hierarchy(treeData, d => d.children);
          root.descendants().forEach(d => { d.id = ++i; });

          root.x0 = height / 2;
          root.y0 = 0;
          activeNode = root;

          Shiny.setInputValue('", ns("selected_node"), "', 'Rscience');

          // Al inicio, colapsamos todos los descendientes de la raíz.
          if (root.children) {
            root.children.forEach(collapse);
          }

          update(root);
        }

        function update(source) {
          if (topAlignMode && activeNode) reorderPathToTop(activeNode);

          // 1. Calculamos la estructura de datos FINAL (LAYOUT predictivo)
          const nodes = treemap(root).descendants();
          const links = nodes.slice(1);

          // Fijamos la distancia entre niveles
          nodes.forEach(d => d.y = d.depth * 850);

          // 2. EJECUTAMOS EL ZOOM PREDICTIVO (Sincronizado con la animación)
          fitToViewport(nodes);

          // 3. GESTIÓN DE LINKS (Creación y Animación secuencial)
          const link = g.selectAll('path.link').data(links, d => d.id);

          // Creamos los links en la posición inicial (origen del padre)
          const linkEnter = link.enter().insert('path', 'g').attr('class', 'link')
            .attr('d', d => {
              const o = {y: source.y0 || 0, x: source.x0 || 0};
              return diagonal(o, o);
            });

          // Animación de los links existentes y nuevos (crecimiento de ramas)
          linkEnter.merge(link).transition().duration(duration)
            .attr('d', d => diagonal(d, d.parent))
            .style('stroke', d => (d === activeNode || isAncestor(d)) ? '#ff9100' : '#00FFFF')
            .style('stroke-width', d => (d === activeNode || isAncestor(d)) ? '25px' : '12px')
            .style('opacity', d => (ghostMode && !isAncestor(d) && d !== activeNode) ? 0.08 : 0.6);

          link.exit().transition().duration(duration)
            .attr('d', d => { const o = {y: source.y, x: source.x}; return diagonal(o, o); })
            .remove();

          // 4. GESTIÓN DE NODOS (Creación y Animación secuencial)
          const node = g.selectAll('g.node').data(nodes, d => d.id);

          // Creamos los nodos hijos en la posición del padre (origen)
          const nodeEnter = node.enter().append('g').attr('class', 'node')
            .attr('transform', d => `translate(${source.y0 || 0},${source.x0 || 0})`)
            .on('click', (event, d) => {
              activeNode = d;
              Shiny.setInputValue('", ns("selected_node"), "', d.data.name, {priority: 'event'});

              // --- SECUENCIA DE ANIMACIÓN: PADRE -> RAMAS -> HIJOS ---
              if (d.children) {
                d._children = d.children;
                d.children = null;
                update(d); // Colapsar es inmediato
              } else {
                d.children = d._children;
                d._children = null;
                // Al expandir, forzamos que source (el padre) anime primero su estado 'abierto'
                update(d);
              }
            });

          nodeEnter.append('circle').attr('r', 28);

          nodeEnter.append('text').attr('dy', '.35em')
            .attr('x', d => d.children || d._children ? -45 : 45)
            .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
            .text(d => d.data.name);

          const nodeUpdate = nodeEnter.merge(node);

          // Animación de los nodos existentes y nuevos (avance hacia el final de la rama)
          nodeUpdate.transition().duration(duration)
            .attr('transform', d => `translate(${d.y},${d.x})`);

          // --- ESTILO DE NODO FINAL (VERDE) CORREGIDO ---
          nodeUpdate.select('circle')
            .style('fill', d => {
              // Si NO tiene hijos (es nodo final) -> VERDE
              if (!d.children && !d._children) return '#00FF00';
              // Si está seleccionado o es ancestro -> NARANJA
              if (d === activeNode || isAncestor(d)) return '#ff9100';
              // Por defecto -> CIAN
              return '#00FFFF';
            })
            // El stroke (borde interior) es NARANJA solo si está seleccionado, sino BLANCO
            .style('stroke', d => (d === activeNode) ? '#ff9100' : '#fff')
            .style('stroke-width', '6px')
            // El borde VERDE distintivo se mantiene siempre en los nodos finales
            .style('border', d => (!d.children && !d._children) ? '4px solid #00FF00' : 'none')
            .style('opacity', d => (ghostMode && d !== activeNode && !isAncestor(d)) ? 0.15 : 1);

          node.exit().transition().duration(duration)
            .attr('transform', d => `translate(${source.y},${source.x})`)
            .remove();

          // Guardar posiciones para el próximo ciclo
          nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });
        }

        function fitToViewport(targetNodes) {
          const container = document.getElementById('tree-container');
          if (!container || !targetNodes || targetNodes.length === 0) return;
          const width = container.clientWidth, height = container.clientHeight;

          // Calculamos límites basados en las posiciones futuras (LAYOUT predictivo)
          const minX = d3.min(targetNodes, d => d.x), maxX = d3.max(targetNodes, d => d.x);
          const minY = d3.min(targetNodes, d => d.y), maxY = d3.max(targetNodes, d => d.y);

          const contentWidth = (maxY - minY) || 1, contentHeight = (maxX - minX) || 1;
          const padding = 160;
          // Factor de escala predictivo
          let scale = Math.min(width / (contentWidth + padding * 6), height / (contentHeight + padding * 2));
          scale = Math.min(Math.max(scale, 0.05), 0.8);

          const midX = (minY + maxY) / 2;
          const midY = (minX + maxX) / 2;

          const transform = d3.zoomIdentity
            .translate(width / 2, height / 2)
            .scale(scale)
            .translate(-midX, -midY);

          svg.transition().duration(duration)
            .ease(d3.easeCubicInOut)
            .call(zoomHandler.transform, transform);
        }

        // --- FUNCIONES DE CONTROL (Resto del código intacto) ---

        function runCollapseOthers() {
          if (!activeNode || activeNode === root) return;
          const ancestors = [];
          let curr = activeNode;
          while (curr) { ancestors.push(curr.id); curr = curr.parent; }
          root.descendants().forEach(d => {
            if (!ancestors.includes(d.id) && d.children) { d._children = d.children; d.children = null; }
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
        function resetMap() { activeNode = root; if(root.children) { root.children.forEach(collapse); } update(root); }
        function fullExpand() { expand(root); update(root); }

        function reorderPathToTop(d) {
          let curr = d;
          while (curr && curr.parent) {
            let p = curr.parent;
            let children = p.children || p._children;
            if (children) {
              let idx = children.findIndex(c => c.id === curr.id);
              if (idx > -1) { children.unshift(children.splice(idx, 1)[0]); }
            }
            curr = p;
          }
        }

        setTimeout(initTree, 200);
        window.addEventListener('resize', () => fitToViewport(treemap(root).descendants()));
        "
      )))
    })

    return(info_nodo)
  })
}

