library(shiny)
library(jsonlite)
library(dplyr)

# ==========================================
# 1. MÓDULO UI (mod_tools_ui)
# ==========================================
mod_tools_ui <- function(id) {
  ns <- NS(id)

  # --- Gestión de Rutas ---
  ruta_base <- system.file(package = "Rscience2027")
  if (ruta_base == "") {
    posible_raiz <- getwd()
    ruta_base <- if (basename(posible_raiz) == "special_box") dirname(posible_raiz) else posible_raiz
  }

  # Registro del recurso para D3 local
  shiny::addResourcePath(
    prefix = "libs_tools",
    directoryPath = normalizePath(file.path(ruta_base, "external_soft"), mustWork = FALSE)
  )

  # --- Motor de Datos (Jerarquía) ---
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

  # Carga de datos
  if (!exists("tree_data")) {
    ruta_rda <- file.path(ruta_base, "inst/data/tree_data.rda")
    if(file.exists(ruta_rda)) load(ruta_rda, envir = .GlobalEnv)
  }

  # Generar JSON
  niveles_cols <- get("tree_data", envir = .GlobalEnv) %>% select(starts_with("nivel"))
  tree_json <- jsonlite::toJSON(df_a_jerarquia(niveles_cols), auto_unbox = TRUE)

  tagList(
    tags$head(
      # Fallback a CDN si el local falla, o viceversa
      tags$script(src = "https://d3js.org/d3.v6.min.js"),

      tags$style(HTML(paste0("
        #", ns("tree-container"), " { background: #000; height: 100vh; width: 100vw; position: relative; overflow: hidden; }
        .node text { pointer-events: none; fill: #fff; font-size: 40px !important; font-weight: 900; text-shadow: 6px 6px 12px #000; transition: opacity 0.4s; }
        .link { fill: none; transition: all 0.4s; }

        .panel-ctrl {
          position: absolute; z-index: 1000;
          background: rgba(10, 20, 25, 0.95); border: 2px solid #00FFFF; border-radius: 12px;
          color: #00FFFF; padding: 15px; font-family: 'Segoe UI', sans-serif;
          box-shadow: 0 0 20px rgba(0, 255, 255, 0.2);
          transition: all 0.5s cubic-bezier(0.4, 0, 0.2, 1);
        }
        .panel-left { top: 20px; left: 20px; width: 220px; }
        .panel-right { top: 20px; right: 20px; width: 260px; }
        .panel-left.collapsed { left: -180px; opacity: 0.5; }
        .panel-right.collapsed { right: -220px; opacity: 0.5; }

        .btn-toggle {
          position: absolute; top: 10px; cursor: pointer; background: none; border: none;
          color: #00FFFF; font-size: 18px; font-weight: bold; padding: 5px; z-index: 1001;
        }
        .btn-cmd {
          background: transparent; color: #00FFFF; border: 1px solid #00FFFF;
          padding: 10px; width: 100%; font-weight: bold; font-size: 11px;
          cursor: pointer; margin-bottom: 8px; border-radius: 6px; transition: 0.2s;
          text-transform: uppercase; text-align: center;
        }
        .btn-cmd:hover { background: #00FFFF; color: #000; box-shadow: 0 0 10px #00FFFF; }

        .switch-container { display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px; }
        .switch { position: relative; display: inline-block; width: 44px; height: 22px; }
        .switch input { opacity: 0; width: 0; height: 0; }
        .slider { position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0; background-color: #333; transition: .4s; border-radius: 22px; }
        .slider:before { position: absolute; content: ''; height: 14px; width: 14px; left: 4px; bottom: 3px; background-color: white; transition: .4s; border-radius: 50%; }
        input:checked + .slider { background-color: #00FFFF; }
        input:checked + .slider:before { transform: translateX(20px); background-color: #000; }
      ")))
    ),

    div(id = ns("tree-container"),
        # PANEL IZQUIERDO
        div(id = ns("panel-left"), class = "panel-ctrl panel-left",
            tags$button(class = "btn-toggle", style="right:10px", "✖",
                        onclick = sprintf("togglePanel('%s', 'left')", id)),
            div(class = "panel-title", "⚡ ACTIONS"),
            tags$button("↺ Reset View", class = "btn-cmd", onclick = "resetMap()"),
            tags$button("🎯 Focus Branch", class = "btn-cmd", onclick = "runCollapseOthers()"),
            tags$button("⇱ Expand All", class = "btn-cmd", onclick = "fullExpand()")
        ),
        # PANEL DERECHO
        div(id = ns("panel-right"), class = "panel-ctrl panel-right",
            tags$button(class = "btn-toggle", style="left:10px", "✖",
                        onclick = sprintf("togglePanel('%s', 'right')", id)),
            div(class = "panel-title", "🛠️ VIEW MODES"),
            div(class = "switch-container",
                span("👻 GHOST MODE"),
                tags$label(class = "switch", tags$input(type = "checkbox", onclick = "toggleGhost()"), span(class = "slider"))
            ),
            div(class = "switch-container",
                span("🔝 TOP ALIGN"),
                tags$label(class = "switch", tags$input(type = "checkbox", onclick = "toggleTopAlign()"), span(class = "slider"))
            )
        )
    ),

    # Inyección de Datos y Lógica JS
    tags$script(HTML(sprintf("var treeData = %s;", tree_json))),
    tags$script(HTML("
      function togglePanel(id, side) {
        const panel = document.getElementById(id + '-panel-' + side);
        panel.classList.toggle('collapsed');
        event.target.innerHTML = panel.classList.contains('collapsed') ? '☰' : '✖';
      }

      var root, svg, g, treemap, zoom, i = 0, duration = 600, activeNode = null;
      var ghostMode = false, topAlignMode = false;
      var originalOrder = new Map();

      function initTree() {
        const container = d3.select(\"[id$='tree-container']\");
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

        root.x0 = window.innerHeight / 2; root.y0 = 0;
        activeNode = root;
        if (root.children) root.children.forEach(collapse);
        update(root);
      }

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
              if (d === activeNode || isAncestor(d)) return '#ff9100';
              if (!d.children && !d._children) return '#00FF00';
              return '#00FFFF';
            })
            .style('stroke', d => ((!d.children && !d._children) && (d === activeNode || isAncestor(d))) ? '#00FF00' : '#fff')
            .style('stroke-width', '8px');

        const link = g.selectAll('path.link').data(links, d => d.id);
        const linkEnter = link.enter().insert('path', 'g').attr('class', 'link');
        linkEnter.merge(link).transition().duration(duration).attr('d', d => diagonal(d, d.parent))
            .style('stroke', d => (d === activeNode || isAncestor(d)) ? '#ff9100' : '#00FFFF')
            .style('stroke-width', d => (d === activeNode || isAncestor(d)) ? '36px' : '18px')
            .style('opacity', d => (d === activeNode || isAncestor(d)) ? 1 : 0.4);

        link.exit().remove();
        nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });

        const minX = d3.min(nodes, d => d.x) - 200, maxX = d3.max(nodes, d => d.x) + 200;
        const minY = d3.min(nodes, d => d.y) - 200, maxY = d3.max(nodes, d => d.y) + 800;
        const w = window.innerWidth, h = window.innerHeight;
        const scale = 0.8 / Math.max((maxY - minY) / w, (maxX - minX) / h);
        const t = d3.zoomIdentity.translate(w/2 - scale*(minY+(maxY-minY)/2), h/2 - scale*(minX+(maxX-minX)/2)).scale(scale);
        d3.select(\"[id$='tree-container'] svg\").transition().duration(duration).call(zoom.transform, t);
      }

      function isAncestor(d) {
        let curr = activeNode ? activeNode.parent : null;
        while(curr) { if (curr === d) return true; curr = curr.parent; }
        return false;
      }
      function diagonal(s, d) { return `M ${s.y} ${s.x} C ${(s.y+d.y)/2} ${s.x}, ${(s.y+d.y)/2} ${d.x}, ${d.y} ${d.x}`; }
      function collapse(d) { if(d.children) { d._children = d.children; d._children.forEach(collapse); d.children = null; } }
      function expand(d) { if(d._children) { d.children = d._children; d._children = null; } if(d.children) d.children.forEach(expand); }
      function toggleGhost() { ghostMode = !ghostMode; update(activeNode); }
      function toggleTopAlign() { topAlignMode = !topAlignMode; update(activeNode); }
      function resetMap() { activeNode = root; if (root.children) root.children.forEach(collapse); update(root); }
      function fullExpand() { expand(root); update(root); }
      function runCollapseOthers() {
        if (!activeNode) return;
        let path = []; let curr = activeNode; while(curr) { path.push(curr); curr = curr.parent; }
        root.descendants().forEach(d => { if (!path.includes(d) && d.children) { d._children = d.children; d.children = null; } });
        update(activeNode);
      }

      $(document).on('shiny:connected', initTree);
    "))
  )
}

# ==========================================
# 2. MÓDULO SERVER (mod_tools_server)
# ==========================================
mod_tools_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # El server se mantiene vacío porque la lógica es JS puro
    # alimentado por el JSON generado en el UI
  })
}

# ==========================================
# 3. APP PRINCIPAL
# ==========================================
ui <- fluidPage(
  mod_tools_ui("tree_module")
)

server <- function(input, output, session) {
  mod_tools_server("tree_module")
}

shinyApp(ui, server)
