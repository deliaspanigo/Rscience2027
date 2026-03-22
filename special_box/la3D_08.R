# --- 0. CONFIGURACIÓN ---
if (interactive()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

library(shiny)
library(readxl)
library(jsonlite)
library(dplyr)
library(bslib)

# --- 1. MOTOR DE DATOS ---
preparar_jerarquia <- function(ruta_excel) {
  if (!file.exists(ruta_excel)) {
    test_data <- list(name = "Rscience",
                      children = list(list(name = "Sin Archivo",
                                           children = list(list(name = "Cargue arbol_estadistico.xlsx")))))
    return(toJSON(test_data, auto_unbox = TRUE))
  }
  tryCatch({
    df <- read_xlsx(ruta_excel)
    df <- df %>% mutate(across(everything(), as.character))
    build_node <- function(data, name = "Rscience") {
      node <- list(name = as.character(name))
      if (ncol(data) > 0) {
        children_names <- unique(data[[1]])
        children_names <- children_names[!is.na(children_names) & children_names != "NA" & children_names != ""]
        if (length(children_names) > 0) {
          node$children <- lapply(children_names, function(cn) {
            child_data <- data[data[[1]] == cn, -1, drop = FALSE]
            build_node(child_data, cn)
          })
        }
      }
      return(node)
    }
    niveles <- intersect(names(df), c("Nivel1", "Nivel2", "Nivel3", "Nivel4", "Nivel5"))
    return(toJSON(build_node(df[, niveles]), auto_unbox = TRUE))
  }, error = function(e) {
    return(toJSON(list(name = "Error Excel", children = list(list(name = e$message))), auto_unbox = TRUE))
  })
}

# --- 2. JAVASCRIPT CON BOTONES INTERNOS ---
js_logic <- "
var root, svg, g, treemap, zoom, i = 0, duration = 600, activeNode = null;

function initTree() {
  const container = d3.select('#tree-container');
  const width = container.node().getBoundingClientRect().width;
  const height = container.node().getBoundingClientRect().height;

  zoom = d3.zoom().scaleExtent([0.001, 4]).on('zoom', (event) => {
    g.attr('transform', event.transform);
  });

  svg = container.append('svg')
      .attr('width', '100%')
      .attr('height', '100%')
      .call(zoom)
      .append('g');

  g = svg.append('g');
  treemap = d3.tree().nodeSize([250, 1000]);

  root = d3.hierarchy(treeData, d => d.children);
  root.x0 = height / 2;
  root.y0 = 0;

  // Estado inicial colapsado
  if (root.children) root.children.forEach(collapse);
  update(root);
}

// FUNCIONES DE CONTROL (BOTONES)
function resetMap() {
  activeNode = null;
  if (root.children) root.children.forEach(collapse);
  update(root);
}

function fullExpand() {
  expand(root);
  update(root);
}

function showSelectedOnly() {
  if (!activeNode) {
    alert('Haz clic en un nodo primero para seleccionarlo');
    return;
  }
  // Colapsar todo excepto el camino al nodo activo
  root.children.forEach(collapse);
  let curr = activeNode;
  while(curr) {
    if(curr._children) {
      curr.children = curr._children;
      curr._children = null;
    }
    curr = curr.parent;
  }
  update(root);
}

function downloadView() {
  alert('Capturando vista actual...');
  // Aquí se podría implementar exportación a PNG/SVG
}

function collapse(d) {
  if(d.children) { d._children = d.children; d._children.forEach(collapse); d.children = null; }
}

function expand(d) {
  if(d._children) { d.children = d._children; d._children = null; }
  if(d.children) d.children.forEach(expand);
}

function update(source) {
  const nodes = treemap(root).descendants();
  const links = nodes.slice(1);
  nodes.forEach(d => d.y = d.depth * 1000);

  const node = g.selectAll('g.node').data(nodes, d => d.id || (d.id = ++i));
  const nodeEnter = node.enter().append('g')
      .attr('class', 'node')
      .attr('transform', d => `translate(${source.y0},${source.x0})`)
      .on('click', (event, d) => {
         activeNode = d;
         if (d.children) { d._children = d.children; d.children = null; }
         else { d.children = d._children; d._children = null; }
         update(d);
      });

  nodeEnter.append('circle').attr('r', 30);
  nodeEnter.append('text')
      .attr('dy', '.35em')
      .attr('x', d => d.children || d._children ? -55 : 55)
      .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
      .text(d => d.data.name);

  const nodeUpdate = nodeEnter.merge(node);
  nodeUpdate.transition().duration(duration).attr('transform', d => `translate(${d.y},${d.x})`);

  nodeUpdate.select('circle')
      .style('fill', d => isNodeInActivePath(d) ? '#ff9100' : (d._children ? '#ff9100' : '#00FFFF'))
      .style('stroke', '#fff').style('stroke-width', '8px');

  node.exit().transition().duration(duration).attr('transform', d => `translate(${source.y},${source.x})`).remove();

  const link = g.selectAll('path.link').data(links, d => d.id);
  const linkEnter = link.enter().insert('path', 'g').attr('class', 'link')
      .attr('d', d => { const o = {x: source.x0, y: source.y0}; return diagonal(o, o); });

  linkEnter.merge(link).transition().duration(duration)
      .attr('d', d => diagonal(d, d.parent))
      .style('stroke', d => isNodeInActivePath(d) ? '#ff9100' : '#00FFFF')
      .style('stroke-width', d => isNodeInActivePath(d) ? '36px' : '18px')
      .style('opacity', d => isNodeInActivePath(d) ? 1 : 0.4);

  link.exit().transition().duration(duration).attr('d', d => { const o = {x: source.x, y: source.y}; return diagonal(o, o); }).remove();

  nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });

  // MOTOR DE ENFOQUE AUTOMÁTICO
  const minX = d3.min(nodes, d => d.x) - 200;
  const maxX = d3.max(nodes, d => d.x) + 200;
  const minY = d3.min(nodes, d => d.y) - 200;
  const maxY = d3.max(nodes, d => d.y) + 800;

  const treeWidth = maxY - minY;
  const treeHeight = maxX - minX;
  const container = d3.select('#tree-container').node();
  const width = container.getBoundingClientRect().width;
  const height = container.getBoundingClientRect().height;

  const scale = 0.85 / Math.max(treeWidth / width, treeHeight / height);
  const transform = d3.zoomIdentity
    .translate(width / 2 - scale * (minY + treeWidth / 2),
               height / 2 - scale * (minX + treeHeight / 2))
    .scale(scale);

  d3.select('#tree-container svg')
    .transition().duration(duration)
    .call(zoom.transform, transform);
}

function isNodeInActivePath(d) {
  if (!activeNode) return false;
  let curr = activeNode;
  while(curr) { if (curr === d) return true; curr = curr.parent; }
  return false;
}

function diagonal(s, d) { return `M ${s.y} ${s.x} C ${(s.y + d.y) / 2} ${s.x}, ${(s.y + d.y) / 2} ${d.x}, ${d.y} ${d.x}`; }

$(document).on('shiny:connected', function() {
  initTree();
});
"

# --- 3. UI ---
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://d3js.org/d3.v6.min.js"),
    tags$style(HTML("
      body, html { height: 100%; width: 100%; background-color: #000; overflow: hidden; margin: 0; padding: 0; }
      #tree-container { background: #000; height: 100vh; width: 100vw; position: relative; cursor: grab; }
      #tree-container:active { cursor: grabbing; }

      .node text { pointer-events: none; fill: #fff; font-size: 40px !important; font-weight: 900; text-shadow: 6px 6px 12px #000; }
      .link { fill: none; transition: stroke 0.4s, stroke-width 0.4s; }

      /* PANEL DE CONTROL FLOTANTE */
      .internal-controls {
        position: absolute;
        top: 20px;
        left: 20px;
        display: flex;
        flex-direction: column;
        gap: 10px;
        z-index: 999;
        background: rgba(0,0,0,0.6);
        padding: 15px;
        border-radius: 12px;
        border: 1px solid #333;
        backdrop-filter: blur(5px);
      }

      .btn-custom {
        background: #1a1a1a;
        color: #00FFFF;
        border: 2px solid #00FFFF;
        padding: 10px 20px;
        font-weight: bold;
        text-transform: uppercase;
        font-size: 14px;
        cursor: pointer;
        transition: all 0.3s;
        border-radius: 6px;
        text-align: left;
      }

      .btn-custom:hover { background: #00FFFF; color: #000; box-shadow: 0 0 15px #00FFFF; }
      .btn-reset { border-color: #ff4444; color: #ff4444; }
      .btn-reset:hover { background: #ff4444; color: #fff; box-shadow: 0 0 15px #ff4444; }
      .btn-focus { border-color: #ff9100; color: #ff9100; }
      .btn-focus:hover { background: #ff9100; color: #fff; box-shadow: 0 0 15px #ff9100; }

      .brand { color: #fff; font-size: 12px; opacity: 0.5; margin-bottom: 5px; font-weight: bold; }
    ")),
    tags$script(HTML(sprintf("var treeData = %s;", preparar_jerarquia("arbol_estadistico.xlsx"))))
  ),

  div(id = "tree-container",
      # Botones dentro del diagrama
      div(class = "internal-controls",
          div(class = "brand", "RSCIENCE v6.0"),
          tags$button("↺ Resetear Mapa", class = "btn-custom btn-reset", onclick = "resetMap()"),
          tags$button("⇱ Abrir Full", class = "btn-custom", onclick = "fullExpand()"),
          tags$button("🎯 Solo Seleccionados", class = "btn-custom btn-focus", onclick = "showSelectedOnly()"),
          tags$button("📷 Capturar Vista", class = "btn-custom", onclick = "downloadView()")
      )
  ),
  tags$script(HTML(js_logic))
)

# --- 4. SERVER ---
server <- function(input, output, session) {}

shinyApp(ui, server)
