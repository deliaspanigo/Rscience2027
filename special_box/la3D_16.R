# --- 0. CONFIGURACIÓN DE RUTAS ---
library(shiny)
library(jsonlite)
library(dplyr)
library(bslib)

# Detectar ruta para el archivo JS (d3.v6.min.js)
ruta_base <- system.file(package = "Rscience2027")
if (ruta_base == "") {
  posible_raiz <- getwd()
  ruta_base <- if (basename(posible_raiz) == "special_box") dirname(posible_raiz) else posible_raiz
}

# Registrar la ruta para que tags$script(src = "d3.v6.min.js") funcione
shiny::addResourcePath(
  prefix = "libs",
  directoryPath = normalizePath(file.path(ruta_base, "external_soft"))
)

# --- 1. MOTOR DE DATOS (CONECTADO A TU OBJETO R) ---
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

# Preparar el JSON usando tu objeto tree_data
if (!exists("tree_data")) {
  # Intentar cargar si no está en el environment
  ruta_rda <- file.path(ruta_base, "data", "tree_data.rda")
  if(file.exists(ruta_rda)) load(ruta_rda)
}

# Procesamos el tibble para que D3 lo entienda
niveles_cols <- tree_data %>% select(starts_with("nivel"))
tree_json <- toJSON(df_a_jerarquia(niveles_cols), auto_unbox = TRUE)

# --- 2. JAVASCRIPT (TU VERSIÓN ORIGINAL INTACTA) ---
js_logic <- "
var root, svg, g, treemap, zoom, i = 0, duration = 600, activeNode = null;
var ghostMode = false, topAlignMode = false;
var originalOrder = new Map();

function initTree() {
  const container = d3.select('#tree-container');
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

  if (root.children) root.children.forEach(collapse);
  update(root);
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
  restoreOriginalOrder(root);
  if (root.children) root.children.forEach(collapse);
  update(root);
}

function fullExpand() { expand(root); update(root); }

function runCollapseOthers() {
  if (!activeNode) return;
  let path = [];
  let curr = activeNode;
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
      .style('stroke', d => {
        if ((!d.children && !d._children) && (d === activeNode || isAncestor(d))) return '#00FF00';
        return '#fff';
      })
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
        return (d === activeNode || isAncestor(d)) ? 1 : 0.4;
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
$(document).on('shiny:connected', initTree);
"

# --- 3. UI (TU VERSIÓN ORIGINAL INTACTA) ---
ui <- fluidPage(
  tags$head(
    # Referencia al archivo JS mediante el alias 'libs'
    tags$script(src = "libs/d3.v6.min.js"),
    tags$style(HTML("
      body, html { height: 100%; width: 100%; background-color: #000; overflow: hidden; margin: 0; padding: 0; }
      #tree-container { background: #000; height: 100vh; width: 100vw; position: relative; }
      .node text { pointer-events: none; fill: #fff; font-size: 40px !important; font-weight: 900; text-shadow: 6px 6px 12px #000; transition: opacity 0.4s; }
      .link { fill: none; transition: all 0.4s; }
      .panel-ctrl {
        position: absolute; z-index: 1000;
        background: rgba(10, 20, 25, 0.9); border: 2px solid #00FFFF; border-radius: 12px;
        color: #00FFFF; padding: 15px; font-family: 'Segoe UI', sans-serif;
        box-shadow: 0 0 20px rgba(0, 255, 255, 0.2);
      }
      .panel-left { top: 20px; left: 20px; width: 220px; }
      .panel-right { top: 20px; right: 20px; width: 260px; }
      .panel-title { font-weight: 900; font-size: 14px; margin-bottom: 12px; border-bottom: 1px solid #00FFFF; padding-bottom: 5px; opacity: 0.8; }
      .btn-cmd {
        background: transparent; color: #00FFFF; border: 1px solid #00FFFF;
        padding: 10px; width: 100%; font-weight: bold; font-size: 11px;
        cursor: pointer; margin-bottom: 8px; border-radius: 6px; transition: 0.2s;
        text-transform: uppercase; text-align: center;
      }
      .btn-cmd:hover { background: #00FFFF; color: #000; box-shadow: 0 0 10px #00FFFF; }
      .btn-snap { border-color: #ff9100; color: #ff9100; }
      .btn-snap:hover { background: #ff9100; color: #000; box-shadow: 0 0 10px #ff9100; }
      .btn-focus { border-color: #ADFF2F; color: #ADFF2F; }
      .btn-focus:hover { background: #ADFF2F; color: #000; box-shadow: 0 0 10px #ADFF2F; }
      .switch-container { display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px; }
      .switch-label { font-size: 12px; font-weight: bold; }
      .switch { position: relative; display: inline-block; width: 44px; height: 22px; }
      .switch input { opacity: 0; width: 0; height: 0; }
      .slider {
        position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0;
        background-color: #333; transition: .4s; border-radius: 22px; border: 1px solid #555;
      }
      .slider:before {
        position: absolute; content: ''; height: 14px; width: 14px; left: 4px; bottom: 3px;
        background-color: white; transition: .4s; border-radius: 50%;
      }
      input:checked + .slider { background-color: #00FFFF; border-color: #00FFFF; }
      input:checked + .slider:before { transform: translateX(20px); background-color: #000; }
    "))
  ),
  div(id = "tree-container",
      div(class = "panel-ctrl panel-left",
          div(class = "panel-title", "⚡ ACCIONES"),
          tags$button("↺ Resetear Vista", class = "btn-cmd", onclick = "resetMap()"),
          tags$button("🎯 Colapsar Otros", class = "btn-cmd btn-focus", onclick = "runCollapseOthers()"),
          tags$button("⇱ Abrir Todo", class = "btn-cmd", onclick = "fullExpand()"),
          tags$button("📷 Captura", class = "btn-cmd btn-snap", onclick = "alert('Captura realizada')")
      ),
      div(class = "panel-ctrl panel-right",
          div(class = "panel-title", "🛠️ MODOS DE VISTA"),
          div(class = "switch-container",
              span(class = "switch-label", "👻 GHOST MODE"),
              tags$label(class = "switch", tags$input(type = "checkbox", onclick = "toggleGhost()"), span(class = "slider"))
          ),
          div(class = "switch-container",
              span(class = "switch-label", "🔝 ALINEAR SUPERIOR"),
              tags$label(class = "switch", tags$input(type = "checkbox", onclick = "toggleTopAlign()"), span(class = "slider"))
          )
      )
  ),
  # Inyectamos el JSON generado desde tree_data
  tags$script(HTML(sprintf("var treeData = %s;", tree_json))),
  tags$script(HTML(js_logic))
)

server <- function(input, output, session) {}

# Lanzar App
shinyApp(ui, server)
