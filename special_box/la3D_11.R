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

# --- 2. JAVASCRIPT ---
js_logic <- "
var root, svg, g, treemap, zoom, i = 0, duration = 600, activeNode = null, ghostMode = false, topAlignMode = false;

function initTree() {
  const container = d3.select('#tree-container');
  const width = container.node().getBoundingClientRect().width;
  const height = container.node().getBoundingClientRect().height;

  zoom = d3.zoom().scaleExtent([0.001, 4]).on('zoom', (event) => {
    g.attr('transform', event.transform);
  });

  svg = container.append('svg').attr('width', '100%').attr('height', '100%').call(zoom).append('g');
  g = svg.append('g');
  treemap = d3.tree().nodeSize([250, 1000]);

  root = d3.hierarchy(treeData, d => d.children);
  root.x0 = height / 2;
  root.y0 = 0;

  activeNode = root;
  update(root);
}

function toggleMenu() { document.getElementById('menu-content').classList.toggle('hidden'); }

function resetMap() {
  activeNode = root;
  ghostMode = false;
  topAlignMode = false;
  // Resetear orden original si fuera necesario (requeriría backup, por ahora reset visual)
  if (root.children) root.children.forEach(collapse);
  update(root);
}

function fullExpand() { expand(root); update(root); }

function toggleGhostMode() { ghostMode = !ghostMode; update(activeNode || root); }

function toggleTopAlign() {
  topAlignMode = !topAlignMode;
  if (activeNode) reorderPathToTop(activeNode);
  update(activeNode || root);
}

// FUNCIÓN MAESTRA DE REORDENAMIENTO
function reorderPathToTop(d) {
  let curr = d;
  while (curr && curr.parent) {
    let p = curr.parent;
    // Buscamos el índice del nodo actual en los hijos de su padre
    let children = p.children || p._children;
    if (children) {
      let index = children.indexOf(curr);
      if (index > -1) {
        // Lo movemos a la primera posición (arriba)
        children.splice(index, 1);
        children.unshift(curr);
      }
    }
    curr = p;
  }
}

function showSelectedOnly() {
  if (!activeNode) return;
  let path = [];
  let curr = activeNode;
  while(curr) { path.push(curr); curr = curr.parent; }
  root.descendants().forEach(d => {
    if (!path.includes(d) && d.children) { d._children = d.children; d.children = null; }
  });
  update(root);
}

function collapse(d) { if(d.children) { d._children = d.children; d._children.forEach(collapse); d.children = null; } }
function expand(d) { if(d._children) { d.children = d._children; d._children = null; } if(d.children) d.children.forEach(expand); }

function update(source) {
  // Si el modo Top-Align está activo, reordenamos antes de calcular el layout
  if (topAlignMode && activeNode) {
    reorderPathToTop(activeNode);
  }

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
  nodeEnter.append('text').attr('dy', '.35em')
      .attr('x', d => d.children || d._children ? -55 : 55)
      .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
      .text(d => d.data.name);

  const nodeUpdate = nodeEnter.merge(node);
  nodeUpdate.transition().duration(duration).attr('transform', d => `translate(${d.y},${d.x})`);

  nodeUpdate.select('circle')
      .style('fill', d => {
        if (d === activeNode || isAncestorOfActive(d)) return '#ff9100';
        if (!d.children && !d._children) return '#00FF00';
        return '#00FFFF';
      })
      .style('stroke', '#fff').style('stroke-width', '8px')
      .style('opacity', d => (ghostMode && d !== activeNode && !isAncestorOfActive(d)) ? 0.1 : 1);

  nodeUpdate.select('text')
      .style('opacity', d => (ghostMode && d !== activeNode && !isAncestorOfActive(d)) ? 0.1 : 1);

  node.exit().transition().duration(duration).attr('transform', d => `translate(${source.y},${source.x})`).remove();

  const link = g.selectAll('path.link').data(links, d => d.id);
  const linkEnter = link.enter().insert('path', 'g').attr('class', 'link')
      .attr('d', d => { const o = {x: source.x0, y: source.y0}; return diagonal(o, o); });

  linkEnter.merge(link).transition().duration(duration)
      .attr('d', d => diagonal(d, d.parent))
      .style('stroke', d => (d === activeNode || isAncestorOfActive(d)) ? '#ff9100' : '#00FFFF')
      .style('stroke-width', d => (d === activeNode || isAncestorOfActive(d)) ? '36px' : '18px')
      .style('opacity', d => {
        if (ghostMode && !isAncestorOfActive(d) && d !== activeNode) return 0.05;
        return (d === activeNode || isAncestorOfActive(d)) ? 1 : 0.4;
      });

  link.exit().transition().duration(duration).attr('d', d => { const o = {x: source.x, y: source.y}; return diagonal(o, o); }).remove();

  nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });

  // FOCO
  const minX = d3.min(nodes, d => d.x) - 200;
  const maxX = d3.max(nodes, d => d.x) + 200;
  const minY = d3.min(nodes, d => d.y) - 200;
  const maxY = d3.max(nodes, d => d.y) + 800;
  const width = window.innerWidth, height = window.innerHeight;
  const scale = 0.85 / Math.max((maxY - minY) / width, (maxX - minX) / height);
  const transform = d3.zoomIdentity.translate(width / 2 - scale * (minY + (maxY - minY) / 2), height / 2 - scale * (minX + (maxX - minX) / 2)).scale(scale);
  d3.select('#tree-container svg').transition().duration(duration).call(zoom.transform, transform);
}

function isAncestorOfActive(d) {
  if (!activeNode) return false;
  let curr = activeNode.parent;
  while(curr) { if (curr === d) return true; curr = curr.parent; }
  return false;
}

function diagonal(s, d) { return `M ${s.y} ${s.x} C ${(s.y + d.y) / 2} ${s.x}, ${(s.y + d.y) / 2} ${d.x}, ${d.y} ${d.x}`; }
$(document).on('shiny:connected', function() { initTree(); });
"

# --- 3. UI ---
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://d3js.org/d3.v6.min.js"),
    tags$style(HTML("
      body, html { height: 100%; width: 100%; background-color: #000; overflow: hidden; margin: 0; padding: 0; }
      #tree-container { background: #000; height: 100vh; width: 100vw; position: relative; }
      .node text { pointer-events: none; fill: #fff; font-size: 40px !important; font-weight: 900; text-shadow: 6px 6px 12px #000; transition: opacity 0.4s; }
      .link { fill: none; transition: all 0.4s; }

      .internal-controls {
        position: absolute; top: 20px; left: 20px; z-index: 1000;
        background: rgba(10, 25, 30, 0.9); border: 2px solid #00FFFF; border-radius: 8px;
        color: #00FFFF; padding: 10px; font-family: 'Segoe UI', sans-serif;
        box-shadow: 0 0 15px rgba(0, 255, 255, 0.3);
      }
      .menu-header { cursor: pointer; display: flex; align-items: center; justify-content: space-between; gap: 20px; font-weight: bold; font-size: 18px; padding: 5px; }
      .hidden { display: none !important; }
      .btn-stack { display: flex; flex-direction: column; gap: 8px; margin-top: 10px; border-top: 1px solid rgba(0, 255, 255, 0.3); padding-top: 10px; }
      .btn-custom { background: transparent; color: #00FFFF; border: 1px solid #00FFFF; padding: 10px 15px; font-weight: bold; text-transform: uppercase; font-size: 11px; cursor: pointer; transition: all 0.2s; border-radius: 4px; text-align: left; }
      .btn-custom:hover { background: #00FFFF; color: #000; box-shadow: 0 0 10px #00FFFF; }
      .btn-active { background: #00FFFF !important; color: #000 !important; }
      .btn-ghost { border-color: #BB86FC; color: #BB86FC; }
      .btn-ghost:hover { background: #BB86FC; color: #000; }
      .btn-snap { color: #ff9100; border-color: #ff9100; }
    "))
  ),
  div(id = "tree-container",
      div(class = "internal-controls",
          div(class = "menu-header", onclick = "toggleMenu()", span("☰ COMANDOS"), span("▼")),
          div(id = "menu-content", class = "btn-stack",
              tags$button("↺ Resetear Vista", class = "btn-custom", onclick = "resetMap()"),
              tags$button("⇱ Abrir Todo", class = "btn-custom", onclick = "fullExpand()"),
              tags$button("🎯 Colapsar Otros", class = "btn-custom", onclick = "showSelectedOnly()"),
              tags$button("👻 Ghost Mode", class = "btn-custom btn-ghost", onclick = "toggleGhostMode()"),
              tags$button("🔝 Alinear Superior", class = "btn-custom", id="btn-topalign", onclick = "this.classList.toggle('btn-active'); toggleTopAlign()"),
              tags$button("📷 Captura", class = "btn-custom btn-snap", onclick = "alert('Capturando...')")
          )
      )
  ),
  tags$script(HTML(sprintf("var treeData = %s;", preparar_jerarquia("arbol_estadistico.xlsx")))),
  tags$script(HTML(js_logic))
)

server <- function(input, output, session) {}
shinyApp(ui, server)
