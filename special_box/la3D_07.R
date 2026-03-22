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

# --- 2. JAVASCRIPT CON FOCO CORREGIDO ---
js_logic <- "
var root, svg, g, treemap, zoom, i = 0, duration = 600, activeNode = null;

function initTree() {
  const container = d3.select('#tree-container');
  const width = container.node().getBoundingClientRect().width;
  const height = container.node().getBoundingClientRect().height;

  zoom = d3.zoom().scaleExtent([0.001, 3]).on('zoom', (event) => {
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

  if (root.children) root.children.forEach(collapse);
  update(root);
}

function collapse(d) {
  if(d.children) {
    d._children = d.children;
    d._children.forEach(collapse);
    d.children = null;
  }
}

function expand(d) {
  if(d._children) { d.children = d._children; d._children = null; }
  if(d.children) d.children.forEach(expand);
}

function update(source) {
  // 1. Calculamos la estructura de nodos
  const nodes = treemap(root).descendants();
  const links = nodes.slice(1);
  nodes.forEach(d => d.y = d.depth * 1000);

  // 2. Dibujamos Nodos
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

  // 3. Dibujamos Líneas
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

  // 4. MOTOR DE ENFOQUE (CORREGIDO)
  // Calculamos los límites basados en los datos de los nodos, no en el dibujo actual
  const minX = d3.min(nodes, d => d.x) - 200;
  const maxX = d3.max(nodes, d => d.x) + 200;
  const minY = d3.min(nodes, d => d.y) - 200;
  const maxY = d3.max(nodes, d => d.y) + 600; // Más espacio a la derecha por el texto 40px

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
  Shiny.addCustomMessageHandler('toggle_expand', function(msg) {
    if(msg) { expand(root); } else { if(root.children) root.children.forEach(collapse); activeNode = null; }
    update(root);
  });
});
"

# --- 3. UI ---
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  tags$head(
    tags$script(src = "https://d3js.org/d3.v6.min.js"),
    tags$style(HTML("
      body, html { height: 100%; width: 100%; background-color: #000; overflow: hidden; margin: 0; }
      .container-fluid { padding: 0 !important; }
      #tree-container { background: #000; height: 85vh; width: 100vw; border-top: 4px solid #00FFFF; cursor: grab; }
      #tree-container:active { cursor: grabbing; }
      .node { cursor: pointer; }
      .node text { pointer-events: none; fill: #fff; font-size: 40px !important; font-weight: 900; text-shadow: 6px 6px 12px #000; }
      .link { fill: none; transition: stroke 0.4s, stroke-width 0.4s; }
      .controls { background: #111; padding: 20px 40px; border-bottom: 2px solid #333; }
      h2 { font-weight: 900; color: #00FFFF; margin: 0; font-size: 32px; }
      .shiny-input-container { font-size: 26px; font-weight: bold; color: #ff9100 !important; }
    ")),
    tags$script(HTML(sprintf("var treeData = %s;", preparar_jerarquia("arbol_estadistico.xlsx"))))
  ),
  div(class = "controls",
      fluidRow(
        column(5, h2("Rscience FIXED-FOCUS")),
        column(7, checkboxInput("expand_all", "DESPLEGAR MAPA COMPLETO", value = FALSE))
      )
  ),
  div(id = "tree-container"),
  tags$script(HTML(js_logic))
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  observe({ session$sendCustomMessage("toggle_expand", input$expand_all) })
}

shinyApp(ui, server)
