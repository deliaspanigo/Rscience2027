# --- 0. CONFIGURACIÓN ---
if (interactive()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

library(shiny)
library(readxl)
library(jsonlite)
library(dplyr)
library(bslib)

# --- 1. MOTOR DE DATOS (R) ---
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

# --- 2. JAVASCRIPT (D3.js v.4.5.0) ---
js_logic <- "
var root, svg, g, treemap, i = 0, duration = 750, activeNode = null;

function initTree() {
  const container = d3.select('#tree-container');
  svg = container.append('svg')
      .attr('width', '100%')
      .attr('height', '100%')
      .call(d3.zoom().scaleExtent([0.005, 3]).on('zoom', (event) => {
          g.attr('transform', event.transform);
      }))
      .append('g');

  // Zoom inicial muy alejado para que el texto de 40px quepa en pantalla al arrancar
  g = svg.append('g').attr('transform', 'translate(100, 400) scale(0.15)');

  // nodeSize EXTREMO: 250px de espacio vertical entre nodos
  treemap = d3.tree().nodeSize([250, 800]);

  root = d3.hierarchy(treeData, d => d.children);
  root.x0 = 0; root.y0 = 0;

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
  const treeData = treemap(root);
  const nodes = treeData.descendants(), links = treeData.descendants().slice(1);

  // Separación horizontal de 1000px para que los nombres gigantes no se pisen
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

  nodeEnter.append('circle').attr('r', 30); // Círculos de 30px para anclar letra de 40px
  nodeEnter.append('text')
      .attr('dy', '.35em')
      .attr('x', d => d.children || d._children ? -50 : 50)
      .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
      .text(d => d.data.name);

  const nodeUpdate = nodeEnter.merge(node);
  nodeUpdate.transition().duration(duration).attr('transform', d => `translate(${d.y},${d.x})`);

  nodeUpdate.select('circle')
      .style('fill', d => isNodeInActivePath(d) ? '#ff9100' : (d._children ? '#ff9100' : '#00FFFF'))
      .style('stroke', '#fff')
      .style('stroke-width', '8px');

  node.exit().transition().duration(duration)
      .attr('transform', d => `translate(${source.y},${source.x})`).remove();

  const link = g.selectAll('path.link').data(links, d => d.id);
  const linkEnter = link.enter().insert('path', 'g').attr('class', 'link')
      .attr('d', d => { const o = {x: source.x0, y: source.y0}; return diagonal(o, o); });

  linkEnter.merge(link).transition().duration(duration)
      .attr('d', d => diagonal(d, d.parent))
      .style('stroke', d => isNodeInActivePath(d) ? '#ff9100' : '#00FFFF')
      .style('stroke-width', d => isNodeInActivePath(d) ? '36px' : '18px')
      .style('opacity', d => isNodeInActivePath(d) ? 1 : 0.4);

  link.exit().transition().duration(duration)
      .attr('d', d => { const o = {x: source.x, y: source.y}; return diagonal(o, o); }).remove();

  nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });
}

function isNodeInActivePath(d) {
  if (!activeNode) return false;
  let curr = activeNode;
  while(curr) {
    if (curr === d) return true;
    curr = curr.parent;
  }
  return false;
}

function diagonal(s, d) { return `M ${s.y} ${s.x} C ${(s.y + d.y) / 2} ${s.x}, ${(s.y + d.y) / 2} ${d.x}, ${d.y} ${d.x}`; }

$(document).on('shiny:connected', function() {
  initTree();
  Shiny.addCustomMessageHandler('toggle_expand', function(msg) {
    if(msg) { expand(root); } else { root.children.forEach(collapse); activeNode = null; }
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

      #tree-container {
        background: #000;
        height: 85vh;
        width: 100vw;
        border-top: 4px solid #00FFFF;
        cursor: grab;
      }
      #tree-container:active { cursor: grabbing; }

      /* ESTADOS DEL CURSOR */
      .node { cursor: pointer; }

      .node text {
        pointer-events: none;
        fill: #fff;
        font-size: 40px !important; /* TEXTO GIGANTE 40PX */
        font-weight: 900;
        text-shadow: 6px 6px 12px #000;
      }
      .link { fill: none; transition: stroke 0.4s, stroke-width 0.4s; }

      .controls {
        background: #111;
        padding: 20px 40px;
        border-bottom: 2px solid #333;
      }
      h2 { font-weight: 900; color: #00FFFF; margin: 0; font-size: 32px; }
      .shiny-input-container { font-size: 26px; font-weight: bold; color: #ff9100 !important; }
    ")),
    tags$script(HTML(sprintf("var treeData = %s;", preparar_jerarquia("arbol_estadistico.xlsx"))))
  ),

  div(class = "controls",
      fluidRow(
        column(5, h2("Rscience MURAL-ENGINE")),
        column(7, checkboxInput("expand_all", "DESPLEGAR MAPA COMPLETO", value = FALSE))
      )
  ),

  div(id = "tree-container"),
  tags$script(HTML(js_logic))
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  observe({
    session$sendCustomMessage("toggle_expand", input$expand_all)
  })
}

shinyApp(ui, server)
