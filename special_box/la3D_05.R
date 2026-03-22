# --- 0. CONFIGURACIÓN DE ENTORNO ---
# Forzamos que el directorio de trabajo sea donde está este archivo
# if (interactive()) {
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# }

library(shiny)
library(readxl)
library(jsonlite)
library(dplyr)
library(bslib)

# --- 1. PROCESAMIENTO DE DATOS (R) ---
preparar_jerarquia <- function(ruta_excel) {
  # Validación de existencia
  if (!file.exists(ruta_excel)) {
    return(toJSON(list(name = "ARCHIVO NO ENCONTRADO"), auto_unbox = TRUE))
  }

  tryCatch({
    # Leer Excel
    df <- read_xlsx(ruta_excel)

    # Limpieza extrema: Todo a texto y quitar filas vacías
    df <- df %>%
      mutate(across(everything(), as.character)) %>%
      filter(!is.na(Nivel1))

    # Función recursiva para crear el JSON que D3 necesita
    build_node <- function(data, name = "Rscience") {
      node <- list(name = as.character(name))
      if (ncol(data) > 0) {
        # Obtenemos hijos únicos ignorando NAs
        children_names <- unique(data[[1]])
        children_names <- children_names[!is.na(children_names) & children_names != "NA" & children_names != ""]

        if (length(children_names) > 0) {
          node$children <- lapply(children_names, function(cn) {
            # Recursión: mandamos el resto de las columnas para ese hijo
            child_data <- data[data[[1]] == cn, -1, drop = FALSE]
            build_node(child_data, cn)
          })
        }
      }
      return(node)
    }

    # Seleccionar solo niveles existentes
    niveles <- intersect(names(df), c("Nivel1", "Nivel2", "Nivel3", "Nivel4", "Nivel5"))
    df_clean <- df[, niveles]

    return(toJSON(build_node(df_clean), auto_unbox = TRUE))

  }, error = function(e) {
    return(toJSON(list(name = paste("Error R:", e$message)), auto_unbox = TRUE))
  })
}

# --- 2. LÓGICA JAVASCRIPT (D3.js) ---
js_logic <- "
$(document).on('shiny:connected', function() {
  const container = d3.select('#tree-container');
  const width = container.node().getBoundingClientRect().width;
  const height = 800;

  // Crear SVG con Zoom
  const svg = container.append('svg')
      .attr('width', '100%')
      .attr('height', height)
      .call(d3.zoom().scaleExtent([0.1, 3]).on('zoom', (event) => {
          g.attr('transform', event.transform);
      }))
      .append('g');

  const g = svg.append('g')
      .attr('transform', 'translate(150, 400) scale(0.7)');

  let i = 0, duration = 750, root;
  const treemap = d3.tree().nodeSize([60, 250]);

  root = d3.hierarchy(treeData, d => d.children);
  root.x0 = 0; root.y0 = 0;

  function collapse(d) {
    if(d.children) {
      d._children = d.children;
      d._children.forEach(collapse);
      d.children = null;
    }
  }

  if (root.children) root.children.forEach(collapse);
  update(root);

  function update(source) {
    const treeData = treemap(root);
    const nodes = treeData.descendants(), links = treeData.descendants().slice(1);
    nodes.forEach(d => d.y = d.depth * 280);

    const node = g.selectAll('g.node').data(nodes, d => d.id || (d.id = ++i));

    const nodeEnter = node.enter().append('g')
        .attr('class', 'node')
        .attr('transform', d => `translate(${source.y0},${source.x0})`)
        .on('click', (event, d) => {
           if (d.children) { d._children = d.children; d.children = null; }
           else { d.children = d._children; d._children = null; }
           update(d);
           highlightPath(d);
        })
        .on('mouseover', function() {
          d3.select(this).select('circle').transition().duration(200).style('fill', '#fff');
          d3.select(this).select('text').transition().duration(200).style('fill', '#ff9100').style('font-size', '16px');
        })
        .on('mouseout', function(event, d) {
          d3.select(this).select('circle').transition().duration(200).style('fill', d._children ? '#ff9100' : '#444');
          d3.select(this).select('text').transition().duration(200).style('fill', '#fff').style('font-size', '13px');
        });

    nodeEnter.append('circle').attr('r', 10).style('fill', d => d._children ? '#ff9100' : '#444').style('stroke', '#ff9100');
    nodeEnter.append('text')
        .attr('dy', '.35em')
        .attr('x', d => d.children || d._children ? -18 : 18)
        .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
        .text(d => d.data.name);

    const nodeUpdate = nodeEnter.merge(node);
    nodeUpdate.transition().duration(duration).attr('transform', d => `translate(${d.y},${d.x})`);

    node.exit().transition().duration(duration).attr('transform', d => `translate(${source.y},${source.x})`).remove();

    const link = g.selectAll('path.link').data(links, d => d.id);
    const linkEnter = link.enter().insert('path', 'g').attr('class', 'link')
        .attr('d', d => { const o = {x: source.x0, y: source.y0}; return diagonal(o, o); });
    linkEnter.merge(link).transition().duration(duration).attr('d', d => diagonal(d, d.parent));
    link.exit().transition().duration(duration).attr('d', d => { const o = {x: source.x, y: source.y}; return diagonal(o, o); }).remove();

    nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });
    function diagonal(s, d) { return `M ${s.y} ${s.x} C ${(s.y + d.y) / 2} ${s.x}, ${(s.y + d.y) / 2} ${d.x}, ${d.y} ${d.x}`; }

    function highlightPath(d) {
      g.selectAll('path.link').style('stroke', '#444').style('opacity', 0.2).style('stroke-width', '2px');
      g.selectAll('g.node').style('opacity', 0.4);
      let curr = d;
      while(curr) {
        g.selectAll('g.node').filter(n => n === curr).style('opacity', 1);
        if(curr.parent) {
          g.selectAll('path.link').filter(l => l === curr).style('stroke', '#ff9100').style('opacity', 1).style('stroke-width', '4px');
        }
        curr = curr.parent;
      }
    }
  }
});"

# --- 3. UI ---
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  tags$head(
    tags$script(src = "https://d3js.org/d3.v6.min.js"),
    tags$style(HTML("
      #tree-container {
        background: #000; border-radius: 15px; height: 85vh; width: 100%;
        border: 1px solid #333; cursor: grab; margin-top: 10px;
      }
      #tree-container:active { cursor: grabbing; }
      .node circle { cursor: pointer; stroke-width: 3px; }
      .node text { pointer-events: none; fill: #fff; font-size: 13px; font-weight: bold; text-shadow: 2px 2px 3px #000; }
      .link { fill: none; stroke: #444; stroke-width: 2px; }
    ")),
    # Inyectamos datos
    tags$script(HTML(sprintf("var treeData = %s;", preparar_jerarquia("arbol_estadistico.xlsx"))))
  ),
  titlePanel("Rscience Interactive Engine v.3.7"),
  div(id = "tree-container"),
  tags$script(HTML(js_logic))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
