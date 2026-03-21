library(shiny)
library(readxl)
library(jsonlite)
library(dplyr)

# --- 1. PROCESAMIENTO DE DATOS (R) ---
# Esta función prepara tu Excel para que JavaScript lo entienda como un árbol
preparar_jerarquia <- function(ruta_excel) {
  tryCatch({
    df <- read_xlsx(ruta_excel)

    build_node <- function(data, name = "Rscience") {
      node <- list(name = name)
      # Si hay más columnas, seguimos profundizando
      if (ncol(data) > 0) {
        children_names <- unique(data[[1]])
        # Filtramos NAs para evitar nodos vacíos
        children_names <- children_names[!is.na(children_names)]

        if (length(children_names) > 0) {
          node$children <- lapply(children_names, function(cn) {
            child_data <- data[data[[1]] == cn, -1, drop = FALSE]
            build_node(child_data, cn)
          })
        }
      }
      return(node)
    }

    # Seleccionamos las 5 columnas de niveles
    df_clean <- df[, c("Nivel1", "Nivel2", "Nivel3", "Nivel4", "Nivel5")]
    json_data <- build_node(df_clean)
    return(toJSON(json_data, auto_unbox = TRUE))

  }, error = function(e) {
    # En caso de error, devolvemos una estructura mínima para que no rompa la App
    return(toJSON(list(name = "Error al cargar Excel"), auto_unbox = TRUE))
  })
}

# --- 2. LÓGICA JAVASCRIPT (D3.js v6) ---
# Incluye Zoom, Pan, Animaciones y Highlighting
js_logic <- "
$(document).on('shiny:connected', function() {
  const container = d3.select('#tree-container');
  const width = $('#tree-container').width();
  const height = 800;

  // Limpiar contenedor por si acaso
  container.selectAll('*').remove();

  // Crear el SVG base con ZOOM y PAN
  const svg = container.append('svg')
      .attr('width', '100%')
      .attr('height', height)
      .style('cursor', 'grab')
      .call(d3.zoom().scaleExtent([0.2, 3]).on('zoom', (event) => {
          g.attr('transform', event.transform);
      }))
      .append('g');

  // Grupo principal donde se dibuja el árbol
  // Lo centramos un poco a la izquierda y al medio
  const g = svg.append('g')
      .attr('transform', 'translate(120, 400) scale(0.8)');

  let i = 0, duration = 750, root;

  // nodeSize controla el espacio entre nodos [alto, ancho]
  const treemap = d3.tree().nodeSize([50, 220]);

  // Convertir datos a jerarquía D3
  root = d3.hierarchy(treeData, d => d.children);
  root.x0 = 0;
  root.y0 = 0;

  // Función para colapsar nodos
  function collapse(d) {
    if(d.children) {
      d._children = d.children;
      d._children.forEach(collapse);
      d.children = null;
    }
  }

  // Colapsar todo menos la raíz al inicio
  if (root.children) {
    root.children.forEach(collapse);
  }

  update(root);

  function update(source) {
    const treeData = treemap(root);
    const nodes = treeData.descendants(),
          links = treeData.descendants().slice(1);

    // Ajuste de profundidad horizontal
    nodes.forEach(d => d.y = d.depth * 250);

    // --- NODOS ---
    const node = g.selectAll('g.node')
        .data(nodes, d => d.id || (d.id = ++i));

    const nodeEnter = node.enter().append('g')
        .attr('class', 'node')
        .attr('transform', d => `translate(${source.y0},${source.x0})`)
        .on('click', (event, d) => {
           if (d.children) { d._children = d.children; d.children = null; }
           else { d.children = d._children; d._children = null; }
           update(d);
           highlightPath(d);
        });

    nodeEnter.append('circle').attr('r', 1e-6);
    nodeEnter.append('text')
        .attr('dy', '.35em')
        .attr('x', d => d.children || d._children ? -15 : 15)
        .attr('text-anchor', d => d.children || d._children ? 'end' : 'start')
        .text(d => d.data.name)
        .style('fill', '#fff')
        .style('font-weight', 'bold')
        .style('text-shadow', '2px 2px 4px #000');

    const nodeUpdate = nodeEnter.merge(node);
    nodeUpdate.transition().duration(duration)
        .attr('transform', d => `translate(${d.y},${d.x})`);

    nodeUpdate.select('circle')
        .attr('r', 10)
        .style('fill', d => d._children ? '#ff9100' : '#fff')
        .style('stroke', '#ff9100')
        .style('stroke-width', '3px');

    const nodeExit = node.exit().transition().duration(duration)
        .attr('transform', d => `translate(${source.y},${source.x})`)
        .remove();

    nodeExit.select('circle').attr('r', 1e-6);

    // --- LINKS (CONECTORES) ---
    const link = g.selectAll('path.link')
        .data(links, d => d.id);

    const linkEnter = link.enter().insert('path', 'g')
        .attr('class', 'link')
        .attr('d', d => {
          const o = {x: source.x0, y: source.y0};
          return diagonal(o, o);
        });

    const linkUpdate = linkEnter.merge(link);
    linkUpdate.transition().duration(duration)
        .attr('d', d => diagonal(d, d.parent));

    link.exit().transition().duration(duration)
        .attr('d', d => {
          const o = {x: source.x, y: source.y};
          return diagonal(o, o);
        })
        .remove();

    nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });

    // Función para dibujar curvas suaves
    function diagonal(s, d) {
      return `M ${s.y} ${s.x} C ${(s.y + d.y) / 2} ${s.x}, ${(s.y + d.y) / 2} ${d.x}, ${d.y} ${d.x}`;
    }

    // Función de Aislamiento Visual
    function highlightPath(d) {
      // Apagar todo
      g.selectAll('path.link').style('stroke', '#444').style('opacity', 0.2).style('stroke-width', '2px');
      g.selectAll('g.node').style('opacity', 0.4);

      let curr = d;
      while(curr) {
        // Encender nodo actual
        g.selectAll('g.node').filter(n => n === curr).style('opacity', 1);
        // Encender cable al padre
        if(curr.parent) {
          g.selectAll('path.link').filter(l => l === curr)
             .style('stroke', '#ff9100').style('opacity', 1).style('stroke-width', '4px');
        }
        curr = curr.parent;
      }
    }
  }
});"

# --- 3. UI ---
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),

  tags$head(
    # Importar D3.js v6
    tags$script(src = "https://d3js.org/d3.v6.min.js"),

    tags$style(HTML("
      body { overflow: hidden; }
      #tree-container {
        background: #111;
        border-radius: 15px;
        height: 85vh;
        width: 100%;
        border: 1px solid #333;
        margin-top: 10px;
      }
      .link { fill: none; stroke: #444; stroke-width: 2px; }
      .node text { pointer-events: none; user-select: none; }
      #tree-container:active { cursor: grabbing; }
    ")),

    # Inyectamos los datos del Excel al inicio como variable Global
    tags$script(HTML(sprintf("var treeData = %s;", preparar_jerarquia("arbol_estadistico.xlsx"))))
  ),

  titlePanel("Rscience Interactive Engine v.3.0"),

  fluidRow(
    column(12,
           p("• Haz clic en los nodos para expandir/colapsar.", style="color:#aaa; display:inline; margin-right:20px;"),
           p("• Arrastra el lienzo para mover el árbol.", style="color:#aaa; display:inline; margin-right:20px;"),
           p("• Usa la rueda del ratón para Zoom.", style="color:#aaa; display:inline;")
    )
  ),

  div(id = "tree-container"),

  # Inyectar la lógica del motor al final
  tags$script(HTML(js_logic))
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  # El servidor está vacío porque toda la interactividad
  # ocurre en el lado del cliente (Navegador) para máxima velocidad.
}

shinyApp(ui, server)
