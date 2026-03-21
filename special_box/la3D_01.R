library(shiny)
library(collapsibleTree)
library(readxl)
library(dplyr)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),

  tags$head(tags$style(HTML("
    /* 1. HEADER */
    .path-display-area {
      background: #111827; border: 1px solid #374151; padding: 20px;
      border-radius: 15px; margin-bottom: 20px; min-height: 100px;
    }
    .node-chip {
      background: #ff9100; color: white; padding: 6px 16px;
      border-radius: 8px; font-weight: 800; border: 2px solid #b36600;
    }

    /* 2. NODOS */
    .node-active circle {
      fill: #ff9100 !important; stroke: #fff !important;
      stroke-width: 3px !important; r: 12px !important;
    }
    .node-active text {
      fill: #ff9100 !important; font-weight: 900 !important; font-size: 15px !important;
    }

    /* 3. REGLA MAESTRA DE CONECTORES */
    #tree-output path.link {
      stroke: #000000 !important; stroke-width: 4px !important; opacity: 0.4;
    }

    /* Resaltado de la ruta activa */
    #tree-output path.link.link-active {
      stroke: #ff9100 !important; stroke-width: 6px !important; opacity: 1 !important;
    }

    /* --- LÓGICA DE VISIBILIDAD SOLICITADA --- */

    /* Si 'Aislar' está activo, ocultamos nodos no seleccionados */
    .isolate-mode g.node:not(.node-active) {
      display: none !important;
    }

    /* REGLA DE ORO: Ocultar conectores que toquen nodos ocultos */
    /* Usamos pointer-events para que JS detecte qué está visible realmente */
    .isolate-mode path.link {
      visibility: hidden;
    }

    /* Solo mostramos el conector si sus nodos extremos están en el DOM (vía clase activa) */
    .isolate-mode path.link.link-active {
      visibility: visible !important;
    }

    .map-box { background: white; border-radius: 20px; padding: 15px; }
  "))),

  titlePanel("Rscience: Conexiones Vivas"),

  div(class = "path-display-area", uiOutput("breadcrumb_ui")),

  sidebarLayout(
    sidebarPanel(
      h4("Filtros de Red"),
      checkboxInput("cb_isolate", "AISLAR RUTA (NODOS Y LINKS)", FALSE),
      hr(),
      p("Al aislar, solo verás los nodos de la cadena y los conectores que los unen."),
      width = 3
    ),

    mainPanel(
      div(id = "tree-wrapper", class = "map-box",
          collapsibleTreeOutput("tree_output", height = "750px")
      ),

      tags$script(HTML("
        $(document).on('click', '.node', function() {
          var targetData = d3.select(this).datum();
          if(!targetData) return;

          var ancestorObjects = [];
          var curr = targetData;
          while(curr) {
            ancestorObjects.push(curr);
            curr = curr.parent;
          }

          // Marcar Nodos
          d3.selectAll('g.node').classed('node-active', false);
          d3.selectAll('g.node').filter(function(d) {
            return ancestorObjects.indexOf(d) !== -1;
          }).classed('node-active', true);

          // Marcar Links (Solo si ambos extremos son visibles/activos)
          d3.selectAll('path.link').classed('link-active', false);
          d3.selectAll('path.link').filter(function(d) {
            return ancestorObjects.indexOf(d.source) !== -1 &&
                   ancestorObjects.indexOf(d.target) !== -1;
          }).classed('link-active', true);

          var pathNames = ancestorObjects.map(function(n) { return n.data.name; });
          Shiny.setInputValue('selected_nodes', pathNames.reverse());
        });
      "))
    )
  )
)

server <- function(input, output, session) {

  tree_data <- reactive({
    read_xlsx("arbol_estadistico.xlsx", sheet = 1)
  })

  output$tree_output <- renderCollapsibleTree({
    collapsibleTree(
      tree_data(),
      hierarchy = c("Nivel1", "Nivel2", "Nivel3", "Nivel4", "Nivel5"),
      root = "Rscience",
      zoomable = TRUE,
      collapsed = FALSE
    )
  })

  output$breadcrumb_ui <- renderUI({
    req(input$selected_nodes)
    tagList(lapply(input$selected_nodes, function(step) span(class = "node-chip", step)))
  })

  observe({
    if(input$cb_isolate) addClass("tree-wrapper", "isolate-mode") else removeClass("tree-wrapper", "isolate-mode")
  })
}

shinyApp(ui, server)
