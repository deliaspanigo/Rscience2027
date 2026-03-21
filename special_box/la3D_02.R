library(shiny)
library(collapsibleTree)
library(readxl)
library(dplyr)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),

  tags$head(tags$style(HTML("
    .path-display-area { background: #111827; padding: 20px; border-radius: 15px; margin-bottom: 20px; border: 1px solid #374151; }
    .node-chip { background: #ff9100; color: white; padding: 5px 12px; border-radius: 6px; font-weight: bold; margin-right: 5px; }

    /* ESTILOS MAPA */
    #tree-output path.link { stroke: #4a5568 !important; stroke-width: 2px !important; opacity: 0.2; }
    .node-active circle { fill: #ff9100 !important; stroke: #fff !important; stroke-width: 3px !important; r: 10px !important; }
    .link-active-idx { stroke: #ff9100 !important; stroke-width: 8px !important; opacity: 1 !important; }

    /* MODO AISLAR */
    .isolate-mode g.node:not(.node-active) { display: none !important; }
    .isolate-mode path.link:not(.link-active-idx) { display: none !important; }

    .map-box { background: white; border-radius: 15px; padding: 15px; }
    pre { background: #1a202c; color: #48bb78; padding: 10px; border-radius: 8px; }
  "))),

  titlePanel("Rscience: Index-Match Engine v.5.0"),

  div(class = "path-display-area", uiOutput("breadcrumb_ui")),

  sidebarLayout(
    sidebarPanel(
      h4("Control Maestro"),
      checkboxInput("cb_isolate", "AISLAR POR ÍNDICE", FALSE),
      hr(),
      helpText("Haz clic en un nodo. Si el índice coincide, la Tab 4 mostrará la conexión."),
      width = 3
    ),

    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("1. Mapa",
                 div(id = "tree-wrapper", class = "map-box",
                     collapsibleTreeOutput("tree_output", height = "700px")
                 )
        ),
        tabPanel("2. Índices de Nodos Activos",
                 verbatimTextOutput("debug_nodes_idx")
        ),
        tabPanel("3. Todos los Conectores (Source IDs)",
                 verbatimTextOutput("debug_links_raw")
        ),
        tabPanel("4. Conectores que coinciden",
                 verbatimTextOutput("debug_links_match")
        )
      ),

      # JAVASCRIPT: BUSQUEDA POR REFERENCIA DE ÍNDICE
      tags$script(HTML("
        $(document).on('click', '.node', function() {
          var d = d3.select(this).datum();
          if(!d) return;

          // 1. Identificar ancestros y sus IDs únicos de objeto
          var activeObjects = [];
          var curr = d;
          while(curr) {
            activeObjects.push(curr);
            curr = curr.parent;
          }

          // 2. Pintar Nodos
          d3.selectAll('g.node').classed('node-active', false);
          d3.selectAll('g.node').filter(function(n) {
            return activeObjects.indexOf(n) !== -1;
          }).classed('node-active', true);

          // 3. RELACIÓN DE CONECTORES
          var allLinks = [];
          var matchedLinks = [];

          d3.selectAll('path.link').classed('link-active-idx', false);

          d3.selectAll('path.link').each(function(l) {
            // Intentamos capturar cualquier forma de identificación que tenga el link
            var sID = l.source ? (l.source.data ? l.source.data.name : 'ROOT') : 'UNKNOWN';
            var tID = l.target ? (l.target.data ? l.target.data.name : 'UNKNOWN') : 'UNKNOWN';

            allLinks.push('De: ' + sID + ' -> A: ' + tID);

            // La prueba de fuego: ¿El objeto source y el objeto target están en mi lista?
            if(activeObjects.indexOf(l.source) !== -1 && activeObjects.indexOf(l.target) !== -1) {
              d3.select(this).classed('link-active-idx', true);
              matchedLinks.push('MATCH: ' + sID + ' -> ' + tID);
            }
          });

          // Enviar a Shiny
          Shiny.setInputValue('js_node_names', activeObjects.map(function(n){ return n.data.name; }).reverse());
          Shiny.setInputValue('js_all_links', allLinks);
          Shiny.setInputValue('js_match_links', matchedLinks);
        });
      "))
    )
  )
)

server <- function(input, output, session) {

  tree_data <- reactive({ read_xlsx("arbol_estadistico.xlsx", sheet = 1) })

  output$tree_output <- renderCollapsibleTree({
    collapsibleTree(tree_data(), hierarchy = c("Nivel1", "Nivel2", "Nivel3", "Nivel4", "Nivel5"),
                    root = "Rscience", zoomable = TRUE, collapsed = FALSE)
  })

  output$breadcrumb_ui <- renderUI({
    req(input$js_node_names)
    tagList(lapply(input$js_node_names, function(n) span(class = "node-chip", n)))
  })

  # Debug Outputs
  output$debug_nodes_idx <- renderPrint({ req(input$js_node_names); input$js_node_names })
  output$debug_links_raw <- renderPrint({ req(input$js_all_links); input$js_all_links })
  output$debug_links_match <- renderPrint({
    if(is.null(input$js_match_links) || length(input$js_match_links) == 0) return("Sin coincidencias")
    input$js_match_links
  })

  observe({
    if(input$cb_isolate) addClass("tree-wrapper", "isolate-mode") else removeClass("tree-wrapper", "isolate-mode")
  })
}

shinyApp(ui, server)
