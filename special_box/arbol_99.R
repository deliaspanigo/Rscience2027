library(shiny)
library(collapsibleTree)

df <- data.frame(
  Nivel1 = c("A", "A", "B", "B"),
  Nivel2 = c("A1", "A2", "B1", "B21"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  style = "background-color: #f4f7f6; padding: 20px;",

  # 1. INYECCIÓN DE CSS DE ALTA PRIORIDAD
  tags$head(
    tags$style(HTML("
      /* Esta regla aplasta cualquier color que el Tree intente poner */
      .nodo-activo circle {
        fill: #ff9800 !important;
        stroke: #e65100 !important;
        stroke-width: 4px !important;
        r: 8px !important; /* Opcional: aumenta un poco el tamaño */
      }
    "))
  ),

  h2("Rscience: Absolute Priority v.0.0.1", style="font-weight:900; color:#2c3e50;"),

  # 2. JS PARA GESTIONAR LA CLAVE (LA CLASE)
  tags$script(HTML("
    var lastClickedName = null;

    function refreshActiveNode() {
      if (!lastClickedName) return;

      // Quitamos la clase de todos los nodos
      d3.selectAll('g.node').classed('nodo-activo', false);

      // Se la ponemos solo al que coincide con el nombre guardado
      d3.selectAll('g.node').filter(function(d) {
        return d.data.name == lastClickedName;
      }).classed('nodo-activo', true);
    }

    $(document).on('shiny:visualchange', function(event) {
      if (event.target.id === 'tree_estatico') {

        // Al hacer clic, guardamos el nombre y refrescamos
        d3.select('#tree_estatico').selectAll('circle.node').on('click.custom', function(d) {
          lastClickedName = d.data.name;
          refreshActiveNode();
        });

        // El vigilante asegura que si el árbol se redibuja, la clase se vuelva a poner
        var observer = new MutationObserver(function(mutations) {
          window.requestAnimationFrame(refreshActiveNode);
        });
        observer.observe(document.getElementById('tree_estatico'), { childList: true, subtree: true });
      }
    });
  ")),

  wellPanel(
    style = "background: white; border-radius: 15px; border-left: 6px solid #ff9800;",
    h5("NODO SELECCIONADO:", style="color:#7f8c8d; font-size: 0.7rem;"),
    span(textOutput("ruta_exacta"), style="font-weight:bold; font-size: 1.4rem; color:#ff9800;")
  ),

  div(style = "background: white; border-radius: 20px; padding: 10px; border: 1px solid #ddd;",
      collapsibleTreeOutput("tree_estatico", height = "500px")
  )
)

server <- function(input, output, session) {

  colores_base <- unname(as.character(c("black", "gray", "gray", "white", "white", "white", "white")))

  output$tree_estatico <- renderCollapsibleTree({
    collapsibleTree(
      df, hierarchy = c("Nivel1", "Nivel2"), root = "INICIO",
      fill = colores_base,
      collapsed = FALSE, zoomable = FALSE, inputId = "seleccion_arbol"
    )
  })

  output$ruta_exacta <- renderText({
    sel <- unlist(input$seleccion_arbol)
    if(is.null(sel)) return("Selecciona un nodo...")
    paste(rev(c(sel, "INICIO")), collapse = " > ")
  })
}

shinyApp(ui, server)
