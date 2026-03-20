library(shiny)
library(collapsibleTree)

# 1. Datos base
df <- data.frame(
  Nivel1 = c("A", "A", "B", "B"),
  Nivel2 = c("A1", "A2", "B1", "B21"),
  stringsAsFactors = FALSE
)

# Diccionario de posiciones: 1:Inicio, 2:A, 3:B, 4:A1, 5:A2, 6:B1, 7:B21
nodos_id <- c("INICIO", "A", "B", "A1", "A2", "B1", "B21")

ui <- fluidPage(
  style = "background-color: #f4f7f6; padding: 20px;",
  h2("Rscience: Árbol Reactivo v.0.0.1", style="font-weight:900; color:#2c3e50;"),

  wellPanel(
    style = "background: white; border-radius: 15px; border-left: 6px solid #ff9800;",
    h5("RUTA SELECCIONADA:", style="color:#7f8c8d; font-size: 0.7rem;"),
    span(textOutput("ruta_exacta"), style="font-weight:bold; font-size: 1.2rem; color:#ff9800;")
  ),

  # Solo UN output para evitar conflictos de ID
  div(style = "background: white; border-radius: 20px; padding: 20px; border: 1px solid #ddd;",
      collapsibleTreeOutput("tree_unico", height = "500px")
  )
)

server <- function(input, output, session) {

  # Calculamos el vector de colores basado en el clic
  colores_reactivos <- reactive({
    # Colores base
    cols <- c("black", "gray", "gray", "white", "white", "white", "white")

    # Obtenemos la selección (asegurándonos de que sea texto plano)
    sel <- unlist(input$seleccion_arbol)

    if(!is.null(sel) && length(sel) > 0) {
      nodo_click <- sel[1]
      idx <- which(nodos_id == nodo_click)
      if(length(idx) > 0) {
        cols[idx] <- "#ff9800" # Naranja para el seleccionado
      }
    }
    return(cols)
  })

  # Renderizamos el árbol UNICA VEZ, pero reactivo al color
  output$tree_unico <- renderCollapsibleTree({
    # Forzamos que se actualice cuando cambien los colores
    fill_vec <- colores_reactivos()

    collapsibleTree(
      df,
      hierarchy = c("Nivel1", "Nivel2"),
      root = "INICIO",
      fill = fill_vec,
      collapsed = FALSE, # SIEMPRE ABIERTO
      zoomable = FALSE,
      inputId = "seleccion_arbol"
    )
  })

  output$ruta_exacta <- renderText({
    sel <- unlist(input$seleccion_arbol)
    if(is.null(sel)) return("INICIO")
    paste(rev(c(sel, "INICIO")), collapse = " > ")
  })
}

shinyApp(ui, server)
