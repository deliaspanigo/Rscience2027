library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body { background-color: #0b0f19; color: white; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }

      .engine-container {
        display: flex;
        justify-content: center;
        gap: 30px;
        margin-top: 100px;
      }

      /* Estilo de la Tarjeta */
      .engine-card {
        width: 200px;
        height: 250px;
        background: #161b22;
        border: 2px solid #30363d;
        border-radius: 20px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: all 0.5s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }

      .engine-card i { font-size: 50px; margin-bottom: 20px; transition: 0.5s; }
      .engine-card span { font-weight: bold; text-transform: uppercase; letter-spacing: 2px; font-size: 0.8rem; }

      /* Colores por ID */
      #data_eng { color: #00d4ff; }
      #stat_eng { color: #ccff00; }
      #plot_eng { color: #ff007a; }

      /* Efecto Hover */
      .engine-card:hover {
        transform: translateY(-10px);
        border-color: currentColor;
        box-shadow: 0 10px 30px -10px currentColor;
      }

      /* ESTADO SELECCIONADO */
      .selected {
        background: currentColor !important;
        color: #0b0f19 !important;
        border-color: white !important;
        transform: scale(1.1);
        z-index: 10;
      }

      .selected i, .selected span { color: #0b0f19 !important; }

      /* ESTADO DESACTIVADO (Los otros) */
      .disabled-card {
        opacity: 0.2;
        filter: grayscale(1);
        pointer-events: none;
        transform: scale(0.9);
      }

      .reset-btn {
        margin-top: 50px;
        background: transparent;
        border: 1px solid #30363d;
        color: #8b949e;
        padding: 10px 20px;
        border-radius: 30px;
        transition: 0.3s;
      }
      .reset-btn:hover { border-color: white; color: white; }
    "))
  ),

  div(class = "text-center",
      h2("SISTEMA DE CONTROL", style = "letter-spacing: 5px; margin-top: 50px;"),
      p("Seleccione un motor para inicializar el proceso", style = "color: #8b949e;")
  ),

  div(class = "engine-container",
      # Tarjeta 1
      div(id = "data_eng", class = "engine-card",
          icon("database"), span("Data Engine")),
      # Tarjeta 2
      div(id = "stat_eng", class = "engine-card",
          icon("microchip"), span("Stat Engine")),
      # Tarjeta 3
      div(id = "plot_eng", class = "engine-card",
          icon("chart-line"), span("Plot Engine"))
  ),

  div(class = "text-center",
      actionButton("reset", "REINICIAR SISTEMA", class = "reset-btn")
  ),

  # Output oculto para tracking
  textOutput("selected_val", inline = TRUE)
)

server <- function(input, output, session) {

  # Reactive para guardar la selección
  choice <- reactiveVal(NULL)

  # IDs de nuestros elementos
  ids <- c("data_eng", "stat_eng", "plot_eng")

  # Crear los observadores para cada tarjeta (simulando clics)
  lapply(ids, function(id) {
    onclick(id, {
      choice(id)

      # Lógica visual:
      # 1. Al elegido le ponemos 'selected'
      addClass(id, "selected")
      removeClass(id, "disabled-card")

      # 2. A los otros los desactivamos
      others <- ids[ids != id]
      for(other in others) {
        addClass(other, "disabled-card")
        removeClass(other, "selected")
      }
    })
  })

  # Lógica de Reset
  observeEvent(input$reset, {
    choice(NULL)
    for(id in ids) {
      removeClass(id, "selected")
      removeClass(id, "disabled-card")
    }
  })

  output$selected_val <- renderText({
    if(is.null(choice())) "Esperando selección..."
    else paste("Motor activo:", choice())
  })
}

shinyApp(ui, server)
