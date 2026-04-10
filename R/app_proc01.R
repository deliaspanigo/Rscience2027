library(shiny)
library(bslib)
library(shinyWidgets)

# UI
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bg = "#0b1218",
    fg = "#ffffff",
    primary = "#00d4ff",
    base_font = font_google("Inter")
  ),

  # Estilos CSS para el look "Control Room"
  tags$head(
    tags$style(HTML("
      .matrix-container {
        background: #0f171e;
        border: 1px solid #2a3b47;
        border-radius: 8px;
        margin-top: 20px;
      }
      .matrix-header-row {
        background: #1a262f;
        color: #00d4ff;
        font-weight: 800;
        text-transform: uppercase;
        font-size: 0.7rem;
        letter-spacing: 1.5px;
        border-bottom: 2px solid #00d4ff;
      }
      .matrix-row {
        border-bottom: 1px solid #1a262f;
        transition: background 0.2s;
      }
      .matrix-row:hover { background: #152029; }
      .matrix-cell {
        padding: 12px 15px;
        font-family: 'JetBrains Mono', monospace;
        font-size: 0.85rem;
      }
      .step-id { color: #00d4ff; font-weight: bold; border-right: 1px solid #1a262f; text-align: center; }
      .ready-col { text-align: center; }
      .time-col { color: #566b7a; font-size: 0.8rem; }
      .btn-master {
        border-radius: 50px;
        font-weight: 800;
        padding: 12px 40px;
        text-transform: uppercase;
        letter-spacing: 2px;
        box-shadow: 0 4px 15px rgba(0, 212, 255, 0.3);
      }
    "))
  ),

  # 1. Botón Superior de Acción
  div(class = "d-flex justify-content-center py-5",
      actionButton("run_process", "Initialize Pipeline",
                   class = "btn-primary btn-master",
                   icon = icon("play-circle"))
  ),

  # 2. Matriz de Procesamiento
  div(class = "container-fluid",
      div(class = "matrix-container shadow-lg",
          # Header
          div(class = "row g-0 matrix-header-row",
              div(class = "col-1 matrix-cell text-center", "Step"),
              div(class = "col-3 matrix-cell", "Task Description"),
              div(class = "col-3 matrix-cell", "Technical Details"),
              div(class = "col-1 matrix-cell text-center", "Ready"),
              div(class = "col-2 matrix-cell", "Start Log"),
              div(class = "col-2 matrix-cell", "End Log")
          ),
          # Cuerpo de la matriz
          uiOutput("matrix_body")
      )
  )
)

# SERVER
server <- function(input, output, session) {

  # Datos de los pasos
  steps <- list(
    list(id = "01", task = "Dar play", detail = "Core signal activation", ready = TRUE),
    list(id = "02", task = "Tomar fecha", detail = "ISO-8601 Timestamp Sync", ready = TRUE),
    list(id = "03", task = "Conformar carpeta", detail = "FS: Recursive Directory Creation", ready = FALSE),
    list(id = "04", task = "Copiar carpeta", detail = "Binary Stream Transfer", ready = FALSE),
    list(id = "05", task = "Ejecutar 01", detail = "Primary Script Execution", ready = FALSE),
    list(id = "06", task = "Ejecutar 02", detail = "Final Validation Loop", ready = FALSE)
  )

  output$matrix_body <- renderUI({
    lapply(steps, function(s) {
      div(class = "row g-0 matrix-row align-items-center",
          div(class = "col-1 matrix-cell step-id", s$id),
          div(class = "col-3 matrix-cell", span(s$task, style="color: #e9ecef; font-weight: 600;")),
          div(class = "col-3 matrix-cell", span(s$detail, style="color: #888;")),
          div(class = "col-1 matrix-cell ready-col",
              if(s$ready) icon("check", style="color: #00bc8c;")
              else icon("clock", style="color: #f39c12; opacity: 0.5;")),
          div(class = "col-2 matrix-cell time-col", "00:00:00.000"),
          div(class = "col-2 matrix-cell time-col", "00:00:00.000")
      )
    })
  })
}

shinyApp(ui, server)
