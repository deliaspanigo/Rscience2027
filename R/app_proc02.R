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

  tags$head(
    tags$style(HTML("
      .matrix-container { background: #0f171e; border: 1px solid #2a3b47; border-radius: 8px; }
      .matrix-header-row {
        background: #1a262f; color: #00d4ff; font-weight: 800;
        text-transform: uppercase; font-size: 0.7rem; letter-spacing: 1px;
        border-bottom: 2px solid #00d4ff;
      }
      .matrix-row { border-bottom: 1px solid #1a262f; }
      .matrix-cell { padding: 12px 10px; font-family: 'JetBrains Mono', monospace; font-size: 0.8rem; }
      .step-id { color: #00d4ff; font-weight: bold; text-align: center; background: rgba(0,212,255,0.05); }

      /* Estilo para los 3 checks */
      .check-group { display: flex; justify-content: space-around; font-size: 0.9rem; }
      .check-active { color: #00bc8c; }
      .check-inactive { color: #343a40; }

      .time-code { color: #566b7a; }
      .total-time-card { background: #1a262f; border: 1px solid #00d4ff; border-radius: 10px; padding: 15px; }
      .main-btn { border-radius: 50px; font-weight: 800; padding: 12px 40px; letter-spacing: 2px; }
    "))
  ),

  # Cabecera: Botón y Tiempo Total
  div(class = "container py-4",
      div(class = "row align-items-center",
          div(class = "col-md-6",
              actionButton("run_process", "INITIALIZE MASTER PROCESS",
                           class = "btn-primary main-btn shadow-lg",
                           icon = icon("play"))
          ),
          div(class = "col-md-6",
              div(class = "total-time-card d-flex justify-content-between align-items-center",
                  span("TOTAL ELAPSED TIME:", style="color: #00d4ff; font-weight: 700; font-size: 0.8rem;"),
                  span("00:00:00.000", style="font-family: 'JetBrains Mono'; font-size: 1.5rem; color: #fff;")
              )
          )
      )
  ),

  # Matriz de Procesamiento
  div(class = "container-fluid",
      div(class = "matrix-container shadow-lg",
          # Header
          div(class = "row g-0 matrix-header-row",
              div(class = "col-1 matrix-cell text-center", "Step"),
              div(class = "col-2 matrix-cell", "Task"),
              div(class = "col-3 matrix-cell", "Technical Details"),
              div(class = "col-2 matrix-cell text-center", "Status (R | S | E)"),
              div(class = "col-2 matrix-cell", "Start Time"),
              div(class = "col-2 matrix-cell", "End Time")
          ),
          # Filas generadas por el server
          uiOutput("matrix_body")
      )
  )
)

# SERVER
server <- function(input, output, session) {

  steps <- list(
    list(id = "01", task = "Dar play", detail = "Core Signal Activation", r=T, s=T, e=T),
    list(id = "02", task = "Tomar fecha", detail = "ISO-8601 Timestamp Sync", r=T, s=T, e=F),
    list(id = "03", task = "Conformar carpeta", detail = "Recursive Directory Creation", r=T, s=F, e=F),
    list(id = "04", task = "Copiar carpeta", detail = "Binary Stream Transfer", r=F, s=F, e=F),
    list(id = "05", task = "Ejecutar 01", detail = "Primary Script Execution", r=F, s=F, e=F),
    list(id = "06", task = "Ejecutar 02", detail = "Final Validation Loop", r=F, s=F, e=F)
  )

  output$matrix_body <- renderUI({
    lapply(steps, function(s) {
      div(class = "row g-0 matrix-row align-items-center",
          div(class = "col-1 matrix-cell step-id", s$id),
          div(class = "col-2 matrix-cell", span(s$task, style="font-weight: 600;")),
          div(class = "col-3 matrix-cell text-muted", s$detail),

          # Triple Check Column
          div(class = "col-2 matrix-cell",
              div(class = "check-group",
                  icon("check-circle", class = if(s$r) "check-active" else "check-inactive"),
                  icon("play-circle",  class = if(s$s) "check-active" else "check-inactive"),
                  icon("stop-circle",  class = if(s$e) "check-active" else "check-inactive")
              )
          ),

          div(class = "col-2 matrix-cell time-code", if(s$s) "14:20:05.122" else "--:--:--.---"),
          div(class = "col-2 matrix-cell time-code", if(s$e) "14:20:08.450" else "--:--:--.---")
      )
    })
  })
}

shinyApp(ui, server)
