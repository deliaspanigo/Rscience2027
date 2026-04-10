library(shiny)
library(bslib)
library(shinyjs)

ui <- page_fluid(
  useShinyjs(),
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

  tags$head(
    tags$style(HTML("
      /* Estética RScience */
      .matrix-container { background: #0f171e; border: 1px solid #2a3b47; border-radius: 8px; overflow: hidden; }
      .matrix-header-row {
        background: #1a262f; color: #00d4ff; font-weight: 800;
        text-transform: uppercase; font-size: 0.7rem; letter-spacing: 1.5px;
        border-bottom: 2px solid #00d4ff;
      }
      .matrix-row { border-bottom: 1px solid #1a262f; transition: all 0.5s ease; height: 55px; }
      .matrix-cell { padding: 12px 10px; font-family: 'JetBrains Mono', monospace; font-size: 0.8rem; }

      /* Control de Luces */
      .check-icon { color: #22303a; font-size: 1rem; transition: all 0.4s; }
      .status-on { color: #00bc8c; text-shadow: 0 0 10px #00bc8c; }
      .status-proc { color: #00d4ff; animation: blink 0.8s infinite; }
      @keyframes blink { 0% { opacity: 0.3; } 50% { opacity: 1; } 100% { opacity: 0.3; } }

      .row-active { background: rgba(0, 212, 255, 0.08); }
      .step-id { color: #00d4ff; font-weight: bold; text-align: center; }
      .time-code { color: #566b7a; }
      .time-active { color: #adb5bd; }

      .total-time-card { background: #1a262f; border: 1px solid #00d4ff; border-radius: 10px; padding: 15px; }
      .main-btn { border-radius: 50px; font-weight: 800; padding: 12px 40px; letter-spacing: 2px; }
    "))
  ),

  div(class = "container py-4",
      div(class = "row align-items-center",
          div(class = "col-md-6",
              actionButton("run_process", "INITIALIZE PIPELINE", class = "btn-primary main-btn shadow-lg", icon = icon("bolt")),
              actionButton("reset_process", "RESET", class = "btn-outline-secondary ms-2", style="border-radius:50px;")
          ),
          div(class = "col-md-6",
              div(class = "total-time-card d-flex justify-content-between align-items-center",
                  span("TOTAL ELAPSED TIME:", style="color: #00d4ff; font-weight: 700; font-size: 0.7rem;"),
                  span(id = "total_clock", "00:00:00.000", style="font-family: 'JetBrains Mono'; font-size: 1.5rem; color: #fff;")
              )
          )
      )
  ),

  div(class = "container-fluid",
      div(class = "matrix-container shadow-lg",
          # Header con las columnas originales
          div(class = "row g-0 matrix-header-row",
              div(class = "col-1 matrix-cell text-center", "Step"),
              div(class = "col-2 matrix-cell", "Task"),
              div(class = "col-3 matrix-cell", "Technical Details"),
              div(class = "col-2 matrix-cell text-center", "Status (R | S | E)"),
              div(class = "col-2 matrix-cell", "Start Time"),
              div(class = "col-2 matrix-cell", "End Time")
          ),
          # Cuerpo estático
          uiOutput("matrix_static_body")
      )
  )
)

server <- function(input, output, session) {

  steps_data <- data.frame(
    id = c("01", "02", "03", "04", "05", "06"),
    task = c("Dar play", "Tomar fecha", "Conformar carpeta", "Copiar carpeta", "Ejecutar 01", "Ejecutar 02"),
    detail = c("Signal Activation", "ISO Timestamp Sync", "FS: Directory Creation", "Binary Stream Transfer", "Primary Logic Script", "Validation Loop")
  )

  # Renderizamos la estructura una sola vez para evitar el flash
  output$matrix_static_body <- renderUI({
    lapply(1:nrow(steps_data), function(i) {
      div(id = paste0("row_", i), class = "row g-0 matrix-row align-items-center",
          div(class = "col-1 matrix-cell step-id", steps_data$id[i]),
          div(class = "col-2 matrix-cell", span(steps_data$task[i], style="font-weight: 600;")),
          div(class = "col-3 matrix-cell text-muted", steps_data$detail[i]),

          # Triple Check Column
          div(class = "col-2 matrix-cell",
              div(style="display:flex; justify-content:space-around;",
                  icon("check-circle", id = paste0("r_", i), class = "check-icon"),
                  icon("play-circle",  id = paste0("s_", i), class = "check-icon"),
                  icon("stop-circle",  id = paste0("e_", i), class = "check-icon")
              )
          ),

          div(class = "col-2 matrix-cell time-code", id = paste0("t_start_", i), "--:--:--.---"),
          div(class = "col-2 matrix-cell time-code", id = paste0("t_end_", i), "--:--:--.---")
      )
    })
  })

  is_running <- reactiveVal(FALSE)
  start_time <- reactiveVal(NULL)

  observeEvent(input$run_process, {
    if(is_running()) return()
    is_running(TRUE)
    start_time(Sys.time())

    # Simulación de la Matrix
    for(i in 1:6) {
      local({
        idx <- i
        # INICIO DEL PASO
        delay(idx * 900, {
          addClass(paste0("row_", idx), "row-active")
          addClass(paste0("r_", idx), "status-on")
          addClass(paste0("s_", idx), "status-proc")
          html(paste0("t_start_", idx), format(Sys.time(), "%H:%M:%OS3"))
          addClass(paste0("t_start_", idx), "time-active")
        })
        # FIN DEL PASO
        delay(idx * 900 + 800, {
          removeClass(paste0("s_", idx), "status-proc")
          addClass(paste0("s_", idx), "status-on")
          addClass(paste0("e_", idx), "status-on")
          html(paste0("t_end_", idx), format(Sys.time(), "%H:%M:%OS3"))
          addClass(paste0("t_end_", idx), "time-active")
          removeClass(paste0("row_", idx), "row-active")
          if(idx == 6) {
            is_running(FALSE)
            runjs("document.getElementById('total_clock').style.color = '#00bc8c';")
          }
        })
      })
    }
  })

  # Reloj global independiente para no estresar el DOM
  observe({
    if (!is_running()) return()
    invalidateLater(60)
    diff <- difftime(Sys.time(), start_time(), units = "secs")
    ms <- sprintf("%03d", round((as.numeric(diff) %% 1) * 1000))
    txt <- paste0("00:", format(as.POSIXct(as.numeric(diff), origin="1970-01-01", tz="UTC"), "%M:%S"), ".", ms)
    html("total_clock", txt)
  })

  observeEvent(input$reset_process, {
    runjs("location.reload();")
  })
}

shinyApp(ui, server)
