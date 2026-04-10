library(shiny)
library(bslib)
library(shinyWidgets)

# ==============================================================================
# UI
# ==============================================================================
ui <- page_fluid(
  theme = bs_theme(
    version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff",
    base_font = font_google("Inter")
  ),

  tags$head(
    tags$style(HTML("
      .matrix-container { background: #0f171e; border: 1px solid #2a3b47; border-radius: 8px; }
      .matrix-header-row {
        background: #1a262f; color: #00d4ff; font-weight: 800;
        text-transform: uppercase; font-size: 0.7rem; letter-spacing: 1.5px;
        border-bottom: 2px solid #00d4ff;
      }
      .matrix-row { border-bottom: 1px solid #1a262f; transition: background 0.4s; }
      .row-active { background: rgba(0, 212, 255, 0.08); }
      .matrix-cell { padding: 12px 10px; font-family: 'JetBrains Mono', monospace; font-size: 0.8rem; }
      .step-id { color: #00d4ff; font-weight: bold; text-align: center; }

      .check-group { display: flex; justify-content: space-around; font-size: 0.95rem; }
      .check-active { color: #00bc8c; text-shadow: 0 0 5px #00bc8c; }
      .check-process { color: #00d4ff; animation: blink 0.8s infinite; }
      .check-inactive { color: #22303a; }

      @keyframes blink { 0% { opacity: 0.4; } 50% { opacity: 1; } 100% { opacity: 0.4; } }

      .total-time-card { background: #1a262f; border: 1px solid #00d4ff; border-radius: 10px; padding: 15px; }
      .time-code { color: #adb5bd; }
      .main-btn { border-radius: 50px; font-weight: 800; padding: 12px 40px; letter-spacing: 2px; transition: 0.3s; }
    "))
  ),

  div(class = "container py-4",
      div(class = "row align-items-center",
          div(class = "col-md-6",
              uiOutput("ui_button") # Botón dinámico para reset/start
          ),
          div(class = "col-md-6",
              div(class = "total-time-card d-flex justify-content-between align-items-center",
                  span("TOTAL ELAPSED TIME:", style="color: #00d4ff; font-weight: 700; font-size: 0.8rem;"),
                  uiOutput("total_timer_display")
              )
          )
      )
  ),

  div(class = "container-fluid",
      div(class = "matrix-container shadow-lg",
          div(class = "row g-0 matrix-header-row",
              div(class = "col-1 matrix-cell text-center", "Step"),
              div(class = "col-2 matrix-cell", "Task"),
              div(class = "col-3 matrix-cell", "Technical Details"),
              div(class = "col-2 matrix-cell text-center", "Status (R | S | E)"),
              div(class = "col-2 matrix-cell", "Start Time"),
              div(class = "col-2 matrix-cell", "End Time")
          ),
          uiOutput("matrix_body")
      )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # 1. ESTADOS REACTIVOS
  is_running <- reactiveVal(FALSE)
  is_finished <- reactiveVal(FALSE)
  start_global_time <- reactiveVal(NULL)
  end_global_time <- reactiveVal(NULL)
  current_step <- reactiveVal(0)

  steps_df <- reactiveValues(data = data.frame(
    id = c("01", "02", "03", "04", "05", "06"),
    task = c("Dar play", "Tomar fecha", "Conformar carpeta", "Copiar carpeta", "Ejecutar 01", "Ejecutar 02"),
    detail = c("Signal Activation", "ISO Timestamp Sync", "FS: Directory Creation", "Binary Stream Transfer", "Primary Logic Script", "Validation Loop"),
    r = rep(TRUE, 6), s = rep(FALSE, 6), e = rep(FALSE, 6),
    t_start = rep(NA_character_, 6), t_end = rep(NA_character_, 6),
    stringsAsFactors = FALSE
  ))

  # 2. TIMER DINÁMICO (Solo consume recursos si is_running es TRUE)
  observe({
    if (!is_running()) return()

    # Cada 100ms actualizamos la lógica
    invalidateLater(100, session)

    step_idx <- current_step()

    if (step_idx == 0) {
      current_step(1)
      steps_df$data$s[1] <- TRUE
      steps_df$data$t_start[1] <- format(Sys.time(), "%H:%M:%OS3")
    } else if (step_idx <= nrow(steps_df$data)) {

      # Simulación de carga: probabilidad de finalizar el paso actual
      if (runif(1) > 0.90) {
        steps_df$data$e[step_idx] <- TRUE
        steps_df$data$t_end[step_idx] <- format(Sys.time(), "%H:%M:%OS3")

        if (step_idx < nrow(steps_df$data)) {
          new_idx <- step_idx + 1
          current_step(new_idx)
          steps_df$data$s[new_idx] <- TRUE
          steps_df$data$t_start[new_idx] <- format(Sys.time(), "%H:%M:%OS3")
        } else {
          # FINALIZACIÓN TOTAL
          is_running(FALSE)
          is_finished(TRUE)
          end_global_time(Sys.time())
        }
      }
    }
  })

  # 3. CONTROL DE BOTÓN
  output$ui_button <- renderUI({
    if(is_finished()) {
      actionButton("reset_process", "RESET PIPELINE", class = "btn-secondary main-btn", icon = icon("sync"))
    } else {
      actionButton("run_process", "START SIMULATION",
                   class = paste("btn-primary main-btn", if(is_running()) "disabled"),
                   icon = icon("bolt"))
    }
  })

  observeEvent(input$run_process, {
    if(is_running()) return()
    start_global_time(Sys.time())
    end_global_time(NULL)
    is_running(TRUE)
    is_finished(FALSE)
  })

  observeEvent(input$reset_process, {
    steps_df$data$s <- FALSE
    steps_df$data$e <- FALSE
    steps_df$data$t_start <- NA
    steps_df$data$t_end <- NA
    current_step(0)
    is_finished(FALSE)
    start_global_time(NULL)
  })

  # 4. TIMER DISPLAY (Se congela al terminar)
  output$total_timer_display <- renderUI({
    # Si está corriendo, se invalida cada 100ms
    if(is_running()) invalidateLater(100, session)

    txt <- "00:00:00.000"
    ref_time <- if(is_finished()) end_global_time() else Sys.time()

    if (!is.null(start_global_time())) {
      diff <- difftime(ref_time, start_global_time(), units = "secs")
      base_t <- format(as.POSIXct(as.numeric(diff), origin = "1970-01-01", tz = "UTC"), "%M:%S")
      ms <- sprintf("%03d", round((as.numeric(diff) %% 1) * 1000))
      txt <- paste0("00:", base_t, ".", ms)
    }
    span(txt, style=paste("font-family: 'JetBrains Mono'; font-size: 1.5rem;",
                          if(is_finished()) "color: #00bc8c;" else "color: #fff;"))
  })

  # 5. RENDER MATRIX
  output$matrix_body <- renderUI({
    df <- steps_df$data
    lapply(1:nrow(df), function(i) {
      is_active <- current_step() == i && is_running()

      div(class = paste("row g-0 matrix-row align-items-center", if(is_active) "row-active"),
          div(class = "col-1 matrix-cell step-id", df$id[i]),
          div(class = "col-2 matrix-cell", span(df$task[i], style="font-weight: 600;")),
          div(class = "col-3 matrix-cell text-muted", df$detail[i]),

          div(class = "col-2 matrix-cell",
              div(class = "check-group",
                  icon("check-circle", class = if(df$r[i]) "check-active" else "check-inactive"),
                  icon("play-circle",  class = if(df$e[i]) "check-active" else if(df$s[i]) "check-process" else "check-inactive"),
                  icon("stop-circle",  class = if(df$e[i]) "check-active" else "check-inactive")
              )
          ),

          div(class = "col-2 matrix-cell time-code", if(!is.na(df$t_start[i])) df$t_start[i] else "--:--:--.---"),
          div(class = "col-2 matrix-cell time-code", if(!is.na(df$t_end[i])) df$t_end[i] else "--:--:--.---")
      )
    })
  })
}

shinyApp(ui, server)
