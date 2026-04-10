library(shiny)
library(bslib)
library(shinyjs)
library(quarto)

# ==============================================================================
# UI MÓDULO
# ==============================================================================
mod_pipeline_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        .matrix-container { background: #0f171e; border: 1px solid #2a3b47; border-radius: 8px; overflow: hidden; }
        .matrix-header-row {
          background: #1a262f; color: #00d4ff; font-weight: 800;
          text-transform: uppercase; font-size: 0.7rem; letter-spacing: 1.5px;
          border-bottom: 2px solid #00d4ff;
        }
        .matrix-row { border-bottom: 1px solid #1a262f; transition: all 0.5s ease; min-height: 65px; }
        .matrix-cell { padding: 12px 10px; font-family: 'JetBrains Mono', monospace; font-size: 0.8rem; color: #fff; }
        .check-icon { color: #22303a; font-size: 1rem; transition: all 0.4s; }
        .status-on { color: #00bc8c; text-shadow: 0 0 10px #00bc8c; }
        .status-proc { color: #00d4ff; animation: blink 0.8s infinite; }
        .status-error { color: #ff4b5c; text-shadow: 0 0 10px #ff4b5c; }
        @keyframes blink { 0% { opacity: 0.3; } 50% { opacity: 1; } 100% { opacity: 0.3; } }
        .row-active { background: rgba(0, 212, 255, 0.08); }
        .step-id { color: #00d4ff; font-weight: bold; text-align: center; }
        .total-time-card { background: #1a262f; border: 1px solid #00d4ff; border-radius: 10px; padding: 15px; }
        .main-btn { border-radius: 50px; font-weight: 800; padding: 12px 40px; letter-spacing: 2px; }
        .log-txt { font-size: 0.65rem; color: #adb5bd; display: block; margin-top: 4px; opacity: 0.8; }
      ")))
    ),

    div(class = "container-fluid py-4",
        div(class = "row align-items-center",
            div(class = "col-md-6",
                actionButton(ns("run_process"), "INITIALIZE PIPELINE",
                             class = "btn-primary main-btn shadow-lg", icon = icon("bolt")),
                actionButton(ns("reset_process"), "RESET",
                             class = "btn-outline-secondary ms-2", style="border-radius:50px;")
            ),
            div(class = "col-md-6",
                div(class = "total-time-card d-flex justify-content-between align-items-center",
                    span("TOTAL ELAPSED TIME:", style="color: #00d4ff; font-weight: 700; font-size: 0.7rem;"),
                    span(id = ns("total_clock"), "00:00:00.000",
                         style="font-family: 'JetBrains Mono'; font-size: 1.5rem; color: #fff;")
                )
            )
        )
    ),

    div(class = "container-fluid",
        div(class = "matrix-container shadow-lg",
            div(class = "row g-0 matrix-header-row",
                div(class = "col-1 matrix-cell text-center", "Step"),
                div(class = "col-4 matrix-cell", "Process Task"),
                div(class = "col-3 matrix-cell text-center", "Status (R | S | E)"),
                div(class = "col-4 matrix-cell", "Time Registry & Logs")
            ),
            uiOutput(ns("matrix_body"))
        )
    )
  )
}

# ==============================================================================
# SERVER MÓDULO
# ==============================================================================
mod_pipeline_server <- function(id, folder_target, list_settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Normalización de la ruta de entrada (Como lo tenías)
    internal_folder_target <- reactive({
      if(is.function(folder_target)) folder_target() else folder_target
    })

    # --- Definición de Pasos (Tus Pasos sin cambios) ---
    steps_data <- data.frame(
      id = 1:7,
      task = c("Folder Target Verify", "Quarto Path Sub-folder", "QMD Files Local Audit",
               "Temp Sandbox Creation", "File System Sync", "Sandbox Validation", "Cloud Engine Render"),
      detail = c("Item 01: External root check", "Item 02: f02_quarto_proc existence",
                 "Item 03: Constructing & verifying paths", "Item 04: RScience Temp Dir",
                 "Item 05: Cloning binary stream", "Item 06: Vial verify in sandbox", "Item 07: Quarto Render")
    )

    output$matrix_body <- renderUI({
      lapply(1:nrow(steps_data), function(i) {
        div(id = ns(paste0("row_", i)), class = "row g-0 matrix-row align-items-center",
            div(class = "col-1 matrix-cell step-id", sprintf("%02d", steps_data$id[i])),
            div(class = "col-4 matrix-cell",
                span(steps_data$task[i], style="font-weight: 600; display:block;"),
                span(steps_data$detail[i], style="font-size: 0.65rem; color: #566b7a;"),
                span(id = ns(paste0("log_", i)), "", class = "log-txt")
            ),
            div(class = "col-3 matrix-cell",
                div(style="display:flex; justify-content:space-around;",
                    icon("check-circle", id = ns(paste0("r_", i)), class = "check-icon"),
                    icon("play-circle",  id = ns(paste0("s_", i)), class = "check-icon"),
                    icon("stop-circle",  id = ns(paste0("e_", i)), class = "check-icon")
                )
            ),
            div(class = "col-4 matrix-cell", style="font-size: 0.7rem;",
                div(span("START: ", style="color:#566b7a;"), span(id = ns(paste0("t_start_", i)), "--:--:--")),
                div(span("END:   ", style="color:#566b7a;"), span(id = ns(paste0("t_end_", i)), "--:--:--"))
            )
        )
      })
    })

    is_running <- reactiveVal(FALSE)
    start_time <- reactiveVal(NULL)

    observeEvent(input$run_process, {
      req(internal_folder_target())
      if(is_running()) return()
      is_running(TRUE)
      start_time(Sys.time())

      # Funciones auxiliares de UI
      update_step <- function(i, status = "start", log = "") {
        if(status == "start") {
          addClass(paste0("row_", i), "row-active")
          addClass(paste0("r_", i), "status-on")
          addClass(paste0("s_", i), "status-proc")
          html(paste0("t_start_", i), format(Sys.time(), "%H:%M:%OS2"))
          if(log != "") html(paste0("log_", i), log)
        } else if (status == "done") {
          removeClass(paste0("s_", i), "status-proc")
          addClass(paste0("s_", i), "status-on")
          addClass(paste0("e_", i), "status-on")
          html(paste0("t_end_", i), format(Sys.time(), "%H:%M:%OS2"))
          if(log != "") html(paste0("log_", i), log)
          removeClass(paste0("row_", i), "row-active")
        } else {
          addClass(paste0("s_", i), "status-error")
          html(paste0("log_", i), "CRITICAL FAILURE")
        }
      }

      # --- LÓGICA DE PROCESO (Tus pasos reconstruidos) ---

      # Item 01: Folder Target
      update_step(1, log = "Verifying Root...")
      Sys.sleep(0.5)
      path_val <- internal_folder_target()
      if(dir.exists(path_val)) update_step(1, "done", "ROOT FOUND") else { update_step(1, "error"); return() }

      # Item 02: Subcarpeta f02
      update_step(2, log = "Checking f02_quarto_proc...")
      Sys.sleep(0.5)
      path_f02 <- file.path(path_val, "f02_quarto_proc")
      if(dir.exists(path_f02)) update_step(2, "done", "SUBFOLDER OK") else { update_step(2, "error"); return() }

      # Item 03: QMD Files Audit (Fusionando paths)
      update_step(3, log = "Auditing QMD Paths...")
      Sys.sleep(0.5)
      list_qmd <- list(
        "pack01" = "g02_quarto_mod/AAA_01_RUNNER_g02_quarto_mod.qmd",
        "pack02" = "g04_script_external/AAA_01_RUNNER_g04_script_external.qmd"
      )
      # Aquí está tu lógica de fusión: path_f02 + subcarpeta_archivo
      full_paths <- file.path(path_f02, unlist(list_qmd))
      if(all(file.exists(full_paths))) update_step(3, "done", "ALL RUNNERS READY") else { update_step(3, "error"); return() }

      # Item 04: Temp Folder
      update_step(4, log = "Creating Environment...")
      timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
      path_temp <- file.path(tempdir(), paste0("Rscience_", timestamp))
      if(dir.create(path_temp, recursive = TRUE)) update_step(4, "done", basename(path_temp)) else { update_step(4, "error"); return() }

      # Item 05: Clone
      update_step(5, log = "Syncing Files...")
      if(file.copy(path_f02, path_temp, recursive = TRUE)) update_step(5, "done", "SYNC COMPLETE") else { update_step(5, "error"); return() }

      # Item 06: Sandbox Verify
      update_step(6, log = "Validating Sandbox...")
      if(dir.exists(file.path(path_temp, "f02_quarto_proc"))) update_step(6, "done", "Vial check OK") else { update_step(6, "error"); return() }

      # Item 07: Quarto Render
      update_step(7, log = "Executing Engine...")
      tryCatch({
        quarto::quarto_render(full_paths[1], quiet = TRUE) # Ejemplo con el primero
        update_step(7, "done", "PROCESS FINISHED")
      }, error = function(e) update_step(7, "error"))

      is_running(FALSE)
      runjs(sprintf("document.getElementById('%s').style.color = '#00bc8c';", ns("total_clock")))
    })

    # Reloj Global
    observe({
      if (!is_running()) return()
      invalidateLater(60)
      diff <- difftime(Sys.time(), start_time(), units = "secs")
      ms <- sprintf("%03d", round((as.numeric(diff) %% 1) * 1000))
      txt <- paste0("00:", format(as.POSIXct(as.numeric(diff), origin="1970-01-01", tz="UTC"), "%M:%S"), ".", ms)
      html("total_clock", txt)
    })

    observeEvent(input$reset_process, { shinyjs::runjs("history.go(0);") })
  })
}

# ==============================================================================
# APP DE PRUEBA
# ==============================================================================
# CREAR ESTRUCTURA DE PRUEBA
test_path <- file.path(tempdir(), "Rscience_Project")
dir.create(file.path(test_path, "f02_quarto_proc/g02_quarto_mod"), recursive = TRUE)
dir.create(file.path(test_path, "f02_quarto_proc/g04_script_external"), recursive = TRUE)
file.create(file.path(test_path, "f02_quarto_proc/g02_quarto_mod/AAA_01_RUNNER_g02_quarto_mod.qmd"))
file.create(file.path(test_path, "f02_quarto_proc/g04_script_external/AAA_01_RUNNER_g04_script_external.qmd"))

ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  mod_pipeline_ui("pipeline_1")
)

server <- function(input, output, session) {
  mod_pipeline_server("pipeline_1", folder_target = test_path)
}

shinyApp(ui, server)
