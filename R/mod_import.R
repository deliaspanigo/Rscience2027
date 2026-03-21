# ==============================================================================
# IMPORT MODULE - v.0.0.8 (FINAL: EDIT & RESET SYNC + CLEAN DEBUG)
# ==============================================================================
library("shinyjs")
library("DT")
library("vroom")
library("readxl")
library("bslib")

mod_import_ui <- function(id) {
  ns <- NS(id)
  root_sel <- paste0(".", ns("import-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        /* --- BLOQUEO DE COLUMNA CON OVERLAY --- */
        ", root_sel, " .lock-wrapper { position: relative; transition: all 0.3s ease; }

        ", root_sel, " .locked-disabled::after {
            content: '';
            position: absolute;
            top: -0px; left: -5px; right: -15px; bottom: -15px;
            background: rgba(230, 230, 230, 0.5);
            backdrop-filter: blur(2px);
            z-index: 100;
            border-radius: 20px;
            border: 2px solid rgba(0,0,0,0.05);
            cursor: not-allowed !important;
        }

        ", root_sel, " .locked-disabled {
            pointer-events: none !important;
            user-select: none;
            filter: grayscale(0.5);
        }

        /* --- SELECTION HEADER (MARQUESINA SUPERIOR) --- */
        ", root_sel, " .selection-header {
            display: flex; justify-content: space-between; align-items: center;
            padding: 15px 25px; border-radius: 12px; background: #ffffff;
            border: 1px solid #e0e0e0; transition: all 0.3s ease;
            box-shadow: 0 2px 5px rgba(0,0,0,0.02);
        }

        ", root_sel, " .selection-header.active-selection {
            background: #f0faff; border: 1px solid #00d4ff; color: #007bff;
        }

        ", root_sel, " .selection-header.confirmed {
            background: #f6fff8; border: 1px solid #28a745; color: #1e7e34;
        }

        ", root_sel, " .header-id {
            font-family: 'Monaco', 'Courier New', monospace; font-weight: 700;
            font-size: 0.85rem; background: rgba(0,0,0,0.05); padding: 4px 12px; border-radius: 20px;
        }

        /* --- BOTONES PILDORA XL --- */
        ", root_sel, " .btn.btn-pill-xl {
            border-radius: 50px !important; padding: 15px 35px !important;
            font-weight: 800 !important; font-size: 1.1rem !important;
            text-transform: uppercase !important; letter-spacing: 1px !important;
            display: inline-flex !important; align-items: center !important;
            justify-content: center !important; gap: 10px !important;
            transition: all 0.3s ease !important;
        }

        ", root_sel, " .btn.btn-pill-xl:hover { transform: translateY(-2px) !important; filter: brightness(1.1); }

        ", root_sel, " .action-row-right {
            display: flex; flex-direction: row; justify-content: flex-end;
            align-items: center; gap: 12px; height: 100%;
        }

        /* --- LABELS Y ESTADOS --- */
        ", root_sel, " .section-label {
            font-weight: 800; font-size: 1.1rem !important;
            color: #1a1a1a !important; text-transform: uppercase;
            margin-bottom: 10px; letter-spacing: 1px;
        }

        ", root_sel, " .status-closed { color: #28a745 !important; font-weight: 900 !important; margin-left: 8px; }

        ", root_sel, " .file-info-banner {
            background: rgba(40, 167, 69, 0.05); border-left: 5px solid #28a745;
            padding: 15px; margin-bottom: 20px; border-radius: 0 8px 8px 0;
        }

        /* --- DEBUG PANEL CLEAN --- */
        ", root_sel, " .debug-panel-clean {
            margin-top: 30px; padding: 20px;
            background: #ffffff; border: 1px solid #dee2e6;
            border-radius: 8px; font-family: monospace;
            box-shadow: inset 0 1px 3px rgba(0,0,0,0.05);
        }

        .selectize-dropdown { z-index: 999999 !important; }
      ")))
    ),

    div(class = paste("container-fluid", ns("import-container")),

        # FILA 0: HEADER SUPERIOR
        div(class = "row",
            div(class = "col-12", uiOutput(ns("import_header")))
        ),
        br(),
        div(style = "border-top: 4px solid rgba(0,212,255,0.15); margin: 35px 0;"),

        # FILA 1: CUERPO PRINCIPAL
        div(class = "row g-4 align-items-start",
            div(class = "col-md-7",
                div(id = ns("main_input_col"), class = "lock-wrapper",
                    div(class = "row g-3",
                        div(class = "col-md-4",
                            div(id = ns("label_source"), class = "section-label", "Source Type"),
                            selectInput(ns("source"), NULL, choices = c("Local File" = "local_file", "R Example" = "example"), width = "100%")
                        ),
                        div(class = "col-md-8",
                            div(id = ns("label_selection"), class = "section-label", "Data Selection"),
                            div(id = ns("div_menu01"), uiOutput(ns("menu01_local_file"))),
                            div(id = ns("div_menu02"), uiOutput(ns("menu02_RData")))
                        )
                    ),
                    div(id = ns("div_options"), uiOutput(ns("options_ui")))
                )
            ),

            div(class = "col-md-1"),

            div(class = "col-md-4",
                div(class = "action-row-right",
                    actionButton(ns("btn_import"), span(icon("check"), "Import"), class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-warning btn-pill-xl"),
                    actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-primary btn-pill-xl")
                )
            )
        ),

        div(style = "border-top: 4px solid rgba(0,212,255,0.15); margin: 35px 0;"),

        # FILA 2: RESULTADOS Y PREVIEW
        uiOutput(ns("dataset_info_ui")),

        div(class = "row g-0",
            div(class = "col-12",
                div(class = "section-label mb-3", icon("table"), " Data Preview"),
                div(style = "width: 100%; overflow-x: auto; background: white; border-radius: 12px; border: 1px solid #eee; padding: 10px;",
                    DTOutput(ns("preview"))
                ),
                uiOutput(ns("the_debug_container"))
            )
        )
    )
  )
}

mod_import_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- REACTIVE VALUES ---
    data_store <- reactiveValues(
      df = NULL, name_orig = NULL, name_mod = NULL,
      sheet = NULL, sep = NULL, dec = NULL, header = NULL,
      timestamp = NULL, file_obj = NULL, full_path = NULL,
      rows = NULL, cols = NULL
    )
    is_locked <- reactiveVal(FALSE)
    closed_suffix <- HTML("<span class='status-closed' style='font-size: 0.8rem;'>(Locked) <i class='fa fa-lock'></i></span>")

    # --- LOGIC: LOCK & RESET ---
    toggle_import_controls <- function(lock_it) {
      if (lock_it) {
        shinyjs::disable("btn_import")
        shinyjs::html("label_source", paste0("Source Type", closed_suffix))
        shinyjs::html("label_selection", paste0("Data Selection", closed_suffix))
        shinyjs::addClass(id = "main_input_col", class = "locked-disabled")
      } else {
        shinyjs::enable("btn_import")
        shinyjs::html("label_source", "Source Type")
        shinyjs::html("label_selection", "Data Selection")
        shinyjs::removeClass(id = "main_input_col", class = "locked-disabled")
      }
      is_locked(lock_it)
    }

    deep_reset_values <- function() {
      data_store$df <- NULL; data_store$name_orig <- NULL; data_store$name_mod <- NULL
      data_store$sheet <- NULL; data_store$sep <- NULL; data_store$dec <- NULL; data_store$header <- NULL
      data_store$timestamp <- NULL; data_store$file_obj <- NULL; data_store$full_path <- NULL
      data_store$rows <- NULL; data_store$cols <- NULL
    }

    # --- RENDERS: HEADER ---
    output$import_header <- renderUI({
      current_name <- if(input$source == "local_file") {
        if(!is.null(input$file_input)) input$file_input$name else NULL
      } else {
        if(!is.null(input$example_dataset) && input$example_dataset != "") input$example_dataset else NULL
      }

      if (is_locked()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" DATA IMPORTED:", data_store$name_mod)),
            span(class = "header-id", "STATUS: READY"))
      } else if (!is.null(current_name)) {
        div(class = "selection-header active-selection",
            span(icon("file-import"), paste(" Selected:", current_name)),
            span(class = "header-id", "STATUS: PENDING"))
      } else {
        div(class = "selection-header",
            span(icon("bolt"), " Waiting for user selection..."),
            span(class = "header-id", "STATUS: IDLE"))
      }
    })

    # --- RENDERS: MENUS DINÁMICOS ---
    output$menu01_local_file <- renderUI({ req(input$source == 'local_file'); fileInput(ns("file_input"), NULL, buttonLabel = "Browse...", width = "100%") })
    output$menu02_RData <- renderUI({
      req(input$source == 'example')
      selectizeInput(ns("example_dataset"), NULL, choices = c("(Select Dataset)" = "", "mtcars", "iris", "airquality"),
                     width = "100%", options = list(dropdownParent = 'body', placeholder = '(Select Dataset)'))
    })

    output$options_ui <- renderUI({
      req(input$source == "local_file", input$file_input)
      ext <- tolower(tools::file_ext(input$file_input$name))
      if (ext %in% c("csv", "tsv", "txt")) {
        div(class = "mt-3", div(class = "section-label", "Parsing Options"),
            fluidRow(
              column(4, selectizeInput(ns("sep"), "Separator", choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab" = "\t"), options = list(dropdownParent = 'body'))),
              column(4, selectizeInput(ns("dec"), "Decimal", choices = c("Period (.)" = ".", "Comma (,)" = ","), options = list(dropdownParent = 'body'))),
              column(4, div(style = "padding-top: 35px;", checkboxInput(ns("header"), "Header", TRUE)))
            ))
      } else if (ext %in% c("xls", "xlsx")) {
        sheets <- tryCatch({ readxl::excel_sheets(input$file_input$datapath) }, error = function(e) NULL)
        div(class = "mt-3", div(class = "section-label", "Sheet Selection"),
            fluidRow(column(4, selectizeInput(ns("excel_sheet"), NULL, choices = sheets, width = "100%", options = list(dropdownParent = 'body')))))
      }
    })

    # --- ACCIONES ---
    observeEvent(input$btn_import, {
      tryCatch({
        now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        if (input$source == "local_file") {
          req(input$file_input); path <- input$file_input$datapath; ext <- tolower(tools::file_ext(input$file_input$name))
          data_store$file_obj <- input$file_input; data_store$full_path <- path; data_store$name_orig <- input$file_input$name
          if (ext %in% c("csv", "tsv", "txt")) {
            temp_df <- vroom::vroom(path, delim = input$sep, show_col_types = FALSE)
            data_store$sep <- input$sep; data_store$dec <- input$dec; data_store$header <- input$header; data_store$name_mod <- input$file_input$name
          } else {
            req(input$excel_sheet); temp_df <- readxl::read_excel(path, sheet = input$excel_sheet)
            data_store$sheet <- input$excel_sheet; data_store$name_mod <- paste0(input$file_input$name, " [", input$excel_sheet, "]")
          }
        } else {
          req(input$example_dataset != ""); temp_df <- get(input$example_dataset, "package:datasets")
          data_store$name_orig <- input$example_dataset; data_store$name_mod <- paste0(input$example_dataset, " (Example)")
          data_store$full_path <- "Internal R memory"
        }
        data_store$df <- as.data.frame(temp_df)
        data_store$rows <- nrow(data_store$df); data_store$cols <- ncol(data_store$df); data_store$timestamp <- now
        toggle_import_controls(TRUE)
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    # EDIT: Resetea los valores reactivos y desbloquea para nuevos cambios
    observeEvent(input$btn_edit,  {
      deep_reset_values()
      toggle_import_controls(FALSE)
    })

    # RESET: Limpia todo e interfaz al estado inicial
    observeEvent(input$btn_reset, {
      deep_reset_values()
      shinyjs::reset("main_input_col")
      toggle_import_controls(FALSE)
    })

    # --- OUTPUTS ---
    output$preview <- renderDT({
      req(data_store$df)
      datatable(data_store$df, rownames = TRUE, class = 'cell-border hover',
                options = list(scrollX = TRUE, pageLength = 5, dom = 'ltip',
                               rowCallback = JS("function(row, data, displayNum, displayIndex) {
                                 $(row).css('background-color', displayIndex % 2 === 0 ? '#f0ffff' : '#ffffff');
                               }"),
                               initComplete = JS("function(settings, json) {
                                 $(this.api().table().header()).css({'background-color': '#00d4ff', 'color': 'white', 'text-transform': 'uppercase'});
                               }")))
    })

    output$dataset_info_ui <- renderUI({
      req(data_store$df)
      div(class = "file-info-banner", fluidRow(
        column(6, span(style="font-weight:900; color:#28a745; font-size:1.1rem;", "DATASET: "), span(style="font-size:1.1rem; font-weight:600;", data_store$name_mod)),
        column(3, span(style="font-weight:900; color:#28a745;", "ROWS: "), span(data_store$rows)),
        column(3, span(style="font-weight:900; color:#28a745;", "COLS: "), span(data_store$cols))
      ))
    })

    # Panel de Debug Limpio
    output$the_debug_container <- renderUI({
      req(show_debug)
      div(class = "debug-panel-clean",
          div(style = "color: #007bff; font-weight: bold; margin-bottom: 10px;", "DEBUG: DATA_STORE CONTENT"),
          verbatimTextOutput(ns("debug_print"))
      )
    })
    output$debug_print <- renderPrint({ reactiveValuesToList(data_store) })

    return(reactive({ list(df = data_store$df, locked = is_locked(), metadata = reactiveValuesToList(data_store)) }))
  })
}
