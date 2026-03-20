# ==============================================================================
# IMPORT MODULE - v.0.0.1 DEFINITIVE (FULL METADATA & DEEP RESET)
# ==============================================================================
library("shinyjs")
library("DT")
library("vroom")
library("readxl")

mod_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .container-fluid { overflow-x: hidden !important; padding-top: 20px !important; }

        /* BOTONES PILDORA XL - TEXTO GRANDE */
        .btn-pill-xl {
          border-radius: 50px !important; padding: 15px 35px !important;
          font-weight: 800; font-size: 1.1rem !important;
          text-transform: uppercase; letter-spacing: 1px;
          display: inline-flex; align-items: center; justify-content: center; gap: 10px;
        }

        .action-row-right {
          display: flex; flex-direction: row; justify-content: flex-end;
          align-items: center; gap: 15px; height: 100%; padding-top: 25px;
        }

        /* LABELS GRANDES Y NEGROS */
        .section-label {
          font-weight: 800; font-size: 1.2rem !important;
          color: #000000; text-transform: uppercase;
          margin-bottom: 12px; letter-spacing: 1.2px;
        }

        /* ESTADO CLOSED CON CHECK VERDE */
        .status-closed { color: #28a745 !important; font-weight: 900 !important; margin-left: 8px; }

        .engine-divider { border-top: 1px solid rgba(0,212,255,0.2); margin: 30px 0; }

        /* BLOQUEO TOTAL Y CURSOR PROHIBITIVO 🚫 */
        .locked-disabled,
        .locked-disabled .selectize-input,
        .locked-disabled .selectize-control,
        .locked-disabled .btn,
        .locked-disabled input,
        .locked-disabled select {
          cursor: not-allowed !important;
          pointer-events: none !important;
        }
        .locked-disabled { opacity: 0.6; filter: grayscale(0.4); transition: all 0.3s ease; }

        /* TABLA: ZEBRA CYAN + ALINEACIÓN IZQUIERDA */
        table.dataTable thead th, table.dataTable tbody td { text-align: left !important; padding: 12px 15px !important; }
        table.dataTable tr.odd { background-color: #f0ffff !important; }
        table.dataTable tr.even { background-color: #ffffff !important; }
        table.dataTable tbody tr:hover { background-color: #e0f7fa !important; }

        /* BANNER INFORMATIVO VERDE */
        .file-info-banner {
          background: rgba(40, 167, 69, 0.05); border-left: 4px solid #28a745;
          padding: 15px; margin-bottom: 20px; border-radius: 0 8px 8px 0;
        }
      "))
    ),

    div(class = "container-fluid",
        div(class = "row g-0 align-items-center",
            div(class = "col-md-8",
                div(class = "row g-3",
                    div(class = "col-md-4",
                        div(id = ns("div_source"),
                            div(id = ns("label_source"), class = "section-label", "Source Type"),
                            selectInput(ns("source"), NULL, choices = c("Local File" = "local_file", "R Example" = "example"), width = "100%")
                        )
                    ),
                    div(class = "col-md-7",
                        div(id = ns("label_selection"), class = "section-label", "Data Selection"),
                        div(id = ns("div_menu01"), uiOutput(ns("menu01_local_file"))),
                        div(id = ns("div_menu02"), uiOutput(ns("menu02_RData")))
                    )
                ),
                div(id = ns("div_options"), uiOutput(ns("options_ui")))
            ),
            div(class = "col-md-4",
                div(class = "action-row-right",
                    actionButton(ns("btn_import"), span(icon("check"), "Import"), class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-warning btn-pill-xl"),
                    actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-primary btn-pill-xl")
                )
            )
        ),
        div(class = "engine-divider"),
        uiOutput(ns("dataset_info_ui")),
        div(class = "row g-0",
            div(class = "col-12",
                div(class = "section-label mb-3", icon("table"), " Data Preview"),
                div(style = "width: 100%;", DTOutput(ns("preview"))),
                uiOutput(ns("the_debug"))
            )
        )
    )
  )
}

mod_import_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$the_debug <- renderUI({
      req(show_debug)
      div(
        div(class = "engine-divider"),
        div(class = "row g-4",
            div(class = "col-md-6", div(class = "section-label", "System Status"), verbatimTextOutput(ns("status_txt"))),
            div(class = "col-md-6", div(class = "section-label", "Selection Debug"), verbatimTextOutput(ns("debug_selection")))
        )
      )

    })
    # --- ESTADO INTERNO ULTRA COMPLETO ---
    data_store <- reactiveValues(
      df = NULL, name_orig = NULL, name_mod = NULL,
      sheet = NULL, sep = NULL, dec = NULL, header = NULL,
      timestamp = NULL, file_obj = NULL, full_path = NULL,
      rows = NULL, cols = NULL
    )

    is_locked <- reactiveVal(FALSE)
    closed_suffix <- HTML("<span class='status-closed' style='font-size: 1.1rem;'>(Closed) <i class='fa fa-check'></i></span>")

    # --- FUNCIÓN DE LIMPIEZA ATÓMICA ---
    deep_reset_values <- function() {
      data_store$df <- NULL; data_store$name_orig <- NULL; data_store$name_mod <- NULL
      data_store$sheet <- NULL; data_store$sep <- NULL; data_store$dec <- NULL; data_store$header <- NULL
      data_store$timestamp <- NULL; data_store$file_obj <- NULL; data_store$full_path <- NULL
      data_store$rows <- NULL; data_store$cols <- NULL
    }

    toggle_import_controls <- function(lock_it) {
      # Añadimos 'label_selection' y 'label_source' a la lista de IDs a los que aplicar la clase CSS
      ids <- c("div_source", "label_source", "div_menu01", "div_menu02", "label_selection", "div_options")
      ids <- c("div_source", "div_menu01", "div_menu02", "label_selection", "div_options")

      if (lock_it) {
        shinyjs::disable("btn_import")
        shinyjs::html("label_source", paste0("Source Type", closed_suffix))
        shinyjs::html("label_selection", paste0("Data Selection", closed_suffix))
        for (i in ids) shinyjs::addClass(id = i, class = "locked-disabled")
      } else {
        shinyjs::enable("btn_import")
        shinyjs::html("label_source", "Source Type")
        shinyjs::html("label_selection", "Data Selection")
        for (i in ids) shinyjs::removeClass(id = i, class = "locked-disabled")
      }
      is_locked(lock_it)
    }

    output$menu01_local_file <- renderUI({ req(input$source == 'local_file'); fileInput(ns("file_input"), NULL, buttonLabel = "Browse...", width = "100%") })
    output$menu02_RData <- renderUI({ req(input$source == 'example'); selectInput(ns("example_dataset"), NULL, choices = c("(Select Dataset)" = "", "mtcars", "iris", "airquality", "diamonds"), width = "100%") })

    output$options_ui <- renderUI({
      req(input$source == "local_file", input$file_input)
      ext <- tolower(tools::file_ext(input$file_input$name))

      lbl_opts <- if(is_locked()) tagList("Parsing Options", closed_suffix) else "Parsing Options"
      lbl_sheet <- if(is_locked()) tagList("Sheet Selection", closed_suffix) else "Sheet Selection"

      if (ext %in% c("csv", "tsv", "txt")) {
        div(class = "mt-3",
            div(class = "section-label", lbl_opts),
            fluidRow(
              column(4, selectInput(ns("sep"), "Separator", choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab" = "\t"), selected = input$sep)),
              column(4, selectInput(ns("dec"), "Decimal", choices = c("Period (.)" = ".", "Comma (,)" = ","), selected = input$dec)),
              column(4, div(style = "padding-top: 35px;", checkboxInput(inputId = ns("header"), label = "Header", value = TRUE)))
            )
        )
      } else if (ext %in% c("xls", "xlsx")) {
        sheets <- tryCatch({ readxl::excel_sheets(input$file_input$datapath) }, error = function(e) NULL)
        div(class = "mt-3",
            div(class = "section-label", lbl_sheet),
            # --- CAMBIO AQUÍ: Envolvemos en Row/Column para limitar el ancho ---
            fluidRow(
              column(4, selectInput(ns("excel_sheet"), NULL, choices = sheets, selected = input$excel_sheet, width = "100%"))
            )
        )
      }
    })

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
            data_store$sheet <- input$excel_sheet; data_store$name_mod <- paste0(input$file_input$name, " - ", input$excel_sheet)
          }
        } else {
          req(input$example_dataset != ""); temp_df <- get(input$example_dataset, "package:datasets")
          data_store$name_orig <- input$example_dataset; data_store$name_mod <- paste0("Example: ", input$example_dataset)
          data_store$full_path <- "Internal R memory"
        }

        data_store$df <- as.data.frame(temp_df)
        rownames(data_store$df) <- 1:nrow(data_store$df)

        data_store$rows <- nrow(data_store$df)
        data_store$cols <- ncol(data_store$df)
        data_store$timestamp <- now

        toggle_import_controls(TRUE)
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    observeEvent(input$btn_edit,  { deep_reset_values(); toggle_import_controls(FALSE) })
    observeEvent(input$btn_reset, { deep_reset_values(); shinyjs::reset("file_input"); toggle_import_controls(FALSE) })

    # --- OBJETO DE SALIDA MAESTRO (TODO LO QUE VENIMOS DISCUTIENDO) ---
    user_selection <- reactive({
      list(
        is_done   = !is.null(data_store$df),
        locked    = is_locked(),
        metadata  = list(
          name_orig = data_store$name_orig,
          name_mod  = data_store$name_mod,
          path      = data_store$full_path,
          sheet     = data_store$sheet,
          sep       = data_store$sep,
          dec       = data_store$dec,
          header    = data_store$header,
          timestamp = data_store$timestamp,
          file_obj  = data_store$file_obj
        ),
        stats     = list(rows = data_store$rows, cols = data_store$cols),
        dataset   = data_store$df
      )
    })

    output$preview <- renderDT({ req(data_store$df); datatable(data_store$df, rownames = TRUE, options = list(scrollX = TRUE, pageLength = 5, dom = 'ltip')) })

    output$dataset_info_ui <- renderUI({
      req(data_store$df)
      div(class = "file-info-banner",
          fluidRow(
            column(6, span(style="font-weight:900; color:#28a745; font-size:1.2rem;", "DATASET: "), span(style="font-size:1.2rem; font-weight:600;", data_store$name_mod)),
            column(3, span(style="font-weight:900; color:#28a745;", "ROWS: "), span(data_store$rows)),
            column(3, span(style="font-weight:900; color:#28a745;", "COLS: "), span(data_store$cols))
          ))
    })

    output$status_txt <- renderText({ if (is.null(data_store$df)) "STATUS: [ OFFLINE ]" else paste0("STATUS: [ ONLINE ] ", data_store$rows, " rows") })
    output$debug_selection <- renderPrint({ user_selection() })

    return(user_selection)
  })
}
