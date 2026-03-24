

# ==============================================================================
# IMPORT MODULE UI - v.0.1.0 (CLEAN LOCK & DYNAMIC COLORS)
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
        /* --- BLOQUEO DE COLUMNA: NITIDEZ TOTAL --- */
        ", root_sel, " .lock-wrapper {
            position: relative;
            transition: all 0.3s ease;
        }

        ", root_sel, " .locked-disabled::after {
            content: '';
            position: absolute;
            top: -8px; left: -8px; right: -8px; bottom: -8px;
            background: rgba(0, 0, 0, 0.03);
            backdrop-filter: none !important;
            -webkit-backdrop-filter: none !important;
            z-index: 100;
            border-radius: 15px;
            border: 1px dashed rgba(0,0,0,0.1);
            cursor: not-allowed !important;
        }

        ", root_sel, " .locked-disabled {
            pointer-events: none !important;
            user-select: none;
            filter: none !important;
            opacity: 0.85;
        }

        /* --- SELECTION HEADER (MARQUESINA DINÁMICA) --- */
        ", root_sel, " .selection-header {
            display: flex; justify-content: space-between; align-items: center;
            padding: 15px 25px; border-radius: 12px; transition: all 0.4s ease;
            box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        }

        ", root_sel, " .selection-header.waiting-mode { background: #f0fdff; border: 1px solid #00cfd4; color: #008184; }
        ", root_sel, " .selection-header.active-selection { background: #fff9f0; border: 1px solid #ff9100; color: #b36600; }
        ", root_sel, " .selection-header.confirmed { background: #f6fff8; border: 1px solid #28a745; color: #1e7e34; }

        ", root_sel, " .header-id {
            font-family: 'Monaco', 'Courier New', monospace; font-weight: 700;
            font-size: 0.85rem; background: rgba(0,0,0,0.08); padding: 4px 15px; border-radius: 20px;
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

        /* --- LABELS Y SECCIONES --- */
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

        # FILA 0: EL HEADER SUPERIOR (INDEPENDIENTE)
        div(class = "row",
            div(class = "col-12", uiOutput(ns("import_header")))
        ),
        br(),

        # FILA 1: PANEL DE CONTROL
        div(class = "row g-4 align-items-start",
            # Columna de Entradas
            div(class = "col-md-7",
                div(id = ns("main_input_col"), class = "lock-wrapper",
                    div(class = "row g-3",
                        div(class = "col-md-4",
                            div(id = ns("label_source"), class = "section-label", "Source Type"),
                            selectInput(ns("source"), NULL, choices = c("Local File" = "local_file",
                                                                        "R Example" = "R_dataset"), width = "100%")
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

            # Columna de Acciones
            div(class = "col-md-4",
                div(class = "action-row-right",
                    actionButton(ns("btn_import"), span(icon("check"), "Import"), class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-warning btn-pill-xl"),
                    actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-primary btn-pill-xl")
                )
            )
        ),

        div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 35px 0;"),

        # FILA 2: DATOS E INFORMACIÓN
        uiOutput(ns("dataset_info_ui")),

        div(class = "row g-0",
            div(class = "col-12",
                div(class = "section-label mb-3", icon("table"), " Data Preview"),
                div(style = "width: 100%; overflow-x: auto; background: white; border-radius: 12px; border: 1px solid #eee; padding: 10px;",
                    DTOutput(ns("preview"))
                ),
                listviewer::jsoneditOutput(ns("debug_json"), height = "600px")                )
        )
    )
  )
}

# ==============================================================================
# IMPORT MODULE SERVER - v.0.1.0 (COMPLETED WITH NESTED METADATA)
# ==============================================================================
# ==============================================================================
# IMPORT MODULE SERVER - v.0.1.0 (CORRECTED)
# ==============================================================================
mod_import_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- REACTIVE VALUES ---
    list_default <- list(
      "details" = "*** Rscience - Import action ***",
      "is_done" = NULL,
      "is_locked" = NULL,
      "error_msg" = NULL,
      "success_msg" = NULL,
      "metadata" = list(
        selected_source = NULL,
        name_orig = NULL, name_mod = NULL,
        sheet = NULL, sep = NULL, dec = NULL, header = NULL,
        timestamp = NULL, file_obj = NULL, full_path = NULL,
        rows = NULL, cols = NULL,
        vector_colnames = NULL,
        is_dataframe = NULL),
      "df" = NULL
    )

    data_store <- do.call(reactiveValues, list_default)
    is_done   <- reactiveVal(FALSE)
    is_locked <- reactiveVal(FALSE)


    # --- FUNCIONES DE SOPORTE ---
    reset_data_store <- function() {
      for (name in names(list_default)) {
        data_store[[name]] <- list_default[[name]]
      }
      message("--- [DATA_STORE] Reset completo ---")
    }

    toggle_import_controls <- function(lock_it) {
      closed_suffix <- HTML("<span class='status-closed' style='font-size: 0.8rem;'>(Locked) <i class='fa fa-lock'></i></span>")

      is_locked(lock_it)

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

    }

    # --- RENDER: HEADER DINÁMICO ---
    output$import_header <- renderUI({
      # Consistencia de IDs: usamos input$selected_R_dataset
      current_name <- if(input$source == "local_file") {
        if(!is.null(input$file_input)) input$file_input$name else NULL
      } else {
        if(!is.null(input$selected_R_dataset) && input$selected_R_dataset != "") input$selected_R_dataset else NULL
      }

      if (is_locked()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" DATA IMPORTED:", data_store$metadata$name_mod)),
            span(class = "header-id", "STATUS: READY"))
      } else if (!is.null(current_name)) {
        div(class = "selection-header active-selection",
            span(icon("file-import"), paste(" SELECTED:", current_name)),
            span(class = "header-id", "STATUS: PENDING"))
      } else {
        div(class = "selection-header waiting-mode",
            span(icon("bolt"), " Waiting for user selection..."),
            span(class = "header-id", "STATUS: WAITING"))
      }
    })

    # --- RENDERS: MENUS DINÁMICOS ---
    output$menu01_local_file <- renderUI({ req(input$source == 'local_file'); fileInput(ns("file_input"), NULL, buttonLabel = "Browse...", width = "100%") })

    output$menu02_RData <- renderUI({
      req(input$source == 'R_dataset')
      selectizeInput(ns("selected_R_dataset"), NULL, choices = c("(Select Dataset)" = "", "mtcars", "iris", "airquality"),
                     width = "100%", options = list(dropdownParent = 'body'))
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
            fluidRow(column(6, selectizeInput(ns("excel_sheet"), "Select Sheet", choices = sheets, width = "100%", options = list(dropdownParent = 'body')))))
      }
    })

    # --- ACCIÓN IMPORTAR ---
    # --- ACCIÓN IMPORTAR (v.0.1.1 - Validated) ---
    observeEvent(input$btn_import, {

      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      data_store$metadata$timestamp <- now
      data_store$metadata$selected_source <- input$source


      # 1. VALIDACIÓN DE ENTRADAS (v.0.1.1)
      # ------------------------------------------------------------------------
      if (input$source == "local_file") {
        # Caso: Archivo Local
        if (is.null(input$file_input)) {
          string_error <- "Friendly message: No file has been selected for import."
          data_store$error_msg <- string_error
          showNotification(string_error, type = "warning")
          return()
        }

        ext <- tolower(tools::file_ext(input$file_input$name))
        if (ext %in% c("xlsx") && (is.null(input$excel_sheet) || input$excel_sheet == "")) {
          string_error <- "Friendly message: Select sheet from your xlsx file."
          data_store$error_msg <- string_error
          showNotification(string_error, type = "warning")
          return()
        }

      } else if (input$source == "R_dataset") {
        # Caso: Dataset de R
        if (is.null(input$selected_R_dataset) || input$selected_R_dataset == "") {
          string_error <- "Friendly message: Select an example dataset from the list."
          data_store$error_msg <- string_error
          showNotification(string_error, type = "warning")
          return()
        }

      } else {
        # Caso: Error interno (input$source tiene un valor no controlado)
        string_error <- paste0(
          "Error internal 01: Problems with input$source. The option is '",
          input$source,
          "' but is not a valid option."
        )
        data_store$error_msg <- string_error
        showNotification(string_error, type = "error")
        return()
      }

      # 2. PROCESAMIENTO DE DATOS
      # ------------------------------------------------------------------------

      data_store$error_msg <- NULL # Limpiamos errores anteriores

      tryCatch({
        #data_store$metadata$selected_source <- input$source

        if (input$source == "local_file") {
          path <- input$file_input$datapath
          ext <- tolower(tools::file_ext(input$file_input$name))
          data_store$metadata$name_orig <- input$file_input$name

          if (ext %in% c("csv", "tsv", "txt")) {
            # Uso de vroom para velocidad (ideal para Data Science)
            temp_df <- vroom::vroom(
              path,
              delim = input$sep,
              col_types = list(),
              show_col_types = FALSE
            )
            data_store$metadata$name_mod <- input$file_input$name

          } else if (ext %in% c("xlsx")) {
            temp_df <- readxl::read_excel(path, sheet = input$excel_sheet)
            data_store$metadata$name_mod <- paste0(input$file_input$name, " [", input$excel_sheet, "]")
          }
           else if (ext %in% c("xls")) {
             string_error <- "Friendly message: The old '.xls' format is not supported. Please convert to '.xlsx' and try again in Rscience."
             data_store$error_msg <- string_error
             showNotification(string_error, type = "warning")
             stop(paste0("Extension '", ext, "' is not supported."))
           }else {
             string_error <- paste0("Friendly message: Your '", ext, "' file is not valid in Rscience. In Rscience, the valid formats for external files are xlsx, txt, csv, and tsv.")
             data_store$error_msg <- string_error
            stop(paste0("Extension '", ext, "' is not supported."))
           }
        } else {
          # Carga de datasets de R
          temp_df <- get(input$selected_R_dataset, "package:datasets")
          data_store$metadata$name_mod <- paste0(input$selected_R_dataset, " (R Dataset)")
        }

        # 3. ACTUALIZACIÓN DEL DATA_STORE
        # ------------------------------------------------------------------------
        data_store$df <- as.data.frame(temp_df)
        data_store$metadata$rows <- nrow(data_store$df)
        data_store$metadata$cols <- ncol(data_store$df)
        data_store$metadata$vector_colnames <- colnames(data_store$df)


        # Finalización exitosa
        data_store$metadata$is_dataframe <- is.data.frame(data_store$df)
        if(data_store$metadata$is_dataframe == TRUE) is_done(TRUE)

        if(is_done() == TRUE) {
          isolate({
          #print("A")
          str_message <- paste("Success:", data_store$metadata$name_mod, "imported.")
          data_store$"success_msg" <- str_message
          showNotification(str_message, type = "message")
          toggle_import_controls(TRUE)
          })
        }

        if(is_locked() == TRUE) {
          isolate({
          #print("B")

          str_message <- paste("Success: Import session locked.")
          data_store$"success_msg" <- str_message
          showNotification(str_message, type = "message")
          })
        }

      }, error = function(e) {
        # Captura errores de lectura (archivo corrupto, delimitador erróneo, etc.)
        data_store$error_msg <- e$message
        showNotification(paste("Error Internal 02:", e$message), type = "error")
      })
    })

    # --- ACCIONES EDIT Y RESET ---
    observeEvent(input$btn_edit, {
      reset_data_store()
      #data_store$is_done <- FALSE
      toggle_import_controls(FALSE)
    })

    observeEvent(input$btn_reset, {
      reset_data_store()
      shinyjs::reset("main_input_col")
      toggle_import_controls(FALSE)
    })

    # --- OUTPUTS ---
    output$preview <- renderDT({
      req(data_store$df)
      datatable(data_store$df, options = list(scrollX = TRUE, pageLength = 5))
    })

    output$dataset_info_ui <- renderUI({
      req(data_store$df)
      div(class = "file-info-banner", fluidRow(
        column(6, span(style="font-weight:900; color:#28a745;", "DATASET: "), span(data_store$metadata$name_mod)),
        column(3, span(style="font-weight:900; color:#28a745;", "ROWS: "), span(data_store$metadata$rows)),
        column(3, span(style="font-weight:900; color:#28a745;", "COLS: "), span(data_store$metadata$cols))
      ))
    })

    # --- LÓGICA DE DEBUG DINÁMICA ---


    # El renderPrint que ya tenías (ahora sí tiene un destino en la UI)
    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      # Convertimos reactiveValues a lista normal para el visor
      listviewer::jsonedit(listdata =reactiveValuesToList(data_store), mode = "text")
    })

    return(reactive({ reactiveValuesToList(data_store) }))
  })
}
