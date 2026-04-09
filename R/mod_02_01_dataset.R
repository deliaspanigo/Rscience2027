# ==============================================================================
# IMPORT MODULE UI - v.0.1.0 (CLEAN LOCK & DYNAMIC COLORS)
# ==============================================================================
library("shinyjs")
library("DT")
library("vroom")
library("readxl")
library("bslib")

mod_02_01_dataset_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # CLASE RAÍZ ÚNICA PARA AISLAMIENTO TOTAL
    div(
      id = ns("import_container"),
      class = "rs-mod-dataset-container",


      # FILA 0: HEADER
      div(id = ns("the_header"),
          class = "pack-style-unlock",
          div(class = "row",
          div(class = "col-12", uiOutput(ns("import_header")))
          )
      ),
      br(),

      # FILA 1: PANEL DE CONTROL
      div(class = "row g-4 align-items-start",
          div(class = "col-md-8",
              # Aplicamos las clases base: pack-style-unlock para el color y neon-glow-RUN para el latido
              div(id = ns("the_menu"),
                  class = "lock-wrapper pack-style-unlock neon-glow-RUN",
                  div(class = "row g-3",
                      div(class = "col-md-4",
                          div(id = ns("label_source"), class = "section-label", "Source Type"),
                          selectInput(inputId = ns("source_dataset"), label = NULL,
                                      choices = c("Select a source..." = "",
                                                  "01 - Local File" = "local_file",
                                                  "02 - R Example" = "R_dataset"),
                                      width = "100%")
                      ),
                      div(class = "col-md-8",
                          div(id = ns("div_menu01"), uiOutput(ns("menu01_local_file"))),
                          div(id = ns("div_menu02"), uiOutput(ns("menu02_RData")))
                      )
                  ),
                  div(id = ns("div_options"), uiOutput(ns("options_ui")))
              )
          ),

          div(id = ns("the_control"),
              class = "col-md-4",
              mod_07_00_engine_control_ui(ns("main_switch"))

          )
      ),

      div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 35px 0;"),

      # Reemplázala por esto:
      div(id = ns("the_summary"),
          class = "pack-style-unlock",
          uiOutput(ns("import_summary"))
      ),

      # FILA 2: PREVIEW
      div(class = "row",
          div(class = "col-12",
              div(id = ns("the_view"),
                  class = "pack-style-unlock rs-table-wrapper",
                    div(class = "section-label mb-3", icon("table"), " Data Preview"),
                        DTOutput(ns("preview"))
                    ),
              uiOutput(ns("show_debug_internal"))
          )
      )
    )
  )
}

mod_02_01_dataset_DEBUG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("show_debug_external"))
  )
}

# ==============================================================================
# IMPORT MODULE SERVER - v.0.1.0 (CORRECTED)
# ==============================================================================
mod_02_01_dataset_server <- function(id, show_debug = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)

    # Engine Control
    engine_state <- mod_07_00_engine_control_server("main_switch", show_debug = internal_show_debug)

    # --- REACTIVE VALUES ---
    get_default_data <- function() {
      list(
        "details" = "*** RScience - Import Engine ***",
        "my_sys_time" = Sys.time(),
        "click_count" = 0, # Corregido el nombre si era click_count
        "is_done" = FALSE,
        "is_locked" = FALSE,
        "error_msg" = NULL,
        "metadata" = list(
          selected_source = NULL, name_mod = NULL, rows = NULL, cols = NULL, "my_sys_time" = NULL
        ),
        "df" = NULL
      )
    }
    reset_data_store <- function() {
      defaults <- get_default_data()

      # mapply recorre los nombres y valores de la lista de defaults
      # y los asigna uno a uno al objeto reactiveValues
      mapply(function(val, name) {
        data_store[[name]] <- val
      }, defaults, names(defaults))
    }
    data_store <- do.call(reactiveValues, get_default_data())

    # --- LÓGICA DE IMPORTACIÓN ---
    import_logic <- function() {
      source <- input$source_dataset
      if (source == "") {
        showNotification("Please select a source first.", type = "warning")
        return()
      }

      tryCatch({
        temp_df <- NULL
        if (source == "local_file") {
          req(input$file_input)
          path <- input$file_input$datapath
          ext <- tolower(tools::file_ext(input$file_input$name))

          if (ext %in% c("csv", "tsv", "txt")) {
            temp_df <- vroom::vroom(path, delim = input$sep, show_col_types = FALSE)
            data_store$metadata$name_mod <- input$file_input$name
          } else if (ext == "xlsx") {
            req(input$excel_sheet)
            temp_df <- readxl::read_excel(path, sheet = input$excel_sheet)
            data_store$metadata$name_mod <- paste0(input$file_input$name, " [", input$excel_sheet, "]")
          }
        } else {
          req(input$selected_R_dataset)
          temp_df <- get(input$selected_R_dataset, "package:datasets")
          data_store$metadata$name_mod <- paste(input$selected_R_dataset, "(R)")
        }

        data_store$df <- as.data.frame(temp_df)
        data_store$metadata$rows <- nrow(data_store$df)
        data_store$metadata$cols <- ncol(data_store$df)
        data_store$metadata$"my_sys_time" <- format(Sys.time(), "%H:%M:%S")
        data_store$is_done <- TRUE

        toggle_import_controls(TRUE)
        showNotification(paste("Imported:", data_store$metadata$name_mod), type = "message")

      }, error = function(e) {
        showNotification(paste("Import Error:", e$message), type = "error")
      })
    }

    # --- OBSERVER PRINCIPAL ---
    observeEvent(engine_state(), {
      state <- engine_state()$mode
      data_store$click_count <- data_store$click_count + 1

      if (state == "lock") {
        import_logic()
        if(data_store$is_done){
          data_store$is_locked <- TRUE
        } else {
          data_store$is_locked <- FALSE
          showNotification("Selection is not completed... Status Unlock", type = "warning")
          reset_data_store()
          shinyjs::delay(1000, {
            shinyWidgets::updateRadioGroupButtons(
              session = session,
              inputId = "main_switch-engine_mode",
              selected = "unlock"
            )
          })
        }
      } else if (state == "unlock") {
        toggle_import_controls(FALSE)
        data_store$is_locked <- FALSE

        reset_data_store()
        # unblock_opts()
      } else if (state == "reset") {
        reset_all()
        # block_opts()
      }
    })

    # --- FUNCIONES DE ACCIÓN ---


    toggle_import_controls <- function(lock_it) {
      vector_obj <- c("root_id" = "import_container",
                      "header_id"  = "the_header",
                      "menu_id" = "the_menu",
                      "control_id" = "the_control",
                      "summary_id" = "the_summary",
                      "view_id" = "the_view")

      selected_root <- "import_container"
      selected_summary <-vector_obj["summary_id"]
      selected_menu <-vector_obj["menu_id"]

      if (lock_it) {
        # Hay que bloquar...
        # Pasamos a modo LOCK (Verde)
        ## Summary al estado Lock (VERDE)
        shinyjs::removeClass(selected_summary, "pack-style-unlock pack-style-reset")   # Quitamos los colores...
        shinyjs::addClass(selected_summary, "pack-style-lock") # Aplicamos el color de lock

        ## Bloqueamos el menu de seleccion
        shinyjs::removeClass(selected_menu, "rs-clean-block")   # Quitamos el clean...
        shinyjs::addClass(selected_menu, "rs-block-smoke")      # Aplicamos block...
        shinyjs::removeClass(selected_menu, "neon-glow-RUN")    # Quitamos el neon...


      } else {
        # Hay que desplockear
        # Pasamos a modo UNLOCK (Cian)

        # Limpiamos los colores de todos...
        lapply(vector_obj, function(selected_id) {
          shinyjs::removeClass(selected_id, "pack-style-lock  pack-style-reset")
          shinyjs::addClass(selected_id, "pack-style-unlock")
        })

        # Cambios varios
        shinyjs::addClass(selected_menu, "neon-glow-RUN")  # aplicamos el neon
        shinyjs::removeClass(selected_menu, "rs-block-smoke") # Quitamos el block smote
        shinyjs::removeClass(selected_menu, "rs-block-invisible") # Quitamos el block invisible del menu
        shinyjs::removeClass(selected_root, "rs-block-invisible") # Quitamos el block invisible de la pagina principal


      }
    }

    reset_all <- function() {
      vector_obj <- c("root_id" = "import_container",
                      "header_id"  = "the_header",
                      "menu_id" = "the_menu",
                      "control_id" = "the_control",
                      "summary_id" = "the_summary",
                      "view_id" = "the_view")

      #  Descatados...
      selected_root <- "import_container"
      selected_menu <- vector_obj["menu_id"]

      # Cambios por reset
      shinyjs::removeClass(selected_menu, "rs-block-smoke")  # Quitamos smoke (por las dudas...)
      shinyjs::addClass(selected_menu, "neon-glow-RUN")      # colocamos el neon...

      shinyjs::addClass(selected_root, "rs-block-invisible") # Bloqueamos todo hasta que finalice el reseteo...



      reset_data_store()  # Reseteo interno del reactive vallues...
      shinyjs::reset(selected_menu) # Reseteamos las opciones del menu a default

      # Mandamos a todos lso colores de reset....
      lapply(vector_obj, function(selected_id) {
        shinyjs::removeClass(selected_id, "pack-style-lock pack-style-unlock pack-style-reset")
        shinyjs::addClass(selected_id, "pack-style-reset")
      })


    }

    # block_opts <- function(){
    #   root_id <- "import_container"
    #   input_panel_id <- "main_input_col"
    #   # shinyjs::addClass(root_id , "rs-block-invisible")
    # }
    #
    # unblock_opts <- function(){
    #   root_id <- "import_container"
    #   input_panel_id <- "main_input_col"
    #   # shinyjs::removeClass(root_id , "rs-block-invisible")
    # }


    # --- RENDERS ---
    output$import_header <- renderUI({
      state <- engine_state()$mode
      if (state == "lock" && data_store$is_done) {
        div(class = "selection-header confirmed", span("DATASET - ", icon("lock"), " - IMPORTED AND LOCKED"), span(class="header-id", "LOCK"))
      } else if (state == "unlock") {
        div(class = "selection-header active-selection", span("DATASET - ", icon("lock-open"), " - READY FOR SELECTION"), span(class="header-id", "UNLOCK"))
      } else {
        div(class = "selection-header waiting-mode", span("DATASET - ", icon("bolt"), " - WAITING..."))
      }
    })

    output$menu01_local_file <- renderUI({
      req(input$source_dataset == 'local_file')
      tagList(
        div(id = ns("label_selection"), class = "section-label", "Data Selection"),
        fileInput(ns("file_input"), NULL, buttonLabel = "Browse...", width = "100%")
      )
    })

    output$menu02_RData <- renderUI({
      req(input$source_dataset == 'R_dataset')
      tagList(
        div(id = ns("label_selection"), class = "section-label", "Data Selection"),
        selectInput(ns("selected_R_dataset"), NULL,
                    choices = c("(Select source first)" = "", "mtcars", "iris", "airquality"),
                    width = "100%")
      )
    })

    output$import_summary <- renderUI({
      has_data <- !is.null(data_store$df) && data_store$is_done
      state_class <- if(has_data) "rs-status-locked" else "rs-status-waiting"
      div(class = paste("rs-minimal-bar", state_class),
          div(class = "status-segment",
              div(class = "led-indicator"),
              span(class = "status-text", if(has_data) "DATASET CONFIRMED" else "AWAITING CONFIRMATION...")),
          div(class = "info-segment",
              span(class = "info-label", "FILE:"),
              span(class = "info-val", if(has_data) data_store$metadata$name_mod else "---")),
          div(class = "info-segment",
              span(class = "info-label", "ROWS:"),
              span(class = "info-val", if(has_data) data_store$metadata$rows else "0")),
          div(class = "info-segment",
              span(class = "info-label", "COLS:"),
              span(class = "info-val", if(has_data) data_store$metadata$cols else "0"))
      )
    })

    output$options_ui <- renderUI({
      req(input$source_dataset == "local_file", input$file_input)
      ext <- tolower(tools::file_ext(input$file_input$name))
      if (ext == "xlsx") {
        sheets <- readxl::excel_sheets(input$file_input$datapath)
        div(class = "row mt-2", div(class = "col-12",
                                    div(id = ns("label_sheet"), class = "section-label", "Excel Sheet Selection"),
                                    selectInput(ns("excel_sheet"), NULL, choices = sheets, width = "100%")))
      } else if (ext %in% c("csv", "tsv", "txt")) {
        div(class = "row mt-2", div(class = "col-12",
                                    div(id = ns("label_sep"), class = "section-label", "Delimiter / Separator"),
                                    selectInput(ns("sep"), NULL, choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab (\t)" = "\t"), selected = ",", width = "100%")))
      }
    })

    output$preview <- renderDT({
      req(data_store$df)
      datatable(data_store$df, options = list(scrollX = TRUE, scrollY = "400px", scrollCollapse = TRUE, pageLength = 5, dom = 'ftpi'))
    })


    # # # DEBUG
    output$debug_internal <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(listdata = reactiveValuesToList(data_store), mode = "text")
    })

    output$show_debug_internal <- renderUI({
      req(internal_show_debug())
      div(class = "debug-section", style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",
          div(class = "section-label", style = "justify-content: flex-start !important; gap: 8px;", icon("bug"), " Internal Debug - Dataset"),
          listviewer::jsoneditOutput(ns("debug_internal"), height = "auto"))
    })

    output$debug_external <- listviewer::renderJsonedit({
      listviewer::jsonedit(listdata = reactiveValuesToList(data_store), mode = "text")
    })

    output$show_debug_external <- renderUI({
      div(style = "background: #1a1a1a; padding: 15px; border-radius: 8px;",
          div(class = "row",
              div(class = "col-md-8",
                  div(class = "section-label", style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 15px;", icon("bug"), " External Debug - Dataset"),
                  listviewer::jsoneditOutput(ns("debug_external"), height = "500px")),
              div(class = "col-md-4",
                  div(style = "border-left: 1px solid #333; padding-left: 15px; height: 100%;",
                      mod_07_00_engine_control_DEBUG_ui(id = ns("main_switch"))))))
    })

    # # # OUTPUT
    return(reactive({ reactiveValuesToList(data_store) }))
  })
}
