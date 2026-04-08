

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
      div(class = "row",
          div(class = "col-12", uiOutput(ns("import_header")))
      ),
      br(),

      # FILA 1: PANEL DE CONTROL
      div(class = "row g-4 align-items-start",
          div(class = "col-md-8",
              div(id = ns("main_input_col"), class = "lock-wrapper",
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
                          #div(id = ns("label_selection"), class = "section-label", "Data Selection"),
                          div(id = ns("div_menu01"), uiOutput(ns("menu01_local_file"))),
                          div(id = ns("div_menu02"), uiOutput(ns("menu02_RData")))
                      )
                  ),
                  div(id = ns("div_options"), uiOutput(ns("options_ui")))

              )
          ),

          div(class = "col-md-4",
              div(class = "action-row-right",
                  mod_07_00_engine_control_ui(ns("main_switch"))
              )
          )
      ),



      div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 35px 0;"),

      uiOutput(ns("import_summary")),
      # FILA 2: PREVIEW
      div(class = "row",
          div(class = "col-12",
              div(class = "section-label mb-3", icon("table"), " Data Preview"),
              div(class = "rs-table-wrapper", # Clase específica para la tabla,

                  DTOutput(ns("preview"))
              ),
              uiOutput(ns("show_debug_internal"))
          )
      )
    )
  )
}


mod_02_01_dataset_DEBUG_ui <-  function(id) {

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


    internal_show_debug     <- reactive( if(is.function(show_debug)) show_debug() else show_debug)


    # Invocamos el Engine Control y recibimos su estado reactivo (mode y last_update)
    engine_state <- mod_07_00_engine_control_server("main_switch", show_debug = internal_show_debug)

    # --- REACTIVE VALUES ---
    list_default <- list(
      "details" = "*** RScience - Import Engine ***",
      "my_sys_time" = Sys.time(),
      click_count_count = 0,
      "is_done" = FALSE,
      "is_locked" = FALSE,
      "error_msg" = NULL,
      "metadata" = list(
        selected_source = NULL, name_mod = NULL, rows = NULL, cols = NULL, timestamp = NULL
      ),
      "df" = NULL
    )
    data_store <- do.call(reactiveValues, list_default)
    ##############################################################################################
    import_logic <- function() {
      # 1. Validaciones
      source <- input$source_dataset
      if (source == "") {
        showNotification("Please select a source first.", type = "warning")
        return()
      }

      # 2. Procesamiento
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

        # 3. Guardar y Bloquear
        data_store$df <- as.data.frame(temp_df)
        data_store$metadata$rows <- nrow(data_store$df)
        data_store$metadata$cols <- ncol(data_store$df)
        data_store$metadata$timestamp <- format(Sys.time(), "%H:%M:%S")
        data_store$is_done <- TRUE
        #data_store$is_locked <- TRUE

        toggle_import_controls(TRUE)
        showNotification(paste("Imported:", data_store$metadata$name_mod), type = "message")

      }, error = function(e) {
        showNotification(paste("Import Error:", e$message), type = "error")
      })
    }
    ##############################################################################################

    # --- OBSERVER PRINCIPAL (EL CEREBRO) ---
    observeEvent(engine_state(), {
      state <- engine_state()$mode

      data_store$click_count <- data_store$click_count + 1
      if (state == "lock") {
        # Acción: Intentar importar
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
                inputId = "main_switch-engine_mode", # SIN ns()
                selected = "unlock"
              )
            })

            message("--- [IMPORT] Validación fallida: Forzando vuelta a UNLOCK ---")
        }
      } else if (state == "unlock") {
        # Acción: Liberar interfaz
        toggle_import_controls(FALSE)
        data_store$is_locked <- FALSE
        reset_data_store()
        #reset_all()
      } else if (state == "reset") {
        # Acción: Limpiar todo
        reset_all()
        reset_data_store
      }
    })

    # --- FUNCIONES DE ACCIÓN ---
    # reset_data_store <- function() {
    #   for (name in names(list_default)) data_store[[name]] <- list_default[[name]]
    # }
    # --- FUNCIONES DE ACCIÓN CORREGIDAS ---
    reset_data_store <- function() {
      # Reseteamos valores simples
      data_store$details <- "*** RScience - Import Engine ***"
      data_store$is_done <- FALSE
      data_store$is_locked <- FALSE
      data_store$error_msg <- NULL
      data_store$df <- data.frame()  # Crucial para que renderDT se limpie

      # Reseteamos la lista anidada de metadata explícitamente
      data_store$metadata <- list(
        selected_source = NULL,
        name_mod = NULL,
        rows = NULL,
        cols = NULL,
        timestamp = NULL
      )

      message("--- [IMPORT] Data Store limpiado con éxito ---")
    }

    reset_all <- function() {
      reset_data_store()
      shinyjs::reset("main_input_col")
      toggle_import_controls(FALSE)
      message("--- [IMPORT] Reset total ejecutado ---")
    }

    toggle_import_controls <- function(lock_it) {
      suffix <- if(lock_it) HTML(" <span class='status-closed'>(Locked) <i class='fa fa-lock'></i></span>") else ""

      if (lock_it) {
        shinyjs::addClass(id = "main_input_col", class = "locked-disabled")

        # Labels principales
        shinyjs::html("label_source", paste0("Source Type", suffix))
        shinyjs::html("label_selection", paste0("Data Selection", suffix))

        # Labels Dinámicos (Solo si existen en el UI en ese momento)
        shinyjs::html("label_sheet", paste0("Excel Sheet Selection", suffix))
        shinyjs::html("label_sep", paste0("Delimiter / Separator", suffix))

      } else {
        shinyjs::removeClass(id = "main_input_col", class = "locked-disabled")

        # Reset labels principales
        shinyjs::html("label_source", "Source Type")
        shinyjs::html("label_selection", "Data Selection")

        # Reset labels dinámicos
        shinyjs::html("label_sheet", "Excel Sheet Selection")
        shinyjs::html("label_sep", "Delimiter / Separator")
      }
    }



    # --- RENDERIZADO DE INTERFAZ (Se mantiene similar) ---
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

    # (Aquí irían los renders de menu01, menu02 y options_ui que ya tenías)
    output$menu01_local_file <- renderUI({
      req(input$source_dataset == 'local_file')

      tagList(
        div(id = ns("label_selection"), class = "section-label", "Data Selection"),
        fileInput(ns("file_input"), NULL, buttonLabel = "Browse...", width = "100%")
      )
    })



    # Ejemplo para el selector de R_dataset
    output$menu02_RData <- renderUI({
      req(input$source_dataset == 'R_dataset')

      tagList(
        div(id = ns("label_selection"), class = "section-label", "Data Selection"),
        selectInput(ns("selected_R_dataset"), NULL,
                    choices = c("(Select source first)" = "", "mtcars", "iris", "airquality"),
                    width = "100%")
      )
    })


    # --- INFO DE DATASET IMPORTADO ---
    output$import_summary <- renderUI({
      has_data <- !is.null(data_store$df) && data_store$is_done

      # Clase principal: cambia de 'waiting' (blanco/glow) a 'locked' (verde)
      state_class <- if(has_data) "rs-status-locked" else "rs-status-waiting"

      div(class = paste("rs-minimal-bar", state_class),
          # Segmento 1: LED + Texto de Estado
          div(class = "status-segment",
              div(class = "led-indicator"),
              span(class = "status-text", if(has_data) "DATASET CONFIRMED" else "AWAITING CONFIRMATION...")),

          # Segmento 2: Info (Sola fila)
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

    # --- RENDERIZADO DE OPCIONES DINÁMICAS (Sheet, Sep, etc.) ---
    output$options_ui <- renderUI({
      req(input$source_dataset == "local_file")
      req(input$file_input)

      ext <- tolower(tools::file_ext(input$file_input$name))

      if (ext == "xlsx") {
        sheets <- readxl::excel_sheets(input$file_input$datapath)
        div(class = "row mt-2",
            div(class = "col-12",
                # ID AÑADIDO: label_sheet
                div(id = ns("label_sheet"), class = "section-label", "Excel Sheet Selection"),
                selectInput(ns("excel_sheet"), NULL, choices = sheets, width = "100%")
            )
        )
      } else if (ext %in% c("csv", "tsv", "txt")) {
        div(class = "row mt-2",
            div(class = "col-12",
                # ID AÑADIDO: label_sep
                div(id = ns("label_sep"), class = "section-label", "Delimiter / Separator"),
                selectInput(ns("sep"), NULL,
                               choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab (\t)" = "\t"),
                               selected = ",", width = "100%")
            )
        )
      }
    })

    output$preview <- renderDT({
      req(data_store$df)
      datatable(
        data_store$df,
        options = list(
          scrollX = TRUE,
          scrollY = "400px",  # <--- ESTO LIMITA EL ALTO DE LA TABLA INTERNA
          scrollCollapse = TRUE,
          pageLength = 5,
          dom = 'ftpi' # f:filter, t:table, p:pagination, i:info (quitamos la 'l' de length para ahorrar espacio)
        )
      )
    })


    ########3
    # 4. DEBUG RENDERS
    output$debug_internal <- listviewer::renderJsonedit({
      req(internal_show_debug())
      # Solo devolvemos la lista. El estilo se lo das al contenedor en la UI.
      listviewer::jsonedit(
        listdata = reactiveValuesToList(data_store),
        mode = "text"
      )
    })

    output$show_debug_internal <- renderUI({
      req(internal_show_debug())
      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",
          div(class = "section-label", style = "justify-content: flex-start !important; gap: 8px;", icon("bug"), " Internal Debug - Dataset"),
          listviewer::jsoneditOutput(ns("debug_internal"), height = "auto")
      )
    })

    output$debug_external <- listviewer::renderJsonedit({
      listviewer::jsonedit(listdata = reactiveValuesToList(data_store), mode = "text")
    })

    output$show_debug_external <- renderUI({
      div(style = "background: #1a1a1a; padding: 15px; border-radius: 8px;",
          # Título (Header)


          # Contenedor de dos columnas
          div(class = "row",
              # Columna Izquierda: Visor JSON
              div(class = "col-md-8",
                  div(class = "section-label",
                      style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 15px;",
                      icon("bug"), " External Debug - Dataset"),
                  listviewer::jsoneditOutput(ns("debug_external"), height = "500px")
              ),

              # Columna Derecha: Controles de Engine
              div(class = "col-md-4",
                  div(style = "border-left: 1px solid #333; padding-left: 15px; height: 100%;",
                      mod_07_00_engine_control_DEBUG_ui(id = ns("main_switch"))
                  )
              )
          )
      )
    })

    return(reactive({ reactiveValuesToList(data_store) }))
  })
}
