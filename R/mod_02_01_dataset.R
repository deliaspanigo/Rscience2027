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
    # Inyectamos dependencias necesarias para los sub-componentes del motor
    shinyjs::useShinyjs(),

    div(
      id = ns("import_container"),
      class = "rs-mod-dataset-container",
      style = "padding: 20px;",

      # ========================================================================
      # CABECERA: TÍTULO + MOTOR (Inline)
      # ========================================================================
      div(id = ns("the_control"),
          style = "display: flex; align-items: center; justify-content: flex-start; gap: 20px; margin-bottom: 25px;",

          # --- ELEMENTOS A LA IZQUIERDA ---
          div(class = "section-label",
              style = "margin: 0; white-space: nowrap; font-size: 1.2rem; font-weight: 700; color: #8b949e;",
              icon("database"), " DATASET"
          ),

          # El interruptor principal
          mod_07_00_toggle_ui(ns("main_switch")),

          # --- ESPACIADOR MÁGICO ---
          div(style = "flex-grow: 1;"),

          # --- ELEMENTOS A LA DERECHA ---
          # Agrupamos los labels y controles auxiliares
          mod_07_00_label_ui(ns("main_switch")),        # Label from control engine
          mod_07_00_refresh_ui(ns("main_switch")),      # Refres button
          mod_07_00_unlock_ghost_ui(ns("main_switch"))  # Ghost button
      ),

      # ========================================================================
      # CUERPO PRINCIPAL: 2 COLUMNAS (30/70)
      # ========================================================================
      div(style = "display: flex; flex-direction: row; align-items: stretch; gap: 25px; width: 100%;",

          # --- COLUMNA 01: CONFIGURACIÓN (IZQUIERDA) ---
          div(style = "flex: 0 0 30%; max-width: 300px;",
              div(id = ns("the_menu"),
                  class = "pack-style-unlock",
                  style = "padding: 20px; border-radius: 15px; background: rgba(255,255,255,0.02); border: 1px solid rgba(255,255,255,0.05);",

                  div(id = ns("label_source"), class = "section-label mb-2",
                      style = "color: #58a6ff; font-size: 0.8rem; text-transform: uppercase;",
                      "Source Type"),

                  selectInput(inputId = ns("source_dataset"), label = NULL,
                              choices = c("Select a source..." = ""),
                              width = "100%"),

                  # Menús dinámicos
                  div(id = ns("div_menu01"), style = "margin-top: 15px;", uiOutput(ns("menu01_local_file"))),
                  div(id = ns("div_menu02"), style = "margin-top: 15px;", uiOutput(ns("menu02_RData"))),

                  # Opciones extra
                  div(id = ns("div_options"),
                      style = "margin-top: 20px; border-top: 1px solid rgba(255,255,255,0.1); padding-top: 15px;",
                      uiOutput(ns("options_ui")))
              )
          ),

          # --- COLUMNA 02: VISUALIZACIÓN (DERECHA) ---
          div(style = "flex: 1 1 70%; display: flex; flex-direction: column; gap: 20px;",

              # Resumen superior
              div(id = ns("the_summary"),
                  class = "pack-style-unlock",
                  uiOutput(ns("import_summary"))
              ),

              # Tabla Preview
              div(id = ns("the_view"),
                  class = "pack-style-unlock rs-table-wrapper",
                  style = "padding: 20px; border-radius: 15px; background: rgba(255,255,255,0.01); border: 1px solid rgba(255,255,255,0.05);",
                  div(class = "section-label mb-3", style = "color: #00e5ff;", icon("eye"), " Data Preview"),
                  DT::DTOutput(ns("preview"))
              ),

              # Debug del motor (aparecerá aquí si show_debug = TRUE en el server)
              # Usamos el namespace del switch para que el motor inyecte aquí el JSON

          )
      ),
      uiOutput(ns("show_debug"))
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
# ==============================================================================
# IMPORT MODULE SERVER - v.0.1.1 (FIXED & SYNCED)
# ==============================================================================
mod_02_01_dataset_server <- function(id, show_debug = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)

    # 1. Engine Control Instance
    rlist_control_btn <- mod_07_00_engine_control_server("main_switch",
                                                         show_debug = internal_show_debug,
                                                         show_ghost = FALSE)

    # 2. Data sources setup ----------------------------------------------------
    vector_hard_source <- c("Select a source..." = "",
                            "01 - Local File" = "local_file",
                            "02 - R Example" = "R_dataset")

    observe({
      updateSelectInput(session = session, inputId = "source_dataset", choices = vector_hard_source)
    })

    # 3. Metadata & Data Store Initializers ------------------------------------
    get_default_metadata_dataset <- function() {
      list(
        description = "*** Rscience - Dataset details ***",
        is_done = FALSE,
        is_locked = FALSE,
        selected_external_source = NULL,
        selected_internal_source = NULL,
        code_import_internal = "",
        code_import_external = "",
        name_mod = NULL,
        rows = NULL,
        cols = NULL,
        my_timestamp = timestamp(),
        df = data.frame()
      )
    }

    RValues_metadata_dataset <- do.call(reactiveValues, get_default_metadata_dataset())

    reset_default_metadata_dataset <- function() {
      defaults <- get_default_metadata_dataset()
      mapply(function(val, name) { RValues_metadata_dataset[[name]] <- val }, defaults, names(defaults))
    }

    get_default_RValues_data_store <- function() {
      list(
        "details" = "*** RScience - Import Engine ***",
        "my_timestamp" = timestamp(),
        "click_count" = 0,
        "is_done" = FALSE,
        "is_locked" = FALSE,
        "error_msg" = NULL,
        "metadata_control_btn" = list(),
        "metadata_dataset" = list()
      )
    }

    RValues_data_store <- do.call(reactiveValues, get_default_RValues_data_store())

    reset_RValues_data_store <- function() {
      defaults <- get_default_RValues_data_store()
      mapply(function(val, name) { RValues_data_store[[name]] <- val }, defaults, names(defaults))
    }

    # 4. UI Actions (Visual Locks) ---------------------------------------------
    toggle_import_controls <- function(lock_it) {
      vector_obj <- c("root_id" = "import_container", "menu_id" = "the_menu",
                      "summary_id" = "the_summary", "view_id" = "the_view")

      if (lock_it) {
        shinyjs::removeClass(vector_obj["summary_id"], "pack-style-unlock pack-style-reset")
        shinyjs::addClass(vector_obj["summary_id"], "pack-style-lock")
        shinyjs::removeClass(vector_obj["menu_id"], "rs-clean-block")
        shinyjs::addClass(vector_obj["menu_id"], "rs-block-smoke")
      } else {
        lapply(vector_obj, function(id) {
          shinyjs::removeClass(id, "pack-style-lock pack-style-reset")
          shinyjs::addClass(id, "pack-style-unlock")
        })
        shinyjs::removeClass(vector_obj["menu_id"], "rs-block-smoke")
      }
    }

    # 5. Core Import Logic -----------------------------------------------------
    import_logic <- function() {
      # 1. Recuperar la fuente seleccionada
      source <- input$source_dataset

      # ============================================================================
      # VALIDACIÓN 01: SOURCE SELECTION
      # ============================================================================
      if (is.null(source) || source == "") {
        showNotification("Please select a source first.", type = "warning")
        # Forzamos el regreso a 'unlock' en el motor
        shinyjs::delay(500, shinyjs::click("main_switch-btn_unlock_ghost"))
        return()
      }

      tryCatch({
        # ============================================================================
        # VALIDACIÓN 02: PARÁMETROS ESPECÍFICOS POR FUENTE
        # ============================================================================

        # --- CASO: LOCAL FILE (CSV/Excel) ---
        if (source == "local_file") {
          if (is.null(input$file_input)) {
            showNotification("No file uploaded. Please browse for a file.", type = "warning")
            shinyjs::delay(500, shinyjs::click("main_switch-btn_unlock_ghost"))
            return()
          }

          selected_file_path <- input$file_input$datapath
          selected_file_name <- input$file_input$name
          ext <- tolower(tools::file_ext(selected_file_name))

          if (ext %in% c("csv", "tsv", "txt")) {
            # Validar parámetros de delimitación
            if (is.null(input$sep) || is.null(input$dec)) return()
            if (input$sep == input$dec) {
              showNotification("Separator and decimal must be different.", type = "warning")
              shinyjs::delay(500, shinyjs::click("main_switch-btn_unlock_ghost"))
              return()
            }

            # Generar código para VROOM
            code_template <- "vroom::vroom(file = '{P}', delim = '{S}', locale = vroom::locale(decimal_mark = '{D}'), show_col_types = FALSE)"
            RValues_metadata_dataset$code_import_external <- gsub("{P}", selected_file_name, code_template, fixed = TRUE)
            RValues_metadata_dataset$code_import_internal <- gsub("{P}", selected_file_path, code_template, fixed = TRUE)

            # Inyectar separadores
            RValues_metadata_dataset$code_import_internal <- gsub("{S}", input$sep, RValues_metadata_dataset$code_import_internal, fixed = TRUE)
            RValues_metadata_dataset$code_import_internal <- gsub("{D}", input$dec, RValues_metadata_dataset$code_import_internal, fixed = TRUE)

            RValues_metadata_dataset$name_mod <- selected_file_name

          } else if (ext == "xlsx") {
            # Validar hoja de Excel
            if (is.null(input$excel_sheet) || input$excel_sheet == "") {
              showNotification("Please select a sheet from the Excel file.", type = "warning")
              shinyjs::delay(500, shinyjs::click("main_switch-btn_unlock_ghost"))
              return()
            }

            selected_sheet <- input$excel_sheet
            excel_template <- "readxl::read_excel(path = '{P}', sheet = '{S}')"

            RValues_metadata_dataset$code_import_external <- gsub("{P}", selected_file_name, excel_template, fixed = TRUE)
            RValues_metadata_dataset$code_import_external <- gsub("{S}", selected_sheet, RValues_metadata_dataset$code_import_external, fixed = TRUE)

            RValues_metadata_dataset$code_import_internal <- gsub("{P}", selected_file_path, excel_template, fixed = TRUE)
            RValues_metadata_dataset$code_import_internal <- gsub("{S}", selected_sheet, RValues_metadata_dataset$code_import_internal, fixed = TRUE)

            RValues_metadata_dataset$name_mod <- paste0(selected_file_name, " [", selected_sheet, "]")
          }

          # --- CASO: R DATASET (datasets package) ---
        } else if (source == "R_dataset") {
          if (is.null(input$selected_R_dataset) || input$selected_R_dataset == "") {
            showNotification("Please select an R example dataset.", type = "warning")
            shinyjs::delay(500, shinyjs::click("main_switch-btn_unlock_ghost"))
            return()
          }

          r_code <- sprintf("get('%s', 'package:datasets')", input$selected_R_dataset)
          RValues_metadata_dataset$code_import_internal <- r_code
          RValues_metadata_dataset$code_import_external <- r_code
          RValues_metadata_dataset$name_mod <- paste(input$selected_R_dataset, "(R)")
        }

        # ============================================================================
        # EJECUCIÓN DEL IMPORT (EVALUACIÓN)
        # ============================================================================
        # Solo llegamos aquí si todas las validaciones de arriba pasaron con éxito
        df_temp <- eval(parse(text = RValues_metadata_dataset$code_import_internal))

        # Actualizar estado reactivo de los datos
        RValues_metadata_dataset$df <- as.data.frame(df_temp)
        RValues_metadata_dataset$rows <- nrow(RValues_metadata_dataset$df)
        RValues_metadata_dataset$cols <- ncol(RValues_metadata_dataset$df)
        RValues_metadata_dataset$is_done <- TRUE

        # Bloquear interfaz (UI Lock) y confirmar éxito
        toggle_import_controls(TRUE)
        showNotification(paste("Success:", RValues_metadata_dataset$name_mod), type = "message")

      }, error = function(e) {
        # Captura errores críticos (ej: archivo mal formado, paquetes faltantes)
        showNotification(paste("Import Error:", e$message), type = "error")
        RValues_metadata_dataset$is_done <- FALSE

        # Reset del motor tras el fallo técnico
        shinyjs::delay(500, shinyjs::click("main_switch-btn_unlock_ghost"))
      })
    }

    # 6. Observers -------------------------------------------------------------

    # Reset automático si cambia la fuente
    observeEvent(input$source_dataset, {
      req(input$source_dataset)
      shinyjs::click(ns("main_switch-unlock_ghost"))
      toggle_import_controls(FALSE)
    })

    # Escucha al Motor (Play / Unlock / Reset)
    observeEvent(rlist_control_btn(), {
      state <- rlist_control_btn()$mode

      if (state == "unlock") {
        reset_default_metadata_dataset()
        reset_RValues_data_store()
        toggle_import_controls(FALSE)
      } else if (state == "lock") {
        import_logic()

        # Si falló la importación, forzamos el regreso a Unlock
        if(!RValues_metadata_dataset$is_done) {
          ####============================================================================
          shinyjs::delay(500, shinyjs::click(ns("main_switch-unlock_ghost")))
          ####============================================================================

        } else RValues_data_store$is_locked <- TRUE
      } else if (state == "reset") {
        # 1. Reseteamos los valores internos

        # 3. Forzamos a que shinyjs limpie el input de archivos (si existe)
        shinyjs::reset("file_input")
        shinyjs::reset("the_menu")
        shinyjs::reset("source_dataset")


        reset_RValues_data_store()
        reset_default_metadata_dataset()

        # 2. Update corregido (SIN el ns())
        #updateSelectInput(session, "source_dataset", selected = "")
        updateSelectInput(session = session, inputId = "source_dataset", choices = vector_hard_source)



        # 4. Volvemos a los colores originales
        toggle_import_controls(FALSE)

        showNotification("Interface reset to defaults", type = "message")
      }
    })

    # Sincronización Metadata -> Data Store (Final)
    observe({
      req(RValues_metadata_dataset$is_done)

      RValues_data_store$is_done <- TRUE
      RValues_data_store$metadata_dataset <- reactiveValuesToList(RValues_metadata_dataset)
      RValues_data_store$metadata_control_btn <- rlist_control_btn()
      RValues_data_store$my_timestamp <- timestamp()
    })

    # 7. Renders ---------------------------------------------------------------
    output$menu01_local_file <- renderUI({
      req(input$source_dataset == 'local_file')
      tagList(
        div(class = "section-label", "Data Selection"),
        fileInput(ns("file_input"), NULL, buttonLabel = "Browse...", width = "100%")
      )
    })

    output$menu02_RData <- renderUI({
      req(input$source_dataset == 'R_dataset')
      selectInput(ns("selected_R_dataset"), "Select Dataset",
                  choices = c("Select a dataset" = "", "mtcars", "iris", "airquality"), width = "100%")
    })

    output$options_ui <- renderUI({
      req(input$source_dataset == "local_file", input$file_input)
      ext <- tolower(tools::file_ext(input$file_input$name))

      if (ext == "xlsx") {
        sheets <- readxl::excel_sheets(input$file_input$datapath)
        div(class = "mt-2",
            div(class = "section-label", "Excel Sheet Selection"),
            selectInput(ns("excel_sheet"), NULL, choices = sheets, width = "100%")
        )
      } else if (ext %in% c("csv", "tsv", "txt")) {
        # Contenedor vertical (sin flex)
        div(style = "display: block; width: 100%; margin-top: 15px;",

            # Bloque Separador (Arriba)
            div(style = "margin-bottom: 15px;",
                div(class = "section-label", style = "font-size: 0.75rem;", "Separator"),
                selectInput(ns("sep"), NULL,
                            choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab (\t)" = "\t"),
                            selected = ";", width = "100%")
            ),

            # Bloque Decimal (Abajo)
            div(style = "margin-bottom: 5px;",
                div(class = "section-label", style = "font-size: 0.75rem;", "Decimal"),
                selectInput(ns("dec"), NULL,
                            choices = c("Comma (,)" = ",", "Dot (.)" = "."),
                            selected = ".", width = "100%")
            )
        )
      }
    })


    output$import_summary <- renderUI({
      has_data <- RValues_data_store$is_done
      div(class = "rs-minimal-bar",
          span("FILE: ", strong(if(has_data) RValues_data_store$metadata_dataset$name_mod else "---")),
          span(" | ROWS: ", strong(if(has_data) RValues_data_store$metadata_dataset$rows else "0"))
      )
    })

    output$preview <- renderDT({
      req(RValues_data_store$is_done)
      datatable(RValues_data_store$metadata_dataset$df,
                options = list(scrollX = TRUE, pageLength = 5, dom = 'ltpi'))
    })

    # 8. Debug Sections --------------------------------------------------------
    output$debug_data_store <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(reactiveValuesToList(RValues_data_store), mode = "text")
    })

    # 8. Debug Sections --------------------------------------------------------
    output$debug_control <- listviewer::renderJsonedit({
      req(internal_show_debug())
      flat_rlist_control_btn <- rlist_control_btn()
      listviewer::jsonedit(flat_rlist_control_btn, mode = "text")
    })

    output$show_debug <- renderUI({
      req(internal_show_debug())

      div(class = "debug-section",
          style = "margin-top: 30px; border: 1px dashed #ef4444; padding: 20px; border-radius: 12px; background: rgba(255, 0, 0, 0.05);",

          # Título de la sección Debug
          div(style = "color: #ef4444; font-weight: bold; margin-bottom: 15px;",
              icon("bug"), " RS-INTERNAL DEBUGGER"),

          # Contenedor de columnas
          div(class = "row",

              # Columna Izquierda: Data Store
              div(class = "col-md-6",
                  span(style = "color: #8b949e; font-size: 0.75rem; text-transform: uppercase;", "Data Store:"),
                  listviewer::jsoneditOutput(ns("debug_data_store"), height = "350px")
              ),

              # Columna Derecha: Control Engine
              div(class = "col-md-6",
                  span(style = "color: #8b949e; font-size: 0.75rem; text-transform: uppercase;", "Control Engine:"),
                  listviewer::jsoneditOutput(ns("debug_control"), height = "350px")
              )
          )
      )
    })

    return(reactive({ reactiveValuesToList(RValues_data_store) }))
  })
}
