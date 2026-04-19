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
    #shinyjs::useShinyjs(),

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
mod_02_01_dataset_server <- function(id, show_debug = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)

    # Engine Control
    rlist_control_btn <- mod_07_00_engine_control_server("main_switch", show_debug = internal_show_debug, show_ghost = FALSE)

    # Data sources details -----------------------------------------------------
    vector_hard_source <- c("Select a source..." = "",
                            "01 - Local File" = "local_file",
                            "02 - R Example" = "R_dataset")

    observe({
      updateSelectInput(session = session, inputId = "source_dataset", choices = vector_hard_source)
    })

    # Metadata - Dataset --------------------------------------------------------------------
    get_default_metadata_dataset <- function() {
        list(
            description = "*** Rscience - Dataset details ***",
            my_timestamp = timestamp(),
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
    reset_default_metadata_dataset <- function() {
      defaults <- get_default_metadata_dataset()

      # mapply recorre los nombres y valores de la lista de defaults
      # y los asigna uno a uno al objeto reactiveValues
      mapply(function(val, name) {
        RValues_metadata_dataset[[name]] <- val
      }, defaults, names(defaults))
    }
    RValues_metadata_dataset <- do.call(reactiveValues, get_default_metadata_dataset())


    # Metadata - Dataset --------------------------------------------------------------------
    get_default_RValues_data_store <- function() {
      list(
        "details" = "*** RScience - Import Engine ***",
        "my_timestamp" = timestamp(),
        "click_count" = 0, # Corregido el nombre si era click_count
        "is_done" = FALSE,
        "is_locked" = FALSE,
        "error_msg" = NULL,
        "metadata_control_btn" = list(),
        "metadata_dataset" = list()

      )
    }
    reset_RValues_data_store <- function() {
      defaults <- get_default_RValues_data_store()

      # mapply recorre los nombres y valores de la lista de defaults
      # y los asigna uno a uno al objeto reactiveValues
      mapply(function(val, name) {
        RValues_data_store[[name]] <- val
      }, defaults, names(defaults))
    }
    RValues_data_store <- do.call(reactiveValues, get_default_RValues_data_store())



    # --- LÓGICA DE IMPORTACIÓN --------------------------------------------------------------------
    # Here is for option seleccition and define is_done...
    import_logic <- function() {

      reset_default_metadata_dataset()
      reset_RValues_data_store()

      source <- input$source_dataset
      if (source == "") {
        showNotification("Please select a source first.", type = "warning")

        return()
      }

      tryCatch({

        if (source == "local_file") {
          req(input$file_input)
          path <- input$file_input$datapath
          ext <- tolower(tools::file_ext(input$file_input$name))
          selected_file_name <- basename(path)
          selected_sep <- input$sep
          selected_dec <- input$dec

          check_sep_dec_no_equal <- selected_sep != selected_dec
          if(check_sep_dec_no_equal == FALSE) {
            showNotification(paste("Friendly message: ", "Separator and decimal must be differentes."), type = "warning")
            return()
          }



          if (ext %in% c("csv", "tsv", "txt")) {

            # 1. Definimos la plantilla con palabras clave fáciles de identificar
            template_multi <- "vroom::vroom(file = '{FILE_PATH}',
                                      delim = '{SEP}',
                                      locale = vroom::locale(decimal_mark = '{DEC}'),
                                      show_col_types = FALSE,
                                      col_names = TRUE,
                                      na = c('', 'NA'))"

            # 2. Realizamos las sustituciones
            import_code_external <- template_multi
            import_code_external <- gsub("{FILE_PATH}", selected_file_name, import_code_external, fixed = TRUE)
            import_code_external <- gsub("{SEP}", selected_sep, import_code_external, fixed = TRUE)
            import_code_external <- gsub("{DEC}", selected_dec, import_code_external, fixed = TRUE)

            # 2. Realizamos las sustituciones
            import_code_internal <- template_multi
            import_code_internal <- gsub("{FILE_PATH}", path, import_code_internal, fixed = TRUE)
            import_code_internal <- gsub("{SEP}", selected_sep, import_code_internal, fixed = TRUE)
            import_code_internal <- gsub("{DEC}", selected_dec, import_code_internal, fixed = TRUE)



            # 3. Ejecutamos el código final
            RValues_metadata_dataset$code_import_external <-import_code_external
            RValues_metadata_dataset$code_import_internal <-import_code_internal
            #temp_df <- eval(parse(text = import_code_internal))
            RValues_metadata_dataset$name_mod <- input$file_input$name


          } else if (ext == "xlsx") {
            req(input$excel_sheet)
            # 1. Definimos la plantilla (Template)
            excel_template <- "readxl::read_excel(path = '{FILE_PATH}',
                                   sheet = '{SHEET}',
                                   col_names = TRUE)"

            # 2. Realizamos las sustituciones
            import_code_excel_external <- excel_template
            import_code_excel_external <- gsub("{FILE_PATH}", selected_file_name, import_code_excel_external, fixed = TRUE)
            import_code_excel_external <- gsub("{SHEET}", input$excel_sheet, import_code_excel_external, fixed = TRUE)

            # 2. Realizamos las sustituciones
            import_code_excel_internal <- excel_template
            import_code_excel_internal <- gsub("{FILE_PATH}", path, import_code_excel_internal, fixed = TRUE)
            import_code_excel_internal <- gsub("{SHEET}", input$excel_sheet, import_code_excel_internal, fixed = TRUE)

            # 3. Ejecutamos el código
            # 3. Ejecutamos el código final
            RValues_metadata_dataset$code_import_external <- import_code_excel_external
            RValues_metadata_dataset$code_import_internal <- import_code_excel_internal
            #temp_df <- eval(parse(text = import_code_excel_internal))
            RValues_metadata_dataset$name_mod <- paste0(input$file_input$name, " [", input$excel_sheet, "]")
          }
        } else if (source == "R_dataset") {
          req(input$selected_R_dataset)
          selected_R_dataset <- input$selected_R_dataset

          import_Rdataset <- "get('{DATASET_NAME}', 'package:datasets')"
          import_code_Rdataset <- import_Rdataset
          import_code_Rdataset <- gsub("{DATASET_NAME}", selected_R_dataset, import_code_Rdataset, fixed = TRUE)

          RValues_metadata_dataset$code_import_external <- import_code_Rdataset
          RValues_metadata_dataset$code_import_internal <- import_code_Rdataset
          #temp_df <- eval(parse(text = import_code_Rdataset))
          RValues_metadata_dataset$name_mod <- paste(input$selected_R_dataset, "(R)")
        } else {
            # AGREGAR ALGO PARA CUANDO NO DETECTA NINGUNA SOURCE VALIDA
        }

        # Si llegamos aca, es que todo fue exitoso.
        str_import_internal <- RValues_metadata_dataset$code_import_internal
        RValues_metadata_dataset$df <- eval(parse(text = str_import_internal))
        RValues_metadata_dataset$df <- as.data.frame(RValues_metadata_dataset$df)
        RValues_metadata_dataset$rows <- nrow(RValues_metadata_dataset$df)
        RValues_metadata_dataset$cols <- ncol(RValues_metadata_dataset$df)
        RValues_metadata_dataset$"my_timestamp" <- timestamp()
        RValues_metadata_dataset$is_done <- TRUE

        toggle_import_controls(TRUE)
        showNotification(paste("Imported:", RValues_metadata_dataset$name_mod), type = "message")


      }, error = function(e) {
        showNotification(paste("Import Error:", e$message), type = "error")
      })
    }

    # functions
    # --- FUNCIONES DE ACCIÓN --------------------------------------------------------------------
    toggle_import_controls <- function(lock_it) {
      vector_obj <- c("root_id" = "import_container",
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
        #shinyjs::addClass(selected_menu, "neon-glow-RUN")  # aplicamos el neon
        shinyjs::removeClass(selected_menu, "rs-block-smoke") # Quitamos el block smote
        shinyjs::removeClass(selected_menu, "rs-block-invisible") # Quitamos el block invisible del menu
        shinyjs::removeClass(selected_root, "rs-block-invisible") # Quitamos el block invisible de la pagina principal


      }
    }

    reset_all <- function() {
      vector_obj <- c("root_id" = "import_container",
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



      reset_RValues_data_store()  # Reseteo interno del reactive vallues...
      #shinyjs::reset(selected_menu) # Reseteamos las opciones del menu a default
      shinyjs::reset(selected_menu)


      # Mandamos a todos lso colores de reset....
      lapply(vector_obj, function(selected_id) {
        shinyjs::removeClass(selected_id, "pack-style-lock pack-style-unlock pack-style-reset")
        shinyjs::addClass(selected_id, "pack-style-reset")
      })


    }



    # --- OBSERVER PRINCIPAL ---
    # Here is for running import logic and define is_locked...
    observeEvent(rlist_control_btn(), {

      flat_rlist_control_btn <- rlist_control_btn()
      control_state <- flat_rlist_control_btn$mode

      #RValues_data_store$click_count <- RValues_data_store$click_count + 1
      RValues_metadata_dataset$"my_timestamp" = timestamp()
      RValues_metadata_dataset$selected_internal_source <- input$"source_dataset"
      RValues_metadata_dataset$selected_external_source <- names(vector_hard_source)[vector_hard_source == input$"source_dataset"]

      if (control_state == "unlock") {

        reset_default_metadata_dataset()
        reset_RValues_data_store()
        toggle_import_controls(FALSE)

        return()
      }

      if (control_state == "lock") {
        import_logic()



        if(RValues_data_store$is_done == TRUE){
          RValues_data_store$is_locked <- TRUE
          return()
        }

        if(RValues_metadata_dataset$is_done == FALSE) {
          RValues_metadata_dataset$is_locked <- FALSE
          showNotification("Selection is not completed... Status Unlock", type = "warning")
          reset_RValues_data_store()


          shinyjs::delay(1000, {
            shinyjs::click("main_switch-btn_unlock_ghost")
          })
          ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### -----
          return()
        }
      }

      if (control_state == "reset") {
        reset_default_metadata_dataset()
        reset_RValues_data_store()
        reset_all()


        return()
      }

    })


    observeEvent(list(rlist_control_btn(), RValues_metadata_dataset), {

      flat_rlist_control_btn <- rlist_control_btn()
      is_locked <- flat_rlist_control_btn$is_locked

      flat_metadata_dataset <- reactiveValuesToList(RValues_metadata_dataset)
      is_done_df <- flat_metadata_dataset$is_done
      req(is_locked, is_done_df)
      if(is_locked && is_done_df){

        RValues_data_store$"my_timestamp" <- timestamp()
        RValues_data_store$"click_count"  <- RValues_data_store$"click_count" + 1
        RValues_data_store$"is_done"      <- TRUE
        RValues_data_store$"is_locked"    <- TRUE
        RValues_data_store$"metadata_control_btn" <- flat_rlist_control_btn
        RValues_data_store$"metadata_dataset"  <- flat_metadata_dataset


        #reset_default_metadata_dataset()
      }

    })



    # --- RENDERS ---
    output$import_header <- renderUI({
      state <- rlist_control_btn()$mode
      if (state == "lock" && RValues_data_store$is_done) {
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
      has_data <- !is.null(RValues_data_store$metadata_dataset$df) && RValues_data_store$is_done
      state_class <- if(has_data) "rs-status-locked" else "rs-status-waiting"
      div(class = paste("rs-minimal-bar", state_class),
          div(class = "status-segment",
              div(class = "led-indicator"),
              span(class = "status-text", if(has_data) "DATASET CONFIRMED" else "AWAITING CONFIRMATION...")),
          div(class = "info-segment",
              span(class = "info-label", "FILE:"),
              span(class = "info-val", if(has_data) RValues_data_store$metadata_dataset$name_mod else "---")),
          div(class = "info-segment",
              span(class = "info-label", "ROWS:"),
              span(class = "info-val", if(has_data) RValues_data_store$metadata_dataset$rows else "0")),
          div(class = "info-segment",
              span(class = "info-label", "COLS:"),
              span(class = "info-val", if(has_data) RValues_data_store$metadata_dataset$cols else "0"))
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
        div(class = "row mt-2",
            div(class = "col-4",
              div(id = ns("label_sep"), class = "section-label", "Delimiter / Separator"),
              selectInput(inputId = ns("sep"),
                          label =  NULL,
                          choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab (\t)" = "\t"),
                          selected = ";", width = "100%")),
            div(class = "col-4",
                div(id = ns("label_dec"), class = "section-label", "Decimal"),
                selectInput(inputId = ns("dec"),
                            label =  NULL,
                            choices = c("Comma (,)" = ",", "Dot (.)" = "."),
                            selected = ".", width = "100%")))
      }
    })

    output$preview <- renderDT({
      req(RValues_data_store$metadata_dataset$df)
      flat_df <- RValues_data_store$metadata_dataset$df

      datatable(flat_df, options = list(scrollX = TRUE, scrollY = "400px", scrollCollapse = TRUE, pageLength = 5, dom = 'ftpi'))
    })


    # # # DEBUG
    output$debug_control_btn <- listviewer::renderJsonedit({
      req(internal_show_debug())
      flat_control_btn <- rlist_control_btn()
      listviewer::jsonedit(listdata = flat_control_btn, mode = "text")
    })

    output$debug_metadata_dataset <- listviewer::renderJsonedit({
      req(internal_show_debug())
      flat_metadata_dataset <- reactiveValuesToList(RValues_metadata_dataset)
      listviewer::jsonedit(listdata = flat_metadata_dataset, mode = "text")
    })

    output$debug_data_store <- listviewer::renderJsonedit({
      req(internal_show_debug())
      flat_data_store <- reactiveValuesToList(RValues_data_store)
      listviewer::jsonedit(listdata = flat_data_store, mode = "text")
    })

    output$show_debug <- renderUI({
      req(internal_show_debug())

      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 15px;",

          # Título de la sección
          div(class = "section-label",
              style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 15px;",
              icon("bug"), " Internal Debug - Dataset"
          ),

          # Contenedor de Columnas
          div(class = "row",
              div(class = "col-md-4",
                  span(style = "color: #8b949e; font-size: 0.8rem;", "Data store - Dataset:"),
                  listviewer::jsoneditOutput(ns("debug_data_store"), height = "auto")
              ),
              # Columna 1: Metadata
              div(class = "col-md-4",
                  span(style = "color: #8b949e; font-size: 0.8rem;", "Metadata - Dataset:"),
                  listviewer::jsoneditOutput(ns("debug_metadata_dataset"), height = "auto")
              ),
              # Columna 2: Control Button
              div(class = "col-md-4",
                  span(style = "color: #8b949e; font-size: 0.8rem;", "Control Engine State - Dataset:"),
                  listviewer::jsoneditOutput(ns("debug_control_btn"), height = "auto")
              )
          )
      )
    })



    # # # OUTPUT
    return(reactive({ reactiveValuesToList(RValues_data_store) }))
  })
}
