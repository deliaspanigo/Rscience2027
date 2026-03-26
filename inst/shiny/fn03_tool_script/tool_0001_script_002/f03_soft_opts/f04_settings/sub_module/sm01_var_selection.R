library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(DT)
library(openxlsx)
library(jsonlite)
library(listviewer)
library(stringr)

# ==============================================================================
# MODULE: VARIABLE SELECTOR (RV, FACTOR & ALPHA) - RScience v.0.0.1
# ==============================================================================

SUB_mod_var_selection_ui <- function(id) {
  ns <- NS(id)
  scope_id <- paste0("#", ns("var_selector_container"))

  css_custom <- paste0("
    ", scope_id, " .card-container { overflow: visible !important; position: relative; }
    .selectize-dropdown { z-index: 999999 !important; position: absolute !important; }

    ", scope_id, " .locked-overlay {
      position: absolute; top: 0; left: 0; width: 100%; height: 100%;
      background: rgba(230, 230, 230, 0.5); z-index: 2000; border-radius: 15px;
      display: flex; justify-content: center; align-items: center;
      visibility: hidden; opacity: 0; transition: all 0.3s ease;
    }
    ", scope_id, " .is-locked .locked-overlay { visibility: visible; opacity: 1; }
    ", scope_id, " .lock-icon { font-size: 5rem; color: #28a745; filter: drop-shadow(0 0 5px white); }

    ", scope_id, " .selection-header { padding: 15px 25px; border-radius: 12px; margin-bottom: 20px; font-weight: 800; display: flex; justify-content: space-between; align-items: center; }
    ", scope_id, " .confirmed { background: #d4edda; color: #155724; border-left: 8px solid #28a745; }
    ", scope_id, " .waiting { background: #e9ecef; color: #495057; border-left: 8px solid #adb5bd; }
    ", scope_id, " .active-selection { background: #fff3cd; color: #856404; border-left: 8px solid #ffc107; }
    ", scope_id, " .error-selection { background: #f8d7da; color: #721c24; border-left: 8px solid #dc3545; }

    ", scope_id, " .btn-pill-xl { border-radius: 50px !important; padding: 10px 25px !important; font-weight: 700 !important; text-transform: uppercase; border: none !important; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
    ", scope_id, " .action-row-right { display: flex; gap: 10px; justify-content: flex-end; margin-bottom: 20px; align-items: center; }
  ")

  tagList(
    tags$head(tags$style(HTML(css_custom))),
    useShinyjs(),

    div(id = ns("var_selector_container"),
        uiOutput(ns("status_header")),

        div(class = "action-row-right",
            actionButton(ns("btn_import"), span(icon("check"), "Accept Selection"), class = "btn-success btn-pill-xl"),
            actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-warning btn-pill-xl"),
            actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-primary btn-pill-xl")
        ),

        div(id = ns("card_inputs_wrapper"), class = "card-container",
            div(class = "locked-overlay", div(class = "lock-icon", icon("lock"))),
            div(style = "background: #ffffff; padding: 25px; border-radius: 15px; border: 1px solid #e0e0e0; box-shadow: 0 4px 15px rgba(0,0,0,0.08);",
                fluidRow(
                  column(4,
                         selectizeInput(ns("var_rv"), "Response Variable (RV)",
                                        choices = NULL,
                                        options = list(placeholder = 'Select RV...',
                                                       onInitialize = I('function() { this.setValue(""); }')))
                  ),
                  column(4,
                         selectizeInput(ns("var_factor"), "Factor Variable (Factor)",
                                        choices = NULL,
                                        options = list(placeholder = 'Select Factor...',
                                                       onInitialize = I('function() { this.setValue(""); }')))
                  ),
                  column(4,
                         selectizeInput(ns("alpha_value"), "Significance Alpha",
                                        choices = c("1% (0.01)" = 0.01, "5% (0.05)" = 0.05, "10% (0.10)" = 0.10),
                                        selected = 0.05)
                  )
                ),
                hr(),
                DTOutput(ns("info_table"))
            )
        ),

        div(style = "margin-top: 20px;",
            listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
        )
    )
  )
}

# ==============================================================================
# SERVER: CONTROL LOGIC AND OUTPUT
# ==============================================================================

# ==============================================================================
# SERVER: CONTROL LOGIC AND OUTPUT - RScience v.0.0.1 (FIXED)
# ==============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

SUB_mod_var_selection_server <- function(id, df_input = mtcars, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. INITIAL STATE
    list_default <- list(
      "details"    = "*** Variable Selection - RScience ***",
      "is_done"    = FALSE,
      "is_locked"  = FALSE,
      "time_stamp" = Sys.time(),
      "metadata"   = list(rv = NULL, factor = NULL, alpha = 0.05, controls_passed = FALSE)
    )

    data_store <- do.call(reactiveValues, list_default)
    output_rv  <- reactiveValues(val = list_default)

    the_dataset <- reactive({ if (is.reactive(df_input)) df_input() else df_input })

    # 2. INPUT UPDATES
    observe({
      df <- the_dataset(); req(df)
      cols <- names(df)
      n_digits <- nchar(length(cols))
      vector_pos_mod <- sprintf(paste0("%0", n_digits, "d"), 1:length(cols))
      vector_letter <- openxlsx::int2col(1:length(cols))
      max_width <- max(nchar(vector_letter))
      vector_letter_mod <- stringr::str_pad(vector_letter, width = max_width, side = "left")

      choices_list <- cols
      names(choices_list) <- paste0(vector_pos_mod, " - ", vector_letter_mod, " - ", cols)

      updateSelectizeInput(session, "var_rv", choices = choices_list, selected = "", server = TRUE)
      updateSelectizeInput(session, "var_factor", choices = choices_list, selected = "", server = TRUE)
    })

    # 3. STATISTICAL VALIDATION (CON GUARDAS CONTRA NULL)
    check_analysis_validity <- reactive({
      # Evitamos el error de 'missing value' asegurando que no sean NULL
      v_rv  <- input$var_rv
      v_fac <- input$var_factor

      # Si son NULL o vacíos, devolvemos estado de espera sin evaluar lógica pesada
      if (is.null(v_rv) || is.null(v_fac) || v_rv == "" || v_fac == "") {
        return(list(valid = TRUE, msg = "Waiting for User Selection"))
      }

      if (v_rv == v_fac) return(list(valid = FALSE, msg = "Error: Duplicate Variables Selected"))

      df_raw <- the_dataset()
      # Verificamos que las columnas realmente existan en el df actual antes de filtrar
      if (!all(c(v_rv, v_fac) %in% names(df_raw))) return(list(valid = TRUE, msg = "Syncing..."))

      mini_db <- df_raw[, c(v_rv, v_fac), drop = FALSE] %>% na.omit()

      if (!is.numeric(mini_db[[v_rv]])) return(list(valid = FALSE, msg = "Error: RV must be numeric"))

      fac_vec <- as.factor(mini_db[[v_fac]])
      if (nlevels(fac_vec) < 2) return(list(valid = FALSE, msg = "Error: Factor needs >= 2 levels"))

      if (nrow(mini_db) > 0 && var(mini_db[[v_rv]]) == 0) return(list(valid = FALSE, msg = "Error: RV total variance is zero"))

      return(list(valid = TRUE, msg = "Ready to Accept Selection"))
    })

    is_valid_ui <- reactive({
      check <- check_analysis_validity()
      # Valida solo si el objeto check existe y los inputs tienen contenido
      v_rv  <- input$var_rv %||% ""
      v_fac <- input$var_factor %||% ""
      return(isTRUE(check$valid) && v_rv != "" && v_fac != "")
    })

    observe({
      # shinyjs::toggleState es más seguro para inicializaciones
      valid <- is_valid_ui()
      locked <- isTRUE(data_store$is_locked)
      if (valid && !locked) shinyjs::enable("btn_import") else shinyjs::disable("btn_import")
    })

    # --- CLICK ACCEPT ---
    observeEvent(input$btn_import, {
      req(is_valid_ui())
      check <- check_analysis_validity()

      data_store$is_locked <- TRUE
      data_store$is_done <- TRUE
      data_store$time_stamp <- Sys.time()
      data_store$metadata <- list(
        rv = input$var_rv,
        factor = input$var_factor,
        alpha = as.numeric(input$alpha_value %||% 0.05),
        controls_passed = check$valid
      )

      output_rv$val <- reactiveValuesToList(data_store)

      shinyjs::addClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::disable("var_rv"); shinyjs::disable("var_factor"); shinyjs::disable("alpha_value")
    })

    # --- CLICK EDIT ---
    observeEvent(input$btn_edit, {
      data_store$is_locked <- FALSE
      data_store$is_done   <- FALSE
      data_store$time_stamp <- Sys.time()
      output_rv$val <- list_default

      shinyjs::removeClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::enable("var_rv"); shinyjs::enable("var_factor"); shinyjs::enable("alpha_value")
    })

    # --- CLICK RESET ---
    observeEvent(input$btn_reset, {
      for (name in names(list_default)) { data_store[[name]] <- list_default[[name]] }
      data_store$time_stamp <- Sys.time()
      output_rv$val <- list_default

      updateSelectizeInput(session, "var_rv", selected = "")
      updateSelectizeInput(session, "var_factor", selected = "")
      updateSelectizeInput(session, "alpha_value", selected = 0.05)
      shinyjs::removeClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::enable("var_rv"); shinyjs::enable("var_factor"); shinyjs::enable("alpha_value")
    })

    # 5. RENDERS
    output$status_header <- renderUI({
      locked <- isTRUE(data_store$is_locked)
      check  <- check_analysis_validity()
      v_rv   <- input$var_rv %||% ""
      v_fac  <- input$var_factor %||% ""

      if (locked) {
        cls <- "confirmed"; msg <- "Selection Locked"; ico <- "lock"
      } else if (!isTRUE(check$valid)) {
        cls <- "error-selection"; msg <- check$msg; ico <- "times-circle"
      } else if (v_rv != "" && v_fac != "") {
        cls <- "active-selection"; msg <- check$msg; ico <- "hand-point-up"
      } else {
        cls <- "waiting"; msg <- check$msg; ico <- "hourglass-half"
      }

      div(class = paste("selection-header", cls),
          span(icon(ico), msg),
          span(style="font-size: 0.8rem; opacity: 0.7;", format(data_store$time_stamp, "%H:%M:%S")))
    })

    output$info_table <- renderDT({
      df <- the_dataset(); req(df)
      cols <- names(df)
      v_rv  <- input$var_rv %||% ""
      v_fac <- input$var_factor %||% ""

      get_meta <- function(var_name, role_name, abbr) {
        if (var_name == "" || !(var_name %in% cols)) {
          return(data.frame(Role = role_name, Abbr = abbr, Name = "---", Pos = "---", Let = "---"))
        }
        idx <- which(cols == var_name)
        n_digits <- nchar(length(cols))
        pos_num <- sprintf(paste0("%0", n_digits, "d"), idx)
        pos_let <- openxlsx::int2col(idx)

        data.frame(Role = role_name, Abbr = abbr, Name = var_name, Pos = pos_num, Let = pos_let, stringsAsFactors = FALSE)
      }

      res <- rbind(get_meta(v_rv, "Response Variable", "RV"), get_meta(v_fac, "Factor Variable", "Factor"))

      datatable(res, colnames = c("Role", "Abbr.", "Variable Name", "Col. Nº", "Col. Let."),
                rownames = FALSE, options = list(dom = 't', ordering = FALSE,
                                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })

    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      listviewer::jsonedit(listdata = output_rv$val, mode = "view")
    })

    return(reactive({ output_rv$val }))
  })
}
