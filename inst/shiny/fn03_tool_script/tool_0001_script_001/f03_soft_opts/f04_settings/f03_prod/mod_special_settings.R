# ==============================================================================
# COORDINADOR: PACK SETTINGS (VARS + LEVELS) - RScience v.0.0.1
# ==============================================================================

mod_special_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),

    # Monitor de estado (Solo se ve si show_debug = TRUE)
    uiOutput(ns("global_status_tag")),
    uiOutput(ns("debug_container")),

    card(
      full_screen = TRUE,
      card_header(
        uiOutput(ns("master_title_ui"))
      ),
      navset_pill(
        id = ns("main_tabs"),
        nav_panel(
          title = uiOutput(ns("label_p1"), inline = TRUE),
          value = "panel_vars",
          # Contenedor dinámico para evitar errores si el source tarda
          uiOutput(ns("ui_step1"))
        ),
        nav_panel(
          title = uiOutput(ns("label_p2"), inline = TRUE),
          value = "panel_levels",
          uiOutput(ns("ui_step2"))
        )
      )
    )
  )
}

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

    # 2. INPUT UPDATES (ANTI-RESET & EMPTY START LOGIC)
    observe({
      df <- the_dataset()
      req(df); cols <- names(df); req(length(cols) > 0)

      n_digits <- nchar(length(cols))
      vector_pos_mod <- sprintf(paste0("%0", n_digits, "d"), 1:length(cols))
      vector_letter <- openxlsx::int2col(1:length(cols))
      max_width <- max(nchar(vector_letter))
      vector_letter_mod <- stringr::str_pad(vector_letter, width = max_width, side = "left")

      choices_list <- cols
      names(choices_list) <- paste0(vector_pos_mod, " - ", vector_letter_mod, " - ", cols)

      c_rv  <- isolate(input$var_rv)
      c_fac <- isolate(input$var_factor)

      if ((is.null(c_rv) || c_rv == "") && (is.null(c_fac) || c_fac == "")) {
        if (!is.null(input$var_rv)) {
          updateSelectizeInput(session, "var_rv", choices = choices_list, selected = character(0), server = TRUE)
          updateSelectizeInput(session, "var_factor", choices = choices_list, selected = character(0), server = TRUE)
        } else {
          session$onFlushed(function() {
            shinyjs::delay(100, {
              updateSelectizeInput(session, "var_rv", choices = choices_list, selected = character(0), server = TRUE)
              updateSelectizeInput(session, "var_factor", choices = choices_list, selected = character(0), server = TRUE)
            })
          }, once = TRUE)
        }
      }
    })

    # 3. STATISTICAL VALIDATION (CORREGIDO: Jerarquía de mensajes)
    check_analysis_validity <- reactive({
      v_rv  <- input$var_rv
      v_fac <- input$var_factor

      # A. CASO INICIAL: Si alguno está vacío, no es error, es espera.
      if (is.null(v_rv) || is.null(v_fac) || v_rv == "" || v_fac == "") {
        return(list(valid = FALSE, msg = "Waiting for User Selection", type = "waiting"))
      }

      # B. CASO DUPLICADO: Solo si ambos tienen texto y son iguales.
      if (v_rv == v_fac) {
        return(list(valid = FALSE, msg = "Error: Duplicate Variables Selected", type = "error"))
      }

      # C. CASO DATOS: Verificaciones técnicas sobre el dataset
      df_raw <- the_dataset()
      req(all(c(v_rv, v_fac) %in% names(df_raw)))

      mini_db <- df_raw[, c(v_rv, v_fac), drop = FALSE] %>% na.omit()
      if (nrow(mini_db) == 0) return(list(valid = FALSE, msg = "Error: Empty dataset (NAs)", type = "error"))
      if (!is.numeric(mini_db[[v_rv]])) return(list(valid = FALSE, msg = "Error: RV must be numeric", type = "error"))

      fac_vec <- as.factor(mini_db[[v_fac]])
      if (nlevels(fac_vec) < 2) return(list(valid = FALSE, msg = "Error: Factor needs >= 2 levels", type = "error"))

      return(list(valid = TRUE, msg = "Ready to Accept Selection", type = "success"))
    })

    # Botón dinámico Enable/Disable
    observe({
      valid <- isTRUE(check_analysis_validity()$valid)
      locked <- isTRUE(data_store$is_locked)
      if (valid && !locked) shinyjs::enable("btn_import") else shinyjs::disable("btn_import")
    })

    # 4. CLICK EVENTS
    observeEvent(input$btn_import, {
      req(check_analysis_validity()$valid)
      data_store$is_locked <- TRUE
      data_store$is_done <- TRUE
      data_store$time_stamp <- Sys.time()
      data_store$metadata <- list(
        rv = input$var_rv,
        factor = input$var_factor,
        alpha = as.numeric(input$alpha_value %||% 0.05),
        controls_passed = TRUE
      )
      output_rv$val <- reactiveValuesToList(data_store)
      shinyjs::addClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::disable("var_rv"); shinyjs::disable("var_factor"); shinyjs::disable("alpha_value")
    })

    observeEvent(input$btn_edit, {
      data_store$is_locked <- FALSE
      data_store$is_done   <- FALSE
      data_store$time_stamp <- Sys.time()
      output_rv$val <- list_default
      shinyjs::removeClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::enable("var_rv"); shinyjs::enable("var_factor"); shinyjs::enable("alpha_value")
    })

    observeEvent(input$btn_reset, {
      for (name in names(list_default)) { data_store[[name]] <- list_default[[name]] }
      data_store$time_stamp <- Sys.time()
      output_rv$val <- list_default
      updateSelectizeInput(session, "var_rv", selected = character(0))
      updateSelectizeInput(session, "var_factor", selected = character(0))
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

      # Determinamos clase e icono según el "type" de la validación
      if (locked) {
        cls <- "confirmed"; msg <- "Selection Locked"; ico <- "lock"
      } else if (check$type == "error") {
        cls <- "error-selection"; msg <- check$msg; ico <- "times-circle"
      } else if (check$type == "success") {
        cls <- "active-selection"; msg <- check$msg; ico <- "hand-point-up"
      } else {
        # El caso "waiting" cae aquí
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
