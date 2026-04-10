# --- UI CRONÓMETRO ---
mod_timer_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("timer_display"))
}

# --- SERVER CRONÓMETRO ---
mod_timer_server <- function(id, active = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    timer_val <- reactiveVal(0)

    # Reiniciar si el motor se activa de nuevo
    observeEvent(active(), {
      if (active()) timer_val(0)
    })

    # El corazón del cronómetro
    observe({
      if (active()) {
        invalidateLater(1000, session)
        isolate(timer_val(timer_val() + 1))
      }
    })

    output$timer_display <- renderUI({
      req(active() || timer_val() > 0)
      t <- timer_val()
      mins <- t %/% 60
      secs <- t %% 60
      time_str <- sprintf("%02d:%02d", mins, secs)

      div(
        style = "padding: 10px 20px; background: #0b1218; border: 1px solid #00d4ff;
                 border-radius: 6px; font-family: 'JetBrains Mono', monospace;
                 color: #00d4ff; font-weight: 800; font-size: 1.2rem;
                 box-shadow: inset 0 0 10px rgba(0, 212, 255, 0.2);",
        icon("clock", style = "margin-right: 10px; opacity: 0.7;"),
        time_str
      )
    })
    return(timer_val)
  })
}




# --- UI PRINCIPAL ---
mod_pipeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px; padding: 15px; background: #1a262f; border-radius: 8px;
               border: 1px dashed #00d4ff; display: flex; align-items: center; justify-content: center; gap: 20px;",
      actionButton(
        ns("start_pipeline"),
        label = "LAUNCH RSPIPELINE ENGINE",
        icon = icon("rocket"),
        class = "btn-lg",
        style = "background: #00d4ff; color: #0b1218; font-weight: 800; border: none; box-shadow: 0 0 15px rgba(0, 212, 255, 0.4);"
      ),
      mod_timer_ui(ns("timer_engine"))
    ),
    uiOutput(ns("item01_folder_target")),
    uiOutput(ns("item02_folder_quarto_render")),
    uiOutput(ns("item03_qmd_files")),
    uiOutput(ns("item04_temp_folder_Rscience")),
    uiOutput(ns("item05_copy_files")),
    uiOutput(ns("item06_qmd_files_temp")),
    uiOutput(ns("item07_quarto_exec"))
  )
}

# --- SERVER PRINCIPAL ---
mod_pipeline_server <- function(id, folder_target, list_settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --------------------------------------------------------------------------
    # 0. ESTADOS Y CONTROL
    # --------------------------------------------------------------------------
    engine <- reactiveValues(started = FALSE, running = FALSE)
    render_status <- reactiveValues()
    current_idx <- reactiveVal(0)

    # Iniciar Cronómetro subordinado
    mod_timer_server("timer_engine", active = reactive({ engine$running }))

    observeEvent(input$start_pipeline, {
      engine$started <- TRUE
      engine$running <- TRUE
    })

    render_standby <- function(title) {
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px; opacity: 0.4;",
          div(style = "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background: #566b7a;"),
          div(style = "font-weight: 800; font-size: 0.75rem; color: #566b7a;", icon("lock", class = "me-2"), paste0(toupper(title), " - WAITING...")))
    }

    internal_folder_target <- reactive({
      if(is.function(folder_target)) folder_target() else folder_target
    })

    # --------------------------------------------------------------------------
    # 1. LÓGICA DE VALIDACIÓN (ITEMS 01-06)
    # --------------------------------------------------------------------------

    rlist_item01_folder_target <- reactive({
      req(engine$started); path_val <- internal_folder_target()
      exists_val <- dir.exists(path_val)
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c"
      list(path = path_val, is_done = exists_val, text = if(exists_val) "FOLDER VERIFIED" else "PATH NOT FOUND",
           color = color_hex, icon_name = if(exists_val) "check-circle" else "exclamation-triangle", shadow = paste0("0 0 12px ", color_hex))
    }) %>% debounce(1000)

    rlist_item02_quarto_proc <- reactive({
      req(rlist_item01_folder_target()$is_done)
      path_val <- internal_folder_target()
      path_folder_absolute <- normalizePath(file.path(path_val, "f02_quarto_proc"), mustWork = FALSE)
      exists_val <- dir.exists(path_folder_absolute)
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c"
      list(path = path_folder_absolute, is_done = exists_val, text = if(exists_val) "FOLDER VERIFIED" else "PATH NOT FOUND",
           color = color_hex, icon_name = if(exists_val) "check-circle" else "exclamation-triangle", shadow = paste0("0 0 12px ", color_hex))
    }) %>% debounce(1000)

    rlist_item03_qmd_files <- reactive({
      req(rlist_item02_quarto_proc()$is_done)
      path_val <- rlist_item02_quarto_proc()$path
      list_render_qmd_file <- list(
        "pack01" = list(qmd_file_path_relative = "g02_quarto_mod/AAA_01_RUNNER_g02_quarto_mod.qmd"),
        "pack02" = list(qmd_file_path_relative = "g04_script_external/AAA_01_RUNNER_g04_script_external.qmd"),
        "pack03" = list(qmd_file_path_relative = "g05_shiny_output/AAA_01_RUNNER_g05_shiny_output.qmd"),
        "pack04" = list(qmd_file_path_relative = "g06_asa/AAA_01_RUNNER_g06_asa.qmd")
      )
      list_processed <- lapply(list_render_qmd_file, function(item) {
        item$qmd_file_path_abs_local <- normalizePath(file.path(path_val, item$qmd_file_path_relative), mustWork = FALSE)
        item$exists_local <- file.exists(item$qmd_file_path_abs_local)
        return(item)
      })
      all_exist <- all(sapply(list_processed, function(x) x$exists_local))
      color_hex <- if(all_exist) "#00bc8c" else "#ff4b5c"
      list(is_done = all_exist, text = if(all_exist) "ALL RUNNERS VERIFIED" else "SOME RUNNERS MISSING",
           color = color_hex, icon_name = if(all_exist) "check-double" else "file-circle-exclamation",
           shadow = paste0("0 0 12px ", color_hex), list_qmd = list_processed)
    }) %>% debounce(1000)

    rlist_item04_temp_folder_Rscience <- reactive({
      req(rlist_item03_qmd_files()$is_done)
      timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
      full_path_temp <- file.path(tempdir(), paste0("Rscience_", timestamp))
      folder_created <- dir.create(full_path_temp, showWarnings = FALSE, recursive = TRUE)
      color_hex <- if(folder_created) "#00bc8c" else "#ff4b5c"
      list(path = full_path_temp, folder_name = basename(full_path_temp), is_done = folder_created,
           text = if(folder_created) "TEMP REPOSITORY CREATED" else "FAILED",
           color = color_hex, icon_name = "folder-plus", shadow = paste0("0 0 12px ", color_hex))
    }) %>% debounce(1000)

    rlist_item05_copy_files <- reactive({
      req(rlist_item04_temp_folder_Rscience()$is_done)
      path_origin <- rlist_item02_quarto_proc()$path
      path_dest <- rlist_item04_temp_folder_Rscience()$path
      copy_status <- file.copy(from = path_origin, to = path_dest, recursive = TRUE, overwrite = TRUE)
      color_hex <- if(copy_status) "#00bc8c" else "#ff4b5c"
      list(is_done = copy_status, text = if(copy_status) "REPOSITORY CLONED" else "ERROR",
           color = color_hex, icon_name = "clone", shadow = paste0("0 0 12px ", color_hex), dest = path_dest)
    }) %>% debounce(1000)

    rlist_item06_qmd_files_temp <- reactive({
      req(rlist_item05_copy_files()$is_done)
      path_temp_base <- rlist_item05_copy_files()$dest
      folder_cloned <- basename(rlist_item02_quarto_proc()$path)
      list_qmd_origin <- rlist_item03_qmd_files()$list_qmd

      list_processed_temp <- lapply(names(list_qmd_origin), function(pkg_name) {
        item <- list_qmd_origin[[pkg_name]]
        new_path <- normalizePath(file.path(path_temp_base, folder_cloned, item$qmd_file_path_relative), mustWork = FALSE)
        list(qmd_file_path_abs_temp = new_path, exists_temp = file.exists(new_path))
      })
      names(list_processed_temp) <- names(list_qmd_origin)
      all_exist_temp <- all(sapply(list_processed_temp, function(x) x$exists_temp))
      color_hex <- if(all_exist_temp) "#00bc8c" else "#ff4b5c"
      list(details = list_processed_temp, is_done = all_exist_temp, text = if(all_exist_temp) "SANDBOX VERIFIED" else "CORRUPTED",
           color = color_hex, icon_name = "vial-circle-check", shadow = paste0("0 0 12px ", color_hex))
    }) %>% debounce(1000)

    # --------------------------------------------------------------------------
    # 2. MOTOR DE EJECUCIÓN (ITEM 07)
    # --------------------------------------------------------------------------

    observeEvent(rlist_item06_qmd_files_temp()$is_done, {
      req(rlist_item06_qmd_files_temp()$is_done)
      pkgs <- names(rlist_item06_qmd_files_temp()$details)
      for(p in pkgs) { render_status[[p]] <- "pending" }
      current_idx(1)
    })

    engine_heartbeat <- reactive({ current_idx() }) %>% debounce(1000)

    observe({
      req(engine$started, rlist_item06_qmd_files_temp()$is_done)
      idx <- engine_heartbeat()
      details <- rlist_item06_qmd_files_temp()$details
      pkg_names <- names(details)
      if(idx == 0) return()

      # Final de la tubería
      if(idx > length(pkg_names)) {
        engine$running <- FALSE
        return()
      }

      pkg_name <- pkg_names[idx]
      isolate({
        render_status[[pkg_name]] <- "processing"
        tryCatch({
          quarto::quarto_render(input = details[[pkg_name]]$qmd_file_path_abs_temp, quiet = TRUE)
          render_status[[pkg_name]] <- "done"
        }, error = function(e) { render_status[[pkg_name]] <- "error" })
        current_idx(idx + 1)
      })
    })

    # --------------------------------------------------------------------------
    # 3. RENDERIZADOS DE UI
    # --------------------------------------------------------------------------

    # Helper para renderizar items 01-05 de forma consistente
    render_item_ui <- function(res) {
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(style = "width: 100%; overflow: hidden;",
              div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              if(!is.null(res$path)) div(res$path, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #adb5bd; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;") else tags$span()
          ))
    }

    output$item01_folder_target <- renderUI({
      if(!engine$started) return(render_standby("Item 01 - Target Folder"))
      render_item_ui(rlist_item01_folder_target())
    })

    output$item02_folder_quarto_render <- renderUI({
      if(!engine$started) return(render_standby("Item 02 - Quarto Folder"))
      req(rlist_item02_quarto_proc())
      render_item_ui(rlist_item02_quarto_proc())
    })

    output$item03_qmd_files <- renderUI({
      if(!engine$started) return(render_standby("Item 03 - Runner Verification"))
      req(rlist_item03_qmd_files())
      render_item_ui(rlist_item03_qmd_files())
    })

    output$item04_temp_folder_Rscience <- renderUI({
      if(!engine$started) return(render_standby("Item 04 - Temp Environment"))
      req(rlist_item04_temp_folder_Rscience()); res <- rlist_item04_temp_folder_Rscience()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(res$folder_name, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #00d4ff;")))
    })

    output$item05_copy_files <- renderUI({
      if(!engine$started) return(render_standby("Item 05 - Sync"))
      req(rlist_item05_copy_files())
      render_item_ui(rlist_item05_copy_files())
    })

    output$item06_qmd_files_temp <- renderUI({
      if(!engine$started) return(render_standby("Item 06 - Sandbox"))
      req(rlist_item06_qmd_files_temp()); res <- rlist_item06_qmd_files_temp()
      div(style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = "display: flex; align-items: center; margin-bottom: 8px;",
              div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
              span(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text)),
          div(style = "display: flex; gap: 10px; background: #0b1218; padding: 5px; border-radius: 4px;",
              lapply(names(res$details), function(p) div(style="font-size: 0.6rem; color: #adb5bd;", icon("file-code"), p))))
    })

    output$item07_quarto_exec <- renderUI({
      if(!engine$started) return(render_standby("Item 07 - Execution Log"))
      get_conf <- function(s) {
        switch(s,
               "pending"    = list(col = "#566b7a", icon = "hourglass"),
               "processing" = list(col = "#00d4ff", icon = "spinner fa-spin", class = "processing-pulse"),
               "done"       = list(col = "#00bc8c", icon = "check-double"),
               "error"      = list(col = "#ff4b5c", icon = "times-circle"))
      }
      tags$div(
        tags$style("@keyframes pulse-border {0% {box-shadow: 0 0 0 0px rgba(0, 212, 255, 0.4);} 70% {box-shadow: 0 0 0 10px rgba(0,212,255,0);} 100% {box-shadow: 0 0 0 0px rgba(0,212,255,0);}}
                    .processing-pulse {animation: pulse-border 1.5s infinite; border: 1px solid #00d4ff !important; background: #1c2d3a !important;}"),
        div(style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px;",
            div(style = "margin-bottom: 10px; font-weight: 800; font-size: 0.75rem; color: #adb5bd;", icon("microchip"), " ITEM 07 - ENGINE STATUS"),
            div(style = "display: grid; gap: 6px;",
                lapply(names(render_status), function(pkg) {
                  s <- render_status[[pkg]]
                  conf <- get_conf(s)
                  div(class = if(!is.null(conf$class)) conf$class else "",
                      style = paste0("display: flex; align-items: center; padding: 8px 12px; background: #0b1218; border-left: 3px solid ", conf$col, "; border-radius: 4px; transition: all 0.3s;"),
                      div(style = paste0("width: 8px; height: 8px; border-radius: 50%; margin-right: 12px; background:", conf$col, "; box-shadow: 0 0 8px ", conf$col)),
                      span(pkg, style = "font-family: 'JetBrains Mono'; font-size: 0.68rem; color: #fff; flex-grow: 1;"),
                      span(toupper(s), style = paste0("font-size: 0.6rem; font-weight: 900; color: ", conf$col, "; margin-right: 10px;")),
                      icon(conf$icon, style = paste0("color: ", conf$col))
                  )
                })))
      )
    })
  })
}
