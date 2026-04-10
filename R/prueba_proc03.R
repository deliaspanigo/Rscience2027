mod_pipeline_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # --- BOTÓN DE ARRANQUE ---
    div(
      style = "margin-bottom: 20px; padding: 15px; background: #1a262f; border-radius: 8px; border: 1px dashed #00d4ff; text-align: center;",
      actionButton(
        ns("start_pipeline"),
        label = "LAUNCH RSPIPELINE ENGINE",
        icon = icon("rocket"),
        class = "btn-lg",
        style = "background: #00d4ff; color: #0b1218; font-weight: 800; border: none; box-shadow: 0 0 15px rgba(0, 212, 255, 0.4);"
      )
    ),

    uiOutput(ns("item01_folder_target")),
    uiOutput(ns("item02_folder_quarto_render")),
    uiOutput(ns("item03_qmd_files")),
    uiOutput(ns("item04_temp_folder_Rscience")),
    uiOutput(ns("item05_copy_files")),
    uiOutput(ns("item06_qmd_files_temp")),
    uiOutput(ns("item07_quarto_exec")) # Asegúrate de tener este output al final
  )
}


mod_pipeline_server <- function(id, folder_target, list_settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ==========================================================================
    # 0. CONTROL DE ESTADO DEL MOTOR
    # ==========================================================================
    # Usamos un flag para saber si el usuario ya dio clic
    engine <- reactiveValues(started = FALSE)

    observeEvent(input$start_pipeline, {
      engine$started <- TRUE
    })

    # Función auxiliar para mostrar el estado "Standby" (Gris/Inactivo)
    render_standby <- function(title) {
      div(
        style = "display: flex; align-items: center; padding: 12px; background: #1a262f;
                 border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px; opacity: 0.4;",
        div(style = "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background: #566b7a;"),
        div(
          style = "font-weight: 800; font-size: 0.75rem; color: #566b7a;",
          icon("lock", class = "me-2"),
          paste0(toupper(title), " - WAITING...")
        )
      )
    }

    # Normalización de la ruta de entrada
    internal_folder_target <- reactive({
      if(is.function(folder_target)) folder_target() else folder_target
    })

    # ==========================================================================
    # 1. REACTIVOS DE LÓGICA (Solo corren tras el clic)
    # ==========================================================================

    # Item 01 - Folder Target
    rlist_item01_folder_target <- reactive({
      req(engine$started)
      req(internal_folder_target())
      path_val <- internal_folder_target()
      exists_val <- dir.exists(path_val)
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c"
      list(path = path_val, exists = exists_val, text = if(exists_val) "FOLDER VERIFIED" else "PATH NOT FOUND",
           color = color_hex, icon_name = if(exists_val) "check-circle" else "exclamation-triangle",
           shadow = paste0("0 0 12px ", color_hex), is_done = exists_val)
    })

    # Item 02 - Quarto Proc Folder
    rlist_item02_quarto_proc <- reactive({
      req(rlist_item01_folder_target()$is_done)
      path_val <- internal_folder_target()
      path_folder_relative <- file.path(path_val, "f02_quarto_proc")
      path_folder_absolute <- normalizePath(path_folder_relative, mustWork = FALSE)
      exists_val <- dir.exists(path_folder_absolute)
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c"
      list(path = path_folder_absolute, exists = exists_val, text = if(exists_val) "FOLDER VERIFIED" else "PATH NOT FOUND",
           color = color_hex, icon_name = if(exists_val) "check-circle" else "exclamation-triangle",
           shadow = paste0("0 0 12px ", color_hex), is_done = exists_val)
    })

    # Item 03 - QMD Files Check
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
        item$status_color <- if(item$exists_local) "#00bc8c" else "#ff4b5c"
        return(item)
      })
      all_exist <- all(sapply(list_processed, function(x) x$exists_local))
      color_hex <- if(all_exist) "#00bc8c" else "#ff4b5c"
      list(is_done = all_exist, text = if(all_exist) "ALL RUNNERS VERIFIED" else "SOME RUNNERS MISSING",
           color = color_hex, icon_name = if(all_exist) "check-double" else "file-circle-exclamation",
           shadow = paste0("0 0 12px ", color_hex), list_qmd = list_processed)
    })

    # Item 04 - Temp Folder Creation
    rlist_item04_temp_folder_Rscience <- reactive({
      req(rlist_item03_qmd_files()$is_done)
      timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
      folder_name <- paste0("Rscience_", timestamp)
      full_path_temp <- file.path(tempdir(), folder_name)
      folder_created <- dir.create(full_path_temp, showWarnings = FALSE, recursive = TRUE)
      color_hex <- if(folder_created) "#00bc8c" else "#ff4b5c"
      list(path = full_path_temp, folder_name = folder_name, is_done = folder_created,
           text = if(folder_created) "TEMP REPOSITORY CREATED" else "FAILED",
           color = color_hex, icon_name = "folder-plus", shadow = paste0("0 0 12px ", color_hex))
    })

    # Item 05 - Copy Files (Sync)
    rlist_item05_copy_files <- reactive({
      req(rlist_item04_temp_folder_Rscience()$is_done)
      path_origin <- rlist_item02_quarto_proc()$path
      path_dest <- rlist_item04_temp_folder_Rscience()$path
      copy_status <- file.copy(from = path_origin, to = path_dest, recursive = TRUE, overwrite = TRUE)
      color_hex <- if(copy_status) "#00bc8c" else "#ff4b5c"
      list(origin = path_origin, dest = path_dest, is_done = copy_status,
           text = if(copy_status) "REPOSITORY CLONED" else "ERROR",
           color = color_hex, icon_name = "clone", shadow = paste0("0 0 12px ", color_hex))
    })

    # Item 06 - Verify Sandbox
    rlist_item06_qmd_files_temp <- reactive({
      req(rlist_item05_copy_files()$is_done)
      path_temp_base <- rlist_item05_copy_files()$dest
      folder_cloned <- basename(rlist_item02_quarto_proc()$path)
      list_qmd_origin <- rlist_item03_qmd_files()$list_qmd

      list_processed_temp <- lapply(names(list_qmd_origin), function(pkg_name) {
        item <- list_qmd_origin[[pkg_name]]
        new_path <- normalizePath(file.path(path_temp_base, folder_cloned, item$qmd_file_path_relative), mustWork = FALSE)
        list(qmd_file_path_abs_temp = new_path, exists_temp = file.exists(new_path),
             status_color = if(file.exists(new_path)) "#00bc8c" else "#ff4b5c")
      })
      names(list_processed_temp) <- names(list_qmd_origin)
      all_exist_temp <- all(sapply(list_processed_temp, function(x) x$exists_temp))
      color_hex <- if(all_exist_temp) "#00bc8c" else "#ff4b5c"
      list(details = list_processed_temp, is_done = all_exist_temp, text = if(all_exist_temp) "SANDBOX VERIFIED" else "CORRUPTED",
           color = color_hex, icon_name = "vial-circle-check", shadow = paste0("0 0 12px ", color_hex))
    })

    # ==========================================================================
    # 2. RENDERIZADO DE INTERFAZ (UI OUTPUTS)
    # ==========================================================================

    output$item01_folder_target <- renderUI({
      if(!engine$started) return(render_standby("Item 01 - Target Folder"))
      res <- rlist_item01_folder_target()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(style = "width: 100%; overflow: hidden;",
              div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(res$path, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #adb5bd; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;")))
    })

    output$item02_folder_quarto_render <- renderUI({
      if(!engine$started) return(render_standby("Item 02 - Quarto Render Folder"))
      req(rlist_item02_quarto_proc())
      res <- rlist_item02_quarto_proc()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(res$path, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #adb5bd;")))
    })

    output$item03_qmd_files <- renderUI({
      if(!engine$started) return(render_standby("Item 03 - Runner Verification"))
      req(rlist_item03_qmd_files())
      res <- rlist_item03_qmd_files()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text))
    })

    output$item04_temp_folder_Rscience <- renderUI({
      if(!engine$started) return(render_standby("Item 04 - Temp Environment"))
      req(rlist_item04_temp_folder_Rscience())
      res <- rlist_item04_temp_folder_Rscience()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(res$folder_name, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #00d4ff;")))
    })

    output$item05_copy_files <- renderUI({
      if(!engine$started) return(render_standby("Item 05 - File System Sync"))
      req(rlist_item05_copy_files())
      res <- rlist_item05_copy_files()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(style = "font-size: 0.65rem; color: #adb5bd;", "SYNC COMPLETE FROM LOCAL TO TEMP")))
    })

    output$item06_qmd_files_temp <- renderUI({
      if(!engine$started) return(render_standby("Item 06 - Sandbox Integrity"))
      req(rlist_item06_qmd_files_temp())
      res <- rlist_item06_qmd_files_temp()
      div(style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = "display: flex; align-items: center; margin-bottom: 8px;",
              div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
              span(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text)),
          div(style = "display: flex; gap: 10px; background: #0b1218; padding: 5px; border-radius: 4px;",
              lapply(names(res$details), function(p) div(style="font-size: 0.6rem; color: #adb5bd;", icon("file-code"), p))))
    })

    # ==========================================================================
    # 3. EJECUCIÓN FINAL (QUARTO ENGINE)
    # ==========================================================================
    render_status <- reactiveValues(pack01 = "pending", pack02 = "pending", pack03 = "pending", pack04 = "pending")

    observeEvent(rlist_item06_qmd_files_temp()$is_done, {
      req(rlist_item06_qmd_files_temp()$is_done)
      details <- rlist_item06_qmd_files_temp()$details
      for(pkg_name in names(details)) {
        render_status[[pkg_name]] <- "processing"



        tryCatch({
          quarto::quarto_render(input = details[[pkg_name]]$qmd_file_path_abs_temp)
          render_status[[pkg_name]] <- "done"
        }, error = function(e) { render_status[[pkg_name]] <- "error" })
        # invalidateLater(2000, session)
      }
    })

    output$item07_quarto_exec <- renderUI({
      if(!engine$started) return(render_standby("Item 07 - Cloud Engine"))
      get_color <- function(s) switch(s, "pending"="#566b7a", "processing"="#00d4ff", "done"="#00bc8c", "error"="#ff4b5c")
      div(style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px;",
          div(style = "margin-bottom: 10px; font-weight: 800; font-size: 0.75rem; color: #adb5bd;", icon("microchip"), " ITEM 07 - EXECUTION"),
          div(style = "display: grid; gap: 5px;",
              lapply(names(render_status), function(pkg) {
                s <- render_status[[pkg]]
                c <- get_color(s)
                div(style = paste0("display: flex; align-items: center; padding: 5px 10px; background: #0b1218; border-left: 3px solid ", c),
                    div(style = paste0("width: 8px; height: 8px; border-radius: 50%; margin-right: 10px; background:", c)),
                    span(pkg, style = "font-family: 'JetBrains Mono'; font-size: 0.65rem; color: #fff; flex-grow: 1;"),
                    span(toupper(s), style = paste0("font-size: 0.6rem; font-weight: bold; color: ", c)))
              })))
    })
  })
}
# ==============================================================================
# APP DE PRUEBA (SOLO PARA TEST)
# ==============================================================================

folder_target <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002", package = "Rscience2027")


ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  mod_pipeline_ui("pipeline_1")
)
server <- function(input, output, session) {
  mod_pipeline_server("pipeline_1", folder_target = folder_target, list_settings = NULL)
}
shinyApp(ui, server)
