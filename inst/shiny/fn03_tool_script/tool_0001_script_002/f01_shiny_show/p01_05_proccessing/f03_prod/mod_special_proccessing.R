mod_special_proccessing_ui <- function(id) {
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
    uiOutput(ns("item05_quarto_exec")) # Ahora el antiguo 07 es el 05
  )
}


mod_special_proccessing_server <- function(id, local_folder_tool_script, temp_folder_tool_script, list_settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_local_folder_tool_script <- reactive(if(is.function(local_folder_tool_script)) local_folder_tool_script() else local_folder_tool_script)
    internal_temp_folder_tool_script  <- reactive(if(is.function(temp_folder_tool_script)) temp_folder_tool_script() else temp_folder_tool_script)


    # --------------------------------------------------------------------------
    # 0. CONTROL DE ESTADO
    # --------------------------------------------------------------------------
    engine <- reactiveValues(started = FALSE)
    render_status <- reactiveValues()
    current_idx <- reactiveVal(0)
    current_pos_render <- reactiveVal(0)

    observeEvent(input$start_pipeline, {
      engine$started <- TRUE
    })

    render_standby <- function(title) {
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px; opacity: 0.4;",
          div(style = "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background: #566b7a;"),
          div(style = "font-weight: 800; font-size: 0.75rem; color: #566b7a;", icon("lock", class = "me-2"), paste0(toupper(title), " - WAITING...")))
    }

    # --------------------------------------------------------------------------
    # 1. LÓGICA REACTIVA (Items 01 a 04)
    # --------------------------------------------------------------------------

    rlist_item01_local_folder_tool_script <- reactive({
      req(engine$started)
      path_val <- internal_local_folder_tool_script()
      exists_val <- dir.exists(path_val)
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c"
      list(path = path_val, is_done = exists_val, text = if(exists_val) "FOLDER VERIFIED" else "PATH NOT FOUND",
           color = color_hex, icon_name = if(exists_val) "check-circle" else "exclamation-triangle",
           shadow = paste0("0 0 12px ", color_hex))
    }) %>% debounce(1000)

    rlist_item02_temp_folder_tool_script <- reactive({
      req(rlist_item01_local_folder_tool_script()$is_done)
      path_val <- if(is.function(internal_temp_folder_tool_script)) internal_temp_folder_tool_script() else internal_temp_folder_tool_script
      exists_val <- dir.exists(path_val)
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c"
      list(path = path_val, is_done = exists_val, text = if(exists_val) "TEMP VERIFIED" else "TEMP NOT FOUND",
           color = color_hex, icon_name = "microchip", shadow = paste0("0 0 12px ", color_hex))
    }) %>% debounce(1000)

    rlist_item03_quarto_proc <- reactive({
      req(rlist_item02_temp_folder_tool_script()$is_done)
      path_val <- rlist_item02_temp_folder_tool_script()$path
      path_folder_absolute <- normalizePath(file.path(path_val, "f02_quarto_proc"), mustWork = FALSE)
      exists_val <- dir.exists(path_folder_absolute)
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c"
      list(path = path_folder_absolute, is_done = exists_val, text = if(exists_val) "STRUCTURE OK" else "STRUCTURE MISSING",
           color = color_hex, icon_name = "folder-tree", shadow = paste0("0 0 12px ", color_hex))
    }) %>% debounce(1000)

    rlist_item04_qmd_files <- reactive({
      req(rlist_item03_quarto_proc()$is_done)
      path_val <- rlist_item03_quarto_proc()$path
      list_render_qmd_file <- list(
        "pack01" = list(qmd_file_path_relative = "g01_quarto_original/AAA_01_RUNNER_g01_quarto_original.qmd",
                        label_on_rendering = "Applying setting on R scripts."),
        "pack02" = list(qmd_file_path_relative = "g02_quarto_mod/AAA_01_RUNNER_g02_quarto_mod.qmd",
                        label_on_rendering = "Running R script"),
        "pack03" = list(qmd_file_path_relative = "g04_script_external/AAA_01_RUNNER_g04_script_external.qmd",
                        label_on_rendering = "Packaging R scripts for user (.R - zip)."),
        "pack04" = list(qmd_file_path_relative = "g05_shiny_output/AAA_01_RUNNER_g05_shiny_output.qmd",
                        label_on_rendering = "View - Shiny Outputs"),
        "pack05" = list(qmd_file_path_relative = "g06_asa/AAA_01_RUNNER_g06_asa.qmd",
                        label_on_rendering = "View - Automatic Statistic Asesor (ASA)")
      )
      list_processed <- lapply(list_render_qmd_file, function(item) {
        item$qmd_file_path_abs_local <- normalizePath(file.path(path_val, item$qmd_file_path_relative), mustWork = FALSE)
        item$exists_local <- file.exists(item$qmd_file_path_abs_local)
        return(item)
      })
      all_exist <- all(sapply(list_processed, function(x) x$exists_local))
      color_hex <- if(all_exist) "#00bc8c" else "#f39c12"
      list(is_done = all_exist, text = if(all_exist) "ALL RUNNERS READY" else "SOME RUNNERS MISSING",
           color = color_hex, icon_name = "check-double", shadow = paste0("0 0 12px ", color_hex),
           list_qmd = list_processed)
    }) %>% debounce(1000)

    rlist_item05_proccessing <- reactive({

      req(rlist_item03_quarto_proc()$is_done)
      flat_rlist_item03_quarto_proc <- rlist_item03_quarto_proc()
      flat_rlist_item03_quarto_proc$is_done

    }) %>% debounce(1000)
    # --------------------------------------------------------------------------
    # 2. MOTOR DINÁMICO (Anterior 07, ahora vinculado al 04)
    # --------------------------------------------------------------------------
    observeEvent(rlist_item04_qmd_files()$is_done, {
      req(rlist_item04_qmd_files()$is_done)
      pkgs <- names(rlist_item04_qmd_files()$list_qmd)
      for(p in pkgs) {
        render_status[[p]] <- "pending"
      }
      current_idx(1)
    })

    # delay_current_idx <- reactive({
    #
    #   current_idx()
    # }) %>% debounce(1000)

    pack_current_pos_render <- reactive({
      req(engine$started, rlist_item04_qmd_files()$is_done)
      idx <- current_idx()
      details <- rlist_item04_qmd_files()$list_qmd
      pkg_names <- names(details)

      if(idx == 0 || idx > length(pkg_names)) return(NULL)

      pkg_name <- pkg_names[idx]

      # Cambiamos a processing AQUÍ para que se vea durante el debounce
      render_status[[pkg_name]] <- "processing"

      list(
        idx = idx,
        pkg_name = pkg_name,
        path = details[[pkg_name]]$qmd_file_path_abs_local
      )
    }) %>% debounce(1000) # El delay que te gusta

    observe({
      # Extraemos la info del reactive debounced
      flat_pack <- pack_current_pos_render()
      req(flat_pack) # Evita que corra si es NULL

      selected_pkg_name <- flat_pack$pkg_name
      selected_idx <- flat_pack$idx
      selected_path <- flat_pack$path

      isolate({
        tryCatch({
          selected_folder_path <- dirname(selected_path)
          selected_qmd_file_name <- basename(selected_path)

          old_wd <- getwd()
          new_wd <- selected_folder_path

          setwd(new_wd)
          quarto::quarto_render(input = selected_qmd_file_name, quiet = FALSE)
          setwd(old_wd)

          # quarto::quarto_render(input = selected_path, quiet = TRUE)
          render_status[[selected_pkg_name]] <- "done"
        }, error = function(e) {
          render_status[[selected_pkg_name]] <- "error"
        })
        # Avanzamos el índice
        current_idx(selected_idx + 1)
      })
    })

    # --------------------------------------------------------------------------
    # 3. RENDERS UI
    # --------------------------------------------------------------------------

    output$item01_folder_target <- renderUI({
      if(!engine$started) return(render_standby("Item 01 - Target Folder"))
      res <- rlist_item01_local_folder_tool_script()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(style = "width: 100%; overflow: hidden;",
              div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(res$path, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #adb5bd; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;")))
    })

    output$item02_folder_quarto_render <- renderUI({
      if(!rlist_item01_local_folder_tool_script()$is_done) return(render_standby("Item 02 - Temp Folder"))
      res <- rlist_item02_temp_folder_tool_script()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(style = "width: 100%; overflow: hidden;",
              div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(res$path, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #adb5bd; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;")))
    })

    output$item03_qmd_files <- renderUI({
      if(!rlist_item02_temp_folder_tool_script()$is_done) return(render_standby("Item 03 - Quarto Structure"))
      res <- rlist_item03_quarto_proc()
      div(style = "display: flex; align-items: center; padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
          div(style = "width: 100%; overflow: hidden;",
              div(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text),
              div(res$path, style = "font-family: 'JetBrains Mono'; font-size: 0.72rem; color: #adb5bd; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;")))
    })

    #-----------------------------------------------------------------------------------------------

    output$item04_temp_folder_Rscience <- renderUI({
      if(!rlist_item03_quarto_proc()$is_done) return(render_standby("Item 04 - QMD Verification"))
      res <- rlist_item04_qmd_files()
      div(style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",
          div(style = "display: flex; align-items: center; margin-bottom: 8px;",
              div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px; background:", res$color, "; box-shadow:", res$shadow)),
              span(style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color), icon(res$icon_name, class="me-2"), res$text)),
          div(style = "display: flex; gap: 10px; background: #0b1218; padding: 8px; border-radius: 4px; overflow-x: auto;",
              lapply(names(res$list_qmd), function(p) {
                file_exists <- res$list_qmd[[p]]$exists_local
                status_color <- if(file_exists) "#00bc8c" else "#ff4b5c"
                status_icon <- if(file_exists) "file-circle-check" else "file-circle-xmark"
                div(style = paste0("font-size: 0.65rem; color: ", status_color, "; white-space: nowrap; padding: 2px 6px; border: 1px solid ", status_color, "44; border-radius: 3px; background: ", status_color, "11;"),
                    icon(status_icon),
                    p)
              })))
    })
    #-----------------------------------------------------------------------------------------------

    # ITEM 05: Log de Ejecución (Anterior 07)
    # UI de la fila individual (sin renderUI, solo el contenedor)
    render_file_row_ui <- function(id_row) {
      uiOutput(id_row)
    }

    # Lógica para renderizar solo una fila
    render_file_row_server <- function(pkg_name, s) {
      get_conf <- function(st) {
        switch(st,
               "pending"    = list(col = "#566b7a", icon = "hourglass", class = ""),
               "processing" = list(col = "#00d4ff", icon = "spinner", class = "processing-pulse"),
               "done"       = list(col = "#00bc8c", icon = "check-double", class = ""),
               "error"      = list(col = "#ff4b5c", icon = "times-circle", class = ""))
      }

      conf <- get_conf(s)

      div(class = conf$class,
          style = paste0("display: flex; align-items: center; padding: 8px 12px; background: #0b1218; border-left: 3px solid ", conf$col, "; border-radius: 4px; transition: all 0.3s; margin-bottom: 6px;"),
          div(style = paste0("width: 8px; height: 8px; border-radius: 50%; margin-right: 12px; background:", conf$col, "; box-shadow: 0 0 8px ", conf$col)),
          span(pkg_name, style = "font-family: 'JetBrains Mono'; font-size: 0.68rem; color: #fff; flex-grow: 1;"),
          span(toupper(s), style = paste0("font-size: 0.6rem; font-weight: 900; color: ", conf$col, "; margin-right: 10px;")),
          icon(conf$icon, class = if(s == "processing") "fa-spin" else NULL, style = paste0("color: ", conf$col))
      )
    }


    # --- DENTRO DE mod_pipeline_server ---

    # 1. El contenedor principal ahora es casi estático
    # --- ITEM 05: Log de Ejecución con Efecto Neón Potenciado ---
    output$item05_quarto_exec <- renderUI({
      if(!rlist_item04_qmd_files()$is_done) return(render_standby("Item 05 - Engine Status"))

      pkgs <- names(rlist_item04_qmd_files()$list_qmd)

      tags$div(
        tags$style("
          @keyframes pulse-border {
            0% {
              box-shadow: 0 0 0 0px rgba(0, 212, 255, 0.7);
              border-color: #00d4ff;
            }
            50% {
              box-shadow: 0 0 25px 10px rgba(0, 212, 255, 0.5);
              border-color: #00fbff;
            }
            100% {
              box-shadow: 0 0 40px 20px rgba(0, 212, 255, 0);
              border-color: #00d4ff;
            }
          }
          .processing-pulse {
            animation: pulse-border 2s infinite cubic-bezier(0.4, 0, 0.6, 1);
            border: 2px solid #00d4ff !important;
            background: #1c2d3a !important;
            z-index: 10;
            position: relative;
          }
        "),
        div(style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px;",
            div(style = "margin-bottom: 15px; font-weight: 800; font-size: 0.75rem; color: #adb5bd;", icon("microchip"), " ITEM 05 - ENGINE STATUS"),
            div(id = ns("rows_container"),
                lapply(pkgs, function(p) uiOutput(ns(paste0("row_", p))))
            )
        )
      )
    })

    # 2. Creamos los renders individuales dinámicamente
    observe({
      req(rlist_item04_qmd_files()$is_done)
      flat_rlist_item04_qmd_files <- rlist_item04_qmd_files()
      pkgs <- names(flat_rlist_item04_qmd_files$list_qmd)

      for(p in pkgs) {
        # Localizamos p para el scope del render
        local({
          pkg_id <- p
          label_id <- flat_rlist_item04_qmd_files$list_qmd[[pkg_id]]$label_on_rendering

          output[[paste0("row_", pkg_id)]] <- renderUI({
            # ESTA ES LA MAGIA: Solo este render se dispara cuando render_status[[pkg_id]] cambia
            #render_file_row_server(pkg_id, render_status[[pkg_id]])
            render_file_row_server(label_id, render_status[[pkg_id]])
          })
        })
      }
    })
  })
}

# ==============================================================================
# APP DE PRUEBA (SOLO PARA TEST)
# ==============================================================================

# ==============================================================================
# APP DE PRUEBA (SOLO PARA TEST)
# ==============================================================================
#
# library(bslib)
# library(shiny)
# library(tidyverse)
#
# # Asumo que esta ruta existe y contiene la estructura necesaria
# path_test <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002", package = "Rscience2027")
#
# ui <- page_fluid(
#   theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
#   mod_pipeline_ui("pipeline_1")
# )
#
# server <- function(input, output, session) {
#   # CORRECCIÓN AQUÍ: Usar los nombres de argumentos definidos en el módulo
#   mod_pipeline_server(
#     id = "pipeline_1",
#     local_folder_tool_script = path_test,
#     temp_folder_tool_script = path_test, # O la ruta que desees para el proceso
#     list_settings = NULL
#   )
# }
#
# shinyApp(ui, server)
