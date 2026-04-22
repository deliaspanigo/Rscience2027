library(future)
library(promises)

# Configuración necesaria (esto va fuera del server o al inicio)
plan(multisession)


mod_special_proccessing_DEBUG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Este uiOutput cargará todo lo que definiste en output$show_debug_external
    uiOutput(ns("debug_external"))
  )
}

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

    # DOS COLUMNAS
    bslib::layout_columns(
      col_widths = c(8, 4),  # 4 columnas para la izquierda, 8 para la derecha (total 12)



      # COLUMNA DERECHA (item 5)
      div(
        style = "height: 100%;",
        uiOutput(ns("item05_quarto_exec"))
      ),
      # COLUMNA IZQUIERDA (items 1-4)
      div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        uiOutput(ns("item01_folder_target")),
        uiOutput(ns("item02_folder_quarto_render")),
        uiOutput(ns("item03_qmd_files")),
        uiOutput(ns("item04_temp_folder_Rscience"))
      )
    ),

    uiOutput(ns("debug_internal_ui"))
  )
}

mod_special_proccessing_server <- function(id, local_folder_tool_script, temp_folder_tool_script, list_quarto_replacement, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message(">>> [MODULO] Inicializado con ID: ", id, " en ", Sys.time())

    # --- 1. ESTADO CENTRALIZADO ---
    state <- reactiveValues(
      engine_started = FALSE,
      super_done = FALSE,
      current_task = NULL
    )

    # Estado individual para cada tarea
    task_status <- reactiveValues()
    task_labels <- reactiveValues()

    # Lock para evitar ejecuciones múltiples
    execution_lock <- FALSE

    # --- CONTROL DE POSICIÓN CON TRES DEBOUNCE ---
    current_index <- reactiveVal(0)

    # TIMER 1: Solo para mostrar "processing" en UI (delay ANTES de mostrar)
    task_ui_trigger <- reactive({
      idx <- current_index()
      if(idx > 0) {
        message(">>> [TIMER-1-UI] Delay 0.3s para mostrar 'processing' en tarea ", idx)
      }
      idx
    }) %>% debounce(300)

    # TIMER 2: Para iniciar el procesamiento REAL (delay después del UI)
    task_process_trigger <- reactiveVal(0)
    task_process <- reactive({
      val <- task_process_trigger()
      if(val > 0) {
        message(">>> [TIMER-2-PROCESS] Delay 0.2s para iniciar procesamiento REAL")
      }
      val
    }) %>% debounce(200)

    # TIMER 3: Delay DESPUÉS de completar el procesamiento
    task_complete_trigger <- reactiveVal(0)
    task_complete <- reactive({
      val <- task_complete_trigger()
      if(val > 0) {
        message(">>> [TIMER-3-COMPLETE] Delay 0.5s DESPUÉS de completar")
      }
      val
    }) %>% debounce(500)

    # --- 2. ENTRADAS ---
    inputs_bundle <- reactive({
      loc <- if(is.function(local_folder_tool_script)) local_folder_tool_script() else local_folder_tool_script
      temp <- if(is.function(temp_folder_tool_script)) temp_folder_tool_script() else temp_folder_tool_script
      repl <- if(is.function(list_quarto_replacement)) list_quarto_replacement() else list_quarto_replacement
      req(loc, temp, repl)
      list(local_path = loc, temp_path = temp, replacement = repl)
    }) %>% debounce(500)

    # Metadata de tareas
    tasks_metadata <- reactive({
      req(inputs_bundle())

      base_path  <- normalizePath(file.path(inputs_bundle()$temp_path, "f02_quarto_proc"), mustWork = FALSE)

      list(
        "pack01" = list(rel = "g01_quarto_original/AAA_02_STONE_01_copying_files.qmd",
                        label = "📁 Copying files...",
                        format = "html"),
        "pack02" = list(rel = "g01_quarto_original/AAA_02_STONE_02_modying_files.qmd",
                        label = "⚙️ Applying settings...",
                        format = "html"),
        ####################################################################################
        "pack03" = list(rel = "f02_quarto_mod/file01_anova_import_and_control.qmd",
                        label = "⚙️ Import and Control...",
                        format = "html"),
        "pack04" = list(rel = "f02_quarto_mod/file02_anova_full_test.qmd",
                        label = "⚙️ Full Test...",
                        format = "html"),
        "pack05" = list(rel = "f02_quarto_mod/file03_anova_model.qmd",
                        label = "⚙️ Anova Model...",
                        format = "html"),
        "pack06" = list(rel = "f02_quarto_mod/file04_anova_descriptive_rv.qmd",
                        label = "⚙️ Descriptive RV...",
                        format = "html"),
        "pack07" = list(rel = "f02_quarto_mod/file05_anova_descriptive_residuals.qmd",
                        label = "⚙️ Descriptive Residual...",
                        format = "html"),
        "pack08" = list(rel = "f02_quarto_mod/file06_anova_asa.qmd",
                        label = "⚙️ Automatic Statistic Asesor...",
                        format = "html"),
        ####################################################################################
        "pack09" = list(rel = "f05_shiny_output/tab01_control.qmd",
                        label = "👁 Shiny Output - Tab01 Control",
                        format = "html"),
        "pack10" = list(rel = "f05_shiny_output/tab02_anova_and_tukey.qmd",
                        label = "👁️ Shiny Output - Tab02 Anova and Tukeys",
                        format = "html"),
        "pack11" = list(rel = "f05_shiny_output/tab03_requeriments.qmd",
                        label = "👁️Shiny Output - requerimentsts",
                        format = "html"),
        "pack12" = list(rel = "f05_shiny_output/tab04_model.qmd",
                        label = "👁️ Shiny Output - Tab04 Models",
                        format = "html"),
        "pack13" = list(rel = "f05_shiny_output/tab05_descriptive_rv.qmd",
                        label = "👁️ Shiny output- Tab05 Descriptive RVs",
                        format = "html"),
        "pack14" = list(rel = "f05_shiny_output/tab06_residuals.qmd",
                        label = "👁️Shiny Output - Descriptive residualss",
                        format = "html"),
        ####################################################################################
        "pack15" = list(rel = "f06_asa/tab01_summary_anova.qmd",
                        label = "👁️ASAt - Ta014 Summarys",
                        format = "html"),
        "pack16" = list(rel = "f06_asa/tab02_level02.qmd",
                        label = "👁️ASA - Tab02 Level02s",
                        format = "html"),
        "pack17" = list(rel = "f06_asa/tab03_level03.qmd",
                        label = "👁️ASA - Tab03 Level03s",
                        format = "html"),
        ####################################################################################

        # "pack10" = list(rel = "g07_save_plots/AAA_01_RUNNER_g07_save_plots.qmd",
        #                 label = "📈 Saving plots",
        #                 format = "html"),
        "pack18" = list(rel = "f08_pdf/report_pdf.qmd",
                        label = "📄 PDF Report",
                        format = "typst")
      ) %>% lapply(function(x) {
        full_path <- file.path(base_path, x$rel)
        x$abs_path <- normalizePath(full_path, mustWork = FALSE)
        x$exists <- file.exists(full_path)
        x
      })
    })

    # --- 3. INICIALIZAR TAREAS ---
    observe({
      meta <- tasks_metadata()
      req(meta)

      for(task_id in names(meta)) {
        if(is.null(task_status[[task_id]])) {
          task_status[[task_id]] <- "pending"
        }
        task_labels[[task_id]] <- meta[[task_id]]$label
      }
    })

    # --- 4. FUNCIÓN PARA RENDERIZAR UNA FILA INDIVIDUAL ---
    create_task_output <- function(task_id) {
      output_name <- paste0("task_", task_id)

      output[[output_name]] <- renderUI({
        status <- if(is.null(task_status[[task_id]])) "pending" else task_status[[task_id]]
        label <- if(is.null(task_labels[[task_id]])) task_id else task_labels[[task_id]]

        color <- switch(status,
                        "pending" = "#566b7a",
                        "processing" = "#f39c12",
                        "done" = "#2ecc71",
                        "error" = "#e74c3c",
                        "#566b7a"
        )

        div(id = ns(paste0("task_div_", task_id)),
            style = paste0("display: flex; align-items: center; padding: 10px 12px; background: #0b1218; ",
                           "border-left: 3px solid ", color, "; border-radius: 4px; margin-bottom: 8px;",
                           "transition: all 0.2s ease;"),
            div(style = "width: 30px; text-align: center; margin-right: 12px;",
                if(status == "processing") {
                  icon("sync-alt", class = "fa-spin", style = paste0("color: ", color))
                } else if(status == "done") {
                  icon("check-circle", style = paste0("color: ", color))
                } else if(status == "error") {
                  icon("times-circle", style = paste0("color: ", color))
                } else {
                  icon("circle", style = paste0("color: ", color))
                }
            ),
            span(label, style = "font-family: 'Segoe UI', sans-serif; font-size: 0.75rem; color: #fff; flex-grow: 1;"),
            if(status == "processing") {
              tags$i(class = "fas fa-sync-alt fa-spin", style = paste0("color: ", color))
            } else if(status == "done") {
              tags$i(class = "fas fa-check", style = paste0("color: ", color))
            } else if(status == "error") {
              tags$i(class = "fas fa-exclamation-triangle", style = paste0("color: ", color))
            } else {
              tags$i(class = "far fa-clock", style = paste0("color: ", color))
            }
        )
      })
    }

    # --- 5. CREAR OUTPUTS ---
    observe({
      meta <- tasks_metadata()
      req(meta)

      for(task_id in names(meta)) {
        create_task_output(task_id)
      }
    })

    # --- 6. CONTENEDOR PRINCIPAL ---
    output$item05_quarto_exec <- renderUI({
      meta <- tasks_metadata()
      req(meta)

      task_placeholders <- lapply(names(meta), function(task_id) {
        uiOutput(ns(paste0("task_", task_id)))
      })

      div(style = "padding: 15px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px;",
          div(style = "margin-bottom: 15px; font-weight: 700; font-size: 0.8rem; color: #adb5bd; letter-spacing: 1px;",
              icon("microchip"), " ENGINE PIPELINE STATUS"
          ),
          div(style = "margin-bottom: 15px;",
              div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
                  span("Progress:", style = "font-size: 0.7rem; color: #adb5bd;"),
                  span(textOutput(ns("progress_text")), style = "font-size: 0.7rem; color: #2ecc71; font-weight: bold;")
              ),
              div(style = "width: 100%; background: #0b1218; border-radius: 4px; overflow: hidden;",
                  uiOutput(ns("progress_bar_fill"))
              )
          ),
          div(style = "margin-bottom: 15px; padding: 10px; background: #0b1218; border-radius: 4px;",
              div(style = "display: flex; align-items: center; gap: 10px;",
                  uiOutput(ns("current_icon")),
                  span("CURRENT:", style = "font-size: 0.65rem; color: #adb5bd; font-weight: bold;"),
                  span(textOutput(ns("current_task_text")),
                       style = "font-size: 0.7rem; color: #f39c12;")
              )
          ),
          div(style = "display: flex; flex-direction: column; gap: 2px; max-height: 400px; overflow-y: auto;",
              task_placeholders
          )
      )
    })

    # --- 7. OUTPUTS SIMPLES ---
    output$progress_text <- renderText({
      meta <- tasks_metadata()
      req(meta)
      total <- length(meta)
      completed <- sum(sapply(names(meta), function(id) {
        if(is.null(task_status[[id]])) 0 else if(task_status[[id]] == "done") 1 else 0
      }), na.rm = TRUE)
      paste0(round(completed / total * 100), "%")
    })

    output$progress_bar_fill <- renderUI({
      meta <- tasks_metadata()
      req(meta)
      total <- length(meta)
      completed <- sum(sapply(names(meta), function(id) {
        if(is.null(task_status[[id]])) 0 else if(task_status[[id]] == "done") 1 else 0
      }), na.rm = TRUE)
      width_pct <- if(total > 0) round(completed / total * 100) else 0
      div(style = paste0("width: ", width_pct, "%; height: 6px; background: #2ecc71; transition: width 0.3s ease;"))
    })

    output$current_task_text <- renderText({
      if(state$engine_started && !is.null(state$current_task)) {
        state$current_task
      } else if(state$super_done) {
        "✅ Complete!"
      } else {
        "⏳ Waiting..."
      }
    })

    output$current_icon <- renderUI({
      if(state$engine_started && !is.null(state$current_task)) {
        icon("sync-alt", class = "fa-spin", style = "color: #f39c12;")
      } else if(state$super_done) {
        icon("check-circle", style = "color: #2ecc71;")
      } else {
        icon("circle", style = "color: #566b7a;")
      }
    })

    # --- 8. OUTPUTS INFORMATIVOS ---
    output$item01_folder_target <- renderUI({
      req(inputs_bundle())
      div(style = "margin-bottom: 8px; padding: 6px 8px; background: #0b1218; border-radius: 4px;",
          icon("folder", style = "color: #2ecc71;"),
          span(" Local: ", style = "color: #adb5bd; font-size: 0.65rem;"),
          span(basename(inputs_bundle()$local_path), style = "color: #fff; font-size: 0.65rem; font-family: monospace;")
      )
    })

    output$item02_folder_quarto_render <- renderUI({
      req(inputs_bundle())
      div(style = "margin-bottom: 8px; padding: 6px 8px; background: #0b1218; border-radius: 4px;",
          icon("folder-open", style = "color: #2ecc71;"),
          span(" Temp: ", style = "color: #adb5bd; font-size: 0.65rem;"),
          span(basename(inputs_bundle()$temp_path), style = "color: #fff; font-size: 0.65rem; font-family: monospace;")
      )
    })

    output$item03_qmd_files <- renderUI({
      meta <- tasks_metadata()
      req(meta)
      div(style = "margin-bottom: 8px; padding: 6px 8px; background: #0b1218; border-radius: 4px;",
          icon("file-alt", style = "color: #2ecc71;"),
          span(" Tasks: ", style = "color: #adb5bd; font-size: 0.65rem;"),
          span(length(meta), style = "color: #fff; font-size: 0.65rem; font-weight: bold;")
      )
    })

    output$item04_temp_folder_Rscience <- renderUI({
      div(style = "margin-bottom: 8px; padding: 6px 8px; background: #0b1218; border-radius: 4px;",
          icon("info-circle", style = "color: #2ecc71;"),
          span(" Status: ", style = "color: #adb5bd; font-size: 0.65rem;"),
          span(if(state$engine_started) "🔄 Processing..." else if(state$super_done) "✅ Complete" else "⏳ Ready",
               style = paste0("font-size: 0.65rem; font-weight: bold; color: ",
                              if(state$engine_started) "#f39c12" else if(state$super_done) "#2ecc71" else "#566b7a", ";"))
      )
    })

    output$debug_internal_ui <- renderUI({
      req(show_debug)
      div(style = "margin-top: 15px; padding: 8px; background: #0b1218; border-radius: 4px; font-family: monospace; font-size: 0.6rem;",
          pre(paste("Engine:", state$engine_started,
                    "\nDone:", state$super_done,
                    "\nTask:", state$current_task,
                    "\nIndex:", current_index()))
      )
    })

    # --- 9. FUNCIÓN PARA RENDERIZAR QUARTO EN BACKGROUND ---
    render_quarto_worker <- function(qmd_path, replacements, format = "html") {
    #  base_app   <- "C:/Users/Legion/bulk/MyInstallers/Rscience2027_installer/App"
      base_app   <- dirname(here::here())
      base_app   <- file.path(base_app, "/App")
      quarto_bin <- file.path(base_app, "Quarto/bin/quarto.exe")
      r_portable <- file.path(base_app, "R-Portable")
      r_libs     <- file.path(r_portable, "library")
      chrome_bin <- file.path(base_app, "Chrome/GoogleChromePortable/App/Chrome-bin/chrome.exe")

      formatos_validos <- c("typst", "html", "pdf", "docx", "pptx", "latex", "markdown")
      if (!format %in% formatos_validos) {
        warning("Formato '", format, "' no reconocido. Usando 'html' por defecto.")
        format <- "html"
      }

      qmd_dir <- dirname(qmd_path)
      bat_file <- file.path(qmd_dir, paste0("run_", tools::md5sum(qmd_path), ".bat"))
      config_file <- file.path(qmd_dir, "_quarto.yml")

      config_list <- list(params = list(list_quarto_replacement = replacements))
      yaml::write_yaml(config_list, config_file)

      bat_content <- c(
        "@echo off",
        paste0('set "R_HOME=', normalizePath(r_portable, winslash = "\\"), '"'),
        paste0('set "R_LIBS_USER=', normalizePath(r_libs, winslash = "\\"), '"'),
        paste0('set "QUARTO_CHROME=', normalizePath(chrome_bin, winslash = "\\"), '"'),
        paste0('set "PATH=', normalizePath(file.path(base_app, "Quarto/bin"), winslash = "\\"), ";",
               normalizePath(file.path(r_portable, "bin"), winslash = "\\"), ';%PATH%"'),
        paste0('"', normalizePath(quarto_bin, winslash = "\\"), '" render "', basename(qmd_path), '" --to ', format)
      )
      writeLines(bat_content, bat_file)

      message("🚀 Renderizando: ", basename(qmd_path), " -> Formato: ", format)

      old_wd <- getwd()
      setwd(qmd_dir)

      res <- system2("cmd.exe", args = c("/c", basename(bat_file)), stdout = TRUE, stderr = TRUE)

      setwd(old_wd)
      if(file.exists(bat_file)) unlink(bat_file)
      if(file.exists(config_file)) unlink(config_file)

      status <- attr(res, "status")
      if (!is.null(status) && status != 0) {
        stop(paste(res, collapse = "\n"))
      }

      message("✅ Renderizado completado: ", basename(qmd_path))
      return(TRUE)
    }

    render_quarto_async <- function(task_path, idx, replacements, format) {
      future({
        result <- tryCatch({
          params_to_send <- if(idx == 1) replacements else NULL
          render_quarto_worker(qmd_path = task_path, replacements = params_to_send, format = format)
          list(success = TRUE, error = NULL)
        }, error = function(e) {
          list(success = FALSE, error = e$message)
        })
        result
      })
    }

    # --- 10. OBSERVERS ---

    # OBSERVER 1: Solo actualiza la UI a "processing"
    observeEvent(task_ui_trigger(), {
      idx <- task_ui_trigger()

      if (!state$engine_started || idx == 0) return()

      meta <- isolate(tasks_metadata())
      pkg_names <- names(meta)
      total_tasks <- length(pkg_names)

      if (idx > total_tasks) {
        return()
      }

      curr_id <- pkg_names[idx]

      task_status[[curr_id]] <- "processing"
      state$current_task <- task_labels[[curr_id]]
      message(">>> [UI] 🎨 Mostrando 'processing' para: ", curr_id)

      task_process_trigger(idx)

    }, ignoreInit = TRUE)

    # OBSERVER 2: Procesamiento REAL
    observeEvent(task_process(), {
      idx <- task_process()

      if (!state$engine_started || idx == 0) return()

      meta <- isolate(tasks_metadata())
      pkg_names <- names(meta)
      total_tasks <- length(pkg_names)

      if (idx > total_tasks) {
        return()
      }

      curr_id <- pkg_names[idx]

      message(">>> [PROCESS] ⚙️ Iniciando procesamiento REAL de: ", curr_id)

      task <- meta[[curr_id]]
      task_path <- task$abs_path
      replacements <- isolate(inputs_bundle()$replacement)

      # OBTENER EL FORMATO DE LA TAREA
      task_format <- if (!is.null(task$format)) task$format else "html"

      render_quarto_async(task_path, idx, replacements, task_format) %...>%
        (function(result) {
          if (result$success) {
            task_status[[curr_id]] <- "done"
            message(">>> [PROCESS] ✅ OK: ", curr_id)
            task_complete_trigger(idx)
          } else {
            task_status[[curr_id]] <- "error"
            state$engine_started <- FALSE
            state$current_task <- NULL
            execution_lock <<- FALSE
            current_index(0)
            message(">>> [PROCESS] ❌ ERROR: ", result$error)
            showNotification(paste("Error:", result$error), duration = 10, type = "error")
          }
        }) %...!%
        (function(error) {
          task_status[[curr_id]] <- "error"
          state$engine_started <- FALSE
          execution_lock <<- FALSE
          message(">>> [PROCESS] ❌ Future error: ", error$message)
        })

    }, ignoreInit = TRUE)

    # OBSERVER 3: Delay después de completar
    observeEvent(task_complete(), {
      idx <- task_complete()

      if (!state$engine_started || idx == 0) return()

      meta <- isolate(tasks_metadata())
      total_tasks <- length(names(meta))

      next_idx <- idx + 1

      if (next_idx > total_tasks) {
        state$super_done <- TRUE
        state$engine_started <- FALSE
        state$current_task <- NULL
        execution_lock <<- FALSE
        current_index(0)
        message(">>> [COMPLETE] 🎉 PIPELINE COMPLETO")
        showNotification("Pipeline completed!", duration = 3, type = "message")
      } else {
        current_index(next_idx)
        message(">>> [COMPLETE] 👉 Siguiente tarea: ", next_idx)
      }

    }, ignoreInit = TRUE)

    # --- 11. BOTÓN DE INICIO ---
    observeEvent(input$start_pipeline, {
      if (execution_lock) {
        showNotification("Pipeline already running!", duration = 2, type = "warning")
        return()
      }

      execution_lock <<- TRUE
      message(">>> 🚀 Iniciando Pipeline")

      meta <- isolate(tasks_metadata())
      if (is.null(meta) || length(meta) == 0) {
        execution_lock <<- FALSE
        showNotification("No tasks available!", type = "error")
        return()
      }

      missing_files <- names(meta)[!sapply(meta, function(x) x$exists)]
      if(length(missing_files) > 0) {
        showNotification(paste("Missing files:", paste(missing_files, collapse = ", ")), duration = 5, type = "error")
        execution_lock <<- FALSE
        return()
      }

      pkg_names <- names(meta)
      total_tasks <- length(pkg_names)

      for(task_id in pkg_names) {
        task_status[[task_id]] <- "pending"
        task_labels[[task_id]] <- meta[[task_id]]$label
      }
      state$super_done <- FALSE
      state$engine_started <- TRUE
      state$current_task <- NULL
      task_complete_trigger(0)
      task_process_trigger(0)

      showNotification(paste("Pipeline started -", total_tasks, "tasks"), duration = 3, type = "message")
      current_index(1)
      message(">>> 👉 Primera tarea programada (UI primero, luego proceso)")

    }, ignoreInit = TRUE)

    # --- 12. RETORNO ---
    return(reactive({ state$super_done }))
  })
}#
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
