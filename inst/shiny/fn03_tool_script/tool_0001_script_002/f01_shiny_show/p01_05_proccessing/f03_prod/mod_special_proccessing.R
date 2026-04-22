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

    uiOutput(ns("item01_folder_target")),
    uiOutput(ns("item02_folder_quarto_render")),
    uiOutput(ns("item03_qmd_files")),
    uiOutput(ns("item04_temp_folder_Rscience")),
    uiOutput(ns("item05_quarto_exec")), # Ahora el antiguo 07 es el 05

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
      status_list = list(),
      super_done = FALSE,
      current_task = NULL
    )

    # Lock para evitar ejecuciones múltiples
    execution_lock <- FALSE

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
      base_path <- normalizePath(file.path(inputs_bundle()$temp_path, "f02_quarto_proc"), mustWork = FALSE)

      message(">>> [METADATA] Base path: ", base_path)

      list(
        "pack01" = list(rel = "g01_quarto_original/AAA_02_STONE_01_copying_files.qmd", label = "Copying files..."),
        "pack02" = list(rel = "g01_quarto_original/AAA_02_STONE_02_modying_files.qmd", label = "Applying settings on R scripts"),
        "pack03" = list(rel = "g02_quarto_mod/AAA_01_RUNNER_g02_quarto_mod.qmd", label = "Running R script"),
        "pack04" = list(rel = "g04_script_external/AAA_01_RUNNER_g04_script_external.qmd", label = "Packaging R scripts (.R - zip)"),
        "pack05" = list(rel = "g05_shiny_output/AAA_01_RUNNER_g05_shiny_output.qmd", label = "View - Shiny Outputs"),
        "pack06" = list(rel = "g06_asa/AAA_01_RUNNER_g06_asa.qmd", label = "View - ASA"),
        "pack07" = list(rel = "g07_save_plots/AAA_01_RUNNER_g07_save_plots.qmd", label = "Plots as png/html"),
        "pack08" = list(rel = "f08_pdf/report_pdf.qmd", label = "PDF Report")
      ) %>% lapply(function(x) {
        full_path <- file.path(base_path, x$rel)
        x$abs_path <- normalizePath(full_path, mustWork = FALSE)
        x$exists <- file.exists(full_path)
        if(!x$exists) message(">>> [METADATA] ⚠️ No existe: ", full_path)
        x
      })
    })

    # --- 3. UI RENDER (SIMPLE Y DIRECTO) ---

    render_file_row_server <- function(label, s) {
      if (is.null(s) || length(s) == 0) s <- "pending"

      icon_name <- switch(s,
                          "pending" = "hourglass",
                          "processing" = "spinner",
                          "done" = "check-double",
                          "error" = "times-circle",
                          "hourglass"
      )

      icon_class <- if(s == "processing") "fa-spin" else NULL
      color <- switch(s,
                      "pending" = "#566b7a",
                      "processing" = "#00d4ff",
                      "done" = "#00bc8c",
                      "error" = "#ff4b5c",
                      "#566b7a"
      )

      div(
        style = paste0("display: flex; align-items: center; padding: 8px 12px; background: #0b1218; ",
                       "border-left: 3px solid ", color, "; border-radius: 4px; margin-bottom: 6px;"),
        div(style = paste0("width: 8px; height: 8px; border-radius: 50%; margin-right: 12px; ",
                           "background:", color, "; box-shadow: 0 0 8px ", color),
        ),
        span(label, style = "font-family: 'JetBrains Mono'; font-size: 0.68rem; color: #fff; flex-grow: 1;"),
        icon(icon_name, class = icon_class, style = paste0("color: ", color))
      )
    }

    # Output principal del pipeline status
    output$item05_quarto_exec <- renderUI({
      meta <- tasks_metadata()
      req(meta)
      current_status <- state$status_list
      current_task <- state$current_task

      # Calcular progreso
      total <- length(meta)
      if(length(current_status) > 0) {
        completed <- sum(unlist(current_status) == "done", na.rm = TRUE)
      } else {
        completed <- 0
      }
      progress_pct <- if(total > 0) round(completed / total * 100) else 0

      rows <- lapply(names(meta), function(id) {
        s_val <- if(!is.null(current_status[[id]])) current_status[[id]] else "pending"
        render_file_row_server(meta[[id]]$label, s_val)
      })

      div(style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px;",
          div(style = "margin-bottom: 15px; font-weight: 800; font-size: 0.75rem; color: #adb5bd;",
              icon("microchip"), " ENGINE PIPELINE STATUS"
          ),
          # Barra de progreso
          div(style = "margin-bottom: 15px;",
              div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
                  span("Progress:", style = "font-size: 0.7rem; color: #adb5bd;"),
                  span(paste0(progress_pct, "%"), style = "font-size: 0.7rem; color: #00d4ff; font-weight: bold;")
              ),
              div(style = "width: 100%; background: #0b1218; border-radius: 4px; overflow: hidden;",
                  div(style = paste0("width: ", progress_pct, "%; height: 4px; background: #00d4ff; transition: width 0.3s ease;"))
              )
          ),
          # Tarea actual
          if(!is.null(current_task) && state$engine_started)
            div(style = "margin-bottom: 15px; padding: 8px; background: #0b1218; border-radius: 4px;",
                icon("play", style = "color: #00d4ff; font-size: 0.7rem;"),
                span(" Current: ", style = "font-size: 0.7rem; color: #adb5bd;"),
                span(current_task, style = "font-size: 0.7rem; color: #00d4ff; font-weight: bold;")
            ),
          div(rows)
      )
    })

    # Outputs informativos
    output$item01_folder_target <- renderUI({
      req(inputs_bundle())
      div(style = "margin-bottom: 10px; padding: 8px; background: #0b1218; border-radius: 4px;",
          icon("folder", style = "color: #00d4ff;"),
          span(" Local: ", style = "color: #adb5bd; font-size: 0.7rem;"),
          span(inputs_bundle()$local_path, style = "color: #fff; font-size: 0.7rem; font-family: monospace;")
      )
    })

    output$item02_folder_quarto_render <- renderUI({
      req(inputs_bundle())
      div(style = "margin-bottom: 10px; padding: 8px; background: #0b1218; border-radius: 4px;",
          icon("folder-open", style = "color: #00d4ff;"),
          span(" Temp: ", style = "color: #adb5bd; font-size: 0.7rem;"),
          span(inputs_bundle()$temp_path, style = "color: #fff; font-size: 0.7rem; font-family: monospace;")
      )
    })

    output$item03_qmd_files <- renderUI({
      meta <- tasks_metadata()
      req(meta)
      div(style = "margin-bottom: 10px; padding: 8px; background: #0b1218; border-radius: 4px;",
          icon("file-alt", style = "color: #00d4ff;"),
          span(" QMD files: ", style = "color: #adb5bd; font-size: 0.7rem;"),
          span(length(meta), style = "color: #fff; font-size: 0.7rem; font-weight: bold;")
      )
    })

    output$item04_temp_folder_Rscience <- renderUI({
      div(style = "margin-bottom: 10px; padding: 8px; background: #0b1218; border-radius: 4px;",
          icon("info-circle", style = "color: #00d4ff;"),
          span(" Status: ", style = "color: #adb5bd; font-size: 0.7rem;"),
          span(if(state$engine_started) "Processing..." else if(state$super_done) "Complete" else "Ready",
               style = paste0("font-size: 0.7rem; font-weight: bold; color: ",
                              if(state$engine_started) "#00d4ff" else if(state$super_done) "#00bc8c" else "#566b7a", ";"))
      )
    })

    # Debug interno
    output$debug_internal_ui <- renderUI({
      req(show_debug)
      div(style = "margin-top: 20px; padding: 10px; background: #0b1218; border-radius: 4px; font-family: monospace; font-size: 0.7rem;",
          pre(paste(
            "Engine started:", state$engine_started,
            "\nSuper done:", state$super_done,
            "\nCurrent task:", state$current_task
          ))
      )
    })

    # --- 4. BOTÓN Y MOTOR (VERSIÓN SIMPLIFICADA) ---

    observeEvent(input$start_pipeline, {
      message(">>> [BOTON] start_pipeline clickeado")

      if (execution_lock) {
        message(">>> [MOTOR] ⚠️ Pipeline ya en ejecución")
        showNotification("Pipeline already running!", type = "warning", duration = 2)
        return()
      }

      # Adquirir lock
      execution_lock <<- TRUE
      message(">>> [MOTOR] 🔒 Lock adquirido")

      meta <- isolate(tasks_metadata())
      if (is.null(meta) || length(meta) == 0) {
        message(">>> [MOTOR] ❌ No hay metadata")
        execution_lock <<- FALSE
        showNotification("No tasks metadata available!", type = "error")
        return()
      }

      # Verificar que los archivos existan
      missing_files <- names(meta)[!sapply(meta, function(x) x$exists)]
      if(length(missing_files) > 0) {
        message(">>> [MOTOR] ❌ Archivos faltantes: ", paste(missing_files, collapse = ", "))
        showNotification(paste("Missing files:", paste(missing_files, collapse = ", ")), type = "error", duration = 5)
        execution_lock <<- FALSE
        return()
      }

      pkg_names <- names(meta)
      total_tasks <- length(pkg_names)

      # Reset estados
      state$status_list <- setNames(rep("pending", total_tasks), pkg_names)
      state$super_done <- FALSE
      state$engine_started <- TRUE
      state$current_task <- NULL

      message(">>> [MOTOR] 🚀 Iniciando Pipeline (", total_tasks, " tareas)")
      showNotification(paste("Pipeline started -", total_tasks, "tasks"), type = "message", duration = 3)

      # Función recursiva con delays para permitir UI updates
      run_task <- function(idx) {
        if (!state$engine_started) {
          message(">>> [MOTOR] Pipeline detenido")
          execution_lock <<- FALSE
          return()
        }

        if (idx > total_tasks) {
          state$super_done <- TRUE
          state$engine_started <- FALSE
          state$current_task <- NULL
          execution_lock <<- FALSE
          message(">>> [MOTOR] 🎉 PIPELINE COMPLETO")
          showNotification("Pipeline completed successfully!", type = "success", duration = 5)
          return()
        }

        curr_id <- pkg_names[idx]
        state$status_list[[curr_id]] <- "processing"
        state$current_task <- meta[[curr_id]]$label

        message(">>> [MOTOR] ⚙️ [", idx, "/", total_tasks, "] ", curr_id, ": ", meta[[curr_id]]$label)

        task <- meta[[curr_id]]
        task_path <- task$abs_path
        replacements <- isolate(inputs_bundle()$replacement)

        # Pequeño delay para que la UI se actualice
        Sys.sleep(0.05)

        # Ejecutar la tarea
        tryCatch({
          old_wd <- getwd()
          setwd(dirname(task_path))

          if(idx == 1 && !is.null(replacements)) {
            quarto::quarto_render(
              input = basename(task_path),
              execute_params = list(list_quarto_replacement = replacements),
              quiet = FALSE
            )
          } else {
            quarto::quarto_render(input = basename(task_path), quiet = FALSE)
          }
          setwd(old_wd)

          # Éxito
          state$status_list[[curr_id]] <- "done"
          message(">>> [MOTOR] ✅ OK: ", curr_id)

          # Pequeño delay antes de la siguiente tarea
          Sys.sleep(0.05)

          # Siguiente tarea
          run_task(idx + 1)

        }, error = function(e) {
          setwd(old_wd)
          state$status_list[[curr_id]] <- "error"
          state$engine_started <- FALSE
          state$current_task <- NULL
          execution_lock <<- FALSE
          message(">>> [MOTOR] ❌ ERROR en ", curr_id, ": ", e$message)
          showNotification(paste("Error in", curr_id, ":", e$message), type = "error", duration = 10)
        })
      }

      # Iniciar
      run_task(1)

    }, ignoreInit = TRUE)

    # --- 5. RETORNO ---
    return(reactive({ state$super_done }))
  })
}#
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
