library(shiny)
library(bslib)
library(quarto)
library(shinyjs)

# ==============================================================================
# UI HELPER: BOTONERA GENERALIZADA
# ==============================================================================
btn_group_rscience <- function(ns, id_suffix) {
  div(class = "d-flex gap-2 mb-3",
      actionButton(ns(paste0("run_", id_suffix)), " Renderizar",
                   icon = icon("play"), class = "btn-success"),
      actionButton(ns(paste0("open_", id_suffix)), " Abrir en Ventana",
                   icon = icon("external-link-alt"), class = "btn-info"),
      downloadButton(ns(paste0("download_", id_suffix)), " Descargar HTML",
                     class = "btn-outline-secondary")
  )
}

# ==============================================================================
# MÓDULO UI
# ==============================================================================
mod_special_script_comment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Requiere shinyjs() en la UI principal
    navset_card_tab(
      title = "Editor de Reportes Quarto",
      nav_panel(
        title = "Anova Original 01",
        btn_group_rscience(ns, "file01"),
        uiOutput(ns("debug_panel_file01")), # Debug específico 01
        uiOutput(ns("view_html_file01"))
      ),
      nav_panel(
        title = "Anova Original 02",
        btn_group_rscience(ns, "file02"),
        uiOutput(ns("debug_panel_file02")), # Debug específico 02
        uiOutput(ns("view_html_file02"))
      )
    )
  )
}

# ==============================================================================
# MÓDULO SERVER
# ==============================================================================
mod_special_script_comment_server <- function(id, folder_temp_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. RUTA DINÁMICA DE LA CARPETA
    # --------------------------------------------------------------------------
    special_folder <- reactive({
      req(folder_temp_path())
      file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "f02_quarto_mod")
    })

    # 2. REGISTRO DE RECURSOS (Alias para el navegador)
    # --------------------------------------------------------------------------
    res_prefix <- paste0("res_path_", id)
    observe({
      req(special_folder())
      if (dir.exists(special_folder())) {
        addResourcePath(prefix = res_prefix, directoryPath = special_folder())
      }
    })

    # Mapeo de archivos (ID en UI -> Nombre de archivo real)
    files_map <- list(
      file01 = "file01_anova_original",
      file02 = "file01_anova_original_02"
    )

    # 3. LÓGICA POR ARCHIVO (Scope local protegido)
    # --------------------------------------------------------------------------
    for (key in names(files_map)) {

      local({
        k <- key
        file_base <- files_map[[k]]

        # --- A. PANEL DE DEBUG INDEPENDIENTE ---
        output[[paste0("debug_panel_", k)]] <- renderUI({
          # Se invalida cuando se pulsa renderizar para refrescar estados
          input[[paste0("run_", k)]]

          req(special_folder())
          qmd_path  <- file.path(special_folder(), paste0(file_base, ".qmd"))
          html_path <- file.path(special_folder(), paste0(file_base, ".html"))

          div(style = "background: #f8f9fa; border: 1px solid #dee2e6; border-left: 4px solid #0d6efd; padding: 12px; margin-bottom: 15px; font-family: 'Courier New', monospace; font-size: 0.9em;",
              tags$b(paste("SISTEMA DE DEBUG - ID:", k)), br(),
              "• Carpeta detectada: ", tags$code(special_folder()), br(),
              "• Archivo Quarto (.qmd): ", if(file.exists(qmd_path)) span("✔ ENCONTRADO", style="color:green; font-weight:bold") else span("✘ NO EXISTE", style="color:red; font-weight:bold"), br(),
              "• Reporte HTML: ", if(file.exists(html_path)) span("✔ GENERADO", style="color:green; font-weight:bold") else span("✘ PENDIENTE", style="color:red; font-weight:bold"), br(),
              "• URL Virtual: ", tags$code(paste0(res_prefix, "/", file_base, ".html"))
          )
        })

        # --- B. LÓGICA DE RENDERIZADO ---
        observeEvent(input[[paste0("run_", k)]], {
          req(special_folder())
          qmd_path <- file.path(special_folder(), paste0(file_base, ".qmd"))

          if (!file.exists(qmd_path)) {
            showNotification(paste("Error: No se encontró el archivo fuente .qmd para", k), type = "error")
            return()
          }

          withProgress(message = paste('Procesando Quarto:', file_base), value = 0, {
            tryCatch({
              setProgress(0.5)
              quarto::quarto_render(input = qmd_path, output_format = "html")
              showNotification(paste("Renderizado exitoso:", file_base), type = "message")
            }, error = function(e) {
              showNotification(paste("Error en Quarto:", e$message), type = "error", duration = 10)
            })
          })
        })

        # --- C. VISUALIZACIÓN IFRAME ---
        output[[paste0("view_html_", k)]] <- renderUI({
          # Se actualiza tras el renderizado
          input[[paste0("run_", k)]]
          req(special_folder())

          html_phys <- file.path(special_folder(), paste0(file_base, ".html"))

          if (file.exists(html_phys)) {
            # URL con cache-busting (timestamp)
            url <- paste0(res_prefix, "/", file_base, ".html?t=", as.numeric(Sys.time()))

            tags$iframe(
              src = url,
              style = "width:100%; height:800px; border:1px solid #ddd; border-radius: 8px; background: white;",
              frameborder = "0"
            )
          } else {
            div(class = "alert alert-light border mt-3 text-center",
                style = "padding: 30px;",
                icon("file-export", class = "fa-2x mb-3 text-muted"), br(),
                "No hay un reporte generado para visualizar aún.")
          }
        })

        # --- D. BOTONES EXTERNOS ---
        observeEvent(input[[paste0("open_", k)]], {
          url <- paste0(res_prefix, "/", file_base, ".html?t=", as.numeric(Sys.time()))
          shinyjs::runjs(sprintf("window.open('%s', '_blank');", url))
        })

        output[[paste0("download_", k)]] <- downloadHandler(
          filename = function() { paste0(file_base, ".html") },
          content = function(file) {
            src <- file.path(special_folder(), paste0(file_base, ".html"))
            if (file.exists(src)) {
              file.copy(src, file)
            } else {
              showNotification("El archivo no existe para descargar.", type = "warning")
            }
          }
        )
      })
    }

    message("--- [Módulo Debug] Server inicializado para: ", id)
  })
}
