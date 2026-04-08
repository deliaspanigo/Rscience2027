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
    # Importante: se requiere shinyjs en la app principal para 'open window'
    navset_card_tab(
      title = "Editor de Reportes Quarto",
      nav_panel(
        title = "Anova Original 01",
        btn_group_rscience(ns, "file01"),
        uiOutput(ns("view_html_01"))
      ),
      nav_panel(
        title = "Anova Original 02",
        btn_group_rscience(ns, "file02"),
        uiOutput(ns("view_html_02"))
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

    # 1. RUTAS DINÁMICAS BASADAS EN EL BUNDLE
    # --------------------------------------------------------------------------
    special_folder <- reactive({
      req(folder_temp_path())
      the_temp_folder <- folder_temp_path
      # Ruta según tu estructura: f02_quarto_lab/f02_quarto_render/f02_quarto_mod
      file.path(the_temp_folder,
                "f02_quarto_lab", "f02_quarto_render", "f02_quarto_mod")
    })

    # Mapeo de archivos (Lista para facilitar mantenimiento)
    files_map <- list(
      file01 = "file01_anova_original",
      file02 = "file01_anova_original_02"
    )

    # 2. SERVIR ARCHIVOS TEMPORALES
    # --------------------------------------------------------------------------
    # Creamos un alias para que el navegador pueda acceder a la carpeta temporal
    observe({
      req(special_folder())
      addResourcePath(prefix = paste0("res_path_", id),
                      directoryPath = special_folder())
    })

    # 3. LÓGICA POR ARCHIVO (Iterativa)
    # --------------------------------------------------------------------------
    lapply(names(files_map), function(key) {
      file_base <- files_map[[key]]

      # --- RENDERIZAR (.qmd -> .html) ---
      observeEvent(input[[paste0("run_", key)]], {
        qmd_path <- file.path(special_folder(), paste0(file_base, ".qmd"))
        req(file.exists(qmd_path))

        withProgress(message = paste('Renderizando:', file_base), value = 0.5, {
          tryCatch({
            quarto::quarto_render(input = qmd_path, output_format = "html")
            showNotification(paste("Renderizado exitoso:", file_base), type = "message")
          }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
          })
        })
      })

      # --- ABRIR EN VENTANA NUEVA (JS) ---
      observeEvent(input[[paste0("open_", key)]], {
        html_url <- paste0("res_path_", id, "/", file_base, ".html")
        # Usamos shinyjs para abrir una pestaña limpia
        shinyjs::runjs(sprintf("window.open('%s', '_blank');", html_url))
      })

      # --- VISUALIZACIÓN IFRAME ---
      output[[paste0("view_html_", key)]] <- renderUI({
        # Forzamos re-renderizado del UI si el archivo cambia (usando un trigger de tiempo o el botón)
        input[[paste0("run_", key)]]

        html_path_phys <- file.path(special_folder(), paste0(file_base, ".html"))
        if (!file.exists(html_path_phys)) {
          return(div(class = "alert alert-warning", "El archivo HTML no existe. Presione 'Renderizar'."))
        }

        url <- paste0("res_path_", id, "/", file_base, ".html")
        tags$iframe(src = url, style = "width:100%; height:800px; border:1px solid #ddd; border-radius: 8px;")
      })

      # --- DESCARGAR ---
      output[[paste0("download_", key)]] <- downloadHandler(
        filename = function() { paste0(file_base, ".html") },
        content = function(file) {
          file.copy(file.path(special_folder(), paste0(file_base, ".html")), file)
        }
      )
    })

    message("--- [Sub-Módulo] Script Comment (Render/View) inicializado para: ", id)
  })
}
