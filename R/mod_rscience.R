# ==============================================================================
# MÓDULO RSCIENCE (UI) - v.0.0.1 ENCAPSULADO & CYAN EDITION
# ==============================================================================
library("bslib")
library("shiny")

path_tool <- system.file("shiny", "fn03_tool_script", "tool_0001_script_001", "f01_settings",
                         package = "Rscience2027")

# Para cargar el pack_module.R específicamente:
source(file.path(path_tool, "pack_module.R"))

path_tool02 <- system.file("shiny", "fn03_tool_script", "tool_0001_script_001", "f01_settings", "sub_module",
                           package = "Rscience2027")

# Para cargar el pack_module.R específicamente:
source(file.path(path_tool02, "sm01_var_selection.R"))
source(file.path(path_tool02, "sm02_levels.R"))


mod_rscience_ui <- function(id) {
  ns <- NS(id)

  # ID único para el contenedor raíz del motor
  wrapper_id <- ns("engine_wrapper")

  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* --- ENCAPSULAMIENTO DEL MOTOR --- */

        /* 1. Reset de visibilidad para el contenedor del Engine */
        #", wrapper_id, " {
          display: block;
          width: 100vw;
          height: 100vh;
          overflow: hidden;
        }

        /* 2. Ocultar el encabezado de las pestañas (específico del módulo) */
        #", ns("engine_switcher"), " { display: none; }

        /* 3. Fix de Sidebar (BSLIB) dentro de este módulo */
        #", wrapper_id, " aside.sidebar {
          height: 100vh !important;
          overflow: visible !important;
        }

        #", wrapper_id, " .sidebar-content {
          padding: 0 !important;
          margin: 0 !important;
          height: 100% !important;
          overflow: visible !important;
        }

        /* 4. Estilos de los títulos de sección */
        #", wrapper_id, " .section-title {
           text-transform: uppercase; font-weight: 800; font-size: 0.65rem;
           color: #008eb3 !important; margin-top: 12px; margin-bottom: 5px;
           letter-spacing: 0.5px;
        }

        /* 5. Botón de Salida (Go Home) */
        #", wrapper_id, " .btn-go-home {
          width: 100%; font-weight: 800; text-transform: uppercase; font-size: 0.7rem;
          border-radius: 8px; transition: 0.3s; padding: 8px;
          background: #fff !important; color: #ff4d4d !important;
          border: 1.5px solid #ff4d4d !important;
        }
        #", wrapper_id, " .btn-go-home:hover { background: #ff4d4d !important; color: #fff !important; }

        #", wrapper_id, " hr { border-top: 1px solid #00d4ff !important; opacity: 0.15 !important; margin: 10px 0 !important; }

        /* 6. Selectize por encima de todo */
        .selectize-dropdown { z-index: 10000 !important; }

        /* 7. TABLA PREVIEW: ZEBRA CYAN (Específica del ID de importación) */
        #", ns("my_ns_import-preview"), " table.dataTable thead th {
          background-color: #00d4ff !important;
          color: white !important;
        }

        #", ns("my_ns_import-preview"), " table.dataTable tbody tr.odd {
          background-color: #e6faff !important;
        }

        #", ns("my_ns_import-preview"), " table.dataTable tbody tr.even {
          background-color: #ffffff !important;
        }

        #", ns("my_ns_import-preview"), " table.dataTable tbody tr:hover {
          background-color: #ccf5ff !important;
        }

        /* Color del Switch cuando está ENCENDIDO */
        #", wrapper_id, " .form-check-input:checked {
            background-color: #00d4ff !important;
            border-color: #00d4ff !important;
        }

        /* Color de la sombra/foco al hacer clic */
        #", wrapper_id, " .form-check-input:focus {
            border-color: #00d4ff;
            box-shadow: 0 0 0 0.25rem rgba(0, 212, 255, 0.25);
        }

        /* Estilo del Switch cuando está APAGADO */
        #", wrapper_id, " .form-check-input {
            background-color: #dee2e6;
            border-color: #ced4da;
            background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='black'/%3e%3c/svg%3e\") !important;
        }
      ")))
    ),

    # Envolvemos el layout de bslib en el div controlador
    div(id = wrapper_id,
        page_sidebar(
          theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),
          fillable = TRUE,

          sidebar = sidebar(
            width = 300, padding = 0,
            div(
              style = "height: 100vh; background: linear-gradient(180deg, #ffffff 0%, #e6faff 100%); border-right: 3px solid #00d4ff; display: flex; flex-direction: column; overflow: visible;",

              div(class = "text-center", style = "padding: 20px 0 5px 0;",
                  img(src = "Rscience_logo_sticker.png", style = "width: 180px;")
              ),

              # Área de Switches
              div(style = "padding: 0 1.5rem; flex-grow: 1; overflow-y: visible;",
                  hr(),
                  div(class = "section-title", "1. Setup Phase"),
                  input_switch(ns("sw_dataset"), "Dataset Selection", value = TRUE),
                  input_switch(ns("sw_tool"), "Tool Engine", value = FALSE),
                  input_switch(ns("sw_script"), "Script Engine", value = FALSE),
                  hr(),
                  #######################################################################################
                  div(class = "section-title", "2. Information Phase"),
                  input_switch(ns("sw_theory"), "Theory", value = FALSE),
                  input_switch(ns("sw_bibliography"), "Bibliography", value = FALSE),
                  input_switch(ns("sw_cite"), "Cite Rscience!", value = FALSE),
                  hr(),
                  #######################################################################################
                  div(class = "section-title", "3. Data Analysis Phase"),
                  input_switch(ns("sw_settings"), "Settings", value = FALSE),
                  input_switch(ns("sw_shiny"), "Shiny Outputs", value = FALSE),
                  input_switch(ns("sw_asesor"), "Automatic Statistic Asesor (ASA)", value = FALSE),
                  #######################################################################################
                  hr(),
                  div(class = "section-title", "4. R Code Phase"),
                  input_switch(ns("sw_script_comments"), "Scripts + Comments", value = FALSE),
                  input_switch(ns("sw_script_basic"), "Scripts", value = FALSE),
                  #######################################################################################
                  hr(),
                  div(class = "section-title", "5. Final Phase"),
                  input_switch(ns("sw_reporting"), "Reporting and Download", value = FALSE),

              ),

              div(style = "padding: 15px 1.5rem; background: rgba(255,255,255,0.4);",
                  actionButton(ns("btn_go_home"), "Exit to Launchpad", icon = icon("door-open"), class = "btn-go-home")
              )
            )
          ),

          # ÁREA PRINCIPAL (Tabset oculto)
          tabsetPanel(
            id = ns("engine_switcher"),
            type = "hidden",
            tabPanelBody("tab_welcome", div(class="vh-100 d-flex align-items-center justify-content-center", h4("Select a module", class="text-muted"))),
            #######################################################################################
            tabPanelBody("tab_dataset", div(class="p-3", mod_import_ui(ns("my_ns_import")))),
            tabPanelBody("tab_tool", div(mod_tools_ui(ns("my_ns_tool")))),
            tabPanelBody("tab_script", "Waiting for script..."),
            #######################################################################################
            tabPanelBody("tab_theory", "Waiting for Theory..."),
            tabPanelBody("tab_bibliography", "Waiting for bibliography..."),
            tabPanelBody("tab_cite", "Waiting for Cite..."),
            #######################################################################################
            tabPanelBody("tab_settings", div(class="p-4", PACK_mod_main_ui(ns("mi_app")))),
            tabPanelBody("tab_shiny", "Waiting for Shiny..."),
            tabPanelBody("tab_asesor", "Waiting for Asesor..."),
            #######################################################################################
            tabPanelBody("tab_script_comments", "Waiting for Scripts comments..."),
            tabPanelBody("tab_script_basic", "Waiting for Scripts basics..."),
            #######################################################################################
            tabPanelBody("tab_reporting", "Waiting for Reporting..."),
            #######################################################################################
            tabPanelBody("tab_EXTRA", "EXTRA")


          )
        )
    )
  )
}

# ==============================================================================
# MÓDULO RSCIENCE (SERVER)
# ==============================================================================
mod_rscience_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ############################################################################






    # 3. Exclusividad de Switches
    all_sw <- c("sw_dataset", "sw_tool", "sw_script",
                ##################################################
                "sw_theory", "sw_bibliography", "sw_cite",
                ##################################################
                "sw_settings", "sw_shiny", "sw_asesor",
                ##################################################
                "sw_script_comments", "sw_script_basic",
                ##################################################
                "sw_reporting"
                )

    active_sw_id <- reactiveVal(NULL)

    lapply(all_sw, function(id_sw) {
      observeEvent(input[[id_sw]], {
        if (input[[id_sw]]) {
          others <- all_sw[all_sw != id_sw]
          for (other in others) {
            updateCheckboxInput(session, other, value = FALSE)
          }
          active_sw_id(id_sw)
        }
      }, ignoreInit = TRUE)
    })

    # 2. Lógica de Navegación (Switches -> Tabs)
    active_tab_id <- reactiveVal(NULL)

    observe({
      target <- "tab_welcome"
        ######################################
      if (isTRUE(input$sw_dataset)) {
        target <- "tab_dataset"
      } else if (isTRUE(input$sw_tool)) {
        target <- "tab_tool"
      } else if (isTRUE(input$sw_script)) {
        target <- "tab_script"
        ######################################
      } else if (isTRUE(input$sw_theory)) {
        target <- "tab_theory"
      } else if (isTRUE(input$sw_bibliography)) {
        target <- "tab_bibliography"
      } else if (isTRUE(input$sw_cite)) {
        target <- "tab_cite"
        ######################################
      } else if (isTRUE(input$sw_settings)) {
        target <- "tab_settings"
      } else if (isTRUE(input$sw_shiny)) {
        target <- "tab_shiny"
      } else if (isTRUE(input$sw_asesor)) {
        target <- "tab_asesor"
        ######################################
      } else if (isTRUE(input$sw_script_comments)) {
        target <- "tab_script_comments"
      } else if (isTRUE(input$sw_script_basic)) {
        target <- "tab_script_basic"
        ######################################
      } else if (isTRUE(input$sw_reporting)) {
        target <- "tab_reporting"
      }
      active_tab_id(target)

      updateTabsetPanel(session, "engine_switcher", selected = target)
    })



    ############################################################################

    # 1. Módulos Internos
    rlist_import <- mod_import_server(id = "my_ns_import")
    rlist_tool   <- mod_tools_server(id = "my_ns_tool")

    resultados <- PACK_mod_main_server(
      id = "mi_app",
      df_input = reactive(mtcars),
      show_debug = TRUE
    )

    ############################################################################


    # ==========================================================================
    # LÓGICA DE CONTROL POR ETAPAS (CHECK POINTS)
    # ==========================================================================

    # El Big Bang es la semilla inicial (siempre TRUE)
    bigbang <- TRUE

    # --- STAGE 01: DATASET, TOOLS, SCRIPT ---

    ## 1.1. Dataset Check
    # Verifica si el módulo de importación tiene datos listos
    check_dataset <- reactive({
      isTruthy(rlist_import()) && isTRUE(rlist_import()$is_locked)
    })

    ## 1.2. Tool Check
    # Verifica si se ha seleccionado una herramienta válida
    check_tool <- reactive({
      isTruthy(rlist_tool()) && !is.null(rlist_tool()$selected_tool)
    })

    ## 1.3 Script Check
    # Aquí podrías verificar si el script se ha generado o cargado
    # Por ahora lo dejamos como TRUE o una condición lógica de tus reactivos
    check_script <- reactive({
      # Ejemplo: isTruthy(res_script()$ready)
      TRUE
    })

    ## 1.4 Check Point Stage 01
    # Consolidación del estado de la Etapa 1
    stage01_status <- reactive({
      previous <- bigbang

      # Vector de validaciones individuales de esta etapa
      vector_each_check <- c(
        dataset = check_dataset(),
        tool    = check_tool(),
        script  = check_script()
      )

      # Combinamos con el estado previo (Big Bang)
      all_done <- c(previous = previous, vector_each_check)

      # La etapa está terminada solo si TODOS los checks son TRUE
      is_done <- all(all_done)

      list(
        checks = vector_each_check,
        all_done = all_done,
        is_done = is_done
      )
    })

    # Ejemplo de uso: Mostrar en consola cuando Stage 01 esté completo
    observe({
      if (stage01_status()$is_done) {
        message("--- [STAGE 01 COMPLETE] ---")
      }
    })



  })
}
