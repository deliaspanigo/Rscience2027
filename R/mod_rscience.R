# ==============================================================================
# MÓDULO RSCIENCE (UI) - v.0.0.1 ENCAPSULADO & CYAN EDITION
# ==============================================================================
library("bslib")
library("shiny")

path_tool <- system.file("shiny", "fn03_tool_script", "tool_0001_script_001", "f01_settings",
                         package = "Rscience2027")

# Para cargar el pack_module.R específicamente:
source(file.path(path_tool, "mod_PACK_settings.R"))

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
        #", ns("my_ns_dataset-preview"), " table.dataTable thead th {
          background-color: #00d4ff !important;
          color: white !important;
        }

        #", ns("my_ns_dataset-preview"), " table.dataTable tbody tr.odd {
          background-color: #e6faff !important;
        }

        #", ns("my_ns_dataset-preview"), " table.dataTable tbody tr.even {
          background-color: #ffffff !important;
        }

        #", ns("my_ns_dataset-preview"), " table.dataTable tbody tr:hover {
          background-color: #ccf5ff !important;
        }


        @keyframes pulse {
        0% { opacity: 1; }
        50% { opacity: 0.4; }
        100% { opacity: 1; }
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
                  input_switch(ns("sw_dataset"), label = tags$span(id = ns("label_dataset"), "Dataset Selection"), value = TRUE),
                  input_switch(ns("sw_tool"),    label = tags$span(id = ns("label_tool"),    "Tool Engine")     , value = FALSE),
                  input_switch(ns("sw_script"),    label = tags$span(id = ns("label_script"),    "Script Engine")     , value = FALSE),
                  hr(),
                  #######################################################################################
                  div(class = "section-title", "2. Information Phase"),
                  input_switch(ns("sw_theory"), "Theory", value = FALSE),
                  input_switch(ns("sw_bibliography"), "Bibliography", value = FALSE),
                  input_switch(ns("sw_cite"), "Cite Rscience!", value = FALSE),
                  hr(),
                  #######################################################################################
                  div(class = "section-title", "3. Data Analysis Phase"),
                  input_switch(ns("sw_settings"),    label = tags$span(id = ns("label_settings"),    "Settings")     , value = FALSE),
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
            tabPanelBody("tab_dataset", div(class="p-3", mod_dataset_ui(ns("my_ns_dataset")))),
            tabPanelBody("tab_tool", div(mod_tools_ui(ns("my_ns_tool")))),
            tabPanelBody("tab_script", "Waiting for script..."),
            #######################################################################################
            tabPanelBody("tab_theory", "Waiting for Theory..."),
            tabPanelBody("tab_bibliography", "Waiting for bibliography..."),
            tabPanelBody("tab_cite", "Waiting for Cite..."),
            #######################################################################################
            tabPanelBody("tab_settings", div(class="p-4", mod_PACK_settings_ui(ns("my_ns_PACK_settings")))),
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




    ############################################################################
    # 1. Módulos Internos

    # 1.1 Dataset
    res_dataset <- mod_dataset_server(id="my_ns_dataset")
    dataset_is_locked <- reactive({
      # 1. Capturamos el estado del módulo
      data_mod <- res_dataset()

      # 2. Si el módulo no ha devuelto nada (NULL), asumimos que NO está bloqueado
      if (is.null(data_mod)) {
        return(FALSE)
      }

      # 3. Verificamos el valor de is_locked de forma segura
      # isTRUE() garantiza que si el valor es NA o NULL, devuelva FALSE
      isTRUE(data_mod$is_locked)
    })

    # 1.2. Tool
    res_tool    <- mod_tools_server(id="my_ns_tool")
    tool_is_locked <- reactive({
      # 1. Capturamos el estado del módulo
      data_mod <- res_tool()

      # 2. Si el módulo no ha devuelto nada (NULL), asumimos que NO está bloqueado
      if (is.null(data_mod)) {
        return(FALSE)
      }

      # 3. Verificamos el valor de is_locked de forma segura
      # isTRUE() garantiza que si el valor es NA o NULL, devuelva FALSE
      isTRUE(data_mod$is_locked)
    })

    # 1.3. Script
    script_is_locked <- reactive({
      T
    })

    # 1.4.
    resultados <- mod_PACK_settings_server(
      id = "my_ns_PACK_settings",
      df_input = reactive(mtcars),
      show_debug = TRUE
    )
    settings_is_locked <- reactive({
      T
    })

    # Combined status for both Dataset and Tool
    dataset_tool_are_locked <- reactive({
      # Since both underlying reactives are already sanitized (TRUE/FALSE),
      # we just need to verify that BOTH are TRUE.

      dataset_ok <-  isTRUE(dataset_is_locked()) # Returns TRUE/FALSE
      tool_ok    <-  isTRUE(tool_is_locked())    # Returns TRUE/FALSE

      # Logical AND
      return(dataset_ok && tool_ok)
    })

    dataset_tool_script_are_locked <- reactive({
      # Usamos isTRUE para que si alguno es NULL/NA, el resultado sea FALSE y no un error
      dataset_ok <- isTRUE(dataset_is_locked())
      tool_ok    <- isTRUE(tool_is_locked())
      script_ok  <- isTRUE(script_is_locked())

      # Retorna TRUE solo si los tres están bloqueados (verdes)
      return(dataset_ok && tool_ok && script_ok)
    })

    dataset_tool_script_settings_are_locked  <- reactive({
      # Usamos isTRUE para que si alguno es NULL/NA, el resultado sea FALSE y no un error
      dataset_ok <- isTRUE(dataset_is_locked())
      tool_ok    <- isTRUE(tool_is_locked())
      script_ok  <- isTRUE(script_is_locked())
      settings_ok <- isTRUE(settings_is_locked())

      # Retorna TRUE solo si los tres están bloqueados (verdes)
      return(dataset_ok && tool_ok && script_ok && settings_ok)
    })

    # 1. Definición Plana de Estados (Evita colisiones de invalidación)
    # Creamos un vector de nombres para automatizar la creación de reactiveValues
    step_names <- sprintf("step%02d", 1:16)
    initial_values <- list()
    for(s in step_names) {
      initial_values[[paste0(s, "_ready")]] <- FALSE
      initial_values[[paste0(s, "_done")]]  <- FALSE
    }

    rlist_stone_steps <- do.call(reactiveValues, initial_values)

    # --- INICIO DEL FLUJO ---
    observe({
      rlist_stone_steps$step01_ready <- TRUE
    })

    # Step 01: Lockdown inicial
    observeEvent(rlist_stone_steps$step01_ready, {
      req(rlist_stone_steps$step01_ready, !rlist_stone_steps$step01_done)

      lapply(all_sw, shinyjs::disable)

      rlist_stone_steps$step01_done  <- TRUE
      rlist_stone_steps$step02_ready <- TRUE
    })

    # Step 02: Dataset Engine
    observeEvent(rlist_stone_steps$step02_ready, {
      req(rlist_stone_steps$step02_ready, !rlist_stone_steps$step02_done)

      shinyjs::enable("sw_dataset")

      rlist_stone_steps$step02_done  <- TRUE
      rlist_stone_steps$step03_ready <- TRUE
    })

    # Step 03: Tool Engine
    observeEvent(rlist_stone_steps$step03_ready, {
      req(rlist_stone_steps$step03_ready, !rlist_stone_steps$step03_done)

      shinyjs::enable("sw_tool")

      rlist_stone_steps$step03_done  <- TRUE
      # Aquí el flujo espera a que tool_is_locked() sea TRUE para el Step 04
    })

    # Step 04: Script Engine (Depende del Candado de Tool)
    observe({
      # Esta lógica debe ser reactiva al candado
      status_tool <- isTRUE(tool_is_locked())

      if(status_tool && rlist_stone_steps$step03_done) {
        rlist_stone_steps$step04_ready <- TRUE
      } else {
        rlist_stone_steps$step04_ready <- FALSE
        rlist_stone_steps$step04_done  <- FALSE
        shinyjs::disable("sw_script")
        updateCheckboxInput(session, "sw_script", value = FALSE)
      }
    })

    observeEvent(rlist_stone_steps$step04_ready, {
      req(rlist_stone_steps$step04_ready, !rlist_stone_steps$step04_done)

      shinyjs::enable("sw_script")

      rlist_stone_steps$step04_done  <- TRUE
      rlist_stone_steps$step05_ready <- TRUE # Habilita la fase de validación
    })


    # Step 05: Check Dataset Tool y script (Filtro de seguridad)
    observe({
      # Esta lógica debe ser reactiva al candado
      status_tool <- isTRUE(dataset_tool_script_are_locked())
      is_ready <- isTRUE(rlist_stone_steps$step05_ready)

      if(status_tool && is_ready) {
        rlist_stone_steps$step05_done <- TRUE
      } else {
        rlist_stone_steps$step05_ready <- FALSE
        rlist_stone_steps$step05_done  <- FALSE
        #shinyjs::disable("sw_script")
        #updateCheckboxInput(session, "sw_script", value = FALSE)
      }
    })
    observeEvent(rlist_stone_steps$step05_ready, {
      req(rlist_stone_steps$step05_ready, !rlist_stone_steps$step05_done)

      # Si llegamos aquí, la infraestructura base está lista
      rlist_stone_steps$step05_done  <- TRUE
      rlist_stone_steps$step06_ready <- TRUE
    })

    # --- FASE DE INFORMACIÓN: DESGLOSE SECUENCIAL ---

    # Step 06: Theory Engine
    observeEvent(rlist_stone_steps$step06_ready, {
      req(rlist_stone_steps$step06_ready, !rlist_stone_steps$step06_done)

      shinyjs::enable("sw_theory")

      # Marcamos como hecho y preparamos el siguiente
      rlist_stone_steps$step06_done  <- TRUE
      rlist_stone_steps$step07_ready <- TRUE
    })

    # Step 07: Bibliography Engine
    observeEvent(rlist_stone_steps$step07_ready, {
      req(rlist_stone_steps$step07_ready, !rlist_stone_steps$step07_done)

      shinyjs::enable("sw_bibliography")

      # Marcamos como hecho y preparamos el siguiente
      rlist_stone_steps$step07_done  <- TRUE
      rlist_stone_steps$step08_ready <- TRUE
    })

    # Step 08: Cite Engine
    observeEvent(rlist_stone_steps$step08_ready, {
      req(rlist_stone_steps$step08_ready, !rlist_stone_steps$step08_done)

      shinyjs::enable("sw_cite")

      # Marcamos como hecho y saltamos al control de la Fase de Análisis
      rlist_stone_steps$step08_done  <- TRUE
      rlist_stone_steps$step09_ready <- TRUE
    })

    # Step 09: Control de Paso a Settings
    observeEvent(rlist_stone_steps$step09_ready, {
      req(rlist_stone_steps$step09_ready)

      # Aquí habilitamos Settings una vez que la Fase de Info está desplegada
      shinyjs::enable("sw_settings")

      rlist_stone_steps$step09_done  <- TRUE
      # El Step 10 se activará solo cuando settings_is_locked() sea TRUE
    })

    observe({
      # Esta lógica debe ser reactiva al candado
      status_tool <- isTRUE(dataset_tool_script_are_locked())
      is_ready <- isTRUE(rlist_stone_steps$step09_ready)

      if(status_tool && is_ready) {
        rlist_stone_steps$step05_done <- TRUE
      } else {
        rlist_stone_steps$step05_ready <- FALSE
        rlist_stone_steps$step05_done  <- FALSE
        #shinyjs::disable("sw_script")
        #updateCheckboxInput(session, "sw_script", value = FALSE)
      }
    })
    observeEvent(rlist_stone_steps$step05_ready, {
      req(rlist_stone_steps$step05_ready, !rlist_stone_steps$step05_done)

      # Si llegamos aquí, la infraestructura base está lista
      rlist_stone_steps$step05_done  <- TRUE
      rlist_stone_steps$step06_ready <- TRUE
    })

    # --- FASE DE ANÁLISIS (Steps 10-16) ---
    observeEvent(rlist_stone_steps$step10_ready, {
      req(rlist_stone_steps$step10_ready)

      # Activación en cascada de herramientas de salida
      lapply(c("sw_settings", "sw_shiny", "sw_asesor",
               "sw_script_comments", "sw_script_basic", "sw_reporting"), shinyjs::enable)

      message("--- [RScience] Analysis Phase Fully Enabled ---")
    })


    ##########################################

    # Modificadores de sw (Feedback visual de progreso)
    # Modificadores de sw (Feedback visual de seguridad)
    observeEvent(dataset_is_locked(), {
      # 1. Extraemos el contenido del reactivo del módulo
      the_status <- dataset_is_locked()

      # 2. Lógica de Iconos y Estados
      if (the_status) {

        # ESTADO: COMPLETADO (Candado Cerrado Verde)
        shinyjs::runjs(sprintf(
          "$('#%s').html('Dataset Selection <i class=\"fa fa-lock\" style=\"color:#28a745; margin-left:5px;\"></i>');",
          ns("label_dataset")
        ))

        # Marcamos la lógica interna como terminada
        rlist_stone_steps$step02$is_done <- TRUE

      } else {

        shinyjs::runjs(sprintf(
          "$('#%s').html('Dataset Selection <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_dataset")
        ))

        #rlist_stone_steps$step02$is_done <- FALSE
      }
    })


    observeEvent(tool_is_locked(), {
      # 1. Extraemos el contenido del reactivo del módulo
      the_status <- tool_is_locked()

      # 2. Lógica de Iconos y Estados
      if (the_status) {

        # ESTADO: COMPLETADO (Candado Cerrado Verde)
        shinyjs::runjs(sprintf(
          "$('#%s').html('Tool Engine <i class=\"fa fa-lock\" style=\"color:#28a745; margin-left:5px;\"></i>');",
          ns("label_tool")
        ))

        # Marcamos la lógica interna como terminada
        rlist_stone_steps$step02$is_done <- TRUE

      } else {

        shinyjs::runjs(sprintf(
          "$('#%s').html('Tool Engine <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_tool")
        ))

        #rlist_stone_steps$step02$is_done <- FALSE
      }
    })

    observeEvent(script_is_locked(), {
      # 1. Extraemos el contenido del reactivo del módulo
      the_status <- script_is_locked()

      # 2. Lógica de Iconos y Estados
      if (the_status) {

        # ESTADO: COMPLETADO (Candado Cerrado Verde)
        shinyjs::runjs(sprintf(
          "$('#%s').html('Script Engine <i class=\"fa fa-lock\" style=\"color:#28a745; margin-left:5px;\"></i>');",
          ns("label_script")
        ))

        # Marcamos la lógica interna como terminada
        rlist_stone_steps$step02$is_done <- TRUE

      } else {

        shinyjs::runjs(sprintf(
          "$('#%s').html('Script Engine <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_script")
        ))

        #rlist_stone_steps$step02$is_done <- FALSE
      }
    })

    observeEvent(settings_is_locked(), {
      # 1. Extraemos el contenido del reactivo del módulo
      the_status <- settings_is_locked()

      # 2. Lógica de Iconos y Estados
      if (the_status) {

        # ESTADO: COMPLETADO (Candado Cerrado Verde)
        shinyjs::runjs(sprintf(
          "$('#%s').html('Settings <i class=\"fa fa-lock\" style=\"color:#28a745; margin-left:5px;\"></i>');",
          ns("label_settings")
        ))

        # Marcamos la lógica interna como terminada
        #rlist_stone_steps$step02$is_done <- TRUE

      } else {

        shinyjs::runjs(sprintf(
          "$('#%s').html('Setting <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_settings")
        ))

        #rlist_stone_steps$step02$is_done <- FALSE
      }
    })
  })
}
