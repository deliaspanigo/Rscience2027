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


mod_02_00_rscience_ui <- function(id) {
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
            tabPanelBody("tab_dataset", div(class="p-3", mod_02_01_dataset_ui(ns("my_ns_dataset")))),
            tabPanelBody("tab_tool", div(mod_02_02_00_tool_ui(ns("my_ns_tool")))),
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
mod_02_00_rscience_server <- function(id) {
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
    res_dataset <- mod_02_01_dataset_server(id="my_ns_dataset")
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
    res_tool    <- mod_02_02_00_tool_server(id="my_ns_tool")
    tool_is_locked <- reactive({
      # 1. Capturamos el estado del módulo
      data_mod <- res_tool()

      #print(data_mod)
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
    selected_script_tool_subfolder <- reactive({
        "tool_0001_script_001"
    })

    # 1.4.
    # --- En mod_rscience_server ---

    # 1.4. Ejecución del módulo
    res_settings <- mod_PACK_settings_server(
      id = "my_ns_PACK_settings",
      df_input = reactive(mtcars), # Asegúrate de que esto sea reactivo
      show_debug = FALSE
    )

    # 2. Evaluación del Candado
    settings_is_locked <- reactive({
      data_mod <- res_settings() # Invocamos el reactivo que devuelve el módulo

      # IMPORTANTE: Según el código anterior, el módulo devuelve:
      # list("is_ready_for_analysis" = TRUE/FALSE, "settings" = ...)
      # O si usaste la versión simplificada: list("is_locked" = TRUE/FALSE, ...)

      # Ajustamos para que busque la propiedad correcta:
      if (is.null(data_mod)) return(FALSE)

      # Si el módulo devuelve una lista con 'is_ready_for_analysis' (la versión consolidada)
      # o con 'is_locked' (la versión por submódulos)
      isTRUE(data_mod$is_all_locked)
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
    step_names <- sprintf("step%02d", 1:12)
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

      lapply(all_sw, shinyjs::disable)

      rlist_stone_steps$step01_done  <- TRUE
      print("step01_ready")
    })
    observeEvent(rlist_stone_steps$step01_done, {

      the_status <- rlist_stone_steps$step01_done

      if(the_status){
        rlist_stone_steps$step02_ready  <- TRUE
      } else {
        rlist_stone_steps$step02_ready  <- FALSE
      }
      print("step01_done")

    })

    # Step 02: Dataset Engine
    observeEvent(rlist_stone_steps$step02_ready, {

      the_status <- rlist_stone_steps$step02_ready

      if(the_status){
        shinyjs::enable("sw_dataset")
        rlist_stone_steps$step02_done  <- TRUE
      } else {
        shinyjs::disable("sw_dataset")
        rlist_stone_steps$step02_done  <- FALSE
      }
      print("step02_ready")

    })
    observeEvent(rlist_stone_steps$step02_done, {

      the_status <- rlist_stone_steps$step02_done

      if(the_status){
        rlist_stone_steps$step03_ready  <- TRUE
      } else {
        rlist_stone_steps$step03_ready  <- FALSE
      }

      print("step02_done")
    })

    # Step 03: Tool Engine
    observeEvent(rlist_stone_steps$step03_ready, {

      the_status <- rlist_stone_steps$step03_ready

      if(the_status){
        shinyjs::enable("sw_tool")
        rlist_stone_steps$step03_done  <- TRUE
      } else {
        shinyjs::disable("sw_tool")
        rlist_stone_steps$step03_done  <- FALSE
      }


    })
    special_activator01 <- reactive({
      is_step03_ok  <- isTRUE(rlist_stone_steps$step03_done)
      is_tool_ok    <- isTRUE(tool_is_locked())

      # 2. Estado Final (Intersección lógica)
      final_status <- is_tool_ok && is_step03_ok

      return(final_status)
    })
    observeEvent(special_activator01(), {

      # 1. Validación estricta de condiciones
      # Usamos isTRUE para asegurar un resultado Booleano (TRUE/FALSE)
      # incluso si los reactivos están naciendo o son NULL.
      final_status <- special_activator01()


      if (final_status) {
        rlist_stone_steps$step04_ready <- TRUE
        message("--- [RScience] Step 04 Ready: Infrastructure Verified ---")
      } else {
        rlist_stone_steps$step04_ready <- FALSE
        # Opcional: Si el paso 4 depende de esto, podrías resetear su 'done' también
        rlist_stone_steps$step04_done  <- FALSE


      }
    }, ignoreInit = TRUE)

    # Step 04: Script Engine (Depende del Candado de Tool)
    observeEvent(rlist_stone_steps$step04_ready, {

      the_status <- rlist_stone_steps$step04_ready

      if(the_status){
        shinyjs::enable("sw_script")
        rlist_stone_steps$step04_done  <- TRUE
      } else {
        shinyjs::disable("sw_script")
        updateCheckboxInput(session, "sw_script", value = FALSE)
        rlist_stone_steps$step04_done  <- FALSE

      }


    })
    observeEvent(rlist_stone_steps$step04_done, {

      the_status <- rlist_stone_steps$step04_done

      if(the_status){
        rlist_stone_steps$step05_ready  <- TRUE
      } else {
        rlist_stone_steps$step05_ready  <- FALSE
      }
    })


    # Step 05: Activator 02 - Check Dataset Tool y script (Filtro de seguridad)
    observeEvent(rlist_stone_steps$step05_ready, {

      the_status <- rlist_stone_steps$step05_ready

      if(the_status){
        rlist_stone_steps$step05_done <- TRUE
      } else {
        rlist_stone_steps$step05_done <- FALSE
      }

    })
    special_activator02 <- reactive({
      is_step05_done  <- isTRUE(rlist_stone_steps$step05_done)
      is_tool_ok    <- isTRUE(dataset_tool_script_are_locked())

      # 2. Estado Final (Intersección lógica)
      final_status <- is_tool_ok && is_step05_done

      return(final_status)
    })
    observeEvent(special_activator02(), {

      # 1. Validación estricta de condiciones
      # Usamos isTRUE para asegurar un resultado Booleano (TRUE/FALSE)
      # incluso si los reactivos están naciendo o son NULL.
      final_status <- special_activator02()


      if (final_status) {
        rlist_stone_steps$step06_ready <- TRUE
        message("--- [RScience] Step 04 Ready: Infrastructure Verified ---")
      } else {
        rlist_stone_steps$step06_ready <- FALSE
        # Opcional: Si el paso 6 depende de esto, podrías resetear su 'done' también
        rlist_stone_steps$step06_done  <- FALSE


      }
    }, ignoreInit = TRUE)



    # --- FASE DE INFORMACIÓN: DESGLOSE SECUENCIAL ---

    # Step 06: Open Theory, Bibliography, Cite
    observeEvent(rlist_stone_steps$step06_ready, {

      the_status <- rlist_stone_steps$step06_ready
      vector_selected_wd <- c("sw_theory", "sw_bibliography", "sw_cite")

      if(the_status){
        lapply(vector_selected_wd, shinyjs::enable)
        rlist_stone_steps$step06_done  <- TRUE
      } else {
        lapply(vector_selected_wd, shinyjs::disable)
        rlist_stone_steps$step06_done  <- FALSE
      }


    })
    observeEvent(rlist_stone_steps$step06_done, {

      the_status <- rlist_stone_steps$step06_done

      if(the_status){
        rlist_stone_steps$step07_ready  <- TRUE
      } else {
        rlist_stone_steps$step07_ready  <- FALSE
      }
    })



    # Step 07: Activator 03 - Control Previo Settings
    observeEvent(rlist_stone_steps$step07_ready, {

      the_status <- rlist_stone_steps$step07_ready

      if(the_status){
        rlist_stone_steps$step07_done <- TRUE
      } else {
        rlist_stone_steps$step07_done <- FALSE
      }

    })
    special_activator03 <- reactive({
      is_step07_done  <- isTRUE(rlist_stone_steps$step07_done)
      is_tool_ok    <- isTRUE(dataset_tool_script_are_locked())

      # 2. Estado Final (Intersección lógica)
      final_status <- is_tool_ok && is_step07_done

      return(final_status)
    })
    observeEvent(special_activator03(), {

      # 1. Validación estricta de condiciones
      # Usamos isTRUE para asegurar un resultado Booleano (TRUE/FALSE)
      # incluso si los reactivos están naciendo o son NULL.
      final_status <- special_activator03()


      if (final_status) {
        rlist_stone_steps$step08_ready <- TRUE
        message("--- [RScience] Step 04 Ready: Infrastructure Verified ---")
      } else {
        rlist_stone_steps$step08_ready <- FALSE
        # Opcional: Si el paso 6 depende de esto, podrías resetear su 'done' también
        rlist_stone_steps$step08_done  <- FALSE


      }
    }, ignoreInit = TRUE)


    # Step 08: Open Settings
    observeEvent(rlist_stone_steps$step08_ready, {

      the_status <- rlist_stone_steps$step08_ready
      vector_selected_wd <- c("sw_settings")

      if(the_status){
        lapply(vector_selected_wd, shinyjs::enable)
        rlist_stone_steps$step08_done  <- TRUE
      } else {
        lapply(vector_selected_wd, shinyjs::disable)
        rlist_stone_steps$step08_done  <- FALSE
      }


    })
    observeEvent(rlist_stone_steps$step08_done, {

      the_status <- rlist_stone_steps$step08_done

      if(the_status){
        rlist_stone_steps$step09_ready  <- TRUE
      } else {
        rlist_stone_steps$step09_ready  <- FALSE
      }
    })

    # Step 09: Activator 04 - Data, tool, sript and settings OK.
    observeEvent(rlist_stone_steps$step09_ready, {

      the_status <- rlist_stone_steps$step09_ready

      if(the_status){
        rlist_stone_steps$step09_done <- TRUE
      } else {
        rlist_stone_steps$step09_done <- FALSE
      }

    })
    special_activator04 <- reactive({
      is_step09_done  <- isTRUE(rlist_stone_steps$step09_done)
      is_tool_ok      <- isTRUE(dataset_tool_script_settings_are_locked())

      # 2. Estado Final (Intersección lógica)
      final_status <- is_tool_ok && is_step09_done

      return(final_status)
    })
    observeEvent(special_activator04(), {

      # 1. Validación estricta de condiciones
      # Usamos isTRUE para asegurar un resultado Booleano (TRUE/FALSE)
      # incluso si los reactivos están naciendo o son NULL.
      final_status <- special_activator04()


      if (final_status) {
        rlist_stone_steps$step10_ready <- TRUE
        message("--- [RScience] Step 10 Ready: Infrastructure Verified ---")
      } else {
        rlist_stone_steps$step12_ready <- FALSE
        # Opcional: Si el paso 6 depende de esto, podrías resetear su 'done' también
        rlist_stone_steps$step12_done  <- FALSE


      }
    }, ignoreInit = TRUE)


    # Step 10: Open shiny, asesor, script_comments, script_basic, reporting
    observeEvent(rlist_stone_steps$step10_ready, {

      the_status <- rlist_stone_steps$step10_ready
      vector_selected_wd <- c("sw_shiny", "sw_asesor", "sw_script_comments", "sw_script_basic", "sw_reporting")

      if(the_status){
        lapply(vector_selected_wd, shinyjs::enable)
        rlist_stone_steps$step10_done  <- TRUE
      } else {
        lapply(vector_selected_wd, shinyjs::disable)
        rlist_stone_steps$step10_done  <- FALSE
      }


    })
    # observeEvent(rlist_stone_steps$step12_done, {
    #
    #   the_status <- rlist_stone_steps$step06_done
    #
    #   if(the_status){
    #     rlist_stone_steps$step07_ready  <- TRUE
    #   } else {
    #     rlist_stone_steps$step07_ready  <- FALSE
    #   }
    # })

    # --- FASE DE ANÁLISIS (Steps 10) ---





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
        ###rlist_stone_steps$step02_ready <- TRUE

      } else {

        shinyjs::runjs(sprintf(
          "$('#%s').html('Dataset Selection <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_dataset")
        ))

        ###rlist_stone_steps$step02_ready <- FALSE
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
        ###rlist_stone_steps$step03_ready <- FALSE

      } else {

        shinyjs::runjs(sprintf(
          "$('#%s').html('Tool Engine <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_tool")
        ))

        ###rlist_stone_steps$step03_ready <- FALSE
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
        ###rlist_stone_steps$step02$is_done <- TRUE

      } else {

        shinyjs::runjs(sprintf(
          "$('#%s').html('Script Engine <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_script")
        ))

        ###rlist_stone_steps$step02$is_done <- FALSE
      }
    })

    # --- En mod_rscience_server ---

    observeEvent(settings_is_locked(), {
      the_status <- settings_is_locked()

      if (the_status) {
        # ESTADO: COMPLETADO (Verde)
        shinyjs::runjs(sprintf(
          "$('#%s').html('Settings <i class=\"fa fa-lock\" style=\"color:#28a745; margin-left:5px;\"></i>');",
          ns("label_settings")
        ))

        # Activamos la fase final de análisis (Step 10)
        ###rlist_stone_steps$step10_ready <- TRUE

      } else {
        # ESTADO: PENDIENTE (Naranja Animado)
        shinyjs::runjs(sprintf(
          "$('#%s').html('Settings <i class=\"fa fa-lock-open\" style=\"color:#ff9100; margin-left:5px; animation: pulse 1.5s infinite;\"></i>');",
          ns("label_settings")
        ))
      }
    })
  })
}
