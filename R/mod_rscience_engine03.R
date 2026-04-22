mod_rscience_engine03_ui <- function(id) {
  ns <- NS(id)

  # Registro de recursos
  www_folder <- system.file("www", package = "Rscience2027")
  if (www_folder == "") www_folder <- "www"
  try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

  tagList(
    tags$head(
      useShinyjs(),
      tags$link(rel = "stylesheet", type = "text/css",
                href = paste0("RS-STYLES/style_000.css?v=", as.numeric(Sys.time()))),
      tags$style(HTML(paste0("
  /* --- RESET GLOBAL --- */
  html, body {
    margin: 0 !important;
    padding: 0 !important;
    min-height: 100vh;
    width: 100%;
    background-color: #0b1218;
  }

  /* Reset de contenedores automáticos de Shiny/Bootstrap */
  .container-fluid, .tab-pane, .tab-content {
    padding: 0 !important;
    margin: 0 !important;
    min-height: 100vh;
    width: 100% !important;
  }

  /* --- ESTILOS ESPECÍFICOS DEL MÓDULO --- */

  #", id, "-container.rs-module-container {
    min-height: 100vh;
    width: 100%;
    display: flex;
    flex-direction: column;
    margin: 0 !important;
    padding: 0 !important;
    background: #0b1218;
  }

  #", id, "-container .rs-dashboard-wrapper {
    display: flex;
    flex-direction: column;
    min-height: 100vh;
    width: 100%;
    margin: 0 !important;
    padding: 0 !important;
  }

  #", id, "-container .rs-dashboard-header {
    flex: 0 0 auto;
    background: #0b1218;
    border-bottom: 6px solid #00d4ff;
    padding: 10px 20px 15px 20px;
    display: flex;
    align-items: center;
    gap: 30px;
    z-index: 1000;
    position: sticky;
    top: 0;
    margin: 0 !important;
  }

  #", id, "-container .main-content-area {
    flex: 1 0 auto;
    padding: 20px;
    background: #0b1218;
    margin: 0;
  }

  #", id, "-container .rs-brand img {
    width: 140px;
  }

  #", id, "-container .rs-nav-group {
    display: flex;
    background: rgba(255,255,255,0.05);
    border-radius: 50px;
    padding: 5px;
    gap: 5px;
  }

  #", id, "-container .nav-btn-custom {
    border: none;
    background: transparent;
    color: #8a9ba8;
    padding: 8px 18px;
    border-radius: 40px;
    font-size: 0.85rem;
    font-weight: 600;
    transition: 0.3s;
    display: flex;
    align-items: center;
    gap: 8px;
  }

  #", id, "-container .nav-btn-custom:hover {
    background: rgba(0, 212, 255, 0.1);
    color: #00d4ff;
  }

  #", id, "-container .nav-btn-custom.active {
    background: #00d4ff;
    color: #000;
  }

  #", id, "-container .nav-divider {
    width: 2px;
    height: 25px;
    background: rgba(255,255,255,0.1);
    align-self: center;
  }

  /* --- ETIQUETAS SOBRE EL BORDE --- */
  #", id, "-container .rs-header-status-bar {
    position: absolute;
    bottom: 10px;
    right: 20px;
    display: flex;
    gap: 20px;
    pointer-events: none;
  }

  #", id, "-container .rs-status-item {
    font-size: 0.65rem;
    color: #0b1218;
    background: #00d4ff;
    padding: 1px 10px;
    border-radius: 4px 4px 0 0;
    font-family: 'Consolas', 'Monaco', monospace;
    text-transform: uppercase;
    font-weight: bold;
    letter-spacing: 0.5px;
    display: flex;
    align-items: center;
    gap: 5px;
  }

  #", id, "-container .rs-status-item span {
    color: #000000;
  }

  /* --- SCROLL SUAVE Y NATURAL --- */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }

  ::-webkit-scrollbar-track {
    background: #0b1218;
  }

  ::-webkit-scrollbar-thumb {
    background: #2a3b47;
    border-radius: 4px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: #00d4ff;
  }

  /* Firefox scroll */
  * {
    scrollbar-width: thin;
    scrollbar-color: #2a3b47 #0b1218;
  }
"))),
    ),

    # --- CONTENEDOR RAÍZ CON ID ÚNICO ---
    div(id = ns("container"), class = "rs-module-container",
        div(class = "rs-dashboard-wrapper",

            # --- HEADER SUPERIOR ---
            div(class = "rs-dashboard-header",
                div(class = "rs-brand", img(src = "WWW-FOLDER/Rscience_logo_sticker.png")),

                # Grupo 1
                div(class = "rs-nav-group",
                    actionButton(ns("c_data"),   label = list(icon("database"), "Dataset"), class = "nav-btn-custom active"),
                    actionButton(ns("c_tool"),   label = list(icon("tools"), "Tool"),        class = "nav-btn-custom"),
                    actionButton(ns("c_script"), label = list(icon("code"), "Script"),      class = "nav-btn-custom")
                ),

                div(class = "nav-divider"),

                # Grupo 2
                div(class = "rs-nav-group",
                    actionButton(ns("c_settings"), label = list(icon("sliders"), "Settings"), class = "nav-btn-custom"),
                    actionButton(ns("c_play"),     label = list(icon("play"), "Run"),          class = "nav-btn-custom"),
                    hidden(actionButton(ns("c_DEBUG"), label = "DEBUG", class = "nav-btn-custom"))
                ),

                div(class = "nav-divider"),

                # Grupo 3
                div(class = "rs-nav-group",
                    actionButton(ns("c_asa"),                  label = "ASA",     class = "nav-btn-custom"),
                    actionButton(ns("c_pdf"),                  label = "PDF",     class = "nav-btn-custom"),
                    actionButton(ns("c_shiny_output"),        label = "Output",  class = "nav-btn-custom"),
                    actionButton(ns("c_script_and_comments"), label = "Scripts", class = "nav-btn-custom")
                ),

                div(class = "nav-divider"),

                # Grupo 4
                div(class = "rs-nav-group",
                    actionButton(ns("c_theory"),       label = icon("book"),            class = "nav-btn-custom"),
                    actionButton(ns("c_bibliography"), label = icon("list"),            class = "nav-btn-custom"),
                    actionButton(ns("c_cite"),         label = icon("quote-left"),     class = "nav-btn-custom"),
                    actionButton(ns("c_faqs"),         label = icon("question-circle"), class = "nav-btn-custom")
                ),

                # --- ETIQUETAS ABSOLUTAS ---
                div(class = "rs-header-status-bar",
                    span(class = "rs-status-item", icon("database"), "Dataset: ", textOutput(ns("name_dataset"), inline = TRUE)),
                    span(class = "rs-status-item", icon("microchip"), "Tool: ",    textOutput(ns("name_tool"),    inline = TRUE)),
                    span(class = "rs-status-item", icon("file-code"), "Script: ",  textOutput(ns("name_script"),  inline = TRUE))
                )
            ),

            mod_rscience_engine03_main_ui(id = id)
        )
    )
  )
}


mod_rscience_engine03_main_ui <- function(id) {
  ns <- NS(id)
# --- ÁREA DE CONTENIDO ---
div(class = "main-content-area",
    navset_hidden(
      id = ns("main_navset"),
      nav_panel_hidden("c_data",   mod_02_01_dataset_ui(id = ns("my_ns_dataset"))),
      nav_panel_hidden("c_tool",   mod_02_02_00_tool_ui(id = ns("my_ns_tool"))),
      nav_panel_hidden("c_script", mod_02_03_00_script_ui(id = ns("my_ns_script"))),
      nav_panel_hidden("c_settings", mod_04_00_settings_ui(id = ns("my_ns_collector02_settings"))),
      nav_panel_hidden("c_play", mod_10_00_proccessing_ui(id = ns("pipeline_1"))),
      nav_panel_hidden("c_DEBUG", uiOutput(ns("show_debug"))),
      nav_panel_hidden("c_script_and_comments", mod_11_A_script_and_comments_ui(id = ns("pipeline_333"))),
      nav_panel_hidden("c_shiny_output", mod_11_B_shiny_output_ui(id = ns("pather_shiny_output"))),
      nav_panel_hidden("c_asa", mod_11_C_asa_ui(id = ns("pather_asa"))),
      nav_panel_hidden("c_pdf", mod_11_D_pdf_ui(id = ns("pather_pdf"))),
      nav_panel_hidden("c_theory", mod_03_A_theory_ui(ns("txt_1"))),
      nav_panel_hidden("c_bibliography", mod_03_B_bibliography_ui(ns("txt_2"))),
      nav_panel_hidden("c_cite", mod_03_C_cite_ui(ns("txt_3"))),
      nav_panel_hidden("c_faqs", card(card_body("FAQ...")))
    )
)
}

mod_rscience_engine03_server <- function(id, show_debug_tab = F, show_debug_general = F) {
  moduleServer(id, function(input, output, session) {

    # NS
    ns <- session$ns

    # Basics
    internal_show_debug_tab     <- reactive( if(is.function(show_debug_tab)) show_debug_tab() else show_debug_tab)
    internal_show_debug_general <- reactive( if(is.function(show_debug_general)) show_debug_general() else show_debug_general)

    # Show/Hide debug
    observe({
      # Usamos isTRUE para manejar posibles NULLs iniciales
      show_it <- isTRUE(internal_show_debug_tab())

      if (show_it) {
        shinyjs::show("c_DEBUG")
        # Forzamos visibilidad si el CSS de la clase phase-card interfiere
        shinyjs::runjs(sprintf("$('#%s').css('display', 'flex');", ns("c_DEBUG")))
      } else {
        shinyjs::hide("c_DEBUG")
      }
    })

    # Listado maestro de todas las tarjetas para los listeners
    all_cards <- c("c_data", "c_tool", "c_script", "c_settings", "c_play", "c_DEBUG",
                   "c_script_and_comments", "c_shiny_output", "c_asa", "c_pdf",
                   "c_theory", "c_bibliography", "c_cite", "c_faqs")

    # 1. Lógica de Cambio de Pestaña y Brillo
    observeEvent(input$active_card, {
      nav_select("main_navset", selected = input$active_card)

      # Resetear brillo de todas y activar la seleccionada
      lapply(all_cards, function(x) removeClass(x, "active"))
      addClass(input$active_card, "active")
    })

    # 2. Control del Slider Horizontal (Dots)
    observeEvent(input$move, {
      runjs(sprintf("$('#%s').css('transform', 'translateX(%f%%)');", ns("track"), input$move))
      runjs(sprintf("$('.nav-dot-wrapper').removeClass('active');"))
      if(input$move == 0) addClass("dot1", "active")
      if(input$move == -33.333) addClass("dot2", "active")
      if(input$move == -66.666) addClass("dot3", "active")
    })

    # 3. Inyectar Listeners JS para clics en Tarjetas
    lapply(all_cards, function(card_id) {
      runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', '%s', {priority: 'event'}); });",
                    ns(card_id), ns("active_card"), card_id))
    })

    # 4. Inyectar Listeners JS para clics en Dots
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', 0); });", ns("dot1"), ns("move")));
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', -33.333); });", ns("dot2"), ns("move")));
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', -66.666); });", ns("dot3"), ns("move")));

    # Título dinámico
    output$title <- renderText({
      req(input$active_card)
      toupper(gsub("c_", "", input$active_card))
    })

    # Inicialización forzada
    shinyjs::runjs(sprintf("Shiny.setInputValue('%s', 'c_data');", ns("active_card")))



    # OPT 01.01. Dataset -------------------------------------------------------------------------------------------
    rlist_dataset <- mod_02_01_dataset_server(id = "my_ns_dataset", show_debug = internal_show_debug_general())

    output$name_dataset <- renderText({
      req( rlist_dataset())
      flat_rlist_dataset <-  rlist_dataset()

      the_name  <- flat_rlist_dataset$"metadata_dataset"$"name_mod"
      the_name
    })

    # OPT 01.02. Tool -------------------------------------------------------------------------------------------
    rlist_tool <- mod_02_02_00_tool_server(id = "my_ns_tool", show_debug = internal_show_debug_general()) # SIN ns()

    output$name_tool <- renderText({
      req( rlist_tool())
      flat_rlist_tool <-  rlist_tool()

      the_name  <- flat_rlist_tool$"metadata_tree"$"path_mod"
      the_name
    })

    # OPT 01.03. Script -------------------------------------------------------------------------------------------
    HOOK_vector_id_tool_script <- reactive({
      req(rlist_tool())
      flat_rlist_tool <- rlist_tool()
      flat_rlist_tool$metadata_tree$script_id
      })

    rlist_script <-   mod_02_03_00_script_server(id="my_ns_script",
                                                 vector_str_folder_tool_script = HOOK_vector_id_tool_script,
                                                 show_debug = internal_show_debug_general()) # Llamamos a la UI

    output$name_script <- renderText({
      req( rlist_script())
      flat_rlist_script <-  rlist_script()

      the_name  <- flat_rlist_script$"metadata"$"tool_script_name"
      the_name
    })

    # Collector01 - Temporal folder and copying selected tool_script folder from local to temp --------------------
    rlist_collector01 <- reactive({
      # 1. El guardia principal
      req(rlist_script())

      output_list <- list()

      # Local folder path tool-script
      flat_rlist_script <- rlist_script()
      local_folder_path_tool_script <- flat_rlist_script$script_tool_folder_path
      req(!is.null(local_folder_path_tool_script), local_folder_path_tool_script != "")
      check_local_folder_path_tool_script_exists <- dir.exists(local_folder_path_tool_script)
      output_list$local_folder_tool_script <- list()
      output_list$local_folder_tool_script$folder_path   <- local_folder_path_tool_script
      output_list$local_folder_tool_script$folder_exists <- check_local_folder_path_tool_script_exists

      # New temporal folder
      str_time <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
      full_path_temp <- file.path(tempdir(), paste0("Rscience_", str_time))
      check_folder_temp_created <- dir.create(full_path_temp, showWarnings = FALSE, recursive = TRUE)
      output_list$temp_folder <- list()
      output_list$temp_folder$str_time  <- str_time
      output_list$temp_folder$folder_path <- full_path_temp
      output_list$temp_folder$folder_exists <- check_folder_temp_created


      # if (check_folder_temp_created) {
      #   # En Ubuntu, 0755 es lo estándar para carpetas ejecutables/accesibles
      #   # Usamos recursividad para asegurar que los scripts y binarios internos funcionen
      #   system(paste("chmod -R 755", full_path_temp))
      # }

      # Copying files from local to temp
      path_origin <- local_folder_path_tool_script
      path_dest   <- full_path_temp
      copy_status <- file.copy(from = path_origin, to = path_dest, recursive = TRUE, overwrite = TRUE)



      temp_folder_tool_script <- file.path(path_dest, basename(path_origin))
      check_temp_folder_tool_script <- dir.exists(temp_folder_tool_script)

      output_list$temp_folder_tool_script <- list()
      output_list$temp_folder_tool_script$folder_path   <- temp_folder_tool_script
      output_list$temp_folder_tool_script$folder_exists <- check_temp_folder_tool_script


      return(output_list)
    })
    output$debug_collector01_01 <- listviewer::renderJsonedit({
      req(rlist_collector01())
      internal_rlist_collector01 <- rlist_collector01()

      listviewer::jsonedit(listdata = internal_rlist_collector01, mode = "text")
    })
    output$debug_collector01_02 <- listviewer::renderJsonedit({
      req(HOOK_local_folder_path_tool_script(), HOOK_temp_folder_path_tool_script())

      flat_HOOK_local_folder_path_tool_script <- HOOK_local_folder_path_tool_script()
      flat_HOOK_temp_folder_path_tool_script <-  HOOK_temp_folder_path_tool_script()

      the_list <- list(HOOK_local_folder_path_tool_script = flat_HOOK_local_folder_path_tool_script,
                       HOOK_temp_folder_path_tool_script = flat_HOOK_temp_folder_path_tool_script)

      listviewer::jsonedit(listdata = the_list, mode = "text")
    })
    output$show_debug_external_collector01 <- renderUI({
      # Si quieres ver el panel aunque esté vacío, quita el req() de aquí arriba
      # y manéjalo internamente o deja que los jsonedit muestren NULL

      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",



          div(class = "row",
              div(class = "col-md-6",
                  div(class = "section-label",
                      style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 10px;",
                      icon("bug"), " External Debug - Collector 01"),
                  listviewer::jsoneditOutput(ns("debug_collector01_01"), height = "auto"),
                  div(class = "section-label",
                      style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 10px;",
                      icon("bug"), " External Debug - Collector 01"),
                  listviewer::jsoneditOutput(ns("debug_collector01_02"), height = "auto")
              ),
              div(class = "col-md-6",
                  "Parte 2"

              )
          )
      )
    })



    # Collector02 - Modules special files .R from local and temporal ---------------------------------------------
    rlist_collector02 <- reactive({
      req(HOOK_local_folder_path_tool_script(), HOOK_temp_folder_path_tool_script())
      flat_HOOK_local_folder_path_tool_script <- HOOK_local_folder_path_tool_script()
      flat_HOOK_temp_folder_path_tool_script  <-  HOOK_temp_folder_path_tool_script()


      # Folders
      local_folder_path <-  flat_HOOK_local_folder_path_tool_script
      temp_folder_path  <- flat_HOOK_temp_folder_path_tool_script

      # Check folders
      check_folder_local <- dir.exists(local_folder_path)
      check_folder_temp  <- dir.exists(temp_folder_path)

      # List files
      list_files <- list()

      #vector_target_file_name <- c("mod_special_settings.R", "mod_special_theory.R", "mod_special_bibliography.R", "mod_special_cite.R")


      #target <- file.path(p, "f01_shiny_show", "p01_04_settings", "f03_prod", )


      list_files$"file01_04_settings" <- list()
      list_files$"file01_04_settings"$"position"    <- "file01_04_settings"
      list_files$"file01_04_settings"$"file_name"   <- "mod_special_settings.R"
      list_files$"file01_04_settings"$"description" <- "Module for settings from selected tool-script."
      list_files$"file01_04_settings"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p01_04_settings", "f03_prod", list_files$"file01_04_settings"$"file_name")
      list_files$"file01_04_settings"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p01_04_settings", "f03_prod", list_files$"file01_04_settings"$"file_name")
      list_files$"file01_04_settings"$"check_local" <- file.exists(list_files$"file01_04_settings"$"local_file_path")
      list_files$"file01_04_settings"$"check_temp"  <- file.exists(list_files$"file01_04_settings"$"temp_file_path")

      list_files$"file01_05_proccsessing" <- list()
      list_files$"file01_05_proccsessing"$"position"    <- "file01_05_proccsessing"
      list_files$"file01_05_proccsessing"$"file_name"   <- "mod_special_proccessing.R"
      list_files$"file01_05_proccsessing"$"description" <- "Module for settings from selected tool-script."
      list_files$"file01_05_proccsessing"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p01_05_proccessing", "f03_prod", list_files$"file01_05_proccsessing"$"file_name")
      list_files$"file01_05_proccsessing"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p01_05_proccessing", "f03_prod", list_files$"file01_05_proccsessing"$"file_name")
      list_files$"file01_05_proccsessing"$"check_local" <- file.exists(list_files$"file01_05_proccsessing"$"local_file_path")
      list_files$"file01_05_proccsessing"$"check_temp"  <- file.exists(list_files$"file01_05_proccsessing"$"temp_file_path")
      ###########################################################################################################
      list_files$"file02_01_script_and_comments" <- list()
      list_files$"file02_01_script_and_comments"$"position"    <- "file02_01_script_and_comments"
      list_files$"file02_01_script_and_comments"$"file_name"   <- "mod_special_script_and_comments.R"
      list_files$"file02_01_script_and_comments"$"description" <- "Module for settings from selected tool-script."
      list_files$"file02_01_script_and_comments"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p02_01_script_and_comments", "f03_prod", list_files$"file02_01_script_and_comments"$"file_name")
      list_files$"file02_01_script_and_comments"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p02_01_script_and_comments", "f03_prod", list_files$"file02_01_script_and_comments"$"file_name")
      list_files$"file02_01_script_and_comments"$"check_local" <- file.exists(list_files$"file02_01_script_and_comments"$"local_file_path")
      list_files$"file02_01_script_and_comments"$"check_temp"  <- file.exists(list_files$"file02_01_script_and_comments"$"temp_file_path")

      list_files$"file02_02_shiny_output" <- list()
      list_files$"file02_02_shiny_output"$"position"    <- "file02_02_shiny_output"
      list_files$"file02_02_shiny_output"$"file_name"   <- "mod_special_shiny_output.R"
      list_files$"file02_02_shiny_output"$"description" <- "Module for shiny-output from selected tool-script."
      list_files$"file02_02_shiny_output"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p02_02_shiny_output", "f03_prod", list_files$"file02_02_shiny_output"$"file_name")
      list_files$"file02_02_shiny_output"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p02_02_shiny_output", "f03_prod", list_files$"file02_02_shiny_output"$"file_name")
      list_files$"file02_02_shiny_output"$"check_local" <- file.exists(list_files$"file02_02_shiny_output"$"local_file_path")
      list_files$"file02_02_shiny_output"$"check_temp"  <- file.exists(list_files$"file02_02_shiny_output"$"temp_file_path")

      list_files$"file02_03_asa" <- list()
      list_files$"file02_03_asa"$"position"    <- "file02_03_asa"
      list_files$"file02_03_asa"$"file_name"   <- "mod_special_asa.R"
      list_files$"file02_03_asa"$"description" <- "Module for asa from selected tool-script."
      list_files$"file02_03_asa"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p02_03_asa", "f03_prod", list_files$"file02_03_asa"$"file_name")
      list_files$"file02_03_asa"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p02_03_asa", "f03_prod", list_files$"file02_03_asa"$"file_name")
      list_files$"file02_03_asa"$"check_local" <- file.exists(list_files$"file02_03_asa"$"local_file_path")
      list_files$"file02_03_asa"$"check_temp"  <- file.exists(list_files$"file02_03_asa"$"temp_file_path")

      list_files$"file02_04_pdf" <- list()
      list_files$"file02_04_pdf"$"position"    <- "file02_04_pdf"
      list_files$"file02_04_pdf"$"file_name"   <- "mod_special_pdf.R"
      list_files$"file02_04_pdf"$"description" <- "Module for asa from selected tool-script."
      list_files$"file02_04_pdf"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p02_04_pdf", "f03_prod", list_files$"file02_04_pdf"$"file_name")
      list_files$"file02_04_pdf"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p02_04_pdf", "f03_prod", list_files$"file02_04_pdf"$"file_name")
      list_files$"file02_04_pdf"$"check_local" <- file.exists(list_files$"file02_04_pdf"$"local_file_path")
      list_files$"file02_04_pdf"$"check_temp"  <- file.exists(list_files$"file02_04_pdf"$"temp_file_path")


      ###########################################################################################################


      list_files$"file03_01_theory" <- list()
      list_files$"file03_01_theory"$"position"    <- "file03_01_theory"
      list_files$"file03_01_theory"$"file_name"   <- "mod_special_theory.R"
      list_files$"file03_01_theory"$"description" <- "Module theory for selected tool-script."
      list_files$"file03_01_theory"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p03_01_theory", "f03_prod", list_files$"file03_01_theory"$"file_name")
      list_files$"file03_01_theory"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p03_01_theory", "f03_prod", list_files$"file03_01_theory"$"file_name")
      list_files$"file03_01_theory"$"check_local" <- file.exists(list_files$"file03_01_theory"$"local_file_path")
      list_files$"file03_01_theory"$"check_temp"  <- file.exists(list_files$"file03_01_theory"$"temp_file_path")


      list_files$"file03_02_bibliography" <- list()
      list_files$"file03_02_bibliography"$"position"    <- "file03_02_bibliography"
      list_files$"file03_02_bibliography"$"file_name"   <- "mod_special_bibliography.R"
      list_files$"file03_02_bibliography"$"description" <- "Module bibliography for selected tool-script."
      list_files$"file03_02_bibliography"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p03_02_bibliography", "f03_prod", list_files$"file03_02_bibliography"$"file_name")
      list_files$"file03_02_bibliography"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p03_02_bibliography", "f03_prod", list_files$"file03_02_bibliography"$"file_name")
      list_files$"file03_02_bibliography"$"check_local" <- file.exists(list_files$"file03_02_bibliography"$"local_file_path")
      list_files$"file03_02_bibliography"$"check_temp"  <- file.exists(list_files$"file03_02_bibliography"$"temp_file_path")

      list_files$"file03_03_cite" <- list()
      list_files$"file03_03_cite"$"position"    <- "file03_03_cite"
      list_files$"file03_03_cite"$"file_name"   <- "mod_special_cite.R"
      list_files$"file03_03_cite"$"description" <- "Module cite for selected tool-script."
      list_files$"file03_03_cite"$"local_file_path" <- file.path(local_folder_path, "f01_shiny_show", "p03_03_cite", "f03_prod", list_files$"file03_03_cite"$"file_name")
      list_files$"file03_03_cite"$"temp_file_path"  <- file.path(temp_folder_path,  "f01_shiny_show", "p03_03_cite", "f03_prod", list_files$"file03_03_cite"$"file_name")
      list_files$"file03_03_cite"$"check_local" <- file.exists(list_files$"file03_03_cite"$"local_file_path")
      list_files$"file03_03_cite"$"check_temp"  <- file.exists(list_files$"file03_03_cite"$"temp_file_path")


      return(list_files)
    })
    output$debug_collector02_01 <- listviewer::renderJsonedit({
      req(rlist_collector02())
      flat_rlist_collector02 <- rlist_collector02()

      listviewer::jsonedit(listdata = flat_rlist_collector02, mode = "text")
    })
    output$show_debug_external_collector02 <- renderUI({
      # Si quieres ver el panel aunque esté vacío, quita el req() de aquí arriba
      # y manéjalo internamente o deja que los jsonedit muestren NULL

      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",

          div(class = "section-label",
              style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 10px;",
              icon("bug"), " External Debug - Collector 02 - All Module Special"),

          div(class = "row",
              div(class = "col-md-6",
                  # El req() dentro del renderJsonedit ya se encarga de esperar los datos
                  listviewer::jsoneditOutput(ns("debug_collector02_01"), height = "auto")
              )#,
              # div(class = "col-md-6",
              #     listviewer::jsoneditOutput(ns("debug_collector01_02"), height = "auto")
              # )
          )
      )
    })


    # OPT 01.04. Settings -----------------------------------------------------------------------------------------
    HOOK_local_folder_path_tool_script <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector01()
      req(data$local_folder_tool_script$folder_path )

      data$local_folder_tool_script$folder_path
    })
    HOOK_temp_folder_path_tool_script <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector01()
      req(data$temp_folder_tool_script$folder_path )

      data$temp_folder_tool_script$folder_path
    })
    my_full_dataset <- reactive({
      req(rlist_dataset())
      flat_rlist_dataset <- rlist_dataset()
      my_df <- flat_rlist_dataset$metadata_dataset$"df"
      my_df
    })
    rlist_settings <- mod_04_00_settings_server(
      id = "my_ns_collector02_settings",
      df_input = my_full_dataset, #reactive(mtcars), # Asegúrate de que esto sea reactivo
      folder_path_tool_script = HOOK_temp_folder_path_tool_script,
      show_debug = internal_show_debug_general
    )



    # Collector03 - Objects for proccessing -----------------------------------------------------------------------
    rlist_collector03 <- reactive({

      internal_rlist_dataset <- rlist_dataset()
      internal_list_settings <- rlist_settings()
      internal_local_folder_path_tool <- HOOK_temp_folder_path_tool_script()

      req(internal_rlist_dataset, internal_list_settings, internal_local_folder_path_tool)

      my_list <- dplyr::lst()
      ##########################################################################
      my_list$"folder_script_tool" <- internal_local_folder_path_tool
      ##########################################################################

      my_list$"quarto_replacement" <- dplyr::lst()

      str_import_internal <- internal_rlist_dataset$metadata_dataset$"code_import_internal"
      str_import_external <- internal_rlist_dataset$metadata_dataset$"code_import_external"

      my_list$"quarto_replacement"$str_import_internal = dplyr::lst(
        detail = "Str for import - internal",
        name = "Is not an R Objetct.",
        R_value = str_import_internal,
        str_R = as.character(R_value),
        str_quarto = "get('mtcars') ###SECURITY_SEAL - internal###"
      )

      my_list$"quarto_replacement"$str_import_external = dplyr::lst(
        detail = "Str for import - external",
        name = "Is not an R Objetct.",
        R_value = str_import_external,
        str_R = as.character(R_value),
        str_quarto = "get('mtcars') ###SECURITY_SEAL - external###"
      )

      ##########################################################################

      my_list$quarto_replacement <- utils::modifyList(
        my_list$quarto_replacement,
        internal_list_settings$list_clean
      )
      ##########################################################################

      my_list

    })
    output$debug_collector03 <- listviewer::renderJsonedit({
      req(rlist_collector03())
      internal_rlist_collector03 <- list(rlist_collector03())

      listviewer::jsonedit(listdata = internal_rlist_collector03, mode = "text")
    })
    output$show_debug_external_collector03 <- renderUI({
      # Si quieres ver el panel aunque esté vacío, quita el req() de aquí arriba
      # y manéjalo internamente o deja que los jsonedit muestren NULL

      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",

          div(class = "section-label",
              style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 10px;",
              icon("bug"), " External Debug - Collector 03"),

          div(class = "row",
              div(class = "col-md-6",
                  # El req() dentro del renderJsonedit ya se encarga de esperar los datos
                  listviewer::jsoneditOutput(ns("debug_collector03"), height = "auto")
              ),
              div(class = "col-md-6",
                  listviewer::jsoneditOutput(ns("debug_collector03"), height = "auto")
              )
          )
      )
    })


    # OPT 01.05. Proccessing -----------------------------------------------------------------------------------------
    HOOK_temp_file_path_proccessing  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file01_05_proccsessing"$"temp_file_path" )

      data$"file01_05_proccsessing"$"temp_file_path"
    })
    HOOK_local_file_path_proccessing <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file01_05_proccsessing"$"local_file_path" )

      data$"file01_05_proccsessing"$"local_file_path"
    })

    # Pre Proccessing
    pack_proccessing <- reactive({

      req(rlist_collector03())
      flat_rlist_collector03 <- rlist_collector03()
      flat_list_quarto_replacement <- flat_rlist_collector03$"quarto_replacement"

      the_output_list <- list(
        module_proccessing_file_path = HOOK_temp_file_path_proccessing(),
        local_folder_tool_script     = HOOK_local_folder_path_tool_script(),
        temp_folder_tool_script      = HOOK_temp_folder_path_tool_script(),
        list_quarto_replacement      = flat_list_quarto_replacement
      )

      print(the_output_list)
      the_output_list
    })

    rlist_proccessing <- mod_10_00_proccessing_server(
      id = "pipeline_1",
      module_proccessing_file_path = reactive(pack_proccessing()$module_proccessing_file_path),
      local_folder_tool_script     = reactive(pack_proccessing()$local_folder_tool_script),
      temp_folder_tool_script      = reactive(pack_proccessing()$temp_folder_tool_script),
      list_quarto_replacement      = reactive(pack_proccessing()$list_quarto_replacement),
    )

    # Post Proccessing
    HOOK_proccessing_is_done <- reactive({
      req(rlist_proccessing())
      rlist_proccessing()$is_done
    })

    # OPT 03.01. Theory -------------------------------------------------------------------------------------------
    HOOK_temp_folder_path_theory  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file03_01_theory"$"temp_file_path" )

      data$"file03_01_theory"$"temp_file_path"
    })
    HOOK_local_folder_path_theory <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file03_01_theory"$"local_file_path" )

      data$"file03_01_theory"$"local_file_path"
    })
    rlist_theory <-       mod_03_A_theory_server(id = "txt_1",
                                                 module_theory_file_path = HOOK_temp_folder_path_theory,
                                                 show_debug = internal_show_debug_general)


    # OPT 03.02. Bibliography -------------------------------------------------------------------------------------------
    HOOK_temp_folder_path_bibliography  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file03_02_bibliography"$"temp_file_path" )

      data$"file03_02_bibliography"$"temp_file_path"
    })
    HOOK_local_folder_path_bibliography  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file03_02_bibliography"$"local_file_path" )

      data$"file03_02_bibliography"$"local_file_path"
    })
    rlist_theory <-       mod_03_B_bibliography_server(id = "txt_2",
                                                       module_bibliography_file_path = HOOK_temp_folder_path_bibliography ,
                                                       show_debug = internal_show_debug_general)

    # OPT 03.03. Cite -------------------------------------------------------------------------------------------
    HOOK_temp_folder_path_cite  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file03_03_cite"$"temp_file_path" )

      data$"file03_03_cite"$"temp_file_path"
    })
    HOOK_local_folder_path_cite <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file03_03_cite"$"local_file_path" )

      data$"file03_03_cite"$"local_file_path"
    })
    rlist_cite <-         mod_03_C_cite_server(id = "txt_3",
                                               module_cite_file_path = HOOK_temp_folder_path_cite,
                                               show_debug = internal_show_debug_general)

    ############################################################################



    # OPT 02.01. Shiny Outputs -------------------------------------------------------------------------------------------
    HOOK_file_path_pather_module_script_and_comments_temp  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_01_script_and_comments"$"temp_file_path" )

      data$"file02_01_script_and_comments"$"temp_file_path"
    })
    HOOK_file_path_pather_module_script_and_comments_local <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_01_script_and_comments"$"local_file_path" )

      data$"file02_01_script_and_comments"$"local_file_path"
    })


    pack02_01 <- reactive({
      list(HOOK_file_path_pather_module_script_and_comments_temp = HOOK_file_path_pather_module_script_and_comments_temp(),
           HOOK_temp_folder_path_tool_script = HOOK_temp_folder_path_tool_script(),
           HOOK_proccessing_is_done = HOOK_proccessing_is_done(),
           file_existes = file.exists(HOOK_file_path_pather_module_script_and_comments_temp())
           )
    })



    rlist_script_and_comments <-       mod_11_A_script_and_comments_server(
      id = "pipeline_333",
      module_script_and_comments_file_path = reactive(pack02_01()$HOOK_file_path_pather_module_script_and_comments_temp),
      temp_folder_tool_script       = reactive(pack02_01()$HOOK_temp_folder_path_tool_script), # El hijo lo usará de base
      show_file                     = reactive(pack02_01()$HOOK_proccessing_is_done),
      show_debug                    = F
    )


    ##############################################################################################
    ############################################################################



    # OPT 02.02. Shiny Outputs -------------------------------------------------------------------------------------------
    HOOK_file_path_pather_module_shiny_output_temp  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_02_shiny_output"$"temp_file_path" )

      data$"file02_02_shiny_output"$"temp_file_path"
    })
    HOOK_file_path_pather_module_shiny_output_local <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_02_shiny_output"$"local_file_path" )

      data$"file02_02_shiny_output"$"local_file_path"
    })

    pack02_02 <- reactive({
      list(HOOK_file_path_pather_module_shiny_output_temp = HOOK_file_path_pather_module_shiny_output_temp(),
           HOOK_temp_folder_path_tool_script = HOOK_temp_folder_path_tool_script(),
           HOOK_proccessing_is_done = HOOK_proccessing_is_done(),
           file_existes = file.exists(HOOK_file_path_pather_module_shiny_output_temp())
      )
    })

    rlist_shiny_output <-       mod_11_B_shiny_output_server(
      id = "pather_shiny_output",
      module_shiny_output_file_path = reactive(pack02_02()$HOOK_file_path_pather_module_shiny_output_temp),
      temp_folder_tool_script       = reactive(pack02_02()$HOOK_temp_folder_path_tool_script),
      show_file                     = reactive(pack02_02()$HOOK_proccessing_is_done),
      show_debug                    = F
    )


    ##############################################################################################


    # OPT 02.03. ASA -------------------------------------------------------------------------------------------
    HOOK_file_path_pather_module_asa_temp  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_03_asa"$"temp_file_path" )

      data$"file02_03_asa"$"temp_file_path"
    })
    HOOK_file_path_pather_module_asa_local <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_03_asa"$"local_file_path" )

      data$"file02_03_asa"$"local_file_path"
    })

    pack02_03 <- reactive({
      list(HOOK_file_path_pather_module_asa_temp = HOOK_file_path_pather_module_asa_temp(),
           HOOK_temp_folder_path_tool_script = HOOK_temp_folder_path_tool_script(),
           HOOK_proccessing_is_done = HOOK_proccessing_is_done(),
           file_existes = file.exists(HOOK_file_path_pather_module_asa_temp())
      )
    })
    rlist_asa <-       mod_11_C_asa_server(
      id = "pather_asa",
      module_asa_file_path          = reactive(pack02_03()$HOOK_file_path_pather_module_asa_temp),
      temp_folder_tool_script       = reactive(pack02_03()$HOOK_temp_folder_path_tool_script),
      show_file                     = reactive(pack02_03()$HOOK_proccessing_is_done),
      show_debug                    = F
    )


    ##############################################################################################



    # OPT 02.04. pdf -------------------------------------------------------------------------------------------
    HOOK_file_path_pather_module_pdf_temp  <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_04_pdf"$"temp_file_path" )

      data$"file02_04_pdf"$"temp_file_path"
    })
    HOOK_file_path_pather_module_pdf_local <- reactive({
      # Solo si el colector tiene éxito
      data <- rlist_collector02()
      req(data$"file02_04_pdf"$"local_file_path" )

      data$"file02_04_pdf"$"local_file_path"
    })


    pack02_04 <- reactive({
      list(HOOK_file_path_pather_module_pdf_temp = HOOK_file_path_pather_module_pdf_temp(),
           HOOK_temp_folder_path_tool_script = HOOK_temp_folder_path_tool_script(),
           HOOK_proccessing_is_done = HOOK_proccessing_is_done(),
           file_existes = file.exists(HOOK_file_path_pather_module_pdf_temp())
      )
    })

    rlist_pdf <-       mod_11_D_pdf_server(
      id = "pather_pdf",
      module_pdf_file_path          = reactive(pack02_04()$HOOK_file_path_pather_module_pdf_temp),
      temp_folder_tool_script       = reactive(pack02_04()$HOOK_temp_folder_path_tool_script),
      show_file                     = reactive(pack02_04()$HOOK_proccessing_is_done),
      show_debug                    = F
    )


    ##############################################################################################

    ##############################################################################################

    output$show_debug <- renderUI({

      navset_card_tab(
        title = "RScience Engine v.0.0.1",


        nav_panel(
          title = "Dataset",
          icon = icon("book"),
          mod_02_01_dataset_DEBUG_ui(id=ns("my_ns_dataset"))
        ),
        # nav_panel(
        #   title = "Tool",
        #   icon = icon("book"),
        #   mod_02_02_00_tool_DEBUG_ui(id=ns("my_ns_tool"))
        # ),
        nav_panel(
          title = "Script",
          icon = icon("book"),
          mod_02_03_00_script_DEBUG_ui(id=ns("my_ns_script"))
        ),
        nav_panel(
          title = "Collector01",
          icon = icon("book"),
          uiOutput(ns("show_debug_external_collector01"))
        ),
        nav_panel(
          title = "Collector02",
          icon = icon("book"),
          uiOutput(ns("show_debug_external_collector02"))
        ),
        nav_panel(
          title = "Settings",
          icon = icon("book"),
          mod_04_00_settings_DEBUG_ui(ns("my_ns_collector02_settings"))
        ),
        nav_panel(
          title = "Collector03",
          icon = icon("book"),
          uiOutput(ns("show_debug_external_collector03"))
        ),
        nav_panel(
          title = "Proccessing",
          icon = icon("book"),
          mod_10_00_proccessing_DEBUG_ui(id = ns("pipeline_1"))
        ),

        # mod_04_00_settings_ui(id = ns(""))
        nav_panel(
          title = "Theory",
          icon = icon("book"),
          mod_03_A_theory_DEBUG_ui(id=ns("txt_1"))
        )

      )

    })

  })
}



library(shiny)
library(bslib)
library(shinyjs)

library(shiny)
library(DT)
library(bslib)
library(listviewer)
library(shinyjs)
library(shinyWidgets) # Necesario para el radioGroupButtons

# devtools::load_all()

# 2. UI DE LA APP
ui <- fluidPage(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

  # Estilo crítico para que la página no tenga scroll propio
  #tags$style("body, html { overflow: hidden; height: 100%; margin: 0; padding: 0; }"),

  # tags$head(
  #   useShinyjs(),
  #
  #   # IMPORTANTE: No usamos includeCSS.
  #   # Usamos tags$link apuntando al recurso que definiste con addResourcePath
  #   if (!is.null(path_to_css)) {
  #     tags$link(rel = "stylesheet", type = "text/css", href = "lib_www/style_000.css")
  #   }
  # ),

  # El h2 y el módulo
  #div(style = "height: 100vh; display: flex; flex-direction: column;",
  # div(style = "padding: 10px 0; flex-shrink: 0;",
  #     h2("RScience Engine v.0.7.2", style="color: #00d4ff; text-align: center; margin:0;")
  # ),
  mod_rscience_engine03_ui("mi_analisis_01")
  #)
)
#
# # 3. SERVER DE LA APP
# server <- function(input, output, session) {
#
#   # Llamamos al server del módulo
#   # Guardamos el retorno en un reactivo por si queremos usar los datos en la app
#   mod_rscience_engine03_server(id = "mi_analisis_01", show_debug_tab = F, show_debug_general = F)
#
#   # # Ejemplo de cómo acceder a los datos desde fuera del módulo
#   # observe({
#   #   req(datos_importados()$is_done)
#   #   message("App principal detectó carga de: ", datos_importados()$metadata$name_mod)
#   # })
# }
#
# shinyApp(ui, server)
