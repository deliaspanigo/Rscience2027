mod_rscience_engine_ui <- function(id) {
  ns <- NS(id)

  # Registro de recursos CSS
  css_folder <- system.file("www", "css", package = "Rscience2027")
  if (css_folder == "") css_folder <- "www/css"
  try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)

  www_folder <- system.file("www", package = "Rscience2027")
  if (www_folder == "") www_folder <- "www"
  try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

  tagList(
    tags$head(
      useShinyjs(),
      tags$link(rel = "stylesheet", type = "text/css",
                href = paste0("RS-STYLES/style_000.css?v=", as.numeric(Sys.time())))
    ),

    page_sidebar(
      theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

      # --- SIDEBAR ---
      sidebar = sidebar(
        width = 320, id = ns("sidebar_panel"),
        div(class = "text-center", style = "padding: 20px 0 5px 0;",
            img(src = "WWW-FOLDER/Rscience_logo_sticker.png", style = "width: 180px;")
        ),
        div(class="nav-header",
            div(id=ns("dot1"), class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Setup")),
            div(id=ns("dot2"), class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Out")),
            div(id=ns("dot3"), class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Extra"))
        ),
        div(class="sidebar-viewport",
            div(id=ns("track"), class="slider-track",
                div(class="pack-group",
                    div(id=ns("c_data"),     class="phase-card active", icon("database"), span(" Dataset")),
                    div(id=ns("c_tool"),     class="phase-card",        icon("gear"),     span(" Tool Engine")),
                    div(id=ns("c_script"),   class="phase-card",        icon("code"),     span(" Script Engine")),
                    div(id=ns("c_settings"), class="phase-card",        icon("sliders"),  span(" Settings")),
                    div(id=ns("c_play"),     class="phase-card",        icon("play"),     span(" Processing")),
                    br(),
                    div(id=ns("c_DEBUG"),    class="phase-card",        icon("play"),     span(" DEBUG"))
                ),
                div(class="pack-group",
                    div(id=ns("c_script_and_comments"), class="phase-card", icon("desktop"), span(" Scripts and Comments")),
                    div(id=ns("c_shiny_output"),        class="phase-card", icon("desktop"), span(" Shiny Output")),
                    div(id=ns("c_asa"),                 class="phase-card", icon("desktop"), span(" ASA")),
                    div(id=ns("c_pdf"),                 class="phase-card", icon("desktop"), span(" PDF"))

                ),
                div(class="pack-group",
                    div(id=ns("c_theory"),       class="phase-card", icon("book"), span(" Theory")),
                    div(id=ns("c_bibliography"), class="phase-card", icon("list"), span(" Bibliography")),
                    div(id=ns("c_cite"),         class="phase-card", icon("quote-left"), span(" Cite")),
                    div(id=ns("c_faqs"),         class="phase-card", icon("question-circle"), span(" FAQs"))
                )
            )
        )
      ),

      # --- ÁREA PRINCIPAL (100% ALTO) ---
      div(class = "rs-main-layout-container",
          div(class = "rs-content-viewport",
              navset_hidden(
                id = ns("main_navset"),
                nav_panel_hidden("c_data",   mod_02_01_dataset_ui(id = ns("my_ns_dataset"))),
                nav_panel_hidden("c_tool",   mod_02_02_00_tool_ui(id = ns("my_ns_tool"))),
                nav_panel_hidden("c_script", mod_02_03_00_script_ui(id=ns("my_ns_script"))),
                nav_panel_hidden("c_settings", mod_04_00_settings_ui(id = ns("my_ns_collector02_settings"))),
                nav_panel_hidden("c_play", mod_10_00_proccessing_ui(id = ns("pipeline_1"))),
                nav_panel_hidden("c_DEBUG",   uiOutput(ns("show_debug"))),

                nav_panel_hidden("c_script_and_comments", mod_11_A_script_and_comments_ui(id = ns("pipeline_333"))),
                nav_panel_hidden("c_shiny_output", mod_11_B_shiny_output_ui(id = ns("pather_shiny_output"))),
                nav_panel_hidden("c_asa",          mod_11_C_asa_ui(id = ns("pather_asa"))),
                nav_panel_hidden("c_pdf",          mod_11_D_pdf_ui(id = ns("pather_pdf"))),

                nav_panel_hidden("c_theory",  mod_03_A_theory_ui(ns("txt_1"))),
                nav_panel_hidden("c_bibliography",mod_03_B_bibliography_ui(ns("txt_2"))),
                nav_panel_hidden("c_cite",mod_03_C_cite_ui(ns("txt_3"))),
                nav_panel_hidden("c_faqs", card(card_body("FAQ...")))
              )
          )
      )
    )
  )
}

mod_rscience_engine_server <- function(id, show_debug_tab = F, show_debug_general = F) {
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


    # OPT 01.02. Tool -------------------------------------------------------------------------------------------
    rlist_tool <- mod_02_02_00_tool_server(id = "my_ns_tool", show_debug = internal_show_debug_general()) # SIN ns()


    # OPT 01.03. Script -------------------------------------------------------------------------------------------
    rlist_script <-   mod_02_03_00_script_server(id="my_ns_script",
                                               vector_str_folder_tool_script = reactive(c("tool_0001_script_001", "tool_0001_script_002")),
                                               show_debug = internal_show_debug_general()) # Llamamos a la UI


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
      my_df <- flat_rlist_dataset$metadata$"df"
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

      #internal_rlist_dataset <- rlist_dataset()
      internal_list_settings <- rlist_settings()
      internal_local_folder_path_tool <- HOOK_temp_folder_path_tool_script()

      req(internal_rlist_dataset, internal_list, internal_local_folder_path_tool)

      my_list <- list()

      my_list$"folder_script_tool" <- internal_local_folder_path_tool

      # my_list$"dataset" <- list(code_import_external = internal_rlist_dataset$metadata$code_import_external,
      #                           code_import_internal = internal_rlist_dataset$metadata$code_import_internal)

      my_list$"settings" <- internal_list_settings$list_clean

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


    rlist_proccessing <- mod_10_00_proccessing_server(
      id = "pipeline_1",
      module_proccessing_file_path = HOOK_temp_file_path_proccessing,
      local_folder_tool_script = HOOK_local_folder_path_tool_script,
      temp_folder_tool_script =  HOOK_temp_folder_path_tool_script,
      list_settings = NULL
    )
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

    # flat_module_script_and_comments_file_path <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002",
    #                                                   "f01_shiny_show", "p02_01_shiny_output", "f03_prod", "mod_special_shiny_output.R" ,
    #                                                   package = "Rscience2027")

    # file_path <- reactive(system.file("test_shiny_output", "f05_shiny_output", "tab01_control.html", package = "Rscience2027"))
    rlist_script_and_comments <-       mod_11_A_script_and_comments_server(
      id = "pipeline_333",
      module_script_and_comments_file_path = HOOK_file_path_pather_module_script_and_comments_temp,
      temp_folder_tool_script       = HOOK_temp_folder_path_tool_script, # El hijo lo usará de base
      show_file                     = TRUE,
      show_debug                    = TRUE
    )


  ##############################################################################################
    ############################################################################



    # OPT 02.01. Shiny Outputs -------------------------------------------------------------------------------------------
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

    # flat_module_script_and_comments_file_path <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002",
    #                                                   "f01_shiny_show", "p02_01_shiny_output", "f03_prod", "mod_special_shiny_output.R" ,
    #                                                   package = "Rscience2027")

    # file_path <- reactive(system.file("test_shiny_output", "f05_shiny_output", "tab01_control.html", package = "Rscience2027"))
    rlist_shiny_output <-       mod_11_B_shiny_output_server(
      id = "pather_shiny_output",
      module_shiny_output_file_path = HOOK_file_path_pather_module_shiny_output_temp,
      temp_folder_tool_script       = HOOK_temp_folder_path_tool_script, # El hijo lo usará de base
      show_file                     = TRUE,
      show_debug                    = TRUE
    )


    ##############################################################################################


    # OPT 02.01. Shiny Outputs -------------------------------------------------------------------------------------------
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

    # flat_module_script_and_comments_file_path <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002",
    #                                                   "f01_shiny_show", "p02_01_shiny_output", "f03_prod", "mod_special_shiny_output.R" ,
    #                                                   package = "Rscience2027")

    # file_path <- reactive(system.file("test_shiny_output", "f05_shiny_output", "tab01_control.html", package = "Rscience2027"))
    rlist_asa <-       mod_11_C_asa_server(
      id = "pather_asa",
      module_asa_file_path = HOOK_file_path_pather_module_asa_temp,
      temp_folder_tool_script       = HOOK_temp_folder_path_tool_script, # El hijo lo usará de base
      show_file                     = TRUE,
      show_debug                    = TRUE
    )


    ##############################################################################################



    # OPT 02.01. Shiny Outputs -------------------------------------------------------------------------------------------
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

    # flat_module_script_and_comments_file_path <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002",
    #                                                   "f01_shiny_show", "p02_01_shiny_output", "f03_prod", "mod_special_shiny_output.R" ,
    #                                                   package = "Rscience2027")

    # file_path <- reactive(system.file("test_shiny_output", "f05_shiny_output", "tab01_control.html", package = "Rscience2027"))
    rlist_pdf <-       mod_11_D_pdf_server(
      id = "pather_pdf",
      module_pdf_file_path = HOOK_file_path_pather_module_pdf_temp,
      temp_folder_tool_script       = HOOK_temp_folder_path_tool_script, # El hijo lo usará de base
      show_file                     = TRUE,
      show_debug                    = TRUE
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
        nav_panel(
          title = "Tool",
          icon = icon("book"),
          mod_02_02_00_tool_DEBUG_ui(id=ns("my_ns_tool"))
        ),
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
