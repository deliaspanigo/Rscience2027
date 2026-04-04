library(shiny)

# --- Módulo UI ---
mod_special_asa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      span(style = "color: green; font-weight: bold;", "Control de Renderizado - Script 002"),
      br(), br(),
      actionButton(ns("run_render"), "Ejecutar Quarto Render",
                   icon = icon("play"), class = "btn-primary"),
      hr(),
      uiOutput(ns("shiny_view")) # Panel de Debug
    )
  )
}

# --- Módulo Server ---
mod_special_asa_server <- function(id, folder_temp_path) {
  moduleServer(id, function(input, output, session) {

    # Estado para controlar si el render terminó
    is_done <- reactiveVal(FALSE)

    # 1. Carpeta base
    the_temp_folder <- reactive({
      req(folder_temp_path())
      folder_temp_path()
    })

    # 2. Ruta del archivo QMD a renderizar
    the_runner_file_path <- reactive({
      req(the_temp_folder())
      path <- file.path(the_temp_folder(), "f02_quarto_lab", "f02_quarto_render",
                        "g03_gen_shiny_output", "file01_gen_shiny_output.qmd")
      normalizePath(path, mustWork = TRUE)
    })

    # 3. Lógica del Botón: Dispara el Render
    observeEvent(input$run_render, {
      req(the_runner_file_path())

      # Notificación visual
      id_notif <- showNotification("Renderizando Quarto... por favor espere.",
                                   duration = NULL, type = "message")

      tryCatch({
        quarto::quarto_render(input = the_runner_file_path(), output_format = "all")
        is_done(TRUE) # Activamos la carga de datos
        showNotification("Renderizado completado con éxito.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error en render:", e$message), type = "error")
      })

      removeNotification(id_notif)
    })

    # 4. Ruta de subcarpeta donde quedan los RData
    the_subfolder_path <- reactive({
      req(the_temp_folder(), is_done())
      path <- file.path(the_temp_folder(), "f02_quarto_lab", "f02_quarto_render", "f02_quarto_mod")
      normalizePath(path, mustWork = TRUE)
    })

    # 5. Información de los RData encontrados
    the_df_RData_info <- reactive({
      req(the_subfolder_path())
      the_folder <- the_subfolder_path()

      # Corregido: Usamos the_folder directamente
      vector_RData_file_path <- list.files(path = the_folder, pattern = "\\.RData$", full.names = TRUE)

      if(length(vector_RData_file_path) == 0) return(NULL)

      data.frame(
        "order" = seq_along(vector_RData_file_path),
        "file_name" = basename(vector_RData_file_path),
        "file_exists" = file.exists(vector_RData_file_path),
        "file_path" = vector_RData_file_path,
        stringsAsFactors = FALSE
      )
    })

    # 6. La Super Bag (Carga masiva de datos)
    super_bag <- reactive({
      req(the_df_RData_info())
      df_RData <- the_df_RData_info()
      vector_paths <- df_RData$file_path

      all_data <- lapply(vector_paths, function(path) {
        tmp_env <- new.env()
        load(path, envir = tmp_env)
        as.list(tmp_env)
      })

      names(all_data) <- basename(vector_paths)
      all_data
    })

    # --- DEBUG OUTPUT ---
    output$debug_bag <- renderPrint({
      req(super_bag())
      cat("### DEBUG: Contenido de Super Bag ###\n")
      # Mostramos la estructura simplificada (nombres de archivos y objetos dentro)
      print(lapply(super_bag(), names))
    })

    #####################################################################


    output$shiny_view <- renderUI({
      # Usamos req para asegurarnos de que la super_bag tenga datos antes de mostrar la UI
      # Esto evita errores de "objeto no encontrado" al abrir la app
      req(super_bag())

      # 1. DEFINIR NS AQUÍ PARA QUE LA UI SEPA QUIÉN ES
      ns <- session$ns

      # Usamos bslib para un diseño moderno, o tabsetPanel para R-Base
      bslib::navset_tab(
        id = "main_tabs", # ID para rastrear en qué pestaña está el usuario

        # Pestaña 1: Visualización de Datos Crudos
        bslib::nav_panel(
          title = "1. Summary Anova",
          icon = icon("table"),
          uiOutput(ns("pack01_shiny_ui_summary_anova"))
        ),

        # Pestaña 2: Análisis Estadístico (ANOVA/Biometría)
        bslib::nav_panel(
          title = "2. Anova and Tukey",
          icon = icon("calculator"),
          uiOutput(ns("pack02_shiny_ui_anova_and_tukey"))
        ),

        # Pestaña 3: Visualización Gráfica
        bslib::nav_panel(
          title = "3. Requeriments",
          icon = icon("chart-bar"),
          uiOutput(ns("pack03_shiny_ui_requeriments"))
        ),

        # Pestaña 4: Configuración del Reporte
        bslib::nav_panel(
          title = "4. Model",
          icon = icon("file-alt"),
          uiOutput(ns("pack04_shiny_ui_model"))
        ),

        # Pestaña 4: Configuración del Reporte
        bslib::nav_panel(
          title = "5. Descriptive RV",
          icon = icon("file-alt"),
          uiOutput(ns("pack05_shiny_ui_descriptive_rv"))
        ),

        # Pestaña 5: Sistema y Log (Tu Debug)
        bslib::nav_panel(
          title = "5. Descriptive Residuaks",
          icon = icon("terminal"),
          uiOutput(ns("pack06_shiny_ui_descriptive_residuals"))


        )
      )
    })


    output$pack01_shiny_ui_summary_anova <- renderUI({
      req(super_bag())
      ns <- session$ns

      tagList(
        tags$h3("Validación de Importación y Control (Script 01)",
                style = "color: #2c3e50; border-bottom: 2px solid #18bc9c; padding-bottom: 10px;"),

        # 1. Variables Seleccionadas
        wellPanel(style = "background: #f8f9fa;",
                  tags$h5(icon("list-check"), " Variables Seleccionadas"),
                  tableOutput(ns("pack01_01_df_summary_anova"))
        ),

        # 2. Confianza y Alpha
        wellPanel(style = "background: #f8f9fa;",
                  tags$h5(icon("percent"), " Confianza y Alpha"),
                  verbatimTextOutput(ns("pack01_02_text_summary"))
        ),

        # 1. Variables Seleccionadas
        wellPanel(style = "background: #f8f9fa;",
                  tags$h5(icon("list-check"), " Variables Seleccionadas"),
                  tableOutput(ns("pack01_03_df_table_tukey_plot001"))
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack01_04_plot_tukey001"), height = "450px")
        ),

        br(),
        tags$p(style = "color: #7f8c8d; font-style: italic;",
               "Nota: Los datos mostrados provienen del entorno R_obj_env_script01_anova_import_and_control.RData")
      )
    })

    # 1.df_selected_vars
    output$pack01_01_df_summary_anova <- renderTable({
      req(super_bag())
      the_bag <- super_bag()
      the_bag[[6]]$df_summary_anova
    })

    output$pack01_02_text_summary <- renderPrint({
      req(super_bag())
      the_bag <- super_bag()

      vector_obj <- c("phrase01_normality_selected",
                      "phrase01_homogeneity_selected",
                      "phrase01A_requeriments_selected",
                      "phrase01B_requeriments_selected",
                      "phrase01C_requeriments_selected",
                      "phrase01_anova_selected",
                      "phrase01A_tukey_selected",
                      "phrase01B_tukey_selected",
                      "phrase01C_tukey_selected",
                      "phrase01_anova_tukey_selected")

      text01 <- the_bag[[6]][vector_obj]

      print(text01)

      })

    output$pack01_03_df_table_tukey_plot001 <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_table_tukey_plot001
    })

    output$pack01_04_plot_tukey001 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[2]]$plot_tukey001
      req(p)
      p
    })
    ###################################################################################

    output$pack02_shiny_ui_anova_and_tukey <- renderUI({
      req(super_bag())
      ns <- session$ns

      tagList(
        tags$h3("Análisis de Varianza y Pruebas de Tukey (Script 02)",
                style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),

        # --- SECCIÓN ANOVA ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #3498db;",
                  tags$h5(icon("table"), " Tabla ANOVA (Classroom)"),
                  tableOutput(ns("pack02_01_df_table_anova_classroom"))
        ),

        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("exclamation-triangle"), " Error del Modelo"),
                  tableOutput(ns("pack02_02_df_model_error"))
        ),

        wellPanel(style = "background: #2c3e50; color: white;",
                  tags$h5(icon("info-circle"), " Verificación de Balanceo"),
                  verbatimTextOutput(ns("pack02_03_df_phrase_selected_check_unbalanced"))
        ),

        # --- SECCIÓN TUKEY ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #18bc9c;",
                  tags$h5(icon("layer-group"), " Tabla de Tukey (Classroom)"),
                  tableOutput(ns("pack02_04_df_tukey_table_classroom"))
        ),

        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("list-ol"), " Tabla Comparativa de Medios"),
                  tableOutput(ns("pack02_05_df_table_tukey_plot001"))
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack02_06_plot_tukey001"), height = "450px")
        ),

        # --- GRUPOS COMPLETOS (Verbatim) ---
        wellPanel(style = "background: #f8f9fa;",
                  tags$h5(icon("terminal"), " Grupos Tukey 01 - Detalle Completo"),
                  verbatimTextOutput(ns("pack02_07_tukey01_full_groups"))
        ),

        wellPanel(style = "background: #f8f9fa;",
                  tags$h5(icon("terminal"), " Grupos Tukey 02 - Detalle Completo"),
                  verbatimTextOutput(ns("pack02_08_tukey02_full_pairs"))
        ),

        br(),
        tags$p(style = "color: #7f8c8d; font-style: italic;",
               "Nota: Los datos provienen del entorno R_obj_env_script02_anova_and_tukey.RData")
      )
    })

    # --- Renders Pack 02 ---

    output$pack02_01_df_table_anova_classroom <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_table_anova_classroom
    })

    output$pack02_02_df_model_error <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_model_error
    })

    output$pack02_03_df_phrase_selected_check_unbalanced <- renderText({
      req(super_bag())
      # Se mostrará como verbatim en la UI
      super_bag()[[2]]$phrase_selected_check_unbalanced
    })

    output$pack02_04_df_tukey_table_classroom <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_tukey_table_classroom
    })

    output$pack02_05_df_table_tukey_plot001 <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_table_tukey_plot001
    })

    output$pack02_06_plot_tukey001 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[2]]$plot_tukey001
      req(p)
      p
    })

    output$pack02_07_tukey01_full_groups <- renderPrint({ # CAMBIAR A renderPrint
      req(super_bag())
      # Esto imprimirá la lista tal cual la ves en la consola de R
      print(super_bag()[[2]]$tukey01_full_groups)
    })

    output$pack02_08_tukey02_full_pairs <- renderPrint({ # CAMBIAR A renderPrint
      req(super_bag())
      print(super_bag()[[2]]$tukey02_full_pairs)
    })


    ###################################################################################

    output$pack03_shiny_ui_requeriments <- renderUI({
      req(super_bag())
      ns <- session$ns

      tagList(
        tags$h3("Validación de Supuestos del Modelo (Script 02)",
                style = "color: #2c3e50; border-bottom: 2px solid #e67e22; padding-bottom: 10px;"),

        # --- SECCIÓN NORMALIDAD ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #e67e22;",
                  tags$h5(icon("check-double"), " Prueba de Normalidad de Residuos"),
                  verbatimTextOutput(ns("pack03_01_list_test_residuals_normality"))
        ),

        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("table"), " Tabla Resumen de Normalidad"),
                  tableOutput(ns("pack03_02_df_normality"))
        ),

        # --- SECCIÓN HOMOGENEIDAD ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #9b59b6;",
                  tags$h5(icon("balance-scale"), " Prueba de Homogeneidad de Varianzas"),
                  verbatimTextOutput(ns("pack03_03_list_test_residuals_homogeneity"))
        ),

        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("table"), " Tabla Resumen de Homogeneidad (Levene/Bartlett)"),
                  tableOutput(ns("pack03_04_df_homogeneity"))
        ),

        # --- SECCIÓN ERRORES Y RESIDUOS ---
        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("bug"), " Detalle de Errores Crudos"),
                  div(style = "height: 300px; overflow-y: auto;",
                      tableOutput(ns("pack03_05_df_raw_error"))
                  )
        ),

        wellPanel(style = "background: #2c3e50; color: white;",
                  tags$h5(icon("calculator"), " Media de los Residuos"),
                  verbatimTextOutput(ns("pack03_06_mean_residuals"))
        ),

        br(),
        tags$p(style = "color: #7f8c8d; font-style: italic;",
               "Nota: Estos supuestos son críticos para la validez del ANOVA cargado en la Super Bag.")
      )
    })

    # 3.df_est_parameters
    output$pack03_01_list_test_residuals_normality <- renderPrint({
      req(super_bag())
      the_bag <- super_bag()
      print(the_bag[[2]]$list_test_residuals_normality)
    })

    output$pack03_02_df_normality <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_normality
    })

    output$pack03_03_list_test_residuals_homogeneity <- renderPrint({
      req(super_bag())
      the_bag <- super_bag()
      print(the_bag[[2]]$list_test_residuals_homogeneity)
    })

    output$pack03_04_df_homogeneity <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_homogeneity
    })



    output$pack03_05_df_raw_error <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_raw_error
    })


    output$pack03_06_mean_residuals <- renderPrint({
      req(super_bag())
      the_bag <- super_bag()
      print(the_bag[[2]]$mean_residuals)
    })

    ###################################################################################


    output$pack04_shiny_ui_model <- renderUI({
      req(super_bag())
      ns <- session$ns

      tagList(
        tags$h3("Validación de Supuestos del Modelo (Script 02)",
                style = "color: #2c3e50; border-bottom: 2px solid #e67e22; padding-bottom: 10px;"),

        # --- SECCIÓN NORMALIDAD ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #e67e22;",
                  tags$h5(icon("check-double"), " Prueba de Normalidad de Residuos"),
                  verbatimTextOutput(ns("pack04_01_df_rv_position_general"))
        ),

        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("table"), " Tabla Resumen de Normalidad"),
                  tableOutput(ns("pack04_02_df_normality"))
        ),

        br(),
        tags$p(style = "color: #7f8c8d; font-style: italic;",
               "Nota: Estos supuestos son críticos para la validez del ANOVA cargado en la Super Bag.")
      )
    })

    # 3.df_est_parameters
    output$pack04_01_df_rv_position_general <- renderPrint({
      req(super_bag())
      the_bag <- super_bag()
      print(the_bag[[4]]$df_rv_position_general)
    })

    output$pack04_02_df_normality <- renderTable({
      req(super_bag())
      super_bag()[[2]]$df_normality
    })

    ###################################################################################


    output$pack05_shiny_ui_descriptive_rv <- renderUI({
      req(super_bag())
      ns <- session$ns

      tagList(
        tags$h3("Validación de Supuestos del Modelo (Script 02)",
                style = "color: #2c3e50; border-bottom: 2px solid #e67e22; padding-bottom: 10px;"),

        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("table"), " Tabla Resumen de Normalidad"),
                  tableOutput(ns("pack05_01_df_rv_position_levels"))
        ),

        wellPanel(style = "background: #fdfefe;",
                  tags$h5(icon("table"), " Tabla Resumen de Normalidad"),
                  tableOutput(ns("pack05_02_df_rv_dispersion_levels"))
        ),

        # --- SECCIÓN NORMALIDAD ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #e67e22;",
                  tags$h5(icon("check-double"), " Prueba de Normalidad de Residuos"),
                  tableOutput(ns("pack05_03_df_rv_position_general"))
        ),

        # --- SECCIÓN NORMALIDAD ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #e67e22;",
                  tags$h5(icon("check-double"), " Prueba de Normalidad de Residuos"),
                  tableOutput(ns("pack05_04_df_rv_dispersion_general"))
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack05_05_plot_factor001"), height = "450px")
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack05_06_plot_factor002"), height = "450px")
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack05_07_plot_factor003"), height = "450px")
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack05_08_plot_factor004"), height = "450px")
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack05_09_plot_factor005"), height = "450px")
        ),

        wellPanel(
          tags$h5(icon("chart-bar"), " Gráfico de Comparaciones Múltiples (Tukey)"),
          plotly::plotlyOutput(ns("pack05_10_plot_factor006"), height = "450px")
        ),

        br(),
        tags$p(style = "color: #7f8c8d; font-style: italic;",
               "Nota: Estos supuestos son críticos para la validez del ANOVA cargado en la Super Bag.")
      )
    })



    # --- TABLAS DESCRIPTIVAS ---
    output$pack05_01_df_rv_position_levels <- renderTable({
      req(super_bag())
      super_bag()[[4]]$df_rv_position_levels
    })

    output$pack05_02_df_rv_dispersion_levels <- renderTable({
      req(super_bag())
      super_bag()[[4]]$df_rv_dispersion_levels
    })

    output$pack05_03_df_rv_position_general <- renderTable({
      req(super_bag())
      super_bag()[[4]]$df_rv_position_general
    })

    output$pack05_04_df_rv_dispersion_general <- renderTable({
      req(super_bag())
      super_bag()[[4]]$df_rv_dispersion_general
    })

    output$pack05_05_plot_factor001 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[4]]$plot_factor001
      req(p)
      p
    })

    output$pack05_06_plot_factor002 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[4]]$plot_factor002
      req(p)
      p
    })

    output$pack05_07_plot_factor003 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[4]]$plot_factor003
      req(p)
      p
    })

    output$pack05_08_plot_factor004 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[4]]$plot_factor004
      req(p)
      p
    })

    output$pack05_09_plot_factor005 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[4]]$plot_factor005
      req(p)
      p
    })

    output$pack05_10_plot_factor006 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[4]]$plot_factor006
      req(p)
      p
    })


    ###################################################################################


    output$pack06_shiny_ui_descriptive_residuals <- renderUI({
      req(super_bag())
      ns <- session$ns

      tagList(
        tags$h3("Análisis Descriptivo de Residuos (Script 05)",
                style = "color: #2c3e50; border-bottom: 2px solid #e67e22; padding-bottom: 10px;"),

        # --- TABLAS DE RESIDUOS ---
        wellPanel(style = "background: #fdfefe; border-left: 5px solid #3498db;",
                  tags$h5(icon("table"), " Posición de Residuos por Niveles"),
                  tableOutput(ns("pack06_01_df_residuals_position_levels"))
        ),

        wellPanel(style = "background: #fdfefe; border-left: 5px solid #3498db;",
                  tags$h5(icon("table"), " Dispersión de Residuos por Niveles"),
                  tableOutput(ns("pack06_02_df_residual_dispersion_levels"))
        ),

        wellPanel(style = "background: #f8f9fa; border-left: 5px solid #e67e22;",
                  tags$h5(icon("check-double"), " Posición General de Residuos"),
                  tableOutput(ns("pack06_03_df_residuals_position_general"))
        ),

        wellPanel(style = "background: #f8f9fa; border-left: 5px solid #e67e22;",
                  tags$h5(icon("check-double"), " Dispersión General de Residuos"),
                  tableOutput(ns("pack06_04_df_residuals_dispersion_general"))
        ),

        hr(),
        tags$h4(icon("chart-line"), " Visualización de Residuos"),

        # --- GRÁFICOS DE RESIDUOS (Apilados) ---
        wellPanel(tags$h5("Histograma / Densidad de Residuos (01)"),
                  plotly::plotlyOutput(ns("pack06_05_plot_residuals001"), height = "400px")),

        wellPanel(tags$h5("QQ-Plot de Residuos (02)"),
                  plotly::plotlyOutput(ns("pack06_06_plot_residuals002"), height = "400px")),

        wellPanel(tags$h5("Residuos vs Predichos (03)"),
                  plotly::plotlyOutput(ns("pack06_06_plot_residuals003"), height = "400px")),

        wellPanel(tags$h5("Residuos por Factor (04)"),
                  plotly::plotlyOutput(ns("pack06_07_plot_residuals004"), height = "400px")),

        wellPanel(tags$h5("Análisis de Residuos (05)"),
                  plotly::plotlyOutput(ns("pack06_08_plot_residuals005"), height = "400px")),

        wellPanel(tags$h5("Análisis de Residuos (06)"),
                  plotly::plotlyOutput(ns("pack06_09_plot_residuals006"), height = "400px")),

        wellPanel(tags$h5("Análisis de Residuos (07)"),
                  plotly::plotlyOutput(ns("pack06_10_plot_residuals007"), height = "400px")),

        wellPanel(tags$h5("Análisis de Residuos (08)"),
                  plotly::plotlyOutput(ns("pack06_11_plot_residuals008"), height = "400px")),

        wellPanel(tags$h5("Análisis de Residuos (09)"),
                  plotly::plotlyOutput(ns("pack06_12_plot_residuals009"), height = "400px")),

        wellPanel(tags$h5("Análisis de Residuos (10)"),
                  plotly::plotlyOutput(ns("pack06_13_plot_residuals010"), height = "400px")),

        br(),
        tags$p(style = "color: #7f8c8d; font-style: italic;",
               "Nota: La exploración de residuos es fundamental para detectar outliers y patrones no lineales.")
      )
    })

    output$pack06_01_df_residuals_position_levels <- renderTable({
      req(super_bag())
      super_bag()[[5]]$df_residuals_position_levels
    })

    output$pack06_02_df_residual_dispersion_levels <- renderTable({
      req(super_bag())
      super_bag()[[5]]$df_residual_dispersion_levels
    })

    output$pack06_03_df_residuals_position_general <- renderTable({
      req(super_bag())
      super_bag()[[5]]$df_residuals_position_general
    })


    output$pack06_04_df_residuals_dispersion_general <- renderTable({
      req(super_bag())
      super_bag()[[5]]$df_residuals_dispersion_general
    })


    output$pack06_05_plot_residuals001 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals001
      req(p)
      p
    })

    output$pack06_06_plot_residuals002 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals002
      req(p)
      p
    })

    output$pack06_06_plot_residuals003 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals003
      req(p)
      p
    })

    output$pack06_07_plot_residuals004 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals004
      req(p)
      p
    })

    output$pack06_08_plot_residuals005 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals005
      req(p)
      p
    })


    output$pack06_09_plot_residuals006 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals006
      req(p)
      p
    })


    output$pack06_10_plot_residuals007 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals007
      req(p)
      p
    })


    output$pack06_11_plot_residuals008 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals008
      req(p)
      p
    })



    output$pack06_12_plot_residuals009 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals009
      req(p)
      p
    })


    output$pack06_13_plot_residuals010 <- plotly::renderPlotly({
      req(super_bag())
      p <- super_bag()[[5]]$plot_residuals010
      req(p)
      p
    })



  })
}

