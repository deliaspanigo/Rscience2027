



library(shiny)
library(bslib)
library(quarto)
library(shinyjs)


# ==============================================================================
# 2. MÓDULO CONTENEDOR: mod_special_script_comment
# ==============================================================================

mod_special_shiny_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(class = "special-comment-wrapper",
        navset_card_tab(
          title = "Editor de Reportes Quarto",
          nav_panel(
            title = "Control",
            mod_06_00_render_and_show_ui(id = ns("anova_01"))
          ),
          nav_panel(
            title = "Anova and Tukey",
            mod_06_00_render_and_show_ui(id = ns("anova_02"))
          ),
          nav_panel(
            title = "Requeriments",
            mod_06_00_render_and_show_ui(id = ns("anova_03"))
          ),
          nav_panel(
            title = "Model",
            mod_06_00_render_and_show_ui(id = ns("anova_04"))
          ),
          nav_panel(
            title = "Descriptive RV",
            mod_06_00_render_and_show_ui(id = ns("anova_05"))
          ),
          nav_panel(
            title = "Descriptive Residuals",
            mod_06_00_render_and_show_ui(id = ns("anova_06"))
          )
        )
    )
  )
}

mod_special_shiny_output_server <- function(id, folder_temp_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_folder01 <- reactive({
      req(folder_temp_path())
      file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "g05_shiny_output")
    })

    base_folder02 <- reactive({
      req(folder_temp_path())
      file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "f05_shiny_output")
    })

    # base_folder <- reactive({
    #   req(folder_temp_path())
    #   file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "f03_script")
    # })

    # Ejemplo con colores distintos configurados desde el Server
    mod_06_00_render_and_show_server(id = "anova_01",
                                     super_label = "Import and Control",
                                     bg_color = "#e7f1ff", # Un azul muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g05_shiny_output.qmd")),
                                     output_file_path    = reactive(file.path(base_folder02(), "tab01_control.html")))

    mod_06_00_render_and_show_server(id = "anova_02",
                                     super_label = "Full Test",
                                     bg_color = "#f0fff4", # Un verde muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g05_shiny_output.qmd")),
                                     output_file_path    = reactive(file.path(base_folder02(), "tab02_anova_and_tukey.html")))

    mod_06_00_render_and_show_server(id = "anova_03",
                                     super_label = "Model",
                                     bg_color = "#f0fff4", # Un verde muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g05_shiny_output.qmd")),
                                     output_file_path    = reactive(file.path(base_folder02(), "tab03_requeriments.html")))

    mod_06_00_render_and_show_server(id = "anova_04",
                                     super_label = "Response Variable",
                                     bg_color = "#f0fff4", # Un verde muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g05_shiny_output.qmd")),
                                     output_file_path    = reactive(file.path(base_folder02(), "tab04_model.html")))


    mod_06_00_render_and_show_server(id = "anova_05",
                                     super_label = "Residuals",
                                     bg_color = "#f0fff4", # Un verde muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g05_shiny_output.qmd")),
                                     output_file_path    = reactive(file.path(base_folder02(), "tab05_descriptive_rv.html")))



    mod_06_00_render_and_show_server(id = "anova_06",
                                     super_label = "ASA",
                                     bg_color = "#f0fff4", # Un verde muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g05_shiny_output.qmd")),
                                     output_file_path    = reactive(file.path(base_folder02(), "tab06_residuals.html")))


  })
}



