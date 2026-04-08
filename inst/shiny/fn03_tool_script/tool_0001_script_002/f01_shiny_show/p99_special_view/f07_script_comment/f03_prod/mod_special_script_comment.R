library(shiny)
library(bslib)
library(quarto)
library(shinyjs)


# ==============================================================================
# 2. MÓDULO CONTENEDOR: mod_special_script_comment
# ==============================================================================

mod_special_script_comment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(class = "special-comment-wrapper",
        navset_card_tab(
          title = "Editor de Reportes Quarto",
          nav_panel(
            title = "Import and Control",
            mod_06_00_render_and_show_ui(id = ns("anova_01"))
          ),
          nav_panel(
            title = "Full Test",
            mod_06_00_render_and_show_ui(id = ns("anova_02"))
          ),
          nav_panel(
            title = "Model",
            mod_06_00_render_and_show_ui(id = ns("anova_03"))
          ),
          nav_panel(
            title = "Response Variable",
            mod_06_00_render_and_show_ui(id = ns("anova_04"))
          ),
          nav_panel(
            title = "Residuals",
            mod_06_00_render_and_show_ui(id = ns("anova_05"))
          ),
          nav_panel(
            title = "ASA",
            mod_06_00_render_and_show_ui(id = ns("anova_06"))
          )
        )
    )
  )
}

mod_special_script_comment_server <- function(id, folder_temp_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_folder01 <- reactive({
      req(folder_temp_path())
      file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "g02_quarto_mod")
    })

    base_folder02 <- reactive({
      req(folder_temp_path())
      file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "f02_quarto_mod")
    })

    # Ejemplo con colores distintos configurados desde el Server
    mod_06_00_render_and_show_server(id = "anova_01",
                            super_label = "Import and Control",
                            bg_color = "#e7f1ff", # Un azul muy suave
                            input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g02_quarto_mod.qmd")),
                            output_file_path    = reactive(file.path(base_folder02(), "file01_anova_import_and_control.html")))

    mod_06_00_render_and_show_server(id = "anova_02",
                            super_label = "Full Test",
                            bg_color = "#f0fff4", # Un verde muy suave
                            input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g02_quarto_mod.qmd")),
                            output_file_path    = reactive(file.path(base_folder02(), "file02_anova_full_test.html")))

    mod_06_00_render_and_show_server(id = "anova_03",
                             super_label = "Model",
                             bg_color = "#f0fff4", # Un verde muy suave
                             input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g02_quarto_mod.qmd")),
                             output_file_path    = reactive(file.path(base_folder02(), "file03_anova_model.html")))

    mod_06_00_render_and_show_server(id = "anova_04",
                             super_label = "Response Variable",
                             bg_color = "#f0fff4", # Un verde muy suave
                             input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g02_quarto_mod.qmd")),
                             output_file_path    = reactive(file.path(base_folder02(), "file04_anova_descriptive_rv.html")))


    mod_06_00_render_and_show_server(id = "anova_05",
                             super_label = "Residuals",
                             bg_color = "#f0fff4", # Un verde muy suave
                             input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g02_quarto_mod.qmd")),
                             output_file_path    = reactive(file.path(base_folder02(), "file05_anova_descriptive_residuals.html")))



    mod_06_00_render_and_show_server(id = "anova_06",
                             super_label = "ASA",
                             bg_color = "#f0fff4", # Un verde muy suave
                             input_file_path_qmd = reactive(file.path(base_folder01(), "AAA_01_RUNNER_g02_quarto_mod.qmd")),
                             output_file_path    = reactive(file.path(base_folder02(), "file06_anova_asa.html")))


  })
}
