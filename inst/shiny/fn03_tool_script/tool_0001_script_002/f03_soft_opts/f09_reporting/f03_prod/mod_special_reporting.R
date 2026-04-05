



library(shiny)
library(bslib)
library(quarto)
library(shinyjs)


# ==============================================================================
# 2. MÓDULO CONTENEDOR: mod_special_script_comment
# ==============================================================================

mod_special_reporting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(class = "special-comment-wrapper",
        card(
          title = "Editor de Reportes Quarto",
            mod_06_00_render_and_show_ui(id = ns("anova_01")),
            mod_06_00_render_and_show_ui(id = ns("anova_02"))


        )
    )
  )
}

mod_special_reporting_server <- function(id, folder_temp_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_folder01 <- reactive({
      req(folder_temp_path())
      file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render")
    })

    # base_folder02 <- reactive({
    #   req(folder_temp_path())
    #   file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "f03_script_internal", "output")
    # })

    # base_folder <- reactive({
    #   req(folder_temp_path())
    #   file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "f03_script")
    # })

    # Ejemplo con colores distintos configurados desde el Server
    mod_06_00_render_and_show_server(id = "anova_01",
                                     super_label = "Script and Comments (HTML-ZIP)",
                                     bg_color = "#e7f1ff", # Un azul muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "g02_quarto_mod" , "AAA_01_RUNNER_g02_quarto_mod.qmd")),
                                     output_file_path    = reactive(file.path(base_folder01(), "g02_quarto_mod", "output", "script_comments_html.zip")),
                                     show_debug = T,
                                     show_file = F)


    mod_06_00_render_and_show_server(id = "anova_02",
                                     super_label = "Script only (R - ZIP)",
                                     bg_color = "#f0fff4", # Un verde muy suave
                                     input_file_path_qmd = reactive(file.path(base_folder01(), "g04_script_external", "AAA_01_RUNNER_g04_script_external.qmd")),
                                     output_file_path    = reactive(file.path(base_folder01(), "g04_script_external", "output", "R_scripts.zip")),
                                     show_debug = T,
                                     show_file = F)




  })
}



