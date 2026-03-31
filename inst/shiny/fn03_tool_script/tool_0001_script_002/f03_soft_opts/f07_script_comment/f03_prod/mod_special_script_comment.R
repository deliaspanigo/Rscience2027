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
            title = "Anova Original 01",
            mod_06_00_render_and_show_ui(id = ns("anova_01"))
          ),
          nav_panel(
            title = "Anova Original 02",
            mod_06_00_render_and_show_ui(id = ns("anova_02"))
          )
        )
    )
  )
}

mod_special_script_comment_server <- function(id, folder_temp_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_folder <- reactive({
      req(folder_temp_path())
      file.path(folder_temp_path(), "f02_quarto_lab", "f02_quarto_render", "f02_quarto_mod")
    })

    # Ejemplo con colores distintos configurados desde el Server
    mod_06_00_render_and_show_server(id = "anova_01",
                            super_label = "Análisis de Varianza Principal",
                            bg_color = "#e7f1ff", # Un azul muy suave
                            input_file_path_qmd = reactive(file.path(base_folder(), "file01_anova_original.qmd")),
                            output_file_path    = reactive(file.path(base_folder(), "file01_anova_original.html")))

    mod_06_00_render_and_show_server(id = "anova_02",
                            super_label = "Réplica Experimental 02",
                            bg_color = "#f0fff4", # Un verde muy suave
                            input_file_path_qmd = reactive(file.path(base_folder(), "file01_anova_original_02.qmd")),
                            output_file_path    = reactive(file.path(base_folder(), "file01_anova_original_02.html")))
  })
}
