library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

devtools::load_all() # Descomenta si estás en modo desarrollo

#source(file = "mod_special_shiny_output.R")



# Ejecución limpia
ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("RS-STYLES/css/style_000.css?v=", as.numeric(Sys.time())) # Nota el /css/ adicional si registraste 'www'
    )
  ),
  mod_99_show_one_file_ui("my_ns_special_cite"),
  #mod_special_cite_DEBUG_ui("my_ns_special_cite")# Llamamos a la UI
)

#ui <-
file_path <- system.file("test_shiny_output", "f05_shiny_output", "tab01_control.html", package = "Rscience2027")
server <- function(input, output, session) {

  mod_99_show_one_file_server(id = "my_ns_special_cite",
                              file_path = reactive(file_path),
                              show_file = TRUE,
                              show_label = TRUE)
}

shinyApp(ui, server)
