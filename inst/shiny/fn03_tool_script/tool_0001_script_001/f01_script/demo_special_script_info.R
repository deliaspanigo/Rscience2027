# ==========================================================
# 2. LA APP QUE LO ENVUELVE
# ==========================================================


source(file = "mod_special_script_info.R")


ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_special_script_info_ui("id_de_prueba") # Llamamos a la UI
)

server <- function(input, output, session) {
  mod_special_script_info_server("id_de_prueba") # Llamamos al Server
}

# Lanzar
shinyApp(ui, server)
