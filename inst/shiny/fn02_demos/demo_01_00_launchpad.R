# --- LANZADOR DE LA APP ---
devtools::load_all()
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  mod_01_00_launchpad_ui("launchpad_instancia_1")



)



server <- function(input, output, session) {
  mod_01_00_launchpad_server(id = "launchpad_instancia_1", show_debug = T)



}

shinyApp(ui, server)
