# --- LANZADOR DE LA APP ---
devtools::load_all()

ui <- mod_02_00_rscience_ui(id = "engine_instancia_1")

server <- function(input, output, session) {
  mod_02_00_rscience_server(id = "engine_instancia_1")
}

shinyApp(ui, server)
