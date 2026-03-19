# --- LANZADOR DE LA APP ---
ui <- mod_rscience_ui("engine_instancia_1")

server <- function(input, output, session) {
  mod_rscience_server("engine_instancia_1")
}

shinyApp(ui, server)
