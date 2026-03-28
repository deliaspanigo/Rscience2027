library(shiny)

# --- Módulo UI 1 ---
mod_special_theory_ui <- function(id) {
  ns <- NS(id)
  tagList(span(style = "color: green;", "Este es el Módulo de Theory - Script 002."))
}



# --- Módulo Server Único ---
# Este servidor orquestará la lógica (aunque en este caso sea mínima)
mod_special_theory_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Aquí iría la lógica reactiva si fuera necesaria
    message("Módulos dede texto inicializados en: ", id)





  })

}





