library(shiny)

# --- Módulo UI 1 ---
mod_special_reporting_ui <- function(id) {
  ns <- NS(id)
  tagList(span(style = "color: green;", "Este es el Módulo de Reporting - Script 001."))
}



# --- Módulo Server Único ---
# Este servidor orquestará la lógica (aunque en este caso sea mínima)
mod_special_reporting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Aquí iría la lógica reactiva si fuera necesaria
    message("Módulos dede texto inicializados en: ", id)





  })

}





