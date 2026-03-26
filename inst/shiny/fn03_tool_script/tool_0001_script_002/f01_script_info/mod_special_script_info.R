library(shiny)
library(bslib)

# ==========================================================
# 1. EL MÓDULO (UI y Server)
# ==========================================================

# UI del módulo
mod_special_script_info_ui <- function(id) {
  ns <- NS(id) # Namespace: esencial para que no choquen los IDs
  tagList(
    h1("Scrip 002"),
    p("Este es el módulo más simple de RScience."),
    textOutput(ns("mensaje_server"))
  )
}

# Server del módulo
mod_special_script_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$mensaje_server <- renderText({
      "El Server del módulo también está respondiendo correctamente."
    })

  })
}


