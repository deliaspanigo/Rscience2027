library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#00d4ff" # El celeste Rscience
  ),

  # --- ESTILOS CSS PARA FLUIDEZ Y DINAMISMO ---
  tags$head(
    tags$style(HTML("
      body {
        background: radial-gradient(circle at top right, #ffffff, #e9ecef);
        overflow: hidden; /* Evita scroll innecesario en la bienvenida */
      }

      .welcome-card {
        margin-top: 10vh;
        animation: fadeIn 1.2s ease-out;
      }

      /* Animación de Flotación para el Logo */
      .floating-logo {
        width: 220px;
        filter: drop-shadow(0 15px 25px rgba(0,0,0,0.1));
        animation: float 4s ease-in-out infinite;
      }

      @keyframes float {
        0% { transform: translateY(0px); }
        50% { transform: translateY(-15px); }
        100% { transform: translateY(0px); }
      }

      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }

      /* Botón Pro con Brillo */
      .btn-launch {
        background: #00d4ff;
        border: none;
        color: white;
        padding: 15px 40px;
        font-size: 1.4rem;
        font-weight: 800;
        border-radius: 50px;
        transition: all 0.3s ease;
        box-shadow: 0 10px 20px rgba(0,212,255,0.3);
      }

      .btn-launch:hover {
        transform: scale(1.05);
        background: #00b8e6;
        box-shadow: 0 15px 30px rgba(0,212,255,0.5);
      }

      .version-badge {
        font-family: 'Monaco', monospace;
        letter-spacing: 2px;
        background: #2c3e50;
        color: white;
        padding: 5px 15px;
        border-radius: 5px;
        font-size: 0.8rem;
      }
    "))
  ),

  # --- ESTRUCTURA VISUAL ---
  div(
    class = "container welcome-card",
    div(
      class = "row justify-content-center",
      div(
        class = "col-md-6 text-center",

        # Logo Animado
        img(src = "Rscience_logo_sticker.png", class = "floating-logo mb-5"),

        # Textos de Bienvenida
        h1("Rscience 2027", style = "font-weight: 900; font-size: 3.5rem; color: #2c3e50;"),
        p("Advanced Scientific Computing Environment",
          class = "lead text-secondary mb-5",
          style = "font-style: italic;"),

        # Botón de Acción
        actionButton("enter_app", "LAUNCH ENGINE", class = "btn-launch mb-5"),

        br(),

        # Pie de página de la versión
        span("VERSION STABLE", class = "text-muted small text-uppercase me-2"),
        span("v.0.0.1", class = "version-badge")
      )
    )
  )
)

server <- function(input, output, session) {
  # Por ahora, solo un mensaje de consola al hacer clic
  observeEvent(input$enter_app, {
    print("Iniciando motor estadístico Rscience...")
    # Aquí es donde linkearemos la otra app después.
  })
}

shinyApp(ui, server)
