library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: DARK NAVY & NEON CYAN EDITION
# ==============================================================================
css_rscience_v2 <- "
/* Fondo General Estilo Laboratorio */
body { background-color: #080c10; color: #e1e1e1; font-family: 'Inter', sans-serif; }

.bslib-sidebar-content { background-color: #0f171e !important; padding: 10px !important; }

/* ACORDEÓN CUSTOM */
.accordion { --bs-accordion-bg: transparent; --bs-accordion-border-color: rgba(0, 212, 255, 0.1); }

.accordion-item {
  background-color: #16222c !important;
  margin-bottom: 12px;
  border-radius: 10px !important;
  border: 1px solid rgba(255,255,255,0.05) !important;
  transition: all 0.4s ease;
}

/* EL REBORDE CYAN ACTIVO (PACK ACTUAL) */
.active-pack {
  border: 2px solid #00d4ff !important;
  box-shadow: 0 0 15px rgba(0, 212, 255, 0.3);
  transform: translateX(5px);
}

.accordion-button {
  background-color: #16222c !important;
  color: #556b7d !important;
  font-weight: 800;
  text-transform: uppercase;
  font-size: 0.7rem;
  letter-spacing: 1px;
}

.active-pack .accordion-button { color: #00d4ff !important; }

/* ITEMS DEL MENÚ */
.menu-item-btn {
  width: 100%;
  text-align: left;
  background: rgba(255,255,255,0.02);
  border: none;
  color: #a0aec0;
  padding: 10px 15px;
  margin-bottom: 4px;
  border-radius: 6px;
  font-size: 0.8rem;
  display: flex;
  justify-content: space-between;
  align-items: center;
  transition: 0.3s;
}

.menu-item-btn:hover:not(:disabled) {
  background: rgba(0, 212, 255, 0.1);
  color: #fff;
}

.menu-item-btn.selected {
  background: linear-gradient(90deg, #00d4ff 0%, #008eb3 100%) !important;
  color: #080c10 !important;
  font-weight: 700;
  box-shadow: 0 4px 10px rgba(0, 212, 255, 0.2);
}

.menu-item-btn:disabled { opacity: 0.2; cursor: not-allowed; }

/* ICONOS DE ESTADO */
.icon-locked { color: #4a5568; }
.icon-unlocked { color: #ff9100; animation: pulse 2s infinite; }
.icon-done { color: #28a745; }

@keyframes pulse {
  0% { opacity: 1; } 50% { opacity: 0.4; } 100% { opacity: 1; }
}
"

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  title = span("RSCIENCE", style="font-weight:900; color:#00d4ff; letter-spacing:2px;"),
  theme = bs_theme(version = 5, bg = "#080c10", fg = "#e1e1e1", primary = "#00d4ff"),
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_v2))),

  sidebar = sidebar(
    width = 320,
    accordion(
      id = "acc_menu",
      open = "Pack 01",

      # --- PACK 01: SETUP (Settings movido aquí) ---
      accordion_panel(
        "I. Engine Setup",
        value = "Pack 01",
        tags$button(id="btn_dataset",  class="menu-item-btn selected", "Dataset Selection", icon("database")),
        tags$button(id="btn_tool",     class="menu-item-btn", "Tool Engine", icon("screwdriver-wrench")),
        tags$button(id="btn_script",   class="menu-item-btn", "Script Engine", icon("code")),
        hr(style="border-top: 1px solid rgba(0,212,255,0.2)"),
        tags$button(id="btn_settings", class="menu-item-btn", "General Settings", icon("sliders"))
      ),

      # --- PACK 02: ANALYSIS & INFO ---
      accordion_panel(
        "II. Analysis Phase",
        value = "Pack 02",
        tags$button(id="btn_theory",   class="menu-item-btn", "Theory & Docs", icon("book")),
        tags$button(id="btn_shiny",    class="menu-item-btn", "Shiny Outputs", icon("desktop")),
        tags$button(id="btn_report",   class="menu-item-btn", "Final Reporting", icon("file-export"))
      )
    )
  ),

  # CONTENIDO PRINCIPAL
  card(
    full_screen = TRUE,
    card_header("Workstation Console"),
    layout_column_wrap(
      width = 1/2,
      value_box(
        title = "Paso Actual",
        value = textOutput("current_step_txt"),
        showcase = icon("terminal"),
        theme = "secondary"
      ),
      wellPanel(
        h5("Simulador de Flujo"),
        p("Al completar el paso 'Settings', se habilitará el Pack 02."),
        actionButton("lock_settings", "Bloquear Settings y Abrir Pack 02", class="btn-cyan w-100")
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # 1. Estado Inicial
  observe({
    # Deshabilitamos Pack 02
    disable("btn_theory")
    disable("btn_shiny")
    disable("btn_report")

    # Marcamos Pack 01 como activo (Reborde Cyan)
    addClass(selector = ".accordion-item:nth-child(1)", class = "active-pack")
  })

  # 2. Manejo de Selección de Botones
  all_btns <- c("btn_dataset", "btn_tool", "btn_script", "btn_settings",
                "btn_theory", "btn_shiny", "btn_report")

  active_btn <- reactiveVal("btn_dataset")

  lapply(all_btns, function(id) {
    observeEvent(input[[id]], {
      lapply(all_btns, function(b) removeClass(b, "selected"))
      addClass(id, "selected")
      active_btn(id)
    })
  })

  # 3. Lógica de Transición (Settings -> Pack 02)
  observeEvent(input$lock_settings, {
    # Efecto visual de 'Completado' en Settings
    runjs("$('#btn_settings').css('border-right', '4px solid #28a745');")

    # Habilitar Pack 02
    enable("btn_theory")
    enable("btn_shiny")
    enable("btn_report")

    # Cambiar Reborde Cyan al Pack 02
    removeClass(selector = ".accordion-item", class = "active-pack")
    addClass(selector = ".accordion-item:nth-child(2)", class = "active-pack")

    # Abrir el acordeón automáticamente
    accordion_panel_open("acc_menu", values = "Pack 02")
  })

  output$current_step_txt <- renderText({ active_btn() })
}

shinyApp(ui, server)
