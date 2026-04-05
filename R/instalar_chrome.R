# ============================================
# CÓDIGO CORREGIDO - Funciona en Linux
# ============================================

# 1. Instalar paquetes (si no están)
if (!require("plotly")) install.packages("plotly")
if (!require("htmlwidgets")) install.packages("htmlwidgets")
if (!require("webshot2")) install.packages("webshot2")
if (!require("chromote")) install.packages("chromote")

# 2. Cargar paquetes
library(plotly)
library(htmlwidgets)
library(webshot2)
library(chromote)

# 3. Configurar Chrome para usar --no-sandbox (SOLUCIÓN PARA LINUX)
# Esto evita el error de sandbox
options(chromote.chrome_args = c("--no-sandbox", "--disable-dev-shm-usage"))

# 4. Verificar que Chrome está disponible
chrome_path <- chromote::find_chrome()
cat("Chrome en:", chrome_path, "\n")

# 5. Crear el gráfico
fig <- plot_ly(
  data = mtcars,
  x = ~wt,
  y = ~mpg,
  type = "scatter",
  mode = "markers",
  marker = list(size = 12, color = 'rgba(255, 71, 71, .9)')
) %>%
  layout(title = "Prueba de Exportación - Linux")

# 6. Guardar como HTML temporal
temp_html <- tempfile(fileext = ".html")
saveWidget(fig, temp_html, selfcontained = TRUE)
cat("HTML creado:", temp_html, "\n")

# 7. Convertir a PNG (ahora con --no-sandbox)
cat("Convirtiendo a PNG...\n")
webshot2::webshot(
  url = temp_html,
  file = "mi_grafico_prueba.png",
  vwidth = 1000,
  vheight = 800,
  zoom = 2
)

# 8. Limpiar
unlink(temp_html)

# 9. Verificar
if (file.exists("mi_grafico_prueba.png")) {
  cat("\n==========================================\n")
  cat("✅ ÉXITO!\n")
  cat("Archivo:", file.path(getwd(), "mi_grafico_prueba.png"), "\n")
  cat("Tamaño:", round(file.size("mi_grafico_prueba.png") / 1024, 1), "KB\n")
  cat("==========================================\n")
} else {
  cat("\n❌ FALLÓ\n")
}
