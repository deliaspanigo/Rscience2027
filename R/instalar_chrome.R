# Instalar processx si no lo tienes
install.packages("processx")

# Cargar librerías
library(plotly)
library(processx)

# Crear el gráfico
fig <- plot_ly(mtcars, x = ~wt, y = ~mpg, type = 'scatter', mode = 'markers')

# Exportar usando orca (necesitas tener orca instalado en el sistema)
# Si no tienes orca, la instalación es:
# conda install -c plotly plotly-orca
# O descarga desde: https://github.com/plotly/orca/releases

orca(fig, "grafico_mtcars.png")
