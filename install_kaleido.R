# Instala/miniconda si no lo tenés
reticulate::install_miniconda(update = TRUE, force = FALSE)

# Instala los paquetes de Python necesarios
reticulate::py_install("plotly", pip = TRUE)
reticulate::py_install("kaleido", pip = TRUE)

# O en una sola línea (recomendado):
reticulate::py_install(c("plotly", "kaleido"), pip = TRUE)
