# --------------------------------------------------
# FROM FILE: f00_01_RQuarto.qmd
# --------------------------------------------------

# # # # # Section 13 - Descriptive Analysis ----------------------------------
# Librerías necesarias para esta sección
library(dplyr)
library(ggplot2)
library(plotly)
library(moments) # Para asimetría y curtosis
library(knitr)
library(kableExtra)

# Aseguramos que los datos provengan del minidataset generado en la sección 06
minidataset <- mtcars
var_name_rv <- "mpg"
data_vector <- minidataset[[var_name_rv]]
var_label   <- var_name_rv

# Función para calcular el bloque estadístico
df_stats_rv <- data.frame(
 "Métrica" = c("N Total", "N Válidos", "Mínimo", "Máximo", "Media", 
               "Mediana", "Varianza", "Desvío Estándar", "Coef. Variación (%)",
               "Error Estándar", "Asimetría", "Curtosis"),
 "Valor" = c(
   length(data_vector),
   sum(!is.na(data_vector)),
   min(data_vector, na.rm = TRUE),
   max(data_vector, na.rm = TRUE),
   mean(data_vector, na.rm = TRUE),
   median(data_vector, na.rm = TRUE),
   var(data_vector, na.rm = TRUE),
   sd(data_vector, na.rm = TRUE),
   (sd(data_vector, na.rm = TRUE) / mean(data_vector, na.rm = TRUE)) * 100,
   sd(data_vector, na.rm = TRUE) / sqrt(sum(!is.na(data_vector))),
   moments::skewness(data_vector, na.rm = TRUE),
   moments::kurtosis(data_vector, na.rm = TRUE)
 )
)

knitr::kable(df_stats_rv, digits = 4, caption = paste("Estadísticos de", var_label)) %>%
 kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

prob_vector <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
df_percentiles <- data.frame(
 "Percentil" = paste0(prob_vector * 100, "%"),
 "Valor" = quantile(data_vector, probs = prob_vector, na.rm = TRUE)
)

knitr::kable(df_percentiles, row.names = FALSE) %>%
 kableExtra::kable_styling(bootstrap_options = "bordered", full_width = F)

plot_hist <- ggplot(data.frame(val = data_vector), aes(x = val)) +
 geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
 geom_density(color = "darkred", size = 1) +
 labs(title = paste("Histograma de", var_label), x = var_label, y = "Densidad") +
 theme_minimal()

plotly::ggplotly(plot_hist)

plot_box <- ggplot(data.frame(val = data_vector), aes(y = val, x = "")) +
 geom_boxplot(fill = "lightgray", outlier.color = "red", outlier.shape = 16) +
 labs(title = paste("Boxplot de", var_label), y = var_label, x = "") +
 theme_minimal() +
 coord_flip()

plotly::ggplotly(plot_box)


