fn_local_quarto_pretty_table_html <- function(df, title = "", color = "#2c3e50") {

  # Validación de datos
  if (is.null(df) || nrow(df) == 0) return("No hay datos disponibles.")

  # IMPORTANTE: kbl() pertenece a kableExtra
  df |>
    kableExtra::kbl(caption = title, align = "c") |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = TRUE,
      position = "center",
      font_size = 14
    ) |>
    kableExtra::column_spec(1, bold = TRUE, border_right = TRUE) |>
    kableExtra::row_spec(0, background = color, color = "white", bold = TRUE)
}
