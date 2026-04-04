fn_local_quarto_DT_table_html <- function(df, caption = "Exploración del Dataset") {

  # Validación de datos
  if (is.null(df) || nrow(df) == 0) return("El dataset está vacío.")

  # Definición del caption con estilo usando htmltools
  # Usamos el prefijo para asegurar que 'tags' se reconozca correctamente
  cap <- htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; color: #2c3e50; font-weight: bold; font-size: 1.2em;',
    caption
  )

  # Llamada principal a DT
  DT::datatable(
    df,
    caption = cap,
    filter = 'top',
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50),
      autoWidth = TRUE,
      # dom define la estructura: Buttons/Filter arriba (top), Table (t), Info/Paginado abajo (bottom)
      dom = '<"top"Br>t<"bottom"lip>',
      buttons = list(
        list(extend = 'copy', text = 'Copiar'),
        list(extend = 'csv', text = 'CSV'),
        list(extend = 'excel', text = 'Excel')
      ),
      # Usamos DT::JS para inyectar el código JavaScript de personalización visual
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
        "}"
      )
    ),
    class = 'hover row-border'
  )
}
