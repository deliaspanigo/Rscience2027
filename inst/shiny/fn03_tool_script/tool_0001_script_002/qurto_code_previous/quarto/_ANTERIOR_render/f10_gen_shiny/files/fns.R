library(knitr)
library(kableExtra)

fn_BIG_BLUE_LINE <- function(){


linea_azul_gruesa <- '<div style="
  width: 100%;                  /* Ocupa todo el ancho disponible */
  height: 0.5cm;                /* Medio centímetro de grosor */
  background-color: #2E86C1;       /* Color azul */
  margin-top: 20px;             /* Espacio superior para separación */
  margin-bottom: 20px;          /* Espacio inferior para separación */
"></div>'

  linea_azul_gruesa
}

################################################################################

fn_show_table_html <- function(object_name, caption_text = ""){



  # 1. Recuperar el data frame del entorno global
  my_df <- tryCatch(
    { get(object_name, envir = .GlobalEnv) },
    error = function(e) { stop(paste("Error: El objeto", object_name, "no se encontró en el entorno global.")) }
  )

  # 2. Generar la tabla HTML
  # CAMBIO 1: Añadir align = "c" (centra el contenido de las celdas)
  # Necesitas una 'c' por cada columna en tu data frame.
  # Usaremos paste(rep("c", ncol(my_df)), collapse = "") para hacerlo dinámico.
  col_align <- paste(rep("c", ncol(my_df)), collapse = "")

  kable(my_df, format = "html", caption = caption_text, align = col_align) %>%
    # CAMBIO 2: Añadir position = "center" (centra la tabla completa en la página)
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE,
                              position = "center")
}

fn_show_table_pdf <- function(object_name, caption_text = ""){

  # 1. Recuperar el data frame del entorno global
  my_df <- tryCatch(
    { get(object_name, envir = .GlobalEnv) },
    error = function(e) { stop(paste("Error: El objeto", object_name, "no se encontró en el entorno global.")) }
  )

  # 2. Generar la tabla LaTeX/PDF
  kable(my_df, format = "latex", caption = caption_text, booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "scale_down"),
                              full_width = FALSE)
}

fn_show_table_pdf_typst <- function(object_name, caption_text = ""){
  # 1. Recuperar el data frame del entorno global
  my_df <- tryCatch(
    { get(object_name, envir = .GlobalEnv) },
    error = function(e) { stop(paste("Error: El objeto", object_name, "no se encontró en el entorno global.")) }
  )

  # 2. Generar la tabla en formato Markdown (pipe),
  # que es universalmente bien soportado por Quarto/Typst/PDF/Word.
  # Usaremos kable sin kableExtra para la compatibilidad más amplia en formatos no-LaTeX.
  kable(my_df, format = "pipe", caption = caption_text)
}


# (Mantén fn_show_table_html como está)

fn_show_table_dynamic <- function(object_name, caption_text = ""){

  # 1. Detección del formato de salida
  is_html <- knitr::is_html_output()

  # 2. Llamada a la función específica
  if (is_html) {
    # Llama a la función HTML
    fn_show_table_html(object_name = object_name, caption_text = caption_text)
  } else {
    # Llama a la función PDF (funciona también para Word, aunque la tabla será simple)
    fn_show_table_pdf_typst(object_name = object_name, caption_text = caption_text)
  }
}

################################################################################

fn_show_special_table01_html <- function(object_name, caption_text = ""){



  # 1. Recuperar el data frame del entorno global
  my_df <- tryCatch(
    { get(object_name, envir = .GlobalEnv) },
    error = function(e) { stop(paste("Error: El objeto", object_name, "no se encontró en el entorno global.")) }
  )

  # 2. Generar la tabla HTML
  # CAMBIO 1: Añadir align = "c" (centra el contenido de las celdas)
  # Necesitas una 'c' por cada columna en tu data frame.
  # Usaremos paste(rep("c", ncol(my_df)), collapse = "") para hacerlo dinámico.
  col_align <- paste(rep("c", ncol(my_df)), collapse = "")

  kable(my_df, format = "html", caption = caption_text, align = col_align) %>%
    # CAMBIO 2: Añadir position = "center" (centra la tabla completa en la página)
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE,
                              position = "center",
                              font_size = 12) %>%
    kableExtra::column_spec(1, width = "8%") %>%    # Caso
    kableExtra::column_spec(2, width = "8%") %>%    # Control OK
    kableExtra::column_spec(3, width = "10%") %>%   # Normalidad
    kableExtra::column_spec(4, width = "12%") %>%   # Homogeneidad
    kableExtra::column_spec(5, width = "10%") %>%   # ANOVA
    kableExtra::column_spec(6, width = "12%") %>%   # Tukey
    kableExtra::column_spec(7, width = "40%")       # Decisión (más ancha)
}

fn_show_special_table01_pdf <- function(object_name, caption_text = ""){

  # 1. Recuperar el data frame del entorno global
  my_df <- tryCatch(
    { get(object_name, envir = .GlobalEnv) },
    error = function(e) { stop(paste("Error: El objeto", object_name, "no se encontró en el entorno global.")) }
  )

  # 2. Generar la tabla LaTeX/PDF
  kable(my_df, format = "latex", caption = caption_text, booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "scale_down"),
                              full_width = FALSE)
}

fn_show_special_table01_dynamic <- function(object_name, caption_text = ""){

  # 1. Detección del formato de salida
  is_html <- knitr::is_html_output()

  # 2. Llamada a la función específica
  if (is_html) {
    # Llama a la función HTML
    fn_show_special_table01_html(object_name = object_name, caption_text = caption_text)
  } else {
    # Llama a la función PDF (funciona también para Word, aunque la tabla será simple)
    fn_show_special_table01_pdf(object_name = object_name, caption_text = caption_text)
  }
}
################################################################################
