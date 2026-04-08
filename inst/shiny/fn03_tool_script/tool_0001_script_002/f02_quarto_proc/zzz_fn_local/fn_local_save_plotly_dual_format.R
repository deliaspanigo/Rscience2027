
fn_local_save_plotly_dual_format <- function(list_plots, output_dir = "plotly_exports", width = 1200, height = 800) {

  library(htmlwidgets)
  library(webshot2)
  library(pagedown) # Usamos pagedown para la detección de Chrome

  # --- DETECCIÓN / INSTALACIÓN AUTOMÁTICA ---
  chrome_path <- tryCatch({
    pagedown::find_chrome()
  }, error = function(e) {
    message("No se encontró Chrome. Intentando instalar una versión local...")
    # Si no lo encuentra, podrías dar instrucciones o usar Sys.setenv si sabes que existe
    return(NULL)
  })

  if (!is.null(chrome_path)) {
    Sys.setenv(CHROMOTE_CHROME = chrome_path)
  }

  # --- INFRAESTRUCTURA DE CARPETAS ---
  dir_html <- file.path(output_dir, "html")
  dir_png  <- file.path(output_dir, "png")
  if (!dir.exists(dir_html)) dir.create(dir_html, recursive = TRUE)
  if (!dir.exists(dir_png)) dir.create(dir_png, recursive = TRUE)

  dir_original <- getwd()
  on.exit(setwd(dir_original))

  path_html_abs <- normalizePath(dir_html, winslash = "/", mustWork = TRUE)
  path_png_abs  <- normalizePath(dir_png, winslash = "/", mustWork = TRUE)

  # --- BUCLE DE PROCESADO ---
  for (i in seq_along(list_plots)) {
    nombre_base <- if (!is.null(names(list_plots)[i]) && names(list_plots)[i] != "") {
      names(list_plots)[i]
    } else {
      paste0("plot_", i)
    }

    file_html <- paste0(nombre_base, ".html")
    file_png  <- paste0(nombre_base, ".png")

    try({
      # A. Guardar HTML Autocontenido
      setwd(path_html_abs)
      saveWidget(list_plots[[i]], file_html, selfcontained = TRUE, libdir = "lib_temp")
      if (dir.exists("lib_temp")) unlink("lib_temp", recursive = TRUE)

      # B. Capturar PNG desde el HTML
      full_path_html <- file.path(path_html_abs, file_html)
      setwd(path_png_abs)

      # webshot2 usará automáticamente el CHROMOTE_CHROME que seteamos arriba
      webshot(
        url = full_path_html,
        file = file_png,
        vwidth = width,
        vheight = height,
        delay = 2,    # Un poco más de delay por seguridad
        zoom = 2
      )

      message("Exportado Dual OK: ", nombre_base)
    })
    setwd(dir_original)
  }

  return(invisible(TRUE))
}
