

#' @export
run_app_full <- function() {
  # Buscamos la ruta dentro del paquete instalado
  app_path <- system.file("shiny", "f01_user_apps", "app00", "app_full.R", package = "Rscience2027")

  if (app_path == "") {
    stop("No se encontró el archivo de la aplicación. ¿Has instalado el paquete?")
  }

  shiny::runApp(app_path, display.mode = "normal")
}
