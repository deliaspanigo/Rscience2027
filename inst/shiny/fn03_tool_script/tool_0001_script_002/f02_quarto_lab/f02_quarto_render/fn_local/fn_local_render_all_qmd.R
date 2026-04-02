fn_local_render_all_qmd <- function(vector_qmd_file_path) {

  vector_qmd_file_path <- normalizePath(vector_qmd_file_path, mustWork = TRUE)

  if (length(vector_qmd_file_path) == 0) stop("No se encontraron archivos .qmd")

  old_wd <- getwd()

  for (selected_qmd_file_path in vector_qmd_file_path) {
    tryCatch({
      folder_qmd <- dirname(selected_qmd_file_path)
      qmd_file   <- basename(selected_qmd_file_path)

      setwd(folder_qmd)
      quarto::quarto_render(input = qmd_file, quiet = T,
                            output_format = "all")

    }, error = function(e) cat("✗ Error en", basename(qmd_file), "\n"))
  }
  setwd(old_wd)
}
