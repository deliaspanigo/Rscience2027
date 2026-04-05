fn_local_gen_R_script_from_qmd <- function(vector_input_qmd_file_path,
                                           str_input = "file",
                                           ext_input = ".qmd",
                                           vector_output_R_script,
                                           str_output = "script",
                                           ext_output = ".R") {

  vector_input_qmd_file_path <- normalizePath(vector_input_qmd_file_path, mustWork = TRUE)
  n_files <- length(vector_input_qmd_file_path)

  if (n_files != length(vector_output_R_script)) {
    stop("ERROR CRÍTICO: Los vectores tienen longitudes diferentes.")
  }

  # FASE 1: VERIFICACIÓN
  cat("Fase 1: Verificando coherencia de nombres...\n")
  indices_error <- c()

  for (i in 1:n_files) {
    name_in <- basename(vector_input_qmd_file_path[i])
    id_in   <- gsub(paste0("^", str_input, "|", ext_input, "$"), "", name_in)

    # AQUÍ ESTABA EL ERROR (CORREGIDO)
    name_out_no_ext <- tools::file_path_sans_ext(basename(vector_output_R_script[i]))
    id_out  <- gsub(paste0("^", str_output), "", name_out_no_ext)

    if (id_in != id_out) {
      cat("  [!] Error en índice", i, ": '", id_in, "' vs '", id_out, "'\n")
      indices_error <- c(indices_error, i)
    }
  }

  if (length(indices_error) > 0) {
    stop("PROCESO CANCELADO por inconsistencias en los nombres.")
  }

  # FASE 2: EJECUCIÓN
  cat("Fase 2: Generando archivos .R...\n")
  for (i in 1:n_files) {
    qmd_path <- vector_input_qmd_file_path[i]
    r_path   <- paste0(tools::file_path_sans_ext(vector_output_R_script[i]), ext_output)

    dir_out <- dirname(r_path)
    if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

    tryCatch({
      content <- readLines(qmd_path, warn = FALSE, encoding = "UTF-8")
      writeLines(content, con = r_path, useBytes = TRUE)
      cat("  ✓ Generado:", basename(r_path), "\n")
    }, error = function(e) {
      cat("  ✗ Error al escribir", basename(r_path), ":", e$message, "\n")
    })
  }
  cat("\n¡Proceso terminado!\n")
}
