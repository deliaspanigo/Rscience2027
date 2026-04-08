fn_local_gen_zip_from_vector_file_path <- function(vector_file_path, str_output_zip_file_path) {
  utils::zip(
    zipfile = str_output_zip_file_path,
    files   = vector_file_path,
    flags   = "-9Xj"      # -9 = max compresión, -j = sin rutas (junk paths)
  )
}
