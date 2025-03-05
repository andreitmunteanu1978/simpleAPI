library(plumber)
library(readxl)

#* @post /upload
#* @param req The HTTP request containing file & path
#* @response 200 Returns original file path
function(req) {
  # Extract file
  file <- req$files$file
  file_path <- req$body$file_path  # Extract original file path

  if (is.null(file)) {
    return(list(error = "No file uploaded"))
  }

  return(list(
    message = "File received",
    original_path = file_path
  ))
}
