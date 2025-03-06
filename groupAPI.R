library(jsonlite)
library(readxl)
library(base64enc)

#* @post /upload_file
function(req) {
  # Parse JSON body
  body <- fromJSON(rawToChar(req$postBody))
  
  if (is.null(body$file$value)) {
    return(list(message = "No file uploaded"))
  }
  
  # Decode Base64 to raw binary
  file_bin <- base64decode(body$file$value)
  
  # Save it to a temporary file
  temp_file <- tempfile(fileext = ".xlsx")
  writeBin(file_bin, temp_file)
  
  # Read the Excel file into a dataframe
  data <- read_excel(temp_file)
  
  return(head(data))  # Return first few rows
}
