library(plumber)
library(readxl)

#* @apiTitle File Upload API
#* @apiDescription This API uploads an attachment and counts rows from an Excel file.

#* Upload file and count rows
#* @post /upload
#* @param req The HTTP request containing the file data
#* @response 200 Returns the row count of the Excel file
function(req) {
  # Read the raw binary content from the request body
  body <- rawToChar(req$postBody)
  
  # Decode the base64 content (if provided in base64)
  decoded_data <- base64enc::base64decode(body)
  
  # Save the decoded data as an Excel file
  file_path <- "temp.xlsx"
  writeBin(decoded_data, file_path)
  
  # Read the Excel file to count rows
  data <- read_excel(file_path, sheet="Daily RPP")
  num_rows <- nrow(data)
  
  # Return the row count
  return(list(row_count = num_rows))
}
