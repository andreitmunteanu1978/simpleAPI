library(plumber)
library(base64enc)
library(readxl)

#* @apiTitle File Upload API
#* @apiDescription This API uploads an attachment, decodes it, and reads the Excel file from memory.

#* Upload file and count rows in the Excel sheet
#* @post /upload
#* @param req The HTTP request containing the file data
#* @response 200 Returns the row count of the Excel file
function(req) {
  # 1. Read the raw POST body (base64 encoded)
  body <- rawToChar(req$postBody)
  
  # 2. Decode the base64 content
  decoded_data <- base64decode(body)
  
  # 3. Create a raw connection to read the Excel file directly from memory
  conn <- rawConnection(decoded_data)
  
  # 4. Read the Excel file from the raw connection
  data <- read_excel(conn)
  
  # 5. Close the connection
  close(conn)
  
  # 6. Count rows in the data
  num_rows <- nrow(data)
  
  # 7. Return the row count
  return(list(row_count = num_rows))
}
