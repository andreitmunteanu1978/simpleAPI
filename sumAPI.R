library(plumber)
library(readxl)

#* @apiTitle Excel File Row Count API
#* @apiDescription Upload an Excel file and returns the number of rows in the data.

#* Upload an Excel file and return the number of rows
#* @param file:file The Excel file to upload
#* @post /upload_excel
function(req) {
  # Check if the file is uploaded
  if (is.null(req$files$file)) {
    return(list(error = "No file uploaded."))
  }
  
  # Extract the file details
  file_info <- req$files$file
  
  # Read the Excel file
  tryCatch({
    # Read the first sheet of the Excel file (you can adjust this for specific sheets if needed)
    data <- read_excel(file_info$datapath)
    
    # Get the number of rows in the data
    row_count <- nrow(data)
    
    # Return the row count
    return(list(row_count = row_count))
  }, error = function(e) {
    # If an error occurs (e.g., the file isn't a valid Excel file)
    return(list(error = paste("Error reading Excel file:", e$message)))
  })
}
