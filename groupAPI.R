# Load necessary libraries
library(plumber)
library(readxl)
library(dplyr)

#* @post /upload_file
#* @param file:file
#* @serializer json
function(req) {
  
  # Create variable to store the binary file data 
  file_binary <- req$body$file$value

  # Check if a file is uploaded
  if (is.null(file_binary) || length(file_binary)==0) {
    return(list(error = "No file uploaded"))
  }
  
  # Create a temporary directory to store the file
  temp_file <- tempfile(fileext = ".xlsx")
  
  # Save the file
  writeBin(file_binary, temp_file)
  
  # Import the data frame
  df <- read_excel(temp_file)
  
  # Delete the temporary file
  unlink(temp_file)
  
  # Return the results JSON
  return(msg=paste("The imported data frame contains:  ", nrow(df)," rows.", sep = ""))

}
