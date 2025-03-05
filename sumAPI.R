library(plumber)
library(readxl)

# Define the API

#* @get /count_rows
#* @param file_path The path to the Excel file
#* @response 200 Returns the number of rows in the Excel file
function(file_path) {
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Count the number of rows in the first sheet
  num_rows <- nrow(data)
  
  # Return the row count
  return(num_rows)
}
