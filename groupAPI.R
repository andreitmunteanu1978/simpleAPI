# Load necessary libraries
library(plumber)
library(readxl)
library(dplyr)
library(jsonlite)
library(base64enc)

#* @post data
#* @param File:file
#* @serializer json
function(req) {
  
  print(req$body$category)
  print(as.Date(req$body$refdate))

  # Define the reference date
  refDate <- as.Date(req$body$refdate)
  
  # Create variable to store the binary file data 
  file_binary <- req$body$File

  # Check if a file is uploaded
  if (is.null(file_binary) || length(file_binary)==0) {
    return(list(error = "No file uploaded"))
  }
  
  # Create a temporary directory to store the file
  temp_file <- tempfile(fileext = ".xlsx")
  
  # Save the file
  writeBin(base64decode(file_binary), temp_file)
  
  # Import the data frame
  df <- data.frame(read_excel(temp_file))
  
  # Delete the temporary file
  unlink(temp_file)

  # Select the grouping columns
  df <- df[,colnames(df) %in% c("Name.of.the.ship.to.party","Deliv..date.From.to.","Description","Delivery.quantity")]
  colnames(df) <- c("CUSTOMER","DATE","PRODUCT","QUANTITY")
  aggregation_columns <- c("QUANTITY")
  
  # Filter the data by latest +/- 10 days
  df <- df[format(as.Date(df$DATE),"%Y-%m-%d") %in% as.Date(refDate+seq(-5,5)),]
  
  # Group the dataset
  #response <- df %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% aggregation_columns]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
  # Group the dataset
  DS1 <- df %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns)]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
  DS2 <- df %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns,"CUSTOMER")]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")

  DataList <- list(
    list(Table="Customer/Date/Product", Data = DS1, Format = sapply(DS1, class)),
    list(Table="Date/Product", Data = DS2, Format = sapply(DS2, class))
  )
  
  # Return the results JSON
  return(fromJSON(toJSON(DataList,pretty=TRUE, auto_unbox = TRUE)))
}

#_______________________________________________________________

#* @post dataold
#* @param File:file
#* @serializer json
function(req) {
  
  # Create variable to store the binary file data 
  file_binary <- req$body$File

  # Check if a file is uploaded
  if (is.null(file_binary) || length(file_binary)==0) {
    return(list(error = "No file uploaded"))
  }
  
  # Create a temporary directory to store the file
  temp_file <- tempfile(fileext = ".xlsx")
  
  # Save the file
  writeBin(base64decode(file_binary), temp_file)
  
  # Import the data frame
  df <- data.frame(read_excel(temp_file))

  # Retrieve parameters from request body
  group_by_columns <- as.vector(unlist(req$body$group_by_columns))
  aggregation_columns <- as.vector(unlist(req$body$aggregation_columns))
  aggregation_method <- as.vector(unlist(req$body$aggregation_method))
  
  # Validate group_by_columns
  if (is.null(group_by_columns) || !all(group_by_columns %in% colnames(df))) {
    return(list(error = "Invalid group_by_columns"))
  }
  
  # Validate aggregation_columns
  if (is.null(aggregation_columns) || !all(aggregation_columns %in% colnames(df))) {
    return(list(error = "Invalid aggregation_columns"))
  }
  
  # Define aggregation function
  agg_func <- switch(aggregation_method,
                     "sum" = sum,
                     "mean" = mean,
                     "max" = max,
                     "min" = min,
                     sum)
  
  # Delete the temporary file
  unlink(temp_file)
  
  response <- df %>% group_by(across(all_of(group_by_columns))) %>% summarise(across(all_of(aggregation_columns), agg_func, na.rm = TRUE), .groups = "drop")

  # Return the results JSON
  return(response)

}

#_______________________________________________________________

#* @post schema
#* @param File:file
#* @serializer json
function(req) {

  print(file$body$category)
  
  # Create variable to store the binary file data 
  file_binary <- req$body$File
  
  # Check if a file is uploaded
  if (is.null(file_binary) || length(file_binary)==0) {
    return(list(error = "No file uploaded"))
  }
  
  # Create a temporary directory to store the file
  temp_file <- tempfile(fileext = ".xlsx")
  
  # Save the file
  writeBin(base64decode(file_binary), temp_file)
  
  # Import the data frame
  df <- data.frame(read_excel(temp_file))

  # Delete the temporary file
  unlink(temp_file)
  
  response <- data.frame(matrix(data=cbind(names(df[1,]),t(df[1,])),ncol = 2, nrow = nrow(t(df[1,])), dimnames = list(NULL,c("Key","Value"))))
  
  # Return the results JSON
  return(response)
}
