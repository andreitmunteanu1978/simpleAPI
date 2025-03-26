# Load necessary libraries
library(plumber)
library(readxl)
library(dplyr)
library(jsonlite)
library(base64enc)

# Create all applicable functions for the API POST action
  
  # Declare the function for processing the Livrari Depozite.xlsx file
  f_shipments_depots <- function(bin_file, ref_date, ref_range) {
    
    # Create a temporary directory to store the file
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Save the file
    writeBin(base64decode(bin_file), temp_file)

    # Declare the relevant Excel sheets
    sheets <- excel_sheets(temp_file)
    sheets <- sheets[grepl("dws",tolower(sheets)) & grepl("-",tolower(sheets)) | grepl("omv",tolower(sheets)) | grepl("socar",tolower(sheets))]

    # Define a list to store the data frames
    df_list <- vector(mode = "list", length = 3)
    names(df_list) <- c("DWS","OMV","SOCAR")
    
    # Import the Excel data into a data.frame
    for (d in names(df_list))
    {
      df_temp <- cbind("ENTITY"=d, data.frame(read_excel(temp_file, sheet = sheets[grepl(tolower(d), tolower(sheets))])))
      df_temp <- df_temp[!is.na(df_temp$Data.expedierii),colnames(df_temp) %in% c("ENTITY","Data.expedierii","Produs","Cantitate.expediata.tone")]
      
      colnames(df_temp) <- c("ENTITY","DATE","PRODUCT","QUANTITY")
      
      df_temp <- df_temp[tolower(df_temp$PRODUCT)!="produs",]
      if(as.numeric(df_temp$DATE[1])>60000)
      {
        df_temp$DATE <- as.Date(df_temp$DATE)
      } else
      {
        df_temp$DATE <- as.Date(as.numeric(df_temp$DATE), origin="1899-12-30")
      }
      
      df_temp$QUANTITY <- as.numeric(df_temp$QUANTITY)
      df_list[[d]] <- df_temp
    }
    
    # Delete the temporary file
    unlink(temp_file)

    # Create the composite data.frame
    df <- do.call(rbind, df_list)
    aggregation_columns <- c("QUANTITY")
    
    # Filter the data by latest +/- 10 days
    df <- df[format(as.Date(df$DATE),"%Y-%m-%d") %in% as.Date(ref_date+seq(-ref_range,ref_range)),]
    
    # Create 3 response data.frames
    df_DWS <- df[df$ENTITY=="DWS",] %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns,"ENTITY")]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
    df_OMV <- df[df$ENTITY=="OMV",] %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns, "ENTITY")]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
    df_SOCAR <- df[df$ENTITY=="SOCAR",] %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns, "ENTITY")]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
    
    # Define the array of group_by_columns
    group_by_colums <- colnames(df)[!colnames(df) %in% c(aggregation_columns,"ENTITY")]
    
    # Store the data.frames into a list
    DataList <- list(Response = "Success",
      Info = list(
        list(Table = "DWS", Data = df_DWS, Groups = group_by_colums),
        list(Table = "OMV", Data = df_OMV, Groups = group_by_colums),
        list(Table = "SOCAR", Data = df_SOCAR, Groups = group_by_colums)
        )
    )
    
    # Return the results JSON
    return(fromJSON(toJSON(DataList, pretty=TRUE, auto_unbox = TRUE)))
  }
    
  
  # Declare the function for processing the Avize Transfer VEGA.xlsx file
  f_shipments_vega <- function(bin_file, ref_date, ref_range, category_file) {
    
    # Create a temporary directory to store the file
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Save the file
    writeBin(base64decode(bin_file), temp_file)
    
    # Import the Excel data into a data.frame
    df <- data.frame(read_excel(temp_file))
    
    # Delete the temporary file
    unlink(temp_file)

    # Store the data.frames into a list
    DataList <- list(Response = "Success",
      Info = list()
    )

    # Return the results JSON
    return(fromJSON(toJSON(DataList, pretty=TRUE, auto_unbox = TRUE)))
  }
  
  # Declare function for processing the IPPA.xlsx file
  f_ippa <- function(bin_file, ref_date, ref_range) {
    
    # Create a temporary directory to store the file
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Save the file
    writeBin(base64decode(bin_file), temp_file)
    
    # Import the Excel data into a data.frame
    df <- data.frame(read_excel(temp_file))
    
    # Delete the temporary file
    unlink(temp_file)
    
    # Select the grouping columns
    df <- df[,colnames(df) %in% c("Name.of.the.ship.to.party","Deliv..date.From.to.","Description","Delivery.quantity")]
    colnames(df) <- c("CUSTOMER","DATE","PRODUCT","QUANTITY")
    aggregation_columns <- c("QUANTITY")
    
    # Format the 'DATE' column using the as.Date formula
    df$DATE <- as.Date(df$DATE)
    
    # Filter the data by latest +/- 10 days
    df <- df[format(as.Date(df$DATE),"%Y-%m-%d") %in% as.Date(ref_date+seq(-ref_range,ref_range)),]
    
    # Create 2 response data.frames
    df_1 <- df %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns)]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
    df_2 <- df %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns, "CUSTOMER")]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
    
    # Define the array of group_by_columns
    group_by_colums <- colnames(df)[!colnames(df) %in% aggregation_columns]
    
    # Store the data.frames into a list
    DataList <- list(Response = "Success",
      Info = list(
      list(Table="Customer/Date/Product", Data = df_1, Groups = group_by_colums),
      list(Table="Date/Product", Data = df_2, Groups = group_by_colums)
        )
    )
    
    # Return the results JSON
    return(fromJSON(toJSON(DataList, pretty=TRUE, auto_unbox = TRUE)))
  }
  
  # Declare the function for processing the JET.xlsx file
  f_jet <- function(bin_file, ref_date, ref_range) {
    
    # Create a temporary directory to store the file
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Save the file
    writeBin(base64decode(bin_file), temp_file)
    
    # Import the Excel data into a data.frame
    df <- data.frame(read_excel(temp_file))
    
    # Delete the temporary file
    unlink(temp_file)
    
    # Select the grouping columns
    df <- df[,colnames(df) %in% c("Name.of.the.ship.to.party","Deliv..date.From.to.","Description","Delivery.quantity")]
    colnames(df) <- c("CUSTOMER","DATE","PRODUCT","QUANTITY")
    aggregation_columns <- c("QUANTITY")
    
    # Format the 'DATE' column using the as.Date formula
    df$DATE <- as.Date(df$DATE)
    
    # Filter the data by latest +/- 10 days
    df <- df[format(as.Date(df$DATE),"%Y-%m-%d") %in% as.Date(ref_date+seq(-ref_range,ref_range)),]
    
    # Create 2 response data.frames
    df_1 <- df %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns)]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
    df_2 <- df %>% group_by(across(all_of(colnames(df)[!colnames(df) %in% c(aggregation_columns, "CUSTOMER")]))) %>% summarise(across(all_of(aggregation_columns), sum, na.rm = TRUE), .groups = "drop")
    
    # Define the array of group_by_columns
    group_by_colums <- colnames(df)[!colnames(df) %in% aggregation_columns]
    
    # Store the data.frames into a list
    DataList <- list(Response = "Success",
      Info = list(
        list(Table="Customer/Date/Product", Data = df_1, Groups = group_by_colums),
        list(Table="Date/Product", Data = df_2, Groups = group_by_colums)
        )
    )
    
    # Return the results JSON
    return(fromJSON(toJSON(DataList, pretty=TRUE, auto_unbox = TRUE)))
  }
  
  # Declare the function for processing the Program vag si vapoare.xlsx file
  f_trains_barges <- function(bin_file, ref_date, ref_range, category_file) {
    
    # Create a temporary directory to store the file
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Save the file
    writeBin(base64decode(bin_file), temp_file)
    
    # Import the Excel data into a data.frame
    df <- data.frame(read_excel(temp_file))
    
    # Delete the temporary file
    unlink(temp_file)

    # Store the data.frames into a list
    DataList <- list(Response = "Success",
      Info = list()
    )

    # Return the results JSON
    return(fromJSON(toJSON(DataList, pretty=TRUE, auto_unbox = TRUE)))
  }
  
  # Declare the function for processing the LIVRARI.xlsx file
  f_shipments_others <- function(bin_file, ref_date, ref_range, category_file) {
    
    # Create a temporary directory to store the file
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Save the file
    writeBin(base64decode(bin_file), temp_file)
    
    # Import the Excel data into a data.frame
    df <- data.frame(read_excel(temp_file))
    
    # Delete the temporary file
    unlink(temp_file)

    DataList <- list(Response = "Success",
      Info = list()
    )

    # Return the results JSON
    return(fromJSON(toJSON(DataList, pretty=TRUE, auto_unbox = TRUE)))
  }
  
  # Declare the function for processing the Stoc.xlsx file
  f_stocks <- function(bin_file, ref_date, ref_range, category_file) {
    
    # Create a temporary directory to store the file
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Save the file
    writeBin(base64decode(bin_file), temp_file)
    
    # Import the Excel data into a data.frame
    df <- data.frame(read_excel(temp_file))
    
    # Delete the temporary file
    unlink(temp_file)

    # Store the data.frames into a list
    DataList <- list(Response = "Success",
      Info = list()
    )

    # Return the results JSON
    return(fromJSON(toJSON(DataList, pretty=TRUE, auto_unbox = TRUE)))
  }

#_______________________________________________________________
  
#* @post data
#* @param File:file
#* @serializer json
function(req) {
  
  # Import all POST body components and verify their accuracy/existance
  
  #1:  Define the file category
  file_category <- req$body$category
  
  # Check if a file category exists
  if (is.null(file_category) || length(file_category)==0) {
    return(list(error = "You have not selected a file processing method."))
  }
  
  #2. Define the reference date
  refDate <- as.Date(req$body$refDate)
  
  if (is.null(refDate) || length(refDate)==0) {
    return(list(error = "You have not choosen a reference date."))
  }
  
  #3. Define the reference range
  refRange <- as.numeric(req$body$refRange)
  
  if (is.null(refRange) || length(refRange)==0) {
    return(list(error = "You have not choosen a valid reference range for your reference date."))
  }
  
  # Create variable to store the binary file data 
  file_binary <- req$body$File
  
  # Check if a file is uploaded
  if (is.null(file_binary) || length(file_binary)==0) {
    return(list(error = "No file uploaded"))
  }
  
  switch(file_category,
         "Shipments_Depots" = f_shipments_depots(file_binary, refDate, refRange),
         "Shipments_Vega" = f_shipments_vega(file_binary, refDate, refRange, file_category),
         "IPPA" = f_ippa(file_binary, refDate, refRange),
         "Jet" = f_jet(file_binary, refDate, refRange),
         "Trains_Barges" = f_trains_barges(file_binary, refDate, refRange, file_category),
         "Shipments_Others" = f_shipments_others(file_binary, refDate, refRange, file_category),
         "Stocks" = f_stocks(file_binary, refDate, refRange, file_category),
  )
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

