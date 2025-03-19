library(RMySQL)
library(DBI)
library(tidyverse)
source("source/functions.R")

# connection to Database
Connect_to_DB <- function(pw) {
  # host
  host <- "lx51.hoststar.hosting"
  #Database name 
  DB_name <- "ch367079_gui"
  # DB_user
  DB_user <- "ch367079_flo"
  
  if(!r_is.defined(con)) {
    # Example connection
    con <- dbConnect(
      MySQL(),  # or MariaDB()
      host = host,
      user = DB_user,
      password = pw,
      dbname = DB_name,
      port = 3306
    )
    return(con)
  }else return(con)
}

# Copy a data frame to SQL DB (slow done for each row because of DB batch restrictions)
copy_table_to_db <- function(df_data, con, table_name, delete_existing = TRUE) {
  # Load necessary libraries
  library(DBI)
  library(hms)
  
  # Get column names and wrap them in backticks to handle spaces and special characters
  col_names <- paste0("`", colnames(df_data), "`", collapse = ", ")
  
  # Check if table exists
  table_exists <- dbExistsTable(con, table_name)
  
  if (table_exists && delete_existing) {
    message(sprintf("Table '%s' exists. Deleting all existing rows...", table_name))
    # Delete all rows in the table
    dbExecute(con, sprintf("DELETE FROM `%s`;", table_name))
    message(sprintf("All rows deleted from '%s'.", table_name))
  } else if (!table_exists) {
    message(sprintf("Table '%s' does not exist. Creating table...", table_name))
  }
  
  if (!table_exists) {
    # Infer SQL column types based on R data types (MySQL-specific)
    sql_types <- sapply(df_data, function(x) {
      if (is.integer(x)) {
        return("INT")
      } else if (is.numeric(x)) {
        return("DOUBLE")
      } else if (inherits(x, "Date")) {
        return("DATE")
      } else if (inherits(x, "hms")) {
        return("TIME")  # Use TIME for time of day
      } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
        return("DATETIME")  # Use DATETIME for date-time values
      } else if (is.character(x) || is.factor(x)) {
        return("TEXT")  # Use TEXT for character or factor columns
      } else {
        stop(sprintf("Unsupported data type for column: %s", class(x)))
      }
    })
    
    # Construct CREATE TABLE query with backticks around column names
    create_query <- sprintf(
      "CREATE TABLE `%s` (%s);",
      table_name,
      paste(paste0("`", colnames(df_data), "` ", sql_types), collapse = ", ")
    )
    
    # Execute table creation
    dbExecute(con, create_query)
    message(sprintf("Table '%s' created successfully.", table_name))
  }
  
  # Loop through each row and insert data
  for (i in 1:nrow(df_data)) {
    # Skip empty rows
    if (all(is.na(df_data[i, ]))) {
      next
    }
    
    # Format values for MySQL
    values <- sapply(df_data[i, ], function(x) {
      if (is.na(x)) {
        return("NULL")  # Handle NA values properly
      } else if (is.numeric(x)) {
        return(as.character(x))  # Keep numeric values as is
      } else if (inherits(x, "Date")) {
        return(sprintf("'%s'", as.character(x)))  # Format Date as 'YYYY-MM-DD'
      } else if (inherits(x, "hms")) {
        return(sprintf("'%s'", as.character(x)))  # Format time as 'HH:MM:SS'
      } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
        return(sprintf("'%s'", format(x, "%Y-%m-%d %H:%M:%S")))  # Format datetime as 'YYYY-MM-DD HH:MM:SS'
      } else if (is.character(x) || is.factor(x)) {
        return(sprintf("'%s'", gsub("'", "''", as.character(x))))  # Escape single quotes in strings
      } else {
        stop(sprintf("Unsupported data type for value: %s", class(x)))
      }
    })
    
    # Create SQL query with backticks around column names
    query <- sprintf(
      "INSERT INTO `%s` (%s) VALUES (%s);",
      table_name,
      col_names,
      paste(values, collapse = ", ")
    )
    
    # Ensure NULL values are correctly formatted (without quotes)
    query <- gsub("'NULL'", "NULL", query)
    
    # Execute query
    dbExecute(con, query)
  }
  
  message(sprintf("Data inserted into '%s' successfully!", table_name))
}

# Conversion template
convert_to_template_types <- function(df_sql, df_template) {
  # Align columns (keep only those present in both data frames)
  common_cols <- intersect(colnames(df_sql), colnames(df_template))
  df_sql <- df_sql |> select(all_of(common_cols))
  df_template <- df_template |> select(all_of(common_cols))
  
  # Convert data types
  for (col in common_cols) {
    col_type <- class(df_template[[col]])

    if (any(col_type == "Date")) {
      df_sql[[col]] <- as.Date(df_sql[[col]])
    } else if (any(col_type == "hms")) {
      df_sql[[col]] <- hms::as_hms(df_sql[[col]])
    } else if (any(col_type %in% c("POSIXct", "POSIXlt"))) {
      df_sql[[col]] <- as.POSIXct(df_sql[[col]])
    } else if (any(col_type == "double")) {
      df_sql[[col]] <- as.numeric(df_sql[[col]])
    } else if (any(col_type == "integer")) {
      df_sql[[col]] <- as.integer(df_sql[[col]])
    } else if (any(col_type == "numeric")) {
      df_sql[[col]] <- as.numeric(df_sql[[col]])
    } else if (any(col_type == "character")) {
      df_sql[[col]] <- as.character(df_sql[[col]])
    } else if (any(col_type == "factor")) {
      df_sql[[col]] <- as.factor(df_sql[[col]])
    } else {
      warning(sprintf("Unsupported data type for column '%s': %s", col, paste(col_type, collapse = ", ")))
    }
  }
  
  return(df_sql)
}

# convert data from DB to R with correct conversion template
convert_DB_to_R <- function(data,template) {
  # Convert data types for each table
  data_converted <- names(data) |>
    map(~ {
      table_name <- .x
      df_sql <- data[[table_name]]
      df_template <- template[[table_name]]
      
      # Convert data types
      convert_to_template_types(df_sql, df_template)
    })
  # Assign names to the converted list
  names(data_converted) <- names(data)
  return(data_converted)
}

# update all data in DB
update_DB_all <- function(l_data, con) {
  shiny::withProgress(message = "Running script...", value = 0, {
    for (ii in 1:length(l_data)) {
      shiny::incProgress(length(l_data) / ii, detail = paste("Step", ii, "of", length(l_data)))
      copy_table_to_db(l_data[[ii]], con, names(l_data)[ii])    
    }
  })
}

# get all data defined by the template l_data
get_Data <- function(l_data, con, download = TRUE) {
  if(download){
    temp <- names(l_data)|>
      lapply(function(x){
        tbl(con, x)|>
          collect()
      })
  } else {
    temp <- names(l_data)|>
      lapply(function(x){
        tbl(con, x)
      })
  }
  names(temp) <- names(l_data)
  return(temp)
}

