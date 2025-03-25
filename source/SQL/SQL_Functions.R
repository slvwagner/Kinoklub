library(RMySQL)
library(DBI)
library(tidyverse)
source("source/functions.R")

# connection to Database
DB_connect <- function(pw, DB_user = "ch367079_flo", con = NULL) {
  # Database credentials
  host <- "lx51.hoststar.hosting"
  DB_name <- "ch367079_gui"
  
  # Check if connection already exists and is valid
  if (!is.null(con)) {
    return(con)
  } else {
    # Create a new connection
    con <- tryCatch({
      dbConnect(
        MySQL(),
        host = host,
        user = DB_user,
        password = pw,
        dbname = DB_name,
        port = 3306
      )
    }, error = function(e) {
      stop("Failed to connect to the database: ", e$message)
    })
    return(con)
  }
}

# Update all tables in DB with ID as Primary Key
DB_update_all <- function(l_data, con) {
  if (!is.list(l_data) || is.null(names(l_data))) {
    stop("l_data must be a named list where names correspond to table names.")
  }
  
  lapply(names(l_data), function(table_name) {
    DB_copy_table(l_data[[table_name]], con, table_name)
  })
}

# get all data defined by the template l_data
DB_get_Data <- function(l_template, con, download = TRUE) {
  if(download){
    temp <- names(l_template)|>
      lapply(function(x){
        tbl(con, x)|>
          collect()
      })
  } else {
    temp <- names(l_template)|>
      lapply(function(x){
        tbl(con, x)
      })
  }
  names(temp) <- names(l_template)
  return(temp)
}

DB_get_table <- function(table_name, con, download = TRUE){
  if(download){
    tbl(con, table_name)|>
      collect()
  } else {
    tbl(con, table_name)
  }
}

# Copy a data frame to SQL DB (slow done for each row because of DB batch restrictions)
DB_copy_table <- function(df_data, con, table_name, delete_existing = TRUE) {
  # Load necessary libraries
  library(DBI)
  library(hms)
  
  # Validate database connection
  if (!dbIsValid(con)) {
    stop("Invalid database connection.")
  }
  
  # Get column names and wrap them in backticks to handle spaces and special characters
  col_names <- paste0("`", colnames(df_data), "`", collapse = ", ")
  
  # Check if table exists
  table_exists <- dbExistsTable(con, table_name)
  
  # If table exists and delete_existing is TRUE, delete all rows
  if (table_exists && delete_existing) {
    message(sprintf("Table '%s' exists. Deleting all existing rows...", table_name))
    dbExecute(con, sprintf("DELETE FROM `%s`;", table_name))
    message(sprintf("All rows deleted from '%s'.", table_name))
  }
  
  # If table does not exist, create it
  if (!table_exists) {
    message(sprintf("Table '%s' does not exist. Creating table...", table_name))
    
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

# Function to add a row to any table
DB_add_row <- function(con, table_name, new_row) {
  # Validate inputs
  if (!dbIsValid(con)) {
    stop("Invalid database connection.")
  }
  if (!dbExistsTable(con, table_name)) {
    stop("Table '", table_name, "' does not exist in the database.")
  }
  
  # Ensure new_row is a named list or data frame
  if (!is.list(new_row) || is.null(names(new_row))) {
    stop("new_row must be a named list or data frame.")
  }
  
  # Get the table's column names and types
  # library(rebus)
  # p <- SPC
  # as.character(p)
  p <- "\\s"
  
  # handle column names correctly
  if(str_detect(table_name, p)) {
    table_name <- paste0("`",table_name,"`")
  }
  
  table_info <- dbGetQuery(con, paste0("DESCRIBE ", table_name))
  
  col_names <- table_info$Field
  col_types <- table_info$Type
  
  # # Debug: Print column names and new_row names
  # message("Column names in table: ", paste(col_names, collapse = ", "))
  # message("Column names in new_row: ", paste(names(new_row), collapse = ", "))
  
  # Validate the new row data
  if (!all(names(new_row) %in% col_names)) {
    stop("New row contains invalid column names.")
  }
  
  # Ensure all column names are non-empty
  if (any(names(new_row) == "")) {
    stop("One or more column names in new_row are empty.")
  }
  
  # Ensure the new row has all required columns (non-NULL columns without defaults)
  required_cols <- table_info |>
    filter(Null == "NO" & is.na(Default)) |>
    pull(Field)
  missing_cols <- setdiff(required_cols, names(new_row))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Replace NA values with NULL for SQL
  new_row <- lapply(new_row, function(x) if (is.na(x)) NULL else x)
  
  # Debug: Print new_row values
  # message("Values in new_row: ", paste(new_row, collapse = ", "))
  
  # Prepare the SQL query
  sql_cols <- paste(paste0("`", names(new_row), "`"), collapse = ", ")
  sql_vals <- paste(sapply(new_row, function(x) {
    if (is.null(x)) "NULL"
    else if (is.character(x)) paste0("'", x, "'")
    else if (is.Date(x)) paste0("'", x, "'")
    else if (is.factor(x)) paste0("'", as.character(x), "'")
    else x
  }), collapse = ", ")
  sql_query <- paste0(
    "INSERT INTO ", table_name, " (", sql_cols, ") VALUES (", sql_vals, ")"
  )
  
  # # Debug: Print SQL query
  # message("Executing SQL query: ", sql_query)
  
  # Execute the query
  dbExecute(con, sql_query)
  
  message("Row ",new_row$ID ," added successfully to table '", table_name, "'.")
}

# Function to edit a row in table
DB_edit_row_in_table <- function(con, table_name, primary_key_col, primary_key_value, updated_values) {
  # Validate inputs
  if (!dbIsValid(con)) {
    stop("Invalid database connection.")
  }
  if (!dbExistsTable(con, table_name)) {
    stop("Table '", table_name, "' does not exist in the database.")
  }
  
  # Get the table's column names and types
  table_info <- dbGetQuery(con, paste("DESCRIBE", table_name))
  col_names <- table_info$Field
  col_types <- table_info$Type
  
  # Validate the primary key column
  if (!primary_key_col %in% col_names) {
    stop("Primary key column '", primary_key_col, "' does not exist in the table.")
  }
  
  # Validate the updated values
  if (!all(names(updated_values) %in% col_names)) {
    stop("Updated values contain invalid column names.")
  }
  
  # Replace NA values with NULL for SQL
  updated_values <- lapply(updated_values, function(x) if (is.na(x)) NULL else x)
  
  # Prepare the SET clause for the SQL query
  set_clause <- paste(
    sapply(names(updated_values), function(col) {
      value <- updated_values[[col]]
      if (is.null(value)) {
        paste0("`", col, "` = NULL")
      } else if (is.character(value)) {
        paste0("`", col, "` = '", value, "'")
      } else if (inherits(value, "POSIXt") || inherits(value, "Date")) {
        paste0("`", col, "` = '", format(value, "%Y-%m-%d"), "'")
      } else if (inherits(value, "difftime")) {
        paste0("`", col, "` = '", format(as.POSIXct(value, origin = "1970-01-01"), "%H:%M:%S"), "'")
      } else {
        paste0("`", col, "` = ", value)
      }
    }),
    collapse = ", "
  )
  
  # Prepare the WHERE clause for the SQL query
  where_clause <- paste0("`", primary_key_col, "` = ", if (is.character(primary_key_value)) paste0("'", primary_key_value, "'") else primary_key_value)
  
  # Construct the SQL query
  sql_query <- paste0(
    "UPDATE ", table_name, " SET ", set_clause, " WHERE ", where_clause
  )
  
  # Print the SQL query for debugging
  # message("Executing SQL query:\n", sql_query)
  
  # Execute the query
  dbExecute(con, sql_query)
  
  message("Row with ", primary_key_col, " = ", primary_key_value, " updated successfully in table '", table_name, "'.")
}

# Function to delete a row from any table
DB_delete_row <- function(con, table_name, primary_key_col, primary_key_value) {
  # Validate inputs
  if (!dbIsValid(con)) {
    stop("Invalid database connection.")
  }
  if (!dbExistsTable(con, table_name)) {
    stop("Table '", table_name, "' does not exist in the database.")
  }
  
  # Get the table's column names
  table_info <- dbGetQuery(con, paste("DESCRIBE", table_name))
  col_names <- table_info$Field
  
  # Validate the primary key column
  if (!primary_key_col %in% col_names) {
    stop(paste("Primary key column '", primary_key_col, "' does not exist in the table."))
  }

  # Prepare the WHERE clause for the SQL query
  where_clause <- paste0("`", primary_key_col, "` = ", if (is.character(primary_key_value)) paste0("'", primary_key_value, "'") else primary_key_value)
  
  # Construct the SQL query
  sql_query <- paste0(
    "DELETE FROM `", table_name, "` WHERE ", where_clause
  )
  
  # Print the SQL query for debugging
  # message("Executing SQL query: ", sql_query)
  
  # Execute the query
  test <- dbExecute(con, sql_query)
  
  if(test)  message("Row with ", primary_key_col, " = ", primary_key_value, " deleted successfully from table '", table_name, "'.")
  else stop("Row with ", primary_key_col, " = ", primary_key_value, " have not been deleted from table '", table_name, "'.")
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
