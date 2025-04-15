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

# Copy a data frame to SQL DB (slow because it is done for each row => DB batch restrictions) ####
DB_copy_table <- function(df_data, con, table_name, delete_existing = TRUE) {
  library(DBI)
  library(hms)
  
  if (!dbIsValid(con)) {
    stop("Invalid database connection.")
  }
  
  # Get first column name (intended as primary key)
  primary_key_col <- names(df_data)[1]
  
  # Ensure primary key column has unique values
  if (anyDuplicated(df_data[[primary_key_col]]) > 0) {
    stop(sprintf("Primary key column '%s' contains duplicate values.", primary_key_col))
  }
  
  # All column names for insert
  col_names <- paste0("`", colnames(df_data), "`", collapse = ", ")
  
  table_exists <- dbExistsTable(con, table_name)
  
  if (table_exists && delete_existing) {
    message(sprintf("Table '%s' exists. Dropping it...", table_name))
    dbExecute(con, sprintf("DROP TABLE `%s`;", table_name))
    
    sql_types <- sapply(df_data, function(x) {
      if (is.integer(x)) {
        return("INT")
      } else if (is.numeric(x)) {
        return("DOUBLE")
      } else if (is.logical(x)) {
        return("BOOLEAN")
      } else if (inherits(x, "Date")) {
        return("DATE")
      } else if (inherits(x, "hms")) {
        return("TIME")
      } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
        return("DATETIME")
      } else if (is.character(x) || is.factor(x)) {
        return("TEXT")
      } else {
        stop(sprintf("Unsupported data type for column: %s", class(x)))
      }
    })
    
    # Construct CREATE TABLE query with primary key on first column
    column_defs <- paste0("`", names(sql_types), "` ", sql_types)
    column_defs[1] <- paste(column_defs[1], "PRIMARY KEY")
    
    create_query <- sprintf(
      "CREATE TABLE `%s` (%s);",
      table_name,
      paste(column_defs, collapse = ", ")
    )
    
    dbExecute(con, create_query)
    message(sprintf("Table '%s' created successfully.", table_name))
  }
  
  for (i in 1:nrow(df_data)) {
    if (all(is.na(df_data[i, ]))) next
    
    values <- sapply(df_data[i, ], function(x) {
      if (is.na(x)) {
        return("NULL")
      } else if (is.logical(x)) {
        return(as.character(as.integer(x)))
      } else if (is.numeric(x)) {
        return(as.character(x))
      } else if (inherits(x, "Date")) {
        return(sprintf("'%s'", as.character(x)))
      } else if (inherits(x, "hms")) {
        return(sprintf("'%s'", as.character(x)))
      } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
        return(sprintf("'%s'", format(x, "%Y-%m-%d %H:%M:%S")))
      } else if (is.character(x) || is.factor(x)) {
        return(sprintf("'%s'", gsub("'", "''", as.character(x))))
      } else {
        stop(sprintf("Unsupported data type for value: %s", class(x)))
      }
    })
    
    query <- sprintf(
      "INSERT INTO `%s` (%s) VALUES (%s);",
      table_name,
      col_names,
      paste(values, collapse = ", ")
    )
    
    query <- gsub("'NULL'", "NULL", query)
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
  table_info <- DB_describe_table(con, table_name)
  
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
  
  library(hms)
  
  is_time <- function(x) {
    # Apply tryCatch on each element to handle errors
    check_each <- sapply(x, function(val) {
      tryCatch({
        !is.na(as_hms(val))  # Returns TRUE if valid, FALSE if NA
      }, error = function(e) FALSE)  # Catch errors and return FALSE
    })
    
    check_each  # Ensure all elements are valid times
  }
  
  # Prepare the SQL query
  sql_cols <- paste(paste0("`", names(new_row), "`"), collapse = ", ")
  sql_vals <- 
    paste(
      sapply(new_row, function(x) {
        if (is.null(x)) "NULL"
        else if (is.character(x)) paste0("'", x, "'")
        else if (is.Date(x)) paste0("'", as.character(x), "'")
        else if (is_time(x)) paste0("'", as.character(x), "'")
        else if (is.factor(x)) paste0("'", as.character(x), "'")
        else x
        }), 
      collapse = ", ")
  sql_query <- paste0(
    "INSERT INTO ", "`",table_name, "`"," (", sql_cols, ") VALUES (", sql_vals, ")"
  )
  
  # # Debug: Print SQL query
  # message("Executing SQL query: ", sql_query)
  
  # Execute the query
  dbExecute(con, sql_query)
  
  message("Row ",new_row[[1]][1] ," added successfully to table '","`", table_name,"`", "'.")
}

# 
DB_describe_table <- function(con, table_name){
  dbGetQuery(con, paste0("DESCRIBE ","`", table_name ,"`"))
}

# Function to edit a row in table
DB_edit_row_in_table <- function(con, table_name, primary_key_col, primary_key_value, updated_values, c_class) {
  # Validate inputs
  if (!DBI::dbIsValid(con)) {
    stop("Invalid database connection.")
  }
  
  if (!DBI::dbExistsTable(con, table_name)) {
    stop("Table '", table_name, "' does not exist in the database.")
  }
  
  # Get the table's column names and types
  table_info <- DB_describe_table(con, table_name)
  col_names <- table_info$Field
  
  # Validate the primary key column
  if (!primary_key_col %in% col_names) {
    stop("Primary key column '", primary_key_col, "' does not exist in the table.")
  }
  
  # Validate the updated values
  if (!all(names(updated_values) %in% col_names)) {
    stop("Updated values contain invalid column names.")
  }
  
  # Replace NA values with NULL explicitly for factors 
  updated_values <- as.list(updated_values)
  for (ii in 1:length(c_class)) {
    if(c_class[ii] == "factor"){
      if(!is.na(updated_values[[ii]])){
        if(updated_values[[ii]] == "NA"){
          updated_values[[ii]] <- NA
        }
      }
    }
  }
  
  # Replace NA values with NULL explicitly
  updated_values <- lapply(updated_values, function(x) {
    if (is.atomic(x) && length(x) == 1 && is.na(x)) {
      NULL
    } else {
      x
    }
  })

  # Prepare the SET clause for the SQL query
  set_clause <- paste(
    vapply(names(updated_values), function(col) {
      value <- updated_values[[col]]
      
      if (is.null(value)) {
        paste0("`", col, "` = NULL")
      } else if (is.character(value)) {
        paste0("`", col, "` = '", gsub("'", "''", value), "'")
      } else if (inherits(value, "POSIXt") || inherits(value, "Date")) {
        paste0("`", col, "` = '", format(value, "%Y-%m-%d %H:%M:%S"), "'")
      } else if (inherits(value, "difftime")) {
        paste0("`", col, "` = '", format(as.POSIXct(value, origin = "1970-01-01"), "%H:%M:%S"), "'")
      } else {
        paste0("`", col, "` = ", value)
      }
    }, character(1)),
    collapse = ", "
  )
  
  # Prepare the WHERE clause
  where_clause <- paste0(
    "`", primary_key_col, "` = ",
    if (is.character(primary_key_value)) paste0("'", gsub("'", "''", primary_key_value), "'") else primary_key_value
  )
  
  # Construct and execute the SQL query
  sql_query <- paste0(
    "UPDATE `", table_name, "` SET ", set_clause, " WHERE ", where_clause
  )
  
  DBI::dbExecute(con, sql_query)
  
  message("Row with ", primary_key_col, " = ", primary_key_value,
          " updated successfully in table '", table_name, "'.")
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
  table_info <- DB_describe_table(con, table_name)
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


DB_update_cell <- function(con, table_name, primary_key_col, primary_key_value, target_col, new_value) {
  # Validate inputs
  if (!dbIsValid(con)) {
    stop("Invalid database connection.")
  }
  if (!dbExistsTable(con, table_name)) {
    stop("Table '", table_name, "' does not exist in the database.")
  }
  
  # Get the table's column names
  table_info <- DB_describe_table(con, table_name)
  col_names <- table_info$Field
  
  # Validate column names
  if (!primary_key_col %in% col_names) {
    stop(paste("Primary key column '", primary_key_col, "' does not exist in the table."))
  }
  if (!target_col %in% col_names) {
    stop(paste("Target column '", target_col, "' does not exist in the table."))
  }
  
  # Format the new value for SQL
  formatted_value <- if (is.null(new_value) || is.na(new_value)) {
    "NULL"
  } else if (is.character(new_value)) {
    paste0("'", gsub("'", "''", new_value), "'")  # Escape single quotes in strings
  } else if (inherits(new_value, "POSIXt") || inherits(new_value, "Date")) {
    paste0("'", format(new_value, "%Y-%m-%d"), "'")
  } else {
    new_value
  }
  
  # Prepare the WHERE clause for the SQL query
  where_clause <- paste0("`", primary_key_col, "` = ", 
                         if (is.character(primary_key_value)) paste0("'", primary_key_value, "'") else primary_key_value)
  
  # Construct the SQL query
  sql_query <- paste0(
    "UPDATE `", table_name, "` SET `", target_col, "` = ", formatted_value, 
    " WHERE ", where_clause
  )
  
  # Execute the query
  dbExecute(con, sql_query)
  
  message("Cell in table '", table_name, "' updated successfully: ", target_col, " = ", new_value, 
          " (Row where ", primary_key_col, " = ", primary_key_value, ").")
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
