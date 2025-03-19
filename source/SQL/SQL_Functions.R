library(RMySQL)
library(DBI)
library(tidyverse)
source("source/functions.R")

Connect_to_DB <- function() {
  # get passwort for hoststar DB from the environment variable 
  pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
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
con <- Connect_to_DB()
con
tables <- dbListTables(con)
print(tables)

copy_table_to_db <- function(df_data, con, table_name) {
  # Get column names and wrap them in backticks to handle spaces and special characters
  col_names <- paste0("`", colnames(df_data), "`", collapse = ", ")
  
  # Check if table exists
  table_exists <- dbExistsTable(con, table_name)
  
  if (table_exists) {
    message(sprintf("Table '%s' exists. Deleting all existing rows...", table_name))
    # Delete all rows in the table
    dbExecute(con, sprintf("DELETE FROM `%s`;", table_name))
    message(sprintf("All rows deleted from '%s'.", table_name))
  } else {
    message(sprintf("Table '%s' does not exist. Creating table...", table_name))
  }
  
  if (!table_exists) {
    # Infer SQL column types based on R data types
    sql_types <- sapply(df_data, function(x) {
      if (is.integer(x)) {
        return("INT")
      } else if (is.numeric(x)) {
        return("DOUBLE")
      } else if (inherits(x, "Date")) {
        return("DATE")
      } else if (inherits(x, "hms")) {
        return("TIME")
      } else {
        return("TEXT")  # Default to TEXT for character, factor, etc.
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
    
    values <- sapply(df_data[i, ], function(x) {
      if (is.na(x)) {
        return("NULL")  # Handle NA values properly
      } else if (is.numeric(x)) {  
        return(as.character(x))  # Keep numeric values as is (int, double)
      } else if (inherits(x, "Date")) {  
        return(sprintf("'%s'", as.character(x)))  # Format Date as 'YYYY-MM-DD'
      } else if (inherits(x, "hms")) {  
        return(sprintf("'%s'", as.character(x)))  # Handle time class (hms)
      } else if (is.factor(x)) {  
        return(sprintf("'%s'", as.character(x)))  # Convert factor to string
      } else {  
        return(sprintf("'%s'", x))  # Assume character type
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

# Load the data
c_file <- "Input/Data.Rds"
if(file.exists(c_file)){
  l_data <- readRDS(c_file)
  c_backup_number <- length(list.files(path = "Input/backup", pattern = "backup"))
  if(!dir.exists("Input/backup")) dir.create("Input/backup")
  saveRDS(l_data, paste0("Input/backup/Data_backup",c_backup_number + 1,".Rds")) # Save the updated list to the file
}else{ # or load template date 
  c_file <- "Input/template.Rds"
  l_data <- readRDS(c_file)
  c_file <- "Input/Data.Rds"
}

l_data

# create and update tables on SQL
1:length(l_data)|>
  lapply(function(ii){
    copy_table_to_db(l_data[[ii]], con, names(l_data)[ii])    
  })

l_data_sql <- names(l_data)|>
  lapply(function(x){
    tbl(con, x)|>
      collect()
  })
l_data_sql

# get data type
l_data_type <- l_data|>
  lapply(function(x){
    x|>
      apply(2,class)
  })
l_data_type|>
  str()



tbl(con, "Ausgaben")|>
  filter(Kategorie == "Personalaufwand")|>
  explain()
tbl(con, "Einnahmen")|>
  show_query()

tbl(con, "Ausgaben")
tbl(con, "Spezialpreisekiosk")
tbl(con, "Einkauf Kiosk")
tbl(con, "Programm")
tbl(con, "Einsatzplan")

# Disconnect from DB
dbDisconnect(con)

