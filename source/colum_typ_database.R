########################################################################
# Excel column type definition data base 
########################################################################
# This script need the some function form the slvwagner library
devtools::install_git("https://github.com/wagnius-GmbH/slvwagner/",   build_manual = TRUE,
                      build_vignettes = TRUE)

rm(list = ls())

########################################################################
# find all columnnames in all excel spread sheets
########################################################################
find_col_names_excel <- function(c_file) {
  # Error handling
  stopifnot(file.exists(c_file))
  
  # Get sheet names
  c_sheets <- readxl::excel_sheets(c_file)
  
  # Read data from each sheet with specified column types
  Einnahmen_und_Ausgaben <- 
    lapply(c_sheets, function(sheet_name) {
      readxl::read_excel(
        c_file,
        sheet = sheet_name,
        col_types = NULL # Apply column types
      )
    })
  names(Einnahmen_und_Ausgaben) <- c_sheets
  Einnahmen_und_Ausgaben|>
    lapply(names)
}

########################################################################
# Read excel with defined column typ data base
########################################################################
get_excel_data <- function(c_file) {
  if(!r_is.defined(col_env)) stop("col_env could not be found")
  sheet_name <- readxl::excel_sheets(c_file)
  c_col_names <- col_env$find_col_names_excel(c_file)
  
  c_col_type <-
    c_col_names |>
    lapply(function(x) {
      col_env$dict_get_values(x, col_env)
    })
  
  l_data <- list()
  for (ii in 1:length(sheet_name)) {
    l_data[[ii]] <- readxl::read_excel(
      c_file,
      sheet = sheet_name[ii],
      col_types = c_col_type[[sheet_name[ii]]] # Apply column types
    )|>
      suppressWarnings()
  }
  names(l_data) <- sheet_name
  return(l_data)
}

########################################################################
# Find all excel files to read in
c_file <- list.files("input", pattern = "xlsx", full.names = T)
c_file

c_cols <- c_file|>
  lapply(function(x){
    find_col_names_excel(x)
  })
c_cols

c_cols <- c_cols|>
  unlist()|>
  unique()|>
  sort()
c_cols

df_col_type <- tibble::tibble(
  key = c_cols,
  value = c(
    "numeric",
    "numeric",
    "text",
    "text",
    "text",
    "text",
    
    "numeric",
    "text",
    "text",
    "text",
    "date",
    "text",
    
    "numeric",
    "text",
    "numeric",
    "text",
    "text",
    "text",
    
    "date",
    "text",
    "numeric",
    "text",
    "numeric",
    "text",
    
    "text",
    "text",
    "date",
    "text",
    "text",
    "numeric",
    
    "text"
  )
)


df_col_type

library(slvwagner)
col_env <- df_col_type|>
  dict_from_data.frame()

# Export funtions to envirnonment
col_env$find_col_names_excel <- find_col_names_excel
col_env$dict_get_values <- dict_get_values
col_env$df_col_type <- df_col_type
col_env$get_excel_data <- get_excel_data

# Save the environment to a file
save(list = ls(col_env), file = "col_env.RData", envir = col_env)

# # remove everything
# rm(list = ls())

# Load into a specific environment
col_env <- new.env()
load("col_env.RData", envir = col_env)

# Test
col_env$dict_get_values(col_env$df_col_type$key, col_env)

# get standard functions
source("source/functions.R")
###########################################################################################
# Get data from excel file with defined column types 
###########################################################################################
print(clc)
# Find all excel files to read in
c_file <- list.files("input", pattern = "xlsx", full.names = T)[2]
c_file

l_data <- col_env$get_excel_data(c_file)
l_data

###########################################################################################
# Find all sheets 
###########################################################################################

# Find all excel files to read in
c_file <- list.files("input", pattern = "xlsx", full.names = T)
c_file

l_data <- c_file|>
  lapply(function(x){
    print(x)
    col_env$get_excel_data(x)
  })
names(l_data) <- c_file
print(clc)

l_data|>
  print()

