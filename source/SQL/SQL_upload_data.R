
source("source/SQL/SQL_Functions.R")

#Load the data
c_file <- "Backup/Data.Rds"
l_data <- readRDS(c_file)


# # Change columns
# l_data$Programm <- l_data$Programm|>
#   mutate(`Link to Event ID` = factor(`Link to Event ID`))

# update template
l_template <- l_data|>
  lapply(function(df){
    df|>
      slice(1)
  })

# Update
saveRDS(l_template,"source/SQL/template.Rds")
saveRDS(l_data, c_file)

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

con <- DB_connect(pw, "ch367079_flo")

DB_update_all(l_data ,con)

###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run done: data base is now up to date")
