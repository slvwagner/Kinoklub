
source("source/SQL/SQL_Functions.R")

#Load the data
c_file <- "Backup/Data.Rds"
l_data <- readRDS(c_file)


# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")

DB_update_all(l_data ,con)

###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run done: data base is now up to date")
