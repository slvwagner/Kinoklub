
source("source/SQL/SQL_Functions.R")

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")
l_data <- DB_backup_DB(con)

dbExecute(con, sprintf("DROP TABLE IF EXISTS `%s`", "df_Eintritt"))

saveRDS(l_data, "Backup/Data.Rds")

###################################################
# Disconnect from DB
dbDisconnect(con)

message("\nDatabase backup done")


