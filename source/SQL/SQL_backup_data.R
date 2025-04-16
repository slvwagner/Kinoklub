
source("source/SQL/SQL_Functions.R")

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")
l_data <- DB_backup_DB(con)
saveRDS(l_data, "Backup/Data.Rds")

# update template
l_template <- l_data|>
  lapply(function(df){
    df|>
      slice(1)
  })

# Update
saveRDS(l_template,"source/SQL/template.Rds")
saveRDS(l_data, c_file)


# dbExecute(con, sprintf("DROP TABLE IF EXISTS `%s`", "df_Eintritt"))

###################################################
# Disconnect from DB
dbDisconnect(con)

message("\nDatabase backup done")


