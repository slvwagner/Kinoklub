
source("source/SQL/SQL_Functions.R")

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")
l_data <- DB_backup_DB(con)
l_data <- convert_DB_to_R(l_data, l_template)

# l_data$Programm <- l_data$Programm|>
#   left_join(l_data$Einsatzplan)

saveRDS(l_data, "Backup/Data.Rds")

l_data

# l_template <- readRDS("Input/backup/Data_backup1.Rds")
# l_template[["Verleiherabgaben"]] <- NULL
# saveRDS(l_template,"source/SQL/template.Rds")

# update template
l_template <- l_data|>
  lapply(function(df){
    df|>
      slice(1)
  })

# Update template
saveRDS(l_template,"source/SQL/template.Rds")

# dbExecute(con, sprintf("DROP TABLE IF EXISTS `%s`", "Verleiherabgaben"))

###################################################
# Disconnect from DB
dbDisconnect(con)

message("\nDatabase backup done")


