
source("source/SQL/SQL_Functions.R")

## Data base credentials from system variables ####
DB_host <- Sys.getenv("DB_host")
DB_name <- Sys.getenv("DB_name")
DB_user <- Sys.getenv("DB_user")
DB_pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

# read template
l_template <- readRDS("source/SQL/template.RDS")

l_template$Einsatzplan


# # change template
# l_template$Einsatzplan
# l_template$Programm
# l_template$Programm <- l_template$Programm|>
#   mutate(`Link to Event ID` = factor(`Link to Event ID`))
# l_template$Programm
# 
# saveRDS(l_template, "source/SQL/template.Rds")


################################
# Convert to R data type

# Backup
l_data <- DB_backup_DB(con)
l_data <- convert_DB_to_R(l_data, l_template)

list.files(path = "Backup")

saveRDS(l_data, paste0("Backup/Data",length(list.files(path = "Backup")) + 1L,".Rds"))

l_data$Einsatzplan


###################################################
# Disconnect from DB
dbDisconnect(con)

message("******************************\nDatabase backup done\n******************************")


