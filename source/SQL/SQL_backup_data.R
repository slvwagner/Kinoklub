
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

# Backup
l_data <- DB_backup_DB(con)|>
  convert_DB_to_R(l_template)

# l_template$Filmvorschlag <- l_template$Filmvorschlag|>
#   mutate(`Start-Datum` = as.Date(`Start-Datum`))|>
#   slice(1)
# 
# identical(l_data$Filmvorschlag[1,], l_template$Filmvorschlag[1,])

# l_template$`Eintritt files` <- l_data$`Eintritt files`[1,]
# l_template$`Kiosk files` <- l_data$`Kiosk files`[1,]
# 
# # save template
saveRDS(l_template, "source/SQL/template.Rds")

################################
# Convert to R data type
l_data <- convert_DB_to_R(l_data, l_template)

# Update Programm and Einsatzplan
l_data$Einsatzplan <- l_data$Programm|>
  select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`)|>
  left_join(l_data$Einsatzplan|>
              select(-(2:6)
                     ),
            by = join_by(`Event ID`)
            )|>
  arrange(Datum)

list.files(path = "Backup")

saveRDS(l_data, paste0("Backup/Data",length(list.files(path = "Backup")) + 1L,".Rds"))

# dbExecute(con, sprintf("DROP TABLE IF EXISTS `%s`", "Verleiherabgaben"))

###################################################
# Disconnect from DB
dbDisconnect(con)

message("******************************\nDatabase backup done\n******************************")


