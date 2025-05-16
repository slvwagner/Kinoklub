
source("source/SQL/SQL_Functions.R")

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")
l_data <- DB_backup_DB(con)
# read template
l_template <- readRDS("source/SQL/template.RDS")


# # update template
# l_template$`Verleiher mapping`
# df_temp <- DB_get_table("Verleiher", con)
# df_temp <- left_join(
#   df_temp,
#   l_template$`Verleiher mapping`,
#   by = join_by(Verleihername)
#   )|>
#   mutate(ID.x = NULL)|>
#   rename(ID = ID.y)
# 
# l_template$`Verleiher mapping` <- df_temp|>
#   slice(-1)|>
#   mutate(ID = row_number())|>
#   select(ID, Verleiher_procinema, Verleihername)|>
#   slice(1)
# 
# l_data$`Verleiher mapping` <- df_temp|>
#   slice(-1)|>
#   mutate(ID = row_number())|>
#   select(ID, Verleiher_procinema, Verleihername)

# l_template$`Verleiher mapping` <- l_data$`Verleiher mapping`
# l_template$Filmvorschlag <- l_template$Filmvorschlag|>
#   mutate(Verleiher = as.factor(Verleiher))
# saveRDS(l_template, "source/SQL/template.RDS")

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
saveRDS(l_data, "Backup/Data.Rds")


# dbExecute(con, sprintf("DROP TABLE IF EXISTS `%s`", "Verleiherabgaben"))

###################################################
# Disconnect from DB
dbDisconnect(con)

message("******************************\nDatabase backup done\n******************************")


