
source("source/SQL/SQL_Functions.R")

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")
l_data <- DB_backup_DB(con)
# read template
l_template <- readRDS("source/SQL/template.RDS")
l_template

# data_env <- new.env()
# source("source/calculate.R", local = data_env)
# 
# df_temp <- data_env$df_Kiosk
# df_temp <- bind_cols(ID = 1:nrow(df_temp), df_temp)
# df_temp
# 
# l_template$df_Kiosk <- df_temp|>
#   slice(1)
# 
# saveRDS(l_template, "source/SQL/template.Rds")
# 
# df_temp|>
#   DB_copy_table(con, "df_Kiosk")


# ################################
# # update template
# l_template$Ausgaben
# df_temp <- DB_get_table("Ausgaben", con)
# df_temp <- convert_to_template_types(df_temp, l_template$Ausgaben)
# df_temp
# 
# df_temp <- df_temp|>
#   mutate(Abrechnungsjahr = 2024L)
# df_temp
# 
# slvwagner::r_names(df_temp)
# 
# df_temp <- df_temp|>
#   select("ID", "Kategorie", "Event ID", "Bezeichnung", "Datum", "Abrechnungsjahr", "Betrag [CHF]", "Firmennamen", "Adresse", "Referenz", "Rechnungsnummer", "Buchungskonto")
# df_temp
# 
# l_template$Ausgaben <- df_temp|>
#   slice(1)
# 
# # saveRDS(l_template, "source/SQL/template.RDS")
# DB_copy_table(df_temp, con, "Ausgaben")
# 
# ################################
# # update template
# df_temp <- DB_get_table("Einnahmen", con)
# df_temp <- convert_to_template_types(df_temp, l_template$Einnahmen)
# df_temp
# 
# df_temp <- df_temp|>
#   mutate(Abrechnungsjahr = 2024L)
# df_temp
# 
# slvwagner::r_names(df_temp)
# 
# df_temp <- df_temp|>
#   select("ID", "Kategorie", "Bezeichnung", "Event ID", "Datum", "Abrechnungsjahr", "Betrag [CHF]", "Firmennamen", "Adresse", "Rechnungsnummer")
# df_temp
# 
# l_template$Einnahmen <- df_temp|>
#   slice(1)
# 
# l_template$Einnahmen
# 
# # saveRDS(l_template, "source/SQL/template.RDS")
# DB_copy_table(df_temp, con, "Einnahmen")
# 
# 
# ################################
# saveRDS(l_template, "source/SQL/template.Rds")
# 
# ################################

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


