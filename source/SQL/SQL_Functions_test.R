
source("source/SQL/SQL_Functions.R")

#Load the data
c_file <- "Input/Data.Rds"
if(file.exists(c_file)){
  l_data <- readRDS(c_file)
  c_backup_number <- length(list.files(path = "Input/backup", pattern = "backup"))
  if(!dir.exists("Input/backup")) dir.create("Input/backup")
  saveRDS(l_data, paste0("Input/backup/Data_backup",c_backup_number + 1,".Rds")) # Save the updated list to the file
}else{ # or load template date
  c_file <- "Input/template.Rds"
  l_data <- readRDS(c_file)
  c_file <- "Input/Data.Rds"
}

# l_data$Kinoklubmitglieder <- bind_rows(l_data$Kinoklubmitglieder|>
#             slice(1:15),
#           l_data$Kinoklubmitglieder|>
#             filter(Nachname == "Jablonski", Vorname == "Stefan")|>
#             mutate(`Kasse / Bar` = "ja"),
#           l_data$Kinoklubmitglieder|>
#             slice(17:nrow(l_data$Kinoklubmitglieder))
#           )|>
#   mutate(`Kasse / Bar` = factor(`Kasse / Bar`))
# 
# saveRDS(l_data, c_file)


pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
pw
con <- Connect_to_DB(pw, "ch367079_flo")

update_db_all(l_data ,con)

###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run suggessfully")
