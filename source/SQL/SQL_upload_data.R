
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

pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
pw
con <- Connect_to_DB(pw, "ch367079_flo")

update_db_all(l_data ,con)

###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run done")
