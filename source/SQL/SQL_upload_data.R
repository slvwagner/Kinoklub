
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

# # Change columns
# l_data$Programm <- l_data$Programm|>
#   mutate(`Link to Event ID` = factor(`Link to Event ID`))

# update template
l_template <- l_data|>
  lapply(function(df){
    df|>
      slice(1)
  })

# Update
saveRDS(l_template,"source/SQL/template.Rds")
saveRDS(l_data, c_file)

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

con <- DB_connect(pw, "ch367079_flo")

DB_update_all(l_data ,con)

###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run done: data base is now up to date")
