
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
# update template 
l_template <- l_data|>
  lapply(function(df){
    df|>
      slice(1)
  })

# read template 
l_template <- saveRDS(l_template, "source/SQL/template.Rds")

# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
# Data base user 
user <- "ch367079_flo"

# # update data
# l_data$Spezialpreisekiosk <- l_data$Spezialpreisekiosk |>
#   mutate(ID_Programm = 1L,
#          Datum = NULL, 
#          Suisanummer = NULL
#   )|>
#   select(ID, ID_Programm, Spezialpreis, Artikelname)
# saveRDS(l_data, c_file)

pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
pw
con <- DB_connect(pw, "ch367079_flo")

DB_update_all(l_data ,con)

###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run done: data base is now up to date")
