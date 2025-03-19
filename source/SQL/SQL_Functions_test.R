source("source/SQL/SQL_Functions.R")
# DB connection
con <- Connect_to_DB()
con
tables <- dbListTables(con)
print(tables)

# Load the data
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
l_data

# create and update tables on SQL
update_DB_all(l_data, con)

# Convert data types for each table
convert_sql_to_R(l_data_sql,l_data)

all.equal(l_data, l_data_sql_converted)


tbl(con, "Ausgaben")|>
  filter(Kategorie == "Personalaufwand")|>
  explain()
tbl(con, "Einnahmen")|>
  show_query()

tbl(con, "Ausgaben")
tbl(con, "Spezialpreisekiosk")
tbl(con, "Einkauf Kiosk")
tbl(con, "Programm")
tbl(con, "Einsatzplan")


###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run suggessfully")
