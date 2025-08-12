
source("source/SQL/SQL_Functions.R")

# read template
l_template <- readRDS("source/SQL/template.RDS")

l_template$Programm <- l_template$Programm|>
  mutate(Kommentar = "")
l_template$Programm

l_template$Filmvorschlag <- l_template$Filmvorschlag|>
  rename(Kommentar = "Kategorie")
l_template$Filmvorschlag

saveRDS(l_template, "source/SQL/template.Rds")


################################
## Data base credentials from system variables ####
DB_host <- Sys.getenv("DB_host")
DB_name <- Sys.getenv("DB_name")
DB_user <- Sys.getenv("DB_user")
DB_pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

# Backup
l_data <- DB_backup_DB(con)
# Convert to R data type
l_data <- convert_DB_to_R(l_data, l_template)

l_data$Filmvorschlag
l_data$Programm

################################
# create new file name
df_temp <- tibble(file = list.files(path = "Backup"))

# library(rebus)
# p <- capture(one_or_more(DGT))%R%DOT%R%"Rds"%R%END
# as.character(p)
# str_match(df_temp$file, p)

p <- "([\\d]+)\\.Rds$"

df_temp <- df_temp|>
  mutate(ID = str_match(file, p)[,2]|>as.integer())|>
  arrange(ID)
df_temp

################################
# save backup
saveRDS(l_data, paste0("Backup/Data",max(df_temp$ID) + 1L,".Rds"))

###################################################
# Disconnect from DB
dbDisconnect(con)

message("******************************\nDatabase backup done\n******************************")


