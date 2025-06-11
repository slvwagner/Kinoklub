
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

# l_template$Programm <- l_template$Programm|>
#   mutate(`Verleiher Angefragt?` = factor(`Verleiher Angefragt?`))
# 
# saveRDS(l_template, "source/SQL/template.Rds")


################################
# Convert to R data type

# Backup
l_data <- DB_backup_DB(con)
l_data <- convert_DB_to_R(l_data, l_template)


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


