
source("source/SQL/SQL_Functions.R")

#Load the data
c_files <- list.files(path = "Backup", full.names = TRUE)

df_temp <- tibble(files = c_files)

# library(rebus)
# p <- capture(one_or_more(DGT))%R%DOT%R%"Rds"
p <- "([\\d]+)\\.Rds"

# find latest backup
df_temp <- df_temp|>
  mutate(ID = str_match(c_files, p)[,2]|>
           as.integer()
         )|>
  filter(ID == max(ID, na.rm = TRUE))
df_temp

l_data <- readRDS(df_temp$files)

## Data base credentials from system variables ####
DB_host <- Sys.getenv("DB_host")
DB_name <- "ch367079_GUI_testing_envir"
DB_user <- Sys.getenv("DB_user")
DB_pw   <- Sys.getenv("DB_PASSWORD_KINOKLUB")

## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

# update database
DB_update_all(l_data ,con)

###################################################
# Disconnect from DB
dbDisconnect(con)

writeLines("Script run done: data base is now up to date")
