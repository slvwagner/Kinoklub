# load user settings
source("source/functions.R")
source("source/SQL/SQL_Functions.R")

# read template data ######
l_template <- readRDS("source/SQL/template.Rds")

# connect to data base ####
## Data base user password from system variables ####
DB_host <- Sys.getenv("DB_host")
DB_name <- Sys.getenv("DB_name")
DB_user <- Sys.getenv("DB_user")
DB_pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

df_files <- tbl(con, "Eintritt files")|>
  select(`Event ID`, filename)|>
  arrange(`Event ID`)|>
  collect()
df_files

new_rows <- df_files$filename|>
  lapply(convert_data_Film_txt, con)

new_rows <- new_rows|>
  bind_rows()|>
  mutate(ID = row_number())

DB_copy_table(new_rows, con, "df_Eintritt")

print("Done")