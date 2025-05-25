
source("source/SQL/SQL_Functions.R")

## Data base credentials from system variables ####
DB_host <- Sys.getenv("DB_host")
DB_name <- Sys.getenv("DB_name")
DB_user <- Sys.getenv("DB_user")
DB_pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

# Eintritt
df_data <- DB_get_table("Eintritt files", con)
df_data

for (ii in 1:nrow(df_data)) {
  writeLines(df_data$`file content`[ii], paste0("C:/Users/slvwa/Downloads/Kiosk und Eintritt/",df_data$filename[ii]))
}

# Eintritt
df_data <- DB_get_table("Kiosk files", con)
df_data

for (ii in 1:nrow(df_data)) {
  writeLines(df_data$`file content`[ii], paste0("C:/Users/slvwa/Downloads/Kiosk und Eintritt/",df_data$filename[ii]))
}
