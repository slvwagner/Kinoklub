library(tidyverse)
library(rebus)
library(lubridate)

Shows <- read_delim("Shows.txt", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(Tag = col_date(format = "%Y-%m-%d"), 
                                     Anfang = col_time(format = "%H:%M"), 
                                     Ende = col_skip(), Saal = col_skip(), 
                                     Version = col_skip(), Alter = col_skip()), 
                    trim_ws = TRUE, skip = 7)|>
  suppressWarnings()
Shows <- Shows|>
  rename(Zeit = Anfang,
         Filmtitel = Titel,
         Datum = Tag)
Shows

df_files <- tibble(
  Eintritt = list.files("C:/Users/slvwa/OneDrive/Desktop/old/", pattern = "trit", full.names = TRUE),
  Kiosk = list.files("C:/Users/slvwa/OneDrive/Desktop/old/", pattern = "iosk", full.names = TRUE),
  Suisanummer = "",
  Datum = "",
  Filmtitel = ""
  )
df_files

ii <- 1
for (ii in 1:nrow(df_files)) {
  c_raw <-readLines(df_files$Eintritt[ii])|>
    suppressWarnings()
  c_raw
  
  c_suisa <- str_extract(c_raw, pattern = WRD%R%WRD%R%WRD%R%WRD%R%DOT%R%WRD%R%WRD%R%WRD)
  c_suisa <- c_suisa[!is.na(c_suisa)]
  df_files[ii,"Suisanummer"] <- c_suisa
  
  c_Datum <- str_extract(c_raw, pattern = "\t"%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT%R%DGT)|>
    str_remove("\t")
  c_Datum
  c_Datum <- c_Datum[!is.na(c_Datum)]
  df_files[ii,"Datum"] <- c_Datum
  
  # Pattern: 4 digits, a dot, 3 digits, tab, capture the rest
  p <- "\\w{4}\\.\\w{3}\\t(.*)"
  
  c_Film <- str_match(c_raw, p)[,2]
  c_Film <- c_Film[!is.na(c_Film)]
  df_files[ii,"Filmtitel"] <- c_Film[!is.na(c_Film)]
  
}
df_files <- df_files|>
  mutate(Datum = dmy(Datum))
df_files

df_files <- df_files|>
  mutate(`Link to Event ID` = "",
         Verleiher = "",
         `Verleiher Angefragt?` = "Bestätigt",
         `Abzug [%]` = 30,
         `Minimal Abzug [CHF]` = 150,
         `Abzug fix [CHF]`= 0,
         `Verleihervertrag abgelegt` = "",
         `Anzahl bestellter Poster und Flyer` = "",
         `Poster und Flyer erhalten?` = "",
         `Art der Filmlieferung` = "",
         `Besucherzahlen an Verleiher gesendet` = "ja",
         `Rechnung bezahlt und abgelegt` = "ja",
         `KDM ja oder nein` = ""
         )

print(df_files)

#Load the data
c_file <- "Input/Data.Rds"
l_data <- readRDS(c_file)
df_files <-  bind_rows(df_files, l_data$Programm)|>
  arrange(Datum)|>
  mutate(`Event ID` = row_number())
df_files

source("source/SQL/SQL_Functions.R")
# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")

DB_copy_table(df_files, con, "Programm")
