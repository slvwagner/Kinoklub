library(tidyverse)
library(rebus)
library(lubridate)

c_filePath <- "Input/advance tickets/"

Shows <- read_delim(paste0(c_filePath,"Shows.txt"), 
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
  Eintritt = list.files(c_filePath, pattern = "trit", full.names = TRUE),
  Kiosk = list.files(c_filePath, pattern = "iosk", full.names = TRUE),
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

source("source/functions.R")
source("source/SQL/SQL_Functions.R")
# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")

c_suisa <- DB_get_table("Programm", con)|>
  distinct(Suisanummer)|>
  pull()

# search suisa on procinema.ch
l_search <- c_suisa|>
  lapply(search_procinema_by_suisa)
names(l_search) <- df_files$Suisanummer

df_search <- l_search|>
  bind_rows()

# map verleiher 
df_Verleiher <- DB_get_table("Verleiher", con)
df_test <- distinct(df_search, Verleiher)

df_mapping <- 1:nrow(df_test)|>
  lapply(function(ii){
    c_select <- str_detect(tolower(df_Verleiher$Verleihername),tolower(df_test$Verleiher)[ii])
    bind_cols(Verleiher_procinema = df_test$Verleiher[ii],
              Verleiher = df_Verleiher$Verleihername[c_select]
              )
    })|>
  bind_rows()

slvwagner::

# Find details
l_details <- l_search|>
  lapply(function(x){
    if(is.null(x)) return(NULL)
    else film_details(x$link)
  })

df_Filmdetails <- l_details|>
  bind_rows(.id = "Suisanummer")|>
  rename(Inhalt = synopsis,
         Filmtitel = title
         )|>
  mutate(across(contains("release"), 
                ~ as.Date(., format = "%d.%m.%Y")))

df_Filmdetails <- bind_cols(tibble(`ID` = 1:nrow(df_Filmdetails)),
            df_Filmdetails
            )

print(df_Filmdetails)

source("source/SQL/SQL_Functions.R")
# Data base user password from system variables 
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
con <- DB_connect(pw, "ch367079_flo")

DB_copy_table(df_Filmdetails, con, "Filmvorschlag")


l_template <- readRDS("source/SQL/template.Rds")
l_template[["Filmvorschlag"]] <- df_Filmdetails|>
  slice(1)
saveRDS(l_template,"source/SQL/template.Rds")


