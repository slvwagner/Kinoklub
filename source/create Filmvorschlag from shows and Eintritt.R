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

## Data base user password from system variables ####
DB_host <- Sys.getenv("DB_host")
DB_name <- Sys.getenv("DB_name")
DB_user <- Sys.getenv("DB_user")
DB_pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

c_suisa <- DB_get_table("Programm", con)|>
  distinct(Suisanummer)|>
  pull()
# c_suisa <- "1006.964"

# search suisa on procinema.ch
l_search <- c_suisa|>
  lapply(search_procinema_by_suisa)
names(l_search) <- c_suisa

# manual mapping ####
DB_get_verleiher <- function(con) {
  df_Verleiher <- DB_get_table("Verleiher", con)|>
    select(-ID)
  df_Verleiher
  
  df_Verleiher <- bind_rows(
    # procinema TEMP IMPORT
    df_Verleiher |>
      create_empty_line()|>
      mutate(Verleihername = "TEMP IMPORT")|>
      mutate(`Automatisch generiert` = TRUE),
    # procinema Producer
    df_Verleiher |>
      create_empty_line()|>
      mutate(Verleihername = "PRODUCER")|>
      mutate(`Automatisch generiert` = TRUE),
    # procinema WB => Warner Bros
    df_Verleiher|>
      filter(str_detect(Verleihername, "Warner Bros Entertainment Switzerland GmbH"))|>
      mutate(Verleihername = "WB")|>
      mutate(`Automatisch generiert` = TRUE),
    # procinema WB => Pathé
    df_Verleiher|>
      filter(str_detect(Verleihername, "Pathé Films AG"))|>
      mutate(Verleihername = "PATHE")|>
      mutate(`Automatisch generiert` = TRUE),
    # anything else 
    df_Verleiher|>
      mutate(`Automatisch generiert` = FALSE)
  )
  df_Verleiher <- 
    bind_cols(ID = 1:nrow(df_Verleiher),df_Verleiher)|>
    distinct(Verleihername,.keep_all = TRUE)
  df_Verleiher
  return(df_Verleiher)
}

# get Verleiher from DB ####
df_Verleiher <- DB_get_verleiher(con)

# mapping ####
df_search <- l_search|>
  bind_rows()

df_test <- distinct(df_search, Verleiher, .keep_all = TRUE)
df_test

ii <- 1
df_mapping <- 1:nrow(df_test)|>
  lapply(function(ii){
    c_select <- str_detect(tolower(df_Verleiher$Verleihername),tolower(df_test$Verleiher)[ii])
    
    bind_cols(Verleiher_procinema = df_test$Verleiher[ii],
              Verleihername = df_Verleiher$Verleihername[c_select]
              )
    })|>
  bind_rows()

head(df_mapping, n = 20)

## manual adjustment ####
df_mapping[df_mapping$Verleiher_procinema == "WB",2] <- "Warner Bros Entertainment Switzerland GmbH"
df_mapping[df_mapping$Verleiher_procinema == "PATHE",2] <- "Pathé Films AG"
df_mapping

# only update data for dictionary if code above is correct
# df_mapping <- bind_cols(ID = 1:nrow(df_mapping),
#           df_mapping)
# DB_copy_table(df_mapping, con, "Verleiher mapping")


# dictionary ####
dict_env <- dict_from_data.frame(df_mapping)

# only save if dictionary is up to date 
# The code above may need to be edited to achive that
# saveRDS(dict_env,"Input/Verleiher_dict.Rds")


# get Verleiher ####
df_search <- df_search|>
  mutate(Verleiher =  dict_get_values(Verleiher,envir = dict_env))

df_Filmvorschlag <- 
  bind_cols(
    ID = 1:nrow(df_search),
    df_search
    )|>
  mutate(release_date = lubridate::dmy(release_date)|>suppressWarnings())|>
  rename(`Veröffentlichungs-Datum` = release_date,
         `Eintritte eingespielt` = admissions)

# Find details ####
l_details <- 1:nrow(df_Filmvorschlag)|>
  lapply(function(ii){
    pull(df_Filmvorschlag[ii,"link"])|>
      film_details()
  })

df_Filmdetails <- l_details|>
  bind_rows()|> #.id = "Suisanummer")|>
  rename(Inhalt = synopsis,
         Filmtitel = title
         )|>
  mutate(across(contains("release"), 
                ~ as.Date(., format = "%d.%m.%Y")))

df_Filmdetails <- bind_cols(tibble(`ID` = 1:nrow(df_Filmdetails)),
            df_Filmdetails
            )

Filmvorschlag <- df_Filmvorschlag|>
  left_join(df_Filmdetails|>
              select(ID, Inhalt, director, producer, actors, writer)
              )|>
  mutate(Trailer = "")|>
  select("ID", "Suisanummer","Filmtitel", "link", "Trailer", "Verleiher", "Veröffentlichungs-Datum", 
         "Eintritte eingespielt", "Inhalt", "director", "producer", "actors", "writer")|>
  rename(Procinema = link)


