# conver old advaced ticket file to ID format
library(tidyverse)
library(rebus)
library(lubridate)

c_path <- "C:/Users/slvwa/Downloads/2025"

df_files <- 
  tibble(Eintritt = list.files(c_path, "Ein", full.names = TRUE),
         Kiosk = list.files(c_path, "Kiosk", full.names = TRUE),
         Suisanummer = "",
         Datum = ""
         )
df_files

ii <- 1
for (ii in 1:nrow(df_files)) {
  c_raw <- readLines(df_files$Eintritt[ii])|>suppressWarnings()
  
  c_suisa <- str_extract(c_raw, pattern = WRD%R%WRD%R%WRD%R%WRD%R%DOT%R%WRD%R%WRD%R%WRD)
  c_suisa <- c_suisa[!is.na(c_suisa)]
  stopifnot(length(c_suisa) == 1)
  df_files[ii,"Suisanummer"] <- c_suisa
  
  c_Datum <- str_extract(c_raw, pattern = "\t\\d{2}\\.\\d{2}\\.\\d{4}")|>
    str_remove("\t")
  c_Datum <- c_Datum[!is.na(c_Datum)]
  stopifnot(length(c_Datum) == 1)
  df_files[ii,"Datum"] <- c_Datum

}

df_files <- df_files|>
  mutate(Datum = dmy(Datum))|>
  arrange(Datum)|>
  mutate(ID = row_number())

# ID offset due to previously converted files
c_offset <- 49

df_files <- df_files |>
  mutate(
    Eintritt_new = str_replace(Eintritt, pattern = "\\d{2}\\.\\d{2}\\.\\d{2}", paste0("ID", ID + c_offset)),
    Kiosk_new = str_replace(Kiosk, pattern = "\\d{2}\\.\\d{2}\\.\\d{2}", paste0("ID", ID + c_offset))
  ) |>
  mutate(
    Eintritt_new = Eintritt_new |>
      str_remove(pattern = "\\d{4}\\.\\d{3}") |>
      str_replace(pattern = "(ID\\d+)\\s+(?=\\.txt)", replacement = "\\1") |>
      str_trim(),
    
    Kiosk_new = Kiosk_new |>
      str_remove(pattern = "\\d{4}\\.\\d{3}") |>
      str_replace(pattern = "(ID\\d+)\\s+(?=\\.txt)", replacement = "\\1") |>
      str_trim()
  )
df_files$Eintritt_new
df_files$Kiosk_new

file.rename(from = df_files$Kiosk, to = df_files$Kiosk_new)
file.rename(from = df_files$Eintritt , to = df_files$Eintritt_new)
warnings()

