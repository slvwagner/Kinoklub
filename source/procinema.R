library(rebus)
library(readr)
library(tidyverse)

# Exctract date from yearweek
YearWeektoDate <- function(yearweek) {
  year <-  yearweek|>
    str_extract(START%R%DGT%R%DGT%R%DGT%R%DGT)|>
    as.integer()
  week_number <- yearweek|>
    str_extract(DGT%R%DGT%R%END)|>
    as.integer()
  
  first_day_of_year <- ymd(paste0(year, "-01-01"))
  first_day_of_week <- ymd(paste0(year, "-01-01")) + (week_number - 1) * 7
  return(first_day_of_week)
}

# Extract Suisanummer
Get_suisa <- function(c_suisa) {
  first_DGT <-  c_suisa|>
    str_extract(START%R%DGT%R%DGT%R%DGT%R%DGT)
  last_DGT <- c_suisa|>
    str_extract(DGT%R%DGT%R%DGT%R%END)
  return(paste0(first_DGT,".",last_DGT))
}

# error handling
c_file <- "Input/Procinema/Procinema.txt"
if(!file.exists(c_file)) stop(paste0("\nDie Datei \"", c_file, " konnte nicht gefunden werden.",
                                     "\nBitte herunterladen und abspeichern oder über GUI hochladen."))

# read in data and convert
df_Procinema <- 
  read_delim(
    c_file, 
    delim = "\t", escape_double = FALSE, 
    col_types =  cols(
      YearWeek = col_character(),
      ScreenID = col_skip(),
      Screen = col_skip(),
      Part = col_skip(),
      Distr = col_character(),
      SUISA = col_character(),
      OrigTitle = col_character(),
      TotPro = col_integer(),
      TotAdm = col_integer(),
      TotBox = col_integer(),
      Dac = col_character(),
      Pro_OV_2D = col_integer(),
      Adm_OV_2D = col_integer(),
      Box_OV_2D = col_integer(),
      Pro_GV_2D = col_integer(),
      Adm_GV_2D = col_integer(),
      Box_GV_2D = col_integer(),
      Pro_FV_2D = col_integer(),
      Adm_FV_2D = col_integer(),
      Box_FV_2D = col_integer(),
      Pro_IV_2D = col_integer(),
      Adm_IV_2D = col_integer(),
      Box_IV_2D = col_integer(),
      Pro_OV_3D = col_integer(),
      Adm_OV_3D = col_integer(),
      Box_OV_3D = col_integer(),
      Pro_GV_3D = col_integer(),
      Adm_GV_3D = col_integer(),
      Box_GV_3D = col_integer(),
      Pro_FV_3D = col_integer(),
      Adm_FV_3D = col_integer(),
      Box_FV_3D = col_integer(),
      Pro_IV_3D = col_integer(),
      Adm_IV_3D = col_integer(),
      Box_IV_3D = col_integer(),
      TotPro_OV = col_integer(),
      TotAdm_OV = col_integer(),
      TotBox_OV = col_integer(),
      TotPro_GV = col_integer(),
      TotAdm_GV = col_integer(),
      TotBox_GV = col_integer(),
      TotPro_FV = col_integer(),
      TotAdm_FV = col_integer(),
      TotBox_FV = col_integer(),
      TotPro_IV = col_integer(),
      TotAdm_IV = col_integer(),
      TotBox_IV = col_integer(),
      TotPro_2D = col_integer(),
      TotAdm_2D = col_integer(),
      TotBox_2D = col_integer(),
      TotPro_3D = col_integer(),
      TotAdm_3D = col_integer(),
      TotBox_3D = col_integer()
    ),
    trim_ws = TRUE
    )|>
  select(-ends_with(c("2D", "3D", "IV", "FV", "GV", "OV")))|>
  mutate(Datum = YearWeektoDate(YearWeek),
         Suisanummer = Get_suisa(SUISA))|>
  select(-SUISA, -YearWeek, -Dac)|>
  rename(Filmtitel = OrigTitle,
         Spielwochen = TotPro,
         Besucherzahl = TotAdm,
         Besucherzahlen_ch = TotBox)|>
  mutate(ration_from_total_box = (Besucherzahl / Besucherzahlen_ch) * 100)
df_Procinema

# 
s_df_Procinema <- df_Procinema|>
  distinct(Suisanummer, .keep_all = T)|>
  select(Suisanummer, Filmtitel, Distr)|>
  left_join(df_Procinema|>
              group_by(Suisanummer)|>
              reframe(Spielwochen = sum(Spielwochen),
                      Besucherzahl = sum(Besucherzahl),
                      Besucherzahlen_ch = sum(Besucherzahlen_ch))|>
              mutate(ration_from_total_box = (Besucherzahl / Besucherzahlen_ch) * 100),
            by = join_by(Suisanummer)
            )
s_df_Procinema
 
remove(c_file)
 
writeLines("Procinema daten eingelesen")
