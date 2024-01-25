library(tidyverse)
library(rebus)
library(openxlsx)
library(lubridate)

file.remove("error.log")|>suppressWarnings()

source("source/functions.R")

########################################################################
# Einnahmen und Ausgangen einlesen aus Excel 
########################################################################

c_file <- "Einnahmen und Ausgaben.xlsx"
c_sheets <- readxl::excel_sheets(paste0("Input/",c_file))

Einnahmen_und_Ausgaben <- lapply(c_sheets, function(x) {
  readxl::read_excel(paste0("Input/",c_file), 
                     sheet = x)
})
names(Einnahmen_und_Ausgaben) <- c_sheets
Einnahmen_und_Ausgaben

Einnahmen_und_Ausgaben[["Ausgaben"]] <- Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  mutate(Spieldatum = as.Date(Spieldatum),
         Datum = as.Date(Datum))

Einnahmen_und_Ausgaben[["Einnahmen"]] <- Einnahmen_und_Ausgaben[["Einnahmen"]]|>
  mutate(Datum = as.Date(Datum))


########################################################################
# Eintrittabrechnung aus Advanced Tickets konvertieren
########################################################################
convert_data_Film_txt <- function(c_fileName) {
  l_data <- list()
  for(kk in 1:length(c_fileName)){
    # read in data
    c_raw <- suppressWarnings(readLines(c_fileName[kk]))
    c_raw
    l_temp <- list()
    
    # Extract suisa
    p <- START%R%DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT #suisa
    index <- c_raw|>
      str_detect(p)
    
    c_temp <- c_raw[index]|>
      str_split("\t")|>
      unlist()
    
    ii <- 1
    
    l_temp[[ii]] <- c_temp[1]
    names(l_temp)[ii] <- "Suisa"
    ii <- ii+1
    
    # Extract Filmtitel
    l_temp[[ii]] <- c_temp[2]
    names(l_temp)[ii] <- "Filmtitel"
    ii <- ii+1
    
    # Extract Datum
    p <- "\t"%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT%R%DGT #Datum
    index <- c_raw|>
      str_detect(p)
    index
    
    c_temp <- c_raw[index]|>
      str_split("\t")|>
      unlist()
    c_temp
    
    l_temp[[ii]] <- c_temp[2]
    names(l_temp)[ii] <- "Datum"
    ii <- ii+1
    
    # Extract suisa-Vorabzug
    p <- DGT%R%DOT%R%one_or_more(DGT)%R%"%"%R%SPC%R%"SUISA" #Datum
    index <- c_raw|>
      str_detect(p)
    index
    
    c_temp <- c_raw[index]|>
      str_split("%"%R%SPC)|>
      unlist()
    c_temp
    
    l_temp[[ii]] <- c_temp[1]|>as.numeric()
    names(l_temp)[ii] <- "SUISA-Vorabzug"
    ii <- ii+1
    
    # Extract Tabelle
    p <- "Platzkategorie" #Tabellenanfang
    p1 <- "Brutto" # Tabellenende
    
    index <- c_raw|>
      str_detect(p)
    index1 <- c_raw|>
      str_detect(p1)
    
    for (jj in 1:length(c_raw)) {
      if(index[jj]== TRUE) {
        index <- jj 
        break
      }
    }
    for (jj in 1:length(c_raw)) {
      if(index1[jj]== TRUE) {
        index1 <- jj-2 
        break
      }
    }
    df_data <- c_raw[(index+1):index1]|>
      str_split("\t")|>
      bind_cols()|>
      as.matrix()|>
      t()|>
      as.data.frame()|>
      suppressMessages()
    
    names(df_data) <- c_raw[index]|>
      str_split("\t")|>
      unlist()
    
    df_data <- df_data|>
      mutate(Preis = as.numeric(Preis),
             Tax = as.numeric(Tax),
             Anzahl = as.numeric(Anzahl),
             Umsatz= Preis*Anzahl
             )|>
      tibble()
    
    l_temp[[ii]] <- df_data|>
      tibble()
    names(l_temp)[ii] <- "Abrechnung"
    
    l_data[[kk]] <- l_temp[[ii]] |>
      mutate(`Suisa Nummer` = l_temp[[1]],
             Filmtitel = l_temp[[2]],
             Datum_ = l_temp[[3]],
             `SUISA-Vorabzug` = l_temp[[4]]
             )
  }
  return(l_data)
}

########################################################################
# Eintritt aus Advance Tickets
########################################################################

c_files <- list.files(pattern = "Eintritte", recursive = T)
l_Eintritt <- convert_data_Film_txt(c_files)
names(l_Eintritt) <- c_files|>str_extract(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))

l_Eintritt
df_Eintritt <- l_Eintritt|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum),
         Datum_ = NULL,
         Verkaufspreis = Preis ,
         Tax = NULL, 
         Zahlend = if_else(Verkaufspreis == 0, F, T))|>
  select(Datum, Filmtitel,`Suisa Nummer`,Platzkategorie,Zahlend,Verkaufspreis, Anzahl,Umsatz,`SUISA-Vorabzug`)

df_Eintritt

########################################################################
# Filmvorführungen
########################################################################

df_Flimvorfuerungen <- l_Eintritt|>
  lapply( function(x){ 
    distinct(x, Datum_,`Suisa Nummer`)
  })|>
  bind_rows()|>
  mutate(Datum = Datum_|>dmy()|>as.Date())
df_Flimvorfuerungen

c_Date <- df_Flimvorfuerungen$Datum
c_suisa_nr <- df_Flimvorfuerungen$`Suisa Nummer`

########################################################################
# Eventausgaben
########################################################################

Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`

Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  filter(Kategorie == "Eventausgaben")


########################################################################
# Kioskabrechnungen
########################################################################
source("source/Kiosk.R")


########################################################################
# show times
########################################################################

# error handling file not found
try(c_raw <- list.files(pattern = "Shows", recursive = T)|>
      readLines()|>
      suppressWarnings(),
    outFile = "error.log"
    )
if(length(list.files(pattern = "error.log"))>0) stop("\nDatei Shows.txt nicht gefunden. \nBitte herunterladen und abspeichern.")

m <- c_raw[3:length(c_raw)]|>
  str_split("\t", simplify = T)
m <- m[,1:(ncol(m)-1)] 
colnames(m) <- c_raw[2]|>
  str_split("\t", simplify = T)|>as.vector()

df_show <- m|>
  as_tibble()|>
  mutate(Datum = Tag|>lubridate::ymd(),
         Anfang = parse_time(Anfang),
         Ende = parse_time(Ende))|>
  select(Datum,Anfang, Ende, Saal, Titel, Version, Alter)|>
  rename(Filmtitel = Titel)

df_show <- df_show|>
  left_join(df_Eintritt|>
              distinct(Datum, `Suisa Nummer`),
            by = c("Datum" = "Datum")
            )

########################################################################
# Verleiher
########################################################################

# Verleiherabganen
df_verleiherabgaben <- readxl::read_excel("input/Verleiherabgaben.xlsx")|>
  mutate(Datum = as.Date(Datum))
df_verleiherabgaben

df_Eintritt <- df_Eintritt|>
  left_join(df_verleiherabgaben, by = c(`Suisa Nummer` = "Suisa", "Datum"))
df_Eintritt


# Verleiherrechnung 
df_Verleiher_Rechnnung <- df_Eintritt|>
  distinct(Datum,`Suisa Nummer`,.keep_all = T)|>
  select(Datum, Filmtitel, `Suisa Nummer`)
df_Verleiher_Rechnnung

# # Verleiherabgaben sind im Dropdown zu finden
# Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`

df_Verleiher_Rechnnung <- df_Verleiher_Rechnnung|>
  left_join(Einnahmen_und_Ausgaben[["Ausgaben"]] |>
              filter(Kategorie == "Verleiher")|>
              select(-Datum,-Kategorie),
            by = c("Datum" = "Spieldatum"))|>
  mutate(`keine Verleiherrechnung` = if_else(is.na(Betrag), T, F))
df_Verleiher_Rechnnung

df_keine_Rechnnung <- df_Verleiher_Rechnnung|>
  filter(`keine Verleiherrechnung`)

# error handling
if(nrow(df_keine_Rechnnung)>0) {
  warning(paste0("\nAchtung für die diesen Film gibt es keine Verleiherrechnung: \n",
                   day(df_keine_Rechnnung$Datum),".",month(df_keine_Rechnnung$Datum),".", lubridate::year(df_keine_Rechnnung$Datum), " ",df_keine_Rechnnung$Filmtitel,"\n")
          )  
}


########################################################################
# Gewinn/Verlust Eintitt
########################################################################

l_GV <- list()
ii <- 3
for (ii in 1:length(c_Date)) {
  c_Besucher <- df_Eintritt|>
    filter(Datum == c_Date[ii])|>
    reframe(Besucherzahl = sum(Anzahl))|>
    pull()
  c_Besucher
  
  c_Gratis <- df_Eintritt|>
    filter(Datum == c_Date[ii], !Zahlend)|>
    reframe(Gratiseintritte = sum(Anzahl))|>
    pull()
  c_Gratis
  
  c_Umsatz <- df_Eintritt|>
    filter(Datum == c_Date[ii])|>
    reframe(Umsatz = sum(Umsatz))|>
    pull()
  
  c_suisaabzug <- (distinct(df_Eintritt|>filter(Datum == c_Date[ii]), 
                            `SUISA-Vorabzug`)|>pull())/100
  c_suisaabzug
  
  c_verleiherabzug <- (distinct(df_Eintritt|>filter(Datum == c_Date[ii]), 
                               `Abzug [%]`)|>pull())/100
  c_verleiherabzug
  
  c_Verleiherrechnung <- df_Verleiher_Rechnnung |>
        filter(Datum == c_Date[ii]) |>
        select(Betrag) |>
        pull()
  
  c_Verleiherrechnung
  
  c_MWST_Abzug <- round5Rappen(c_Verleiherrechnung-(c_Verleiherrechnung/(1+(c_MWST/100))))
  c_MWST_Abzug
  
  if(is.na(c_MWST_Abzug)) { # Wemm keine Verleiherrechnung vorhanden ist muss die MWST vom Umsatz berechnet werden.
    c_MWST_Abzug <- round5Rappen((c_Umsatz*c_verleiherabzug)*(c_MWST/100))
    }
  c_MWST_Abzug
  
  
  # Error handling Verleiherabgaben
  if(is.na(c_verleiherabzug)) {
    error <- df_Eintritt|>filter(Datum == c_Date[ii])|>
      distinct(Datum,.keep_all = T)|>
      select(Datum, `Suisa Nummer`, Filmtitel)|>
      mutate(Datum = paste0(day(Datum),".",month(Datum),".",year(Datum)))
    error <- str_c("\nFür den Film ", error$Filmtitel," am ", error$Datum, " mit Suisa-Nummer ", error$`Suisa Nummer`, " wurde keine",
                   "\nVerleiherabgabe im file \"Verleiherabgaben.xlsx\"definiert.")
    stop(error)
  }

  l_GV[[ii]] <- tibble(Datum = c_Date[ii],
                       `Suisa Nummer` = c_suisa_nr[ii],
                       Umsatz = c_Umsatz, 
                       Verleiherrechnung = c_Verleiherrechnung,
                       `SUISA-Abzug [%]` = c_suisaabzug*100,
                       `SUISA-Abzug [CHF]` = round5Rappen(c_Umsatz*c_suisaabzug), 
                       `Verleiher-Abzug [%]` =c_verleiherabzug*100,
                       `Verleiher-Abzug [CHF]` = round5Rappen(c_Umsatz*c_verleiherabzug),
                       `MWST Satz auf die Verleiherrechnung [%]` = c_MWST,
                       `MWST auf die Verleiherrechnung [CHF]` = c_MWST_Abzug
                       )|>
    mutate(`Verleiher-Abzug [CHF]` = if_else(`Verleiher-Abzug [CHF]` > 150, `Verleiher-Abzug [CHF]`, 150),
           `Sonstige Kosten [CHF]` = (c_Verleiherrechnung - c_MWST_Abzug) - `Verleiher-Abzug [CHF]`,
           `Gewinn/Verlust [CHF]` = round5Rappen(Umsatz - sum(`SUISA-Abzug [CHF]`,`Verleiher-Abzug [CHF]`,
                                                              `Sonstige Kosten [CHF]`, `MWST auf die Verleiherrechnung [CHF]`,
                                                              na.rm = T))
           )|>
    left_join(df_show, by = join_by(Datum, `Suisa Nummer`))
  
  l_GV[[ii]]
}

l_GV

df_GV_Eintritt <- l_GV|>
  bind_rows()

names(df_GV_Eintritt)
df_GV_Eintritt$`Gewinn/Verlust [CHF]`


########################################################################
# Gewinn Kiosk
########################################################################

ii <- 3

df_Kiosk|>
  group_by(Datum)|>
  reframe(sum(Kassiert), 
          sum(Gewinn))

l_GV_Kiosk <- list()
for (ii in 1:length(c_Date)) {
  l_GV_Kiosk[[ii]] <- df_Kiosk|>
    filter(Datum == c_Date[ii])|>
    reframe(Kassiert = sum(Kassiert, na.rm = T),
            Gewinn = sum(Gewinn, na.rm = T))|>
    mutate(Datum = c_Date[ii],
           `Suisa Nummer` = c_suisa_nr[ii]
           )
  l_GV_Kiosk[[ii]]
}
l_GV_Kiosk

df_GV_Kiosk <- l_GV_Kiosk|>
  bind_rows()|>
  left_join(df_show, by = join_by(Datum, `Suisa Nummer`))|>
  select( Datum,Anfang, `Suisa Nummer`, Filmtitel, Kassiert, Gewinn)

df_GV_Kiosk


########################################################################
# Gewinn Filmvorführung
########################################################################

ii <- 3
l_GV_Vorfuehrung <- list()
for (ii in 1:length(c_Date)) {
  df_Eventausgaben <- Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Datum == c_Date[ii], Kategorie == Einnahmen_und_Ausgaben$dropdown$`drop down`[1])
 
  if(nrow(df_Eventausgaben)<1) c_Eventausgaben <- 0
  else c_Eventausgaben <- sum(df_Eventausgaben$Betrag, na.rm = T)
  
  l_GV_Vorfuehrung[[ii]] <- tibble(
    Datum = c_Date[ii],
    `Suisa Nummer` = c_suisa_nr[ii],
    `Gewinn/Verlust [CHF]` =round5Rappen(l_GV_Kiosk[[ii]]$Gewinn + l_GV[[ii]]$`Gewinn/Verlust [CHF]` + pull(df_manko_uerberschuss|>filter(Datum == c_Date[ii])) - c_Eventausgaben)
  )
}

df_GV_Vorfuehrung <- l_GV_Vorfuehrung|>
  bind_rows()
df_GV_Vorfuehrung

########################################################################
# write to Excel
########################################################################
dir.create("output/") |> suppressWarnings()

list(Eintritte= df_Eintritt,
     `Gewinn Verlust Eintritt` = df_GV_Eintritt,
     Kiosk = df_Kiosk,
     `Gewinn Kiosk` = df_GV_Kiosk,
     Verleiherabgaben  = df_verleiherabgaben,
     Einkaufspreise = df_Einkaufspreise,
     Spezialpreisekiosk = Spezialpreisekiosk,
     Ausgaben = Einnahmen_und_Ausgaben[["Ausgaben"]],
     Einnahmen = Einnahmen_und_Ausgaben[["Einnahmen"]]
     )|>
  write.xlsx(file="output/Auswertung.xlsx", asTable = TRUE)

remove(l_Eintritt,  m, c_raw, l_GV, l_GV_Kiosk, c_Besucher,  df_Eventausgaben,
       c_suisaabzug, c_verleiherabzug, c_Gratis, c_Umsatz, l_GV_Vorfuehrung,ii, c_Eventausgaben,
       convert_data_Film_txt, c_file, c_Verleiherrechnung, c_sheets)


########################################################################
# user interaction
########################################################################
writeLines("Datenkonvertierung erfolgt")

