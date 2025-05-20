# read and caluclate all input data
library(rebus)
library(openxlsx)
library(lubridate)
library(tidyverse)

writeLines("Daten werden einlesen und berechnet...")

# clean
# rm(list = ls())

# load user settings
source("source/functions.R")
source("source/SQL/SQL_Functions.R")

# read template data ######
# read template
l_template <- readRDS("source/SQL/template.Rds")

# connect to data base ####
## Data base user password from system variables ####
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
## Data base user ####
user <- "ch367079_flo"
## Connection ####
con <- DB_connect(pw, "ch367079_flo")

# Data for GUI
l_data <- DB_get_Data(l_template, con)|>
  convert_DB_to_R(l_template)

# check nb of files Eintritt vs Kiosk ####
c_eintritt <- list.files("Input/advance tickets",pattern = "Eintritt")
c_Kiosk <- list.files("Input/advance tickets",pattern = "Kiosk")

if(length(c_eintritt) != length(c_Kiosk)) {
  if(length(c_eintritt) > length(c_Kiosk)){
    stop("\nEs gibt ", length(c_eintritt), " Eintrittsdateien aber ", length(c_Kiosk), " Kioskdateien.") 
  }else {
    stop("\nEs gibt ", length(c_Kiosk), "  Kioskdateien aber ", length(c_eintritt), " Eintrittsdateien.") 
  }
} 
c_test <- str_extract(c_eintritt, one_or_more(DGT)) %in% str_extract(c_Kiosk, one_or_more(DGT))
c_test

if(sum(c_test) != length(c_test)){
  c_index <- tibble(test = )|>
    mutate(index  = row_number())|>
    filter(!test)|>
    select(index)|>
    pull()
  stop("\nEs gibt keine Datei \"Eintritt ID",str_extract(c_Kiosk[c_index], pattern = one_or_more(DGT)),".txt\" aber eine Datei \"", c_Kiosk[c_index], "\"",
       "\nEine der Dateien muss benannt oder gelöscht werden. ", "\nBitte im Verzeichniss  .../Input/advanced tickets/ korrigieren.\n")
  c_Kiosk[c_index]
} 

# Programm check ####
df_temp <- DB_get_table("Programm", con)|>
  convert_to_template_types(l_template$Programm)
df_temp

if(nrow(df_temp) != nrow(l_data$Programm)){
  df_temp <- anti_join(l_data$Programm,
                       df_temp,
                       by = "Event ID"
  )
  
  df_temp <- df_temp|>
    group_by(Suisanummer, Datum, Filmtitel)|>
    reframe(n = n())|>
    filter(n > 1)
  df_temp
  stop(paste0("\nFür den Film ", df_temp$Suisanummer, " / ", format(df_temp$Datum,"%d.%m.%Y")," gibt es mehrere Einträge im Programm.",
              "\nEs ist aber nur einer erlaubt pro Datum und Suisanummer. Bitte das Proramm korrigieren!"))
}

# Einnahmen und Ausgaben einlesen ##################
Einnahmen_und_Ausgaben <- list(Einnahmen = l_data$Einnahmen|>
                                 mutate(`Event ID` = as.character(`Event ID`)|>as.integer())
                               ,
                               Ausgaben = l_data$Ausgaben|>
                                 mutate(`Event ID` = as.character(`Event ID`)|>as.integer())
)

# check suisanummer  ##################
## error handling
p <- or(DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT,
        WRD%R%WRD%R%WRD%R%WRD%R%DOT%R%WRD%R%WRD%R%WRD
)

df_temp <- l_data$Programm|>
  filter(`Verleiher Angefragt?` != "Wird nicht gespielt")|>
  select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, Verleiher, `Verleiher Angefragt?`)|>
  filter(!str_detect(Suisanummer,p))
df_temp

if(nrow(df_temp) != 0) {
  warning(paste0(
    "Für den Film ID ",df_temp$`Event ID` ," / ",df_temp$Filmtitel, " am ",
    day(df_temp$Datum),".",month(df_temp$Datum),".",year(df_temp$Datum),
    " ist die Suisanummer ",df_temp$Suisanummer, " vorhanden aber das Format stimmmt nicht.")
  )}


# Eintritt aus Advanced Tickets ####
# files to read in
c_files <- list.files(pattern = "Eintritte", recursive = T)

# error handling
if(is_empty(c_files)) {
  stop(paste0("\nEs gibt keinen Dateien im Verzeichniss: \".../Kinoklub/Input/advance tickets\"",
              "\nBitte herunterladen ","<https://www.advance-ticket.ch/decomptefilms?lang=de> und abspeichern:",
              "\n\"Eintritte xx.xx.",Abrechungsjahr,"\"\n")
  )
}

# read and convert Eintritte
df_Eintritt <- convert_data_Film_txt(c_files, con)

# Kiosk ####
c_path <- "input/advance tickets"
c_files <- list.files(c_path, pattern = "Kiosk", recursive = TRUE, full.names = TRUE)
df_Kiosk <- convert_data_kiosk_txt(c_files, con)
df_Kiosk

df_manko_uerberschuss <- df_Kiosk|>
  distinct(`Event ID`,.keep_all = TRUE)|>
  select(`Event ID`, `Überschuss / Manko [CHF]`)
df_manko_uerberschuss

# check if all Kiosk entry can be joined by `Event ID`
if(sum(is.na(df_Kiosk$`Event ID`)) > 0){
  df_temp <- df_Kiosk|>
    filter(is.na(`Event ID`))|>
    distinct(`Event ID`, .keep_all = TRUE )
  df_temp
  stop("\nFür den Film mit Suisanummer ", df_temp$Suisanummer, " am ", format(df_temp$Datum, "%d.%m.%Y"), " gibt es keinen Programmeintrag.\nBitte das Programm korrigieren!\n")
}

# remove no more needed variables
remove(ii,
       c_path, c_files
       )

# Abos und Kinogutscheine #########
if(!file.exists("Input/advance tickets/atelierkino_abo.txt")) {
  warning(paste0("\nDie Datei: \".../Input/advance tickets/atelierkino_abo.txt\" wurde nicht gefunden.",
                 "\nBitte herunterladen unter: https://www.advance-ticket.ch/abos?lang=de\n"))
}
df_atelierkino_abo <- read_delim("Input/advance tickets/atelierkino_abo.txt",
                                 delim = "\t", escape_double = FALSE,
                                 col_types = cols(creation = col_date(format = "%Y-%m-%d"),
                                                  first_use = col_date(format = "%Y-%m-%d"),
                                                  last_use = col_date(format = "%Y-%m-%d"),
                                                  expiration = col_date(format = "%Y-%m-%d"),
                                                  count_use = col_integer()), trim_ws = TRUE)

if(!file.exists("Input/advance tickets/atelierkino_foerderer.txt")) {
  warning(paste("\nDie Datei: .../Input/advance tickets/atelierkino_foerderer.txt wurde nicht gefunden.",
                "\nBitte herunterladen unter: https://www.advance-ticket.ch/abos?lang=de\n"))
}
df_atelierkino_foerderer <- read_delim("Input/advance tickets/atelierkino_foerderer.txt",
                                       delim = "\t", escape_double = FALSE,
                                       col_types = cols(creation = col_date(format = "%Y-%m-%d"),
                                                        first_use = col_date(format = "%Y-%m-%d"),
                                                        last_use = col_date(format = "%Y-%m-%d"),
                                                        expiration = col_date(format = "%Y-%m-%d"),
                                                        count_use = col_integer()), trim_ws = TRUE)

if(!file.exists("Input/advance tickets/atelierkino_gutschein.txt")) {
  warning(paste("Die Datei: .../Input/advance tickets/atelierkino_gutschein.txt wurde nicht gefunden.",
                "\nBitte herunterladen\nhttps://www.advance-ticket.ch/abos?lang=de\n"))
}
df_atelierkino_gutschein <- read_delim("Input/advance tickets/atelierkino_gutschein.txt",
                                       delim = "\t", escape_double = FALSE,
                                       col_types = cols(creation = col_date(format = "%Y-%m-%d"),
                                                        first_use = col_date(format = "%Y-%m-%d"),
                                                        last_use = col_date(format = "%Y-%m-%d"),
                                                        expiration = col_date(format = "%Y-%m-%d"),
                                                        amount = col_double(), count_use = col_integer()),
                                       trim_ws = TRUE)


# Verleiherabgaben einlesen ##################
df_temp <- l_data$Programm|>
  select(1:11,-`Link to Event ID`)|>
  left_join(l_data$Verleiher|>
              select(-ID),
            by = c("Verleiher" = "Verleihername"))
df_temp

# Suisa automatisch korrigieren
df_temp$Suisanummer <- df_temp$Suisanummer|>
  str_squish()|>
  str_extract(pattern = DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT)

# error handling
df_temp <- df_temp|>
  filter(is.na(Verleiher))

if(nrow(df_temp)>0){
  warning(paste0("\nEs gibt keinen Verleiher für den Film, ",df_temp$Filmtitel," am ",day(df_temp$Datum), ".", month(df_temp$Datum), ".", year(df_temp$Datum),".",
                 "\nBitte das Programm korrigieren!\n"))
}


# Abrechnung check ################

# Wie muss mit dem Verleiher abgerechnet werden? (Sind die Kinoförderer gratis?)
df_Abrechnung <- l_data$Programm|>
  select(1:11)|>
  left_join(l_data$Verleiher|>
              select(-ID, -`E-Mail`, -Adresse, -PLZ, -Ort),
            by = c(Verleiher = "Verleihername")
  )|>
  mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein", F, T))
df_Abrechnung

# Verleiherrechnung
df_Abrechnung <-
  df_Abrechnung|>
  left_join(Einnahmen_und_Ausgaben$Ausgaben|>
              filter(Kategorie == "Verleiher")|>
              select(1:7,-ID, -Datum, -Kategorie, -Firmennamen)|>
              rename(`Verleiherrechnungsbetrag [CHF]` = `Betrag [CHF]`),
            by = join_by( `Event ID`)
  )
df_Abrechnung


# paste0("\"",names(df_Abrechnung),"\"")|>
#   paste0(collapse = ",")|>
#   writeLines()

# error handling
df_temp <- df_Abrechnung|>
  filter(is.na(`Event ID`))|>
  slice(1)

if(nrow(df_temp) > 0){
  warning(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                 " wurde kein Programm eintrag gefunden.",
                 "\nBitte im Programm korrigieren!\n"
  )
  )
}

# Errorhandling
# kein prozentualer noch fixer abzug definiert
df_temp <- df_Abrechnung|>
  filter(is.na(`Abzug [%]`) & is.na(`Abzug fix [CHF]`),
         `Verleiher Angefragt?` != "Wird nicht gespielt")
df_temp

if(nrow(df_temp)>0){
  warning(paste0("\nFür den Film ID",df_temp$`Event ID`," / ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                 " wurde kein Abzug definiert.",
                 "\nBitte korrigieren im File:",
                 "\nBitte im Programm korrigieren!\n"
  )
  )
}

# kein minimal Abzug definiert (Es muss kein minimaler Abzug definiert werden falls ein Abzug definiert wurde)
df_temp <- df_Abrechnung|>
  filter(is.na(`Minimal Abzug [CHF]`) & !is.na(`Abzug [%]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0) warning(paste0("\nFür den Film ID ", df_temp$`Event ID`," / ", df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                                   "\nwurde werder ein Minimalabzug noch ein Fixabzug definiert.",
                                   "\nBitte im Programm korrigieren!\n"
)
)

# Prozentualer und Fixer Abzug definiert
df_temp <- df_Abrechnung|>
  filter(!is.na(`Abzug [%]`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){
  warning(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                 "\nwurde ein Prozentualer und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
                 "\nBitte im Programm korrigieren!\n"
  )
  )
}

# minimal und Fixer Abzug definiert
df_temp <- df_Abrechnung|>
  filter(!is.na(`Minimal Abzug [CHF]`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){
  warning(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                 "\nwurde ein minimal Abzug und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
                 "\nBitte im Programm korrigieren!\n"
  )
  )
}

# error handling
# Verleiherrechnungbetrag ist kleiner als minimaler Abzug.
df_temp <- df_Abrechnung|>
  mutate(`Minimal Abzug unterschritten` = `Minimal Abzug [CHF]`> `Verleiherrechnungsbetrag [CHF]`,
         `Minimal Abzug unterschritten` = if_else(is.na(`Minimal Abzug unterschritten`), F, `Minimal Abzug unterschritten`)
  )|>
  filter(`Minimal Abzug unterschritten`)

# error handling, keine Verleiherrechnung
if(nrow(df_temp) > 0) {
  warning(paste0("\nAchtung für den Film ID ", df_temp$`Event ID`, " / ", df_temp$Filmtitel," am ", day(df_temp$Datum),".",month(df_temp$Datum),".", lubridate::year(df_temp$Datum),
                 "\nist der Verleiherrechnungsbetrag ",df_temp$`Verleiherrechnungsbetrag [CHF]`,"[CHF] kleiner als die Mindestgarantie ",df_temp$`Minimal Abzug [CHF]`,"[CHF].",
                 "\nBitte im Programm korrigieren!\n"
  )
  )
}

# error handling Verleiherrechnung nicht vorhanden
df_temp <- df_Abrechnung|>
  filter(is.na(`Verleiherrechnungsbetrag [CHF]`))
df_temp

# Error handling: Keine Verleiherrechnung vorhanden
warning(paste0("\nAchtung für den Film ID ",df_temp$`Event ID`," / ", df_temp$Filmtitel," am ", format(df_temp$Datum, "%d.%m.%Y"),
               "\nmit der Suisanummer ", df_temp$Suisanummer,
               " gibt es keine Verleiherrechnung.",
               "\nBitte in den Ausgaben, Kategorie Verleiher korrigieren.\n"))

# Programm check ####
df_Film <- l_data$Programm|>
  group_by(Suisanummer)|>
  reframe(n())|>
  left_join(l_data$Programm|>
              distinct(Suisanummer, .keep_all = TRUE)|>
              select(Suisanummer, Filmtitel)
            ,
            by = join_by(Suisanummer)
  )

df_Film

ii <- "1020.828"
for (ii in df_Film$Suisanummer) {
  df_temp <- l_data$Programm|>
    filter(Suisanummer == ii)
  df_temp
  
  # check for same date
  if(length(df_temp|>distinct(Datum)|>pull()) != nrow(df_temp)){
    
    c_Dates <- df_temp|>distinct(Datum)|>pull()
    jj <- "2025-01-10"
    for (jj in c_Dates) {
      # check for same time
      if(nrow(df_temp|>filter(Datum == jj)) == nrow(df_temp|>filter(Datum == jj)|>distinct(Datum, Zeit))){
        temp <- df_temp|>filter(Datum == jj)
        stop(paste("\nFür den Film ",temp$Suisanummer[1], temp$Filmtitel[1],
                   "\ngibt es mehrere Vorstellungen mit dem gleichen Datum", paste0(temp$Datum, collapse = ", ") , "und Zeit", paste0(temp$Zeit, collapse = ", "),
                   "\nBitte im Programm korrigieren"
        )
        )
      }
    }
  }
}
remove(df_Film)

#  Ticketabrechnung vorbereiten ##################

df_Abrechnung <- df_Abrechnung|>
  filter(Datum <= Sys.Date(),
         `Verleiher Angefragt?` == "Bestätigt") # Nur Filme abrechnen welche bereits vorgeführt wurden
df_Abrechnung <- df_Abrechnung|>
  select(-`Verleiher Angefragt?`,-Bezeichnung)


## Je nach Verleiher müssen die Kinoförderer als Umsatz abgerechnet werden. #####
df_Tickets <- df_Eintritt|>
  left_join(df_Abrechnung,
            by = "Event ID"
  )|>
  select(-Filmtitel.y, -Suisanummer.y, -Datum.y)|>
  rename(Filmtitel = Filmtitel.x,
         Suisanummer = Suisanummer.x,
         Datum = Datum.x
  )|>
  mutate(
    `Verkaufspreis für Netto3 [CHF]` =
      if_else(((Platzkategorie %in% l_data$`Platzkategorien zum Verrechnen`$Kinoförderer) & (!`Kinoförderer gratis?`)),
              l_data$`Platzkategorien zum Verrechnen`$Verkaufspreis[1],
              Verkaufspreis
      ),
    `Umsatz für Netto3 [CHF]` = Anzahl * `Verkaufspreis für Netto3 [CHF]`)|>
  arrange(desc(Datum))|>
  select(c("Event ID","Link to Event ID", "Datum", "Zeit","Suisanummer","Filmtitel",
           "Platzkategorie","Zahlend","Verkaufspreis","Verkaufspreis für Netto3 [CHF]","Anzahl","Umsatz [CHF]","Umsatz für Netto3 [CHF]",
           "SUISA-Vorabzug [%]",
           "Verleiher",
           "Abzug [%]","Minimal Abzug [CHF]","Abzug fix [CHF]","Kinoförderer gratis?",
           "Verleiherrechnungsbetrag [CHF]"
           )
         )|>
  arrange(Datum)


# Umsatz aus Tickets zu Abrechnung hinzufügen ####
df_temp <- df_Tickets|>
  group_by(`Event ID`)|>
  reframe(`Umsatz [CHF]` = sum(`Umsatz [CHF]`,na.rm = T),
          `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`, na.rm = T))
df_temp

df_temp|>
  filter(`Umsatz [CHF]` != `Umsatz für Netto3 [CHF]`)

df_Abrechnung <-left_join(df_Abrechnung, 
                          df_temp,
                          by = join_by(`Event ID`)
)

# Suisavorabzug der Abrechnung hinzufügen ####
df_temp <- df_Tickets|>
  group_by(`Event ID`)|>
  distinct(`SUISA-Vorabzug [%]`)

df_Abrechnung <-left_join(df_Abrechnung, 
                          df_temp,
                          by = join_by(`Event ID`)
)
remove(df_Tickets)


# Umsatz und Verleiherabzug MWST und Ticketgewinn #####
names(df_Abrechnung)

df_Abrechnung <- df_Abrechnung|>
  mutate(`Suisavorabzug [CHF]` = `Umsatz für Netto3 [CHF]` * (`SUISA-Vorabzug [%]` / 100),
         `Umsatz Netto 3 [CHF]` =  `Umsatz für Netto3 [CHF]` - `Suisavorabzug [CHF]`,
         `MWST [CHF]` = if_else(
           is.na(`Verleiherrechnungsbetrag [CHF]`),
           `Umsatz für Netto3 [CHF]` * (l_data$MWST$MWST / 100),
           `Verleiherrechnungsbetrag [CHF]` / (1 + (l_data$MWST$MWST / 100)) 
         ),
         `Verleiherabzug [CHF]` = 
           if_else(is.na(`Abzug fix [CHF]`),
                   (`Umsatz Netto 3 [CHF]` * (`Abzug [%]` / 100)) + `MWST [CHF]`,
                   `Umsatz für Netto3 [CHF]` - `Abzug fix [CHF]`
           ),
         # Verleiherrechnung verwenden falls vorhanden
         `Verleiherabzug [CHF]` = 
           if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                   `Verleiherabzug [CHF]`,
                   `Verleiherrechnungsbetrag [CHF]`
           ),
         `Ticketgewinn [CHF]` = `Umsatz [CHF]` - `Verleiherabzug [CHF]`
  )

df_Abrechnung|>
  select(1:3, `Umsatz [CHF]`,`Verleiherrechnungsbetrag [CHF]`, 15:ncol(df_Abrechnung))


# Kioskgewinn der Abrechnung hinzufügen 
df_temp <- df_Kiosk|>
  group_by(`Event ID`)|>
  reframe(`Kioskgewinn [CHF]` = sum(`Gewinn [CHF]`, na.rm = T))
df_temp  

df_Abrechnung <- left_join(df_Abrechnung, 
                           df_temp,
                           by = join_by(`Event ID`)
)  
df_Abrechnung

# Manko / Überschuss Kasse der Abrechnung hinzufügen ####
df_Abrechnung <- left_join(df_Abrechnung, 
                           df_manko_uerberschuss,
                           by = join_by(`Event ID`)
                           )  
df_Abrechnung

# Eventeinnahmen der Abrechnung hinzufügen ####
df_temp <- l_data$Einnahmen|>
  filter(Kategorie == "Event")|>
  mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
  group_by(`Event ID`)|>
  reframe( `Eventeinnahmen [CHF]` = sum(`Betrag [CHF]`, na.rm = T))

df_Abrechnung <- left_join(df_Abrechnung, 
                           df_temp,
                           by = join_by(`Event ID`)
)  

# Eventausgaben der Abrechnung hinzufügen ####
df_temp <- l_data$Ausgaben|>
  filter(Kategorie == "Event")|>
  mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
  group_by(`Event ID`)|>
  reframe( `Eventausgaben [CHF]` = sum(`Betrag [CHF]`, na.rm = T))
df_temp
df_Abrechnung <- left_join(df_Abrechnung, 
                           df_temp,
                           by = join_by(`Event ID`)
)

# Gewinn aus Filmvorführungen ####
df_temp <- df_Abrechnung|>
  group_by(`Event ID`)|>
  reframe(`Gewinn aus Fimvorführung [CHF]` = 
            sum(`Ticketgewinn [CHF]`,`Kioskgewinn [CHF]`,`Überschuss / Manko [CHF]`, `Eventeinnahmen [CHF]`, -`Eventausgaben [CHF]`,
                na.rm = T)
  )
df_temp
df_Abrechnung <- left_join(df_Abrechnung, df_temp, by = join_by(`Event ID`))

# Gemeinsame Verleiherabrechnung über mehrere Event IDs ####
df_mapping <- df_Abrechnung|>
  select(1:6)|>
  mutate(`Link to Event ID` = as.character(`Link to Event ID`)|>as.integer())
df_mapping

# find all connected Filmvorführungen from Programm and remove all already connected
l_abrechnung <- inspect_link_ids(df_mapping)

l_gemeinsame_Abrechnung_IDs <- l_abrechnung
l_gemeinsame_Abrechnung_IDs
l_abrechnung[[1]]

ID <- 1
cnt <- 1
for (ID in names(l_abrechnung)) {
  # Event ID`s 
  IDs <- l_abrechnung[[ID]]
  
  ## Gemeinsame Abrechnung ####
  Gemeinsame_Abrechnung <- df_Abrechnung|>
    filter(`Event ID` %in% IDs)
  Gemeinsame_Abrechnung
  
  ## Eintritte ####
  Eintritte <- df_Eintritt|> 
    select(- `SUISA-Vorabzug [%]`)|>
    filter(`Event ID` %in% IDs)|>
    select(`Event ID`,Platzkategorie, Verkaufspreis, Anzahl, `Umsatz [CHF]`)
  
  
  ## Summary Eintritte  ####
  Eintritte <- Eintritte|>
    group_by(Platzkategorie)|>
    reframe(Anzahl = sum(Anzahl),
            `Umsatz [CHF]` = sum(`Umsatz [CHF]`))|>
    left_join(Eintritte|>
                distinct(Platzkategorie, .keep_all = TRUE)|>
                select(-Anzahl, - `Umsatz [CHF]`),
              by = join_by(Platzkategorie)
    )|>
    select("Platzkategorie", "Anzahl", "Verkaufspreis", "Umsatz [CHF]")|>
    rename(`Verkaufspreis [CHF]` = Verkaufspreis)
  
  # return ####
  l_abrechnung[[cnt]] <- 
    list(
      Gemeinsame_Abrechnung = Gemeinsame_Abrechnung,
      Eintritte = Eintritte
    )
  cnt <- cnt + 1
}
remove(Eintritte, 
       df_mapping, df_temp
)
l_abrechnung
l_abrechnung[["1"]]

#  Gemeinsame Abrechnung erstellen ####
## Abrechnung Tickets erstellen (für Berichte verwendet) ####
Gemeinsame_Abrechnung <- l_abrechnung|>
  lapply(function(x){
    x$Gemeinsame_Abrechnung
  })|>
  bind_rows()|>
  mutate(`Link to Event ID` = as.integer(`Link to Event ID`))
Gemeinsame_Abrechnung

## Abrechnung Tickets erstellen (für Berichte verwendet) ####
Gemeinsame_Abrechnung_tickes <- l_abrechnung|>
  lapply(function(x){
    x$Eintritte
  })|>
  bind_rows()
Gemeinsame_Abrechnung_tickes

# Daten für Berichet #### 
## Abrechnung ####
df_Abrechnung <- df_Abrechnung|>
  mutate(`Gewinn aus Fimvorführung [CHF]` = round5Rappen(`Gewinn aus Fimvorführung [CHF]`))

## Besucherzahlen  ####
df_Besucherzahlen <- df_Eintritt|>
  group_by(`Event ID`,Datum, Filmtitel, Suisanummer)|>
  reframe(Besucher = sum(Anzahl))
df_Besucherzahlen

## Eventeinnahmen ####
df_Eventeinnahmen <- DB_get_table("Einnahmen", con)|>
  convert_to_template_types(l_template$Einnahmen)|>
  filter(Kategorie == "Event")

## Eventausgaben ####
df_Eventausgaben <- DB_get_table("Ausgaben", con)|>
  convert_to_template_types(l_template$Ausgaben)|>
  filter(Kategorie == "Event")

## Keine Rechnung vorhanden ####
df_keine_Rechnung <- DB_get_table("Ausgaben", con)|>
  convert_to_template_types(l_template$Ausgaben)|>
  filter(is.na(`Betrag [CHF]`))

# Data export: write to Excel ####
c_filePath <- "output/data/"
if(!dir.exists(c_filePath)) dir.create(c_filePath, recursive = T)

list(`Werbung` = df_Besucherzahlen,
     `Tickets` = df_Eintritt,
     `Kiosk` = df_Kiosk,
     `Eventeinnahmen` = df_Eventeinnahmen,
     `Eventausgaben` = df_Eventausgaben,
     `Filmvorführung` = df_Abrechnung
)|>
  write.xlsx(file="output/data/Auswertung.xlsx", asTable = TRUE, overwrite = TRUE)


# remove not used variables ####
remove(ii,
       c_filePath
)

# Data for GUI
l_data <- DB_get_Data(l_template, con)|>
  convert_DB_to_R(l_template)

# Disconnect from DB ####
dbDisconnect(con)

# user interaction ####
writeLines("Good ... Berechnungen erfolgt")

l_abrechnung$`10`
