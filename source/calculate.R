# read and caluclate all input data
library(rebus)
# library(openxlsx)
library(lubridate)
library(tidyverse)

writeLines("Daten werden einlesen und berechnet...")

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


# Add connection validation at start
if(!dbIsValid(con)) {
  warning("Connection lost in DB_get_table(), attempting to reconnect...")
  con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)
}

# This is used to run the code on its own
# However this variable c_Abrechnungsjahr will be inported to the data_env$c_Abrechnungsjahr by the GUI
if(!r_is.defined(c_Abrechnungsjahr)){
  c_Abrechnungsjahr <- 2024L
}

# load data from Database ####
## Programm ####
Programm <- DB_get_table("Programm", con, download = FALSE)|>
  filter(`Verleiher Angefragt?` == "Bestätigt")|>
  collect()|>
  convert_to_template_types(l_template$Programm)|>
  filter(lubridate::year(Datum) == c_Abrechnungsjahr)
Programm

## df_Eintritt ####
df_Eintritt <- DB_get_table("df_Eintritt", con)|>
  convert_to_template_types(l_template$df_Eintritt)|>
  filter(lubridate::year(Datum) == c_Abrechnungsjahr)
df_Eintritt

##  df_Kiosk ####
df_Kiosk <- tbl(con, "df_Kiosk")|>
  left_join(tbl(con, "Programm")|>
              select(`Event ID`, Datum, Filmtitel),
            by = join_by(`Event ID`)
            )|>
  filter(c_Abrechnungsjahr == lubridate::year(Datum))|>
  select(-Datum)|>
  collect()
df_Kiosk

# Add connection validation at start
if(!dbIsValid(con)) {
  warning("Connection lost in DB_get_table(), attempting to reconnect...")
  con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)
}

## Einnahmen ####
Einnahmen <- DB_get_table("Einnahmen", con)|>
  convert_to_template_types(l_template$Einnahmen)|>
  filter(Abrechnungsjahr == c_Abrechnungsjahr)

## Ausgaben ####
Ausgaben <- DB_get_table("Ausgaben", con)|>
  convert_to_template_types(l_template$Ausgaben)|>
  filter(Abrechnungsjahr == c_Abrechnungsjahr)|>
  arrange(desc(Datum))
Ausgaben

## `Einkauf Kiosk` ####
`Einkauf Kiosk` <- DB_get_table("Einkauf Kiosk",con)|>
  convert_to_template_types(l_template$`Einkauf Kiosk` )

## df_Spezialpreise ####
df_Spezialpreisekiosk <- DB_get_table("Spezialpreisekiosk",con)|>
  convert_to_template_types(l_template$Spezialpreisekiosk )|>
  mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
  filter(`Event ID` %in% Programm$`Event ID`)

## Verleiher ####
Verleiher <- DB_get_table("Verleiher",con)|>
  convert_to_template_types(l_template$Verleiher)

# Add connection validation at start
if(!dbIsValid(con)) {
  warning("Connection lost in DB_get_table(), attempting to reconnect...")
  con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)
}

## Lieferant ####
Lieferant <- DB_get_table("Lieferanten",con)|>
  convert_to_template_types(l_template$Lieferanten)

## `Platzkategorien zum Verrechnen` ####
`Platzkategorien zum Verrechnen` <- DB_get_table("Platzkategorien zum Verrechnen",con)|>
  convert_to_template_types(l_template$`Platzkategorien zum Verrechnen`)

## MWST ###
MWST <- DB_get_table("MWST",con)|>
  convert_to_template_types(l_template$MWST)

MWST <- MWST|>
  filter(Abrechnungsjahr == c_Abrechnungsjahr)
if(nrow(MWST) == 1){
  c_MWST  <- MWST|>
    filter(Abrechnungsjahr == c_Abrechnungsjahr)|>
    select(MWST)|>
    pull()
} else stop("Die Mehrwertsteuer konnte für das Abrechnungsjahr ",c_Abrechnungsjahr, " nicht gefunden werden.")

## Kiosk files ####
c_eintritt <- tbl(con, "Eintritt files")|>
  # filter(ID %in% Programm$`Event ID`)|>
  distinct(ID, .keep_all = TRUE)|>
  arrange(ID)|>
  select(filename)|>
  pull()
c_eintritt

## Eintritt files ####
c_Kiosk <- tbl(con, "Kiosk files")|>
  # filter(ID %in% Programm$`Event ID`)|>
  distinct(ID, .keep_all = TRUE)|>
  arrange(ID)|>
  select(filename)|>
  pull()
c_Kiosk

# check nb of files Eintritt vs Kiosk ####

# find file ID (regex)
p <- "([\\d]+)\\.txt$"

# find file that needs to be upload to database ####
if(length(c_eintritt) != length(c_Kiosk)) {
  if(length(c_eintritt) > length(c_Kiosk)){
    c_temp <- c_Kiosk[!(as.integer(str_match(c_eintritt, p)[,2]) %in% as.integer(c_Kiosk, str_match(, p)[,2]))]
    c_temp <- paste0("Kiosk ID",str_match(c_temp, p)[,2], ".txt")
    warning("\nEs gibt ", length(c_eintritt), " Eintrittsdateien aber nur ", length(c_Kiosk), " Kioskdateien.",
            "Bitte die fehlende Datei: `", c_temp, "` hochladen\n"
            )
  }else {
    c_temp <- c_Kiosk[!(as.integer(str_match(c_Kiosk, p)[,2]) %in% as.integer(str_match(c_eintritt, p)[,2]))]
    c_temp <- paste0("Eintritt ID",str_match(c_temp, p)[,2], ".txt")
    warning("\nEs gibt ", length(c_Kiosk), "  Kioskdateien aber nur ", length(c_eintritt), " Eintrittsdateien. ",
            "Bitte die fehlende Datei: `", c_temp, "` hochladen\n")
  }
}


# check Programm ####
df_temp <- Programm|>
  convert_to_template_types(l_template$Programm)
df_temp

if(nrow(df_temp) != nrow(Programm)){
  df_temp <- anti_join(Programm,
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

## check suisanummer  ####
p <- or(DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT,
        WRD%R%WRD%R%WRD%R%WRD%R%DOT%R%WRD%R%WRD%R%WRD
)

df_temp <- Programm|>
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


## check Programm ####
df_Film <- Programm|>
  group_by(Suisanummer)|>
  reframe(n())|>
  left_join(Programm|>
              distinct(Suisanummer, .keep_all = TRUE)|>
              select(Suisanummer, Filmtitel)
            ,
            by = join_by(Suisanummer)
  )

df_Film

ii <- "1020.828"
for (ii in df_Film$Suisanummer) {
  df_temp <- Programm|>
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

# Überschuss / Manko ####
df_manko_uerberschuss <- df_Kiosk|>
  distinct(`Event ID`,.keep_all = TRUE)|>
  select(`Event ID`, `Überschuss / Manko [CHF]`)
df_manko_uerberschuss

# Spezialpreise abgleichen (df_Kiosk) ####
df_spez_preis_na <- df_Kiosk|>
  filter(is.na(ID_Spezialpreisekiosk) & is.na(ID_Kioskartikel ))
df_spez_preis_na

c_Event_IDs <- distinct(df_spez_preis_na, `Event ID`)|>pull()

# update df_Kiosk mit Spezialpreisen
for (ii in c_Event_IDs) {
  df_temp <- df_Spezialpreisekiosk|>
    filter(`Event ID` == ii)
  df_temp

  if(nrow(df_temp) > 0){
    for (jj in 1:nrow(df_temp)) {
      df_temp1 <- df_temp[jj,]
      df_temp1
      
      # Add connection validation at start
      if(!dbIsValid(con)) {
        warning("Connection lost in DB_get_table(), attempting to reconnect...")
        con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)
      }
      
      df_temp2 <- DB_get_table("df_Kiosk",con, download = FALSE)|>
        filter(`Event ID` == ii)|>
        collect()
      
      df_temp3 <- df_temp2|>
        select(-Artikelname, -ID_Spezialpreisekiosk)|>
        left_join(
          df_temp1|>
            rename(ID_Spezialpreisekiosk = ID), 
          by = c("Event ID", `Artikel-Kassensystem` = "Spezialpreis")
        )|>
        filter(!is.na(ID_Spezialpreisekiosk))|>
        select("ID", "Event ID", "ID_Spezialpreisekiosk", "ID_Kioskartikel", 
               "Artikel-Kassensystem", "Artikelname", "Einzelpreis [CHF]", "Anzahl", "Betrag [CHF]", "Gewinn [CHF]", "Überschuss / Manko [CHF]", 
               "Verkaufspreis [CHF]", "Einkaufspreis [CHF]", "Menge", "Lieferant", "Gültig ab Datum")
      df_temp3
      
      if(!is.na(df_temp3$ID_Spezialpreisekiosk)){
        # update df_Kiosk
        DB_edit_row_in_table(con, "df_Kiosk", "ID", df_temp3$ID, df_temp3, get_data_type(df_temp3))
      }
    }
  }
}

##  df_Kiosk ####
df_Kiosk <- tbl(con, "df_Kiosk")|>
  left_join(tbl(con, "Programm")|>
              select(`Event ID`, Datum, Filmtitel),
            by = join_by(`Event ID`)
  )|>
  filter(c_Abrechnungsjahr == lubridate::year(Datum))|>
  select(-Datum)|>
  collect()
df_Kiosk

# check Spezialpreise ####
df_spez_preis_na <- df_Kiosk|>
  filter(is.na(ID_Spezialpreisekiosk) & is.na(ID_Kioskartikel ))
df_spez_preis_na

## Disconnect from database ####
DBI::dbDisconnect(con)

if(nrow(df_spez_preis_na) > 0){
  warning(
    c("\n",
      paste0("Für `Event ID`= ", df_spez_preis_na$`Event ID`, 
             ", `", df_spez_preis_na$Filmtitel, "`, ist der Spezialpreis `", 
             df_spez_preis_na$`Artikel-Kassensystem`,"` nicht  noch nicht definiet worden.",
             collapse = "\n"
             ),
      "\n")
    )
}

# Abos und Kinogutscheine ####
## Kino-Abo ####
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

## Kinoförderer ####
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
## Kinogutscheine ####
df_atelierkino_gutschein <- read_delim("Input/advance tickets/atelierkino_gutschein.txt",
                                       delim = "\t", escape_double = FALSE,
                                       col_types = cols(creation = col_date(format = "%Y-%m-%d"),
                                                        first_use = col_date(format = "%Y-%m-%d"),
                                                        last_use = col_date(format = "%Y-%m-%d"),
                                                        expiration = col_date(format = "%Y-%m-%d"),
                                                        amount = col_double(), count_use = col_integer()),
                                       trim_ws = TRUE)


# check Verleiher in Programm ####
df_temp <- Programm|>
  select(1:11,-`Link to Event ID`)|>
  left_join(Verleiher|>
              select(-ID),
            by = c("Verleiher" = "Verleihername"))
df_temp

# Suisa automatisch korrigieren ####
df_temp$Suisanummer <- df_temp$Suisanummer|>
  str_squish()|>
  str_extract(pattern = DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT)

## error handling ####
df_temp <- df_temp|>
  filter(is.na(Verleiher))

if(nrow(df_temp)>0){
  warning(paste0("\nEs gibt keinen Verleiher für den Film, ",df_temp$Filmtitel," am ",day(df_temp$Datum), ".", month(df_temp$Datum), ".", year(df_temp$Datum),".",
                 "\nBitte das Programm korrigieren!\n"))
}


# Abrechnung erstellen ####
## Wie muss mit dem Verleiher abgerechnet werden? (Sind die Kinoförderer gratis?) ####
c_EventIDs_Eintritte <- df_Eintritt|>
  distinct(`Event ID`,.keep_all = TRUE)|>
  select(`Event ID`)|>
  pull()

df_Abrechnung <- Programm|>
  filter(`Event ID` %in% c_EventIDs_Eintritte)|>
  left_join(Verleiher|>
              select(-ID, -Kontakt,-Besucherzahlen , -Adresse, -PLZ, -Ort),
            by = c(Verleiher = "Verleihername")
  )|>
  mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein", F, T))
df_Abrechnung|>
  tail()

## Verleiherrechnung ####
df_Abrechnung <-
  df_Abrechnung|>
  left_join(Ausgaben|>
              mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
              filter(Kategorie == "Verleiher")|>
              select(1:7,-ID, -Datum, -Kategorie, -Firmennamen)|>
              rename(`Verleiherrechnungsbetrag [CHF]` = `Betrag [CHF]`),
            by = join_by( `Event ID`)
  )
df_Abrechnung

## Error handling ####
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


### kein prozentualer noch fixer abzug definiert ####
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

### kein minimal Abzug definiert (Es muss kein minimaler Abzug definiert werden falls ein Abzug definiert wurde) ####
df_temp <- df_Abrechnung|>
  filter(is.na(`Minimal Abzug [CHF]`) & !is.na(`Abzug [%]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0) warning(paste0("\nFür den Film ID ", df_temp$`Event ID`," / ", df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                                   "\nwurde werder ein Minimalabzug noch ein Fixabzug definiert.",
                                   "\nBitte im Programm korrigieren!\n"
)
)

### Prozentualer und Fixer Abzug definiert ####
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

### minimal und Fixer Abzug definiert ####
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

### Verleiherrechnungbetrag ist kleiner als minimaler Abzug. ####
df_temp <- df_Abrechnung|>
  mutate(`Minimal Abzug unterschritten` = `Minimal Abzug [CHF]`> `Verleiherrechnungsbetrag [CHF]`,
         `Minimal Abzug unterschritten` = if_else(is.na(`Minimal Abzug unterschritten`), F, `Minimal Abzug unterschritten`)
  )|>
  filter(`Minimal Abzug unterschritten`)

if(nrow(df_temp) > 0) {
  warning(paste0("\nAchtung für den Film ID ", df_temp$`Event ID`, " / ", df_temp$Filmtitel," am ", day(df_temp$Datum),".",month(df_temp$Datum),".", lubridate::year(df_temp$Datum),
                 "\nist der Verleiherrechnungsbetrag ",df_temp$`Verleiherrechnungsbetrag [CHF]`,"[CHF] kleiner als die Mindestgarantie ",df_temp$`Minimal Abzug [CHF]`,"[CHF].",
                 "\nBitte im Programm korrigieren!\n"
  )
  )
}


##  Abrechnung Ticket ####
df_Abrechnung <- df_Abrechnung|>
  filter(`Verleiher Angefragt?` == "Bestätigt") # Nur Filme abrechnen welche bereits vorgeführt wurden
df_Abrechnung <- df_Abrechnung|>
  select(-`Verleiher Angefragt?`,-Bezeichnung)


### Je nach Verleiher müssen die Kinoförderer als Umsatz abgerechnet werden. #####
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
      if_else(((Platzkategorie %in% `Platzkategorien zum Verrechnen`$Kinoförderer) & (!`Kinoförderer gratis?`)),
              `Platzkategorien zum Verrechnen`$Verkaufspreis[1],
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


### Umsatz aus Tickets zu Abrechnung hinzufügen ####
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

### Suisavorabzug der Abrechnung hinzufügen ####
df_temp <- df_Tickets|>
  group_by(`Event ID`)|>
  distinct(`SUISA-Vorabzug [%]`)

df_Abrechnung <-left_join(df_Abrechnung, 
                          df_temp,
                          by = join_by(`Event ID`)
)


### Berechnen von Umsatz und Verleiherabzug MWST und Ticketgewinn #####
names(df_Abrechnung)

df_Abrechnung <- df_Abrechnung|>
  mutate(`Suisavorabzug [CHF]` = `Umsatz für Netto3 [CHF]` * (`SUISA-Vorabzug [%]` / 100),
         `Umsatz Netto 3 [CHF]` =  `Umsatz für Netto3 [CHF]` - `Suisavorabzug [CHF]`,
         # Abzug fix?
         `Verleiherabzug [CHF]` = 
           if_else(is.na(`Abzug fix [CHF]`),
                   `Umsatz Netto 3 [CHF]` * (`Abzug [%]` / 100),  
                   `Umsatz Netto 3 [CHF]` - `Abzug fix [CHF]`
           ),
         `Verleiherabzug [CHF]` = 
           if_else(
             is.na(`Verleiherrechnungsbetrag [CHF]`),
             `Verleiherabzug [CHF]`,
             `Verleiherrechnungsbetrag [CHF]`
           ),
         `MWST [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                `Verleiherabzug [CHF]` * (c_MWST / 100),
                                `Verleiherabzug [CHF]` - (`Verleiherabzug [CHF]` / (1 + (c_MWST / 100)))
         ),
         `Verleiherabzug [CHF]` = 
           if_else(
             is.na(`Verleiherrechnungsbetrag [CHF]`),
             `Verleiherabzug [CHF]` + `MWST [CHF]`,
             `Verleiherrechnungsbetrag [CHF]`
           ),
         `Ticketgewinn [CHF]` = `Umsatz [CHF]` - `Verleiherabzug [CHF]`
  )

df_Abrechnung|>
  select(-`Kinoförderer gratis?`, -Abrechnungsjahr, -Procinema,-Trailer,-`Besucherzahlen an Verleiher gesendet`,
         -Verleiher,-`Link to Event ID`, -Datum, -Zeit, -`Rechnung bezahlt und abgelegt`)

### Kioskgewinn der Abrechnung hinzufügen ####
df_temp <- df_Kiosk|>
  group_by(`Event ID`)|>
  reframe(`Kioskgewinn [CHF]` = sum(`Gewinn [CHF]`, na.rm = T))
df_temp  

df_Abrechnung <- left_join(df_Abrechnung, 
                           df_temp,
                           by = join_by(`Event ID`)
)  
df_Abrechnung

df_Abrechnung|>
  select(1:3, `Umsatz [CHF]`, 18:ncol(df_Abrechnung))


### Manko / Überschuss Kasse der Abrechnung hinzufügen ####
df_Abrechnung <- left_join(df_Abrechnung, 
                           df_manko_uerberschuss,
                           by = join_by(`Event ID`)
                           )  
df_Abrechnung
df_Abrechnung|>
  select(1:3, `Umsatz [CHF]`, 20:ncol(df_Abrechnung))


### Eventeinnahmen der Abrechnung hinzufügen ####
df_temp <- Einnahmen|>
  filter(Kategorie == "Event")|>
  mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
  group_by(`Event ID`)|>
  reframe( `Eventeinnahmen [CHF]` = sum(`Betrag [CHF]`, na.rm = T))

df_Abrechnung <- left_join(df_Abrechnung, 
                           df_temp,
                           by = join_by(`Event ID`)
)  
df_Abrechnung|>
  select(1:3, `Umsatz [CHF]`, 20:ncol(df_Abrechnung))


### Eventausgaben der Abrechnung hinzufügen ####
df_temp <- Ausgaben|>
  filter(Kategorie == "Event")|>
  mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
  group_by(`Event ID`)|>
  reframe( `Eventausgaben [CHF]` = sum(`Betrag [CHF]`, na.rm = T))
df_temp
df_Abrechnung <- left_join(df_Abrechnung, 
                           df_temp,
                           by = join_by(`Event ID`)
)
df_Abrechnung|>
  select(1:3, `Umsatz [CHF]`, 20:ncol(df_Abrechnung))

### Gewinn aus Filmvorführungen ####
df_temp <- df_Abrechnung|>
  group_by(`Event ID`)|>
  reframe(`Gewinn aus Fimvorführung [CHF]` = 
            sum(`Ticketgewinn [CHF]`,`Kioskgewinn [CHF]`,`Überschuss / Manko [CHF]`, `Eventeinnahmen [CHF]`, -`Eventausgaben [CHF]`,
                na.rm = T)
  )
df_temp
df_Abrechnung <- left_join(df_Abrechnung, df_temp, by = join_by(`Event ID`))

df_Abrechnung|>
  select(1:3, `Umsatz [CHF]`, 20:ncol(df_Abrechnung))

# Gemeinsame Verleiherabrechnung (Link to Event IDs vorhanden) ####
IDs <- df_Eintritt|>
  distinct(`Event ID`)|>
  pull()
  
df_mapping <- df_Abrechnung|>
  filter(`Event ID` %in% IDs)|>
  select(1:6)|>
  mutate(`Link to Event ID` = as.character(`Link to Event ID`)|>as.integer())
df_mapping

## find all connected Filmvorführungen from Programm and remove all already connected ####
l_abrechnung <- inspect_link_ids(df_mapping)
l_abrechnung
ii <- 1
while (TRUE) {
  if(length(l_abrechnung) < ii ) break
  if(length(l_abrechnung[[ii]]) > 1){
    for (jj in 2:length(l_abrechnung[[ii]])) {
      l_abrechnung[[paste0(l_abrechnung[[ii]][jj])]] <- NULL
    }
  }
  ii <- ii +1
}
l_abrechnung

# Gemeinsame Abrechnung erstellen ####
ID <- "35"
cnt <- 1
for (ID in names(l_abrechnung)) {
  ## Event ID`s ####
  IDs <- l_abrechnung[[ID]]

  ## Eintritte ####
  Eintritte <- df_Eintritt|> 
    select(- `SUISA-Vorabzug [%]`)|>
    filter(`Event ID` %in% IDs)|>
    select(`Event ID`,Platzkategorie, Verkaufspreis, Anzahl, `Umsatz [CHF]`)
  Eintritte

  ## s_Eintritte ####
  s_Eintritte <- 
    bind_rows(
      Eintritte|>
        filter(`Umsatz [CHF]` != 0)|>
        mutate(Zahlend = TRUE),
      Eintritte|>
        filter(`Umsatz [CHF]` == 0)|>
        mutate(Zahlend = FALSE)
      )|>
    group_by(Zahlend, `Event ID`)|>
    reframe(Besucherzahl = sum(Anzahl),
            `Umsatz [CHF]` = sum(`Umsatz [CHF]`),
            )|>
    arrange(desc(Zahlend))
  s_Eintritte
  
  # keine Verleiherrechnung vorhanden für gemeinsame Abrechnung
  df_temp <- df_Abrechnung|>
    filter(`Event ID` %in% IDs)|>
    select(1:6, `Verleiherrechnungsbetrag [CHF]`)|>
    reframe(`Verleiherrechnungsbetrag [CHF]` = sum(`Verleiherrechnungsbetrag [CHF]`, na.rm = TRUE))
  df_temp
  
  df_temp <- bind_cols(
    df_Abrechnung|>
      filter(`Event ID` %in% IDs[1])|>
      select(1:6),
    df_temp
  )
  
  if(df_temp$`Verleiherrechnungsbetrag [CHF]` == 0){
    warning(
      paste0("\n******\n",
        paste0(
          "Für `Event ID` = ", df_temp$`Event ID`, ", ", df_temp$Filmtitel, ", gibt es keine Verleiherrechnung.",
          "\nBitte in der Tabelle: `Ausgaben`, Kategorie: `Verleiher` korrigieren.\n", collapse = "\n"
          )
        )
      )
  }
  
  ## Verleiherrechnung ###
  Verleiherrechnung <- Ausgaben|>
    filter(Kategorie == "Verleiher", 
           `Event ID` %in% IDs)
  Verleiherrechnung
  
  ## Eventeinnahmen ####
  event_einnahmen <- Einnahmen|>
    filter(`Event ID` %in% IDs, Kategorie == "Event")|>
    mutate(`Event ID` = as.character(`Event ID`)|>as.integer())
  event_einnahmen
  
  s_event_einnahmen <- event_einnahmen|>
    reframe(`Eventausgaben [CHF]` = sum(`Betrag [CHF]`))
  s_event_einnahmen
  
  ## Eventausgaben ####
  event_ausgaben <- Ausgaben|>
    filter(`Event ID` %in% IDs, Kategorie == "Event")|>
    mutate(`Event ID` = as.character(`Event ID`)|>as.integer())
  event_ausgaben
  
  s_event_ausgaben <- event_ausgaben|>
    reframe(`Eventausgaben [CHF]` = sum(`Betrag [CHF]`))
  s_event_ausgaben
  
  ## Manko/ Überschuss ####
  manko <- df_Kiosk|>
    filter(`Event ID` %in% IDs)|>
    group_by(`Event ID`)|>
    reframe(`Überschuss / Manko [CHF]` = `Überschuss / Manko [CHF]`[1])
  manko
  
  s_manko <- manko|>
    reframe(`Überschuss / Manko [CHF]` = sum(`Überschuss / Manko [CHF]`))
  s_manko
  
  ## Kiosk ####
  Kiosk <- df_Kiosk|>
    filter(`Event ID` %in% IDs)|>
    group_by(`Event ID`, `Artikel-Kassensystem`, Artikelname)|>
    reframe(Anzahl = sum(Anzahl),
            `Umsatz [CHF]` = sum(`Betrag [CHF]`),
            `Gewinn [CHF]` = sum(`Gewinn [CHF]`)
            )
  Kiosk
  
  ## Spezpreise ####
  df_spezpreise <- Kiosk|>
    filter(is.na(`Gewinn [CHF]`))|>
    select(-`Gewinn [CHF]`)|>
    group_by(`Artikel-Kassensystem`,Artikelname)|>
    reframe(Anzahl = sum(Anzahl), 
            `Umsatz [CHF]` = sum(`Umsatz [CHF]`))
  df_spezpreise
  
  ## Summary Spezpreise ####
  s_df_spezpreise <-
    bind_cols(
      df_spezpreise |>
        reframe(`Umsatz [CHF]` = sum(`Umsatz [CHF]`)),
      event_ausgaben |>
        reframe(`Eventausgaben [CHF]` = sum(`Betrag [CHF]`))
    ) |>
    mutate(`Gewinn Spezialartikel [CHF]` = `Umsatz [CHF]` - `Eventausgaben [CHF]`)|>
    select(`Gewinn Spezialartikel [CHF]`)
  s_df_spezpreise

  ## summary Kiosk #####
  s_Kiosk <-
    bind_cols(Kiosk |>
                filter(!is.na(`Gewinn [CHF]`)) |>
                reframe(`Gewinn Kioskartikel [CHF]` = sum(`Gewinn [CHF]`)),
              s_df_spezpreise,
              s_manko
    )
  s_Kiosk|>
    t()
  
  ## Abrechnung ####
  Abrechnung <- df_Abrechnung|>
    filter(`Event ID` %in% IDs)|>
    select(-`Besucherzahlen an Verleiher gesendet`, -`Rechnung bezahlt und abgelegt`,
           - Abrechnungsjahr
           )
  Abrechnung
  
  ### Abzug [%] ckeck ####
  c_test <- (sum(Abrechnung$`Abzug [%]`[1] == Abrechnung$`Abzug [%]`, na.rm = TRUE) < nrow(Abrechnung) &
               nrow(Abrechnung) > 1
  )
  
  if(c_test){
    warning(
      paste0("\nFür die Filme `",Abrechnung$Filmtitel[1],"`, Suisanummer: `", Abrechnung$Suisanummer[1] ,"`\n",
            paste0(
              "Event ID: `",Abrechnung$`Event ID`,"`, Datum: `",  format(Abrechnung$Datum, "%d.%m.%Y"),"`",
              ", `Abzug [%]`: ", Abrechnung$`Abzug [%]`,
              collapse = "\n"
              ), "\nsind unterschiedliche `Minimal Abzüge` definiert worden. Bitte im Programm korrigieren!\n"
            )
      )
  }
  
  ### Minimal Abzug [CHF] ckeck ####
  c_test <- (sum(Abrechnung$`Minimal Abzug [CHF]`[1] == Abrechnung$`Minimal Abzug [CHF]`, na.rm = TRUE) < nrow(Abrechnung) &
               nrow(Abrechnung) > 1
  )
  
  if(c_test){
    warning(
      paste0("\nFür die Filme `",Abrechnung$Filmtitel[1],"`, Suisanummer: `", Abrechnung$Suisanummer[1] ,"`\n",
             paste0(
               "Event ID: `",Abrechnung$`Event ID`,"`, Datum: `",  format(Abrechnung$Datum, "%d.%m.%Y"),"`",
               ", `Minimal Abzug [CHF]`: ", Abrechnung$`Minimal Abzug [CHF]`,
               collapse = "\n"
             ), "\nsind unterschiedliche `Minimal Abzüge` definiert worden. Bitte im Programm korrigieren!\n"
      )
    )
  }
  
  ### Abzug fix [CHF] ckeck ####
  c_test <- (sum(Abrechnung$`Abzug fix [CHF]`[1] == Abrechnung$`Abzug fix [CHF]`, na.rm = TRUE) < nrow(Abrechnung) &
               (nrow(Abrechnung) > 1) & (is.na(Abrechnung$`Minimal Abzug [CHF]`)|>sum() > 0)
  )
  
  if(c_test){
    warning(
      paste0("\nFür die Filme `",Abrechnung$Filmtitel[1],"`, Suisanummer: `", Abrechnung$Suisanummer[1] ,"`\n",
             paste0(
               "Event ID: `",Abrechnung$`Event ID`,"`, Datum: `",  format(Abrechnung$Datum, "%d.%m.%Y"),"`",
               ", `Abzug fix [CHF]`: ", Abrechnung$`Abzug fix [CHF]`,
               collapse = "\n"
             ), "\nsind unterschiedliche `Minimal Abzüge` definiert worden. Bitte im Programm korrigieren!\n"
      )
    )
  }

  Abrechnung|>
    t()
  
  ## Summary Abrechnung ####
  s_Abrechnung <- Abrechnung|>
    reframe(
      Verleiher = Verleiher[1],
      `Abzug [%]` = `Abzug [%]`[1],
      `Minimal Abzug [CHF]` = `Minimal Abzug [CHF]`[1],
      `Abzug fix [CHF]` = `Abzug fix [CHF]`[1],
      `Kinoförderer gratis?` = `Kinoförderer gratis?`[1], 
      `SUISA-Vorabzug [%]` = `SUISA-Vorabzug [%]`[1],
      `Umsatz [CHF]` = sum(`Umsatz [CHF]`) ,
      `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`) ,
      `Suisavorabzug [CHF]` = `Umsatz für Netto3 [CHF]` * (`SUISA-Vorabzug [%]` / 100) ,
      `Umsatz Netto 3 [CHF]` = `Umsatz für Netto3 [CHF]` - `Suisavorabzug [CHF]`,
      `Verleiherrechnungsbetrag [CHF]` = `Verleiherrechnungsbetrag [CHF]`[1],
      `Überschuss / Manko [CHF]` = sum(`Überschuss / Manko [CHF]`),
      `Eventeinnahmen [CHF]` = sum(`Eventeinnahmen [CHF]`),
      `Eventausgaben [CHF]` = sum(`Eventausgaben [CHF]`)
    )
  s_Abrechnung|>
    t()
  
  s_Abrechnung <- s_Abrechnung|>
    mutate(           
      # Abzug fix?
      `Verleiherabzug [CHF]` =
        if_else(is.na(`Abzug fix [CHF]`),
                `Umsatz Netto 3 [CHF]` * (`Abzug [%]` / 100),
                `Umsatz Netto 3 [CHF]` - `Abzug fix [CHF]`
        ),
      `Verleiherabzug [CHF]` =
        if_else(
          is.na(`Verleiherrechnungsbetrag [CHF]`),
          `Verleiherabzug [CHF]`,
          `Verleiherrechnungsbetrag [CHF]`
        ),
      `MWST [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                             `Verleiherabzug [CHF]` * (c_MWST / 100),
                             `Verleiherabzug [CHF]` - (`Verleiherabzug [CHF]` / (1 + (c_MWST / 100)))
      ),
      `Verleiherabzug [CHF]` =
        if_else(
          is.na(`Verleiherrechnungsbetrag [CHF]`),
          `Verleiherabzug [CHF]` + `MWST [CHF]`,
          `Verleiherrechnungsbetrag [CHF]`
        ),
      `Ticketgewinn [CHF]` = `Umsatz [CHF]` - `Verleiherabzug [CHF]`,
      `Gewinn Kioskartikel [CHF]` = s_Kiosk$`Gewinn Kioskartikel [CHF]`,
      `Gewinn Spezialartikel [CHF]` = s_Kiosk$`Gewinn Spezialartikel [CHF]`,
      `Überschuss / Manko [CHF]` = s_Kiosk$`Überschuss / Manko [CHF]`,
      `Gewinn aus Fimvorführung [CHF]` = 
        `Ticketgewinn [CHF]` + `Gewinn Kioskartikel [CHF]` + `Gewinn Spezialartikel [CHF]` + `Überschuss / Manko [CHF]`
      )
  s_Abrechnung|>
    t()
  
  ## Return values ####
  l_abrechnung[[cnt]] <- 
    list(
      IDs = tibble(IDs = IDs),
      Abrechnung = Abrechnung, 
      s_Abrechnung = s_Abrechnung,
      Einahmen = event_einnahmen,
      Ausgaben = event_ausgaben,
      Verleiherrechnung = Verleiherrechnung,
      Eintritte = Eintritte,
      s_Eintritte = s_Eintritte,
      Kiosk = Kiosk,
      s_Kiosk = s_Kiosk,
      df_spezpreise = df_spezpreise,
      s_df_spezpreise = s_df_spezpreise,
      manko = manko,
      `Summary Kiosk` = s_Eintritte
    )
  cnt <- cnt + 1
}
remove(Eintritte, 
       df_mapping, df_temp,
       Abrechnung,
       event_ausgaben,
       event_einnahmen,
       s_Eintritte,
       s_Kiosk,
       cnt, ID, p, IDs, c_test, c_Kiosk, c_EventIDs_Eintritte, c_eintritt,
       df_Abrechnung
       )
l_abrechnung
l_abrechnung[["35"]]

# Daten für Berichet #### 
## Besucherzahlen  ####
df_Besucherzahlen <- df_Eintritt|>
  group_by(`Event ID`,Datum, Filmtitel, Suisanummer)|>
  reframe(Besucher = sum(Anzahl))
df_Besucherzahlen

## Eventeinnahmen ####
df_Eventeinnahmen <- Einnahmen|>
  filter(Kategorie == "Event")

## Eventausgaben ####
df_Eventausgaben <- Ausgaben|>
  filter(Kategorie == "Event")

## Keine Rechnung vorhanden ####
df_keine_Rechnung <- Ausgaben|>
  filter(is.na(`Betrag [CHF]`))

# Data export: write to Excel ####
c_filePath <- "output/data/"
if(!dir.exists(c_filePath)) dir.create(c_filePath, recursive = T)

# list(`Werbung` = df_Besucherzahlen,
#      `Tickets` = df_Eintritt,
#      `Kiosk` = df_Kiosk,
#      `Eventeinnahmen` = df_Eventeinnahmen,
#      `Eventausgaben` = df_Eventausgaben,
#      `Filmvorführung` = df_Abrechnung
# )|>
#   write.xlsx(file="output/data/Auswertung.xlsx", asTable = TRUE, overwrite = TRUE)

# remove not used variables ####
remove(ii,
       c_filePath
)



# user interaction ####
writeLines("Good ... Berechnungen erfolgt")
