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
df_Kiosk <- DB_get_table("df_Kiosk", con)|>
  convert_to_template_types(l_template$df_Kiosk)|>
  filter(c_Abrechnungsjahr == lubridate::year(Datum))

## Einnahmen ####
Einnahmen <- DB_get_table("Einnahmen", con)|>
  convert_to_template_types(l_template$Einnahmen)|>
  filter(Abrechnungsjahr == c_Abrechnungsjahr)

## Ausgaben ####
Ausgaben <- DB_get_table("Ausgaben", con)|>
  convert_to_template_types(l_template$Ausgaben)|>
  filter(Abrechnungsjahr == c_Abrechnungsjahr)

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
  filter(ID %in% Programm$`Event ID`)|>
  arrange(ID)|>
  select(filename)|>
  pull()
c_eintritt

## Eintritt files ####
c_Kiosk <- tbl(con, "Kiosk files")|>
  filter(ID %in% Programm$`Event ID`)|>
  arrange(ID)|>
  select(filename)|>
  pull()
c_Kiosk


# check nb of files Eintritt vs Kiosk ####
if(length(c_eintritt) != length(c_Kiosk)) {
  if(length(c_eintritt) > length(c_Kiosk)){
    warning("\nEs gibt ", length(c_eintritt), " Eintrittsdateien aber ", length(c_Kiosk), " Kioskdateien.")
  }else {
    warning("\nEs gibt ", length(c_Kiosk), "  Kioskdateien aber ", length(c_eintritt), " Eintrittsdateien.")
  }
}

# check nb of `Event ID` Eintritt vs Kiosk ####
c_eintritt <- df_Eintritt|>
  distinct(`Event ID`)|>
  pull()
c_eintritt

c_Kiosk <- df_Kiosk|>
  filter(lubridate::year(Datum) == c_Abrechnungsjahr)|>
  distinct(`Event ID`)|>
  pull()

if(length(c_eintritt) != length(c_Kiosk)) {
  if(length(c_eintritt) > length(c_Kiosk)){
    warning("\nEs gibt ", length(c_eintritt), " Eintrittsdateien aber ", length(c_Kiosk), " Kioskdateien.")
  }else {
    warning("\nEs gibt ", length(c_Kiosk), "  Kioskdateien aber ", length(c_eintritt), " Eintrittsdateien.")
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
       "\nEine der Dateien muss umbenannt oder gelöscht werden. ", "\nBitte im Verzeichniss  .../Input/advanced tickets/ korrigieren.\n")
  c_Kiosk[c_index]
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

# Spez Verkaufsartikel / Spezialpreise einlesen ####
## Spezialpreise einlesen ####
df_Spezialpreisekiosk <- df_Spezialpreisekiosk|>
  mutate(`Event ID` = as.character(`Event ID`)|>as.integer(),
         Spezialpreis = as.character(Spezialpreis)
  )|>
  arrange(`Event ID`, Spezialpreis)
df_Spezialpreisekiosk

# Spezialpreise in Kiosk daten finden
df_spez_preis <- df_Kiosk|>
  filter(is.na(ID_Kioskartikel)) |>
  arrange(`Event ID`)

# join Filmtitel
df_spez_preis <- df_spez_preis|>
  left_join(Programm|>
              select(`Event ID`,Filmtitel),
            by = join_by(`Event ID`)
  )|>
  left_join( # look up Spezialpreise
    df_Spezialpreisekiosk|>
      select(-ID),
    by = c("Event ID", `Verkaufsartikel` = "Spezialpreis")
  )
df_spez_preis

# check Spezialpreise ####
df_spez_preis_na <- df_spez_preis|>
  filter(is.na(Verkaufsartikel))|>
  filter(str_detect(`Artikelname-Kassensystem`, "Spez")) |>
  arrange(`Event ID`, `Artikelname-Kassensystem`)
df_spez_preis_na

if(nrow(df_spez_preis_na) > 0){
  warning(paste0("\nFür `Event ID`= ", df_spez_preis_na$`Event ID`, 
                 ", `", df_spez_preis_na$Filmtitel, "`, ist der Spezialpreis `", 
                 df_spez_preis_na$`Artikelname-Kassensystem`,"` nicht  noch nicht definiet worden."
                 )
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
  filter(Datum <= Sys.Date(),
         `Verleiher Angefragt?` == "Bestätigt") # Nur Filme abrechnen welche bereits vorgeführt wurden
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
remove(df_Tickets)


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

  # if(length(IDs) > 1){
  #   print("here")
  # }
  
  ## Eintritte ####
  Eintritte <- df_Eintritt|> 
    select(- `SUISA-Vorabzug [%]`)|>
    filter(`Event ID` %in% IDs)|>
    select(`Event ID`,Platzkategorie, Verkaufspreis, Anzahl, `Umsatz [CHF]`)
  Eintritte

  ## Verteilschlüssel ####
  Verteilschlüssel <- Eintritte|>
    group_by(`Event ID`)|>
    reframe(`Umsatz [CHF]` = sum(`Umsatz [CHF]`))|>
    mutate(Verteilschlüssel = `Umsatz [CHF]` / sum(`Umsatz [CHF]`))|>
    select(-`Umsatz [CHF]`)
  Verteilschlüssel
  
  ## s_Eintritte ####
  s_Eintritte <- Eintritte|>
    reframe(Besucherzahl = sum(Anzahl),
            `Umsatz [CHF]` = sum(`Umsatz [CHF]`),
            )
  s_Eintritte
  
  s_Eintritte <- Eintritte|>
    left_join(Verteilschlüssel, by = join_by(`Event ID`))|>
    group_by(`Event ID`, Verteilschlüssel)|>
    reframe(Besucherzahl = sum(Anzahl),
            `Besucherzahl nach Umsatz [CHF] verteilt` = signif(s_Eintritte$Besucherzahl * Verteilschlüssel[1], 4),
            `Umsatz [CHF]` = sum(`Umsatz [CHF]`)
            )|>
    select(`Event ID`, `Umsatz [CHF]`, `Besucherzahl nach Umsatz [CHF] verteilt`, Verteilschlüssel, Besucherzahl)
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
      paste0("\n",
        paste0(
          "Für `Event ID` = ", df_temp$`Event ID`, ", ", df_temp$Filmtitel, ", gibt es keine Verleiherrechnung.",
          "\nBitte in den Ausgaben Kategorie `Verleiher` korrigieren.\n", collapse = "\n"
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
    mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
    left_join(Verteilschlüssel, by = join_by(`Event ID`))|>
    mutate(`Betrag [CHF]` = `Betrag [CHF]` * Verteilschlüssel)
  event_einnahmen
  
  ## Eventausgaben ####
  event_ausgaben <- Ausgaben|>
    filter(`Event ID` %in% IDs, Kategorie == "Event")|>
    mutate(`Event ID` = as.character(`Event ID`)|>as.integer())|>
    left_join(Verteilschlüssel, by = join_by(`Event ID`))|>
    mutate(`Betrag [CHF]` = `Betrag [CHF]` * Verteilschlüssel)
  event_ausgaben
  
  ## Manko/ Überschuss ####
  manko <- df_Kiosk|>
    filter(`Event ID` %in% IDs)|>
    left_join(Verteilschlüssel, by = join_by(`Event ID`))|>
    group_by(`Event ID`)|>
    reframe(`Überschuss / Manko [CHF]` = `Überschuss / Manko [CHF]`[1])
  
  
  ## Kiosk ####
  Kiosk <- df_Kiosk|>
    filter(`Event ID` %in% IDs)|>
    left_join(Verteilschlüssel, by = join_by(`Event ID`))|>
    group_by(`Event ID`, `Artikelname-Kassensystem`, Verkaufsartikel)|>
    reframe(Anzahl = sum(Anzahl),
            `Umsatz [CHF]` = sum(`Umsatz [CHF]`),
            `Gewinn [CHF]` = sum(`Gewinn [CHF]`)
            )
  
  ## summary Kiosk #####
  df_temp <- bind_rows(
    Eintritte|>
      filter(Verkaufspreis != 0 )|>
      reframe(Anzahl = sum(Anzahl), 
              `Umsatz [CHF]` = sum(`Umsatz [CHF]`)
      )|>
      mutate(Zahlend = TRUE),
    Eintritte|>
      reframe(Anzahl = sum(Anzahl), 
              `Umsatz [CHF]` = sum(`Umsatz [CHF]`)
      )|>
      mutate(Zahlend = FALSE)
  )
  df_temp
  
  Gewinn <- Kiosk|>
    reframe(`Gewinn [CHF]` = sum(`Gewinn [CHF]`) - sum(event_ausgaben$`Betrag [CHF]`)
            )|>
    pull()
  Gewinn
  
  Umsatz <- Kiosk|>
    reframe(`Gewinn [CHF]` = sum(`Umsatz [CHF]`)
    )|>
    pull()
  Umsatz
  
  s_Kiosk <- df_temp|>
    mutate(`Umsatz [CHF] pro Gast` = Umsatz / Anzahl,
           `Gewinn [CHF] pro Gast` = Gewinn / Anzahl)
  s_Kiosk
  
  ## Abrechnung ####
  Abrechnung <- df_Abrechnung|>
    filter(`Event ID` %in% IDs)|>
    left_join(Verteilschlüssel, by = join_by(`Event ID`))|>
    select(-`Besucherzahlen an Verleiher gesendet`, -`Rechnung bezahlt und abgelegt`,
           - Abrechnungsjahr
           )|>
    mutate(`Verleiherrechnungsbetrag [CHF]` = sum(`Verleiherrechnungsbetrag [CHF]` ,na.rm = TRUE))
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

  ## Summary Abrechnung ####
  s_Abrechnung <- Abrechnung|>
    reframe(
      Verleiher = Verleiher[1],
      `Abzug [%]` = `Abzug [%]`[1],
      `Minimal Abzug [CHF]` = `Minimal Abzug [CHF]`[1],
      `Abzug fix [CHF]` = `Abzug fix [CHF]`[1],
      `Kinoförderer gratis?` = `Kinoförderer gratis?`[1], 
      `SUISA-Vorabzug [%]` = `SUISA-Vorabzug [%]`[1],
      `Umsatz [CHF]` = sum(`Umsatz [CHF]`) , # this will not change anything because Verteilschlüssel was calculated by Umsatz
      `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`) ,
      `Suisavorabzug [CHF]` = sum(`Suisavorabzug [CHF]`) ,
      `Umsatz Netto 3 [CHF]` = sum(`Umsatz Netto 3 [CHF]`) ,
      `Verleiherrechnungsbetrag [CHF]` = `Verleiherrechnungsbetrag [CHF]`[1],
      `Kioskgewinn [CHF]` = sum(`Kioskgewinn [CHF]`),
      `Überschuss / Manko [CHF]` = sum(`Überschuss / Manko [CHF]`),
      `Eventeinnahmen [CHF]` = sum(`Eventeinnahmen [CHF]`),
      `Eventausgaben [CHF]` = sum(`Eventausgaben [CHF]`)
    )
  
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
      `Gewinn aus Fimvorführung [CHF]` = 
        sum(`Ticketgewinn [CHF]`, `Kioskgewinn [CHF]`, 
            `Eventeinnahmen [CHF]`, -`Eventausgaben [CHF]`, `Überschuss / Manko [CHF]`
            )
      )
  s_Abrechnung
  
  ## Abrechnung (Verteilen nach Verteilschlüssel) ####
  Abrechnung <- Abrechnung|>
    mutate(`Verleiherrechnungsbetrag [CHF]` = `Verleiherrechnungsbetrag [CHF]`[1] * Verteilschlüssel,
           `Umsatz [CHF]` = sum(`Umsatz [CHF]`) * Verteilschlüssel, # this will not change anything because Verteilschlüssel was calculated by Umsatz
           `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`) * Verteilschlüssel,
           `Suisavorabzug [CHF]` = sum(`Suisavorabzug [CHF]`) * Verteilschlüssel,
           `Umsatz Netto 3 [CHF]` = sum(`Umsatz Netto 3 [CHF]`) * Verteilschlüssel,
           `Verleiherrechnungsbetrag [CHF]` = sum(`Verleiherrechnungsbetrag [CHF]`) * Verteilschlüssel,
           `Verleiherabzug [CHF]` = sum(`Verleiherabzug [CHF]`) * Verteilschlüssel,
           `Ticketgewinn [CHF]` = sum(`Ticketgewinn [CHF]`) * Verteilschlüssel,
           `Kioskgewinn [CHF]` = sum(`Kioskgewinn [CHF]`) * Verteilschlüssel,
           `Überschuss / Manko [CHF]` = sum(`Überschuss / Manko [CHF]`) * Verteilschlüssel,
           `Eventeinnahmen [CHF]` = sum(`Eventeinnahmen [CHF]`) * Verteilschlüssel,
           `Eventausgaben [CHF]` = sum(`Eventausgaben [CHF]`) * Verteilschlüssel,
           `Gewinn aus Fimvorführung [CHF]` = sum(`Gewinn aus Fimvorführung [CHF]`) * Verteilschlüssel
           )
  Abrechnung

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
      manko = manko,
      `Summary Kiosk` = s_Eintritte,
      Verteilschlüssel = Verteilschlüssel
    )
  cnt <- cnt + 1
}
remove(Eintritte, 
       df_mapping, df_temp,
       Verteilschlüssel,
       Abrechnung,
       temp_Ausgaben,
       event_einnahmen,
       s_Eintritte,
       s_Kiosk,
       cnt, ID, p, Umsatz, IDs, c_test, c_Kiosk, c_EventIDs_Eintritte, c_eintritt, Gewinn,
       df_Abrechnung, 
       c_test
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

# Disconnect from DB ####
dbDisconnect(con)

# user interaction ####
writeLines("Good ... Berechnungen erfolgt")
