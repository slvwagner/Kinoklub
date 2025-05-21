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
DB_host <- Sys.getenv("DB_host")
DB_name <- Sys.getenv("DB_name")
DB_user <- Sys.getenv("DB_user")
DB_pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

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

# Extrakt Kioskverkauf und Überschuss / Manko #####
convert_data_kiosk_txt <- function(fileName, con) {
  print("convert_data_kiosk_txt")
  
  Programm <- DB_get_table("Programm",con)
  Programm <- convert_to_template_types(Programm, l_template$Programm)
  
  `Einkauf Kiosk` <- DB_get_table("Einkauf Kiosk", con)
  `Einkauf Kiosk` <- convert_to_template_types(`Einkauf Kiosk`, l_template$`Einkauf Kiosk`)
  
  l_temp <- fileName|>
    lapply(function(fileName){
      c_raw <- suppressWarnings(readLines(fileName))
      
      # find ID_Program from file name
      ID <- str_match(fileName, "ID"%R%optional(SPC)%R%capture(one_or_more(DGT)))[2]|>
        as.integer()
      
      # find ID_Program
      df_temp <- Programm|>
        filter(`Event ID` == ID)
      
      # Extract Datum from file
      p <- or("\\b\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}\\b", # format 01.01.2025
              "\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b" # # format 01/01/2025
      )
      
      index <- c_raw|>
        str_detect(p)
      index
      
      if(sum(index) == 0) {
        stop("\nIn der Datei: ",fileName, " kann kein Datum gefunden werden.","\nBitte der Datei ein korrektes Datum hinzufügen")
      }
      
      c_fileDate <- c_raw[index]|>
        str_split("\t")|>
        unlist()|>
        dmy()
      c_fileDate
      
      if(c_fileDate != df_temp$Datum) {
        stop("\nIn der Datei: .../Kinoklub/", fileName,
             "\nwurde das Datum ",format(c_fileDate, "%d.%m.%Y")," gefunden.",
             "\nIm Programm wurde aber das Datum ",format(df_temp$Datum, "%d.%m.%Y")," für Programm ID: ", ID," / ",df_temp$Filmtitel," definiert\n" )
      }
      
      # detect Verkaufarikel in string
      p1 <- or1(paste0(`Einkauf Kiosk`$`Artikelname-Kassensystem`))
      
      # detect Spez Preise
      p2 <- or1(paste0("Spez"%R%SPC, 1:4))
      
      # Detect Überschuss Manko
      p3 <- optional("-") %R% one_or_more(DGT) %R% optional(DOT)%R% one_or_more(DGT)
      
      # create list to store data
      ii <- 1L
      l_extracted <- list()
      
      # get all lines with Verkauf
      l_extracted[[ii]] <-
        list(Verkaufsartikel =
               tibble(Verkaufartikel_string = c(c_raw[str_detect(c_raw, p1)], ## Arikel erfasst in Kassasystem
                                                c_raw[str_detect(c_raw, p2)]  ## Spez Arikel
               )
               )
        )
      # Extract Überschuss Manko der Kasse
      ii <- ii + 1L
      l_extracted[[ii]] <-
        list(
          `Überschuss / Manko` =
            tibble(`Überschuss / Manko` =
                     c_raw[str_detect(c_raw, "Manko")]|>
                     str_extract(p3)|>
                     as.numeric()
            )|>
            mutate(`Überschuss / Manko` = if_else(is.na(`Überschuss / Manko`), 0, `Überschuss / Manko`))
        )
      # `Event ID`
      ii <- ii + 1L
      l_extracted[[ii]] <-
        list(`Event ID` =  ID)
      
      # File date
      ii <- ii + 1L
      l_extracted[[ii]] <-
        list(Datum = c_fileDate[ii])
      
      # extract Verkauf
      m_Kiosk <-
        l_extracted[[1]][["Verkaufsartikel"]]$Verkaufartikel_string |>
        str_split(pattern = "\t", simplify = T)
      m_Kiosk
      
      c_suisanummer <- l_extracted |>
        lapply(function(x) {
          x[["Suisanummer"]]
        })|>
        unlist()
      c_suisanummer
      
      # Wie viele Spalten
      c_lenght <- ncol(m_Kiosk)
      c_lenght
      
      # extract according to nrow(m_Kiosk), not all files have the same number of columns
      if(c_lenght == 7){ # mit Korrekturbuchungen
        m_Kiosk <- m_Kiosk[,c(1:2,4:5,7)]
        x <- m_Kiosk[,2:ncol(m_Kiosk)]|>
          apply(2, as.numeric)
        colnames(x) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
        
        x <- x|>
          as_tibble()|>
          mutate(Anzahl = if_else(!is.na(Korrektur),Anzahl+Korrektur,Anzahl))|>
          select(-Korrektur)
        
        m_Kiosk <-
          bind_cols(
            Verkaufsartikel = m_Kiosk[,1], x,
            tibble(Datum = c_fileDate)
          )
        
      }else if(c_lenght == 5){ # keine Korrekturbuchungen
        m_Kiosk <- m_Kiosk[,c(1:3,5)]
        x <- m_Kiosk[,2:ncol(m_Kiosk)]|>
          apply(2, as.numeric)
        colnames(x) <- c("Einzelpreis", "Anzahl", "Betrag")
        
        m_Kiosk <-
          bind_cols(
            Verkaufsartikel = m_Kiosk[,1], x,
            tibble(Datum = c_fileDate)
          )
      }else if(c_lenght == 0){ # Keine Kioskverkäufe
        m_Kiosk <- tibble(Verkaufsartikel = "Keine Kioskverkäufe",
                          Einzelpreis = 0,
                          Anzahl = 0,
                          Betrag = 0,
                          Datum = c_fileDate
        )
      } else {
        stop(paste0("\nDie Datei: input/advance tickets/Kiosk ",names(m_Kiosk)[ii],".txt",
                    "\nhat hat ein anderes Format und ist noch nicht implementiert.\nBitte wenden dich an die Entwicklung"))
      }
      
      m_Kiosk
      
      # Data returned by function
      df_Kiosk <- m_Kiosk|>
        mutate(`Einzelpreis` = if_else(is.na(Einzelpreis), Betrag / Anzahl, Einzelpreis),
               `Betrag` = if_else(Anzahl == 0, 0, Betrag),
               `Überschuss / Manko [CHF]` = l_extracted[[2]]$`Überschuss / Manko`$`Überschuss / Manko`
        )|>
        rename(`Einzelpreis [CHF]`= Einzelpreis,
               `Betrag [CHF]` = Betrag
        )
    })
  names(l_temp) <- str_match(fileName, capture(one_or_more(DGT))%R%DOT%R%"txt")[,2]
  
  # Kiosk data 
  df_Kiosk <- bind_rows(l_temp, .id = "Event ID")|>
    mutate(`Event ID` = as.integer(`Event ID`))|>
    rename("Artikel-Kassensystem" = Verkaufsartikel)|>
    arrange(`Event ID`)
  df_Kiosk
  
  df_temp01 <- df_Kiosk
  
  # Spez Verkaufsartikel / Spezialpreise einlesen ####
  ## Spezialpreise einlesen ####
  df_Spezialpreisekiosk <- DB_get_table("Spezialpreisekiosk",con) 
  df_Spezialpreisekiosk <- df_Spezialpreisekiosk|>
    mutate(`Event ID` = as.character(`Event ID`)|>as.integer(),
           Spezialpreis = as.character(Spezialpreis)
    )|>
    arrange(`Event ID`, Spezialpreis)
  df_Spezialpreisekiosk
  
  # Spezialpreise in Kiosk daten finden
  df_spez_preis <- df_Kiosk|>
    filter(str_detect(`Artikel-Kassensystem`, "Spez")) |>
    arrange(`Event ID`)
  df_spez_preis
  # join Filmtitel
  df_spez_preis <- df_spez_preis|>
    left_join(Programm|>
                select(`Event ID`,Filmtitel),
              by = join_by(`Event ID`)
    )|>
    left_join( # look up Spezialpreise
      df_Spezialpreisekiosk,
      by = c("Event ID", `Artikel-Kassensystem` = "Spezialpreis")
    )
  df_spez_preis
  
  ## Sind alle Spezialpreise pro `Event ID` definiert? ####
  df_spez_preis_na <- df_spez_preis|>
    filter(str_detect(`Artikel-Kassensystem`, "Spez")) |>
    arrange(`Event ID`, `Artikel-Kassensystem`)
  df_spez_preis_na
  
  df_spez_preis_na <- df_spez_preis_na|>
    filter(is.na(Artikelname))
  df_spez_preis_na
  
  if(nrow(df_spez_preis_na) > 0) {
    warning(
      paste0(
        "\nFür die Filmvorführung ID ",df_spez_preis_na$`Event ID`, " / ", df_spez_preis_na$Filmtitel," am ", format(df_spez_preis_na$Datum, "%d.%m.%Y"),
        "\nwurde der Artikel ", df_spez_preis_na$`Artikel-Kassensystem`," nicht definiert.",
        "\nBitte korrigieren in Spezialpreisekiosk\n"
      )
    )
  }
  bind_rows(df_spez_preis, df_spez_preis_na)
  
  ## join Spezpreise mit Verkaufsartikel ####
  df_Kiosk <- df_Kiosk|>
    left_join(df_Spezialpreisekiosk|>
                select(-ID),
              by = c("Event ID", `Artikel-Kassensystem` = "Spezialpreis")
    )|>
    mutate(Verkaufsartikel = if_else(is.na(Artikelname), `Artikel-Kassensystem`, Artikelname))|>
    select(-Artikelname)
  df_Kiosk
  
  # find spez Artikel
  df_Kiosk|>
    filter(Verkaufsartikel != `Artikel-Kassensystem`)
  
  
  ## Einkaufspreise ####
  df_Einkaufspreise <- `Einkauf Kiosk`|>
    rename(ID_Kioskartikel = ID)
  df_Einkaufspreise

  df_Kiosk
  
  df_mapping <- df_Kiosk|>
    distinct(`Event ID`, `Artikel-Kassensystem`, .keep_all = TRUE)
  
  df_mapping
  
  df_mapping <- df_Kiosk|>
    distinct(`Artikel-Kassensystem`,.keep_all = TRUE)
  df_mapping
  
  # Einkaufspreise nach Datum nachschlagen
  l_temp <- 1:nrow(df_mapping)|>
    lapply(function(ii){
      df_temp <- df_mapping|>
        slice(ii)
      df_temp
      
      df_temp <- df_Einkaufspreise|>
        filter(`Artikelname-Kassensystem` == df_temp$Verkaufsartikel[1])|>
        mutate(Datum = df_temp$Datum,
               Date_deviation = `Gültig ab Datum` - Datum )|>
        filter(Date_deviation == min(Date_deviation) )
      df_temp
      
      if(nrow(df_temp) == 0) {
        df_temp <- df_mapping|>
          slice(ii)
        df_temp
        stop("Es wurde kein Artikelname-Kassensystem gefunden für den Artikel", df_temp$Artikel[1])
      }
      return(df_temp)
    })
  df_Einkaufspreise <- bind_rows(l_temp)
  df_Einkaufspreise

  # Join Einkaufspreise
  df_Kiosk - df_Kiosk|>
      left_join(df_Einkaufspreise|>
                  select(-`Gültig ab Datum`),
                by = c(Verkaufsartikel = "Artikelname-Kassensystem", Datum = "Datum")
      )
  df_Kiosk
  
  ## V1.5 Merge Verkaufsartikel "Popcorn frisch", "Popcorn Salz" zu "Popcorn frisch" ####
  df_Kiosk <- bind_rows(df_Kiosk|>
                          filter(Verkaufsartikel %in% c("Popcorn frisch", "Popcorn Salz"))|>
                          mutate(Verkaufsartikel = "Popcorn frisch"),
                        df_Kiosk|>
                          filter(! Verkaufsartikel %in% c("Popcorn frisch", "Popcorn Salz"))
  )
  df_Kiosk
  
  ## Kioskgewinn ####
  df_Kiosk <- df_Kiosk|>
    mutate(Gewinn = if_else(is.na(`Einkaufspreis [CHF]`),
                            `Betrag [CHF]`,
                            `Betrag [CHF]` - (Anzahl * `Einkaufspreis [CHF]`))
    )|>
    rename(Kassiert = `Betrag [CHF]`,
           Verkaufspreis = `Einzelpreis [CHF]`)
  
  # join Program ID
  df_Kiosk <-
    df_Kiosk|>
    select(-Datum)|>
    left_join(Programm|>
                select(`Event ID`, Datum, Suisanummer, Filmtitel),
              by = join_by(`Event ID`)
    )
  
  df_Kiosk <- df_Kiosk|>
    mutate(ID = row_number())|>
    select("ID", "Event ID", "Datum", "Suisanummer", "Überschuss / Manko [CHF]", "ID_Kioskartikel","Verkaufsartikel", "Lieferant",
           "Verkaufspreis", "Anzahl", "Kassiert",  
           "Einkaufspreis [CHF]"
    )|>
    rename(`Verkaufspreis [CHF]` = Verkaufspreis,
           `Kassiert [CHF]` = Kassiert)|>
    mutate(`Gewinn [CHF]` = Anzahl * (`Verkaufspreis [CHF]`- `Einkaufspreis [CHF]`))|>
    arrange(ID)
  
  # function return
  return(df_Kiosk)
  
}

c_path <- "input/advance tickets"
c_files <- list.files(c_path, pattern = "Kiosk", recursive = TRUE, full.names = TRUE)

df_Kiosk <- convert_data_kiosk_txt(c_files[2], con)
df_Kiosk

c_Event_ID <- distinct(df_Kiosk,`Event ID`,.keep_all = T)|>
  arrange(`Event ID`)|>
  select(`Event ID`)|>
  pull()

c_Event_ID[2]|>
  lapply(function(x){
    df_Kiosk|>
      filter(`Event ID` == x)|>
      print()
  })



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
