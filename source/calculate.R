# read and caluclate all input data
library(rebus)
library(openxlsx)
library(lubridate)
library(tidyverse)

writeLines("Daten werden einlesen und berechnet...")

# load user settings
source("source/functions.R")
source("source/SQL/SQL_Functions.R")

###### read in data ###### 
# # Einlesen Input daten
# c_file <- "Input/Data.Rds"
# if(file.exists(c_file)){
#   l_data <- readRDS(c_file)
#   c_backup_number <- length(list.files(path = "Input/backup", pattern = "backup"))
#   if(!dir.exists("Input/backup")) dir.create("Input/backup")
#   saveRDS(l_data, paste0("Input/backup/Data_backup",c_backup_number + 1,".Rds")) # Save the updated list to the file
# }else{ # or load template date 
#   c_file <- "Input/template.Rds"
#   l_data <- readRDS(c_file)
#   c_file <- "Input/Data.Rds"
# }

# read template 
l_template <- readRDS("source/SQL/template.Rds")

pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
pw

tryCatch({
  # Connect to data base 
  con <- DB_connect(pw, "ch367079_flo")
  # get all data as defined in the template l_data
  l_data_sql <- DB_get_Data(l_template, con)
  # Convert data types for each table
  l_data <- convert_DB_to_R(l_data_sql,l_template)
},error =  function(e){
  stop(e$message)
})

# rename ID from Programm to be unique
l_data$Programm <- l_data$Programm|>
  rename(ID_Programm = ID)

# Eintritte aus Advanced Tickets files
convert_data_Film_txt <- function(fileName) {
  l_Eintritt <- fileName|>
    lapply(function(fileName){
      
      c_suisa <- str_extract(fileName, DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT)
      c_datum <- str_extract(fileName, one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))|>
        lubridate::dmy()
      
      # read in data
      c_raw <- suppressWarnings(readLines(fileName))
      c_raw
      l_temp <- list()
      
      # Extract suisa
      p <- or(START%R%DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT,
              START%R%WRD%R%WRD%R%WRD%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT) #suisa
      index <- c_raw|>
        str_detect(p)
      
      c_temp <- c_raw[index]|>
        str_split("\t")|>
        unlist()
      
      ii <- 1
      
      if(c_temp[1] != c_suisa) {
        warning("In der Datei: .../Kinoklub/", fileName, " wurde einen andere Suisanummer gefunden als im Dateinamen angegeben wurde: ", c_temp[1])
      }
      
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
      
      if(dmy(c_temp[2]) != c_datum) {
        warning("In der Datei: .../Kinoklub/", fileName, " wurde einen anderes Datum gefunden als im Dateinamen angegeben wurde: ", c_temp[2] )
      }
      
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
      
      l_temp[[ii]] |>
        mutate(Suisanummer = l_temp[[1]],
               Filmtitel = l_temp[[2]],
               Datum = dmy(l_temp[[3]]),
               `SUISA-Vorabzug` = l_temp[[4]],
               fileName = fileName
        )
    })
  names(l_Eintritt) <- fileName
  return(l_Eintritt)
}

# Extrakt Verkäufe  und Überschuss / Manko
convert_data_kiosk_txt <- function(c_files) {
  
  l_raw <- lapply(c_files, function (x) suppressWarnings(readLines(x)))
  l_raw
  
  ## extract suisa from file name
  c_files
  p <- capture(one_or_more(WRD)%R%DOT%R%one_or_more(WRD))%R%DOT%R%"txt"
  c_kiosk_suisa <- str_match(c_files, pattern = p)[,2]
  c_kiosk_suisa
  
  ## extract date from file name 
  c_fileDate  <- str_match(c_files,capture(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)))[,2]
  c_fileDate
  
  # detect Verkaufarikel in string
  p1 <- or1(paste0(df_verkaufsartikel$`Artikelname-Kassensystem`))
  
  # detect Spez Preise 
  p2 <- or1(paste0("Spez"%R%SPC, 1:4))
  
  # Detect Überschuss Manko 
  p3 <- optional("-") %R% one_or_more(DGT) %R% optional(DOT)%R% one_or_more(DGT)
  
  l_extracted <- list()
  for (ii in 1:length(l_raw)) {
    l_extracted[[ii]] <- list(Verkaufsartikel = tibble(Verkaufartikel_string = c(l_raw[[ii]][str_detect(l_raw[[ii]], p1)], ## Arikel erfasst in Kassasystem
                                                                                 l_raw[[ii]][str_detect(l_raw[[ii]], p2)] ## Spez Arikel
    )
    ),
    `Überschuss / Manko` = tibble(`Überschuss / Manko` = 
                                    l_raw[[ii]][str_detect(l_raw[[ii]], "Manko")]|>
                                    str_extract(p3)|>
                                    as.numeric()
    )|>
      mutate(`Überschuss / Manko` = if_else(is.na(`Überschuss / Manko`),0, `Überschuss / Manko`)),
    Suisanummer =  c_kiosk_suisa[ii],
    Datum = c_fileDate[ii]
    )
  }
  names(l_extracted) <- paste(c_fileDate, c_kiosk_suisa)
  l_extracted
  
  l_Kiosk <- l_extracted |>
    lapply(function(x) {
      y <- x[["Verkaufsartikel"]]$Verkaufartikel_string |>
        str_split(pattern = "\t", simplify = T)
      # y <- cbind(y, x[["Suisanummer"]])
      return(y)
    })
  l_Kiosk
  
  c_suisanummer <- l_extracted |>
    lapply(function(x) {
      x[["Suisanummer"]]
    })|>
    unlist()
  c_suisanummer
  
  # Wie viele Spalten
  c_lenght <- l_Kiosk|>
    lapply(ncol)|>
    unlist()
  c_lenght
  
  ii <- 1
  for (ii in 1:length(l_Kiosk)) {
    if(c_lenght[ii] == 7){ # mit Korrekturbuchungen
      l_Kiosk[[ii]] <- l_Kiosk[[ii]][,c(1:2,4:5,7)]
      x <- l_Kiosk[[ii]][,2:ncol(l_Kiosk[[ii]])]|>
        apply(2, as.numeric)
      colnames(x) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
      
      x <- x|>
        as_tibble()|>
        mutate(Anzahl = if_else(!is.na(Korrektur),Anzahl+Korrektur,Anzahl))|>
        select(-Korrektur)
      
      l_Kiosk[[ii]] <- bind_cols(Verkaufsartikel = l_Kiosk[[ii]][,1], x, tibble(Suisanummer = c_suisanummer[ii],
                                                                                Datum = c_fileDate[ii]
                                                                                ))
      
    }else if(c_lenght[ii] == 5){ # keine Korrekturbuchungen
      l_Kiosk[[ii]] <- l_Kiosk[[ii]][,c(1:3,5)]
      x <- l_Kiosk[[ii]][,2:ncol(l_Kiosk[[ii]])]|>
        apply(2, as.numeric)
      colnames(x) <- c("Einzelpreis", "Anzahl", "Betrag")
      
      l_Kiosk[[ii]] <- bind_cols(Verkaufsartikel = l_Kiosk[[ii]][,1], x, tibble(Suisanummer = c_suisanummer[ii],
                                                                                Datum = c_fileDate[ii]
                                                                                ))
    }else if(c_lenght[ii] == 0){ # Keine Kioskverkäufe
      l_Kiosk[[ii]] <- tibble(Verkaufsartikel = "Keine Kioskverkäufe",
                              Einzelpreis = 0,
                              Anzahl = 0,
                              Betrag = 0,
                              Suisanummer = c_suisanummer[ii]
                              
      )
    } else {
      stop(paste0("\nDie Datei: input/advance tickets/Kiosk ",names(l_Kiosk)[ii],".txt", 
                  "\nhat hat ein anderes Format und ist noch nicht implementiert.\nBitte wenden dich an die Entwicklung"))
    }
  }
  l_Kiosk

  # Data returned by function
  l_return <- list()
  l_return[["df_Kiosk"]] <- l_Kiosk|>
    bind_rows()|>
    mutate(Datum = dmy(Datum),
           Einzelpreis = if_else(is.na(Einzelpreis), Betrag / Anzahl, Einzelpreis),
           Betrag = if_else(Anzahl == 0, 0, Betrag))
  
  # Extrakt Überschuss / Manko
  l_return[["Überschuss / Manko"]] <- l_extracted |>
    lapply(function(x) {
      cbind(x[["Überschuss / Manko"]],
            tibble(Datum = x[["Datum"]],
                   Suisanummer = x[["Suisanummer"]]
                   )
            )
    })|>
    bind_rows()|>
    as_tibble()|>
    mutate(Datum = lubridate::dmy(Datum))
  
  
  # Error handling, compare filename date and date in file 
  p1 <- one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)
  
  file_datum <- l_raw|>
    lapply( function(x){
      temp <- str_extract(x,p1)  
      temp[!is.na(temp)]
    })|>
    unlist()|>
    dmy()
  
  file_datum
  
  c_test <- dmy(c_fileDate)%in%file_datum
  c_test
  
  if(length(c_test)>sum(c_test)){
    stop(  
      paste0("Für das file: .../Kinoklub/Input/advance tickets/Kiosk ",c_fileDate[!c_test], " stimmt das Datum im Dateinamen nicht mit dem Datum welches im File gefunden wurde überein.")|>
        paste0(collapse = "\n")|>
        writeLines()
    )
  }
  
  return(l_return)
}

################## Einnahmen und Ausgaben einlesen ##################
Einnahmen_und_Ausgaben <- list(Einnahmen = l_data$Einnahmen,
                               Ausgaben = l_data$Ausgaben)

# error handling
# suisa nummer automatisch korrigieren 
Einnahmen_und_Ausgaben$Ausgaben$Suisanummer <- Einnahmen_und_Ausgaben$Ausgaben$Suisanummer|>
  str_squish()|>
  str_extract(pattern = DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT) 
Einnahmen_und_Ausgaben$Ausgaben$Suisanummer

Einnahmen_und_Ausgaben$Einnahmen$Suisanummer <- Einnahmen_und_Ausgaben$Einnahmen$Suisanummer|>
  str_squish()|>
  str_extract(pattern = DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT) 
Einnahmen_und_Ausgaben

# Error handling 
# Suisanummer vorhanden für Kategorie Verleiher / Event in den Ausgaben 
df_temp <- Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  filter(Kategorie %in% c("Event","Verleiher"))|>
  mutate(error = is.na(Suisanummer))|>
  filter(error)

if(nrow(df_temp)>0) { 
  for (ii in 1:nrow(df_temp)) {
    warning((paste("\nFür die Kategorie \"Event\" oder \"Verleiher\" muss in der Datei \"Einnahmen und Ausgaben.xlsx\" \nein Spieldatum und einen Suisanummer definiert werden.",
                   "\n\nKategorie\t\tSpieldatum\t\tSuisanummer\t\tBezeichnung",
                   "\n",df_temp$Kategorie[ii],
                   "\t\t", df_temp$Spieldatum[ii], 
                   "\t\t", df_temp$Suisanummer[ii], 
                   "\t\t", df_temp$Bezeichnung[ii])))
  }
}

# Spieldatum  vorhanden für Kategorie Verleiher / Event in den Ausgaben 
df_temp <- Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  filter(Kategorie %in% c("Event","Verleiher"))|>
  mutate(error = is.na(Spieldatum))|>
  filter(error)

if(nrow(df_temp)>0) { 
  for (ii in 1:nrow(df_temp)) {
    warning((paste("\nFür die Kategorie \"Event\" oder \"Verleiher\" muss in der Datei \"Einnahmen und Ausgaben.xlsx\" \nein Spieldatum und einen Suisanummer definiert werden.",
                   "\n\nKategorie\t\tSpieldatum\t\tSuisanummer\t\tBezeichnung",
                   "\n",df_temp$Kategorie[ii],
                   "\t\t", df_temp$Spieldatum[ii], 
                   "\t\t", df_temp$Suisanummer[ii], 
                   "\t\t", df_temp$Bezeichnung[ii])))
  }
}

#  Datum  vorhanden für Kategorie Verleiher / Event in den Ausgaben 
df_temp <- Einnahmen_und_Ausgaben[["Einnahmen"]]|>
  filter(Kategorie %in% c("Event"))|>
  mutate(error = is.na(Datum))|>
  filter(error)

if(nrow(df_temp)>0) { 
  for (ii in 1:nrow(df_temp)) {
    warning((paste("\nFür die Kategorie \"Event\" oder \"Verleiher\" muss in der Datei \"Einnahmen und Ausgaben.xlsx\" \nein Spieldatum und einen Suisanummer definiert werden.",
                   "\n\nKategorie\t\tDatum\t\tSuisanummer\t\tBezeichnung",
                   "\n",df_temp$Kategorie[ii],
                   "\t\t", df_temp$Spieldatum[ii], 
                   "\t\t", df_temp$Suisanummer[ii], 
                   "\t\t", df_temp$Bezeichnung[ii])))
  }
}

# Suisanummer vorhanden für Kategorie Verleiher / Event in den Ausgaben  
df_temp <- Einnahmen_und_Ausgaben[["Einnahmen"]]|>
  filter(Kategorie %in% c("Event"))|>
  mutate(error = is.na(Suisanummer))|>
  filter(error)

if(nrow(df_temp)>0) { 
  for (ii in 1:nrow(df_temp)) {
    warning((paste("\nFür die Kategorie \"Event\" oder \"Verleiher\" muss in der Datei \"Einnahmen und Ausgaben.xlsx\" \nein Spieldatum und einen Suisanummer definiert werden.",
                   "\n\nKategorie\t\tDatum\t\tSuisanummer\t\tBezeichnung",
                   "\n",df_temp$Kategorie[ii],
                   "\t\t", df_temp$Spieldatum[ii], 
                   "\t\t", df_temp$Suisanummer[ii], 
                   "\t\t", df_temp$Bezeichnung[ii])))
  }
}

################## show times ##################
# read in shows 
df_show <- l_data$Programm|>
  filter(`Verleiher Angefragt?` != "Wird nicht gespielt")|>
  select(ID_Programm, Suisanummer, Filmtitel, Datum, Zeit, Verleiher, `Verleiher Angefragt?`)
df_show

## error handling 
p <- DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT
df_temp <- df_show|>
  filter(!str_detect(Suisanummer,p))
df_temp

if(nrow(df_temp) != 0) {
  warning(paste0(
    "Für den Film: ",df_temp$Filmtitel, " am ", 
    day(df_temp$Datum),".",month(df_temp$Datum),".",year(df_temp$Datum), 
    " ist keine Suisanummer vorhanden oder das Format stimmmt nicht.")
  )}

################## Eintritt aus Advanced Tickets ##################
# files to read in
c_files <- list.files(pattern = "Eintritte", recursive = T)

# error handling
if(is_empty(c_files)) {
  stop(paste0("\nEs gibt keinen Dateien im Verzeichniss: \".../Kinoklub/Input/advance tickets\"",
              "\nBitte herunterladen ","<https://www.advance-ticket.ch/decomptefilms?lang=de> und abspeichern:",
              "\n\"Eintritte xx.xx.",Abrechungsjahr,"\"")
       )
  }
# read and convert Eintritte
l_Eintritt <- convert_data_Film_txt(c_files)
l_Eintritt

# create data frame
df_Eintritt <- l_Eintritt|>
  bind_rows()|>
  mutate(Verkaufspreis = Preis ,
         Zahlend = if_else(Verkaufspreis == 0, F, T))|>
  select(Datum, Suisanummer, Filmtitel, Platzkategorie, Zahlend, Verkaufspreis, Anzahl, Umsatz,`SUISA-Vorabzug`)
df_Eintritt

# join ID_Programm 
df_Eintritt <- df_Eintritt|>
  left_join(l_data$Programm|>
              select(ID_Programm, Datum, Suisanummer),
            by = join_by(Datum, Suisanummer)
            )

if(sum(is.na(df_Eintritt$ID_Programm)) > 0){
  df_temp <- df_Eintritt|>
    filter(is.na(ID_Programm))|>
    distinct(Datum, Suisanummer,.keep_all = TRUE)
  stop("\nFür den Film ", df_temp$Filmtitel, " mit Suisanummer ", df_temp$Suisanummer, " am ", 
       paste0(format(df_temp$Datum, "%d.%m.%Y"), collapse = ", "), " existiert kein Programmeintrag\nBitte das Programm korrigieren!\n"
       )
}
  

################## Kioskabrechnungen ##################
# Einkaufspreise
c_file <- list.files(pattern = "Einkauf Kiosk", recursive = T)
c_file

# error handling
if(length(c_files) == 0) stop("\nEs sind keinen Kiosk-Dateinen vorhanden.\nBitte herunterladen:\nhttps://www.advance-ticket.ch/decomptecaisse?lang=de")

df_verkaufsartikel <- l_data$`Einkauf Kiosk`
df_verkaufsartikel

# Advace tickets Kiosk
c_path <- "input/advance tickets"

# Kioskabrechnung
c_files <- list.files(c_path,pattern = "Kiosk", recursive = TRUE, full.names = TRUE)
c_files

# Extrakt Verkäufe  und Überschuss / Manko
l_temp <- convert_data_kiosk_txt(c_files)
l_temp

df_Kiosk <- l_temp$df_Kiosk


# Manko und Überschuss Kiosk 
df_manko_uerberschuss <- l_temp$`Überschuss / Manko`|>
  left_join(
    l_data$Programm|>select(1:6, -`Link ID`),
    by = join_by(Datum, Suisanummer)
  )
df_manko_uerberschuss

if(sum(is.na(df_manko_uerberschuss$ID_Programm)) > 0){
  df_temp <- df_manko_uerberschuss|>
    filter(is.na(ID_Programm))
  warning("\nFür den Film ", df_temp$Suisanummer, " \"Manko /Übeschuss\" gibt es keine Programm eintrag.\n Bitte Programm korrigieren!\n")
}

df_Kiosk <- df_Kiosk|>
  rename("Artikel-Kassensystem" = Verkaufsartikel)
df_Kiosk

# Spez Verkaufsartikel / Spezialpreise einlesen
# Spezialpreise einlesen
Spezialpreisekiosk <- l_data$Spezialpreisekiosk
Spezialpreisekiosk

# error handling
if(is.na(Spezialpreisekiosk$Suisanummer)|>sum() > 0) stop("\nEs wurden nicht alle Suisanummern in Spezialpreisekiosk definiert. \nBitte korrigieren!")

# error handling
# Sind alle Spezialpreise pro Datum und Suisanummer definiert?  
df_spez_preis_na <- df_Kiosk|>
  filter(str_detect(`Artikel-Kassensystem`, "Spez")) |>
  left_join( # look up Spezialpreise
    Spezialpreisekiosk, 
    by = c(Datum = "Datum", Suisanummer = "Suisanummer", "Artikel-Kassensystem" = "Spezialpreis")
  )|>
  filter(is.na(Artikelname))

df_spez_preis_na <- df_spez_preis_na|>
  left_join(df_Eintritt|> # look up Filmtitel
              distinct(Filmtitel,.keep_all = T),
            by = c("Datum", "Suisanummer")
            )
df_spez_preis_na

if(nrow(df_spez_preis_na) > 0) {
  warning(
    paste0(
      "\nFür die Filmvorführung ", df_spez_preis_na$Filmtitel, " am ", day(df_spez_preis_na$Datum),".",month(df_spez_preis_na$Datum),".",year(df_spez_preis_na$Datum),
      " / ", df_spez_preis_na$Suisanummer,
      "\nwurde der Artikel ", df_spez_preis_na$`Artikel-Kassensystem`," nicht definiert.",
      "\nBitte korrigieren in der Datei:","\n.../Kinoklub/input/Spezialpreisekiosk.xlsx\n"
    )
  )
}


# join Spezpreise mit Verkaufsartikel
df_Kiosk <- df_Kiosk|>
  left_join(Spezialpreisekiosk|>
              select(-ID), 
            by = c(Datum ="Datum", Suisanummer = "Suisanummer", `Artikel-Kassensystem` = "Spezialpreis")
  )|>
  mutate(Verkaufsartikel = if_else(is.na(Artikelname), `Artikel-Kassensystem`, Artikelname))|>
  select(-Artikelname)
df_Kiosk

# Kiosk Einkaufspreise 
df_Einkaufspreise <- l_data$`Einkauf Kiosk`|>
  rename(ID_Kioskartikel = ID)
df_Einkaufspreise

c_Date_Kiosk <- l_temp$`Überschuss / Manko`$Datum
c_Einkaufslistendatum <- distinct(df_Einkaufspreise, `Gültig ab Datum`)|>pull()


df_Mapping_Einkaufspreise <- lapply(c_Einkaufslistendatum, function(x)(x-c_Date_Kiosk)|>as.integer())|>
  bind_cols()|>
  as.matrix()|>
  suppressMessages()
df_Mapping_Einkaufspreise

colnames(df_Mapping_Einkaufspreise) <- c_Einkaufslistendatum|>
  as.character()
rownames(df_Mapping_Einkaufspreise) <- c_Date_Kiosk|>as.character()

if(nrow(df_Mapping_Einkaufspreise) == 1){
  df_Mapping_Einkaufspreise <- df_Mapping_Einkaufspreise|>
    apply(1, function(x){
      c_select <- max(x, na.rm = T)
      y <- x[c_select == x]
      y <- y[!is.na(y)]
      return(names(y))
    })
  
}else{
  df_Mapping_Einkaufspreise <- df_Mapping_Einkaufspreise|>
    apply(2, function(x) ifelse(x >= 0, NA, x))|>
    apply(1, function(x){
      c_select <- max(x, na.rm = T)
      y <- x[c_select == x]
      y <- y[!is.na(y)]
      return(names(y))
    })

}
df_Mapping_Einkaufspreise <- tibble(Einkaufspreise = df_Mapping_Einkaufspreise|>as.Date(),
       Datum = names(df_Mapping_Einkaufspreise)|>as.Date())


# Join Einkaufspreise 
l_Kiosk <- list()
for (ii in 1:nrow(df_Mapping_Einkaufspreise)) {
  l_Kiosk[[ii]] <- df_Kiosk|>
    filter(Datum == df_Mapping_Einkaufspreise$Datum[ii])|>
    left_join(df_Einkaufspreise|>
                # select(-ID)|>
                filter(`Gültig ab Datum` == df_Mapping_Einkaufspreise$Einkaufspreise[ii])|>
                select(-`Gültig ab Datum`), 
              by = c(Verkaufsartikel = "Artikelname-Kassensystem")
    )
}
l_Kiosk

df_Kiosk <- l_Kiosk|>
  bind_rows()
df_Kiosk

# V1.5 Merge Verkaufsartikel "Popcorn frisch", "Popcorn Salz" zu "Popcorn frisch"
df_Kiosk <- bind_rows(df_Kiosk|>
                        filter(Verkaufsartikel %in% c("Popcorn frisch", "Popcorn Salz"))|>
                        mutate(Verkaufsartikel = "Popcorn frisch"),
                      df_Kiosk|>
                        filter(! Verkaufsartikel %in% c("Popcorn frisch", "Popcorn Salz"))
)
df_Kiosk

# Gewinn
df_Kiosk <- df_Kiosk|>
  mutate(Gewinn = if_else(is.na(`Einkaufspreis [CHF]`),
                          `Betrag`, 
                          `Betrag` - (Anzahl * `Einkaufspreis [CHF]`))
  )|>
  rename(Kassiert = `Betrag`,
         Verkaufspreis = Einzelpreis)

# join Program ID
df_Kiosk <- 
  df_Kiosk|>
  left_join(l_data$Programm|>
              select(ID_Programm, Datum, Suisanummer),
            by = join_by(Suisanummer, Datum)
            )
# check if all Kiosk entry can be joined by ID_Programm 
if(sum(is.na(df_Kiosk$ID_Programm)) > 0){
  df_temp <- df_Kiosk|>
    filter(is.na(ID_Programm))|>
    distinct(ID_Programm, .keep_all = TRUE )
  df_temp
  stop("\nFür den Film mit Suisanummer ", df_temp$Suisanummer, " am ", format(df_temp$Datum, "%d.%m.%Y"), " gibt es keinen Programmeintrag.\nBitte das Programm korrigieren!")
}


# remove no more needed variables
remove(df_Mapping_Einkaufspreise,l_Kiosk, 
       df_verkaufsartikel,
       c_Date_Kiosk, c_Einkaufslistendatum,
       ii,
       c_path, c_files, l_temp, l_Eintritt)


################  Gibt es gleich viele Kiosk wie Filmabrechungen? ##############
# Bericht mapping
n_kiosk <- df_Kiosk|>distinct(Datum, .keep_all = T)
n_Film <- df_Eintritt|>distinct(Datum, .keep_all = T )


# Error handling
if(n_kiosk|>nrow() > n_Film|>nrow()){
  df_temp <- anti_join(n_kiosk,n_Film, by = "Datum")|>
    select(Datum)
  
  warning(paste0("\nEs fehlt eine Datei: Eintritt ", day(df_temp$Datum),".",month(df_temp$Datum), ".",year(df_temp$Datum), ".txt\"",
              "\nBitte herunterladen unter: https://www.advance-ticket.ch/decomptefilms?lang=de\n"
  )
  )
}else if(df_Kiosk|>distinct(Datum)|>nrow() < df_Eintritt|>distinct(Datum)|>nrow()){
  
  df_temp <- anti_join(n_Film, n_kiosk, by = "Datum")|>
    select(1:3)
  warning(paste0("\nEs fehlt einen Kioskabrechnug zum Film:\n", 
              df_temp$Filmtitel, " am ", day(df_temp$Datum),".",month(df_temp$Datum), ".",year(df_temp$Datum),
              "\nBitter herunterladen unter: https://www.advance-ticket.ch/decomptecaisse?lang=de\n"
  ))
}
remove(n_kiosk, n_Film)

######### Abos und Kinogutscheine ######### 
if(!file.exists("Input/advance tickets/atelierkino_abo.txt")) {
  warning(paste0("\nDie Datei: \".../Input/advance tickets/atelierkino_abo.txt\" wurde nicht gefunden.",
       "\nBitte herunterladen unter: https://www.advance-ticket.ch/abos?lang=de\n"))
  }
atelierkino_abo <- read_delim("Input/advance tickets/atelierkino_abo.txt", 
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
atelierkino_foerderer <- read_delim("Input/advance tickets/atelierkino_foerderer.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                                     first_use = col_date(format = "%Y-%m-%d"), 
                                                     last_use = col_date(format = "%Y-%m-%d"), 
                                                     expiration = col_date(format = "%Y-%m-%d"), 
                                                     count_use = col_integer()), trim_ws = TRUE)

if(!file.exists("Input/advance tickets/atelierkino_gutschein.txt")) {
  warning(paste("Die Datei: .../Input/advance tickets/atelierkino_gutschein.txt wurde nicht gefunden.",
             "\nBitte herunterladen\nhttps://www.advance-ticket.ch/abos?lang=de"))
  }
atelierkino_gutschein <- read_delim("Input/advance tickets/atelierkino_gutschein.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                                     first_use = col_date(format = "%Y-%m-%d"), 
                                                     last_use = col_date(format = "%Y-%m-%d"), 
                                                     expiration = col_date(format = "%Y-%m-%d"), 
                                                     amount = col_double(), count_use = col_integer()), 
                                    trim_ws = TRUE)


################## Verleiherabgaben einlesen ################## 
df_temp <- l_data$Programm|>
  select(1:11,-`Link ID`)|>
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


################ Abrechnung ################
df_Abrechnung <- l_data$Programm|>
  select(1:11)|>
  left_join(l_data$Verleiher|>
              select(-ID, -Kontakt, -Adresse, -PLZ, -Ort), 
            by = c(Verleiher = "Verleihername")
            )|>
  mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein", F, T))
df_Abrechnung

df_Abrechnung <-
  df_Abrechnung|>
  left_join(Einnahmen_und_Ausgaben$Ausgaben|>
              filter(Kategorie == "Verleiher")|>
              select(1:7,-ID, -Datum, -Kategorie)|>
              rename(`Verleiherrechnungsbetrag [CHF]` = `Betrag [CHF]`),
            by = join_by(Suisanummer)
  )
df_Abrechnung

# paste0("\"",names(df_Abrechnung),"\"")|>
#   paste0(collapse = ",")|>
#   writeLines()

# error handling 
df_temp <- df_Abrechnung|>
  filter(is.na(ID_Programm))|>
  slice(1)

if(nrow(df_temp) > 0){
  warning(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                 " wurde kein Programm eintrag gefunden.",
                 "\nBitte im Programm korrigieren!"
                 )
          )
}

# Errorhandling 
# kein prozentualer noch fixer abzug definiert
df_temp <- df_Abrechnung|>
  filter(is.na(`Abzug [%]`) & is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){ 
  warning(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
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

if(nrow(df_temp)>0) warning(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
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


##################  Ticketabrechnung vorbereiten ################## 

df_Abrechnung <- df_Abrechnung|>
  filter(Datum <= Sys.Date()) # Nur Filme abrechnen welche bereits vorgeführt wurden 
df_Abrechnung

# error handling
# Verleiherrechnungbetrag ist kleiner als minimaler Abzug.
df_temp <- df_Abrechnung|>
  mutate(`Minimal Abzug unterschritten` = `Minimal Abzug [CHF]`> `Verleiherrechnungsbetrag [CHF]`,
         `Minimal Abzug unterschritten` = if_else(is.na(`Minimal Abzug unterschritten`), F, `Minimal Abzug unterschritten`)
  )|>
  select(Datum, Filmtitel, `Minimal Abzug unterschritten`)|>
  filter(`Minimal Abzug unterschritten`)

# error handling, keine Verleiherrechnung
if(nrow(df_temp) > 0) {
  warning(paste0("\nAchtung für den Film \"", df_temp$Filmtitel,"\" am ", day(df_temp$Datum),".",month(df_temp$Datum),".", lubridate::year(df_temp$Datum),
                 "\nist der Verleiherrechnungsbetrag kleiner als die Mindestgarantie.",
                 "\nBitte im Programm korrigieren!\n"
  )
  )  
}

#### Programm check ####
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


##### Je nach Verleiher müssen die Kinoförderer als Umsatz abgerechnet werden. #####

df_temp <- df_Eintritt|>
  left_join(df_Abrechnung,
            by = "ID_Programm"
            )|>
  select(-Filmtitel.y, -Suisanummer.y, -Datum.y)|>
  rename(Filmtitel = Filmtitel.x,
         Suisanummer = Suisanummer.x,
         Datum = Datum.x
         )
df_temp

df_temp <- df_temp|>
  mutate(
    `Verkaufspreis Abgerechnet [CHF]` = 
      if_else(((Platzkategorie %in% l_data$`Platzkategorien zum Verrechnen`$Kinoförderer) & (!`Kinoförderer gratis?`)),
              l_data$`Platzkategorien zum Verrechnen`$Verkaufspreis[1],
              Verkaufspreis
      ),
    `Umsatz für Netto3 [CHF]` = Anzahl * `Verkaufspreis Abgerechnet [CHF]`)|>
  arrange(desc(Datum))
df_temp

df_Abrechnung <- df_temp|>
  select(c("ID_Programm",-"Spieldatum","Datum", "Zeit","Link ID", "Suisanummer",
           "Platzkategorie","Zahlend","Verkaufspreis","Anzahl","Umsatz",
           "SUISA-Vorabzug","Filmtitel",
           "Verleiher",
           "Abzug [%]","Minimal Abzug [CHF]","Abzug fix [CHF]","Kinoförderer gratis?",
           "Bezeichnung","Verleiherrechnungsbetrag [CHF]","Umsatz für Netto3 [CHF]","Verkaufspreis Abgerechnet [CHF]"
           )
         )|>
  rename(`SUISA-Vorabzug [%]` = `SUISA-Vorabzug`)|>
  arrange(Datum)

# error handlin Verleiherrechnung nicht vorhanden
df_temp <- df_Abrechnung|>
  filter(is.na(`Verleiherrechnungsbetrag [CHF]`))|>
  distinct(ID_Programm, .keep_all = T)
df_temp

# Error handling: Keine Verleiherrechnung vorhanden
warning(paste0("Achtung für den Film \"", df_temp$Filmtitel,"\" am ", day(df_temp$Datum),".",month(df_temp$Datum),".", lubridate::year(df_temp$Datum),
               "\nmit der Suisanummer ", df_temp$Suisanummer,
               " gibt es keine Verleiherrechnung.",
               "\nBitte in den Ausgaben, Kategorie Verleiher korrigieren.\n\n"))


####################################  Gemeinsame Abrechnung erstellen #################################### 
df_mapping <- l_data$Programm|>
  distinct(ID_Programm, .keep_all = T)|>
  select(1:5)|>
  filter(!is.na(ID_Programm))|>
  filter(Datum < Sys.Date())
df_mapping

# find all connected Filmvorführungen from Programm and remove all already connected to any gemeinsam abgerechnte
l_abrechnung <- inspect_link_ids(df_mapping)|>
  nullify_used_entries()
l_abrechnung

cnt <- 1
ID <- "6"
for (ID in names(l_abrechnung)) {
  IDs <- l_abrechnung[[ID]]
  df_temp <- df_Abrechnung|>
    filter(ID_Programm %in% IDs)|>
    group_by(ID_Programm)|>
    reframe(`Umsatz [CHF]`= sum(Umsatz),
            `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`)
    )
  df_temp
  
  # Sind die Eintritte daten für jede Filmvorführung vorhanden?
  if(nrow(df_temp) !=  nrow(l_data$Programm|>filter(ID_Programm %in% IDs))){
    temp <- l_data$Programm|>
      filter(ID_Programm %in% IDs)
    temp <- temp|>
      filter(!(temp$ID_Programm %in% df_temp$ID_Programm))
    warning(paste("Für den Film ", temp$Suisanummer[1], temp$Filmtitel[1], "gibt es keine Eintritte. ",
                  # "\nDie gemeinsame Abrechnung über mehrere Spieldaten wird nicht korrekt berechnet.",
                  "\nBitte Eintritte herunterladen und abspeichern!\n\n"))
    next
  }
  
  # Umsatzverteilprodukt berechnen für die gemeinsame Abrechnung
  df_Verteilprodukt <- df_Abrechnung|>
    filter(ID_Programm %in% IDs)|>
    group_by(ID_Programm)|>
    reframe(`Umsatz [CHF]`= sum(Umsatz),
            `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`)
            )|>
    mutate(Verteilprodukt_1 = `Umsatz [CHF]` / sum(`Umsatz [CHF]`),
           Verteilprodukt_2 = `Umsatz für Netto3 [CHF]` / sum(`Umsatz für Netto3 [CHF]`)
           )|>
    left_join(l_data$Programm|>select(1:6)|>select(-`Link ID`),
              by = join_by(ID_Programm)
              )
  df_Verteilprodukt
  
  # Eintritte
  df_Eintritte <- df_Abrechnung|>
    filter(ID_Programm %in% IDs)
  df_Eintritte
  
  # Umsatz 
  Abrechnung <-
    bind_cols(
      l_data$Programm|>
        select(1:11)|>
        slice(as.integer(ID)),
      df_Abrechnung |>
        filter(ID_Programm %in% IDs) |>
        reframe(
          `Umsatz [CHF]` = sum(Umsatz),
          `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`)
        )
    )|> # Kinoförderer
    left_join(l_data$Verleiher|>
                select(Verleihername,`Kinoförderer gratis?`)|>
                mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "ja", 
                                                        T,F)),
              by = c(Verleiher = "Verleihername")
              )
  Abrechnung
  
  # Suisavorabzug
  Abrechnung <- 
    bind_cols(Abrechnung,
              `SUISA-Vorabzug [%]` = df_Eintritt$`SUISA-Vorabzug`[1])

  Abrechnung$`Kinoförderer gratis?`
  Abrechnung$`SUISA-Vorabzug [%]`  
  Abrechnung$`Umsatz für Netto3 [CHF]`
  Abrechnung$`Umsatz [CHF]`
  
  df_temp <- df_Verteilprodukt|>
    filter(ID_Programm == as.integer(ID))
  df_temp
  
  if(nrow(df_temp) == 0) stop("Kein Verteilprodukt vorhanden")
  
  # Netto 3
  Abrechnung <- Abrechnung|>
    mutate(
      Verteilprodukt =  if_else(`Kinoförderer gratis?`, df_Verteilprodukt$Verteilprodukt_1[as.integer(ID)],df_Verteilprodukt$Verteilprodukt_2[as.integer(ID)]), # Umsatzverteilprodukt
      `SUISA-Vorabzug [CHF]` = sum(`Umsatz [CHF]`) * (`SUISA-Vorabzug [%]` /100) * Verteilprodukt,
      `Netto3 [CHF]` = if_else(`Kinoförderer gratis?`, # Der Suisa-Vorabzug muss anders berechnet werden wenn die Kinoförderer verrechnet werden müssen
                               (`Umsatz [CHF]` - sum(`Umsatz [CHF]` * (`SUISA-Vorabzug [%]` /100))) * df_temp$Verteilprodukt_1,
                               (`Umsatz für Netto3 [CHF]` - sum(`Umsatz für Netto3 [CHF]` * (`SUISA-Vorabzug [%]` / 100))) * df_temp$Verteilprodukt_2
      )
    )
  
  if(is.na(Abrechnung$`Netto3 [CHF]`)) stop("Could not calculate Nett3 [CHF] for ", Abrechnung$ID_Programm, Abrechnung$Filmtitel, Abrechnung$Suisanummer)
  
  # Verleiherrechung
  df_temp <- Einnahmen_und_Ausgaben$Ausgaben|>
    filter(Kategorie == "Verleiher", Suisanummer == Abrechnung$Suisanummer)
  df_temp
  
  if(nrow(df_temp) > 0){
    Abrechnung <- bind_cols(Abrechnung, `Verleiherrechnungsbetrag [CHF]` = df_temp$`Betrag [CHF]`)
  }else{
    Abrechnung <- bind_cols(Abrechnung, `Verleiherrechnungsbetrag [CHF]` = NA)
  }
  Abrechnung$`Verleiherrechnungsbetrag [CHF]`
  
  # Je nach dem ob ein fixer betrag oder Prozentualeabgaben mit dem Verleiher vereinbart wurden muss anders gerechnet werden. 
  if((!is.na(Abrechnung$`Abzug fix [CHF]`[1])) > 0){ 
    # fixer Betrag inklusive Mehrwertsteuer mit dem Verleiher vereinbart! 
    Abrechnung <- 
      Abrechnung|>
      mutate(
        `Verleiherabzug [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                         `Abzug fix [CHF]`[1] * Verteilprodukt,     # keine Verleiherrechnung vorhanden
                                         `Verleiherrechnungsbetrag [CHF]` * Verteilprodukt),  # Verleiherrechnung ist vorhanden
        `MWST [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                               sum(`Verleiherabzug [CHF]`) * (l_data$MWST$MWST / 100) * Verteilprodukt,
                               (`Verleiherrechnungsbetrag [CHF]`[1] - (`Verleiherrechnungsbetrag [CHF]`[1] / (1+(l_data$MWST$MWST/100)))) * Verteilprodukt
                               )
        )
  }else{ 
    # Prozentualerabzug mit dem Verleiher vereinbart! 
    Abrechnung<- 
      Abrechnung|>
      mutate(
        `Verleiherabzug [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                         sum(`Netto3 [CHF]`) * (`Abzug [%]`[1] / 100) * Verteilprodukt, # keine Verleiherrechnung vorhanden
                                         `Verleiherrechnungsbetrag [CHF]` * Verteilprodukt              # Verleiherrechnung ist vorhanden
        ), 
        `Verleiherabzug [CHF]` = if_else(`Verleiherabzug [CHF]` > (`Minimal Abzug [CHF]`[1] * Verteilprodukt),
                                         sum(`Verleiherabzug [CHF]`) * Verteilprodukt,
                                         `Minimal Abzug [CHF]`[1] * Verteilprodukt
        ),
        `MWST [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                               sum(`Verleiherabzug [CHF]`) * (l_data$MWST$MWST / 100) * Verteilprodukt,
                               (`Verleiherrechnungsbetrag [CHF]`[1] - (`Verleiherrechnungsbetrag [CHF]`[1] / (1+(l_data$MWST$MWST/100)))) * Verteilprodukt
        )
      )
  }
  Abrechnung
  
  # Gewinn/Verlust Tickets
  Abrechnung <- 
    Abrechnung|>
    mutate(`Gewinn/Verlust Tickets [CHF]` = 
             if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                     `Umsatz [CHF]` - ((`SUISA-Vorabzug [CHF]` + `Verleiherabzug [CHF]` + `MWST [CHF]`) * Verteilprodukt) ,
                     `Umsatz [CHF]` - ((`SUISA-Vorabzug [CHF]` + `Verleiherrechnungsbetrag [CHF]`) * Verteilprodukt))
           )
  Abrechnung
  
  # Verteilen der Eventeinnahmen
  df_Einnahmen <- Einnahmen_und_Ausgaben$Einnahmen|>
    filter(Kategorie == "Event" & (Einnahmen_und_Ausgaben$Einnahmen$Datum %in% (l_data$Programm|>filter(ID_Programm %in% IDs)|>select(Datum)|>pull())))|>
    mutate(`Betrag [CHF]` = Abrechnung$Verteilprodukt * `Betrag [CHF]`
    )
  df_Einnahmen  
  
  # Verteilen der Eventausgaben
  df_Ausgaben <- Einnahmen_und_Ausgaben$Ausgaben|>
    filter(Kategorie == "Event" & (Einnahmen_und_Ausgaben$Ausgaben$Datum %in% (l_data$Programm|>filter(ID_Programm %in% IDs)|>select(Datum)|>pull())))|>
    mutate(`Betrag [CHF]` = Abrechnung$Verteilprodukt * `Betrag [CHF]`
    )
  df_Ausgaben 
  
  # Gewinn Kiosk (wird nie verteilt, da der Verkauf pro Datum und Suisanummer erfolgt)
  df_KioskGewinn <- 
    df_Kiosk|>
    filter(ID_Programm == as.integer(ID))|>
    reframe(Kassiert = sum(Kassiert, na.rm = T),
            Gewinn = sum(Gewinn, na.rm = T))
  df_KioskGewinn
  
  # Update results 
  l_abrechnung[[ID]] <- list(
    Eintritte = df_Eintritt|>
      filter(ID_Programm == as.integer(ID))|>
      select(-`SUISA-Vorabzug`),
    Kiosk = df_Kiosk|>
      filter(ID_Programm %in% IDs),
    Verteilprodukt = df_Verteilprodukt,
    Abrechnung = Abrechnung,
    `Gewinn/Verlust Tickets [CHF]` = Abrechnung$`Gewinn/Verlust Tickets [CHF]`,
    `Eventeinnahmen [CHF]` = df_Einnahmen$`Betrag [CHF]`,
    `Eventausgaben [CHF]` = df_Ausgaben$`Betrag [CHF]`,
    `Gewinn/Verlust Kiosk [CHF]` = df_KioskGewinn$Gewinn,
    `Überschuss / Manko Kiosk [CHF]`= df_manko_uerberschuss|>
      filter(ID_Programm == as.integer(ID))|>
      select(`Überschuss / Manko`)|>
      pull()
    )
  
  l_abrechnung[[ID]] <-
    list(Eintritte = df_Eintritt|>
           filter(ID_Programm == as.integer(ID))|>
           select(-`SUISA-Vorabzug`),
         Kiosk = df_Kiosk|>
           filter(ID_Programm %in% IDs),
         Verteilprodukt = df_Verteilprodukt,
         Abrechnung = Abrechnung,
         `Gewinn/Verlust Tickets [CHF]` = Abrechnung$`Gewinn/Verlust Tickets [CHF]`,
         `Eventeinnahmen [CHF]` = df_Einnahmen$`Betrag [CHF]`,             
         `Eventausgaben [CHF]` = df_Ausgaben$`Betrag [CHF]`,
         `Gewinn/Verlust Kiosk [CHF]` = df_KioskGewinn$Gewinn,
         `Überschuss / Manko Kiosk [CHF]`= df_manko_uerberschuss|>
           filter(ID_Programm == as.integer(ID))|>
           select(`Überschuss / Manko`)|>
           pull(),
         `Gewinn/Verlust Filmvorführungen [CHF]` = 
           (l_abrechnung[[ID]]$`Gewinn/Verlust Tickets [CHF]` + l_abrechnung[[ID]]$`Gewinn/Verlust Kiosk [CHF]`+ 
              l_abrechnung[[ID]]$`Überschuss / Manko Kiosk [CHF]` + sum(l_abrechnung[[ID]]$`Eventeinnahmen [CHF]`) - 
              sum(l_abrechnung[[ID]]$`Eventausgaben [CHF]`))
         )
}
remove(df_Verteilprodukt, df_Film, df_KioskGewinn, temp, l_data_sql, df_mapping, df_Eintritte, df_temp)

l_abrechnung
warnings()
length(l_abrechnung)
l_abrechnung[["1"]]
l_abrechnung[["2"]]
l_abrechnung[["3"]]
l_abrechnung[["4"]]
l_abrechnung[["5"]]
l_abrechnung[["6"]]

l_abrechnung|>
  lapply(function(x){
    
  })


# ##################  Abrechnung Filmvorführung erstellen (für Berichte verwendet) ##################
# Abrechnung Tickets erstellen (für Berichte verwendet)

# Abrechnungen entfehrnen die nich vaild sind
l_abrechnung <- l_abrechnung|>
  lapply(function(x){
    if(length(names(x)) > 0) x
    else NULL
  })

remove_nulls <- function(lst) {
  Filter(Negate(is.null), lst)
}
l_abrechnung <-  remove_nulls(l_abrechnung)


df_Abrechnung_tickes <- l_abrechnung|>
  lapply(function(x){
    x$Eintritte
  })|>
  bind_rows(.id = "ID_Programm")
df_Abrechnung_tickes


# Abrechnung Kiosk erstellen  (für Berichte verwendet)
df_Abrechnung_kiosk <- l_abrechnung|>
  lapply(function(x){
    x$Kiosk
  })|>
  bind_rows(.id = "ID_Programm")
df_Abrechnung_kiosk


# Abrechnung Events erstellen (für Berichte verwendet)
df_Abrechnung_Eventeinnahmen <- l_abrechnung|>
  lapply(function(x){
    x$`Eventeinnahmen [CHF]`
  })|>
  bind_rows(.id = "ID_Programm")
df_Abrechnung_Eventeinnahmen

df_Abrechnung_Eventausgaben <- l_abrechnung|>
  lapply(function(x){
    x$`Eventausgaben [CHF]`
  })|>
  bind_rows(.id = "ID_Programm")
df_Abrechnung_Eventausgaben


# summary Eintritt (für Berichte verwendet)
df_Besucherzahlen <- df_Eintritt|>
  group_by(Datum, Filmtitel, Suisanummer)|>
  reframe(Besucher = sum(Anzahl))
df_Besucherzahlen

################## write to Excel ##################
c_filePath <- "output/data/"
if(!dir.exists(c_filePath)) dir.create(c_filePath, recursive = T )

list(`Werbung` = df_Besucherzahlen,
     `Tickets` = df_Abrechnung_tickes,
     `Kiosk` = df_Abrechnung_kiosk,
     `Eventeinnahmen` = df_Abrechnung_Eventeinnahmen,
     `Eventausgaben` = df_Abrechnung_Eventausgaben,
     `Überschuss Manko` = df_manko_uerberschuss,
     `Filmvorführung` = df_Abrechnung
       )|>
  write.xlsx(file="output/data/Auswertung.xlsx", asTable = TRUE, overwrite = TRUE)


# remove not used variables
remove(c_file,
       ii,
       c_filePath
       )

# user interaction
writeLines("Good ... Berechnungen erfolgt")
