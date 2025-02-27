# read and caluclate all input data
library(rebus)
library(openxlsx)
library(lubridate)
library(tidyverse)

writeLines("Daten werden einlesen und berechnet...")

source("source/functions.R")

# Eintritte aus Advanced Tickets files
convert_data_Film_txt <- function(fileName) {
  l_Eintritt <- fileName|>
    lapply(function(fileName){
      
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
      
      l_temp[[ii]] |>
        mutate(`Suisa Nummer` = l_temp[[1]],
               Filmtitel = l_temp[[2]],
               Datum_ = l_temp[[3]],
               `SUISA-Vorabzug` = l_temp[[4]]
        )
    })
  names(l_Eintritt) <- fileName|>
    str_extract(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))
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
    Suisanummer =  c_kiosk_suisa[ii]
    )
  }
  names(l_extracted) <- c_fileDate
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
      
      l_Kiosk[[ii]] <- bind_cols(Verkaufsartikel = l_Kiosk[[ii]][,1], x, tibble(Suisanummer = c_suisanummer[ii]))
      
    }else if(c_lenght[ii] == 5){ # keine Korrekturbuchungen
      l_Kiosk[[ii]] <- l_Kiosk[[ii]][,c(1:3,5)]
      x <- l_Kiosk[[ii]][,2:ncol(l_Kiosk[[ii]])]|>
        apply(2, as.numeric)
      colnames(x) <- c("Einzelpreis", "Anzahl", "Betrag")
      
      l_Kiosk[[ii]] <- bind_cols(Verkaufsartikel = l_Kiosk[[ii]][,1], x, tibble(Suisanummer = c_suisanummer[ii]))
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
    bind_rows(.id = "Datum")|>
    mutate(Datum = dmy(Datum),
           Einzelpreis = if_else(is.na(Einzelpreis), Betrag / Anzahl, Einzelpreis),
           Betrag = if_else(Anzahl == 0, 0, Betrag))
  
  # Extrakt Überschuss / Manko
  l_return[["Überschuss / Manko"]] <- l_extracted |>
    lapply(function(x) {
      cbind(x[["Überschuss / Manko"]],
            tibble(Suisanummer = x[["Suisanummer"]]))
    })|>
    bind_rows(.id = "Datum")|>
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

# Einlesen Input daten
c_file <- "Input/Data.Rds"
if(file.exists(c_file)){
  l_data <- readRDS(c_file)
  c_backup_number <- length(list.files(path = "Input/backup", pattern = "backup"))
  if(!dir.exists("Input/backup")) dir.create("Input/backup")
  saveRDS(l_data, paste0("Input/backup/Data_backup",c_backup_number + 1,".Rds")) # Save the updated list to the file
}else{ # or load template date 
  c_file <- "Input/template.Rds"
  l_data <- readRDS(c_file)
  c_file <- "Input/Data.Rds"
}

# l_data$MWST <-
#   tibble(
#     MWST = 8.1
#   )
# saveRDS(l_data, c_file) # Save the updated list to the file

# Einnahmen und Ausgaben einlesen
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

# Datum  vorhanden für Kategorie Verleiher / Event in den Ausgaben
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

# Eintritt aus Advanced Tickets
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

df_Eintritt <- l_Eintritt|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum),
         Datum_ = lubridate::dmy(Datum_)
  )

# error handling: check file datum in file name vs in file datum found in the file
df_temp <- df_Eintritt|>
  filter(!Datum%in%Datum_)|>
  distinct(Datum,.keep_all = T)

if(nrow(df_temp)>0){
  stop(paste0("Im file \".../Kinoklub/Input/advance tickets/Eintritt ",day(df_temp$Datum),".",month(df_temp$Datum),".", year(df_temp$Datum)-2000, "\"",
              "\nwurde ein anderes Datum gefunden: ", day(df_temp$Datum_),".",month(df_temp$Datum_),".", year(df_temp$Datum_),
              "\nBitte korrigieren!",
              collapse = "\n")
  )
}

# create data frame
df_Eintritt <- l_Eintritt|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum),
         Datum_ = NULL,
         Verkaufspreis = Preis ,
         Tax = NULL, 
         Zahlend = if_else(Verkaufspreis == 0, F, T))|>
  select(Datum, Filmtitel,`Suisa Nummer`,Platzkategorie,Zahlend,Verkaufspreis, Anzahl,Umsatz,`SUISA-Vorabzug`)
df_Eintritt

# Filmvorführungen
df_Flimvorfuerungen <- l_Eintritt|>
  lapply( function(x){ 
    distinct(x, Datum_,`Suisa Nummer`)
  })|>
  bind_rows()|>
  mutate(Datum = Datum_|>dmy()|>as.Date())

# Bericht mapping
df_mapping <- tibble(Datum = df_Flimvorfuerungen$Datum,
                     Suisanummer = df_Flimvorfuerungen$`Suisa Nummer`)|>
  mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
         index = row_number())
remove(df_Flimvorfuerungen)
df_mapping

# Kioskabrechnungen
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
df_Kiosk <- l_temp$df_Kiosk
df_manko_uerberschuss <- l_temp$`Überschuss / Manko`
remove(l_temp)


# Spez Verkaufsartikel / Spezialpreise einlesen

# errohandling
c_file <- "Spezialpreisekiosk.xlsx"

paste0("Input/", c_file)|>
  file.exists()|>
  stopifnot()

# Spezialpreise einlesen
Spezialpreisekiosk <- l_data$Spezialpreisekiosk
Spezialpreisekiosk

# error handling
if(is.na(Spezialpreisekiosk$Suisanummer)|>sum() > 0) stop("\nEs wurden nicht alle Suisanummern im file:\n .../Kinoklub/input/Spezialpreisekiosk.xlsx definiert")

# error handling
# Sind alle Spezialpreise pro Datum und Suisanummer definiert?  
df_spez_preis_na <- df_Kiosk|>
  filter(str_detect(Verkaufsartikel, "Spez")) |>
  left_join( # look up Spezialpreise
    Spezialpreisekiosk, 
    by = c(Datum = "Datum", Suisanummer = "Suisanummer", Verkaufsartikel = "Spezialpreis")
  )|>
  filter(is.na(Artikelname))

df_spez_preis_na <- df_spez_preis_na|>
  left_join(df_Eintritt|> # look up Filmtitel
              distinct(Filmtitel,.keep_all = T),
            by = c(Datum = "Datum", Suisanummer = "Suisa Nummer")
            )
df_spez_preis_na

if(nrow(df_spez_preis_na) > 0) {
  warning(
    paste0(
      "\nFür die Filmvorführung ", df_spez_preis_na$Filmtitel, " am ", day(df_spez_preis_na$Datum),".",month(df_spez_preis_na$Datum),".",year(df_spez_preis_na$Datum),
      " / ", df_spez_preis_na$Suisanummer,
      "\nwurde der Artikel ", df_spez_preis_na$Verkaufsartikel," nicht definiert.",
      "\nBitte korrigieren in der Datei:","\n.../Kinoklub/input/Spezialpreisekiosk.xlsx\n"
    )
  )
}


# join Spezpreise mit Verkaufsartikel
df_Kiosk <- df_Kiosk|>
  left_join(Spezialpreisekiosk, 
            by = c(Datum ="Datum", Verkaufsartikel = "Spezialpreis", Suisanummer = "Suisanummer")
  )|>
  mutate(Verkaufsartikel = if_else(is.na(Artikelname), Verkaufsartikel, Artikelname))|>
  select(-Artikelname)


# Kiosk Einkaufspreise 
# read Einkaufspreise 
c_files <- list.files(pattern = START%R%"Einkauf", recursive = T)
l_Einkaufspreise <- lapply(c_files, readxl::read_excel)
l_Einkaufspreise

p <- one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)
names(l_Einkaufspreise) <- c_files|>str_extract(p)

df_Einkaufspreise <- l_Einkaufspreise |>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum)|>as.Date())
df_Einkaufspreise


# Kioskeinkauf 
c_files <- list.files(c_path,pattern = START%R%"Kiosk", recursive = T)
c_files <- c_files <- paste0(c_path,"/", c_files)
c_files

c_Date_Kiosk <- c_files|>
  str_extract(DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT)|>
  dmy()|>
  as.Date()
c_Date_Kiosk

c_Einkaufslistendatum <- distinct(df_Einkaufspreise, Datum)|>pull()
c_Einkaufslistendatum

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
                filter(Datum == df_Mapping_Einkaufspreise$Einkaufspreise[ii])|>
                select(-Datum), 
              by = c(Verkaufsartikel = "Artikelname Kassensystem")
    )
}
l_Kiosk

df_Kiosk <- l_Kiosk|>
  bind_rows()

# V1.5 Merge Verkaufsartikel "Popcorn frisch", "Popcorn Salz" zu "Popcorn frisch"
df_Kiosk <- bind_rows(df_Kiosk|>
                        filter(Verkaufsartikel %in% c("Popcorn frisch", "Popcorn Salz"))|>
                        mutate(Verkaufsartikel = "Popcorn frisch"),
                      df_Kiosk|>
                        filter(! Verkaufsartikel %in% c("Popcorn frisch", "Popcorn Salz"))
)


# Gewinn
df_Kiosk <- df_Kiosk|>
  rename(Einkaufspreis = `Einkaufs- preis`)|>
  mutate(Gewinn = if_else(is.na(Einkaufspreis),Betrag,Betrag-(Anzahl*Einkaufspreis))
  )|>
  rename(Kassiert = Betrag,
         Verkaufspreis = Einzelpreis)

# Test
df_Kiosk|>
  group_by(Datum)|>
  reframe(`Gewinn/Verlust` = sum(Kassiert))

# remove no more needed variables
remove(df_Mapping_Einkaufspreise,l_Kiosk, l_Einkaufspreise,
       df_verkaufsartikel,
       c_Date_Kiosk, c_Einkaufslistendatum,
       p,
       ii,
       c_path, c_files)


# Bericht mapping
n_kiosk <- df_Kiosk|>distinct(Datum, .keep_all = T)
n_Film <- df_Eintritt|>distinct(Datum, .keep_all = T )


# Error handling
if(n_kiosk|>nrow() > n_Film|>nrow()){
  df_temp <- anti_join(n_kiosk,n_Film, by = "Datum")|>
    select(Datum)
  
  stop(paste0("Es fehlt eine Datei: Eintritt ", day(df_temp$Datum),".",month(df_temp$Datum), ".",year(df_temp$Datum), ".txt\"",
              "\nBitte herunterladen unter: https://www.advance-ticket.ch/decomptefilms?lang=de"
  )
  )
}else if(df_Kiosk|>distinct(Datum)|>nrow() < df_Eintritt|>distinct(Datum)|>nrow()){
  
  df_temp <- anti_join(n_Film, n_kiosk, by = "Datum")|>
    select(1:3)
  stop(paste0("Es fehlt einen Kioskabrechnug zum Film:\n", 
              df_temp$Filmtitel, " am ", day(df_temp$Datum),".",month(df_temp$Datum), ".",year(df_temp$Datum),
              "\n\nBitter herunterladen unter: https://www.advance-ticket.ch/decomptecaisse?lang=de"
  ))
}


# show times
# error handling file not found
c_file <- "Input/advance tickets/Shows.txt"
c_raw <- paste0("Die Datei \"Shows.txt\" konnte nicht gefunden werden:",
                "\nBitte die Datei über GUI hochladen oder abspeichern unter \".../Kinoklub/",c_file, "\".",
                "\nBitte herunterladen von https://www.advance-ticket.ch/shows?lang=de")
if(!file.exists(c_file)) stop(c_raw)

# read file
c_raw <- readLines(c_file)|>
  suppressWarnings()

c_select <- tibble(found = str_detect(c_raw, "Tag"))|>
  mutate(index = row_number(),
         index = if_else(found, index, NA))|>
  filter(!is.na(index))|>
  arrange(index)|>
  slice(1)|>
  pull()
c_select

m <- c_raw[c_select:length(c_raw)]|>
  str_split("\t", simplify = T)
m
c_names <- m[1,m[1,] != ""]

m <- m[2:nrow(m),m[1,] != ""]
colnames(m) <- c_names
m

df_show <- m|>
  as_tibble()|>
  mutate(Datum = Tag|>lubridate::ymd(),
         Anfang = parse_time(Anfang),
         Ende = parse_time(Ende))|>
  select(Datum,Anfang, Ende, Saal, Titel, Version, Alter)|>
  rename(Filmtitel = Titel)|>
  arrange(Datum)

df_show

df_show <- df_show|>
  left_join(df_Eintritt|>
              distinct(Datum, `Suisa Nummer`, Filmtitel),
            by = c(Datum = "Datum", Filmtitel = "Filmtitel")
  )|>
  arrange(Datum)

df_show <- df_show|>
  filter(!is.na(`Suisa Nummer`))
df_show

## error handling 
df_temp <- df_Eintritt|>distinct(Datum, `Suisa Nummer`)|>
  anti_join(df_show, by = join_by(Datum, `Suisa Nummer`))|>
  left_join(df_Eintritt, by = join_by(Datum, `Suisa Nummer`))|>
  distinct(Datum, .keep_all = T)
df_temp

if(nrow(df_temp) != 0) {
  stop(paste0(
    "Für den Film: ",df_temp$Filmtitel, " am ", 
    day(df_temp$Datum),".",month(df_temp$Datum),".",year(df_temp$Datum), 
    " gibt es keinen Eintrag in der Datei .../Kinoklub/Input/advance tickets/show.txt\nBitte herunterladen und abspeichern\nhttps://www.advance-ticket.ch/shows?lang=de")
  )}


# Abos und Kinogutscheine
if(!file.exists("Input/advance tickets/atelierkino_abo.txt")) {
  stop(paste0("Die Datei: \".../Input/advance tickets/atelierkino_abo.txt\" wurde nicht gefunden.",
       "\nBitte herunterladen unter: https://www.advance-ticket.ch/abos?lang=de"))
  }
atelierkino_abo <- read_delim("Input/advance tickets/atelierkino_abo.txt", 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                               first_use = col_date(format = "%Y-%m-%d"), 
                                               last_use = col_date(format = "%Y-%m-%d"), 
                                               expiration = col_date(format = "%Y-%m-%d"), 
                                               count_use = col_integer()), trim_ws = TRUE)

if(!file.exists("Input/advance tickets/atelierkino_foerderer.txt")) {
  stop(paste("Die Datei: .../Input/advance tickets/atelierkino_foerderer.txt wurde nicht gefunden.",
         "\nBitte herunterladen unter: https://www.advance-ticket.ch/abos?lang=de"))
  }
atelierkino_foerderer <- read_delim("Input/advance tickets/atelierkino_foerderer.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                                     first_use = col_date(format = "%Y-%m-%d"), 
                                                     last_use = col_date(format = "%Y-%m-%d"), 
                                                     expiration = col_date(format = "%Y-%m-%d"), 
                                                     count_use = col_integer()), trim_ws = TRUE)

if(!file.exists("Input/advance tickets/atelierkino_gutschein.txt")) {
  stop(paste("Die Datei: .../Input/advance tickets/atelierkino_gutschein.txt wurde nicht gefunden.",
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


# Verleiherabgaben einlesen
c_file <- "Input/Verleiherabgaben.xlsx"

# error handling
if(!file.exists(c_file)) stop(paste0("\nDie Datei: \".../", c_file, "\" konnte nicht gefunden werden"))


df_verleiherabgaben <- l_data$Verleiherabgaben|>
  left_join(l_data$Verleiher,
            by = c("Verleiher" = "Verleihername"))
df_verleiherabgaben


# Suisa automatisch korrigieren 
df_verleiherabgaben$Suisanummer <- df_verleiherabgaben$Suisanummer|>
  str_squish()|>
  str_extract(pattern = DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT) 

# error handling 
df_temp <- df_verleiherabgaben|>
  filter(is.na(Verleiher))

if(nrow(df_temp)>0){
  stop(paste0("\nEs gibt keinen Verleiher für den Film, ",df_temp$Titel," am ",day(df_temp$Datum), ".", month(df_temp$Datum), ".", year(df_temp$Datum),".",   
              "\nBitte korrrigieren in der Exceldatei .../Kinoklub/input/Verleiherabgaben.xlsx"))
}

# Eintrite 
df_Eintritt <- df_Eintritt|>
  left_join(df_verleiherabgaben|>
              select(-Filmtitel, -Adresse, -PLZ, -Ort),
            by = c(`Suisa Nummer` = "Suisanummer", "Datum"))|>
  mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein", F, T),
         Zahlend = if_else(Verkaufspreis>0, T, F))
df_Eintritt


# Errorhandling 
# kein prozentualer noch fixer abzug definiert
df_temp <- df_Eintritt|>
  filter(is.na(`Abzug [%]`) & is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){ 
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              " wurde kein Abzug definiert.",
              "\nBitte korrigieren im File:",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx korrigieren.")
  )
}

# kein minimal Abzug definiert (Es muss kein minimaler Abzug definiert werden falls ein Abzug definiert wurde)
df_temp <- df_Eintritt|>
  filter(is.na(`Minimal Abzug [CHF]`) & !is.na(`Abzug [%]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0) stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                                "\nwurde werder ein Minimalabzug noch ein Fixabzug definiert.",
                                "\nBitte korrigieren im File:",
                                "\n.../Kinoklub/input/Verleiherabgaben.xlsx korrigieren.")
)

# Prozentualer und Fixer Abzug definiert
df_temp <- df_Eintritt|>
  filter(!is.na(`Abzug [%]`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){ 
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde ein Prozentualer und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
              "\nBitte korrigieren im File:",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx korrigieren.")
  )
}

# minimal und Fixer Abzug definiert
df_temp <- df_Eintritt|>
  filter(!is.na(`Minimal Abzug [CHF]`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde ein minimal Abzug und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
              "\nBitte korrigieren im File",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx")
  )
}


# Ticketabrechnung vorbereiten
df_Abrechnung <- df_Eintritt|>
  distinct(Datum, `Suisa Nummer`, .keep_all = TRUE)|>
  select(-(4:8))|>
  left_join(df_show|>
              select(Datum, `Suisa Nummer`, Anfang, Ende),
            by = join_by(Datum, `Suisa Nummer`))|>
  rename(Suisanummer = `Suisa Nummer`)|>
  left_join( # Verleiherrechnungen 
    Einnahmen_und_Ausgaben[["Ausgaben"]]|>
      filter(Kategorie == pull(l_data$Kategorie)[6])|> # suchen nach den Verleiher Einträgen
      select(Spieldatum, Suisanummer,`Betrag [CHF]`)|>
      # select(1:2)|>
      rename(`Verleiherrechnungsbetrag [CHF]` = `Betrag [CHF]`,
             Datum = Spieldatum),
    by = join_by(Datum, Suisanummer)
  )|>
  rename(`SUISA-Vorabzug [%]` = `SUISA-Vorabzug`,
         )|>
  mutate(Datum = as.Date(Datum))
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
if(nrow(df_temp)>0) {
  warning(paste0("\nAchtung für den Film \"", df_temp$Filmtitel,"\" am ", day(df_temp$Datum),".",month(df_temp$Datum),".", lubridate::year(df_temp$Datum),
                 "\nist der Verleiherrechnungsbetrag kleiner als die Mindestgarantie.",
                 "\nBitte korrigieren in der Datei: .../Kinokulb/input/Verleiherabgaben.xlsx\n"
  )
  )  
}
remove(m, df_temp, n_Film, n_kiosk)


# Je nach Verleiher müssen die Kinoförderer als Umsatz abgerechnet werden. 
# Für die Berechnung von "Netto 3" müssen die Kinoförder als Umsatz verrechnet werden.
# Netto 3 = Umsatz minus SUISA-Vorabzug.
df_Eintritt <- bind_rows(
  df_Eintritt|>
    filter(!`Kinoförderer gratis?`)|>
    mutate(
      `Umsatz für Netto3 [CHF]` = if_else(Platzkategorie %in% l_data$`Platzkategorien zum Verrechnen`$Kinoförderer,
                                    Anzahl * l_data$`Platzkategorien zum Verrechnen`$Verkaufspreis[1],
                                    Umsatz
                                    ),
      `Verkaufspreis Abgerechnet [CHF]` = `Umsatz für Netto3 [CHF]` / Anzahl
    ),
  df_Eintritt|>
    filter(`Kinoförderer gratis?`)|>
    mutate(`Umsatz für Netto3 [CHF]` = Umsatz)
)|>
  arrange(desc(Datum))
df_Eintritt <- df_Eintritt|>
  rename(Suisanummer = `Suisa Nummer`)
df_Eintritt

# Abrechnungsperiode erstellen
l_keineRechnung <- list()
l_abrechnung <- list()
ii <- 6
for (ii in 1:nrow(df_mapping)) {

  l_abrechnung[[ii]] <- list(Abrechnung = df_Abrechnung|>
                               filter(df_mapping$Suisanummer[ii] ==  Suisanummer)|>
                               filter(Datum %in% c(df_mapping$Datum[ii], df_Abrechnung$`Link Datum`[ii]))|>
                               select(Datum, `Link Datum`, Anfang, Ende, Filmtitel, Suisanummer, Verleiher,`Verleiherrechnungsbetrag [CHF]`, 
                                      `SUISA-Vorabzug [%]`, `Link Datum`, `Minimal Abzug [CHF]`, `Abzug [%]`, `Abzug fix [CHF]`, `Kinoförderer gratis?`),
                             Tickets = df_Eintritt|>
                               filter(df_mapping$Suisanummer[ii] == Suisanummer)|>
                               filter(Datum %in% c(df_mapping$Datum[ii], df_Abrechnung$`Link Datum`[ii]))|>
                               select(Datum, Filmtitel, Suisanummer, Platzkategorie, Verkaufspreis, Anzahl, Umsatz, `Verkaufspreis Abgerechnet [CHF]`,`Umsatz für Netto3 [CHF]`)
                             )


  # error handling
  # Verleiherrechnung vorhanden?

  if(l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`|>sum(na.rm = T) > 0) {
    # Gemeinsame Abrechnung (Datum und link Datum): Verleiherrechnung wir auch auf dem Linkdatum eingetragen
    c_Verleiherrechnungsbetrag <- l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`[!is.na(l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`)]
    l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]` <- rep(c_Verleiherrechnungsbetrag, length(l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`))
    remove(c_Verleiherrechnungsbetrag)
  }else{
    # Error handling: Keine Verleiherrechnung vorhanden
    warning(paste0("\nAchtung für den Film \"", l_abrechnung[[ii]]$Abrechnung$Filmtitel,"\" am ",
                   day(l_abrechnung[[ii]]$Abrechnung$Datum),".",month(l_abrechnung[[ii]]$Abrechnung$Datum),".", lubridate::year(l_abrechnung[[ii]]$Abrechnung$Datum),
                   " / ", l_abrechnung[[ii]]$Abrechnung$Suisanummer,
                   "\ngibt es keine Verleiherrechnung. Bitte korrigieren in der Datei:",
                   "\n.../Kinokulb/input/Einnahmen und Ausgaben.xlsx\n")
    )
    # Rechnungen vorhanden (wird im Bericht verwendet)
    l_keineRechnung[[ii]] <- tibble(Datum = l_abrechnung[[ii]]$Abrechnung$Datum ,
                                    Filmtitel = l_abrechnung[[ii]]$Abrechnung$Filmtitel,
                                    Suisanummmer = l_abrechnung[[ii]]$Abrechnung$Suisanummer)
    
  }

  # Berechnung Umsatz für Netto3 (für gemeinsame Abrechnung verwendet)
  l_abrechnung[[ii]]$Abrechnung <- 
    bind_cols(
      l_abrechnung[[ii]]$Abrechnung,
      l_abrechnung[[ii]]$Tickets|>
        group_by(Datum)|>
        reframe(Umsatz = sum(Umsatz),
                `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`))|>
        select(-Datum)
    )
}
names(l_abrechnung) <- paste(df_mapping$Datum, df_mapping$Suisanummer)

df_keine_Rechnung <- l_keineRechnung|>
  bind_rows()
df_keine_Rechnung


# Einnahmen und Abgaben von mehreren Events verhältnismässig nach Umsatzzahlen 
# auf die gelinkten Filme aufteilen (Link im Excel file: .../Kinoklub/Input/Verleiherabgaben.xlsx ) 
ii <- 6

for (ii in 1:nrow(df_mapping)) {
  # umsatz- und Netto3 Umsatz-Berechnung
  l_abrechnung[[ii]]$Abrechnung <- 
    l_abrechnung[[ii]]$Abrechnung|>
    mutate(
      Verteilprodukt =  Umsatz / sum(Umsatz), # Umsatzverteilprodukt
      `SUISA-Vorabzug [CHF]` = sum(Umsatz) * (`SUISA-Vorabzug [%]` /100) * Verteilprodukt,
      `Netto3 [CHF]` = if_else(`Kinoförderer gratis?`, # Der Suisa-Vorabzug muss anders berechnet werden wenn die Kinoförderer verrechnet werden müssen
                       (sum(Umsatz) - sum(sum(Umsatz) * (`SUISA-Vorabzug [%]` /100) * Verteilprodukt)) * Verteilprodukt,
                       (sum(`Umsatz für Netto3 [CHF]`) - sum(sum(`Umsatz für Netto3 [CHF]`) * (`SUISA-Vorabzug [%]` / 100) *  Verteilprodukt))) * Verteilprodukt
    )
  l_abrechnung[[ii]]$Abrechnung  

  # Je nach dem ob ein fixer betrag oder Prozentualeabgaben mit dem Verleiher vereinbart wurden muss anders gerechnet werden. 
  if((!is.na(l_abrechnung[[ii]]$Abrechnung$`Abzug fix [CHF]`[1]))>0){ 
    # fixer Betrag inklusive Mehrwertsteuer mit dem Verleiher vereinbart! 
    l_abrechnung[[ii]]$Abrechnung <- 
      l_abrechnung[[ii]]$Abrechnung|>
      mutate(
        `Verleiherabzug [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                         `Abzug fix [CHF]`[1] * Verteilprodukt,     # keine Verleiherrechnung vorhanden
                                         `Verleiherrechnungsbetrag [CHF]` * Verteilprodukt  # Verleiherrechnung ist vorhanden
                                         ),
        `MWST [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                               sum(`Verleiherabzug [CHF]`) * (l_data$MWST$MWST / 100) * Verteilprodukt,
                               (`Verleiherrechnungsbetrag [CHF]`[1] - (`Verleiherrechnungsbetrag [CHF]`[1] / (1+(l_data$MWST$MWST/100)))) * Verteilprodukt
        )
      )
  }else{ 
    # Prozentualerabzug mit dem Verleiher vereinbart! 
    l_abrechnung[[ii]]$Abrechnung <- 
      l_abrechnung[[ii]]$Abrechnung|>
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

  # Gewinn/Verlust Tickets
  l_abrechnung[[ii]]$Abrechnung <- 
    l_abrechnung[[ii]]$Abrechnung|>
    mutate(`Gewinn/Verlust Tickets [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                                    Umsatz - ((`SUISA-Vorabzug [CHF]` + `Verleiherabzug [CHF]` + `MWST [CHF]`) * Verteilprodukt) ,
                                                    Umsatz - ((`SUISA-Vorabzug [CHF]` + `Verleiherrechnungsbetrag [CHF]`) * Verteilprodukt)
                                                    ) 
           )

  # Extract Verteilprodukt
  df_Verteilprodukt <- l_abrechnung[[ii]]$Abrechnung|>
    select(Datum, Verteilprodukt)
  df_Verteilprodukt


  # Eventeinnahmen (Jede Einnahme wird verteilt bei gemeinsamer Abrechnung)
  l_abrechnung[[ii]]$Eventeinnahmen <-
    Einnahmen_und_Ausgaben$Einnahmen|>
    filter(Kategorie == "Event",
           Datum == df_mapping$Datum[ii] & Datum == df_Verteilprodukt$Datum,
           Suisanummer == df_mapping$Suisanummer[ii]
           )|>
    mutate(Betrag = df_Verteilprodukt|>
             filter(Datum %in% c(df_Verteilprodukt$Datum ,df_mapping$Datum[ii]))|>
             select(Verteilprodukt)|>
             pull() * `Betrag [CHF]`
           )
  l_abrechnung[[ii]]$Eventeinnahmen

  # Eventausgaben (Jede Ausgabe wird verteilt bei gemeinsamer Abrechnung)
  l_abrechnung[[ii]]$Eventausgaben <- Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Kategorie == "Event",
           Suisanummer == df_mapping$Suisanummer[ii])|>
    filter(Spieldatum == df_Verteilprodukt$Datum & Spieldatum == df_mapping$Datum[ii])|>
    mutate(Betrag = df_Verteilprodukt|>
             filter(Datum == df_mapping$Datum[ii])|>
             select(Verteilprodukt)|>
             pull() * `Betrag [CHF]`
    )|>
    mutate(Datum = NULL)|>
    rename(Datum = Spieldatum)
  l_abrechnung[[ii]]$Eventausgaben
    
  # Eventausgaben (Jede Ausgabe wird verteilt bei gemeinsamer Abrechnung)
  l_abrechnung[[ii]]$Eventausgaben <-
    Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Kategorie == "Event",
           Suisanummer == df_mapping$Suisanummer[ii],
           Spieldatum %in% c(df_Verteilprodukt$Datum ,df_mapping$Datum[ii]))|>
    mutate(Betrag = df_Verteilprodukt|>
             filter(Datum == df_mapping$Datum[ii])|>
             select(Verteilprodukt)|>
             pull() * `Betrag [CHF]`
           )|>
    mutate(Datum = NULL)|>
    rename(Datum = Spieldatum)
  l_abrechnung[[ii]]$Eventausgaben
  

  # Gewinn Kiosk (wird nie verteilt, da der Verkauf pro Datum und Suisanummer erfolgt)
  l_abrechnung[[ii]]$Kiosk <- 
    df_Kiosk|>
    filter(Datum == df_mapping$Datum[ii],
           Suisanummer == df_mapping$Suisanummer[ii],
           )|>
    reframe(Kassiert = sum(Kassiert, na.rm = T),
            Gewinn = sum(Gewinn, na.rm = T))|>
    mutate(Datum = df_mapping$Datum[ii],
           `Suisa Nummer` = df_mapping$Suisanummer[ii]
    )|>
    left_join(df_show|>
                filter(`Suisa Nummer` == df_mapping$Suisanummer[ii])|>
                select(Datum, `Suisa Nummer`, Filmtitel),
              by = join_by(Datum, `Suisa Nummer`)
              )|>
    select(Datum, `Suisa Nummer`, Filmtitel, Kassiert, Gewinn)
  l_abrechnung[[ii]]$Kiosk
  

  # Manko und Überschuss Kiosk 
  l_abrechnung[[ii]]$`Manko oder Überschuss [CHF]` <- df_manko_uerberschuss|>
    filter(Datum == df_mapping$Datum[ii], 
           Suisanummer == df_mapping$Suisanummer[ii]
           )


  # Verteilen für gemeinsame Abrechnung
  l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
    mutate(`SUISA-Vorabzug [CHF]` = sum(`SUISA-Vorabzug [CHF]`) *  Verteilprodukt,
           `Verleiherabzug [CHF]` = sum(`Verleiherabzug [CHF]`) *  Verteilprodukt,
           `MWST [CHF]` = sum(`MWST [CHF]`) * Verteilprodukt,
           `Gewinn/Verlust Tickets [CHF]` = sum(`Gewinn/Verlust Tickets [CHF]`) *  Verteilprodukt ,
           `Gewinn/Verlust Kiosk [CHF]` = l_abrechnung[[ii]]$Kiosk$Gewinn , # Kiosk wird nicht verteilt
           `Überschuss / Manko Kiosk [CHF]` = l_abrechnung[[ii]]$`Manko oder Überschuss [CHF]`$`Überschuss / Manko`, # Manko, Überschuss wird nicht verteilt
           `Eventeinnahmen [CHF]` = sum(l_abrechnung[[ii]]$Eventeinnahmen$Betrag), # Eventeinnahmen wurden bereits mit Verteilprodukt berechnet
           `Eventausgaben [CHF]` = sum(l_abrechnung[[ii]]$Eventausgaben$Betrag), # Eventausgaben wurden bereits mit Verteilprodukt berechnet
           `Gewinn/Verlust Filmvorführungen [CHF]` = (`Gewinn/Verlust Tickets [CHF]` + `Gewinn/Verlust Kiosk [CHF]`+ `Überschuss / Manko Kiosk [CHF]` + `Eventeinnahmen [CHF]` - `Eventausgaben [CHF]`)
           )
  

  # Nur Abrechnung für aktuelles Datum behalten
  if(nrow(l_abrechnung[[ii]]$Abrechnung) != 0){
    l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
      filter(Datum == df_mapping$Datum[ii],
             Suisanummer == df_mapping$Suisanummer[ii]
             )  
  }
}


# Abrechnung Filmvorführung erstellen (für Berichte verwendet)
# Runden aller [CHF]  Beträge
df_Abrechnung <- bind_cols(
  l_abrechnung|>
    lapply(function(x){
      x$Abrechnung|>
        select(!ends_with("[CHF]"))
      })|>
    bind_rows(),
  l_abrechnung|>
    lapply(function(x){
      x$Abrechnung|>
        select(ends_with("[CHF]"))
    })|>
    bind_rows()|>
    apply(2, round5Rappen)|>
    as_tibble()
  )|>
  rename(`Umsatz [CHF]` = Umsatz)
df_Abrechnung


# Abrechnung Tickets erstellen (für Berichte verwendet)
df_Abrechnung_tickes <- l_abrechnung|>
  lapply(function(x){
    x$Tickets
  })|>
  bind_rows()|>
  rename(`Verkaufspreis [CHF]` = Verkaufspreis,
         `Umsatz [CHF]` = Umsatz,
         )|>
  left_join(df_show|>
              select(`Suisa Nummer`, Datum, Anfang, Ende),
            by = c("Datum" = "Datum",  "Suisanummer" = "Suisa Nummer")
            )
df_Abrechnung_tickes


# Abrechnung Kiosk erstellen  (für Berichte verwendet) 
df_Abrechnung_kiosk <- l_abrechnung|>
  lapply(function(x){
    x$Kiosk
  })|>
  bind_rows()|>
  rename(`Kassiert [CHF]` = Kassiert,
         `Gewinn [CHF]` = Gewinn
         )|>
  left_join(df_show|>
              select(`Suisa Nummer`, Datum, Anfang, Ende),
            by = join_by(Datum, `Suisa Nummer`)
            )
df_Abrechnung_kiosk


# Abrechnung Events erstellen (für Berichte verwendet)
df_Abrechnung_Eventeinnahmen <- l_abrechnung|>
  lapply(function(x){
    x$Eventeinnahmen
  })|>
  bind_rows()|>
  select(Datum, Bezeichnung, Betrag)|>
  rename(`Betrag [CHF]` = Betrag)
df_Abrechnung_Eventeinnahmen

df_Abrechnung_Eventausgaben <- l_abrechnung|>
  lapply(function(x){
    x$Eventausgaben
  })|>
  bind_rows()|>
  select(Datum, Bezeichnung, Betrag)|>
  rename(`Betrag [CHF]` = Betrag)
df_Abrechnung_Eventausgaben


# summary Eintritt (für Berichte verwendet)
df_Besucherzahlen <- df_Eintritt|>
  group_by(Datum, Filmtitel, Suisanummer)|>
  reframe(Besucher = sum(Anzahl))
df_Besucherzahlen


# write to Excel
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
       c_raw,
       c_select,
       c_names,
       ii,
       l_keineRechnung,
       c_filePath
       )

# user interaction
writeLines("Berechnungen erfolgt")
