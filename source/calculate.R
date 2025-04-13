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

# Data base user password from system variables
pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")
# Data base user
user <- "ch367079_flo"

# Connect to data base
con <- DB_connect(pw, "ch367079_flo")
# get all data as defined in the template l_template
# Convert data types for each table
l_data <- convert_DB_to_R(DB_get_Data(l_template, con),l_template)
# Disconnect from DB
dbDisconnect(con)
remove(l_template, con, pw, user)

# Eintritte aus Advanced Tickets files #####
convert_data_Film_txt <- function(fileName, Programm) {
  print("convert_data_Film_txt")
  l_Eintritt <- fileName|>
    lapply(function(fileName){

      # find ID_Program from file name
      ID <- str_match(fileName, "ID"%R%optional(SPC)%R%capture(one_or_more(DGT)))[2]|>
        as.integer()

      # read in data
      c_raw <- suppressWarnings(readLines(fileName))
      c_raw
      l_temp <- list()

      # Extract suisa from file
      p <- or(START%R%DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT,
              START%R%WRD%R%WRD%R%WRD%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT) #suisa
      index <- c_raw|>
        str_detect(p)

      c_temp <- c_raw[index]|>
        str_split("\t")|>
        unlist()

      # Error handling: Suisa from Programm vs Suisa from Programm
      df_temp <- Programm|>
        filter(`Event ID` == ID)
      df_temp$Suisanummer

      if(c_temp[1] != df_temp$Suisanummer) {
        warning("\nIn der Datei: .../Kinoklub/", fileName,
                "\nwurde die Suisanummer ",c_suisa," gefunden.",
                "\nIm Program wurde aber die Suisanummer ",df_temp$Suisanummer, " für Programm ID: ", ID," / ",df_temp$Filmtitel," definiert\n" )
      }


      # Save Suisa
      ii <- 1
      l_temp[[ii]] <- c_temp[1]
      names(l_temp)[ii] <- "Suisa"
      ii <- ii+1

      # Extract Filmtitel
      l_temp[[ii]] <- c_temp[2]
      names(l_temp)[ii] <- "Filmtitel"
      ii <- ii+1

      # Extract Datum
      p <- or("\t"%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT%R%DGT, # format 01.01.2025
              "\t"%R%DGT%R%DGT%R%"/"%R%DGT%R%DGT%R%"/"%R%DGT%R%DGT%R%DGT%R%DGT  # format 01/01/2025
      )
      index <- c_raw|>
        str_detect(p)
      index

      c_temp <- c_raw[index]|>
        str_split("\t")|>
        unlist()
      c_temp

      if(dmy(c_temp[2]) != df_temp$Datum) {
        stop("\nIn der Datei: .../Kinoklub/", fileName,
                "\nwurde das Datum ",format(c_temp[2], "%d.%m.%Y")," gefunden.",
                "\nIm Programm wurde aber das Datum ",format(df_temp$Datum, "%d.%m.%Y")," für Programm ID: ", ID," / ",df_temp$Filmtitel," definiert\n" )
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

# Extrakt Kioskverkauf und Überschuss / Manko #####
convert_data_kiosk_txt <- function(fileName, Programm, df_Einkauf) {
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
      p <- or(DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT%R%DGT, # format 01.01.2025
              DGT%R%DGT%R%"/"%R%DGT%R%DGT%R%"/"%R%DGT%R%DGT%R%DGT%R%DGT  # format 01/01/2025
      )
      index <- c_raw|>
        str_detect(p)
      index

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
      p1 <- or1(paste0(df_Einkauf$`Artikelname-Kassensystem`))

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
      l_return <- list()
      l_return[["df_Kiosk"]] <- m_Kiosk|>
        mutate(Einzelpreis = if_else(is.na(Einzelpreis), Betrag / Anzahl, Einzelpreis),
               Betrag = if_else(Anzahl == 0, 0, Betrag))

      # Extrakt Überschuss / Manko
      l_return[["Überschuss / Manko"]] <- l_extracted[[2]]$`Überschuss / Manko`
      return(l_return)
    })
  names(l_temp) <- fileName
  return(l_temp)
}

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
df_temp <- l_data$Programm|>
  distinct(Datum, Suisanummer, .keep_all = TRUE)
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
p <- DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT
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


# Eintritt aus Advanced Tickets ##################
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
l_temp <- convert_data_Film_txt(c_files, l_data$Programm)

# create data frame
df_Eintritt <- l_temp|>
  bind_rows()|>
  mutate(Verkaufspreis = Preis ,
         Zahlend = if_else(Verkaufspreis == 0, F, T))|>
  select(Datum, Suisanummer, Filmtitel, Platzkategorie, Zahlend, Verkaufspreis, Anzahl, Umsatz,`SUISA-Vorabzug`)
df_Eintritt

# join `Event ID`
df_Eintritt <- df_Eintritt|>
  left_join(l_data$Programm|>
              filter(`Verleiher Angefragt?` != "Wird nicht gespielt")|>
              select(`Event ID`, Datum, Suisanummer),
            by = join_by(Datum, Suisanummer)
  )
# paste0("`",names(df_Eintritt), "`")|>
#   paste0(collapse = ", ")|>
#   writeLines()
  
df_Eintritt <- df_Eintritt|>
  select(`Event ID`, `Datum`, `Suisanummer`, `Filmtitel`, `Platzkategorie`, `Zahlend`, `Verkaufspreis`, `Anzahl`, `Umsatz`, `SUISA-Vorabzug` )


if(sum(is.na(df_Eintritt$`Event ID`)) > 0){
  df_temp <- df_Eintritt|>
    filter(is.na(`Event ID`))|>
    distinct(Datum, Suisanummer,.keep_all = TRUE)
  stop("\nFür den Film ", df_temp$Filmtitel, " mit Suisanummer ", df_temp$Suisanummer, " am ",
       paste0(format(df_temp$Datum, "%d.%m.%Y"), collapse = ", "), " existiert kein Programmeintrag\nBitte das Programm korrigieren!\n"
       )
}


# Kioskeinkauf ####
# Advace tickets Kiosk
c_path <- "input/advance tickets"
c_files <- list.files(c_path, pattern = "Kiosk", recursive = TRUE, full.names = TRUE)
l_temp <- convert_data_kiosk_txt(c_files, l_data$Programm, l_data$`Einkauf Kiosk`)

df_Kiosk <- l_temp|>
  lapply(function(x){
    x$df_Kiosk
  })|>
  bind_rows(.id = "Event ID")|>
  mutate(`Event ID` = str_extract(`Event ID`, one_or_more(DGT))|>
           as.integer()
         )
df_Kiosk

# Manko und Überschuss Kiosk ####
df_manko_uerberschuss <- l_temp|>
  lapply(function(x){
    x$`Überschuss / Manko`
  })|>
  bind_rows(.id = "Event ID")|>
  mutate(`Event ID` = str_extract(`Event ID`, one_or_more(DGT))|>
           as.integer()
  )
df_manko_uerberschuss

df_Kiosk <- df_Kiosk|>
  rename("Artikel-Kassensystem" = Verkaufsartikel)
df_Kiosk


# Spez Verkaufsartikel / Spezialpreise einlesen ####
# Spezialpreise einlesen
l_data$Spezialpreisekiosk|>
  arrange(`Event ID`, Spezialpreis)

df_Spezialpreisekiosk <- l_data$Spezialpreisekiosk |>
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
  left_join(l_data$Programm|>
              select(`Event ID`,Filmtitel),
            by = join_by(`Event ID`)
            )
df_spez_preis

# Sind alle Spezialpreise pro `Event ID` definiert?
df_spez_preis_na <- df_spez_preis|>
  filter(str_detect(`Artikel-Kassensystem`, "Spez")) |>
  arrange(`Event ID`, `Artikel-Kassensystem`)
df_spez_preis_na

df_spez_preis_na <- df_spez_preis_na|>
  left_join( # look up Spezialpreise
    df_Spezialpreisekiosk,
    by = c("Event ID", `Artikel-Kassensystem` = "Spezialpreis")
  )
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
remove(df_spez_preis)

# join Spezpreise mit Verkaufsartikel
df_Kiosk <- df_Kiosk|>
  left_join(df_Spezialpreisekiosk|>
              select(-ID),
            by = c("Event ID", `Artikel-Kassensystem` = "Spezialpreis")
  )|>
  mutate(Verkaufsartikel = if_else(is.na(Artikelname), `Artikel-Kassensystem`, Artikelname))|>
  select(-Artikelname)
df_Kiosk

# Kiosk Einkaufspreise
df_Einkaufspreise <- l_data$`Einkauf Kiosk`|>
  rename(ID_Kioskartikel = ID)
df_Einkaufspreise

df_mapping <- l_temp|>
  lapply(function(x){
    tibble(
      Datum =
        x$df_Kiosk|>
        distinct(Datum)|>
        pull()|>
        as.Date()
    )
  })|>
  bind_rows(.id = "fileName")

c_Date_Kiosk <- df_mapping$Datum
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
m_Kiosk <- list()
for (ii in 1:nrow(df_Mapping_Einkaufspreise)) {
  m_Kiosk[[ii]] <- df_Kiosk|>
    filter(Datum == df_Mapping_Einkaufspreise$Datum[ii])|>
    left_join(df_Einkaufspreise|>
                # select(-ID)|>
                filter(`Gültig ab Datum` == df_Mapping_Einkaufspreise$Einkaufspreise[ii])|>
                select(-`Gültig ab Datum`),
              by = c(Verkaufsartikel = "Artikelname-Kassensystem")
    )
}
remove(df_Einkaufspreise)
m_Kiosk

df_Kiosk <- m_Kiosk|>
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
  select(-Datum)|>
  left_join(l_data$Programm|>
              select(`Event ID`, Datum, Suisanummer, Filmtitel),
            by = join_by(`Event ID`)
            )

df_Kiosk <- df_Kiosk|>
  select("Event ID", "ID_Kioskartikel","Verkaufsartikel", "Lieferant",
         "Verkaufspreis", "Anzahl", "Kassiert",  
         #"Artikel", "Verkaufspreis [CHF]", "Menge", 
         "Einkaufspreis [CHF]", 
         # "Gewinn", "Datum", "Suisanummer", "Filmtitel"
  )|>
  rename(`Verkaufspreis [CHF]` = Verkaufspreis,
         `Kassiert [CHF]` = Kassiert)

df_Kiosk <- df_Kiosk|>
  mutate(`Gewinn` = Anzahl * (`Verkaufspreis [CHF]`- `Einkaufspreis [CHF]` ))

df_Kiosk

# check if all Kiosk entry can be joined by `Event ID`
if(sum(is.na(df_Kiosk$`Event ID`)) > 0){
  df_temp <- df_Kiosk|>
    filter(is.na(`Event ID`))|>
    distinct(`Event ID`, .keep_all = TRUE )
  df_temp
  stop("\nFür den Film mit Suisanummer ", df_temp$Suisanummer, " am ", format(df_temp$Datum, "%d.%m.%Y"), " gibt es keinen Programmeintrag.\nBitte das Programm korrigieren!\n")
}

# remove no more needed variables
remove(df_Mapping_Einkaufspreise,m_Kiosk,
       c_Date_Kiosk, c_Einkaufslistendatum,
       ii,
       c_path, c_files, l_temp
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


# Je nach Verleiher müssen die Kinoförderer als Umsatz abgerechnet werden. #####
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
           "Platzkategorie","Zahlend","Verkaufspreis","Verkaufspreis für Netto3 [CHF]","Anzahl","Umsatz","Umsatz für Netto3 [CHF]",
           "SUISA-Vorabzug",
           "Verleiher",
           "Abzug [%]","Minimal Abzug [CHF]","Abzug fix [CHF]","Kinoförderer gratis?",
           "Verleiherrechnungsbetrag [CHF]",
           )
         )|>
  rename(`SUISA-Vorabzug [%]` = `SUISA-Vorabzug`,
         `Umsatz [CHF]` = Umsatz)|>
  arrange(Datum)


# Umsatz aus Tickets zu Abrechnung hinzufügen ####
df_temp <- df_Tickets|>
  group_by(`Event ID`)|>
  reframe(`Umsatz [CHF]` = sum(`Umsatz [CHF]`,na.rm = T),
          `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`, na.rm = T))
df_temp
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
  mutate(`Suisavorabzug [CHF]` = 
           if_else(`Kinoförderer gratis?`,
                   `Umsatz [CHF]` * (`SUISA-Vorabzug [%]` / 100),
                   `Umsatz für Netto3 [CHF]` * (`SUISA-Vorabzug [%]` / 100)
                   ),
         `Umsatz Netto 3 [CHF]` = 
           if_else(`Kinoförderer gratis?`,
                   `Umsatz [CHF]` - `Suisavorabzug [CHF]`,
                   `Umsatz für Netto3 [CHF]` - `Suisavorabzug [CHF]`
                   ),
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
  reframe(`Kioskgewinn [CHF]` = sum(Gewinn, na.rm = T))
df_temp  
df_Abrechnung <- left_join(df_Abrechnung, 
                           df_temp,
                           by = join_by(`Event ID`)
                           )  

# Manko / Überschuss Kasse der Abrechnung hinzufügen ####
df_Abrechnung <- left_join(df_Abrechnung, 
                           df_manko_uerberschuss,
                           by = join_by(`Event ID`)
                           )  
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
           sum(`Ticketgewinn [CHF]`,`Kioskgewinn [CHF]`,`Überschuss / Manko`, `Eventeinnahmen [CHF]`, -`Eventausgaben [CHF]`,
               na.rm = T)
         )
df_temp
df_Abrechnung <- left_join(df_Abrechnung, df_temp, by = join_by(`Event ID`))

# Gemeinsame Abrechnung über mehrere Event IDs erstellen ####
df_mapping <- df_Abrechnung|>
  select(1:6)|>
  mutate(`Link to Event ID` = as.character(`Link to Event ID`)|>as.integer())|>
  filter(`Event ID` %in% df_Eintritt$`Event ID`)

# find all connected Filmvorführungen from Programm and remove all already connected
l_abrechnung <- inspect_link_ids(df_mapping)
l_abrechnung <- l_abrechnung|>
  nullify_used_entries()

l_abrechnung[[1]]

ID <- 1
cnt <- 1
for (ID in names(l_abrechnung)) {
  # Event ID`s 
  IDs <- l_abrechnung[[ID]]

  ## Umsatzverteilprodukt berechnen für die gemeinsame Abrechnung ####
  Verteilprodukt <- df_Abrechnung|>
    filter(`Event ID` %in% IDs)|>
    group_by(`Event ID`)|>
    reframe(`Umsatz [CHF]`= sum(`Umsatz [CHF]`),
            `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`)
    )|>
    # Umsatz für Netto 3 wurde bereit je nach Verleiher anders berechntet! (Kinoförderer gratis)
    mutate(Verteilprodukt = `Umsatz für Netto3 [CHF]` / sum(`Umsatz für Netto3 [CHF]`) 
    )|>
    left_join(l_data$Programm|>select(1:6)|>select(-`Link to Event ID`),
              by = join_by(`Event ID`)
    )
  
  # Error handling: Sind die Eintritte daten für jede Filmvorführung vorhanden? ####
  if(nrow(Verteilprodukt) !=  nrow(l_data$Programm|>filter(`Event ID` %in% IDs))){
    df_temp <- l_data$Programm|>
      filter(`Event ID` %in% IDs)
    df_temp <- df_temp|>
      filter(!(df_temp$`Event ID` %in% Verteilprodukt$`Event ID`))
    warning(paste0("\nFür den Film ID ", df_temp$`Event ID`," / ", df_temp$Filmtitel[1], " gibt es keine Eintritte. ",
                   # "\nDie gemeinsame Abrechnung über mehrere Spieldaten wird nicht korrekt berechnet.",
                   "\nBitte Eintritte herunterladen und abspeichern!\n\n"))
    next
  }
  Verteilprodukt <- Verteilprodukt|>
    select(`Event ID`, Verteilprodukt)
  Verteilprodukt
  
  # Verteilen der Abrechnung ####
  Abrechnung <- df_Abrechnung|>
    filter(`Event ID` %in% IDs)|>
    left_join(Verteilprodukt,
              by = join_by(`Event ID`)
    )
  Abrechnung|>select(22:ncol(Abrechnung))

    Abrechnung <- Abrechnung|>
    mutate(`Verleiherrechnungsbetrag [CHF]` = `Verleiherrechnungsbetrag [CHF]` * Verteilprodukt,
           `Umsatz [CHF]` = `Umsatz [CHF]` * Verteilprodukt,
           `Umsatz für Netto3 [CHF]` = `Umsatz für Netto3 [CHF]` * Verteilprodukt,
           `Suisavorabzug [CHF]` = `Suisavorabzug [CHF]` * Verteilprodukt,
           `Verleiherabzug [CHF]` = `Verleiherabzug [CHF]` * Verteilprodukt,
           `Ticketgewinn [CHF]` = `Ticketgewinn [CHF]` *  Verteilprodukt,
           `Eventeinnahmen [CHF]` = `Eventeinnahmen [CHF]` * Verteilprodukt,
           `Eventausgaben [CHF]` = `Eventausgaben [CHF]` * Verteilprodukt,
           `Gewinn aus Fimvorführung [CHF]` = `Gewinn aus Fimvorführung [CHF]` * Verteilprodukt
    )
  Abrechnung|>
    select(16:ncol(Abrechnung))
  
  # Verteilen der Eintritte ####
  Eintritte <- df_Eintritt|> 
    filter(`Event ID` %in% IDs)|>
    select(`Event ID`,Platzkategorie, Verkaufspreis, Anzahl, Umsatz)|>
    left_join(Verteilprodukt, 
              by = join_by(`Event ID`)
              )
  Eintritte <- Eintritte|>
    mutate(`Ticketumsatz [CHF]` = Umsatz * Verteilprodukt)
  Eintritte
  
  # Verteilen der Einnahmen ####
  Einnahmen <- Einnahmen_und_Ausgaben$Einnahmen|>
    select(`Event ID`, Kategorie, Bezeichnung, `Betrag [CHF]`)|>
    filter(`Event ID` %in% IDs)|>
    left_join(Verteilprodukt,
              by = join_by(`Event ID`)
              )
  Einnahmen <- Einnahmen|>
    mutate(`Betrag verteilt [CHF]` = Verteilprodukt * `Betrag [CHF]`)
  Einnahmen
  
  # Verteilen der Ausgaben ####
  Ausgaben <- Einnahmen_und_Ausgaben$Ausgaben|>
    select(`Event ID`, Kategorie, Datum, Bezeichnung, `Betrag [CHF]`)|>
    filter(`Event ID` %in% IDs)|>
    left_join(Verteilprodukt,
              by = join_by(`Event ID`)
    )
  Ausgaben <- Ausgaben|>
    mutate(`Betrag verteilt [CHF]` = Verteilprodukt * `Betrag [CHF]`)
  Ausgaben
  
  # Der Kioskgewinn wird nicht verteilt ####
  Kiosk <- df_Kiosk|>
    filter(`Event ID` %in% IDs)
  
  # Überschuss und Manko wird nicht verteilt ####
  Manko <- df_manko_uerberschuss|>
    filter(`Event ID` %in% IDs)
  
  # return ####
  l_abrechnung[[cnt]] <- 
    list(
      Verteilprodukt = Verteilprodukt,
      Eintritte = Eintritte,
      Eventeinnahmen = Einnahmen,
      Eventausgaben = Ausgaben,
      Kiosk = Kiosk,
      `Manko / Überschuss` = Manko,
      Abrechnung = Abrechnung
    )
  cnt <- cnt + 1
}
remove(Verteilprodukt, Eintritte, Einnahmen, Ausgaben, Kiosk, Manko, Abrechnung,
       df_mapping, df_Abrechnung,
       df_manko_uerberschuss, df_Spezialpreisekiosk, df_temp
       )
l_abrechnung

l_abrechnung[["1"]]


#  Data frames für Berichte erstellen ####

# Abrechnung Tickets erstellen (für Berichte verwendet) ####
df_Abrechnung <- l_abrechnung|>
  lapply(function(x){
    x$Abrechnung
  })|>
  bind_rows(.id = "Event ID")|>
  mutate(`Event ID` = as.integer(`Event ID`))
df_Abrechnung

# Abrechnung Tickets erstellen (für Berichte verwendet) ####
df_Abrechnung_tickes <- l_abrechnung|>
  lapply(function(x){
    x$Eintritte
  })|>
  bind_rows(.id = "Event ID")|>
  rename(`Verkaufspreis [CHF]` = Verkaufspreis,
         `Umsatz [CHF]` = Umsatz)
df_Abrechnung_tickes

df_Abrechnung_tickes <- df_Abrechnung_tickes|>
  mutate(`Event ID` = as.integer(`Event ID`))|>
  left_join(df_Abrechnung,
            by = join_by(`Event ID`)
  )

# Abrechnung Kiosk erstellen  (für Berichte verwendet) ####
df_Abrechnung_kiosk <- l_abrechnung|>
  lapply(function(x){
    x$Kiosk
  })|>
  bind_rows(.id = "Event ID")

df_Abrechnung_kiosk <- df_Abrechnung_kiosk|>
  mutate(`Event ID` = as.integer(`Event ID`))|>
  left_join(df_Abrechnung,
            by = join_by(`Event ID`)
            )


df_Abrechnung_kiosk

# Abrechnung Events erstellen (für Berichte verwendet) ####
df_Abrechnung_Eventeinnahmen <- l_abrechnung|>
  lapply(function(x){
    x$`Eventeinnahmen`
  })|>
  bind_rows(.id = "Event ID")
df_Abrechnung_Eventeinnahmen

df_Abrechnung_Eventausgaben <- l_abrechnung|>
  lapply(function(x){
    x$`Eventausgaben`
  })|>
  bind_rows(.id = "Event ID")
df_Abrechnung_Eventausgaben

# Keine Verleiherrechnung ####
df_keine_Rechnung <- l_abrechnung|>
  lapply(function(x){
    x$Abrechnung
  })|>
  bind_rows(.id = "Event ID")|>
  filter(is.na(`Verleiherrechnungsbetrag [CHF]`))
df_keine_Rechnung$`Verleiherrechnungsbetrag [CHF]`

# Manko / Überschuss ####
df_manko_uerberschuss <- l_abrechnung|>
  lapply(function(x){
    x$`Manko / Überschuss`
  })|>
  bind_rows(.id = "Event ID")
df_manko_uerberschuss

# summary Eintritt (für Berichte verwendet) ####
df_Besucherzahlen <- df_Eintritt|>
  group_by(`Event ID`,Datum, Filmtitel, Suisanummer)|>
  reframe(Besucher = sum(Anzahl))
df_Besucherzahlen

# write to Excel ####
c_filePath <- "output/data/"
if(!dir.exists(c_filePath)) dir.create(c_filePath, recursive = T )

list(`Werbung` = df_Besucherzahlen,
     `Tickets` = df_Abrechnung_tickes,
     `Kiosk` = df_Abrechnung_kiosk,
     `Eventeinnahmen` = df_Abrechnung_Eventeinnahmen,
     `Eventausgaben` = df_Abrechnung_Eventausgaben,
     `Filmvorführung` = df_Abrechnung
       )|>
  write.xlsx(file="output/data/Auswertung.xlsx", asTable = TRUE, overwrite = TRUE)


# remove not used variables ####
remove(ii,
       c_filePath
       )

# user interaction ####
writeLines("Good ... Berechnungen erfolgt")

l_abrechnung$`10`
