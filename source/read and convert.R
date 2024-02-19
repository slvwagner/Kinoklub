library(tidyverse)
library(rebus)
library(openxlsx)
library(lubridate)

file.remove("error.log")|>suppressWarnings()

source("source/functions.R")

if(!r_is.defined(c_MWST)){
  c_MWST <- 8.1
}

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
names(l_Eintritt) <- c_files|>
  str_extract(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))

l_Eintritt
df_Eintritt <- l_Eintritt|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum),
         Datum_ = NULL,
         Verkaufspreis = Preis ,
         Tax = NULL, 
         Zahlend = if_else(Verkaufspreis == 0, F, T))|>
  select(Datum, Filmtitel,`Suisa Nummer`,Platzkategorie,Zahlend,Verkaufspreis, Anzahl,Umsatz,`SUISA-Vorabzug`)

df_Eintritt|>
  filter()


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

n_kiosk <- df_Kiosk|>distinct(Datum, .keep_all = T)
n_Film <- df_Eintritt|>distinct(Datum, .keep_all = T )

#############
# Error handling
if(n_kiosk|>nrow() > n_Film|>nrow()){
  stop("Es fehlt einen Kioskabrechnung")
}else if(df_Kiosk|>distinct(Datum)|>nrow() < df_Eintritt|>distinct(Datum)|>nrow()){
  stop("Es fehlt einen Kinoabrechnug")
}

########################################################################
# show times
########################################################################

# error handling file not found
try(c_raw <- list.files(pattern = "Shows", recursive = T)|>
      readLines()|>
      suppressWarnings(),
    outFile = "error.log"
    )
if(length(list.files(pattern = "error.log"))>0) stop("\nDatei input/Shows.txt nicht gefunden. \nBitte herunterladen und abspeichern.")

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

df_show <- df_show|>
  left_join(df_Eintritt|>
              distinct(Datum, `Suisa Nummer`),
            by = c("Datum" = "Datum")
            )|>
  arrange(Datum)

df_show <- df_show|>
  filter(!is.na(`Suisa Nummer`))

## error handling 
df_temp <- df_Eintritt|>distinct(Datum, `Suisa Nummer`)|>
  anti_join(df_show, by = join_by(Datum, `Suisa Nummer`))|>
  left_join(df_Eintritt, by = join_by(Datum, `Suisa Nummer`))|>
  distinct(Datum, .keep_all = T)
df_temp

if(nrow(df_temp) != 0) {
  stop(paste0(
    "\nFür den Film: ",df_temp$Filmtitel, " am ", 
    day(df_temp$Datum),".",month(df_temp$Datum),".",year(df_temp$Datum), 
    " gibt es keinen Eintrag in der Datei Input/advance tickets/show.txt\nBitte herunterladen und abspeichern")
  )}


########################################################################
# Verleiherabgaben einlesen
########################################################################

c_file <- "input/Verleiherabgaben.xlsx"
c_sheets <- readxl::excel_sheets(c_file)
c_sheets

df_verleiherabgaben <- readxl::read_excel(c_file,c_sheets[1])|>
  mutate(Datum = as.Date(Datum))|>
  left_join(readxl::read_excel(c_file,c_sheets[2]), by = "Verleiher")

df_verleiherabgaben

df_Eintritt <- df_Eintritt|>
  left_join(df_verleiherabgaben, by = c(`Suisa Nummer` = "Suisa", "Datum"))
df_Eintritt

########################################################################
## Errorhandling 

# kein prozentualer noch fixer abzug definiert
df_temp <- df_Eintritt|>
  filter(is.na(`Abzug [%]`) & is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){ 
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde werder kein Abzug definiert.",
              "\nBitte im File input/Verleiherabgaben.xlsx korrigieren.")
       )
}

# kein minimal Abzug definiert (Es muss kein minimaler Abzug definiert werden fall ein Fixerabzug definiert wurde)
df_temp <- df_Eintritt|>
  filter(is.na(`Minimal Abzug`) & !is.na(`Abzug [%]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0) stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                                "\nwurde werder kein Minimal Abzug definiert.",
                                "\nBitte im File input/Verleiherabgaben.xlsx korrigieren.")
)

# Prozentualer und Fixer Abzug definiert
df_temp <- df_Eintritt|>
  filter(!is.na(`Abzug [%]`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){ 
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde ein Prozentualer und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
              "\nBitte im File input/Verleiherabgaben.xlsx korrigieren.")
  )
}

# minimal und Fixer Abzug definiert
df_temp <- df_Eintritt|>
  filter(!is.na(`Minimal Abzug`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde ein minimal Abzug und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
              "\nBitte im File input/Verleiherabgaben.xlsx korrigieren.")
  )
}

########################################################################
# Verleiherrechnung 
########################################################################
df_Verleiher_Rechnnung <- df_Eintritt|>
  distinct(Datum,`Suisa Nummer`,.keep_all = T)|>
  select(Datum, Filmtitel, `Suisa Nummer`)
df_Verleiher_Rechnnung

# # Verleiherabgaben sind im Dropdown zu finden
Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`
Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`[5]

df_Verleiher_Rechnnung <- df_Verleiher_Rechnnung|>
  left_join(Einnahmen_und_Ausgaben[["Ausgaben"]] |>
              filter(Kategorie == Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`[5])|>
              select(-Datum,-Kategorie),
            by = c("Datum" = "Spieldatum"))|>
  mutate(`keine Verleiherrechnung` = if_else(is.na(Betrag), T, F))
df_Verleiher_Rechnnung

df_keine_Rechnnung <- df_Verleiher_Rechnnung|>
  filter(`keine Verleiherrechnung`)

# error handling, keine Verleiherrechnung
if(nrow(df_keine_Rechnnung)>0) {
  warning(paste0("\nAchtung für die diesen Film gibt es keine Verleiherrechnung: \n",
                   day(df_keine_Rechnnung$Datum),".",month(df_keine_Rechnnung$Datum),".", lubridate::year(df_keine_Rechnnung$Datum), " ",df_keine_Rechnnung$Filmtitel,"\n")
          )  
}

df_Eintritt

########################################################################
# Gewinn/Verlust Eintitt
########################################################################
df_Kinopreise <- df_Eintritt|>
  distinct(Platzkategorie,.keep_all = T)|>
  select(Platzkategorie, Verkaufspreis)
df_Kinopreise

# Platzkategorien die für gewisse Verleiherabgerechnet werden müssen
c_P_kat_verechnen <- c("Kinoförderer","Spezialpreis")

c_Date[3]

l_GV <- list()
l_Abgaben <- list()
ii <- 3
for (ii in 1:length(c_Date)) {

  ######################################################################
  # Kinoförderer dürfen nicht bei jedem Verleiher als gratis abgerechnet werden anders behandelt
  c_Kinofoerder_gratis <- df_Eintritt|>
    filter(Datum == c_Date[ii])|>
    distinct(`Kinoförderer gratis?`)|>
    mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "ja", T, F))|>
    pull()
  c_Kinofoerder_gratis

  if(!c_Kinofoerder_gratis){ # Kinoföderer müssen abgerechnet werden 
    df_temp <- df_Eintritt |>
      filter(Datum == c_Date[ii], Zahlend) |>
      bind_rows(
        df_Eintritt |>
          filter(Datum == c_Date[ii], Platzkategorie %in% c_P_kat_verechnen)|>
          mutate(Abrechnungspreis = df_Kinopreise|>
                   filter(Platzkategorie == "Ermässigt")|> # Kategorieren werden mit Ermässigt ersetzt
                   select(Verkaufspreis)|>pull()
                 )
      )|>
      mutate(Umsatz = if_else(is.na(Abrechnungspreis), Umsatz, Abrechnungspreis *  Anzahl))|>
      select(-Abrechnungspreis)
    df_temp
    
    c_Besucher <- df_temp|>
      reframe(Anzahl = sum(Anzahl))|>
      pull()
    c_Besucher
    
    c_Gratis <- df_Eintritt |>
      filter(Datum == c_Date[ii])|>
      reframe(Anzahl = sum(Anzahl))|>
      pull() - c_Besucher
    c_Gratis
    
    ##Brutto
    c_Brutto <- df_temp |>
      filter(Datum == c_Date[ii]) |>
      reframe(Umsatz = sum(Umsatz)) |>
      pull()
    c_Brutto
    
  } else { # Kinoföderer sind gratis
    
    df_temp <- df_Eintritt |>
      filter(Datum == c_Date[ii], Zahlend) 
    df_temp
    
    c_Besucher <- df_Eintritt |>
      filter(Datum == c_Date[ii]) |>
      reframe(Besucherzahl = sum(Anzahl)) |>
      pull()
    c_Besucher
    
    c_Gratis <- df_Eintritt |>
      filter(Datum == c_Date[ii], !Zahlend) |>
      reframe(Gratiseintritte = sum(Anzahl)) |>
      pull()
    c_Gratis
    
    ##Brutto
    c_Brutto <- df_temp |>
      filter(Datum == c_Date[ii]) |>
      reframe(Umsatz = sum(Umsatz)) |>
      pull()
    c_Brutto
  } 
  
  c_Umsatz <- df_Eintritt |>
    filter(Datum == c_Date[ii]) |>
    reframe(Umsatz = sum(Umsatz)) |>
    pull()
  c_Umsatz
  
  l_Abgaben[[ii]] <- df_temp
  l_Abgaben[[ii]]
  
  c_suisaabzug <- (distinct(df_Eintritt |> 
                filter(Datum == c_Date[ii]), `SUISA-Vorabzug`) |>
       pull()) / 100
  c_suisaabzug
  
  ## Netto 3
  c_Netto3 <- c_Brutto - round5Rappen(c_Brutto * c_suisaabzug)
  c_Netto3
  
  
  # minimale Abgaben an den Verleiher
  c_Verleiher_garantie <- df_verleiherabgaben |>
    filter(Datum == c_Date[ii])|>
  select(`Minimal Abzug`)|>
  pull()
  c_Verleiher_garantie
  
  # prozentual Abgabe von Netto 3 an den Verleiher
  c_verleiherabzug_prozent <-(distinct(df_Eintritt |> 
                                         filter(Datum == c_Date[ii]),
                                       `Abzug [%]`) |> pull()) / 100
  c_verleiherabzug_prozent
  
  ##################################################
  # Abzug fix oder prozentual 
  if(is.na(c_verleiherabzug_prozent)) { # Abzug fix
    c_Verleiherabzug <- distinct(df_Eintritt |> 
               filter(Datum == c_Date[ii]), `Abzug fix [CHF]`
             )|>
      pull()
  }else{ # prozentualer Abzug
    # Abgabe an den Verleiher
    c_Verleiherabzug <- c_Netto3 * c_verleiherabzug_prozent
    
    ### Wenn die Abgabe von Netto 3 kleiner als der definierte minimal Abzug ist wird dieser eingesetzt
    if (c_Verleiherabzug < c_Verleiher_garantie) {
      c_Verleiherabzug <- c_Verleiher_garantie
    }
  }
  c_Verleiherabzug
  
  ##################################################
  # Berechnung der Abgaben
  # Verleiherrechnung vorhanden ?
  c_Verleiherrechnung <- df_Verleiher_Rechnnung |> 
    filter(Datum == c_Date[ii])|>
    select(`keine Verleiherrechnung`)|>
    pull()
  
  c_Verleiherrechnung
  
  # Wemm keine Verleiherrechnung vorhanden ist muss die MWST der Verleiherrechnung berechnet werden.
  if (c_Verleiherrechnung) {
    # Mehrwertsteuer auf der Verleiherrechnung
    c_MWST_Abzug <-
      round5Rappen(c_Verleiherabzug - (c_Verleiherabzug / (1 + (c_MWST / 100) ))
                   )
    c_MWST_Abzug
    
  }else {
    c_MWST_Abzug <- round5Rappen(c_Verleiherabzug) * (c_MWST / 100)
    }
  
  c_MWST_Abzug
  
  # Error handling Verleiherabgaben
  if(is.na(c_MWST_Abzug)) {
    error <- df_Eintritt|>
      filter(Datum == c_Date[ii])|>
      distinct(Datum,.keep_all = T)|>
      select(Datum, `Suisa Nummer`, Filmtitel)|>
      mutate(Datum = paste0(day(Datum),".",month(Datum),".",year(Datum)))
    error <- str_c("\nFür den Film ", error$Filmtitel," am ", error$Datum, " mit Suisa-Nummer ", error$`Suisa Nummer`, " wurde keine",
                   "\nVerleiherabgabe im file \"Verleiherabgaben.xlsx\"definiert.")
    stop(error)
  }
  
  # Verleiherrechnung Betrag
  c_Verleiherrechnung <- df_Verleiher_Rechnnung |> 
    filter(Datum == c_Date[ii])|>
    select(Betrag)|>
    pull()
  
  c_Verleiherrechnung
  df_temp
  
  l_GV[[ii]] <- tibble(Datum = c_Date[ii],
                       `Suisa Nummer` = c_suisa_nr[ii],
                       Brutto = c_Umsatz, 
                       Verleiherrechnung = c_Verleiherrechnung,
                       `SUISA-Abzug [%]` = c_suisaabzug*100,
                       `SUISA-Abzug [CHF]` = round5Rappen(c_Brutto*c_suisaabzug), 
                       `Netto 3` = c_Netto3,
                       `Verleiher-Abzug [%]` = c_verleiherabzug_prozent*100,
                       `Verleiher-Abzug [CHF]` = c_Verleiherabzug,
                       `MWST Satz auf die Verleiherrechnung [%]` = c_MWST,
                       `MWST auf die Verleiherrechnung [CHF]` =  c_MWST_Abzug
                       )|>
    mutate(## Gewinn berechnung
           `Sonstige Kosten [CHF]` = (c_Verleiherrechnung - c_MWST_Abzug) - `Verleiher-Abzug [CHF]`,
           `Gewinn/Verlust [CHF]` = round5Rappen(Brutto - sum(`Verleiher-Abzug [CHF]`,
                                                              `Sonstige Kosten [CHF]`, `MWST auf die Verleiherrechnung [CHF]`,
                                                              na.rm = T))
           )|>
    left_join(df_show, by = join_by(Datum, `Suisa Nummer`))
  
  l_GV[[ii]]

}

l_GV

df_GV_Eintritt <- l_GV|>
  bind_rows()
df_GV_Eintritt

df_Abgaben <- l_Abgaben|>
  bind_rows()|>
  bind_rows(df_Eintritt|>
              filter(!Zahlend, !(Platzkategorie %in% c_P_kat_verechnen)))

df_Abgaben


########################################################################
# Gewinn Kiosk
########################################################################

ii <- 3
l_GV_Kiosk <- list()
for (ii in 1:length(c_Date)) {
  df_temp <- df_show|>filter(Datum == c_Date[ii])
  
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

ii <- 4
l_GV_Vorfuehrung <- list()
for (ii in 1:length(c_Date)) {
  df_Eventausgaben <- Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Spieldatum == c_Date[ii], Kategorie == Einnahmen_und_Ausgaben$dropdown$`drop down`[1])
  df_Eventausgaben

  if(nrow(df_Eventausgaben) < 1) {
    c_Eventausgaben <- 0
  } else {
    c_Eventausgaben <- sum(df_Eventausgaben$Betrag, na.rm = T)
  }
    
  df_Eventeinnahmen <- Einnahmen_und_Ausgaben$Einnahmen |>
    filter(Datum == c_Date[ii], Kategorie == Einnahmen_und_Ausgaben$dropdown$`drop down`[6])
  df_Eventeinnahmen
  
  if(nrow(df_Eventeinnahmen) < 1) {
    c_Eventeinnahmen <- 0
  } else {
    c_Eventeinnahmen <- sum(df_Eventeinnahmen$Betrag, na.rm = T)
  }
  
  df_temp <- df_show|>filter(Datum == c_Date[ii])
  df_temp
  
  l_GV_Vorfuehrung[[ii]] <- tibble(
    Datum = c_Date[ii],
    Anfang = df_temp$Anfang,
    Ende = df_temp$Ende,
    `Suisa Nummer` = c_suisa_nr[ii],
    Filmtitel = df_temp$Filmtitel,
    `Gewinn/Verlust [CHF]` =round5Rappen(l_GV_Kiosk[[ii]]$Gewinn + l_GV[[ii]]$`Gewinn/Verlust [CHF]` + pull(df_manko_uerberschuss|>filter(Datum == c_Date[ii])) - c_Eventausgaben + c_Eventeinnahmen)
  )
}

df_GV_Vorfuehrung <- l_GV_Vorfuehrung |>
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
     `Gewinn Verlust Vorführung` = df_GV_Vorfuehrung,
     Verleiherabgaben  = df_verleiherabgaben,
     Einkaufspreise = df_Einkaufspreise,
     Spezialpreisekiosk = Spezialpreisekiosk,
     Ticketpreise = df_Kinopreise,
     Ausgaben = Einnahmen_und_Ausgaben[["Ausgaben"]],
     Einnahmen = Einnahmen_und_Ausgaben[["Einnahmen"]]
     )|>
  write.xlsx(file="output/Auswertung.xlsx", asTable = TRUE)

remove(l_Eintritt,  m, c_raw, l_GV, l_GV_Kiosk, c_Besucher,  df_Eventausgaben, l_Abgaben,
       c_suisaabzug, c_Gratis, c_Umsatz, l_GV_Vorfuehrung,ii, c_Eventausgaben,c_P_kat_verechnen, c_lenght, c_Brutto,
       convert_data_Film_txt, c_file, c_Verleiherrechnung, c_sheets, c_Kinofoerder_gratis, c_MWST_Abzug, c_Netto3, 
       c_Verleiher_garantie, c_Verleiherabzug,n_Film,n_kiosk,
       c_verleiherabzug_prozent)


########################################################################
# user interaction
########################################################################
writeLines("Datenkonvertierung erfolgt")

