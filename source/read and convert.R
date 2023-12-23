library(tidyverse)
library(rebus)
library(openxlsx)
library(lubridate)

file.remove("error.log")|>suppressWarnings()

source("source/functions.R")

########################################################################
# Read Ein und Ausgaben
c_file <- "Einnahmen und Ausgaben.xlsx"
c_sheets <- readxl::excel_sheets(paste0("Input/",c_file))
Einnahmen_und_Ausgaben <- lapply(c_sheets, function(x) {
  readxl::read_excel(paste0("Input/",c_file), 
                     sheet = x)
})
names(Einnahmen_und_Ausgaben) <- c_sheets
c_sheets

Einnahmen_und_Ausgaben[[1]] <- Einnahmen_und_Ausgaben[[1]]|>
  mutate(Spieldatum = as.Date(Spieldatum),
         Datum = as.Date(Datum))

Einnahmen_und_Ausgaben[[2]] <- Einnahmen_und_Ausgaben[[2]]|>
  mutate(Datum = as.Date(Datum))

Einnahmen_und_Ausgaben

########################################################################
# Function to read and convert data from Film.txt files ()
convert_data <- function(c_fileName) {
  l_data <- list()
  for(kk in 1:length(c_fileName)){
    # read in data
    c_raw <- suppressWarnings(readLines(c_fileName[kk]))
    c_raw
    l_temp <- list()
    
    # Extract Swisa
    p <- START%R%DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT #swisa
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
    
    # Extract Swisa-Vorabzug
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

c_files <- list.files(pattern = "Eintritte", recursive = T)
l_Eintritt <- convert_data(c_files)
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
# read Einkaufspreise 
c_files <- list.files(pattern = START%R%"Einkauf", recursive = T)
l_Einkaufspreise <- lapply(c_files, readxl::read_excel)

p <- one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)
names(l_Einkaufspreise) <- c_files|>str_extract(p)

df_Einkaufspreise <- l_Einkaufspreise |>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum)|>as.Date())
df_Einkaufspreise

########################################################################
# Kiosk Spezialpreise
Spezialpreisekiosk <- readxl::read_excel("Input/Spezialpreisekiosk.xlsx")|>
  mutate(Datum = as.Date(Datum))
Spezialpreisekiosk


########################################################################
# Suchen der korrekten Einkaufspreise
c_files<- list.files(pattern = START%R%"Kiosk", recursive = T)

c_Date_Kiosk <- c_files|>
  str_extract(DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT)|>
  dmy()|>
  as.Date()
  

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

df_Mapping_Einkaufspreise <- df_Mapping_Einkaufspreise|>
  apply(2, function(x)ifelse(x>=0, NA, x))|>
  apply(1, function(x){
    c_select <- max(x, na.rm = T)
    y <- x[c_select == x]
    y <- y[!is.na(y)]
    return(names(y))
  })
df_Mapping_Einkaufspreise <- tibble(Einkaufspreise = df_Mapping_Einkaufspreise|>as.Date(),
                                    Datum = names(df_Mapping_Einkaufspreise)|>as.Date())
  
df_Mapping_Einkaufspreise

########################################################################
# Convert data from "Kiosk xx.xx.xx.txt" files
ii <- 3
c_files[3]

l_Kiosk <- list()
for(ii in 1:length(c_files)){
  l_Kiosk[[ii]] <- read_delim(c_files[ii], 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(Platzkategorie = col_character(), 
                                               Preis = col_double(), Tax...3 = col_double(), 
                                               Anzahl = col_double(), Betrag = col_double(), 
                                               Kassiert = col_double(), Vorausbezahlt = col_double(), 
                                               Fakturieren = col_double(), Tax...9 = col_double()), 
                              trim_ws = F)|>
    suppressWarnings()|>
    suppressMessages()
  
  l_Kiosk[[ii]]
  # remove summary
  try(l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    filter(!(l_Kiosk[[ii]]$Platzkategorie|>str_detect(START%R%one_or_more(DGT)))),
    outFile = "error.log"
    )
  if(list.files(pattern = "error.log")|>length()>0) stop(paste0("\n",c_files[ii],"\nscheint ein unbekanntes format zu haben.Bitte korrigieren")) # Error handling

  # Spez Preise Kiosk
  index <- is.na(l_Kiosk[[ii]]$Preis)
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    mutate(Preis = if_else(index, l_Kiosk[[ii]]$Betrag/l_Kiosk[[ii]]$Anzahl,l_Kiosk[[ii]]$Preis),
           Datum = c_Date_Kiosk[ii])
  
  l_Kiosk[[ii]]
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    left_join(Spezialpreisekiosk, by = c("Datum","Platzkategorie" = "Spezialpreis"))
  l_Kiosk[[ii]]
  # Einkaufspreise
  c_select <- df_Mapping_Einkaufspreise|>
    filter(Datum == c_Date_Kiosk[ii])|>
    select(Einkaufspreise)|>
    pull()
  c_select  
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    mutate(Platzkategorie = if_else(!is.na(Artikelname),Artikelname,Platzkategorie)
           )|>
    left_join(df_Einkaufspreise|>
                filter(Datum == c_select),
              by = c(Platzkategorie ="Artikelname Kassensystem"))
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    mutate(Einkaufspreis = if_else(!is.na(Einkaufspreis),Einkaufspreis, `Einkaufs- preis`)
           )|>
    select(1:13)|>
    select(-Artikelname,-Verkaufspreis)
  
  # Error Handling: Kein Gewinn berchenbar da Einkuafspreis nicht vorhanden 
  if(is.na(l_Kiosk[[ii]]$Einkaufspreis)|>sum()>0){
    df_error <- l_Kiosk[[ii]]|>
      filter(is.na(Einkaufspreis))
    df_error
    stop( paste0("Datum:",day(c_Date_Kiosk[ii]),".",month(c_Date_Kiosk[ii]),".",year(c_Date_Kiosk[ii]),
                    "\nIm der Kioskabrechnung ist der Verkaufsartikel \"", df_error$Platzkategorie[1], "\" nicht korrekt definiert. \n",
                    "Bitte prüfe ob dieser Artikel in der \"Spezialpreisekiosk.xlsx\" oder in \"Einkauf Kiosk.xlsx\" definiert ist.\n"))
  }
  

}

df_Kiosk <- l_Kiosk|>
  bind_rows()|>
  mutate(`Tax...3` = NULL,
         Vorausbezahlt = NULL,
         Fakturieren = NULL,
         `Tax...9` = NULL
         )|>
  rename(Verkaufsartikel = Platzkategorie,
         Verkaufspreis = Preis,
         Datum = Datum.x)

df_Kiosk <- df_Kiosk|>
  mutate(Gewinn = Kassiert-(Anzahl*Einkaufspreis))|>
  select(Datum,Verkaufsartikel,Verkaufspreis,Anzahl,Betrag,Kassiert,Einkaufspreis,Gewinn)

########################################################################
# read show times

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
# Verleiherabgaben

df_verleiherabgaben <- readxl::read_excel("input/Verleiherabgaben.xlsx")|>
  mutate(Datum = as.Date(Datum))
df_verleiherabgaben

df_Eintritt <- df_Eintritt|>
  left_join(df_verleiherabgaben, by = c(`Suisa Nummer` = "Suisa", "Datum"))
df_Eintritt

########################################################################
# VerleiherRechnung 
 
df_Verleiher_Rechnnung <- df_Eintritt|>
  distinct(Datum,`Suisa Nummer`,.keep_all = T)|>
  select(Datum, Filmtitel, `Suisa Nummer`)
df_Verleiher_Rechnnung

df_Verleiher_Rechnnung <- df_Verleiher_Rechnnung|>
  left_join(Einnahmen_und_Ausgaben[["Ausgaben"]] |>
              filter(Kategorie == "Film")|>
              select(-Datum,-Kategorie),
            by = c("Datum" = "Spieldatum"))|>
  mutate(`keine Verleiherrechnung` = if_else(is.na(Betrag), T, F))
df_Verleiher_Rechnnung

df_keine_Rechnnung <- df_Verleiher_Rechnnung|>
  filter(`keine Verleiherrechnung`)


if(nrow(df_keine_Rechnnung)>0) {
  warning(paste0("\nAchtung für die diesen Film gibt es keine Verleiherrechnung: \n",
                   day(df_keine_Rechnnung$Datum),".",month(df_keine_Rechnnung$Datum),".", lubridate::year(df_keine_Rechnnung$Datum), " ",df_keine_Rechnnung$Filmtitel,"\n")
          )  
}


########################################################################
# Gewinn/Verlust Eintitt

l_GV <- list()
ii <- 1
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
  
  if(is.na(c_MWST_Abzug)) { # if no Verleiherrechnung just calculate MWST Abzug from Umsatz
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
           `Gewinn/Verlust [CHF]` = (Umsatz - sum(`SUISA-Abzug [CHF]`,`Verleiher-Abzug [CHF]`, `Sonstige Kosten [CHF]`, `MWST auf die Verleiherrechnung [CHF]`,na.rm = T))
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
# error handling Eintritt Abrechnung nicht vorhanden

error <- df_Kiosk|>
  distinct(Datum, .keep_all = T)|>
  left_join(df_Eintritt|>
              distinct(Datum)|>
              mutate(Eintrittabrechnung = T),
            by = join_by(Datum)
  )

if((error$Eintrittabrechnung|>is.na()|>sum())>0) {
  error <- error|>
    filter(is.na(Eintrittabrechnung))|>
    select(Datum)|>
    mutate(Datum = paste0(day(Datum),".",month(Datum),".",year(Datum)-2000))
  error <- str_c("\nEs  gibt keine Datei: ",
                 "Eintritt ",error$Datum,".txt",
                 "\nBitter herunterladen und Abspeichern unter:\n",
                 "Eintritt ",error$Datum,".txt"
                 )
  stop(error)
}


########################################################################
# error handling Kiosk Abrechnung

error <- df_Eintritt|>
  distinct(Datum, .keep_all = T)|>
  left_join(df_Kiosk|>
              distinct(Datum)|>
              mutate(Kioskabrechnung = T),
            by = join_by(Datum)
  )

if((error$Kioskabrechnung|>is.na()|>sum())>0) {
  error <- error|>
    filter(is.na(Kioskabrechnung))|>
    select(Datum, `Suisa Nummer`, Filmtitel)|>
    mutate(Datum = paste0(day(Datum),".",month(Datum),".",year(Datum)-2000))
  error <- str_c("\nFür den Film: ", error$Filmtitel,"am ", error$Datum, " mit Suisa-Nummer ", error$`Suisa Nummer`, " gib es keine Datei:\n",
                 "Kiosk ",error$Datum,".txt")
  stop(error)
}

########################################################################
# Gewinn Kiosk

ii <- 3

l_GV_Kiosk <- list()
for (ii in 1:length(c_Date)) {
  l_GV_Kiosk[[ii]] <- df_Kiosk|>
    filter(Datum == c_Date[ii])|>
    reframe(Kassiert = sum(Kassiert),
            Gewinn = sum(Gewinn, na.rm = T))|>
    mutate(Datum = c_Date[ii],
           `Suisa Nummer` =c_suisa_nr[ii])
  l_GV_Kiosk[[ii]]
}
l_GV_Kiosk

df_GV_Kiosk <- l_GV_Kiosk|>
  bind_rows()|>
  left_join(df_show, by = join_by(Datum, `Suisa Nummer`))|>
  select( Datum,Anfang, `Suisa Nummer`, Filmtitel, Kassiert, Gewinn)


########################################################################
# Gewinn Filmvorführung

l_GV_Vorfuehrung <- list()
for (ii in 1:length(c_Date)) {
  l_GV_Vorfuehrung[[ii]] <- tibble(Datum = c_Date[ii], 
                                   `Suisa Nummer` = c_suisa_nr[ii],
                                   `Gewinn/Verlust [CHF]` = l_GV_Kiosk[[ii]]$Gewinn + l_GV[[ii]]$`Gewinn/Verlust [CHF]`)
}

df_GV_Vorfuehrung <- l_GV_Vorfuehrung|>
  bind_rows()


########################################################################
# write to Excel
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

remove(l_Eintritt, l_Kiosk, c_files, m, c_raw, index, l_GV, l_GV_Kiosk, c_Besucher, c_suisa_nr, 
       c_suisaabzug, c_verleiherabzug, c_Gratis, c_Umsatz, l_GV_Vorfuehrung,ii,
       c_Einkaufslistendatum, c_select,p,l_Einkaufspreise,df_Mapping_Einkaufspreise,
       convert_data, c_Date_Kiosk,c_file, c_Verleiherrechnung, c_sheets)


# user interaction
writeLines("Datenkonvertierung erfolgt")

