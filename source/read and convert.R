library(tidyverse)
library(rebus)
library(openxlsx)
library(lubridate)
source("source/functions.R")

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
    names(l_temp)[ii] <- "Swisa"
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
      mutate(suisa_nr = l_temp[[1]],
             Filmtitel = l_temp[[2]],
             Datum_ = l_temp[[3]],
             `SUISA-Vorabzug` = l_temp[[4]]
             )
  }
  return(l_data)
}

# search data 
c_files <- list.files(pattern = "Eintritte", recursive = T)
c_files

c_fileName <- c_files

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
  select(Datum, Filmtitel,suisa_nr,Platzkategorie,Zahlend,Verkaufspreis, Anzahl,Umsatz,`SUISA-Vorabzug`)

df_Eintritt

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

# df_Einkaufspreise <- c_files|>
#   read.xlsx()|>
#   as_tibble()
# df_Einkaufspreise

########################################################################
# Read Kiosk Spezpreise
Spezialpreisekiosk <- readxl::read_excel("Input/Spezialpreisekiosk.xlsx")|>
  mutate(Datum = as.Date(Datum))
Spezialpreisekiosk

########################################################################
# Convert data from Kiosk.txt files

c_files <- list.files(pattern = START%R%"Kiosk", recursive = T)
c_files

c_Date <- c_files|>str_extract(DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT)|>
  dmy()|>
  as.Date()
c_Date

########################################################################
# Suchen der korreckten Einkaufspreise
distinct(df_Einkaufspreise, Datum)|>pull()

c_Einkaufslistendatum <- distinct(df_Einkaufspreise, Datum)|>pull()
c_Einkaufslistendatum

(c_Date[1]-c_Einkaufslistendatum[1])|>as.integer()
(c_Date[1]-c_Einkaufslistendatum[2])|>as.integer()

# df_temp <- lapply(c_Einkaufslistendatum, function(x)(x-c_Date)|>as.integer())|>
#   bind_cols()|>
#   as.matrix()|>
#   suppressMessages()
# 
# colnames(df_temp) <- c_Einkaufslistendatum|>
#   as.character()
# 
# df_temp|>
#   apply(2, function(x) ifelse(x<=0, x, NA))|>
#   apply(1, )
# 
# df_temp|>
#   as_tibble()|>
#   mutate(c_Date)


for (ii in 1:length(c_Date)) {
  (c_Date[ii]-distinct(df_Einkaufspreise, Datum)|>pull())|>
    print()
}

tibble(c_Date, c_Einkaufslistendatum[1])


l_Kiosk <- list()
for(ii in 1:length(c_files)){
  l_Kiosk[[ii]] <- read_delim(c_files[ii], 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(Platzkategorie = col_character(), 
                                               Preis = col_double(), Tax...3 = col_double(), 
                                               Anzahl = col_double(), Betrag = col_double(), 
                                               Kassiert = col_double(), Vorausbezahlt = col_double(), 
                                               Fakturieren = col_double(), Tax...9 = col_double()), 
                              trim_ws = TRUE)|>
    suppressWarnings()|>
    suppressMessages()
  
  # remove summary
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    filter(!(l_Kiosk[[ii]]$Platzkategorie|>str_detect(START%R%one_or_more(DGT))))
  
  # Spez Preise Kiosk
  index <- is.na(l_Kiosk[[ii]]$Preis)
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    mutate(Preis = if_else(index, l_Kiosk[[ii]]$Betrag/l_Kiosk[[ii]]$Anzahl,l_Kiosk[[ii]]$Preis),
           Datum = c_Date[ii])
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    left_join(Spezialpreisekiosk, by = c("Datum","Platzkategorie" = "Spezialpreis"))
  
  # Einkaufspreise
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    mutate(Platzkategorie = if_else(!is.na(Artikelname),Artikelname,Platzkategorie)
           )|>
    left_join(df_Einkaufspreise,
              by = c(Platzkategorie ="Artikelname Kassensystem"))
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]]|>
    mutate(Einkaufspreis = if_else(!is.na(Einkaufspreis),Einkaufspreis, `Einkaufs- preis`)
           )|>
    select(1:13)|>
    select(-Artikelname,-Verkaufspreis)

}

########################################################################
# 
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
c_raw <- list.files(pattern = "Shows", recursive = T)|>
  readLines()|>
  suppressWarnings()

m <- c_raw[3:length(c_raw)]|>
  str_split("\t", simplify = T)
m <- m[,1:(ncol(m)-1)] 
colnames(m) <- c_raw[2]|>
  str_split("\t", simplify = T)|>as.vector()
m

df_show <- m|>
  as_tibble()|>
  mutate(Datum = Tag|>lubridate::ymd(),
         Anfang = parse_time(Anfang),
         Ende = parse_time(Ende))|>
  select(Datum,Anfang, Ende, Saal, Titel, Version, Alter)|>
  rename(Filmtitel = Titel)

df_show

df_show <- df_show|>
  left_join(df_Eintritt|>
              distinct(Datum, suisa_nr),
            by = c("Datum" = "Datum")
            )

########################################################################
# Verleiherabgaben

df_verleiherabgaben <- readxl::read_excel("input/Verleiherabgaben.xlsx")|>
  mutate(Datum = as.Date(Datum))
df_verleiherabgaben

df_Eintritt <- df_Eintritt|>
  left_join(df_verleiherabgaben, by = c(suisa_nr = "Suisa", "Datum"))
df_Eintritt


########################################################################
# Gewinn/Verlust Eintitt

l_GV <- list()
c_Datum <- distinct(df_Eintritt, Datum)|>pull()
c_suisa_nr <- distinct(df_Eintritt, suisa_nr )|>pull()
c_suisa_nr
c_Datum


ii <- 1
for (ii in 1:length(c_Datum)) {
  c_Besucher <- df_Eintritt|>
    filter(Datum == c_Datum[ii])|>
    reframe(Besucherzahl = sum(Anzahl))|>
    pull()
  c_Besucher
  
  c_Gratis <- df_Eintritt|>
    filter(Datum == c_Datum[ii], !Zahlend)|>
    reframe(Gratiseintritte = sum(Anzahl))|>
    pull()
  c_Gratis
  
  c_Umsatz <- df_Eintritt|>
    filter(Datum == c_Datum[ii])|>
    reframe(Umsatz = sum(Umsatz))|>
    pull()
  
  c_verleiherabzug <- (distinct(df_Eintritt|>filter(Datum == c_Datum[ii]), 
                               `Abzug [%]`)|>pull())/100
  c_verleiherabzug
  
  c_suisaabzug <- (distinct(df_Eintritt|>filter(Datum == c_Datum[ii]), 
                            `SUISA-Vorabzug`)|>pull())/100
  c_suisaabzug
  
  l_GV[[ii]] <- tibble(Datum = c_Datum[ii],
                       suisa_nr = c_suisa_nr[ii],
                       Umsatz = c_Umsatz, 
                       `SUISA-Abzug [%]` = c_suisaabzug*100,
                       `SUISA-Abzug [sFr]` = round5Rappen(c_Umsatz*c_suisaabzug), 
                       `Verleiher-Abzug [%]` =c_verleiherabzug*100,
                       `Verleiher-Abzug [sFr]` = round5Rappen(c_Umsatz*c_verleiherabzug))|>
    mutate(`Verleiher-Abzug [sFr]` = if_else(`Verleiher-Abzug [sFr]` > 150, `Verleiher-Abzug [sFr]`, 150),
           `Gewinn/Verlust` = Umsatz-(`SUISA-Abzug [sFr]`+`Verleiher-Abzug [sFr]`))|>
    left_join(df_show, by = join_by(Datum, suisa_nr))
}
df_GV_Eintritt <- l_GV|>
  bind_rows()|>
  select( Datum,Anfang, suisa_nr, Filmtitel, Umsatz,`SUISA-Abzug [%]`,`Verleiher-Abzug [%]` ,`SUISA-Abzug [sFr]`, `Verleiher-Abzug [sFr]`, `Gewinn/Verlust`)

df_GV_Eintritt


########################################################################
# Gewinn Kiosk
l_GV_Kiosk <- list()
for (ii in 1:length(c_Datum)) {
  l_GV_Kiosk[[ii]] <- df_Kiosk|>
    filter(Datum == c_Datum[ii])|>
    reframe(Kassiert = sum(Kassiert),
            Gewinn = sum(Gewinn)
    )|>
    mutate(Datum = c_Datum[ii],
           suisa_nr =c_suisa_nr[ii])
}
df_GV_Kiosk <- l_GV_Kiosk|>
  bind_rows()|>
  left_join(df_show, by = join_by(Datum, suisa_nr))|>
  select( Datum,Anfang, suisa_nr, Filmtitel, Kassiert, Gewinn)

########################################################################
# Gewinn Filmvorführung
l_GV_Vorfuehrung <- list()
for (ii in 1:length(c_Datum)) {
  l_GV_Vorfuehrung[[ii]] <- tibble(Datum = c_Datum[ii], 
                                   suisa_nr = c_suisa_nr[ii],
                                   `Gewinn/Verlust` = l_GV_Kiosk[[ii]]$Gewinn + l_GV[[ii]]$`Gewinn/Verlust`)
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
     Spezialpreisekiosk = Spezialpreisekiosk)|>
  write.xlsx(file="output/Auswertung.xlsx", asTable = TRUE)

remove(l_Eintritt, l_Kiosk, c_fileName,c_files, m, c_raw, index, l_GV, l_GV_Kiosk, c_Besucher, c_suisa_nr, c_suisaabzug, c_verleiherabzug, c_Gratis, c_Date, c_Umsatz, c_Datum,l_GV_Vorfuehrung,ii)

# user interaction
writeLines("file conversion done")

