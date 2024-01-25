## read in all kiosk files (neu)

library(rebus)
library(tidyverse)
library(readxl)

######################################################################## 
# Read in files
######################################################################## 

# verkaufsartikel
c_files <- list.files(pattern = "Einkauf Kiosk", recursive = T)

if(sum(str_detect(c_files, "~")) > 0) stop("\nEinkauf Kiosk wurde mit Excel geöffnet. Bitte schliessen!")
df_verkaufsartikel <- read_excel(c_files)

c_path <- "input/advance tickets"
# Kioskabrechnung
c_files <- list.files(c_path,pattern = "Kiosk", recursive = T)
c_files <- paste0(c_path,"/", c_files)

l_raw <- lapply(c_files, function (x) suppressWarnings(readLines(x)))
l_raw

## extract file date 
c_fileDate <- str_match(c_files,capture(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))%R%DOT%R%"txt" )[,2]

######################################################################## 
# Extrakt Verkäufe  und Überschuss / Manko
######################################################################## 

# detect Verkaufarikel in string
p1 <- or1(paste0(df_verkaufsartikel$`Artikelname Kassensystem`))

# detect Spez Preise 
p2 <- or1(paste0("Spez"%R%SPC, 1:4))

# Detect Überschuss Manko 
p3 <- optional("-") %R% one_or_more(DGT) %R% optional(DOT %R% one_or_more(DGT))

l_extracted <- list()
for (ii in 1:length(l_raw)) {
  l_extracted[[ii]] <- list(Verkaufsartikel = tibble(Verkaufartikel_string = c(l_raw[[ii]][str_detect(l_raw[[ii]], p1)], ## Arikel erfasst in Kassasystem
                                                        l_raw[[ii]][str_detect(l_raw[[ii]], p2)] ## Spez Arikel
                                                        )),
                            tibble(`Überschuss / Manko` = l_raw[[ii]][str_detect(l_raw[[ii]], "Manko")]|>str_extract(p3)|>as.numeric())
                            )
  }
names(l_extracted) <- c_fileDate
l_extracted

l_Kiosk <- l_extracted |>
  lapply(function(x) {
    y <- x[[1]]$Verkaufartikel_string |>
      str_split(pattern = "\t", simplify = T)
    colnames(y) <- c("Verkaufsartikel", "Einzelpreis", "Anzahl", "V4", "Betrag")
    y <- as_tibble(y) |>
      select(-V4) |>
      mutate(
        Einzelpreis = as.numeric(Einzelpreis),
        Anzahl = as.numeric(Anzahl),
        Betrag = as.numeric(Betrag),
        Einzelpreis = Betrag / Anzahl
      )
  })
l_Kiosk

df_manko_uerberschuss <- l_extracted |>
  lapply(function(x) {
     x[[2]]
  })|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum))
df_manko_uerberschuss

########################################################################
# Kiosk Spez Verkaufsartikel / Spezialpreise einlesen
########################################################################

Spezialpreisekiosk <- readxl::read_excel("Input/Spezialpreisekiosk.xlsx")|>
  mutate(Datum = as.Date(Datum))

Spezialpreisekiosk

########################################################################
# join Spez Verkaufsartikel
########################################################################

l_Kiosk <- l_Kiosk |>
  lapply(function(x) {
     y <- x|>
      left_join(
        Spezialpreisekiosk |>
          filter(Datum == pull(distinct(
            Spezialpreisekiosk, Datum
          ))[ii]),
        by = c(Verkaufsartikel = "Spezialpreis")
      ) |>
      mutate(
        Verkaufsartikel = if_else(is.na(Datum), Verkaufsartikel, Artikelname),
        Einzelpreis = Betrag / Anzahl
      )|>
      select(-Artikelname, -`Anzahl verkaufter Artikel`, -Verkaufspreis, -Datum)
  })
names(l_Kiosk) <- c_fileDate
l_Kiosk

########################################################################
# Kiosk Einkaufspreise 
########################################################################

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

########################################################################
# Kiosk Verkauf 
########################################################################

c_files <- list.files(c_path,pattern = START%R%"Kiosk", recursive = T)
c_files <- c_files <- paste0(c_path,"/", c_files)
c_files

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
# Kioskabrechnungen von advanced Tickets
# Convert data from "Kiosk xx.xx.xx.txt" files
########################################################################

ii <- 2
for(ii in 1:length(c_files)){
  
  # Einkaufspreise
  c_select <- df_Mapping_Einkaufspreise|>
    filter(Datum == c_Date_Kiosk[ii])|>
    select(Einkaufspreise)|>
    pull()
  c_select  
  
  l_Kiosk[[ii]]
  
  l_Kiosk[[ii]] <- l_Kiosk[[ii]] |>
        left_join(
          df_Einkaufspreise |>
            select(-((ncol(df_Einkaufspreise)-1):ncol(df_Einkaufspreise)))|>
            filter(Datum == c_select)|>
            rename(Einkaufspreis = `Einkaufs- preis`),
          by = c(Verkaufsartikel  = "Artikelname Kassensystem")
        )|>
    select(-Datum, -`Verkaufs-preis`, -Menge, -Lieferant, -Gewinn, -Artikel)
}

l_Kiosk

df_Kiosk <- l_Kiosk|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum))

df_Kiosk <- df_Kiosk|>
  mutate(Gewinn = Betrag-(Anzahl*Einkaufspreis))|>
  rename(Kassiert = Betrag,
         Verkaufspreis = Einzelpreis)

# Test
df_Kiosk|>
  group_by(Datum)|>
  reframe(`Gewinn/Verlust` = sum(Kassiert))

# remove no more needed variables
remove(df_Mapping_Einkaufspreise,l_Kiosk, l_Einkaufspreise,
       df_verkaufsartikel,
       l_extracted, l_raw, 
       c_Date_Kiosk, c_Einkaufslistendatum,
       p,p1,p2,p3,
       ii,
       c_select, 
       c_path, c_fileDate, c_files)

## User interaction
writeLines("\nKiosk data read")
