## read in all kiosk files

library(rebus)
library(tidyverse)
library(readxl)

source("source/functions.R")

if(!r_is.defined(c_MWST)){
  c_MWST <- 8.1
  }

######################################################################## 
# Read in files
######################################################################## 

# verkaufsartikel
c_files <- list.files(pattern = "Einkauf Kiosk", recursive = T)

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
p3 <- optional("-") %R% one_or_more(DGT) %R% optional(DOT)%R% one_or_more(DGT)



ii <- 3

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
    return(y)
  })
l_Kiosk

# Wie viele Spalten
c_lenght <- l_Kiosk|>
  lapply(ncol)|>
  unlist()

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
    
    l_Kiosk[[ii]] <- bind_cols(Verkaufsartikel = l_Kiosk[[ii]][,1], x)
    
  }else if(c_lenght[ii] == 5){ # keine Korrekturbuchungen
    l_Kiosk[[ii]] <- l_Kiosk[[ii]][,c(1:3,5)]
    x <- l_Kiosk[[ii]][,2:ncol(l_Kiosk[[ii]])]|>
      apply(2, as.numeric)
    colnames(x) <- c("Einzelpreis", "Anzahl", "Betrag")
    
    l_Kiosk[[ii]] <- bind_cols(Verkaufsartikel = l_Kiosk[[ii]][,1], x)
  }else {
    stop(paste0("\nDie Datei: input/advance tickets/Kiosk ",names(l_Kiosk)[ii],".txt", 
                "\nhat hat ein anderes Format und ist noch nicht implementiert.\nBitte wenden dich an die Entwicklung"))
    }
}

l_Kiosk

df_Kiosk <- l_Kiosk|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = dmy(Datum),
         Einzelpreis = if_else(is.na(Einzelpreis), Betrag / Anzahl, Einzelpreis),
         Betrag = if_else(Anzahl == 0, 0, Betrag))
df_Kiosk

#############################################################################################
# Error handling
# Detect date in file 
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
    paste0("Für das file: .../Kinoklub/Input/advance tickets/Kiosk ",c_fileDate[!c_test], " stimmt das Datum nicht mit dem Datum im File überein.")|>
    paste0(collapse = "\n")|>
    writeLines()
    )
}



######################################################################## 
# Extrakt Überschuss / Manko
######################################################################## 
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


# error handling
# Sind für alle Spezialpreise pro Datum definiert?  
df_spez_preis_na <- df_Kiosk|>
  filter(str_detect(Verkaufsartikel, "Spez")) |>
  distinct(Datum, .keep_all = T) |>
  left_join(
    df_Eintritt |>
      distinct(Datum, .keep_all = T) |>
      select(Datum, Filmtitel, `Suisa Nummer`),
    by = join_by(Datum)
  )|>
  anti_join(Spezialpreisekiosk |>distinct(Datum),
            by = join_by(Datum))

if(nrow(df_spez_preis_na) > 0) {
  warning(
    paste0(
      "\nFür die Filmvorführung ", df_spez_preis_na$Filmtitel, " am ", day(df_spez_preis_na$Datum),".",month(df_spez_preis_na$Datum),".",year(df_spez_preis_na$Datum), 
      " wurde der Artikel ", df_spez_preis_na$Verkaufsartikel," nicht definiert.",
      "\nBitte korrigieren in der Datei:","\n.../Kinoklub/input/Spezialpreisekiosk.xlsx\n"
    )
  )
}

########################################################################
# join Spezpreise mit Verkaufsartikel
########################################################################
ii <- 1

df_Kiosk <- df_Kiosk|>
  left_join(Spezialpreisekiosk, 
            by = c(Datum ="Datum", Verkaufsartikel = "Spezialpreis")
            )|>
  mutate(Verkaufsartikel = if_else(is.na(Artikelname), Verkaufsartikel, Artikelname))|>
  select(-Artikelname, -Verkaufspreis, -`Anzahl verkaufter Artikel`)


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
# Join Einkaufspreise 
########################################################################

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



########################################################################
# Gewinn
########################################################################

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
       l_extracted, l_raw, 
       c_Date_Kiosk, c_Einkaufslistendatum,
       p,p1,p2,p3,
       ii,x,
       c_path, c_fileDate, c_files)

## User interaction
writeLines("\nKiosk data read")
