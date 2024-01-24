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
# Extrakt Verkäufe
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
                            `Überschuss / Manko` = l_raw[[ii]][str_detect(l_raw[[ii]], "Manko")]|>str_extract(p3)
                            )
  }
names(l_extracted) <- c_fileDate

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
        Betrag = as.numeric(Betrag)
      )
    bind_rows(y,
         tibble(
           Verkaufsartikel = "Überschuss / Manko",
           Einzelpreis = as.numeric(NA),
           Anzahl = as.numeric(NA), 
           Betrag = x[[2]]|>as.numeric()
           )
         )
  })

l_Kiosk

df_Kioskverkauf <- bind_rows(l_Kiosk, .id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum))

########################################################################
# Kiosk Spezialpreise einlesen
Spezialpreisekiosk <- readxl::read_excel("Input/Spezialpreisekiosk.xlsx")|>
  mutate(Datum = as.Date(Datum))

Spezialpreisekiosk

########################################################################
# join data
l_Kiosk <- l_Kiosk |>
  lapply(function(x) {
    x |>
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

df_Kioskverkauf <- l_Kiosk|>
  bind_rows(.id = "Datum")

df_Kioskverkauf|>
  group_by(Datum)|>
  reframe(`Gewinn/Verlust` = sum(Betrag))

