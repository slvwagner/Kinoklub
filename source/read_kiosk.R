## read in all kiosk files (neu)

library(rebus)
library(tidyverse)
library(readxl)

######################################################################## 
# Read in files
######################################################################## 

# verkaufsartikel
c_files <- list.files(pattern = "Einkauf Kiosk", recursive = T)
df_verkaufsartikel <- read_excel(c_files)

# Kioskabrechnung
c_files <- list.files(pattern = "Kiosk_neu", recursive = T)
l_raw <- lapply(c_files, function (x) suppressWarnings(readLines(x)))

## extract file date 
c_fileDate <- str_match(c_files,capture(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))%R%DOT%R%"txt" )[,2]

######################################################################## 
# Extrakt Verkäufe
######################################################################## 

# detect Verkaufarikel in string
p <- or1(paste0(df_verkaufsartikel$`Artikelname Kassensystem`))

l_extracted <- list()

for (ii in 1:length(l_raw)) {
  l_extracted[[ii]] <- tibble(Verkaufartikel_string = l_raw[[ii]][str_detect(l_raw[[ii]], p)])
}
names(l_extracted) <- c_fileDate
l_extracted

p <- "Manko" 
for (ii in 1:length(l_raw)) {
  l_extracted[[ii]]["Überschuss / Manko"] <- l_raw[[ii]][str_detect(l_raw[[ii]], "Manko")]
}

df_Kioskverkauf <- l_extracted|>
  lapply(function(x){
    y <- x$Verkaufartikel_string|>
      str_split(pattern = "\t", simplify = T)
    colnames(y) <- c("Verkaufsartikel", "Einzelpreis", "Anzahl", "V4", "Betrag")
    y <- as_tibble(y)|>
      select(-V4)|>
      mutate(Einzelpreis = as.numeric(Einzelpreis),
             Anzahl = as.numeric(Anzahl),
             Betrag = as.numeric(Betrag))
    y$`Überschuss / Manko` <- str_extract(x$`Überschuss / Manko`, optional("-")%R%one_or_more(DGT)%R%optional(DOT%R%one_or_more(DGT)))|>
      as.numeric()
    return(y)
  })

df_Kioskverkauf

df_Kioskverkauf <- bind_rows(df_Kioskverkauf, .id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum))

df_Kioskverkauf|>
  group_by(Datum)|>
  reframe(Betrag = sum(Betrag),
          `Überschuss / Manko` = `Überschuss / Manko`[1])




