## read in all kiosk files

library(rebus)
library(tidyverse)
library(lubridate)
library(readxl)

source("source/functions.R")

######################################################################## 
# files to read in 
######################################################################## 
c_path <- "input/advance tickets/"
c_files <- c(list.files(path = c_path,pattern = "Abo Gutscheine", recursive = T), list.files(path = c_path, pattern = "Kiosk", recursive = F) )
c_files


l_raw <- lapply(c_files, function (x) suppressWarnings(readLines(paste0(c_path,x))))
l_raw

## extract file date 
p <- capture(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))%R%DOT%R%"txt"

c_fileDate <- str_match(c_files, p)[,2]
c_fileDate

p <- rebus::or(rebus::optional("-")%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT),
               rebus::optional("-")%R%one_or_more(DGT)
               )

######################################################################## 
# Verkauf: Abo 
########################################################################

l_Abos <- l_raw|>
  lapply(function(x){
    y <- x[str_detect(x,START%R%"Verkauf Abo")]|>
      str_split(pattern = "\t", simplify = T)
    y
    c_lenght <- ncol(y)
    c_lenght
    
    if(c_lenght == 7){ # mit Korrekturbuchungen
      if(nrow(y) == 1){ #matrix convertierung in vector verhindern ncol = 1
        y <- y[,c(1:2,4:5,7)]|>
          matrix(ncol = 5)
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          lapply(as.numeric)|>
          unlist()|>
          matrix(ncol = 4)
      }else{
        y <- y[,c(1:2,4:5,7)]
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          apply(2, as.numeric)
      }
      y
      
      colnames(y) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)|>
        mutate(Anzahl = if_else(is.na(Korrektur), Anzahl,Anzahl + Korrektur))|>
        select(-Korrektur)
      
    }else if(c_lenght == 5){ # keine Korrekturbuchungen
      y <- y[,c(1:3,5)]
      c_Verkaufsartikel <- y[,1]
      y <- y[,2:ncol(y)]|>
        apply(2, as.numeric)
      colnames(y) <- c("Einzelpreis", "Anzahl", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)
    }
  })

names(l_Abos) <- dmy(c_fileDate)
l_Abos

df_Abo_Verkauf <- bind_rows(l_Abos,.id = "Datum")|>
  mutate(Datum = as.Date(Datum))

df_Abo_Verkauf

######################################################################## 
# Verkauf: Kinogutschein
######################################################################## 
p <- rebus::or(rebus::optional("-")%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT),
               rebus::optional("-")%R%one_or_more(DGT)
)

l_Abos <- l_raw|>
  lapply(function(x){
    y <- x[str_detect(x,START%R%"Verkauf Kinogutschein")]|>
      str_split(pattern = "\t", simplify = T)
    y
    c_lenght <- ncol(y)
    c_lenght
    
    if(c_lenght == 7){ # mit Korrekturbuchungen
      if(nrow(y) == 1){ #matrix convertierung in vector verhindern ncol = 1
        y <- y[,c(1:2,4:5,7)]|>
          matrix(ncol = 5)
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          lapply(as.numeric)|>
          unlist()|>
          matrix(ncol = 4)
      }else{
        y <- y[,c(1:2,4:5,7)]
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          apply(2, as.numeric)
      }
      y
      
      colnames(y) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)|>
        mutate(Anzahl = if_else(is.na(Korrektur), Anzahl,Anzahl + Korrektur))|>
        select(-Korrektur)
      
    }else if(c_lenght == 5){ # keine Korrekturbuchungen
      y <- y[,c(1:3,5)]
      c_Verkaufsartikel <- y[,1]
      y <- y[,2:ncol(y)]|>
        apply(2, as.numeric)
      colnames(y) <- c("Einzelpreis", "Anzahl", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)
    }
  })

names(l_Abos) <- dmy(c_fileDate)

df_Kinogutschein_Verkauf <- bind_rows(l_Abos,.id = "Datum")|>
  mutate(Datum = as.Date(Datum))
  
df_Kinogutschein_Verkauf


######################################################################## 
# Eingelöst: Kinogutschein
######################################################################## 
p <- rebus::or(rebus::optional("-")%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT),
               rebus::optional("-")%R%one_or_more(DGT)
)

l_Abos <- l_raw|>
  lapply(function(x){
    y <- x[str_detect(x,START%R%"Kinogutschein")]|>
      str_split(pattern = "\t", simplify = T)
    y
    c_lenght <- ncol(y)
    c_lenght
    
    if(c_lenght == 7){ # mit Korrekturbuchungen
      if(nrow(y) == 1){ #matrix convertierung in vector verhindern ncol = 1
        y <- y[,c(1:2,4:5,7)]|>
          matrix(ncol = 5)
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          lapply(as.numeric)|>
          unlist()|>
          matrix(ncol = 4)
      }else{
        y <- y[,c(1:2,4:5,7)]
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          apply(2, as.numeric)
      }
      y
      
      colnames(y) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)|>
        mutate(Anzahl = if_else(is.na(Korrektur), Anzahl,Anzahl + Korrektur))|>
        select(-Korrektur)
      
    }else if(c_lenght == 5){ # keine Korrekturbuchungen
      y <- y[,c(1:3,5)]
      c_Verkaufsartikel <- y[,1]
      y <- y[,2:ncol(y)]|>
        apply(2, as.numeric)
      colnames(y) <- c("Einzelpreis", "Anzahl", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)
    }
  })

names(l_Abos) <- dmy(c_fileDate)
l_Abos

df_Kinogutschein_Eingeloest <- bind_rows(l_Abos,.id = "Datum")|>
  mutate(Datum = as.Date(Datum))|>
  left_join(df_show, by = join_by(Datum))


######################################################################## 
# Kassabestand : Solde
######################################################################## 
p <- rebus::or(rebus::optional("-")%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT),
               rebus::optional("-")%R%one_or_more(DGT)
)
x <- l_raw[[1]]
  
l_Abos <- l_raw|>
  lapply(function(x){
    y <- x[str_detect(x,START%R%"Solde")]|>
      str_split(pattern = "\t", simplify = T)
    y <- y[,ncol(y)]
    if(is.vector(y)) return(tibble(Saldo = y))
    })
names(l_Abos) <- dmy(c_fileDate)

df_KassaSaldo <- bind_rows(l_Abos,.id = "Datum")|>
  mutate(Datum = as.Date(Datum),
         Saldo = as.numeric(Saldo))|>
  left_join(df_show, by = join_by(Datum))

df_KassaSaldo


## User interaction
writeLines("\nVerkauf von Abos und Gutscheinen")
