library(tidyverse)
library(rebus)
library(openxlsx)
library(lubridate)
library(readxl)

## pleas set your working directory 
setwd("Z:/R Packages/Kinoklub")
# setwd("D:/Kinoklub")

#######################################################################################
# spez. Round for Swiss currency "CHF"
#######################################################################################

round5Rappen <- function(zahl) {
  result <- lapply(zahl, function(zahl){
    if(is.na(zahl)){
      return(NA)
    }
    else{
      x <- round(zahl-as.integer(zahl*10)/10,6)
      if(x>=0.075){
        return((as.integer(zahl*10)/10)+0.1)
      }else {
        if(x>=0.025){
          return((as.integer(zahl*10)/10)+0.05)
        }else{
          return((as.integer(zahl*10)/10)+0.0)
        }
      }  
    }
  })
  result|>
    unlist()
}

#######################################################################################
# variable is present in global environment
#######################################################################################
r_is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}

# library is loaded in global environment
r_is.library_loaded <- function(package_name) {
  is_loaded <- FALSE
  tryCatch({
    is_loaded <- requireNamespace(package_name, quietly = TRUE)
  }, error = function(e) {
    is_loaded <- FALSE
  })
  return(is_loaded)
}

##  --------------------------------------------------------
##  Cleanup R console: clc
##  Version 1.0
##  by Hubert Ronald
##  --------------------------------------------------------
##  check first if you have "clc.R" in your currently root
##  sum(list.files() %in% "clc.R")

##  if TRUE then
##  Write in console: source("clc.R")
##  else
##  setwd() or source("yourPath/clc.R")

##  after write "clc" in console or when 
##  you need clean it

##  Console or Terminal'll be clean

##  --------------------------------------------------------
##  Reference:

##  English
##  https://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r-and-rstudio

##  Espaniol - Spanish
##  https://es.stackoverflow.com/questions/237112/c%C3%B3mo-limpiar-la-consola-de-rstudio-y-los-terminales-rstudio-y-macos
##  --------------------------------------------------------

##  The MIT License
##  Copyright 2019 Hubert Ronald

##  Permission is hereby granted, free of charge, to any person obtaining a copy
##  of this software and associated documentation files (the "Software"), to deal
##  in the Software without restriction, including without limitation the rights
##  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
##  copies of the Software, and to permit persons to whom the Software is furnished 
##  to do so, subject to the following conditions:

##  The above copyright notice and this permission notice shall be included in 
##  all copies or substantial portions of the Software.

##  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
##  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
##  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
##  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
##  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
##  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
##  IN THE SOFTWARE.


##  ------------------------------------------------------------
##  Detect RStudio Terminal or RStudio Console or Terminal macOS
##  --------------------------------------------------------------
if (commandArgs()[1]=='RStudio'){
  ##  method print: \f: Form Feed
  print.cleanup <- function(cleanupObject) cat("\f")     
  
  
}else if(substr(commandArgs()[1], nchar(commandArgs()[1]), nchar(commandArgs()[1])) == "R"){        
  
  ##  -------------------------------------------------------------
  ##  }else if(tail(strsplit(commandArgs()[1], split = "")[[1]], n=length(commandArgs()[1])))       
  ##  -------------------------------------------------------------
  ##  Reference
  ##  Rapp defaul configuration in macOS:
  ##  "/Library/Frameworks/R.framework/Resources/bin/exec/R"
  ##  --------------------------------------------------------------
  ##  http://ascii-table.com/ansi-escape-sequences.php                                                (check)
  ##  https://invisible-island.net/ncurses/man/clear.1.html                                           (check)
  ##  https://stackoverflow.com/questions/1348563/clearing-output-of-a-terminal-program-linux-c-c     (check)
  print.cleanup <- function(cleanupObject) cat(c("\033[2J","\033[H"))
  
}else{print(paste0("not support: ",commandArgs()[1]))}                                                                         


##  ----------------------------------------
##  About 'clc'
##  ----------------------------------------
##  Can replace 'clc' by:

##  'cls' command DOS
##  'clear' terminal macOS / Linux / Unix
##  or whatever you choose

##  Directly in this code or in console or terminal

##  For example change clc by clear
##  source("clc.R") or source("yourPath/clc.R")
##  clear <- 0
##  class(clear) <- 'cleanup'
##  ----------------------------------------                                                                     
clc <- 0                                        ##  variable from class numeric
class(clc) <- 'cleanup'                         ##  class cleanup
print(clc)                                      ##  when you load this source,
##  it cleans all console


################################################################################
#
################################################################################
create_df <- function(c_Rmd) {
  p <- "^```"
  df_data <- data.frame(
    index = 1:length(c_Rmd),
    c_Rmd,
    code_sections = lapply(c_Rmd, function(x)
      stringr::str_detect(x, p)) |> unlist(),
    is.heading = stringr::str_detect(c_Rmd, "^#")
  )
  
  # search and exclude code sections
  c_start_ii <- 0
  for (ii in 1:nrow(df_data)) {
    if (df_data$code_sections[ii] &  (c_start_ii != 0)) {
      df_data$code_sections[c_start_ii:ii] <-
        rep(TRUE, length(c_start_ii:ii))
      c_start_ii <- 0
    } else if (df_data$code_sections[ii]) {
      c_start_ii <- ii
    }
  }
  
  # remove heading in code section
  df_data$is.heading <- ifelse(df_data$code_sections, FALSE, df_data$is.heading)
  
  # Store headings
  df_data$`#` <- stringr::str_detect(df_data$c_Rmd, "^#\\s") |> ifelse(1, 0)
  df_data$`##` <-  stringr::str_detect(df_data$c_Rmd, "^##\\s") |> ifelse(1, 0)
  df_data$`###` <- stringr::str_detect(df_data$c_Rmd, "^###\\s") |> ifelse(1, 0)
  df_data$`####` <- stringr::str_detect(df_data$c_Rmd, "^####\\s") |> ifelse(1, 0)
  df_data$`#####` <- stringr::str_detect(df_data$c_Rmd, "^#####\\s") |> ifelse(1, 0)
  df_data$`######` <- stringr::str_detect(df_data$c_Rmd, "^######\\s") |> ifelse(1, 0)
  return(df_data)
}

################################################################################
#
################################################################################
r_toc_for_Rmd <- function(
    c_Rmd,
    toc_heading_string = "Table of Contents" ,
    create_nb = TRUE, create_top_link = TRUE , nb_front = TRUE, set_first_heading_level = FALSE,
    pagebreak_level = "non"
)
{
  ##########################################################################
  # create data frame to work with
  df_data <- create_df(c_Rmd)
  
  ##########################################################################
  # Headings
  m <- df_data[df_data$is.heading, 5:ncol(df_data)]
  
  ##########################################################################
  # Analyze heading structure
  heading_struct <- m|>
    apply(2, function(x) {
      sum(x)>0
    })
  
  # highest order heading column index
  for (ii in 1:ncol(m)) {
    if(heading_struct[ii]) {
      highest_order_jj <- ii
      break
    }
  }
  
  # highest order heading row index
  for (ii in 1:nrow(m)) {
    if(m[ii,highest_order_jj]) {
      highest_order_ii <- ii
      break
    }
  }
  
  # find first heading
  for (ii in 1:6) {
    if(m[1,ii]>0){
      first_heading_column <- ii
    }
  }
  
  ##########################################################################
  # correct heading structure
  c_names <- c("#","##","###","####","#####","######")
  
  if(highest_order_jj != first_heading_column){
    # correct structure
    temp <- m[1:(highest_order_ii-1),first_heading_column:6]
    temp
    temp <- switch (first_heading_column ,
                    temp = temp,
                    temp = cbind(temp,p1 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp)),p5 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp)),p5 = rep(0,nrow(temp)),p6 = rep(0,nrow(temp))),
    )
    temp
    
    temp1 <- m[highest_order_ii:nrow(m),highest_order_jj:6]
    temp1 <- switch (highest_order_jj,
                     temp1 = temp1,
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1)),p5 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1)),p5 = rep(0,nrow(temp1)),p6 = rep(0,nrow(temp1))),
    )
    names(temp1) <- c_names[highest_order_jj:6]
    names(temp) <- c_names[highest_order_jj:6]
    m_ <- rbind(temp,temp1)
  }else{
    if(highest_order_jj>0){ # remove not populated columns
      m_ <- switch (highest_order_jj,
                    m_ = m,
                    m_ = cbind(m[,2:6],p1 = rep(0,nrow(m))),
                    m_ = cbind(m[,3:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m))),
                    m_ = cbind(m[,4:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m))),
                    m_ = cbind(m[,5:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m)),p4 = rep(0,nrow(m))),
                    m_ = cbind(m[,6:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m)),p4 = rep(0,nrow(m)),p5 = rep(0,nrow(m)))
      )
    }else{
      m_ <- m
    }
  }
  m_
  
  ##########################################################################
  # create structure number system
  # Heading structure counts
  heading_cnt <- rep(0, 6)
  heading_cnt_ <- rep(0, 6)
  last_heading_edited <- 0
  
  # Structure string
  c_add <- c("* ",
             "    + ",
             "        + ",
             "            + ",
             "                + ",
             "                    + ")
  
  c_add_structure <- 1:nrow(m_)
  column_cnt <- 0
  m__ <- m_
  c_Heading_level <- 1:nrow(m_)
  for (ii in 1:nrow(m_)) {
    for (jj in 1:6)
      if (m_[ii, jj] > 0) {
        heading_cnt[jj] <- heading_cnt[jj] + 1
        if (last_heading_edited > jj) {
          # if heading order changes to higher order clear heading_cnt accordingly
          heading_cnt[(jj + 1):length(heading_cnt)] <- 0
          
        }
        last_heading_edited <- jj
        break
      }
    m__[ii, 1:6] <- heading_cnt
    heading_cnt_ <- heading_cnt
    if(set_first_heading_level){
      c_Heading_level[ii] <- c_names[jj]
    }else{
      c_Heading_level[ii] <- c_names[jj + (highest_order_jj-1)]
    }
    c_add_structure[ii] <- c_add[jj]
    
  }
  
  ##########################################################################
  # create structure number
  c_nb <- m__ |>
    apply(1, function(x) {
      temp <- x[x > 0]
      paste0(temp, collapse = ".")
    })
  
  ##########################################################################
  # create link link to table of contents
  c_top_link <-  paste0("\n[", toc_heading_string, "](#", toc_heading_string, ")\n")
  c_top_link
  
  ##########################################################################
  c_Heading <- c_Rmd[df_data$is.heading]|>stringr::str_remove_all("#")|>stringr::str_trim()
  c_Heading
  
  ##########################################################################
  # create anchor
  if (create_nb) {
    if (nb_front) { # number system in front of heading
      c_anchor <- paste0(
        c_Heading_level," " , c_nb, " ", c_Heading ,
        "<a name=\"",
        "A_", # add some characters to ensure html links will work
        c_nb, "_", c_Heading ,
        "\"></a>",
        if(create_top_link) c_top_link
      )
      c_toc <- paste0("[", c_nb,  " ", c_Heading,"](#A_", c_nb,"_", c_Heading, ")")
    } else {  # heading flowed by number system
      c_anchor <- paste0(
        c_Heading_level, " " , c_Heading, " ", c_nb,
        "<a name=\"",
        "A_", # add some characters to ensure html links will work
        c_Heading, " ", c_nb,
        "\"></a>",
        if(create_top_link) c_top_link
      )
      c_toc <- paste0("[", c_Heading, " ",c_nb,"](#A_", c_Heading," ",c_nb,")")
    }
  } else { # No numbering system / Do not Include number system
    c_anchor <- paste0(
      c_Heading_level, " ", c_Heading,
      "<a name=\"",
      "A_", # add some characters to ensure html links will work
      c_Heading,
      "\"></a>",
      if(create_top_link) c_top_link
    )
    c_toc <- paste0("[", c_Heading, "](#A_", c_Heading, ")")
  }
  
  # format toc according to found heading structure
  c_toc <- paste0(c_add_structure, c_toc)
  
  #########################################################################
  # Enhance headings
  df_data_ <- dplyr::left_join(df_data[, 1:4],
                               data.frame(index = rownames(m__) |> as.integer(),
                                          c_anchor),
                               by = "index")
  
  df_data_$c_Rmd_ <-  ifelse(!is.na(df_data_$c_anchor), df_data_$c_anchor, c_Rmd)
  
  
  
  #########################################################################
  # create TOC
  highest_order_jj <- ifelse(set_first_heading_level, 1, highest_order_jj)
  c_toc_link <- switch(highest_order_jj,
                       paste0(c_names[1]," ",toc_heading_string),
                       paste0(c_names[2]," ",toc_heading_string),
                       paste0(c_names[3]," ",toc_heading_string),
                       paste0(c_names[4]," ",toc_heading_string),
                       paste0(c_names[5]," ",toc_heading_string),
                       paste0(c_names[6]," ",toc_heading_string)
  )
  
  c_toc_link <- ifelse(create_top_link,
                       paste0(c_toc_link, "<a name=\"", toc_heading_string, "\"></a>"),
                       c_toc_link)
  
  #########################################################################
  # find position to insert table of contents
  check <- stringr::str_detect(c_Rmd, "---")
  c_start <- 1
  cnt <- 0
  
  for (ii in 1:length(c_Rmd)) {
    if (check[ii]) {
      c_start <- ii
      cnt <- cnt + 1
      if(cnt == 2) break
    }
  }
  
  #########################################################################
  # Insert table of contents
  c_Rmd <- c(df_data_$c_Rmd_ [1:(c_start)],
             c_toc_link,
             c_toc,
             "\n",
             df_data_$c_Rmd_[(c_start+1):nrow(df_data)]
  )
  
  #########################################################################
  # Insert page breaks
  
  #create data frame to work with
  df_data <- create_df(c_Rmd)
  
  # Headings
  m <- df_data[df_data$is.heading, 5:ncol(df_data)]
  
  # Analyze heading structure
  heading_struct <- m|>
    apply(2, function(x) {
      sum(x)>0
    })
  
  # highest order heading column index
  for (ii in 1:ncol(m)) {
    if(heading_struct[ii]) {
      highest_order_jj <- ii
      break
    }
  }
  
  if(highest_order_jj > 1 & pagebreak_level != "non") pagebreak_level <- (pagebreak_level|>as.integer() +  highest_order_jj - 1)|>as.character()
  
  m_pb <- switch (
    pagebreak_level,
    "non" = FALSE,
    "1" = m[, 1:1]|>matrix(dimnames =list(row.names(m),"#"))|>as.data.frame(),
    "2" = m[, 1:2],
    "3" = m[, 1:3],
    "4" = m[, 1:4],
    "5" = m[, 1:5],
    "6" = m[, 1:6],
  )
  
  # add html page break tag
  if(is.data.frame(m_pb)) {
    for (ii in 2:nrow(m_pb)) {
      for (jj in 1:ncol(m_pb)) {
        if (m_pb[ii, jj] > 0) {
          index <- row.names(m_pb)[ii] |> as.integer()
          c_Rmd[index] <-
            paste0("\n",
                   "\\newpage",
                   # "<div style=\"page-break-after: always\"></div>",
                   "\n",
                   c_Rmd[index])
        }
      }
    }
  }
  
  return(c_Rmd)
}



r_is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}

#######################################################################################
# 
#######################################################################################
r_signif <- function (x, significant_digits = 3)
{
  format(x, format = "g", digits = significant_digits)
}

if(!r_is.defined(c_MWST)){
  c_MWST <- 8.1
}

if(!r_is.defined(df_P_kat_verechnen)){
  df_P_kat_verechnen <- tibble(Kinoförderer = c("Kinoförderer"), Verkaufspreis =  c(13))
  
}

c_openfiles <- list.files(paste0("Input/"),"~")
if(length(c_openfiles) > 0) stop(paste0("\nFile: ", c_openfiles ," ist geöffnet und muss geschlossen werden!"))
remove(c_openfiles)

########################################################################
# Einnahmen und Ausgangen einlesen aus Excel 
########################################################################

c_file <- "Einnahmen und Ausgaben.xlsx"
c_sheets <- readxl::excel_sheets(paste0("Input/",c_file))

Einnahmen_und_Ausgaben <- lapply(c_sheets, function(x) {
  readxl::read_excel(paste0("Input/", c_file),
                     sheet = x)
})
names(Einnahmen_und_Ausgaben) <- c_sheets
Einnahmen_und_Ausgaben


Einnahmen_und_Ausgaben[["Ausgaben"]] <- Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  mutate(Spieldatum = as.Date(Spieldatum),
         Datum = as.Date(Datum))

Einnahmen_und_Ausgaben[["Einnahmen"]] <- Einnahmen_und_Ausgaben[["Einnahmen"]]|>
  mutate(Datum = as.Date(Datum))

# Datachecking 
df_temp <- Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  filter(Kategorie %in% c("Event","Verleiher"))|>
  mutate(error = is.na(Spieldatum))|>
  filter(error)

if(nrow(df_temp)>0) { 
  for (ii in 1:nrow(df_temp)) {
    warning((paste("\nFür die Kategorie \"Event\" oder \"Verleiher\" muss in der Datei \"Einnahmen und Ausgaben.xlsx\" ein Eventdatum definiert werden.",
                   "\n\nKategorie\tSpieldatum\tBezeichnung\n",df_temp$Kategorie[ii],"\t\t", df_temp$Spieldatum[ii], "\t\t", df_temp$Bezeichnung[ii])))
  }
}

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
  filter(Kategorie == "Event")


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

ii <- 1

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


######################################################################## 
# Extrakt Überschuss / Manko
######################################################################## 
df_manko_uerberschuss <- l_extracted |>
  lapply(function(x) {
    x[[2]]
  })|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum))|>
  arrange(Datum)
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
  select(-Artikelname, -Verkaufspreis, -`Anzahl verkaufter Artikel`)|>
  arrange(Datum)

df_Kiosk

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
if(length(list.files(pattern = "error.log"))>0) stop("\nEs sind nicht alle shows vorhanden: \nDatei .../Kinoklub/input/advance tickets/Shows.txt nicht gefunden. \nBitte herunterladen und abspeichern.")

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
  filter(!is.na(`Suisa Nummer`))|>
  mutate(Datum = as.POSIXct(Datum),
         Anfang = paste(as.character(Datum), as.character(Anfang))|>as.POSIXct(),
         Ende = paste(as.character(Datum), as.character(Ende))|>as.POSIXct()
  )

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
    " gibt es keinen Eintrag in der Datei .../Kinoklub/Input/advance tickets/show.txt\nBitte herunterladen und abspeichern")
  )}


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
              "wurde werder kein Abzug definiert.",
              "\nBitte korrigieren im File:",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx korrigieren.")
  )
}

# kein minimal Abzug definiert (Es muss kein minimaler Abzug definiert werden fall ein Fixerabzug definiert wurde)
df_temp <- df_Eintritt|>
  filter(is.na(`Minimal Abzug`) & !is.na(`Abzug [%]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0) stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                                "\nwurde werder kein Minimal Abzug definiert.",
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
  filter(!is.na(`Minimal Abzug`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde ein minimal Abzug und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
              "\nBitte korrigieren im File",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx")
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
  warning(paste0("\nAchtung für die diesen Film ", df_keine_Rechnnung$Filmtitel," am ",
                 day(df_keine_Rechnnung$Datum),".",month(df_keine_Rechnnung$Datum),".", lubridate::year(df_keine_Rechnnung$Datum)," gibt es keine Verleiherrechnung.",
                 "\nBitte korrigieren in der Datei:",
                 "\n.../Kinokulb/input/Einnahmen und Ausgaben.xlsx\n"
  )
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



c_Date[3]

l_GV <- list()
l_Abgaben <- list()
ii <- 1
for (ii in 1:length(c_Date)) {
  
  ######################################################################
  # Kinoförderer dürfen nicht bei jedem Verleiher als gratis abgerechnet werden und müssen anders behandelt werden. 
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
          filter(Datum == c_Date[ii], Platzkategorie %in% df_P_kat_verechnen$Kinoförderer )|>
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
  
  df_Eintritt|> filter(Datum == c_Date[ii])
  
  # minimale Abgaben an den Verleiher
  c_Verleiher_garantie <- df_verleiherabgaben |>
    filter(Datum == c_Date[ii])|>
    select(`Minimal Abzug`)|>
    pull()
  c_Verleiher_garantie
  
  if(length(c_Verleiher_garantie) > 1) {
    print(df_verleiherabgaben |>
            filter(Datum == c_Date[ii]))
    stop("In der Datei .../input/Verleiherabgaben.xlsx gibt es mehrere Filme am selben Datum")}
  
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
                   "\nVerleiherabgabe im file .../Kinoklub/input/Verleiherabgaben.xlsx definiert.")
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

#############################################################
# error handling 
# Verleiherrechnung ist kleiner als der Vereinbarte minimale Abgabebetrag
df_Verleiher_Rechnnung

df_temp <- df_GV_Eintritt|>
  filter(df_GV_Eintritt$`Sonstige Kosten [CHF]` < 0)|>
  left_join(df_verleiherabgaben, by = join_by(Datum))|>
  select(Datum, `Suisa Nummer`, Filmtitel, `Minimal Abzug`)|>
  filter(!is.na(`Minimal Abzug`))
df_temp

if(nrow(df_temp) > 0 ) {
  warning(
    paste0(paste0("\nDer Verleiherrechnungsbetrag ist kleiner als die minimal vereibarte mindest Garantie: ", 
                  day(df_temp$Datum),".", month(df_temp$Datum),".",year(df_temp$Datum),".", " ", df_temp$Filmtitel
    ),
    "\nBitte Abklären ob die Verleiherrechnung oder die mindest Garantie korrekt ist:\nBitte die Datei .../Input/Verleiherabgaben.xlsx oder die Datei .../Input/Einnahmen und Ausgaben.xlsx korrigieren."
    )
  )
}

###
df_Abgaben <- l_Abgaben|>
  bind_rows()|>
  bind_rows(df_Eintritt|> # 
              filter(!Zahlend, !(Platzkategorie %in% df_P_kat_verechnen$Kinoförderer )))|>
  mutate(Verkaufspreis = if_else(Platzkategorie == df_P_kat_verechnen$Kinoförderer, df_P_kat_verechnen$Verkaufspreis, Verkaufspreis)
  )
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
    filter(Datum == c_Date[ii], Kategorie == Einnahmen_und_Ausgaben$dropdown$`drop down`[1])
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
    `Gewinn/Verlust [CHF]` =round5Rappen(
      l_GV_Kiosk[[ii]]$Gewinn + l_GV[[ii]]$`Gewinn/Verlust [CHF]` + pull(df_manko_uerberschuss|>filter(Datum == c_Date[ii])) - c_Eventausgaben + c_Eventeinnahmen
    )
  )
}

df_GV_Vorfuehrung <- l_GV_Vorfuehrung |>
  bind_rows() 

########################################################################
# remove 
########################################################################
remove(l_Eintritt,  m, c_raw, l_GV, l_GV_Kiosk, c_Besucher,  df_Eventausgaben, l_Abgaben,
       c_suisaabzug, c_Gratis, c_Umsatz, l_GV_Vorfuehrung,ii, c_Eventausgaben,df_P_kat_verechnen, c_lenght, c_Brutto,
       convert_data_Film_txt, c_file, c_Verleiherrechnung, c_sheets, c_Kinofoerder_gratis, c_MWST_Abzug, c_Netto3, 
       c_Verleiher_garantie, c_Verleiherabzug,n_Film,n_kiosk,
       c_verleiherabzug_prozent)

remove(df_Eventeinnahmen, df_temp, df_keine_Rechnnung, df_Abgaben, df_spez_preis_na, df_s_Eintritt,
       Einnahmen_und_Ausgaben, l_Abos, l_raw)


########################################################################
# user interaction
########################################################################
writeLines("Datenkonvertierung erfolgt")