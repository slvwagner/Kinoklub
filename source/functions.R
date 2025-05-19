library(httr)
library(rvest)
library(tidyverse)
library(purrr)

# spez. Round for Swiss currency "CHF"
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
    unlist()|>
    round(2)
}

# variable is present in global environment ####
r_is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}

# library is loaded in global environment ####
r_is.library_loaded <- function(package_name) {
  is_loaded <- FALSE
  tryCatch({
    is_loaded <- requireNamespace(package_name, quietly = TRUE)
  }, error = function(e) {
    is_loaded <- FALSE
  })
  return(is_loaded)
}

# clean console ####
if (commandArgs()[1]=='RStudio'){
  print.cleanup <- function(cleanupObject) cat("\f")     
} else if(substr(commandArgs()[1], nchar(commandArgs()[1]), nchar(commandArgs()[1])) == "R"){        
  print.cleanup <- function(cleanupObject) cat(c("\033[2J","\033[H"))
} else {
  print(paste0("not support: ",commandArgs()[1]))
}                                                                         
clc <- 0                                        ##  variable from class numeric
class(clc) <- 'cleanup'                         ##  class cleanup
#print(clc)                                      ##  when you load this source,
##  it cleans all console

# Inhaltsverzeichnis für Markdown ####
r_toc_for_Rmd <- function(
    c_Rmd,
    toc_heading_string = "Table of Contents" ,
    create_nb = TRUE, create_top_link = TRUE , nb_front = TRUE, set_first_heading_level = FALSE,
    pagebreak_level = "non"
)
{
  create_df <- function(c_Rmd) {
    p <- "^```"
    df_data <- data.frame(
      index = 1:length(c_Rmd),
      c_Rmd,
      code_sections = lapply(c_Rmd, function(x)
        stringr::str_detect(x, p)) |> unlist(),
      is.heading = stringr::str_detect(c_Rmd, "^[#]+\\s")
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
  # create data frame to work with
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
  
  # create structure number
  c_nb <- m__ |>
    apply(1, function(x) {
      temp <- x[x > 0]
      paste0(temp, collapse = ".")
    })
  
  # create link link to table of contents
  c_top_link <-  paste0("\n[", toc_heading_string, "](#", toc_heading_string, ")\n")
  c_top_link
  
  c_Heading <- c_Rmd[df_data$is.heading]|>stringr::str_remove_all("#")|>stringr::str_trim()
  c_Heading
  
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
  
  # Enhance headings
  df_data_ <- dplyr::left_join(df_data[, 1:4],
                               data.frame(index = rownames(m__) |> as.integer(),
                                          c_anchor),
                               by = "index")
  
  df_data_$c_Rmd_ <-  ifelse(!is.na(df_data_$c_anchor), df_data_$c_anchor, c_Rmd)
  
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
  
  # Insert table of contents
  c_Rmd <- c(df_data_$c_Rmd_ [1:(c_start)],
             c_toc_link,
             c_toc,
             "\n",
             df_data_$c_Rmd_[(c_start+1):nrow(df_data)]
  )
  
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

# Variel ist definiert? ####
r_is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}

# signif but return sting 
r_signif <- function (x, significant_digits = 3){
  format(x, format = "g", digits = significant_digits)
}

# inspect links ####
inspect_link <- function(df_mapping, ID){
  link <- df_mapping|>
    filter(`Event ID` == ID)|>
    pull()
  if(length(link) > 0) return(link)
  else return(NULL)
}

inspect_link_ids <- function(df_mapping) {
  result <- vector("list", nrow(df_mapping))  # Initialize an empty list
  names(result) <- df_mapping$`Event ID`  # Set names to ID_Programm
  
  for (ii in seq_len(nrow(df_mapping))) {
    link_ids <- df_mapping[ii,"Event ID"]|>pull()  # Store all consecutive Link IDs for this row
    link <- df_mapping[ii,"Link to Event ID"]|>pull()
    run <- TRUE
    if (!is.na(link)){
      link_ids <- c(link_ids, link)
      while (run) {
        link <- inspect_link(df_mapping, link)
        if (!is.na(link)){
          link_ids <- c(link_ids, link)
        }else {
          run <- FALSE
        }
      }
    }
    result[[ii]] <- link_ids
  }
  return(result)
}

# Collect all values that appear in chains (excluding the first element of each list) ####
nullify_used_entries <- function(lst) {
 
  used_values <- unlist(lapply(lst, function(x) x[-1])) 
  
  # Convert list names to numeric for comparison
  used_names <- as.numeric(names(lst))
  
  # Nullify entries whose names are used in another chain
  lst[used_names %in% used_values] <- NULL
  
  return(lst)
}

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
      
      # Error handling: Suisa from Eintritt vs Suisa from Programm
      df_temp <- Programm|>
        filter(`Event ID` == ID)
      df_temp$Suisanummer
      
      if(length(df_temp$Suisanummer) == 0){
        stop("\nEs gibt keinen Programmeintrag für: .../Kinoklub/", fileName,
             "\nBitte im Program einen Eintrag erstellen für Programm ID", ID,"\n" )
      }
      
      if(c_temp[1] != df_temp$Suisanummer) {
        warning("\nIn der Datei: .../Kinoklub/", fileName,
                "\nwurde die Suisanummer ",c_temp[1]," gefunden.",
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
  
  # create data frame
  df_Eintritt <- l_Eintritt |>
    bind_rows() |>
    mutate(Verkaufspreis = Preis ,
           Zahlend = if_else(Verkaufspreis == 0, F, T)) |>
    select(
      Datum,
      Suisanummer,
      Filmtitel,
      Platzkategorie,
      Zahlend,
      Verkaufspreis,
      Anzahl,
      Umsatz,
      `SUISA-Vorabzug`
    )
  df_Eintritt
  
  # join `Event ID`
  df_Eintritt <- df_Eintritt |>
    left_join(
      Programm |>
        filter(`Verleiher Angefragt?` != "Wird nicht gespielt") |>
        select(`Event ID`, Datum, Suisanummer),
      by = join_by(Datum, Suisanummer)
    ) |>
    rename(`Umsatz [CHF]` = Umsatz)
  
  df_Eintritt <- df_Eintritt |>
    select(
      `Event ID`,
      `Datum`,
      `Suisanummer`,
      `Filmtitel`,
      `Platzkategorie`,
      `Zahlend`,
      `Verkaufspreis`,
      `Anzahl`,
      `Umsatz [CHF]`,
      `SUISA-Vorabzug`
    )
  
  if (sum(is.na(df_Eintritt$`Event ID`)) > 0) {
    df_temp <- df_Eintritt |>
      filter(is.na(`Event ID`)) |>
      distinct(Datum, Suisanummer, .keep_all = TRUE)
    stop(
      "\nFür den Film ",
      df_temp$Filmtitel,
      " mit Suisanummer ",
      df_temp$Suisanummer,
      " am ",
      paste0(format(df_temp$Datum, "%d.%m.%Y"), collapse = ", "),
      " existiert kein Programmeintrag\nBitte das Programm korrigieren!\n"
    )
  }
  
  return(df_Eintritt)
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
      p <- or("\\b\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}\\b", # format 01.01.2025
              "\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b" # # format 01/01/2025
      )
      
      index <- c_raw|>
        str_detect(p)
      index
      
      if(sum(index) == 0) {
        stop("\nIn der Datei: ",fileName, " kann kein Datum gefunden werden.","\nBitte der Datei ein korrektes Datum hinzufügen")
      }
      
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

# search procinem by a given Suisanummber
search_procinema_by_suisa <- function(suisa_number) {
  # Create the form POST request
  response <- POST(
    "https://www.procinema.ch/de/statistics/filmdb/",
    body = list(
      sta_fdb_movid = suisa_number,
      sta_fdb_search = "Suchen",  # The search button value
      process = "Filter"          # The submit action
    ),
    encode = "form"
  )
  
  # Check if successful
  if(status_code(response) != 200) {
    message("Request failed with status: ", status_code(response))
    return(tibble())
  }
  
  # Parse the HTML content
  html_content <- content(response, as = "text") %>% 
    read_html()
  
  # Check if results were found
  results_header <- html_content %>% 
    html_node("h3") %>% 
    html_text(trim = TRUE)
  
  if(is.na(results_header) || !str_detect(results_header, "Suchresultate")) {
    message("No results found for SUISA number: ", suisa_number)
    return(tibble())
  }
  
  # Extract film information
  film_nodes <- html_content %>% html_nodes(".listline")
  
  if(length(film_nodes) == 0) {
    message("No film nodes found in the results")
    return(tibble())
  }
  
  # Process each film
  results <- map_df(film_nodes, function(node) {
    tibble(
      Filmtitel = node %>% html_node(".fl a") %>% html_text(trim = TRUE),
      link = node %>% html_node(".fl a") %>% html_attr("href") %>% 
        paste0("https://www.procinema.ch", .),
      Verleiher = node %>% html_node(".fc") %>% html_text(trim = TRUE) %>% 
        str_replace_all("\\s+", " ") %>% str_trim(),
      Suisanummer = node %>% html_node(".fdbsuisa") %>% html_text(trim = TRUE),
      release_date = node %>% html_node(".fdbrelease") %>% html_text(trim = TRUE),
      admissions = node %>% html_node(".fdbadm") %>% html_text(trim = TRUE) %>% 
        str_remove_all("'") %>% as.integer()
    )
  })
  
  return(results)
}

# # Example usage
# suisa_number <- "1020.295"  # Example SUISA number
# results <- search_procinema_by_suisa(suisa_number)
# print(results)

# get detailed information for a film ####
film_details <- function(url) {
  if(is_empty(url)) {
    return(NULL)
  }
  suppressPackageStartupMessages({
    require(rvest)
    require(dplyr)
    require(stringr)
    require(purrr)
    require(tibble)
    require(httr)
  })
  
  # Helper function to extract values from faditem blocks
  extract_faditem <- function(page, label) {
    items <- page %>% html_nodes(".faditem")
    for (item in items) {
      lbl <- item %>% html_node(".faditemlbl") %>% html_text(trim = TRUE)
      if (!is.na(lbl) && str_detect(lbl, label)) {
        return(item %>% html_node(".faditemcont") %>% html_text(trim = TRUE))
      }
    }
    return(NA_character_)
  }
  
  # Helper function to clean numbers
  clean_number <- function(x) {
    if (is.na(x) || x == "") return(NA_integer_)
    as.integer(str_remove_all(x, "[^0-9]"))
  }
  
  # Helper function to clean dates
  clean_date <- function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    x
  }
  
  # Fetch the page
  page <- tryCatch({
    resp <- GET(url, timeout(2))
    if (http_error(resp)) stop("HTTP error")
    read_html(resp)
  }, error = function(e) {
    message(str_glue("Error loading {url}: {e$message}"))
    return(NULL)
  })
  
  if (is.null(page)) return(tibble())
  
  # Extract all information
  tibble(
    # Basic info
    title = page %>% html_node("h1") %>% html_text(trim = TRUE) %||% NA_character_,
    link = url,
    
    # Titles
    original_title = extract_faditem(page, "Original"),
    german_title = extract_faditem(page, "Deutsch"),
    french_title = extract_faditem(page, "Französisch"),
    italian_title = extract_faditem(page, "Italienisch"),
    
    # Release dates
    release_ch = clean_date(extract_faditem(page, "Schweiz")),
    release_dch = clean_date(extract_faditem(page, "Deutschschweiz")),
    release_fch = clean_date(extract_faditem(page, "Suisse romande")),
    release_ich = clean_date(extract_faditem(page, "Tessin")),
    
    # Admissions
    admissions_ch = clean_number(extract_faditem(page, "Schweiz$")),
    admissions_dch = clean_number(extract_faditem(page, "Deutschschweiz$")),
    admissions_fch = clean_number(extract_faditem(page, "Suisse romande$")),
    admissions_ich = clean_number(extract_faditem(page, "Tessin$")),
    
    # Age ratings
    age_approved = clean_number(extract_faditem(page, "Zugelassen ab")),
    age_recommended = clean_number(extract_faditem(page, "Empfohlen ab")),
    age_ti = clean_number(extract_faditem(page, "Kanton TI")),
    
    # Synopsis
    synopsis = page %>% 
      html_node("h3:contains('INHALT') + p") %>% 
      html_text(trim = TRUE) %||% NA_character_,
    
    # Images (comma-separated URLs)
    images = page %>% 
      html_nodes(".scenimg") %>% 
      html_attr("src") %>% 
      {if (length(.) > 0) str_c("https://www.procinema.ch", .) else NA_character_} %>% 
      str_c(collapse = ", "),
    
    # Crew
    director = extract_faditem(page, "Regie") %>% str_replace_all("<br>", ", "),
    producer = extract_faditem(page, "Produzent"),
    writer = extract_faditem(page, "Drehbuch") %>% str_replace_all("<br>", ", "),
    music = extract_faditem(page, "Musik"),
    actors = extract_faditem(page, "Schauspieler")
  )
}

# # Example usage
# result <- film_details("https://www.procinema.ch/de/statistics/filmdb/1020295.html")
# print(result$synopsis)
# print(result)


# create empty line with correct data type ####

create_empty_line <- function(df_data) {
  df_data|>
  slice(0) |>
    add_row() |>
    mutate(across(everything(), ~ {
      if (is.character(.)) {
        ""
      } else if (is.integer(.)) {
        0L
      } else if (is.numeric(.)) {
        0
      } else if (is.factor(.)) {
        factor(NA, levels = levels(.))
      } else if (inherits(., "Date")) {
        as.Date(NA)
      } else {
        NA
      }
    }))
}

###################################################################
#' Initialize fast dictionary environment
#' The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE


dict_init <- function(length)
{
  new.env(hash = TRUE, parent = emptyenv(), size = length)
}

###################################################################
#' Assigne key and value to fast dictionary
#' The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE


dict_assign_key_values <- Vectorize(assign, vectorize.args = c("x", "value"))


###################################################################
#' The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE


dict_get_values <- Vectorize(get, vectorize.args = "x")

###################################################################
#' Check if key is in dictionary
#' The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE


dict_exists_key <- Vectorize(exists, vectorize.args = "x")


###################################################################
#' Create a fast dictionary from data frame
#' The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE


dict_from_data.frame <- function(df)
{
  df <- as.data.frame(df)
  if(!is.character(df[1,1])){
    c("dict_from_data.frame:\nkey is not character in column 1 of the dataframe argument")|>
      r_colourise("Red")|>
      writeLines()
    return(NULL)
  }else
  {
    # initialize hash
    hash = new.env(hash = TRUE, parent = emptyenv(), size = nrow(df))
    # assign values to keys
    dict_assign_key_values(df[,1], df[,2], hash)
    return(hash)
  }
}


###################################################################
#' Update key/value pairs of fast dictionary
#'  The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE


dict_update <- function(df, dict)
{
  df <- as.data.frame(df)
  if(nrow(df) == 1){
    dict[[df[1,1]]] <- df[1,2]
  }else{
    for (ii in 1:nrow(df)) {
      dict[[df[ii,1]]] <- df[ii,2]
    }
  }
  return(dict)
}








