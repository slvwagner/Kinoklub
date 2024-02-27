#############################################################################################################################################
# Bitte beachte das README.md und die Dokumentation im Verzeichniss ".../doc"
# Diesen Script erstellt alle Berichte für den Kinoclub.
# 
# Autor: Florian Wagner
# florian.wagner@wagnius.ch
# V0.6

#############################################################################################################################################
rm(list = ls())
source("source/functions.R")
library(tidyverse)
library(rebus)
c_WD <- getwd()
#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################
# Sollen für jede Vorführung eine Abrechnung erstellt werden?
c_run_single <- TRUE

# Sollen Inhaltsverzeichnisse erstellt werden
toc <- TRUE

# Mehrwertsteuersatz
c_MWST <- 8.1 #%

# Platzkategorien die für gewisse Verleiherabgerechnet werden müssen
df_P_kat_verechnen <- tibble(Kinoförderer = "Kinoförderer", Verkaufspreis =  13)

# Ausgabeformate
# 1 = only html
# 2 = only docx
# 3 = only pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
# 4 = html and docx
# 5 = html and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
# 6 = docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
# 7 = html, docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
c_render_option <- "1" 

# create Site Map 
c_SiteMap <- TRUE

#############################################################################################################################################
# Script start
#############################################################################################################################################

# Vorlage für Diagramme (Bei einer Änderung soll auch das css (".../source/Kinokulub_dark.css") geändert werden)
my_template <-
  theme_bw() +
  theme(
    panel.background = element_rect(
      fill = "#322f3b",
      colour = "#322f3b",
      linewidth = 0.5,
      linetype = "solid"
    ),
    plot.background = element_rect(fill = "#322f3b"),
    axis.title = element_text(colour = "#f4cccc", size  = 15),
    axis.text = element_text(colour = "#f4cccc"),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "#322f3b", color = "black"),
    legend.text = element_text(color = "#f4cccc"),
    legend.title = element_text(size = 12),
    title = element_text(color = "#f4cccc", size  = 22)
  )


# Ausgabeformat(e)
df_Render <- switch (
  c_render_option,
  "1" = tibble::tibble(Render  = c("html_document"),
                       fileExt = c(".html")),
  "2" = tibble::tibble(Render  = c("word_document"),
                       fileExt = c(".docx")),
  "3" = tibble::tibble(Render  = c("pdf_document"),
                       fileExt = c(".pdf")),
  "4" = tibble::tibble(Render  = c("html_document","word_document"),
                       fileExt = c(".html", ".docx")),
  "5" = tibble::tibble(Render  = c("html_document","pdf_document"),
                       fileExt = c(".html", ".pdf")),
  "6" = tibble::tibble(Render  = c("word_document","pdf_document"),
                       fileExt = c(".docx", ".pdf")),
  "7" = tibble::tibble(Render  = c("html_document","word_document","pdf_document"),
                       fileExt = c(".html", ".docx", ".pdf")),
  stop("\nDie verwendete Renderoption is nicht definiert")
)

# löschen aller files im output folder
c_path <- "output"

if(dir.exists(c_path)){
  c_files <- paste0(c_path,"/",list.files(c_path))
  c_files
  file.remove(c_files)|>suppressWarnings()
  # if(dir.exists(paste0(c_path,"/pict"))) {
  #   # Use unlink() to remove the directory
  #   unlink(paste0(c_path,"/pict/"), recursive = TRUE)
  # }
}

# Daten einlesen und konvertieren
source("source/read and convert.R")

#############################################################################################################################################
# index pro suisa_nr und Datume erstellen
#############################################################################################################################################
df_mapping <- tibble(Datum = c_Date)|>
  mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
         index = row_number())

df_mapping

#############################################################################################################################################
# Jahresrechnung detalliert
#############################################################################################################################################
# Einlesen
c_raw <- readLines("source/Jahresrechnung_detailed.Rmd")
c_raw

# Inhaltsverzeichnis
if(toc){# neues file schreiben mit toc
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines(paste0("source/temp.Rmd"))
}else {# neues file schreiben ohne toc
  c_raw|>
    writeLines(paste0("source/temp.Rmd"))
}

# Render
rmarkdown::render(paste0("source/temp.Rmd"),
                  df_Render$Render,
                  output_dir = paste0(getwd(), "/output"))

# Rename the file
for (jj in 1:length(df_Render$Render)) {
  file.rename(from = paste0(getwd(),"/output/temp",df_Render$fileExt[jj]),
              to   = paste0(getwd(),"/output/", "Jahresrechnung_detailed",df_Render$fileExt[jj] )
  )
}

print(clc)

paste("Bericht: \nJahresrechnung detailliert erstellt")|>
  writeLines()

#############################################################################################################################################
# Jahresrechnung
#############################################################################################################################################
# Einlesen
c_raw <- readLines("source/Jahresrechnung.Rmd")
c_raw

# Inhaltsverzeichnis
if(toc){# neues file schreiben mit toc
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines(paste0("source/temp.Rmd"))
}else {# neues file schreiben ohne toc
  c_raw|>
    writeLines(paste0("source/temp.Rmd"))
}

# Render
rmarkdown::render(paste0("source/temp.Rmd"),
                  df_Render$Render,
                  output_dir = paste0(getwd(), "/output"))

# Rename the file
for (jj in 1:length(df_Render$Render)) {
  file.rename(from = paste0(getwd(),"/output/temp",df_Render$fileExt[jj]),
              to   = paste0(getwd(),"/output/", "Jahresrechnung",df_Render$fileExt[jj] )
  )
}

print(clc)

paste("Bericht: \nJahresrechnung erstellt")|>
  writeLines()


#############################################################################################################################################
# Statistik
#############################################################################################################################################
# Einlesen
c_raw <- readLines("source/Statistik.Rmd")
c_raw

# Inhaltsverzeichnis
if(toc){# neues file schreiben mit toc
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines(paste0("source/temp.Rmd"))
}else {# neues file schreiben ohne toc
  c_raw|>
    writeLines(paste0("source/temp.Rmd"))
}

# Render
rmarkdown::render(paste0("source/temp.Rmd"),
                  df_Render$Render,
                  output_dir = paste0(getwd(), "/output"))

# Rename the file
for (jj in 1:length(df_Render$Render)) {
  file.rename(from = paste0(getwd(),"/output/temp",df_Render$fileExt[jj]),
              to   = paste0(getwd(),"/output/", "Statistik",df_Render$fileExt[jj] )
  )
}

# rmarkdown::render(paste0("source/Statistik.Rmd"),
#                   df_Render$Render,
#                   output_dir = paste0(getwd(), "/output"))
print(clc)

paste("Bericht: \nStatistik erstellt")|>
  writeLines()


#############################################################################################################################################
# Erstellen der Berichte Vorführungen
#############################################################################################################################################
ii <- 1
if(c_run_single){
  for(ii in 1:nrow(df_mapping)){

    ############################################################################################
    # Einlesen template der Abrechnung
    c_raw <- readLines("source/Abrechnung.Rmd")
    c_raw

    # Ändern des Templates mit user eingaben (ii <- ??) verwendet für Datum
    index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
    index
    c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(ii))

    # writeLines(c_raw, paste0("source/temp.Rmd"))

    # Ändern des Templates Titel Filmname
    index <- (1:length(c_raw))[c_raw|>str_detect("Abrechnung Filmvorführung")]
    c_temp1 <- df_GV_Vorfuehrung|>
      filter(Datum == df_GV_Vorfuehrung$Datum[ii])|>
      mutate(Anfang = paste0(lubridate::hour(Anfang),":", lubridate::minute(Anfang)|>as.character()|>formatC(format = "0", width = 2)|>str_replace(SPC,"0")),
             Datum = paste0(day(Datum),".",month(Datum),".",year(Datum))
      )|>
      rename(`Total Gewinn [CHF]`=`Gewinn/Verlust [CHF]`)|>
      select(Filmtitel)|>
      pull()

    c_temp <- c_raw[(index)]|>
      str_split("\"", simplify = T)|>
      as.vector()

    c_temp <- c_temp[1:2]
    c_temp <- paste0(c(c_temp), collapse = "\"")
    c_temp <- paste0(c(c_temp, " "), collapse = "")
    c_temp <- paste0(c(c_temp, c_temp1), collapse = "")
    c_raw[(index)] <- paste0(c(c_temp, "\""), collapse = "")

    # Inhaltsverzeichnis
    if(toc){# neues file schreiben mit toc
      c_raw|>
        r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
        writeLines(paste0("source/temp.Rmd"))
    }else {# neues file schreiben ohne toc
        c_raw|>
          writeLines(paste0("source/temp.Rmd"))
      }

    # Render
    rmarkdown::render(paste0("source/temp.Rmd"),
                      df_Render$Render,
                      output_dir = paste0(getwd(), "/output"))

    # Rename the file
    for (jj in 1:length(df_Render$Render)) {
      file.rename(from = paste0(getwd(),"/output/temp",df_Render$fileExt[jj]),
                  to   = paste0(getwd(),"/output/", "Abrechnung Filmvorführung ",df_mapping$user_Datum[ii],df_Render$fileExt[jj])
                  )
    }

    # user interaction
    print(clc)
    paste("Bericht: \nFilmabrechnung vom", df_mapping$user_Datum[ii], "erstellt")|>
      writeLines()

    ############################################################################################
    # Einlesen template der Verleiherabrechnung
    c_raw <- readLines("source/Verleiherabrechnung.Rmd")
    c_raw

    # Ändern des Templates mit user eingaben (ii <- ??) verwendet für Datum
    index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
    index
    c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(ii))

    # neues file schreiben
    writeLines(c_raw, "Verleiherabrechnung.Rmd")

    # Render
    rmarkdown::render(input = "Verleiherabrechnung.Rmd",
                      output_file = paste0("Verleiherabrechnung ", df_mapping$user_Datum[ii], df_Render$fileExt[jj]),
                      output_format = df_Render$Render,
                      output_dir = paste0(getwd(), "/output"))


    # user interaction
    print(clc)
    paste("Bericht: \nVerleiherabrechnung vom", df_mapping$user_Datum[ii], "erstellt")|>
      writeLines()

  }
  # remove file
  file.remove("Verleiherabrechnung.Rmd")
  remove(c_raw, index,ii,jj)
}

print(clc)

paste("Bericht: \nAbrechnungen für Filmvorführungen wurden erstellt.")|>
  writeLines()

#############################################################################################################################################
# create site map
#############################################################################################################################################

if(c_SiteMap){
  c_fileNames <-list.files(path = paste0(c_WD,"/output/"),
                           pattern = ".html")
  
  # Was für Berichte typen sind vorhanden
  c_typ_Berichte <- c_fileNames|>
    str_extract(START%R%one_or_more(WRD))|>
    factor()|>
    levels()
  c_typ_Berichte
  
  # Convert filenames to URL
  c_url <- paste0("file:///", URLencode(paste0(c_WD,"/output/", c_fileNames)), 
                sep = "")
  
  c_path <- paste0(c_WD,"/output/pict")
  c_path
  dir.create(c_path)|>suppressWarnings()
  
  library(magick)
  
  writeLines("Site-Map wird erstellt, einen Moment bitte: ")
  ii <- 1
  if(!length(list.files("output/", "html")) == length(list.files("output/pict/"))){
    for (ii in 1:length(c_fileNames)) {
      # Set the path to the input image
      input_path <- paste0(c_path, "/",c_fileNames[ii],".png")
  
      # create a webshot, printed html
      webshot::webshot(url = c_url[ii], file = input_path)
  
      # Read the image crop and resize and save
      image_read(input_path)|>
        image_crop(geometry = "992x992+0+0")|>
        image_resize("500x500")|>
        image_write(input_path)
  
      writeLines(".", sep = "")
    }
  }

  # Einlesen template der Verleiherabrechnung
  c_raw <- readLines("source/Site_Map.Rmd")
  c_raw
  
  # function to edit raw markdown files
  edit_Rmd <- function(raw_rmd, index,fileNames, url) {
    # create link to pict and link to file 
    if(length(raw_rmd) == index){
      for (ii in 1:(length(fileNames))) { 
        if(ii == 1){ #letzte Zeile von Rmd
          raw_rmd <- c(raw_rmd[1:index],
                       paste0("[","![",fileNames[ii],"](output/pict/",fileNames[ii],".png)","](", url[ii],")","  \\\n\\")," "
                       )
        }else{ # normales einfügen
          raw_rmd <- c(raw_rmd[1:index],
                       paste0("[","![",fileNames[ii],"](output/pict/",fileNames[ii],".png)","](", url[ii],")","  \\\n\\"),
                       raw_rmd[(index+1):length(raw_rmd)]
                       )
        }
      }
    }else{ # normales einfügen 
      for (ii in 1:(length(fileNames))) {
        raw_rmd <- c(raw_rmd[1:index],
                   paste0("[","![",fileNames[ii],"](output/pict/",fileNames[ii],".png)","](", url[ii],")","  \\\n\\"),
                   raw_rmd[(index+1):length(raw_rmd)]
        )
      }
    }
    return(raw_rmd)
  }
  
  ii <- ii + 1
  for (ii in 1:length(c_typ_Berichte)) {
    # Index where to insert  
    c_index <- (1:length(c_raw))[c_raw|>str_detect(c_typ_Berichte[ii])]
    c_index <- c_index[length(c_index)]
    c_index
    
    c_raw
    c_raw[c_index]
    
    if(c_typ_Berichte[ii] == "Jahresrechnung"){
      c_select <- str_detect(c_fileNames, START%R%c_typ_Berichte[ii]%R%DOT%R%"html")
    }else{
      c_select <- str_detect(c_fileNames, START%R%c_typ_Berichte[ii])
    }
    
    # if(c_typ_Berichte[ii] == "Verleiherabrechnung") stop(paste("\nstop"))
    
    c_raw
    c_fileNames[c_select]
    c_url[c_select]
    
    c_raw <- edit_Rmd(c_raw,c_index,c_fileNames[c_select], c_url[c_select])
    c_raw
    
  }
  c_typ_Berichte[ii]
  c_raw
  
  # neues file schreiben
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines("Site-Map.Rmd")
  
  # Render
  rmarkdown::render(input = "Site-Map.Rmd")

}

file.remove("Site-Map.Rmd")

#############################################################################################################################################
# remove temp files 
list.files(pattern = "temp", recursive = TRUE)|>
  file.remove()

# remove(c_Datum, c_suisa, c_verleiherabgaben, c_run_single, c_Verleiher_garantie )
# remove(df_temp, df_Render, df_mapping, Brutto,
#        c_temp, c_temp1,
#        c_render_option)

#############################################################################################################################################
# User Interaktion
print(clc)
paste("\n********************\nDone\n********************\n")|>
  writeLines()



