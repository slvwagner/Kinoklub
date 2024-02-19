#############################################################################################################################################
# Bitte beachte das README.md
# Diesen Script erstellt die Jahresabrechnung für den Kinoclub.
# Es kann auch pro Vorführung einen Rechnung erstellt werden. 
# Autor: Florian Wagner
# florian.wagner@wagnius.ch
# V0.7.0
#############################################################################################################################################
rm(list = ls())
#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################
# Sollen für jede Vorführung eine Abrechnung erstellt werden?
c_run_single <- TRUE

#Sollen inhaltsverzeichnisse erstellt werden
toc <- TRUE

# Mehrwertsteuersatz
c_MWST <- 8.1 #%

# Ausgabeformate
# 1 = only html
# 2 = html and docx
# 3 = html, docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
c_render_option <- "1" 

# template für diagramme
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



#############################################################################################################################################
# Script start
#############################################################################################################################################

# Ausgabeformat(e)
df_Render <- switch (
  c_render_option,
  "1" = tibble::tibble(Render  = c("html_document"), 
                       fileExt = c(".html")),
  "2" = tibble::tibble(Render  = c("html_document","word_document"), 
                       fileExt = c(".html", ".docx")),
  "3" = tibble::tibble(Render  = c("html_document","word_document","pdf_document"), 
                       fileExt = c(".html", ".docx", ".pdf"))
)

# löschen aller files im output folder
c_path <- "output"

if(dir.exists(c_path)){
  c_files <- paste0(c_path,"/",list.files(c_path))
  c_files
  file.remove(c_files)|>suppressWarnings()
}

# Daten einlesen und konvertieren
source("source/read and convert.R")

#############################################################################################################################################
# index pro suisa_nr und Datume erstellen
df_mapping <- tibble(Datum = c_Date)|>
  mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
         index = row_number())

df_mapping

#############################################################################################################################################
# Jahresrechnung detalliert

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
# Erstellen der Berichte für gewählte Vorführungen
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
    writeLines(c_raw, paste0("source/temp.Rmd"))
    
    # Render
    rmarkdown::render(input = paste0("source/temp.Rmd"),
                      output_format = df_Render$Render,
                      output_dir = paste0(getwd(), "/output"))
    
    # Rename the file
    for (jj in 1:length(df_Render$Render)) {
      file.rename(from = paste0(getwd(),"/output/temp",df_Render$fileExt[jj]), 
                  to   = paste0(getwd(),"/output/", "Verleiherabrechnung ", df_mapping$user_Datum[ii],df_Render$fileExt[jj])
      )
    }
    
    # user interaction
    print(clc)
    paste("Bericht: \nVerleiherabrechnung vom", df_mapping$user_Datum[ii], "erstellt")|>
      writeLines()
    
  }
  
  remove(c_raw, index,ii,jj)
}

remove(c_Datum, c_suisa, c_verleiherabgaben, c_run_single, c_Verleiher_garantie, Einnahmen_und_Ausgaben )

#############################################################################################################################################
# remove temp files 
list.files(pattern = "temp", recursive = TRUE)|>
  file.remove()

remove(df_temp, df_Render, df_mapping, Brutto,
       c_temp, c_temp1,
       c_render_option)

#############################################################################################################################################
# User Interaktion

paste("\nAlle Berichte wurden erstellt und sind im Verzeichniss  \"output\" zu finden.\n")|>
  writeLines()



