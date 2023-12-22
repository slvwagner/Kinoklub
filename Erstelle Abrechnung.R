#############################################################################################################################################
# Bitte beachte das README.md
# Diesen Script erstellt die Jahresabrechnung für den Kinoclub.
# Es kann auch pro Vorführung einen Rechnung erstellt werden. 
#
# V0.6.0
#############################################################################################################################################
rm(list = ls())
#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################
# Sollen für jede Vorführung eine Abrechnung erstellt werden?
c_run_single <- TRUE

# Mehrwertsteuersatz
c_MWST <- 7.7 #%

# Ausgabeformat / html, word und pdf. Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex)
# df_Render <- tibble(Render  = c("html_document","word_document","pdf_document"), 
#                     fileExt = c(".html", ".docx", ".pdf"))

df_Render <- tibble::tibble(Render  = c("html_document"), 
                    fileExt = c(".html"))

#############################################################################################################################################
# Script start

# Daten einlesen und konvertieren
source("source/read and convert.R")

#############################################################################################################################################
# index pro suisa_nr und Datume erstellen
df_mapping <- tibble(Datum = c_Date)|>
  mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
         index = row_number())

#############################################################################################################################################
# Jahresrechnung detalliert
rmarkdown::render(paste0("source/Jahresrechnung_detailed.Rmd"),
                  df_Render$Render,
                  output_dir = paste0(getwd(), "/output"))
print(clc)

paste("Bericht: \nJahresrechnung detailliert erstellt")|>
  writeLines()

#############################################################################################################################################
# Jahresrechnung
rmarkdown::render(paste0("source/Jahresrechnung.Rmd"),
                  df_Render$Render,
                  output_dir = paste0(getwd(), "/output"))
print(clc)

paste("Bericht: \nJahresrechnung erstellt")|>
  writeLines()


#############################################################################################################################################
# Statistik
rmarkdown::render(paste0("source/Statistik.Rmd"),
                  df_Render$Render,
                  output_dir = paste0(getwd(), "/output"))
print(clc)

paste("Bericht: \nStatistik erstellt")|>
  writeLines()


#############################################################################################################################################
# Erstellen der Berichte für gewählte Vorführungen
if(c_run_single){
  for(ii in 1:nrow(df_mapping)){
  
    # Einlesen template der Abrechnung
    c_raw <- readLines("source/Abrechnung.Rmd")
    c_raw
  
    # Ändern des Templates mit user eingaben
    index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
    index
  
    c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(ii))
    writeLines(c_raw, paste0("source/temp.Rmd"))
  
    # Render
    rmarkdown::render(paste0("source/temp.Rmd"),
                      df_Render$Render,
                      output_dir = paste0(getwd(), "/output"))
  
    # Rename the file
    for (jj in 1:length(df_Render$Render)) {
      file.rename(from = paste0(getwd(),"/output/temp",df_Render$fileExt[jj]), 
                  to   = paste0(getwd(),"/output/",df_mapping$user_Datum[ii],df_Render$fileExt[jj])
                  )
    }
  
    # user interaction
    print(clc)
    paste("Bericht: \nAbrechnung vom", df_mapping$user_Datum[ii], "erstellt")|>
      writeLines()
  }
  remove(c_raw, index,ii,jj)
}

remove(c_Datum, c_suisa, c_verleiherabgaben, c_run_single)



#############################################################################################################################################
# remove temp files 
list.files(pattern = "temp", recursive = TRUE)|>
  file.remove()

#############################################################################################################################################
# User Interaktion

paste("\nAlle Berichte wurden erstellt und sind im Verzeichniss  \"output\" zu finden.\n")|>
  writeLines()



