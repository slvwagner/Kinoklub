#############################################################################################################################################
# Scritpt für den Kinoklub 
# Bitte beachte das README.md 
# Diesen Script erstellt Abrechnungen für den Kinoclub.   
# 
# V0.1.0
#############################################################################################################################################

source("source/read and convert.R")

#############################################################################################################################################
# index pro suisa_nr und Datume erstellen 

df_mapping <- df_Eintritt|>
  distinct(Datum, suisa_nr)|>
  mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
         index = row_number())

#############################################################################################################################################
# Abrechnungsdatum durch den User festlegen 

c_select <- select.list(df_mapping$user_Datum,title = "Bitte das Abrechnungs Datum auswaehlen:\n" ,
                        multiple = T,
                        graphics = T)|>
  lubridate::dmy()|>
  as.Date()


#############################################################################################################################################
# Erstellen der Berichte für gewählte Vorführungen

c_index <- df_mapping|>
  filter(Datum %in% c_select)|>
  select(index)|>
  pull()

for(jj in c_index){
  
  # Einlesen template der Abrechnung
  c_raw <- readLines("source/Abrechnung.Rmd")
  c_raw
  
  # Ändern des Templates mit user eingaben
  index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
  index
  
  c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(jj))
  writeLines(c_raw, paste0("temp",".Rmd"))
  
  # Render
  rmarkdown::render(paste0("temp",".Rmd"),
                    c("html_document","word_document"),
                    output_dir = paste0(getwd(), "/output"))
  
  # Rename the file
  file.rename(paste0(getwd(),"/output/temp",".html"), paste0(getwd(),"/output/",df_mapping$Datum[jj],".html"))
  file.rename(paste0(getwd(),"/output/temp",".docx"), paste0(getwd(),"/output/",df_mapping$Datum[jj],".docx"))
  
  # user interaction
  print(clc)
  paste("Abrechnung vom", df_mapping$Datum[jj], "erstellt")|>
    writeLines()
}


#############################################################################################################################################
# Erfolgsrechnung

rmarkdown::render(paste0("source/Erfolgs Rechnung.Rmd"),
                  c("html_document","word_document"),
                  output_dir = paste0(getwd(), "/output"))
print(clc)

paste("Erfolgsrechnung erstellt")|>
    writeLines()


#############################################################################################################################################
# Statistik

rmarkdown::render(paste0("source/Statistik.Rmd"),
                  c("html_document","word_document"),
                  output_dir = paste0(getwd(), "/output"))
print(clc)

paste("Erfolgsrechnung erstellt")|>
  writeLines()

#############################################################################################################################################
# remove temp file
file.remove("temp.Rmd")



