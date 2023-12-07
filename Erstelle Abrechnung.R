#############################################################################################################################################
# Scritpt für den Kinoklub
# Bitte beachte das README.md
# Diesen Script erstellt Abrechnungen für den Kinoclub.
#
# V0.2.0
#############################################################################################################################################

source("source/read and convert.R")

#############################################################################################################################################
# index pro suisa_nr und Datume erstellen

df_mapping <- tibble(Datum = c_Date)|>
  mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
         index = row_number())


#############################################################################################################################################
# Erstellen der Berichte für gewählte Vorführungen

for(ii in 1:nrow(df_mapping)){

  # Einlesen template der Abrechnung
  c_raw <- readLines("source/Abrechnung.Rmd")
  c_raw

  # Ändern des Templates mit user eingaben
  index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
  index

  c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(ii))
  writeLines(c_raw, paste0("temp",".Rmd"))

  # Render
  rmarkdown::render(paste0("temp",".Rmd"),
                    c("html_document","word_document"),
                    output_dir = paste0(getwd(), "/output"))

  # Rename the file
  file.rename(paste0(getwd(),"/output/temp",".html"), paste0(getwd(),"/output/",df_mapping$user_Datum[ii],".html"))
  file.rename(paste0(getwd(),"/output/temp",".docx"), paste0(getwd(),"/output/",df_mapping$user_Datum[ii],".docx"))

  # user interaction
  print(clc)
  paste("Abrechnung vom", df_mapping$user_Datum[ii], "erstellt")|>
    writeLines()
}
file.remove("temp.Rmd")
remove(c_Datum, c_raw, c_suisa, c_verleiherabgaben, index,ii)

#############################################################################################################################################
# Erfolgsrechnung

rmarkdown::render(paste0("source/Erfolgsrechnung.Rmd"),
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

paste("Statistik erstellt")|>
  writeLines()






