#############################################################################################################################################
# Bitte beachte das README.md und die Dokumentation im Verzeichniss ".../doc"
# 
# Autor: Florian Wagner
# florian.wagner@wagnius.ch
# slvwagner@gmail.com

# 2024 V1.0 Go Live mit Stefan Jablonski, Nadia und Florian Wagner
# 2024 V1.1 Verkauf von Abos und Gutscheinen wird in der Jahresabarechnung berücksichtigt  
# 2024 V1.2 Abrechnung für Kinowerbung hinzugefügt:..../output/Auswertung.xlsx und Prognosen in der Statistik überarbeitet
# 2024 V1.3 Neuer Bericht Statistik_DT hinzugefügt. Interaktives durchsuchen aller Tabellen 
# 2024 V1.4 Jahresbarechnung detailed entfernt
# 2024 V1.5 Merge Verkaufsartikel "Popcorn frisch", "Popcorn Salz" zu "Popcorn frisch"
# 2024 V1.6 Statistik: Wochentaganalyse
# 2024 V1.7 Statistik ohne Datatable gelöscht
# 2024 V1.8 Dokumentations update 
# 2024 V1.9 Filmvorschläge from Wordpress 
# 2024 V1.10 PowerBi script
# 2024 V1.11 WordPress Filmvorschläge auswerten
# 2024 V1.12 Verleiherrechnung nur erstellen falls nötig (Kinoförder Gratis => nein, in Verleiherabgaben.xlsx)
# 2024 V1.13 Gemeinsame Abrechnung über Link Datum in Excel file "Verleiherabgaben.xlsx"
# 2024 V1.14 GUI Graphical user interface 
# 2024 V1.15 Fake Suisa Nummer von Advanced Tickets kann nun auch verarbeitet werden 
# 2024 V1.16 Introduction of environments to run GUI
# 2025 V1.17 Data type for excel files are defined by column type database 
# 2025 V2.00 Fist fully tested Version
# 2025 V2.01 New Feature: More than one Film per day can be handled
# 2025 V2.02 New Feature: Files can be uploade via GUI
# 2025 V2.03 Code clean up
# 2025 V2.04 Procinema und Filmvorschläge auswerten
# 2025 V2.05 Bereits gezeigte Filme im Archiv
# 2025 V2.06 Script running status bar
# 2025 V2.07 Speed up
# 2025 V3.00 Input Dateien GUi erstellt, Excel Dateien werden nicht mehr benötigt


#############################################################################################################################################
# Vorbereiten / Installieren
#############################################################################################################################################
rm(list = ls())
source("source/functions.R")
c_script_version <- "V3.00"

#############################################################################################################################################
# Packages loading
#############################################################################################################################################

packages <- c("rmarkdown", "rebus", "openxlsx", "lubridate", "DT", "magick", "webshot", "xml2", "tidyverse")
invisible(lapply(packages, library, character.only = TRUE))
remove(packages)

#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################

# Wiel lange dauer die Sommerpause
sommerpause = 65 # Tage

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
    axis.title = element_text(colour = "#f4cccc", size = 15),
    axis.text = element_text(colour = "#f4cccc"),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "#322f3b", color = "black"),
    legend.text = element_text(color = "#f4cccc"),
    legend.title = element_text(size = 12),
    title = element_text(color = "#f4cccc", size = 22)
  )

#############################################################################################################################################
# Versionskontrolle
#############################################################################################################################################
if(!file.exists("version control.ini")) { # ist kein versions kontrolle vorhanden?
  #versions kontrolle schreiben
  write(c_script_version, "version control.ini")
  
  # Löschen aller output files 
  c_path <- "output"
  c_files <- list.files(c_path, pattern = "html", full.names = T)
  c_files
  file.remove(c_files)|>suppressWarnings()
  
  c_path <- "output/pict"
  c_files <- list.files(c_path, pattern = "html", full.names = T)
  c_files
  file.remove(c_files)|>suppressWarnings()
  
  c_path <- "output/webserver"
  c_files <- list.files(c_path, pattern = "html", full.names = T)
  c_files
  file.remove(c_files)|>suppressWarnings()

}else{
  x <- read_file("version control.ini")|>
    str_remove("\r")|>
    str_remove("\n")
  x
  if(x != c_script_version){ # ist es nicht die aktuelle Version?
    # Löschen aller output files 
    c_path <- "output"
    c_files <- list.files(c_path, pattern = "html", full.names = T)
    c_files
    file.remove(c_files)|>suppressWarnings()
    
    c_path <- "output/pict"
    c_files <- list.files(c_path, pattern = "html", full.names = T)
    c_files
    file.remove(c_files)|>suppressWarnings()
    
    c_path <- "output/webserver"
    c_files <- list.files(c_path, pattern = "html", full.names = T)
    c_files
    file.remove(c_files)|>suppressWarnings()
    #versions kontrolle schreiben
    write(c_script_version, "version control.ini")
    
    # Löschen aller Daten die mit einer anderen Version erstellt wurden
    file.remove("environment.RData")|>
      suppressWarnings()
  }
}

#############################################################################################################################################
# Versionierung
#############################################################################################################################################
# Einlesen template der Verleiherabrechnung
c_raw <- readLines("doc/README.Rmd")

# Index where to find
c_index <- (1:length(c_raw))[c_raw|>str_detect("Script Version")]
c_index <- c_index[length(c_index)]
c_raw[c_index+1]

################################################
# Dokumentation anpassen falls neue Version
if(c_raw[c_index+1] != c_script_version){ 
  ######################################
  # Aktuelle Version ermitteln
  c_raw[c_index+1] <- c_script_version
  
  # neues file schreiben
  c_raw|>
    writeLines("doc/README.Rmd")
  
  ######################################
  # Scrip Versionshistorie ermitteln  
  c_raw <- readLines("user_settings.R")
  p <- "#"%R%SPC%R%one_or_more(DGT)%R%SPC%R%"V"%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)
  c_Version_hist <- c_raw[str_detect(c_raw, p)]|>
    str_remove("# ")|>
    paste0("  \\")
  c_Version_hist
  
  p <- one_or_more(DGT)%R%SPC%R%"V"%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)
  
  if(c_Version_hist[length(c_Version_hist)]|>str_extract(p) != c_script_version) stop("\nDer letzte Eintrag der Versionshistorie stimmt nicht mit der Variable c_script_version überein.\nBitte korrigieren")
  
  # Versionshistorie in Template einfügen
  c_raw <- readLines("doc/README.Rmd")
  c_raw[1:3]
  # Titel suchen
  index <- (1:length(c_raw))[c_raw|>str_detect("# Versionshistorie")]
  index
  # Ändern des Templates
  c(c_raw[1:(index + 1)], c_Version_hist,"\n")|>
    writeLines("doc/README.Rmd")
  
  source("doc/create Readme and Docu.R")
}

remove(c_raw, c_index)

writeLines("script run done: user_settings.R")

