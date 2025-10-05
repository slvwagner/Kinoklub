#############################################################################################################################################
# Autor: Florian Wagner
# florian.wagner@wagnius.ch
# slvwagner@gmail.com
#############################################################################################################################################

# Version history
#############################################################################################################################################
# 2024 V1.00 Go Live mit Stefan Jablonski, Nadia und Florian Wagner
# 2024 V1.01 Verkauf von Abos und Gutscheinen wird in der Jahresabarechnung berücksichtigt  
# 2024 V1.02 Abrechnung für Kinowerbung hinzugefügt:..../output/Auswertung.xlsx und Prognosen in der Statistik überarbeitet
# 2024 V1.03 Neuer Bericht Statistik_DT hinzugefügt. Interaktives durchsuchen aller Tabellen 
# 2024 V1.04 Jahresbarechnung detailed entfernt
# 2024 V1.05 Merge Verkaufsartikel "Popcorn frisch", "Popcorn Salz" zu "Popcorn frisch"
# 2024 V1.06 Statistik: Wochentaganalyse
# 2024 V1.07 Statistik ohne Datatable gelöscht
# 2024 V1.08 Dokumentations update 
# 2024 V1.09 Filmvorschläge from Wordpress 
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
# 2025 V3.01 Kinoklub GUI überarbeitet
# 2025 V3.02 Programmvorschlag Spallte Kategorie in Kommentar umbenannt und Kommentarspalte dem Programm hinzugefügt
# 2025 V3.03 Datei handling wurde geändert im GUI und es ist nun möglich im Input die files zu prüfen.
# 2025 V3.04 Worklow Datein upload Eintritte und Kiosk korrigiert. 

#############################################################################################################################################
# Vorbereiten / Installieren
#############################################################################################################################################

rm(list = ls())
source("source/functions.R")

#############################################################################################################################################
# Packages loading
#############################################################################################################################################

packages <- c("rmarkdown", "rebus", "openxlsx", "lubridate", "DT", "tidyverse", "data.table")
invisible(lapply(packages, library, character.only = TRUE))
remove(packages)

#############################################################################################################################################
# find script version in comments above
#############################################################################################################################################

c_raw <- readLines("user_settings.R")
# library(rebus)
# p <- DGT%R%DGT%R%DGT%R%DGT%R%SPC%R%"V"%R%DGT%R%DOT%R%DGT%R%DGT
p1 <- "\\d\\d\\d\\d\\sV\\d\\.\\d\\d"
# p <- DGT%R%DGT%R%DGT%R%DGT
p2 <- "\\d\\d\\d\\d"

df_version <- tibble(Version = str_extract(c_raw, p1))|>
  mutate(index = row_number(),
         String = c_raw|>
           str_remove("#")|>
           str_trim()
         )|>
  filter(!is.na(Version))|>
  mutate(Version = str_remove(Version, p2)|>
           str_trim(),
         String = paste0(String, "\\")
         )
df_version

c_script_version <- df_version|>
  filter(index == max(index))|>
  select(Version)|>
  pull()
c_script_version

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

# Data table in german ####
DT_language <- list(
  lengthMenu = "Zeige _MENU_ Zeilen pro Seite", # Text für das Dropdown-Menü
  search = "Suchen:", # Text für das Suchfeld
  searchPlaceholder = "Suchbegriff eingeben...", # Platzhaltertext für das Suchfeld
  zeroRecords = "Keine passenden Einträge gefunden", # Text, wenn keine Einträge gefunden wurden
  info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen", # Info-Text
  infoEmpty = "Zeige 0 bis 0 von 0 Einträgen", # Info-Text, wenn keine Einträge vorhanden sind
  infoFiltered = "(gefiltert aus _MAX_ Einträgen)", # Info-Text bei Filterung
  paginate = list(
    first = "Erste Seite", # Text für die erste Seite
    last = "Letzte Seite", # Text für die letzte Seite
    `next` = "Nächste Seite", # Text für die nächste Seite
    previous = "Vorherige Seite" # Text für die vorherige Seite
  )
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
    
    #versions kontrolle schreiben
    write(c_script_version, "version control.ini")
    
    # Einlesen Dokumentation
    c_raw <- readLines("doc/README.Rmd")
    
    # README Dokumentversion
    c_index <- (1:length(c_raw))[c_raw|>str_detect("Script Version")]
    c_index <- c_index[length(c_index)] + 1
    
    # update Dokumentversion
    c_raw[c_index] <- c_script_version
    
    # Titel suchen
    index <- (1:length(c_raw))[c_raw|>str_detect("# Versionshistorie")]
    index
    
    # Ändern des Templates
    c(c_raw[1:index], df_version$String,"\n")|>
      writeLines("doc/README.Rmd")
    
    source("doc/create Readme and Docu.R")
  }
}

remove(df_version)

writeLines("script run done: user_settings.R")

