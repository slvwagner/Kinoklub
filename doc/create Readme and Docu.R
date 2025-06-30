# Dokumentation erstellen 
# 
# Einlesen von doc/README.Rmd
# Einfügen Versionierung
# README.md wird automatisch erstellt
library(tidyverse)

######################################################################################
# Import c_script_version 
c_raw <- readLines("user_settings.R")
c_script_version <- c_raw[c_raw |> str_detect("c_script_version <-")] |>
  str_split(pattern = "\"") |>
  unlist()
c_script_version <- c_script_version[2]

######################################################################################
c_raw <- readLines("doc/README.Rmd")

# Scrip Version einfügen 
index <- (1:length(c_raw))[c_raw|>str_detect("# Kinoklub")]
# Ändern des Templates
c(paste0("Script Version: ",c_script_version, collapse = ""),
  " ",
  c_raw[index:length(c_raw)])|>
  writeLines("README.md")


######################################################################################
# Create TOC for Dokumentation
source("source/functions.R")

readLines("doc/README.Rmd")|>
  writeLines("README.Rmd")

rmarkdown::render(input = paste0("README.Rmd"),
                  output_format = "html_document",
                  output_dir  = "doc/",
                  output_file = "Dokumentation.html")

r_toc_for_Rmd(readLines("doc/README.Rmd"),
              toc_heading_string = "Inhaltsverzeichnis",
              pagebreak_level = "1",
              create_nb = T)|>
  writeLines("README.Rmd")

rmarkdown::render(input = paste0("README.Rmd"),
                  output_format = "pdf_document",
                  output_dir  = "doc/",
                  output_file = "Dokumentation.pdf")



# rmarkdown::render(input = paste0("README.Rmd"),
#                   output_format = "word_document",
#                   output_dir  = "doc/",
#                   output_file = "Dokumentation.docx")

file.remove("README.Rmd")
