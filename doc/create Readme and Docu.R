# Write README
c_raw <- readLines("doc/README.Rmd")

# Titel suchen
index <- (1:length(c_raw))[c_raw|>str_detect("# Kinoklub")]
# Ändern des Templates
c(paste0("Script Version: ",c_script_version, collapse = ""),
  " ",
  c_raw[index:length(c_raw)])|>
  writeLines("README.md")

# Create TOC for Dokumentation
source("source/functions.R")
r_toc_for_Rmd(readLines("doc/README.Rmd"), 
              toc_heading_string = "Inhaltsverzeichnis",
              pagebreak_level = "2",
              create_nb = T)|>
  writeLines("README.Rmd")

rmarkdown::render(input = paste0("README.Rmd"),
                  output_format = "pdf_document",
                  output_dir  = "doc/",
                  output_file = "Dokumentation.pdf")

rmarkdown::render(input = paste0("README.Rmd"),
                  output_format = "html_document",
                  output_dir  = "doc/",
                  output_file = "Dokumentation.html")

# rmarkdown::render(input = paste0("README.Rmd"),
#                   output_format = "word_document",
#                   output_dir  = "doc/",
#                   output_file = "Dokumentation.docx")

file.remove("README.Rmd")