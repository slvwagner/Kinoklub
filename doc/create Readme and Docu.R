# Write README
readLines("doc/README.Rmd")|>
  writeLines("README.md")

# Create TOC for Dokumentation
source("source/functions.R")
r_toc_for_Rmd(readLines("doc/README.Rmd"), 
              toc_heading_string = "Inhaltsverzeichnis",
              create_nb = T)|>
  writeLines("README.Rmd")

rmarkdown::render(input = paste0("README.Rmd"),
                  output_format = "pdf_document",
                  output_dir  = "doc/",
                  output_file = "Dokumentation.pdf")

# rmarkdown::render(input = paste0("README.Rmd"),
#                   output_format = "html_document",
#                   output_dir  = "doc/",
#                   output_file = "Dokumentation.docx")

file.remove("README.Rmd")