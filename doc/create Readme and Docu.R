# Create TOC for doc/README.Rmd file 
source("source/functions.R")
r_toc_for_Rmd(readLines("doc/README.Rmd"), 
              toc_heading_string = "Inhaltsverzeichnis",
              create_nb = F)|>
  writeLines("README.md")

# Render READ me
rmarkdown::render(paste0("README.md"), 
                  output_dir  = "doc/",
                  output_file = "Dokumentation.html")

# "html_document","word_document","pdf_document"

# rmarkdown::render(input = paste0("README.md"),
#                   output_format = "word_document",
#                   output_dir  = "doc/",
#                   output_file = "Dokumentation.docx")

# rmarkdown::render(input = paste0("README.md"),
#                   output_format = "pdf_document",
#                   output_dir  = "doc/",
#                   output_file = "Dokumentation.pdf")