# Create TOC for doc/README.Rmd file 
source("source/functions.R")
r_toc_for_Rmd(readLines("doc/README.Rmd"), 
              toc_heading_string = "Inhaltsverzeichnis",
              create_nb = TRUE)|>
  writeLines("README.md")

# Render READ me
rmarkdown::render(paste0("README.md"), 
                  output_dir  = "doc/",
                  output_file = "Dokumentation.html")