# Dokumentation erstellen 
# 
# Einlesen von doc/README.Rmd
# Einfügen Versionierung
# README.md wird automatisch erstellt
library(tidyverse)

# function to add copy buttons into html file ###
add_copy_buttons_to_html <- function(html_file, output_file = html_file) {
  library(xml2)
  library(rvest)
  
  # Read HTML
  doc <- read_html(html_file)
  
  # Get all <pre><code>...</code></pre>
  code_blocks <- xml_find_all(doc, ".//pre[code]")
  
  # Add copy button inside each <pre>
  for (pre in code_blocks) {
    button <- read_html('<button class="copy-button">Copy</button>') %>% xml_find_first(".//button")
    xml_add_child(pre, button)
  }
  
  # Inject CSS and JS into <head>, if not already present
  head_node <- xml_find_first(doc, "//head")
  if (length(xml_find_all(doc, "//style[contains(.,'copy-button')]")) == 0) {
    style_node <- read_html('
      <style>
        .copy-button {
          position: absolute;
          top: 0.5em;
          right: 0.5em;
          background: #322f3b;
          border: none;
          padding: 4px 8px;
          cursor: pointer;
          font-size: 0.8em;
          border-radius: 4px;
          opacity: 0.6;
        }
        .copy-button:hover {
          opacity: 1;
        }
        pre {
          position: relative;
        }
      </style>') %>% xml_find_first("//style")
    xml_add_child(head_node, style_node)
  }
  
  if (length(xml_find_all(doc, "//script[contains(.,'navigator.clipboard')]")) == 0) {
    script_node <- read_html('
      <script>
      document.addEventListener("DOMContentLoaded", function() {
        document.querySelectorAll("pre code").forEach(function(codeBlock) {
          var button = document.createElement("button");
          button.className = "copy-button";
          button.type = "button";
          button.innerText = "Copy";

          button.addEventListener("click", function() {
            var text = codeBlock.innerText;
            navigator.clipboard.writeText(text).then(function() {
              button.innerText = "Copied!";
              setTimeout(function() {
                button.innerText = "Copy";
              }, 2000);
            });
          });

          var pre = codeBlock.parentNode;
          pre.appendChild(button);
        });
      });
      </script>') %>% xml_find_first("//script")
    xml_add_child(head_node, script_node)
  }
  
  # Save to file
  write_html(doc, file = output_file)
  message("✅ Copy buttons added to: ", output_file)
}

# Github readme.md ####
# Import c_script_version 
c_raw <- readLines("user_settings.R")
c_script_version <- c_raw[c_raw |> str_detect("c_script_version <-")] |>
  str_split(pattern = "\"") |>
  unlist()
c_script_version <- c_script_version[2]


# Tool Dokumentaion ####
c_raw <- readLines("doc/README.Rmd")

# Scrip Version einfügen 
index <- (1:length(c_raw))[c_raw|>str_detect("# Kinoklub")]
index

# Ändern des Templates
c(paste0("Script Version: ",c_script_version, collapse = ""),
  " ",
  c_raw[index[1]:length(c_raw)]
  )|>
  writeLines("README.md")

# Create TOC for Dokumentation
source("source/functions.R")

# Html Dokumentation ####
r_toc_for_Rmd(readLines("doc/README.Rmd"),
              toc_heading_string = "Inhaltsverzeichnis",
              pagebreak_level = "1",
              create_nb = T)|>
  writeLines("README.Rmd")

rmarkdown::render(input = "README.Rmd",
                  output_format = "html_document",
                  output_dir  = "doc/",
                  output_file = "Dokumentation.html")

# Edit html Dokumentation ####
library("xml2")

# Read docu
c_filePath <- "doc/Dokumentation.html"
html <- read_html(c_filePath)

# Find all text nodes
all_text_nodes <- xml_find_all(html, "//*[contains(text(), '## install.packages(c(')]")

# Content 
c_string <- xml_text(all_text_nodes)

# edit content
c_string <- substring(c_string,4, nchar(c_string))

# Change the first match
xml_text(all_text_nodes[[1]]) <- c_string

# Save modified HTML ####
write_html(html, c_filePath)

# add copy buttons to html ####
add_copy_buttons_to_html(c_filePath)


# Ftp server connection ####
ftp_server   <- "ftp://lx51.hoststar.hosting/"
ftp_user     <- Sys.getenv("ftp_user")
ftp_password <- Sys.getenv("ftp_password")

# Base path where to put the files (Must be a public html folder)
ftp_basepath <- "kinoklub.ch/public_html/kkTeam/reports/"

# FTP file upload to reports server ####
ftp_upload("doc/Dokumentation.html", ftp_server, ftp_user, ftp_password, ftp_basepath)

# word
# rmarkdown::render(input = paste0("README.Rmd"),
#                   output_format = "word_document",
#                   output_dir  = "doc/",
#                   output_file = "Dokumentation.docx")


# # PDF
# readLines("doc/README.Rmd")|>
#   writeLines("README.Rmd")
# 
# rmarkdown::render(input = "README.Rmd",
#                   output_format = "pdf_document",
#                   output_dir  = "doc/",
#                   output_file = "Dokumentation.pdf")

file.remove("README.Rmd")

message("Dokumentation erstellt und Hochgeladen")