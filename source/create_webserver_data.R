
# Package names
packages <- c("xml2")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#######################################################
# function to edit Site-Map: insert pictures
#######################################################
instert_picts <- function(raw_rmd, output_dir, index, fileNames, url) {
  # create link to pict and link to file 
  if(length(raw_rmd) == index){
    for (ii in 1:(length(fileNames))) { 
      if(ii == 1){ #letzte Zeile von Rmd
        raw_rmd <- c(raw_rmd[1:index],
                     paste0("[","![",fileNames[ii],"](",output_dir,fileNames[ii],".png)","](", url[ii],")")#,"  \\\n\\")," "
        )
      }else{ # normales einfügen
        raw_rmd <- c(raw_rmd[1:index],
                     paste0("[","![",fileNames[ii],"](",output_dir,fileNames[ii],".png)","](", url[ii],")",if((ii %% 2) == 0) {" \\"}),#,"  \\\n\\"),
                     if((ii %% 2) == 0) {"\\"}, # if index is even put aditional spacing 
                     raw_rmd[(index+1):length(raw_rmd)]
        )
      }
    }
  }else{ # normales einfügen 
    for (ii in 1:(length(fileNames))) {
      raw_rmd <- c(raw_rmd[1:index],
                   paste0("[","![",fileNames[ii],"](",output_dir,fileNames[ii],".png)","](", url[ii],")", if((ii %% 2) == 0) {" \\"}),#,"  \\\n\\"),
                   if((ii %% 2) == 0) {"\\"}, # if index is even put aditional spacing 
                   raw_rmd[(index+1):length(raw_rmd)]
      )
    }
  }
  return(raw_rmd)
}

#######################################################
# Finde die Suisa-Nummern und den Filmtitel im Html
# #######################################################
c_path <- "output/"
x <- list.files(c_path, "html")

df_reports <- tibble(FileName = list.files(c_path, "html"))
df_reports


df_temp1 <-  df_reports|>
  filter(str_detect(FileName, "Abrechnung"))|>
  pull()|>
  lapply(function(x){
    doc <- read_html(paste0(c_path,x))
    # Find elements to edit 
    element <- xml_find_first(doc, "body")|>
      xml_find_first("div")
    element
    
    children <- xml_children(element)[3]
    # Extract data
    c_raw <- xml_text(children)|>
      str_split("\n")|>
      unlist()|>
      str_remove("\r")
    c_raw
    
    # Create data to return
    tibble(`Suisa-Nummer` = c_raw[17],
           Filmtitel = c_raw[14],
           Datum = c_raw[11],
           typ = "Abrechnung Filmvorführungen",
           FileName = x)
    })|>
  bind_rows()
  
df_temp1

df_temp2 <-  df_reports|>
  filter(str_detect(FileName, "Verleiher"))|>
  pull()|>
  lapply(function(x){
    doc <- read_html(paste0(c_path,x))
    # Find elements to edit 
    element <- xml_find_first(doc, "body")|>
      xml_find_first("div")
    
    # Find all children of the node
    children <- xml_children(element)
    children <- children[[5]]|>
      xml_children()
  
    # Extract data
    c_raw <- xml_text(children[[2]])[1]|>
      str_split("\n", simplify = T)
  
    # Create data to return
    tibble(`Suisa-Nummer` = c_raw[,7],
           Filmtitel = c_raw[,8],
           Datum = c_raw[,9],
           FileName = x)
  })|>
  bind_rows()|>
  mutate(`Suisa-Nummer` = str_remove(`Suisa-Nummer`, "\r"),
         Filmtitel = str_remove(Filmtitel, "\r"),
         Datum = str_remove(Datum, "\r"),
         typ = "Verleiherabrechnung",
         )
df_temp1
df_temp2


df_reports <- bind_rows(df_temp2, 
                        df_temp1,
                        tibble(`Suisa-Nummer`= NA,
                               Filmtitel = NA,
                               Datum = NA,
                               typ = "Statistik",
                               FileName = "Statistik.html"),
                        tibble(`Suisa-Nummer`= NA,
                               Filmtitel = NA,
                               Datum = NA,
                               typ = "Jahresrechnung",
                               FileName = "Jahresrechnung.html")
                        )

df_reports|>
  tail()

# Convert filenames to URL
df_reports$url <- paste0("file:///",URLencode(paste0(c_WD,"/output/", df_reports$FileName)), 
                sep = "")

df_reports$path <- paste0(c_WD,"/output/pict")
df_reports

dir.create(c_path)|>suppressWarnings()

#############################################################################################################################################
# create site map
#############################################################################################################################################

if(c_SiteMap){
  # Was für Berichte typen sind vorhanden
  c_typ_Berichte <- df_reports|>
    distinct(typ)|>
    pull()
  
  # Vorschaubilder erzeugen wenn noch nicht vorhanden 
  ii <- 1
  if(!(length(list.files("output/", "html")) == length(list.files("output/pict/")))){
    library(magick)
    writeLines("Site-Map previews werden erstellt, einen Moment bitte: ")
    
    c_select <- !((df_reports$FileName|>str_remove(".html")) %in% (list.files("output/pict/")|>str_remove(".html.png")))
    
    ii <- 1
    for (ii in 1:length(df_reports$FileName[c_select])) {
      # Set the path to the input image
      input_path <- paste0(c_path, "/",df_reports$FileName[c_select][ii],".png")
      input_path
      
      # create a webshot, printed html
      webshot::webshot(url = df_reports$url[c_select][ii], file = input_path)
      
      # Read the image crop and resize and save
      image_read(input_path)|>
        image_crop(geometry = "992x992+0+0")|>
        image_resize("400x400")|>
        image_write(input_path)
      
      writeLines(".", sep = "")
    }
  }
  
  # Einlesen template der Verleiherabrechnung
  c_raw <- readLines("source/Site_Map.Rmd")
  c_raw
  
  ii <- 1
  for (ii in 1:length(c_typ_Berichte)) { # Für jeden Bericht typ muss ein Bild und Link eingefügt werden
    # Index where to insert  
    c_index <- (1:length(c_raw))[c_raw|>str_detect(c_typ_Berichte[ii])]
    c_index <- c_index[length(c_index)]
    c_index
    
    df_temp <- df_reports|>
      filter(typ == c_typ_Berichte[ii])
    df_temp
    
    #########################################
    # Jahresrechnung
    if(c_typ_Berichte[ii] == "Jahresrechnung"){
      c_select <- str_detect(df_reports$FileName, START%R%c_typ_Berichte[ii]%R%DOT%R%"html")
    }else{
      c_select <- str_detect(df_reports$FileName, START%R%c_typ_Berichte[ii])
    }
    
    
    c_raw <- instert_picts(c_raw,"output/pict/",c_index,df_reports$FileName[c_select], df_reports$url[c_select])
    c_raw
    
    #########################################
    # Abrechnung
    if(c_typ_Berichte[ii]=="Abrechnung"){
      for (jj in 1:length(df_reports$FileName[c_select])) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",df_reports$FileName[c_select][jj],"](", df_reports$url[c_select][jj],")  ",df_reports$Filmtitel[jj],"  \\"),
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"),
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
    
    #########################################
    # Verleiherabrechnung
    if(c_typ_Berichte[ii]=="Verleiherabrechnung"){
      df_temp <- df_reports|>
        filter(typ == "Verleiherabrechnung")
      df_temp
      for (jj in 1:length(df_temp)) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",df_reports$FileName[c_select][jj],"](", df_reports$url[c_select][jj],")  ",df_reports$Filmtitel[jj],"  \\"),
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"),
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
  }
  c_typ_Berichte[ii]
  c_raw
  # neues file schreiben
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines("Site-Map.Rmd")
  
  # Render
  rmarkdown::render(input = "Site-Map.Rmd")

}

# # Remove file
# file.remove("Site-Map.Rmd")

#############################################################################################################################################
# Data for Webserver
#############################################################################################################################################

#copy data from .../output to .../output/webserver
c_path <- "output/webserver"

if(!dir.exists(c_path)){
  dir.create(c_path)
}
if(!dir.exists(paste0(c_path,"/pict"))){
  dir.create(paste0(c_path,"/pict"))
}

# copy png
paste0(getwd(),"/output/pict/",list.files("output/pict/", pattern = "png", include.dirs = TRUE, recursive = FALSE))|>
  file.copy(paste0(c_path,"/pict"))


if(c_SiteMap){
  df_reports$FileName <- df_reports$FileName
  c_fileName
  # Was für Berichte typen sind vorhanden
  c_typ_Berichte <- df_reports$FileName|>
    str_extract(START%R%one_or_more(WRD))|>
    factor()|>
    levels()
  c_typ_Berichte
  
  # Convert filenames to URL
  df_reports$url <- paste0("",URLencode(df_reports$FileName))
  df_reports$url
  
  # Einlesen template der Verleiherabrechnung
  c_raw <- readLines("source/Site_Map.Rmd")
  c_raw
  
  # ii <- 4
  for (ii in 1:length(c_typ_Berichte)) { # Für jeden Bericht typ muss ein Bilde und Link eingefügt werden
    # Index where to insert  
    c_index <- (1:length(c_raw))[c_raw|>str_detect(c_typ_Berichte[ii])]
    c_index <- c_index[length(c_index)]
    c_index
    
    c_raw
    c_raw[c_index]
    
    if(c_typ_Berichte[ii] == "Jahresrechnung"){
      c_select <- str_detect(df_reports$FileName, START%R%c_typ_Berichte[ii]%R%DOT%R%"html")
      c_raw <- instert_picts(c_raw,"pict/",c_index,df_reports$FileName[c_select], df_reports$url[c_select])
    }else{
      c_select <- str_detect(df_reports$FileName, START%R%c_typ_Berichte[ii])
      c_raw <- instert_picts(c_raw,"pict/",c_index,df_reports$FileName[c_select], df_reports$url[c_select])
    }
    
    c_raw
    df_reports$FileName[c_select]
    df_reports$url[c_select]

    c_raw <- instert_picts(c_raw,"pict/",c_index,df_reports$FileName[c_select], df_reports$url[c_select])
    c_raw

    c_raw[c_index]
    
    
    # Linkliste einfügen
    if(c_typ_Berichte[ii]=="Verleiherabrechnung"){
      for (jj in 1:length(df_reports$FileName[c_select])) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",df_reports$FileName[c_select][jj],"](", df_reports$url[c_select][jj],")  ",df_reports$Filmtitel[jj],"  \\"), 
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"), 
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
    
    # Linkliste einfügen
    if(c_typ_Berichte[ii]=="Abrechnung"){
      for (jj in 1:length(df_reports$FileName[c_select])) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",df_reports$FileName[c_select][jj],"](", df_reports$url[c_select][jj],")  ",df_reports$Filmtitel[jj],"  \\"), 
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"), 
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
  }
  
  c_typ_Berichte[ii]
  c_raw
  
  # neues file schreiben
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines("output/webserver/index.Rmd")
  
  # Render
  rmarkdown::render(input = "output/webserver/index.Rmd")
  # Remove file
  file.remove("output/webserver/index.Rmd")
  # Remove directory
  unlink(paste0(c_path,"/pict"), recursive = TRUE)
  
}

#############################################################################################################################################
# edit html
#############################################################################################################################################
# Package names
packages <- c("xml2")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


add_SiteMapLink <- function(file_path) {
  # load html file
  doc <- read_html(file_path)
  
  # Find elements to edit 
  element <- xml_find_first(doc, "body")|>
    xml_find_first("div")
  
  # Find all children of the parent node
  children <- xml_children(element)
  
  # Insert Node
  xml_add_child(children[[1]], paste0("a href=\"",URLencode(paste0("index.html")),"\""), "Site-Map")
  write_xml(doc, file_path)
}

#copy data from .../output to .../output/webserver
c_path <- "output/webserver"

# copy html 
paste0("output/",list.files("output/",pattern = "html",include.dirs = FALSE, recursive = FALSE))|>
  file.copy(paste0(c_path,""), overwrite = TRUE)

c_files <- list.files("output/webserver/",pattern = "html"%R%END,include.dirs = FALSE, recursive = FALSE)
c_files <- paste0("output/webserver/",c_files)

# apply Site-Map link
c_files|>
  lapply(add_SiteMapLink)



