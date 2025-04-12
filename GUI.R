# Graphical user interface für den Kinoklub
# Diese App kann mit Run App in Rstudio gestartet werden.

# Vorbereiten / Installieren
rm(list = ls())

# Define libraries to be installed
packages <- c(
  "rmarkdown",  "rebus",  "openxlsx",  "tidyverse",
  "lubridate",  "DT",  "shiny",  "shinyBS",  "magick",
  "webshot",  "xml2",  "furrr", "future"
)
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
packages <- c(
  "rmarkdown",  "rebus",  "openxlsx",  "lubridate",
  "DT",  "magick",  "webshot",  "xml2",  "tidyverse",
  "furrr", "future"
)
invisible(lapply(packages, library, character.only = TRUE))
remove(packages, installed_packages)

# load user settings
source("user_settings.R")

# Load excel column definition database
col_env <- new.env()
load("col_env.RData", envir = col_env)

# create environment to run WordPress scripts
WordPress_env <- new.env()

# Functions
source("source/functions.R")

# Erstellen von Verzeichnissen
dir.create("output/") |> suppressWarnings()
dir.create("output/data/") |> suppressWarnings()

# Function to create icons for the site map
create_icons <- function(m_Film, c_path, c_url) {
  library(furrr)
  library(webshot)  # Ensure webshot is loaded
  library(magick)   # Ensure magick is loaded
  c_select <- !((m_Film$FileName |> str_remove(".html")) %in% 
                  (list.files("output/pict/") |> str_remove(".html.png")))
  
  # Determine the number of cores to use
  num_cores <- availableCores() - 1  # Use all but one core to avoid overloading the system
  if(num_cores > 5) num_cores <- 5
  if(nrow(m_Film) < num_cores) {
    num_cores <- nrow(m_Film)
  }
  paste("Number of cores:", num_cores) |>
    writeLines()
  
  # Set up parallel processing
  plan(multisession, workers = num_cores)  # Use all but one core
  
  # Function to render a single icon
  render_icons <- function(ii, m_Film, c_path, c_url, c_select) {
    # Set the path to the input image
    input_path <- paste0(c_path, "/", m_Film$FileName[c_select][ii], ".png")
    # Create a webshot, printed html
    webshot::webshot(url = c_url[c_select][ii], file = input_path)
    # Read the image, crop, resize, and save
    image_read(input_path) |>
      image_crop(geometry = "992x992+0+0") |>
      image_resize("400x400") |>
      image_write(input_path)
  }
  
  # Apply the function in parallel with a seed for parallel-safe random numbers
  future_map(1:length(m_Film$FileName[c_select]), 
             ~render_icons(.x, m_Film, c_path, c_url, c_select), 
             .options = furrr_options(seed = TRUE)
  )
}

# Function to render a single RMarkdown file
render_single_file <- function(input, output, envir) {
  rmarkdown::render(
    input = input,        # input file name
    output_file = output, # output file name
    output_dir = "output",# where to put the output file (directory) 
    envir = envir, 
    quiet = TRUE  # Suppress output for cleaner logs
  )
}

# Index pro Suisa-Nummer und Datum erstellen
Abrechnung_mapping <- function(data_env, start, end) {
  df_mapping <- tibble(Datum = data_env$df_mapping$Datum, Suisanummer = data_env$df_mapping$Suisanummer) |>
    mutate(user_Datum = format(Datum, "%d.%m.%Y"),
           index = row_number())
  
  # Soll die Verleiherabrechnung erzeugt werden?
  df_mapping <- data_env$df_verleiherabgaben |>
    select(Datum, Suisanummer, `Kinoförderer gratis?`) |>
    right_join(df_mapping, by = join_by(Datum, Suisanummer)) |>
    mutate(
      CreateReportVerleiherabrechnung = if_else(`Kinoförderer gratis?` == "ja", F, T),
      `Kinoförderer gratis?` = NULL
    ) |>
    filter(between(Datum, as.Date(start), as.Date(end)))|>
    mutate(fileName_RMD            = paste0("source/Abrechnung ",user_Datum," ", Suisanummer,".Rmd"),
           fileName_html           = paste0("source/Abrechnung ",user_Datum," ", Suisanummer,".html"),
           fileName_RMD_Verleiher  = paste0("source/Verleiherabrechnung ",user_Datum," ", Suisanummer,".Rmd"),
           fileName_html_Verleiher = paste0("source/Verleiherabrechnung ",user_Datum," ", Suisanummer,".html")
    )|>
    left_join(data_env$df_show|>
                distinct(`Suisa Nummer`,.keep_all = T)|>
                select(`Suisa Nummer`, Filmtitel),
              by = c(Suisanummer = "Suisa Nummer")
    )|>
    arrange(index)
  return(df_mapping)
}

# Erstellen der Abrechnung pro Filmvorführung
AbrechnungErstellen <- function(df_mapping, df_Abrechnung, toc) {
  for (ii in df_mapping$index) {
    # Template der Abrechnung einlesen
    c_raw <- readLines("source/Abrechnung.Rmd")
    
    # Ändern des Templates: Variable im Template ii wird gesetzt. c_Date[ii] wird verwendet um das korrekte Datum für die Bereichterstellung auszuwählen.
    index <- (1:length(c_raw))[c_raw |> str_detect("variablen")]
    c_raw[(index + 1)] <- c_raw[(index + 1)] |> str_replace(one_or_more(DGT), paste0(ii))
    
    # Ändern des Templates Titel Filmname
    index <- (1:length(c_raw))[c_raw |> str_detect("Abrechnung Filmvorführung")]
    c_temp1 <- df_Abrechnung |>
      filter(
        Datum == (df_mapping |> filter(index == ii) |> select(Datum) |> pull()),
        `Suisa Nummer` == (df_mapping |> filter(index == ii) |> select(Suisanummer) |> pull())
      ) |>
      mutate(Anfang = paste0(lubridate::hour(Anfang),":",lubridate::minute(Anfang) |> as.character() |> formatC(format = "0", width = 2) |> str_replace(SPC, "0")),
             Datum = paste0(day(Datum), ".", month(Datum), ".", year(Datum))
      ) |>
      rename(`Total Gewinn [CHF]` = `Gewinn/Verlust Filmvorführungen [CHF]`) |>
      select(Filmtitel) |>
      pull()
    
    c_temp <- c_raw[(index)] |> str_split("\"", simplify = T) |> as.vector()
    c_temp <- c_temp[1:2]
    c_temp <- paste0(c(c_temp), collapse = "\"")
    c_temp <- paste0(c(c_temp, " "), collapse = "")
    c_temp <- paste0(c(c_temp, c_temp1), collapse = "")
    c_raw[(index)] <- paste0(c(c_temp, "\""), collapse = "")
    
    # Create Abrechnung
    c_fileName <- df_mapping|>filter(index == ii)|>select(fileName_RMD)|>pull()
    if (toc) {
      # neues file schreiben mit toc
      c_raw |>
        r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
        writeLines(c_fileName)
    } else {
      # neues file schreiben ohne toc
      c_raw |>
        writeLines(c_fileName)
    }
    
    # Create Verleiherabrechnung
    do_it <- df_mapping|>filter(index == ii)|>select(CreateReportVerleiherabrechnung)|>pull()
    if(do_it){
      # Template der Abrechnung einlesen
      c_raw <- readLines("source/Verleiherabrechnung.Rmd")
      
      # Ändern des Templates: Variable im Template ii wird gesetzt. c_Date[ii] wird verwendet um das korrekte Datum für die Bereichterstellung auszuwählen.
      index <- (1:length(c_raw))[c_raw |> str_detect("variablen")]
      c_raw[(index + 1)] <- c_raw[(index + 1)] |> str_replace(one_or_more(DGT), paste0(ii))
      
      # neues file schreiben ohne toc
      c_fileName <- df_mapping|>filter(index == ii)|>select(fileName_RMD_Verleiher)|>pull()
      c_raw |>
        writeLines(c_fileName)
    }
  }
  
  library(furrr)
  # Determine the number of cores to use
  num_cores <- parallel::detectCores() - 1  # Use all but one core to avoid overloading the system
  if(num_cores > 4) num_cores <- 5
  if(nrow(df_mapping) < num_cores) {
    num_cores <- nrow(df_mapping)
  }
  
  # Render in parallel Abrechnung
  plan(multisession, workers = num_cores)
  
  # Render files in parallel
  future_walk(1:nrow(df_mapping), function(ii) {
    render_single_file(
      df_mapping$fileName_RMD[ii],
      df_mapping$fileName_html[ii],
      data_env
    )
  })
  file.remove(df_mapping$fileName_RMD)
  
  # Render in parallel Verleiherabrechnung
  create_verleiherabrechnung <- df_mapping$CreateReportVerleiherabrechnung |> sum()
  
  if (create_verleiherabrechnung > 0) {
    # Determine the number of cores to use
    num_cores <- parallel::detectCores() - 1  # Use all but one core to avoid overloading the system
    if (num_cores > 4) num_cores <- 5
    
    # Adjust cores based on workload
    if (create_verleiherabrechnung < num_cores) {
      num_cores <- create_verleiherabrechnung
    }
    
    # Select files to render
    c_select <- df_mapping |>
      filter(CreateReportVerleiherabrechnung == TRUE) |>
      select(index) |>
      pull() |>
      as.integer()
    
    # File names to render
    input  <- df_mapping |> filter(index %in% c_select) |> pull(fileName_RMD_Verleiher)
    output <- df_mapping |> filter(index %in% c_select) |> pull(fileName_html_Verleiher)
    
    # Render in parallel Verleiherabrechnung
    library(future)
    plan(multisession, workers = num_cores)
    
    # Render files in parallel
    library(furrr)
    future_walk(1:length(c_select), function(ii) {
      tryCatch({
        render_single_file(
          input[ii],
          output[ii],
          data_env
        )
      }, error = function(e) {
        message("Error rendering file at index ", ii, ": ", e$message)
      })
    })
    
    # Delete selected files
    if (all(file.exists(input))) {
      file.remove(input)
    } else {
      warning("Some files to delete do not exist.")
    }
  }
  return(NULL)
}

# Erstellen der Verleiherabrechnung pro Filmvorführung
VerleiherabrechnungErstellen <- function(df_mapping, df_Abrechnung, toc) {
  for (ii in df_mapping$index) {
    # Create Verleiherabrechnung
    do_it <- df_mapping|>filter(index == ii)|>select(CreateReportVerleiherabrechnung)|>pull()
    if(do_it){
      # Template der Abrechnung einlesen
      c_raw <- readLines("source/Verleiherabrechnung.Rmd")
      
      # Ändern des Templates: Variable im Template ii wird gesetzt. c_Date[ii] wird verwendet um das korrekte Datum für die Bereichterstellung auszuwählen.
      index <- (1:length(c_raw))[c_raw |> str_detect("variablen")]
      c_raw[(index + 1)] <- c_raw[(index + 1)] |> str_replace(one_or_more(DGT), paste0(ii))
      
      # neues file schreiben ohne toc
      c_fileName <- df_mapping|>filter(index == ii)|>select(fileName_RMD_Verleiher)|>pull()
      c_raw |>
        writeLines(c_fileName)
    }
  }
  
  # Render in parallel Verleiherabrechnung
  create_verleiherabrechnung <- df_mapping$CreateReportVerleiherabrechnung |> sum()
  
  if (create_verleiherabrechnung > 0) {
    # Determine the number of cores to use
    num_cores <- parallel::detectCores() - 1  # Use all but one core to avoid overloading the system
    if (num_cores > 4) num_cores <- 5
    
    # Adjust cores based on workload
    if (create_verleiherabrechnung < num_cores) {
      num_cores <- create_verleiherabrechnung
    }
    
    # Select files to render
    c_select <- df_mapping |>
      filter(CreateReportVerleiherabrechnung == TRUE) |>
      select(index) |>
      pull() |>
      as.integer()
    
    # File names to render
    input  <- df_mapping |> filter(index %in% c_select) |> pull(fileName_RMD_Verleiher)
    output <- df_mapping |> filter(index %in% c_select) |> pull(fileName_html_Verleiher)
    
    # Render in parallel Verleiherabrechnung
    library(future)
    plan(multisession, workers = num_cores)
    
    # Render files in parallel
    library(furrr)
    future_walk(1:length(c_select), function(ii) {
      tryCatch({
        render_single_file(
          input[ii],
          output[ii],
          data_env
        )
      }, error = function(e) {
        message("Error rendering file at index ", ii, ": ", e$message)
      })
    })
    
    # Delete selected files
    if (all(file.exists(input))) {
      file.remove(input)
    } else {
      warning("Some files to delete do not exist.")
    }
  }
  return(NULL)
}

# Statistik-Bericht erstellen
StatistikErstellen <- function(toc) {
  # Einlesen
  c_raw <- readLines("source/Statistik.Rmd")
  # Inhaltsverzeichnis
  if (toc |> as.logical()) {
    # neues file schreiben mit toc
    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines(paste0("source/temp.Rmd"))
  } else {
    # neues file schreiben ohne toc
    c_raw |>
      writeLines(paste0("source/temp.Rmd"))
  }
  # Render
  render_single_file(input = "source/temp.Rmd", output = "Statistik.html", envir = data_env)
}

# Filmvorschlag erstellen
FilmvorschlagErstellen <- function(toc, data_env) {
  # Einlesen
  c_raw <- readLines("source/Archiv.Rmd")
  # Inhaltsverzeichnis
  if (toc |> as.logical()) {
    # neues file schreiben mit toc
    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines(paste0("source/temp.Rmd"))
  } else {
    # neues file schreiben ohne toc
    c_raw |>
      writeLines(paste0("source/temp.Rmd"))
  }
  # Render
  render_single_file(input = "source/Archiv.Rmd", output = "Archiv.html", envir = data_env)
}

# Jahresrechnung-Bericht erstellen
JahresrechnungErstellen <- function(toc) {
  # Einlesen
  c_raw <- readLines("source/Jahresrechnung.Rmd")
  # Inhaltsverzeichnis
  if (toc |> as.logical()) {
    # neues file schreiben mit toc
    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines(paste0("source/temp.Rmd"))
  } else {
    # neues file schreiben ohne toc
    c_raw |>
      writeLines(paste0("source/temp.Rmd"))
  }
  # Render
  render_single_file(input = "source/temp.Rmd", output = "Jahresrechnung.html", envir = data_env)
}

# function to edit Site-Map: insert pictures
instert_picts <- function(raw_rmd, output_dir, index, fileNames, url) {
  # create link to pict and link to file
  if (length(raw_rmd) == index) {
    for (ii in 1:(length(fileNames))) {
      if (ii == 1) {
        # letzte Zeile von Rmd
        raw_rmd <- c(
          raw_rmd[1:index],
          paste0(
            "[",
            "![",
            fileNames[ii],
            "](",
            output_dir,
            fileNames[ii],
            ".png)",
            "](",
            url[ii],
            ")"
          ) # ,"  \\\n\\")," "
        )
      } else {
        # normales einfügen
        raw_rmd <- c(
          raw_rmd[1:index],
          paste0(
            "[",
            "![",
            fileNames[ii],
            "](",
            output_dir,
            fileNames[ii],
            ".png)",
            "](",
            url[ii],
            ")",
            if ((ii %% 2) == 0) {
              " \\"
            }
          ),
          # ,"  \\\n\\"),
          if ((ii %% 2) == 0) {
            "\\"
          },
          # if index is even put additional spacing
          raw_rmd[(index + 1):length(raw_rmd)]
        )
      }
    }
  } else {
    # normales einfügen
    for (ii in 1:(length(fileNames))) {
      raw_rmd <- c(
        raw_rmd[1:index],
        paste0(
          "[",
          "![",
          fileNames[ii],
          "](",
          output_dir,
          fileNames[ii],
          ".png)",
          "](",
          url[ii],
          ")",
          if ((ii %% 2) == 0) {
            " \\"
          }
        ),
        # ,"  \\\n\\"),
        if ((ii %% 2) == 0) {
          "\\"
        },
        # if index is even put additional spacing
        raw_rmd[(index + 1):length(raw_rmd)]
      )
    }
  }
  return(raw_rmd)
}

# function to create a site-map
webserver <- function() {
  # Alle Bilder löschen die nicht als html vorhanden sind
  if (dir.exists("output/pict")) {
    c_pict <- list.files("output/pict") |> str_remove(pattern = ".png")
    c_html <- list.files("output/", pattern = "html")
    
    list.files("output/pict", full.names = TRUE)[!(c_pict %in% c_html)] |>
      file.remove()
  }
  
  # Alle html Dateien löschen
  if (dir.exists("output/webserver")) {
    list.files("output/webserver", full.names = TRUE) |>
      file.remove()
  }
  
  # Find reports
  c_path <- paste0("output/")
  writeLines(c_path)
  df_reports <- tibble(FileName = list.files(c_path, "html"))
  df_reports
  
  if (nrow(df_reports) == 0) {
    stop("\nNo Reports can be found in .../output/")
  }
  
  # Abrechnungen suchen
  df_temp1 <- df_reports |>
    filter(str_detect(FileName, "Abrechnung"))
  
  df_temp1 <- df_temp1 |>
    pull() |>
    lapply(function(x) {
      doc <- read_html(paste0(c_path, x))
      # Find elements to edit
      element <- xml_find_first(doc, "body") |>
        xml_find_first("div")
      c_raw <- xml_children(element) |>
        xml_text()
      
      if (sum(str_detect(c_raw, "Inhaltsverzeichnis")) > 0) {
        index <- c_raw |>
          str_detect("Übersicht")
        element <- xml_children(element)[index]
        element
        
        index <- element |>
          xml_text() |>
          str_detect("Filmtitel")
        element <- xml_children(element)[index]
        element
        
        # Extract data
        c_raw <- element[3] |>
          xml_text() |>
          str_split("\n") |>
          unlist() |>
          str_remove("\r")
        c_raw
        
        # Create data to return
        # Create data to return
        tibble(
          `Suisa-Nummer` = c_raw[14],
          Filmtitel = c_raw[11],
          Datum = c_raw[8],
          typ = "Abrechnung Filmvorführungen",
          FileName = x
        )
      } else{
        index <- c_raw |>
          str_detect("Übersicht")
        element <- xml_children(element)[index]
        
        index <- element |>
          xml_text() |>
          str_detect("Filmtitel")
        element <- xml_children(element)[index]
        
        index <- element |>
          xml_text() |>
          str_detect("Filmtitel")
        element <- xml_children(element)[index]
        
        # Extract data
        c_raw <- element[1] |>
          xml_text() |>
          str_split("\n") |>
          unlist() |>
          str_remove("\r")
        c_raw
        
        # Create data to return
        tibble(
          `Suisa-Nummer` = c_raw[7],
          Filmtitel = c_raw[4],
          Datum = c_raw[1],
          typ = "Abrechnung Filmvorführungen",
          FileName = x
        )
      }
    }) |>
    bind_rows()
  
  # Verleiher suchen
  df_temp2 <- df_reports |>
    filter(str_detect(FileName, "Verleiher"))
  
  if (nrow(df_temp2) != 0) {
    df_temp2 <- df_temp2 |>
      pull() |>
      lapply(function(x) {
        doc <- read_html(paste0(c_path, x))
        # Find elements to edit
        element <- xml_find_first(doc, "body") |>
          xml_find_first("div")
        
        # Find all children of the node
        children <- xml_children(element)
        children <- children[[5]] |>
          xml_children()
        
        # Extract data
        c_raw <- xml_text(children[[2]])[1] |>
          str_split("\n", simplify = T)
        
        # Create data to return
        tibble(
          `Suisa-Nummer` = c_raw[, 7],
          Filmtitel = c_raw[, 8],
          Datum = c_raw[, 9],
          FileName = x
        )
      }) |>
      bind_rows() |>
      mutate(
        `Suisa-Nummer` = str_remove(`Suisa-Nummer`, "\r"),
        Filmtitel = str_remove(Filmtitel, "\r"),
        Datum = str_remove(Datum, "\r"),
        typ = "Verleiherabrechnung",
      )
  }
  
  df_temp2
  
  # create
  m_Film <- bind_rows(df_temp2, df_temp1, if (file.exists("output/Statistik.html")) {
    tibble(
      `Suisa-Nummer` = NA,
      Filmtitel = NA,
      Datum = NA,
      typ = "Statistik",
      FileName = "Statistik.html"
    )
  }, if (file.exists("output/Jahresrechnung.html")) {
    tibble(
      `Suisa-Nummer` = NA,
      Filmtitel = NA,
      Datum = NA,
      typ = "Jahresrechnung",
      FileName = "Jahresrechnung.html"
    )
  }, if (file.exists("output/Archiv.html")) {
    tibble(
      `Suisa-Nummer` = NA,
      Filmtitel = NA,
      Datum = NA,
      typ = "Archiv",
      FileName = "Archiv.html"
    )
  }, )
  m_Film
  
  m_Film <- m_Film |>
    mutate(Datum = dmy(Datum)) |>
    arrange(Datum) |>
    mutate(Datum = paste0(day(Datum), ".", month(Datum), ".", year(Datum)))
  m_Film
  
  # create site map
  if (TRUE) {
    # Was für Berichte typen sind vorhanden
    c_typ_Berichte <- m_Film$FileName |>
      str_extract(START %R% one_or_more(WRD)) |>
      factor() |>
      levels()
    c_typ_Berichte
    
    # Convert filenames to URL
    c_url <- paste0("file:///", URLencode(paste0(getwd(), "/output/", m_Film$FileName)), sep = "")
    c_url
    
    c_path <- paste0(getwd(), "/output/pict")
    c_path
    dir.create(c_path) |> suppressWarnings()
    
    # Vorschaubilder erzeugen wenn noch nicht vorhanden
    if (!(length(list.files("output/", "html")) == length(list.files("output/pict/")))) {
      create_icons(m_Film, c_path, c_url) 
    }
    
    # Einlesen template der Verleiherabrechnung
    c_raw <- readLines("source/Site_Map.Rmd")
    c_raw
    
    ii <- 1
    for (ii in 1:length(c_typ_Berichte)) {
      # Für jeden Bericht typ muss ein Bilde und Link eingefügt werden
      # Index where to insert
      c_index <- (1:length(c_raw))[c_raw |> str_detect(c_typ_Berichte[ii])]
      c_index <- c_index[length(c_index)]
      c_index
      
      c_raw
      c_raw[c_index]
      
      # Linkliste einfügen
      if (c_typ_Berichte[ii] == "Jahresrechnung") {
        c_select <- str_detect(m_Film$FileName,
                               START %R% c_typ_Berichte[ii] %R% DOT %R% "html")
        c_raw <- instert_picts(c_raw,
                               "output/pict/",
                               c_index,
                               m_Film$FileName[c_select],
                               c_url[c_select])
      } else{
        c_select <- str_detect(m_Film$FileName, START %R% c_typ_Berichte[ii])
        c_raw <- instert_picts(c_raw,
                               "output/pict/",
                               c_index,
                               m_Film$FileName[c_select],
                               c_url[c_select])
      }
      
      # Linkliste einfügen
      if (c_typ_Berichte[ii] == "Verleiherabrechnung") {
        for (jj in 1:length(m_Film$FileName[c_select])) {
          c_raw <- c(
            c_raw[1:(c_index)],
            paste0(
              "[",
              m_Film$FileName[c_select][jj],
              "](",
              c_url[c_select][jj],
              ")  ",
              m_Film$Filmtitel[jj],
              "  \\"
            ),
            c_raw[(c_index + 1):length(c_raw)]
          )
        }
        c_raw <- c(c_raw[1:(c_index + jj)], paste0("  \\"), c_raw[(c_index + jj + 1):length(c_raw)])
      }
      c_raw
      
      # Linkliste einfügen
      if (c_typ_Berichte[ii] == "Abrechnung") {
        for (jj in 1:length(m_Film$FileName[c_select])) {
          c_raw <- c(
            c_raw[1:(c_index)],
            paste0(
              "[",
              m_Film$FileName[c_select][jj],
              "](",
              c_url[c_select][jj],
              ")  ",
              m_Film$Filmtitel[jj],
              "  \\"
            ),
            c_raw[(c_index + 1):length(c_raw)]
          )
        }
        c_raw <- c(c_raw[1:(c_index + jj)], paste0("  \\"), c_raw[(c_index + jj + 1):length(c_raw)])
      }
      # Linkliste einfügen
      if (c_typ_Berichte[ii] == "Archiv") {
        for (jj in 1:length(m_Film$FileName[c_select])) {
          c_raw <- c(
            c_raw[1:(c_index)],
            paste0(
              "[",
              m_Film$FileName[c_select][jj],
              "](",
              c_url[c_select][jj],
              ")  ",
              m_Film$Filmtitel[jj],
              "  \\"
            ),
            c_raw[(c_index + 1):length(c_raw)]
          )
        }
        c_raw <- c(c_raw[1:(c_index + jj)], paste0("  \\"), c_raw[(c_index + jj + 1):length(c_raw)])
      }
    }
    c_raw
    
    # neues file schreiben
    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines("Site-Map.Rmd")
    
    # Render
    rmarkdown::render(input = "Site-Map.Rmd",
                      envir = data_env,
                      quiet = TRUE
    )
    # Remove file
    file.remove("Site-Map.Rmd")
    
  }
  
  # Data for Webserver
  # copy data from .../output to .../output/webserver
  c_path <- "output/webserver"
  if (!dir.exists(c_path)) {
    dir.create(c_path)
  }
  if (!dir.exists(paste0(c_path, "/pict"))) {
    dir.create(paste0(c_path, "/pict"))
  }
  
  # copy png
  paste0(
    getwd(),
    "/output/pict/",
    list.files(
      "output/pict/",
      pattern = "png",
      include.dirs = TRUE,
      recursive = FALSE
    )
  ) |>
    file.copy(paste0(c_path, "/pict"))
  
  
  if (TRUE) {
    m_Film$FileName <- m_Film$FileName
    
    # Was für Berichte typen sind vorhanden
    c_typ_Berichte <- m_Film$FileName |>
      str_extract(START %R% one_or_more(WRD)) |>
      factor() |>
      levels()
    c_typ_Berichte
    
    # Convert filenames to URL
    c_url <- paste0("", URLencode(m_Film$FileName))
    c_url
    
    # Einlesen template der Verleiherabrechnung
    c_raw <- readLines("source/Site_Map.Rmd")
    c_raw
    
    ii <- 1
    for (ii in 1:length(c_typ_Berichte)) {
      # Für jeden Bericht typ muss ein Bilde und Link eingefügt werden
      # Index where to insert
      c_index <- (1:length(c_raw))[c_raw |> str_detect(c_typ_Berichte[ii])]
      c_index <- c_index[length(c_index)]
      c_index
      
      c_raw
      c_raw[c_index]
      
      if (c_typ_Berichte[ii] == "Jahresrechnung") {
        c_select <- str_detect(m_Film$FileName,
                               START %R% c_typ_Berichte[ii] %R% DOT %R% "html")
      } else{
        c_select <- str_detect(m_Film$FileName, START %R% c_typ_Berichte[ii])
      }
      
      c_raw
      m_Film$FileName[c_select]
      c_url[c_select]
      
      c_raw <- instert_picts(c_raw, "pict/", c_index, m_Film$FileName[c_select], c_url[c_select])
      c_raw
      
      c_raw[c_index]
      
      
      # Linkliste einfügen
      if (c_typ_Berichte[ii] == "Verleiherabrechnung") {
        for (jj in 1:length(m_Film$FileName[c_select])) {
          c_raw <- c(
            c_raw[1:(c_index)],
            paste0(
              "[",
              m_Film$FileName[c_select][jj],
              "](",
              c_url[c_select][jj],
              ")  ",
              m_Film$Filmtitel[c_select][jj],
              "  \\"
            ),
            c_raw[(c_index + 1):length(c_raw)]
          )
        }
        c_raw <- c(c_raw[1:(c_index + jj)], paste0("  \\"), c_raw[(c_index + jj + 1):length(c_raw)])
      }
      
      # Linkliste einfügen
      if (c_typ_Berichte[ii] == "Abrechnung") {
        for (jj in 1:length(m_Film$FileName[c_select])) {
          c_raw <- c(
            c_raw[1:(c_index)],
            paste0(
              "[",
              m_Film$FileName[c_select][jj],
              "](",
              c_url[c_select][jj],
              ")  ",
              m_Film$Filmtitel[c_select][jj],
              "  \\"
            ),
            c_raw[(c_index + 1):length(c_raw)]
          )
        }
        c_raw <- c(c_raw[1:(c_index + jj)], paste0("  \\"), c_raw[(c_index + jj + 1):length(c_raw)])
      }
      c_raw
    }
    
    c_typ_Berichte[ii]
    c_raw
    
    # neues file schreiben
    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines("output/webserver/index.Rmd")
    
    # Render
    rmarkdown::render(input = "output/webserver/index.Rmd", envir = data_env, quiet = TRUE)
    # Remove file
    file.remove("output/webserver/index.Rmd")
    # Remove directory
    unlink(paste0(c_path, "/pict"), recursive = TRUE)
    
  }
  
  # edit html
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
    element <- xml_find_first(doc, "body") |>
      xml_find_first("div")
    
    # Find all children of the parent node
    children <- xml_children(element)
    
    # Insert Node
    xml_add_child(children[[1]],
                  paste0("a href=\"", URLencode(paste0("index.html")), "\""),
                  "Site-Map")
    write_xml(doc, file_path)
  }
  
  #copy data from .../output to .../output/webserver
  c_path <- "output/webserver"
  
  # copy html
  paste0(
    "output/",
    list.files(
      "output/",
      pattern = "html",
      include.dirs = FALSE,
      recursive = FALSE
    )
  ) |>
    file.copy(paste0(c_path, ""), overwrite = TRUE)
  
  c_files <- list.files(
    "output/webserver/",
    pattern = "html" %R% END,
    include.dirs = FALSE,
    recursive = FALSE
  )
  c_files <- paste0("output/webserver/", c_files)
  
  # apply Site-Map link
  c_files |>
    lapply(add_SiteMapLink)
  
  # remove files
  file.remove("Site-Map.html")
  
}

# Envirnoment for Data to create Plots
data_env <- new.env()
error_calculate <-  paste0(
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
  "! Es konnten nicht alle Daten einlesen werden. !\n",
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  )

# Initialize variables
# Initialize variables
calculate_warnings <- ""  # Store warnings
ausgabe_text <- ""        # Store script output
startup_error <- FALSE
error_message <- ""       # Store errors

# Try executing the source file
tryCatch({
  # Capture output separately from errors
  ausgabe_text <- capture.output({
    withCallingHandlers({
      # Use `tryCatch` to capture errors inside `source()`
      tryCatch({
        source("source/calculate.R", local = data_env)
      }, error = function(e) {
        error_message <<- paste("Fehler beim Dateneinlesen:", conditionMessage(e))
        startup_error <<- TRUE
      })
    }, warning = function(w) {
      # Capture warnings in a vector
      calculate_warnings <<- c(calculate_warnings, paste("Warning:", w$message))
      invokeRestart("muffleWarning")  # Suppress warnings
    })
  }, type = "output")  # Captures standard output & messages
  
}, error = function(e) {
  # Capture any error at this level (outside of `source()`)
  error_message <<- paste("Global Error:", conditionMessage(e))
  startup_error <<- TRUE
})

# Ensure everything is formatted properly
warnings_text <- if (length(calculate_warnings) > 0) paste(calculate_warnings, collapse = "\n") else ""
output_text <- if (length(ausgabe_text) > 0) paste(ausgabe_text, collapse = "\n") else ""

# Combine all results
final_output <- paste(
  if (nchar(error_message) > 0) error_message else "No Errors.",
  if (nchar(warnings_text) > 0) warnings_text else "No Warnings.",
  if (nchar(output_text) > 0) output_text else "No Output.",
  sep = "\n"
)

# concatenate feedback
ausgabe_text <- final_output
ausgabe_text

# include some function into data_env
data_env$r_is.defined <- r_is.defined
data_env$round5Rappen <- round5Rappen

# Shiny reactive variables
calculate_warnings <- shiny::reactiveVal(as.character(calculate_warnings))
ausgabe_text <- shiny::reactiveVal(as.character(ausgabe_text))

# Sollen Inhaltsverzeichnisse erstellt werden
toc <- shiny::reactiveVal(TRUE)

# # Ausgabeformate
# c_render_option <- shiny::reactiveVal("1")

# Vektor mit Datumseinträgen
if (exists("df_show", envir = data_env))  {
  datum_vektor <- data_env$df_show$Datum
} else {
  datum_vektor <- seq(as.Date(paste0(Abrechungsjahr, "-01-01")), as.Date(paste0(Abrechungsjahr, "-12-31")), by = "day")
}

# Filmtabelle anzeigen
df_Render <- shiny::reactiveVal(NULL)

# Datum Auswahl für Abrechnung Filmvorführung (Finde letztes Datum)
End_date_choose <- shiny::reactiveVal(Sys.Date() + ((max(datum_vektor) - Sys.Date()) |> as.integer()))

# Does the index.html file exist, is the webserver ready
file_exists <- shiny::reactiveVal(file.exists("output/webserver/index.html"))

# Serve the custom_styles directory
shiny::addResourcePath("custom_styles", "source")

# Map the URL path "custom" to the local directory "output/webserver"
# Webserver root directory
if (!dir.exists("output/webserver")) {
  dir.create("output/webserver", recursive = TRUE)
}
shiny::addResourcePath("reports", "output/webserver")


# UI-Definition fluid page
ui <- function(){
  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles/Kinoklub_dark_gui.css")
    ),
    paste("Kinoklub GUI", c_script_version) |>
      shiny::titlePanel(),
    shiny::sidebarLayout(
      # Render the side panel
      shiny::sidebarPanel(
        shiny::uiOutput("dynamicContent_input_panel")
      ),
      # Render the main panel
      shiny::mainPanel(
        shiny::uiOutput("dynamicContent_output_panel")
      )
    )
  )
}

# # UI-Definition bs4Dash
# library(bs4Dash)
# ui <- dashboardPage(
#   help = TRUE,
#   dark = TRUE,  # Force dark mode
#   dashboardHeader(title = paste("Kinoklub GUI", c_script_version)),
#   dashboardSidebar(shiny::uiOutput("dynamicContent_input_panel")),
#   dashboardBody(shiny::uiOutput("dynamicContent_output_panel")),
#   controlbar = dashboardControlbar(
#     id = "controlbar",
#     skin = "dark",
#     controlbarMenu(
#       id = "controlbarMenu",
#       controlbarItem(
#         title = "Help",
#         icon = icon("question-circle"),
#         p("This is a custom help section.")
#       )
#     )
#   )
# )

# Server-Logik
server <- function(input, output, session) {
  # Überwachung Button: open Excel Einkauf
  shiny::observeEvent(input$open_einkauf, {
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Step", 1, "of 2"))
      #ausgabe_text("Die Excel-Datei Einkauf Kiosk wurde geöffnet.")

      c_file <- list.files(path = "Input")
      c_file <- c_file[str_detect(c_file, "Einkauf")]
      # take the latest date
      if (length(c_file) > 1) {
        df_temp <- tibble(file = c_file, date = dmy(c_file)) |>
          arrange(date)

        c_file <- df_temp$file[nrow(df_temp)]
      }

      file_path <- paste0(getwd(), "/Input/", c_file)  # Update this with your actual file path
      if (file.exists(file_path)) {
        tryCatch({
          # Warnings abfangen
          capture.output({
            shell.exec(file_path)  # Opens the file in Excel
          }, type = "message")
        }, error = function(e) {
          # Fehler abfangen
          ausgabe_text(e$message)
        })
      } else {
        showModal(
          modalDialog(
            title = "Error",
            "File not found! Check the file path.",
            easyClose = TRUE
          )
        )
      }
      shiny::incProgress(2 / 2, detail = paste("Step", 2, "of 2"))
    })
  })

  # Überwachung Button: open Excel Einnahmen und Ausgaben
  shiny::observeEvent(input$open_EinAus, {
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Step", 1, "of 2"))
      #ausgabe_text("Die Excel-Datei Einnahmen und Ausganben wurde geöffnet.")
      c_file <- list.files(path = "Input")
      c_file <- c_file[str_detect(c_file, "Einnahmen")]
      file_path <- paste0(getwd(), "/Input/", c_file)
      if (file.exists(file_path)) {
        tryCatch({
          # Warnings abfangen
          capture.output({
            shell.exec(file_path)  # Opens the file in Excel
          }, type = "message")
        }, error = function(e) {
          # Fehler abfangen
          ausgabe_text(e$message)
        })
      } else {
        showModal(
          modalDialog(
            title = "Error",
            "File not found! Check the file path.",
            easyClose = TRUE
          )
        )
      }
      shiny::incProgress(1 / 2, detail = paste("Step", 2, "of 2"))
    })
  })

  # Überwachung Button: open Excel Spezialpreise
  shiny::observeEvent(input$open_Spez, {
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Step", 1, "of 2"))
      #ausgabe_text("Die Excel-Datei Spezialpreise wurde geöffnet.")
      c_file <- list.files(path = "Input")
      c_file <- c_file[str_detect(c_file, "Spezial")]

      file_path <- paste0(getwd(), "/Input/", c_file)
      if (file.exists(file_path)) {
        tryCatch({
          # Warnings abfangen
          capture.output({
            shell.exec(file_path)  # Opens the file in Excel
          }, type = "message")
        }, error = function(e) {
          # Fehler abfangen
          ausgabe_text(e$message)
        })
      } else {
        showModal(
          modalDialog(
            title = "Error",
            "File not found! Check the file path.",
            easyClose = TRUE
          )
        )
      }
      shiny::incProgress(2 / 2, detail = paste("Step", 2, "of 2"))
    })
  })

  # Überwachung Button: open Excel Verleiherabgaben Excel
  shiny::observeEvent(input$open_Verleih, {
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Step", 1, "of 2"))
      #ausgabe_text("Die Excel-Datei Verleiherabgaben wurde geöffnet.")
      c_file <- list.files(path = "Input")
      c_file <- c_file[str_detect(c_file, "Verleiher")]

      file_path <- paste0(getwd(), "/Input/", c_file)
      if (file.exists(file_path)) {
        tryCatch({
          # Warnings abfangen
          capture.output({
            shell.exec(file_path)  # Opens the file in Excel
          }, type = "message")
        }, error = function(e) {
          # Fehler abfangen
          ausgabe_text(e$message)
        })
      } else {
        showModal(
          modalDialog(
            title = "Error",
            "File not found! Check the file path.",
            easyClose = TRUE
          )
        )
      }
      shiny::incProgress(2 / 2, detail = paste("Step", 2, "of 2"))
    })
  })

  # Überwachung Button Daten Einlesen
  shiny::observeEvent(input$DatenEinlesen, {
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 3, detail = paste("Step", 1, "of 3"))
      # Execution time
      c_time <- Sys.time()
      ausgabe_text("Dateien wurden eingelesen.\n")
      calculate_warnings("")

      # # read data
      # tryCatch({
      #   # Fehler abfangen
      #   ausgabe_text(capture.output({
      #     withCallingHandlers(
      #       {
      #         source("source/calculate.R", local = data_env)
      #         shiny::incProgress(1 / 2, detail = paste("Step", 2, "of 3"))
      #       },
      #       warning = function(w) {
      #         # Capture warnings and store them in calculate_warnings
      #         calculate_warnings(paste(calculate_warnings(), "Warning:", w$message, sep = ""))
      #         invokeRestart("muffleWarning")  # Suppress the warning from being printed
      #       }
      #     )
      #   }, type = "message"))
      # }, error = function(e) {
      #   ausgabe_text(
      #       paste0(
      #         error_calculate,
      #         e$message,
      #         collapse = ""
      #       )
      #     )
      # })
      
      # Initialize variables
      calculate_warnings <- ""  # Store warnings
      error_message <- ""       # Store errors 
      
      # Try executing the source file
      tryCatch({
        # Capture output separately from errors
        capture.output({
          withCallingHandlers({
            # Use `tryCatch` to capture errors inside `source()`
            tryCatch({
              source("source/calculate.R", local = data_env)
            }, error = function(e) {
              error_message <<- paste("Fehler beim Dateneinlesen:", conditionMessage(e))
              startup_error <<- TRUE
            })
          }, warning = function(w) {
            # Capture warnings in a vector
            calculate_warnings <<- c(calculate_warnings, paste("Warning:", w$message))
            invokeRestart("muffleWarning")  # Suppress warnings
          })
        }, type = "output")|>  # Captures standard output & messages
          ausgabe_text() 
      }, error = function(e) {
        # Capture any error at this level (outside of `source()`)
        error_message <<- paste("Global Error:", conditionMessage(e))
        
      })
      
      # Ensure everything is formatted properly
      warnings_text <- if (length(calculate_warnings) > 0) paste(calculate_warnings, collapse = "\n") else ""
      output_text <- if (length(ausgabe_text) > 0) paste(ausgabe_text(), collapse = "\n") else ""
      
      # Combine all results
      final_output <- paste(
        if (nchar(error_message) > 0) error_message else "No Errors.",
        if (nchar(warnings_text) > 0) warnings_text else "No Warnings.",
        if (nchar(output_text) > 0) output_text else "No Output.",
        sep = "\n"
      )
      
      End_date_choose(data_env$df_Abrechnung$Datum|>max())

      shiny::incProgress(1 / 3, detail = paste("step", 3, "of 3"))
      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",final_output)|>
        ausgabe_text()
    })
  })

  # Überwachung Button Filmabrechnung(en) erstellen
  shiny::observeEvent(input$Abrechnung, {
    # Execution time
    c_time <- Sys.time()
    if(!is.null(data_env$df_Abrechnung)){
      shiny::withProgress(message = "Script running... ", value = 0, {
        shiny::incProgress(1 / 4, detail = paste("Filmabrechnungen", 1, "of 4"))
        ausgabe_text("")
        start_datum <- input$dateRange |> min()
        end_datum <- input$dateRange |> max()

        # Überprüfen, ob beide Daten gültig sind
        if (start_datum <= end_datum) {
          # Aktion ausführen
          ausgabe_text(
            paste0(
              "Die Filmabrechnungen für den Zeitraum \n",
              format(start_datum, "%d.%m.%Y"),
              " bis ",
              format(end_datum, "%d.%m.%Y"),
              " wurden erstellt",
              paste0("\n", getwd(), "/output")
            )
          )

          # Filmabrechnungen erstellen mit dateRange user input
          tryCatch({
            df_mapping__ <-
              Abrechnung_mapping(
                data_env,
                start_datum, end_datum
              )
            AbrechnungErstellen(
              df_mapping__,
              data_env$df_Abrechnung,
              toc = toc()
            )
            shiny::incProgress(1 / 4, detail = paste("Verleiherabrechnung erstellen: ", 2, "of 4"))
            VerleiherabrechnungErstellen(
              df_mapping__,
              data_env$df_Abrechnung,
              toc = toc()
            )
            shiny::incProgress(1 / 4, detail = paste("Site-map erstellen: ", 3, "of 4"))
            webserver()

          }, error = function(e) {
            ausgabe_text(
              paste0(
                "Filmabrechnungen erstellen, Fehler beim Bericht erstellen:\n",
                e$message
              )
            )
          })
        } else {
          ausgabe_text("Das Enddatum darf nicht vor dem Startdatum liegen.")
        }
        file_exists(file.exists("output/webserver/index.html"))

        # calculate execution time
        c_time <- c(c_time,end = Sys.time())|>
          diff()
        paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
          ausgabe_text()

        shiny::incProgress(1 / 4, detail = paste("Step", 4, "of 4"))
      })
    }else{
      paste0("Es sind kein Daten vorhanden. Dateien wurden noch nicht eingelesen!\n",
             "Bitte Dateien einlesen und nochmals versuchen.")|>
        ausgabe_text()

      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
        ausgabe_text()
    }
  })

  # Überwachung Button Statistik
  shiny::observeEvent(input$Statistik, {
    # Execution time
    c_time <- Sys.time()
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 5, detail = paste("Step", 1, "of 5"))
      # User feedback
      ausgabe_text(paste0(
        "Bericht: Statistik erstellt",
        paste0("\n", getwd(), "/output")
      ))
      if (exists("data_env")) {
        tryCatch({

          StatistikErstellen(toc())
          shiny::incProgress(1 / 5, detail = paste("Step", 2, "of 5"))
          webserver()
          shiny::incProgress(1 / 5, detail = paste("Step", 3, "of 5"))
        }, error = function(e) {
          ausgabe_text(paste(
            "Statistik, Fehler beim Bericht erstellen:\n",
            e$message
          ))
        })
      } else{
        ausgabe_text(
          "Statistik kann nicht erstellte werden.\nKeine Daten vorhanden bitte neu einlesen!!!!"
        )
      }
      shiny::incProgress(1 / 5, detail = paste("Step", 4, "of 5"))
      file_exists(file.exists("output/webserver/index.html"))

      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
        ausgabe_text()

      shiny::incProgress(1 / 5, detail = paste("Step", 5, "of 5"))
    })

  })

  # Überwachung Button Jahresrechnung
  shiny::observeEvent(input$Jahresrechnung, {
    shiny::withProgress(message = "Running script...", value = 0, {
      # Execution time
      c_time <- Sys.time()
      shiny::incProgress(1 / 5, detail = paste("Step", 1, "of 5"))
      # User feedback
      paste0("Bericht: Jahresrechnung erstellt",
             paste0("\n", getwd(), "/output")) |>
        ausgabe_text()
      if (exists("data_env")) {
        tryCatch({
          shiny::incProgress(1 / 5, detail = paste("Step", 2, "of 5"))
          JahresrechnungErstellen(toc())
          shiny::incProgress(1 / 5, detail = paste("Step", 3, "of 5"))
          webserver()
        }, error = function(e) {
          ausgabe_text(paste(
            "Jahresrechnung, Fehler beim Bericht erstellen:\n",
            e$message
          ))
        })
      } else{
        ausgabe_text(
          "Jahresrechnung kann nicht erstellte werden.\nKeine Daten vorhanden bitte neu einlesen!!!!"
        )
      }
      shiny::incProgress(1 / 5, detail = paste("Step", 4, "of 5"))
      file_exists(file.exists("output/webserver/index.html"))

      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      shiny::incProgress(1 / 5, detail = paste("Step", 5, "of 5"))

      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
        ausgabe_text()
    })
  })

  # Download Handler Werbung
  output$downloadExcel <- downloadHandler(
    filename = function() {
      "Werbung.xlsx"
    },
    content = function(file) {
      write.xlsx(
        data_env$df_Besucherzahlen,
        file = file,
        asTable = TRUE,
        overwrite = TRUE
      )
    }
  )

  # Überwachung Button Wordpress
  shiny::observeEvent(input$wordpress, {
    shiny::withProgress(message = "Running script...", value = 0, {
      # Execution time
      c_time <- Sys.time()
      shiny::incProgress(1 / 5, detail = paste("Step", 1, "of 5"))
      paste0(
        "Filmumfrage, Wordpress daten auswertung ausgeführt.",
        "\nDie Exceldatei kann jetzt heruntergeladen werden."
      ) |>
        ausgabe_text()

      # read WordPress and procinema data and create excel file for Kinoprogramm
      tryCatch({
        source("source/procinema.R", local = WordPress_env)
        shiny::incProgress(1 / 5, detail = paste("Step", 2, "of 5"))
        source("source/read_and_convert_wordPress.R", local = WordPress_env)
        shiny::incProgress(1 / 5, detail = paste("Step", 3, "of 5"))
        FilmvorschlagErstellen(toc(), WordPress_env)
        shiny::incProgress(1 / 5, detail = paste("Step", 4, "of 5"))
        webserver()
      }, error = function(e) {
        ausgabe_text(paste(
          "Filmvorschläge, Fehler beim Bericht erstellen:\n",
          e$message
        ))
      })

      file_exists(file.exists("output/webserver/index.html"))

      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
        ausgabe_text()

      shiny::incProgress(1 / 5, detail = paste("Step", 5, "of 5"))
    })
  })

  # Überwachung Button "Alles erstellen"
  shiny::observeEvent(input$ErstelleAbrechnung, {
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 10, detail = paste("Step", 1, "of 10"))
      # Execution time
      c_time <- Sys.time()
      # User interaction
      "Alles wurde neu erstellt" |>
        ausgabe_text()
      calculate_warnings("")

      # Delete all files prior to creating new files
      list.files("output/", "html", full.names = TRUE) |>
        file.remove()
      list.files("output/pict/", "html", full.names = TRUE) |>
        file.remove()

      # run script calculate.R to finde error spezifcally happening with only this source
      tryCatch({
        # erstellen von Verzeichnissen
        dir.create("output/") |> suppressWarnings()
        dir.create("output/data/") |> suppressWarnings()

        # Daten einlesen und konvertieren
        source("source/calculate.R", local =  data_env)


      }, error = function(e) {
        calculate_warnings("error")
        paste0(
          error_calculate,
          "Alles neu erstellen Fehlermeldung:\n",
          "Daten konnten nicht eingelesen werden. Fehlermeldung: ",
          e$message
        )|>
          ausgabe_text()
      })
      # run the rest of the script
      if(calculate_warnings() == ""){
        tryCatch({
          # Statistik-Bericht erstellen
          StatistikErstellen(toc())
          shiny::incProgress(1 / 10, detail = paste("Step", 2, "of 10"))

          # Jahresrechnung-Bericht erstellen
          JahresrechnungErstellen(toc())
          shiny::incProgress(1 / 10, detail = paste("Step", 3, "of 10"))

          # Bericht(e) Abrechnung pro Filmforführung erstellen
          df_mapping__ <-
            Abrechnung_mapping(
              data_env,
              start = paste0(Abrechungsjahr,"-1-1")|>as.Date(),
              end = paste0(Abrechungsjahr,"-12-31")|>as.Date()
            )
          AbrechnungErstellen(
            df_mapping__,
            data_env$df_Abrechnung,
            toc = toc()
          )
          shiny::incProgress(1 / 10, detail = paste("Step", 4, "of 10"))
          VerleiherabrechnungErstellen(
            df_mapping__,
            data_env$df_Abrechnung,
            toc = toc()
          )
          shiny::incProgress(1 / 10, detail = paste("step", 5, "of 10"))

          # Procinema
          source("source/procinema.R", local = WordPress_env)
          shiny::incProgress(1 / 10, detail = paste("step", 6, "of 10"))
          # Wordpress
          source("source/read_and_convert_wordPress.R", local = WordPress_env)
          shiny::incProgress(1 / 10, detail = paste("step", 7, "of 10"))

          FilmvorschlagErstellen(toc(), WordPress_env)
          shiny::incProgress(1 / 10, detail = paste("step", 8, "of 10"))

          # Create webserver data
          webserver()
          shiny::incProgress(1 / 10, detail = paste("step", 9, "of 10"))

        }, error = function(e) {
          ausgabe_text(paste(
            "Alles neu erstellen Fehlermeldung:\n",
            e$message
          ))
        })
      }
      End_date_choose(Sys.Date() + ((max(datum_vektor) - Sys.Date()) |> as.integer()))
      file_exists(file.exists("output/webserver/index.html"))

      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
        ausgabe_text()

      shiny::incProgress(1 / 10, detail = paste("Step", 10, "of 10"))
    })
  })

  # Download Handler Wordpress
  output$downloadWordPress <- downloadHandler(
    filename = function() {
      "Filmvorschläge.xlsx"
    },
    content = function(file) {
      source_file <- "output/data/Filmvorschläge.xlsx"
      # Check if the file exists before attempting to copy
      if (file.exists(source_file)) {
        file.copy(from = source_file,
                  to = file,
                  overwrite = TRUE)
      } else {
        stop("The file does not exist.")
      }
    }
  )

  # Überwachung Input: Inhaltsverzeichniss
  shiny::observeEvent(input$Inhaltsverzeichnis, {
    print(clc)
    toc(input$Inhaltsverzeichnis)
    print(toc())
    file_exists(file.exists("output/webserver/index.html"))
  })

  # Upload handler
  file_data <- shiny::reactive({
    shiny::req(input$file)
    file_path <- input$file$datapath
    file_name <- input$file$name                  # Get file name
    file_ext <- tools::file_ext(input$file$name)  # Get file extension

    if (file_ext == "xlsx") {
      # save xlsx files
      # Define save path
      save_path <- paste0("Input/", file_name)
      # Save the file to the specified directory
      file.copy(from = file_path,
                to = save_path,
                overwrite = TRUE)

      # user interaction
      paste0(
        "Die Datei \"",
        file_name,
        "\" wurde eingelesen und im Verzeichniss ",
        "\n",getwd(),"/Kinoklub",
        save_path,
        " abgespeichert"
      ) |>
        ausgabe_text()

      # Read all sheet names
      sheet_names <- openxlsx::getSheetNames(save_path)
      return(list(
        type = "xlsx",
        path = save_path,
        sheets = sheet_names
      ))

    } else if (file_ext == "txt") {
      # save txt files
      if (file_name == "Procinema.txt" |
          file_name == "procinema.txt") {
        # save Procinema.txt file
        # Define save path
        save_path <- paste0("Input/Procinema/")
        # remove file
        list.files(save_path, full.names = TRUE) |>
          file.remove()
        # Define save path
        save_path <- paste0("Input/Procinema/", tolower(file_name))
        # Save the file to the specified directory
        file.copy(from = file_path,
                  to = save_path,
                  overwrite = TRUE)
        # user interaction
        paste0(
          "Die Datei \"",
          file_name,
          "\" wurde eingelesen und im Verzeichniss \n.../Kinoklub",
          save_path,
          " abgespeichert"
        ) |>
          ausgabe_text()
        return(list(type = "txt", data = readLines(file_path)))
      } else{
        # save all other txt files
        # Define save path
        save_path <- paste0("Input/advance tickets/", file_name)
        # Save the file to the specified directory
        file.copy(from = file_path,
                  to = save_path,
                  overwrite = TRUE)
        # user interaction
        paste0(
          "Die Datei \"",
          file_name,
          "\" wurde eingelesen und im Verzeichniss \n.../Kinoklub",
          save_path,
          " abgespeichert"
        ) |>
          ausgabe_text()
        return(list(type = "txt", data = readLines(file_path)))
      }
    } else if (file_ext == "csv") {
      # save csv files (WordPress input)
      # Define save path
      save_path <- paste0("Input/WordPress/")
      # remove file
      list.files(save_path, full.names = TRUE) |>
        file.remove()
      # Define save path
      save_path <- paste0("Input/WordPress/", file_name)
      # Save the file to the specified directory
      file.copy(from = file_path, to = save_path)
      # user interaction
      paste0(
        "Die Datei \"",
        file_name,
        "\" wurde eingelesen und im Verzeichniss \n.../Kinoklub",
        save_path,
        " abgespeichert"
      ) |>
        ausgabe_text()
      return(list(type = "csv", data = readLines(file_path)))
    } else {
      paste0(
        "Dateierweiterung: ",
        file_ext,
        " ist nicht bekannt und wird von diesem Script nicht verwendet.",
        "\nDatei wurde \"",
        file_name,
        "\" wurde nicht gespeichert."
      ) |>
        ausgabe_text()
      return(NULL)
    }
  })

  # Read selected sheet data
  selected_data <- shiny::reactive({
    shiny::req(file_data(), input$selected_sheet)
    col_env$get_excel_data(file_data()$path)[[input$selected_sheet]]
  })

  # Reder: Update table with all the dates in the selected range
  output$dateTable <- shiny::renderTable({
    if (exists("data_env")) {
      start_datum <- input$dateRange |> min()
      end_datum <- input$dateRange |> max()

      get("df_Abrechnung", envir = data_env) |>
        filter(between(Datum, start_datum, end_datum)) |>
        arrange(desc(Datum), desc(Anfang)) |>
        mutate(Datum = format(Datum, "%d.%m.%Y"),
               Zeit = format(Anfang, "%H%M")) |>
        select(Datum, Zeit, Filmtitel, `Suisa Nummer`)
    }
  })

  # Render: txt file rendering
  output$text_output <- shiny::renderPrint({
    shiny::req(file_data()$type %in% c("txt", "csv"))
    file_data()$data |>
      writeLines()
  })

  # Render: dynamic sheet selection UI
  output$sheet_selector <- shiny::renderUI({
    shiny::req(file_data())
    shiny::selectInput("selected_sheet",
                       "Excel Blatt auswählen:",
                       choices = file_data()$sheets)
  })

  # Render: Systemrückmeldungen aktualisieren
  output$ausgabe <- renderText({
    ausgabe_text()
  })

  # Render: selected sheet contents
  output$table_output <- shiny::renderTable({
    shiny::req(selected_data())
    selected_data()
  })

  # Render: Dynamically update the input panel content
  output$dynamicContent_input_panel <- shiny::renderUI({
    shiny::tagList(
      # File input handler
      shiny::fileInput(
        "file",
        "Datei hochladen:",
        accept = c(".xlsx", ".txt"),
        multiple = FALSE,
        placeholder = "Drag & drop or browse a file"
      ),
      shiny::uiOutput("sheet_selector"),

      # Button Daten Einlesen
      shiny::actionButton("DatenEinlesen", "Dateien einlesen"),
      shiny::tags$hr(),
      # Add tooltips using shinyBS
      shinyBS::bsTooltip(
        id = "DatenEinlesen",
        title = "Es werden alle Dateien im Ordner .../Kinoklub/input eingelesen.",
        placement = "right",
        trigger = "hover"
      ),

      # Datumsbereich auswählen für die Abrechnung Filmvorführungen
      shiny::dateRangeInput(
        inputId = "dateRange",
        label = "Wählen Sie einen Datumsbereich aus:",
        start = End_date_choose(),
        # Default start date (one week ago)
        end = End_date_choose(),
        # Default end date (last show)
        min = min(datum_vektor),
        # Earliest selectable date
        max = End_date_choose(),
        # Latest selectable date
        format = "dd.mm.yyyy",
        # Set input format to German (DD.MM.YYYY)
        separator = " bis " # Separator for the two dates in German
      ),

      # Button zum Ausführen von Code Filmabrechnunge(n) erstellen
      shiny::actionButton("Abrechnung", "Filmabrechnung(en) erstellen"),
      # Add tooltips using shinyBS
      shinyBS::bsTooltip(
        id = "Abrechnung",
        title = "Es werden die Filmabrechnungen im gewählten Datumsbereich erstellt.",
        placement = "right",
        trigger = "hover"
      ),
      shiny::tags$hr(),

      # Button zum Ausführen von Code Statistik erstellen
      shiny::actionButton("Statistik", "Statistik erstellen"),

      # Button zum Ausführen von Code Jahresrechnung erstellen
      shiny::actionButton("Jahresrechnung", "Jahresrechnung erstellen"),
      shiny::tags$hr(),

      # Button zum Download der Werbung
      shiny::downloadButton("downloadExcel", "Download Werbung"),
      shiny::tags$hr(),

      # Button zum Ausführen von Code Filmumfrage Wordpress auswerten
      shiny::actionButton("wordpress", "Wordpress auswerten"),
      shiny::downloadButton("downloadWordPress", "Download Filmvorschläge"),
      shiny::tags$hr(),

      # Button zum Ausführen von Code Alles erstellen mit Webserver
      shiny::actionButton("ErstelleAbrechnung", "Alles neu erstellen"),
      # Add tooltips using shinyBS
      shinyBS::bsTooltip(
        id = "ErstelleAbrechnung",
        title = "Achtung die Ausführung kann viel Zeit in anspruchnehmen!",
        placement = "right",
        trigger = "hover"
      ),
      shiny::tags$hr(),

      # Inhaltsverzeichnis
      shiny::selectInput(
        inputId = "Inhaltsverzeichnis",
        label = "Inhaltsverzeichnis erstellen?",
        choices = list("Ja" = TRUE, "Nein" = FALSE),
        selected = TRUE # Default value
      ),

      # # Ausgabeformat
      # shiny::selectInput(
      #   inputId = "render_option",
      #   label = "Ausgabeformat wählen:",
      #   choices = list(
      #     "HTML" = "1",
      #     "DOCX" = "2",
      #     "PDF" = "3",
      #     "HTML and DOCX" = "4",
      #     "HTML and PDF" = "5",
      #     "DOCX and PDF" = "6",
      #     "HTML, DOCX, and PDF" = "7"
      #   ),
      #   selected = "1" # Default value
      # ),
      # Add tooltips using shinyBS
      # shinyBS::bsTooltip(
      #   id = "render_option",
      #   title = "PDF options require LaTeX installation (e.g., MikTeX for Windows, MacTeX for Mac).",
      #   placement = "right",
      #   trigger = "hover"
      # ),
    )
  })

  # Render: Dynamically update the output panel content
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      shiny::actionButton("open_einkauf", "Einkauf Kiosk"),
      shiny::actionButton("open_EinAus", "Einnahmen und Ausgaben"),
      shiny::actionButton("open_Spez", "Spezialpreise"),
      shiny::actionButton("open_Verleih", "Verleiherabgaben"),
      if (file_exists()) {
        shiny::tags$h4("Berichte:")
      },
      if (file_exists()) {
        shiny::tags$a(href = "reports/index.html", "Site-map",
                      target = "_blank",
                      style = "font-size: 24px;")
      },
      shiny::tags$h4("Filme in der gewählten Periode"),
      if(!startup_error)shiny::tableOutput("dateTable"),
      shiny::tags$h4("Systemrückmeldungen"),
      shiny::verbatimTextOutput("ausgabe"),
      shiny::tags$hr(),
      shiny::tags$h4("Inhalt der hochgeladen Datei:"),
      shiny::tableOutput("table_output"),
      shiny::verbatimTextOutput("text_output")
    )

  })
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5000,
  # Replace 8080 with your desired port
  launch.browser = TRUE # Automatically open in the system's default browser
)
