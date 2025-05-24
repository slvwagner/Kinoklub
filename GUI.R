# Graphical user interface für den Kinoklub ####
# Diese App kann mit Run App in Rstudio gestartet werden.

# Vorbereiten / Installieren
rm(list = ls())

# Define libraries to be installed
packages <- c(
  "rmarkdown",  "rebus",  "openxlsx",  "tidyverse",
  "lubridate",  "DT", "magick",  "webshot",  "xml2",  "furrr", "future", "processx","RMySQL",
  "shiny",  "shinyjs", "viridis", "colorspace"
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
if(!file.exists("user_settings.R")) {
  stop("Missing required file: user_settings.R")
}
# user settings / documentation
source("user_settings.R")
# Functions
source("source/functions.R")
source("source/SQL/SQL_Functions.R")

# connect to data base ####

## Data base credentials from system variables ####
DB_host <- Sys.getenv("DB_host")
DB_name <- Sys.getenv("DB_name")
DB_user <- Sys.getenv("DB_user")
DB_pw <- Sys.getenv("DB_PASSWORD_KINOKLUB")

## Connection ####
con <- DB_connect(DB_host, DB_name, DB_user, DB_pw)

# read template
l_template <- readRDS("source/SQL/template.RDS")

# create environment to run WordPress scripts
WordPress_env <- new.env()

# Erstellen von Verzeichnissen ####
dir.create("output/", showWarnings = FALSE, recursive = TRUE)
dir.create("output/data/", showWarnings = FALSE, recursive = TRUE) 

## Error if calculation not executing ####
error_calculate <-  paste0("\n",
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
  "! Es konnten nicht alle Daten einlesen werden. !\n",
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  )

# calculate.R ####
data_env <- new.env()

# # concatenate feedback 
# ausgabe_text <- paste0(calculate_warnings, ausgabe_text, collapse = "\n")
ausgabe_text <- ""
startup_error <<- FALSE

# Error handling
if(str_detect(ausgabe_text, pattern = error_calculate)) stop(ausgabe_text)


# include some function into data_env
data_env$r_is.defined <- r_is.defined
data_env$round5Rappen <- round5Rappen

# Serve the custom_styles directory
shiny::addResourcePath("custom_styles", "source")

# Map the URL path "custom" to the local directory "output/webserver"
# Webserver root directory
if (!dir.exists("output/webserver")) {
  dir.create("output/webserver", recursive = TRUE)
}
shiny::addResourcePath("reports", "output/webserver")

# Constance ####
c_lengthMenu = c(5:20, 50, 100) # page length drop down options

# UI-Definition fluid page ####
ui <- 
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

# # UI-Definition bs4Dash ####
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
 
# Server-Logik ####
server <- function(input, output, session) {
  ## Helper functions ####
  ### Function to create icons for the site map #####
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
  
  ### Function to render a single RMarkdown file ####
  render_single_file <- function(input, output, envir) {
    rmarkdown::render(
      input = input,        # input file name
      output_file = output, # output file name
      output_dir = "output",# where to put the output file (directory) 
      envir = envir, 
      quiet = TRUE  # Suppress output for cleaner logs
    )
  }
  
  ### Index pro Suisa-Nummer und Datum erstellen ####
  Abrechnung_mapping <- function(data_env, start, end, ...) {
    # Soll die Verleiherabrechnung erzeugt werden?
    df_mapping <- data_env$df_Abrechnung |>
      select(`Event ID`, Datum , Zeit, Suisanummer, Filmtitel, `Kinoförderer gratis?`)|>
      mutate(user_Datum = format(Datum, "%d.%m.%Y"))|>
      filter(between(Datum, as.Date(start), as.Date(end)))
    
    if(!is.null(...)){
      df_mapping <- df_mapping|>
        filter(`Event ID` %in% ...)
    }
    
    
    if(nrow(df_mapping) > 0){
      df_mapping <- df_mapping|>
        mutate(fileName_RMD            = paste0("source/Abrechnung ID",`Event ID`,".Rmd"),
               fileName_html           = paste0("source/Abrechnung ID",`Event ID`,".html"),
               fileName_RMD_Verleiher  = paste0("source/Verleiherabrechnung ID",`Event ID`,".Rmd"),
               fileName_html_Verleiher = paste0("source/Verleiherabrechnung ID",`Event ID`,".html")
        )
    }else stop("Mapping not possible")
    return(df_mapping)
  }
  
  ### Erstellen der Abrechnung pro `Event ID` ####
  AbrechnungErstellen <- function(df_mapping, df_Abrechnung) {
    for (ii in df_mapping$`Event ID`) {
      # Template der Abrechnung einlesen
      c_raw <- readLines("source/Abrechnung.Rmd")
      
      # Ändern des Templates: Variable im Template ii wird gesetzt. c_Date[ii] wird verwendet um das korrekte Datum für die Bereichterstellung auszuwählen.
      index <- (1:length(c_raw))[c_raw |> str_detect("variablen")]
      c_raw[(index + 1)] <- c_raw[(index + 1)] |> str_replace(one_or_more(DGT), paste0(ii))
      
      # Ändern des Templates Titel Filmname
      index <- (1:length(c_raw))[c_raw |> str_detect("Abrechnung Filmvorführung")]
      c_temp1 <- df_mapping|>
        filter(`Event ID` == ii)|>
        select(Filmtitel) |>
        pull()
      c_temp <- c_raw[(index)] |> str_split("\"", simplify = T) |> as.vector()
      c_temp <- c_temp[1:2]
      c_temp <- paste0(c(c_temp), collapse = "\"")
      c_temp <- paste0(c(c_temp, " "), collapse = "")
      c_temp <- paste0(c(c_temp, c_temp1), collapse = "")
      c_temp <- paste(c_temp, "/ Event ID", ii)
      c_raw[(index)] <- paste0(c(c_temp, "\""), collapse = "")
      
      
      # Create Abrechnung
      c_fileName <- df_mapping|>
        filter(`Event ID` == ii)|>
        select(fileName_RMD)|>
        pull()
      
      # neues file schreiben mit toc
      c_raw |>
        r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
        writeLines(c_fileName)

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
      message("Processing ", ii, " of ", nrow(df_mapping))
      render_single_file(
        df_mapping$fileName_RMD[ii],
        df_mapping$fileName_html[ii],
        data_env
      )
    }, .options = furrr_options(seed = NULL))
    
    # remove temp files RMD files
    file.remove(df_mapping$fileName_RMD)
    
    
    return(NULL)
  }
  
  ### Erstellen der Verleiherabrechnung pro Filmvorführung ####
  VerleiherabrechnungErstellen <- function(df_mapping) {
    for (ii in df_mapping$`Event ID`) {
      # Create Verleiherabrechnung
      # Template der Abrechnung einlesen
      c_raw <- readLines("source/Verleiherabrechnung.Rmd")
      
      # Ändern des Templates: Variable im Template ii wird gesetzt. c_Date[ii] wird verwendet um das korrekte Datum für die Bereichterstellung auszuwählen.
      index <- (1:length(c_raw))[c_raw |> str_detect("variablen")]
      c_raw[(index + 1)] <- c_raw[(index + 1)] |> 
        str_replace(one_or_more(DGT), paste0(ii))
      
      # neues file schreiben ohne toc
      c_fileName <- df_mapping|>
        filter(`Event ID` == ii)|>
        select(fileName_RMD_Verleiher)|>pull()
      c_raw |>
        writeLines(c_fileName)
    }
    
    # Determine the number of cores to use
    num_cores <- parallel::detectCores() - 1  # Use all but one core to avoid overloading the system
    if (num_cores > 4) num_cores <- 5
    
    # Adjust cores based on workload
    if (nrow(df_mapping) < num_cores) {
      num_cores <- nrow(df_mapping)
    }
    
    # Render in parallel Verleiherabrechnung
    library(future)
    plan(multisession, workers = num_cores)
    
    # Render files in parallel
    library(furrr)
    future_walk(1:nrow(df_mapping), function(ii) {
      render_single_file(
        df_mapping$fileName_RMD_Verleiher[ii],
        df_mapping$fileName_html_Verleiher[ii],
        data_env
      )
    })
    
    # Delete selected files
    if (all(file.exists(df_mapping$fileName_RMD_Verleiher))) {
      file.remove(df_mapping$fileName_RMD_Verleiher)
    } else {
      warning("Some files to delete do not exist.")
    }
    return(NULL)
  }
  
  ### Statistik-Bericht erstellen ####
  StatistikErstellen <- function() {
    # Einlesen
    c_raw <- readLines("source/Statistik.Rmd")
    # Inhaltsverzeichnis

    # neues file schreiben mit toc
    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines(paste0("source/temp.Rmd"))

    # Render
    render_single_file(input = "source/temp.Rmd", output = "Statistik.html", envir = data_env)
  }
  
  ### Filmvorschlag erstellen ####
  FilmvorschlagErstellen <- function(data_env) {
    # Einlesen
    c_raw <- readLines("source/Archiv.Rmd")
    # Inhaltsverzeichnis

    # neues file schreiben mit toc
    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines(paste0("source/temp.Rmd"))

    # Render
    render_single_file(input = "source/Archiv.Rmd", output = "Archiv.html", envir = data_env)
  }
  
  ### Jahresrechnung-Bericht erstellen ####
  JahresrechnungErstellen <- function() {
    # Einlesen
    c_raw <- readLines("source/Jahresrechnung.Rmd")
    # Inhaltsverzeichnis

    c_raw |>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
      writeLines(paste0("source/temp.Rmd"))

    # Render
    render_single_file(input = "source/temp.Rmd", output = "Jahresrechnung.html", envir = data_env)
  }
  
  ### function to edit Site-Map: insert pictures ####
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
  
  ### function to create a site-map ####
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
    x <- df_temp1$FileName[1]
    
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
            unlist()|>
            str_remove("\r")
          c_raw
          
          # Create data to return
          # Create data to return
          tibble(
            `Suisa-Nummer` = c_raw[9],
            Filmtitel = c_raw[10],
            Datum = c_raw[9],
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
    df_temp1
    
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


  
  ## Shiny reactive variables ####
  ### warning ####
  # calculate_warnings <- shiny::reactiveVal(as.character(calculate_warnings))
  calculate_warnings <- shiny::reactiveVal("")
  
  ### System rückgaben an user
  ausgabe_text <- shiny::reactiveVal(as.character(ausgabe_text))
  
  ### Abrechnungsjahr ####
  Abrechungsjahr <- shiny::reactiveVal(year(Sys.Date()))
  
  ### Vektor mit Datumseinträgen ####
  if (exists("df_Besucherzahlen", envir = data_env))  {
    datum_vektor <- data_env$df_Besucherzahlen$Datum
  } else {
    datum_vektor <- seq(
      from = as.Date(paste0(year(Sys.Date()), "-01-01")),
      to   = as.Date(paste0(year(Sys.Date()), "-12-31")),
      by   = "day"
    )
  }
  
  ### Filmtabelle anzeigen ####
  df_Render <- shiny::reactiveVal(NULL)
  
  ### Does the index.html file exist, is the webserver ready ####
  file_exists <- shiny::reactiveVal(file.exists("output/webserver/index.html"))
  
  ### Datum Auswahl für Abrechnung Filmvorführung (Finde letztes Datum) ####
  START_date_choose <- shiny::reactiveVal(paste0(year(Sys.Date()),"-01-01")|>as.Date())
  End_date_choose <- shiny::reactiveVal(Sys.Date() + ((max(datum_vektor) - Sys.Date()) |> as.integer()))
  
  ### Store process for secondary app in a reactive value ####
  second_app_process <- reactiveVal(NULL)
  
  current_data <- reactiveVal(NULL)
  
  ### Database connection ####
  DB_con <- shiny::reactiveVal(con)
  ### Database host ####
  DB_host <- shiny::reactiveVal(DB_host)
  ### Database name ####
  DB_name <- shiny::reactiveVal(DB_name)
  ### Database user ####
  DB_user <- shiny::reactiveVal(DB_user)
  ### Database password ####
  DB_pw <- shiny::reactiveVal(DB_pw)

  
  ## Abort: do nothing! ####
  observeEvent(input$abort,{
    removeModal()
  })
  
  ##  Button: Abrechnungsjahr #####
  ### 1 ####  
  shiny::observeEvent(input$c_Abrechnungsjahr,{
    req(input$c_Abrechnungsjahr)
    
    # update Abrechnungsjahr 
    Abrechungsjahr(input$c_Abrechnungsjahr) # used to choose start and end date 
    data_env$c_Abrechnungsjahr <- input$c_Abrechnungsjahr # export to date_env used by Statistik and Jahresrechnung
    
    # Execution time 
    c_time <- Sys.time()
    shiny::withProgress(message = "Berechnung...", value = 0, {
      shiny::incProgress(1 / 3, detail = paste("Step", 1, "of 3"))
      ausgabe_text("Dateien wurden eingelesen.\n")
      calculate_warnings("")
      
      # read data
      tryCatch({
        # Fehler abfangen
        ausgabe_text(capture.output({
          withCallingHandlers(
            {
              data_env$c_Abrechnungsjahr <- Abrechungsjahr()
              source("source/calculate.R", local = data_env)
              shiny::incProgress(1 / 3, detail = paste("Step", 2, "of 3"))
            },
            warning = function(w) {
              # Capture warnings and store them in calculate_warnings
              calculate_warnings(paste(calculate_warnings(), "Warning:", w$message, sep = ""))
              invokeRestart("muffleWarning")  # Suppress the warning from being printed
            }
          )
        }, type = "message"))
      }, error = function(e) {
        ausgabe_text(
          paste0(
            error_calculate,
            e$message,
            collapse = ""
          )
        )
      })
      
     
      
      
      df_temp <- DB_get_table("Programm", DB_con())|>
        filter(year(Datum) == input$c_Abrechnungsjahr)
      if(nrow(df_temp) == 0){
        warning("Es gibt noch keine Vorführung für das Jahr ", input$c_Abrechnungsjahr)
        START_date_choose(paste0(input$c_Abrechnungsjahr,"-01-01")|>as.Date())
        End_date_choose(paste0(input$c_Abrechnungsjahr,"-12-31")|>as.Date())
      }else{
        START_date_choose(paste0(min(df_temp$Datum),"-01-01")|>as.Date())
        End_date_choose(paste0(max(df_temp$Datum),"-12-31")|>as.Date())
        
        START_date_choose()
        End_date_choose()
      }
      
      
      shiny::incProgress(1 / 3, detail = paste("step", 3, "of 3"))
      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text(),"\n",
             "Berechnung für das Jahr ", Abrechungsjahr()," durchgeführt\n",
             calculate_warnings())|>
        ausgabe_text()
    })
  })
  
  ### 2 ####
  shiny::observe({
    shiny::updateNumericInput(session, "c_Abrechnungsjahr", value = Abrechungsjahr())
  })

  ##  Button: Berechnen #####
  shiny::observeEvent(input$calculate, {
    # Execution time 
    c_time <- Sys.time()
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 3, detail = paste("Step", 1, "of 3"))
      ausgabe_text("Dateien wurden eingelesen.\n")
      calculate_warnings("")

      # read data
      tryCatch({
        # Fehler abfangen
        ausgabe_text(capture.output({
          withCallingHandlers(
            {
              data_env$c_Abrechnungsjahr <- Abrechungsjahr()
              source("source/calculate.R", local = data_env)
              shiny::incProgress(1 / 2, detail = paste("Step", 2, "of 3"))
            },
            warning = function(w) {
              # Capture warnings and store them in calculate_warnings
              calculate_warnings(paste0("Warning: ", w$message))
              invokeRestart("muffleWarning")  # Suppress the warning from being printed
            }
          )
        }, type = "message"))
      }, error = function(e) {
        ausgabe_text(
            paste0(
              error_calculate,
              e$message,
              collapse = ""
            )
          )
      })
      shiny::incProgress(1 / 3, detail = paste("step", 3, "of 3"))
      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text(),"\n",
             "Berechnung für das Jahr ", Abrechungsjahr()," durchgeführt\n",
             calculate_warnings())|>
        ausgabe_text()
    })
  })
  
  ## Button: Filmabrechnung(en) erstellen #####
  shiny::observeEvent(input$Abrechnung, {
    # Execution time 
    c_time <- Sys.time()
    if(is.null(input$dateTable_rows_selected)){
      # User interaction
      showModal(
        modalDialog(
          title = "Bitte eine Zeile in der Tabelle markieren",
          easyClose = TRUE,
          footer = modalButton("Abbrechen")
        )
      )
      req(input$dateTable_rows_selected) # exit early from the function
    }else{
      input$dateTable_cells_selected
      df_mapping <- current_data()[input$dateTable_rows_selected,]
      df_mapping
    }
    
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
            if(r_is.defined(df_mapping)){
              df_mapping__ <- 
                Abrechnung_mapping(
                  data_env,
                  start_datum, end_datum,
                  df_mapping$`Event ID`
                )
            } else {
              df_mapping__ <- 
                Abrechnung_mapping(
                  data_env,
                  start_datum, end_datum
                )
            }
            
            shiny::incProgress(1 / 4, detail = paste("Abrechnung: ", 2, "of 4"))
            AbrechnungErstellen(
              df_mapping__,
              data_env$df_Abrechnung
            )
            # webserver
            tryCatch({
              webserver()
            }, error = function(e) {
              ausgabe_text(
                paste0(
                  ausgabe_text(),
                  "\nWebserver erstellen, Fehler:\n",
                  e$message
                )
              )
            })
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
  
  ## Button: Verleiherabrechnung(en) erstellen #####
  shiny::observeEvent(input$Verleiherrechnung, {
    # Execution time 
    c_time <- Sys.time()
    
    # Execution time 
    c_time <- Sys.time()
    if(is.null(input$dateTable_rows_selected)){
      # User interaction
      showModal(
        modalDialog(
          title = "Bitte eine Zeile in der Tabelle markieren",
          easyClose = TRUE,
          footer = modalButton("Abbrechen")
        )
      )
      req(input$dateTable_rows_selected) # exit early from the function
    }else{
      input$dateTable_cells_selected
      df_mapping <- current_data()[input$dateTable_rows_selected,]
      df_mapping
    }
    
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
          
          # Verleiherrechnung erstellen mit dateRange user input
          tryCatch({
            
            if(r_is.defined(df_mapping)){
              df_mapping__ <- 
                Abrechnung_mapping(
                  data_env,
                  start_datum, end_datum,
                  df_mapping$`Event ID`
                )
            } else {
              df_mapping__ <- 
                Abrechnung_mapping(
                  data_env,
                  start_datum, end_datum
                )
            }
 
            df_mapping__ <- df_mapping__|>
              filter(!`Kinoförderer gratis?`)
            
            shiny::incProgress(1 / 4, detail = paste("Verleiherabrechnung: ", 2, "of 4"))
            if(nrow(df_mapping__) > 0){

              VerleiherabrechnungErstellen(
                df_mapping__
              )
              # webserver
              tryCatch({
                webserver()
              }, error = function(e) {
                ausgabe_text(
                  paste0(
                    ausgabe_text(),
                    "\nWebserver erstellen, Fehler:\n",
                    e$message
                  )
                )
              })
            } else {
              ausgabe_text("\nFür diesen Film muss keine Verleiherrechnug erzeugt werden.
                           \nFall doch muss die Tabelle `Verleiher` in der Sektion Dropdowns geändert werden: Spalte `Kinoförderer gratis`")
            }
          }, error = function(e) {
            ausgabe_text(
              paste0(
                ausgabe_text(),
                "\nVerleiherabrechnung erstellen, Fehler beim Bericht erstellen:\n",
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
        
        shiny::incProgress(1 / 4, detail = paste("Step", 3, "of 4"))
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
  
  ## Button: Statistik #####
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
          StatistikErstellen()
          shiny::incProgress(1 / 5, detail = paste("Step", 2, "of 5"))
        }, error = function(e) {
          ausgabe_text(paste(
            "Statistik, Fehler beim Bericht erstellen:\n",
            e$message
          ))
        })
        tryCatch({
          webserver()
          shiny::incProgress(1 / 5, detail = paste("Step", 3, "of 5"))
        }, error = function(e) {
          ausgabe_text(paste(
            "Statistik, Fehler beim webserver erstellen:\n",
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
  
  ## Button: Jahresrechnung #####
  shiny::observeEvent(input$Jahresrechnung, {
    # Execution time 
    c_time <- Sys.time()
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 5, detail = paste("Step", 1, "of 5"))
      # User feedback
      paste0("Bericht: Jahresrechnung erstellt",
             paste0("\n", getwd(), "/output")) |>
        ausgabe_text()
      if (exists("data_env")) {
        tryCatch({
          shiny::incProgress(1 / 5, detail = paste("Step", 2, "of 5"))
          JahresrechnungErstellen()
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
  
  ## Button: Download Handler Werbung #####
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
  
  ## Button: Wordpress #####
  shiny::observeEvent(input$wordpress, {
    # Execution time 
    c_time <- Sys.time()
    shiny::withProgress(message = "Running script...", value = 0, {
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
        FilmvorschlagErstellen(WordPress_env)
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
  
  ## Button: "Alles erstellen" #####
  shiny::observeEvent(input$ErstelleAbrechnung, {
    # Execution time 
    c_time <- Sys.time()
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 10, detail = paste("Step", 1, "of 10"))
      # User interaction
      "Alles wurde neu erstellt" |>
        ausgabe_text()
      calculate_warnings("")
      
      # Delete all files prior to creating new files
      list.files("output/", "html", full.names = TRUE) |>
        file.remove()
      list.files("output/pict/", "html", full.names = TRUE) |>
        file.remove()
      
      # run script calculate.R to finde error specifically happening with only this source
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
          StatistikErstellen()
          shiny::incProgress(1 / 10, detail = paste("Step", 2, "of 10"))
          
          # Jahresrechnung-Bericht erstellen
          JahresrechnungErstellen()
          shiny::incProgress(1 / 10, detail = paste("Step", 3, "of 10"))
          
          # Bericht(e) Abrechnung pro Filmforführung erstellen
          df_mapping__ <- 
            Abrechnung_mapping(
              data_env,
              start = paste0(Abrechungsjahr(),"-1-1")|>as.Date(),
              end = paste0(Abrechungsjahr(),"-12-31")|>as.Date()
            )
          AbrechnungErstellen(
            df_mapping__,
            data_env$df_Abrechnung
          )
          shiny::incProgress(1 / 10, detail = paste("Step", 4, "of 10"))
          df_mapping__ <- df_mapping__|>
            filter(!`Kinoförderer gratis?`)
          if(nrow(df_mapping__)>0){
            VerleiherabrechnungErstellen(
              df_mapping__
            )
          }

          shiny::incProgress(1 / 10, detail = paste("step", 5, "of 10"))
          
          # Procinema
          source("source/procinema.R", local = WordPress_env)
          shiny::incProgress(1 / 10, detail = paste("step", 6, "of 10"))
          # Wordpress
          source("source/read_and_convert_wordPress.R", local = WordPress_env)
          shiny::incProgress(1 / 10, detail = paste("step", 7, "of 10"))
          
          FilmvorschlagErstellen(WordPress_env)
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
      
      End_date_choose(max(data_env$df_Abrechnung$Datum))
      
      file_exists(file.exists("output/webserver/index.html"))
      
      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
        ausgabe_text()
      
      shiny::incProgress(1 / 10, detail = paste("Step", 10, "of 10"))
    })
  })
  
  ## Button: Handler Wordpress #####
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
    
  ## Upload handler #####
  file_data <- shiny::reactive({
    shiny::req(input$file)
    file_path <- input$file$datapath
    file_name <- input$file$name                  # Get file name
    file_ext <- tools::file_ext(input$file$name)  # Get file extension
    
    if(length(file_name) > 1){
      paste0("Die folgenden Dateien wurden hochgeladen:\n" , paste(file_name, collapse = "\n"),
             "\nEs darf nur eine Datei hochgeladen werden!")|>
        ausgabe_text()
      return(NULL)
    }
    

    if (file_ext == "txt") {
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
          "\" wurde im Verzeichniss \n.../Kinoklub",
          save_path,
          " abgespeichert"
        ) |>
          ausgabe_text()
        return(list(type = "txt", data = readLines(file_path)))
      } else{
        if(str_detect(file_name, pattern = "Eintritte")){
          
          # upload file to database 
          c_message <- ""
          tryCatch({
            DB_upload_file(con, file_path = file_path, file_name, table_name = "Eintritt files", overwrite = FALSE)
          }, warning = function(w) {
            c_message <<- paste0("\nWarnung bein Hochladen der Datei:\n", file_name, "\n", conditionMessage(w), "\n")
          }, error = function(e) {
            c_message <<- 
              paste0("\nFehler beim Hochladen der Datei:\n", file_name, "\n",conditionMessage(e), "\n",
                     c_message, "\n")
          })
      
          # read data an create new rows from it
          tryCatch({
            new_rows <- convert_data_Film_txt(file_name, DB_con())
          }, warning = function(w) {
             c_message <<- 
               paste0("\nWarnung bei der Datei konvertierung:\n", file_name, "\n", conditionMessage(w), "\n", 
                      c_message, "\n")
          }, error = function(e) {
            c_message <<- 
              paste0("\nFehler bei der Datei konvertierung:\n", file_name, "\n", c_message, "\n", 
                     c_message ,"\n")
          })
          
          if(table_exists(DB_con(),"df_Eintritt")){
            # test if entries already exists
            test <- DB_get_table("df_Eintritt", DB_con(), download = FALSE)|>
              select(-ID)|>
              filter(`Event ID` %in% new_rows$`Event ID`)|>
              collect()|>
              convert_to_template_types(l_template$df_Eintritt)
            
            c_test <- 
              identical(
                str(new_rows), 
                str(test)
              )
            
            # what needs to be updated?
            new_rows_ <-
              anti_join(
                new_rows, test,
                by = join_by(`Event ID`, Datum, Suisanummer, Filmtitel,
                             Platzkategorie, Zahlend, Verkaufspreis, Anzahl, `Umsatz [CHF]`, `SUISA-Vorabzug [%]`)
              )
            
            if(nrow(new_rows_) > 0){
              c_ID <- DB_get_table("df_Eintritt", DB_con(), download = FALSE)|>
                select(ID)|>
                collect()|>
                pull()|>
                as.integer()|>
                max()
              c_ID <- c_ID + 1L
              
              new_rows_ <- 
                bind_cols(ID = c_ID:(c_ID + nrow(new_rows_) - 1),
                          new_rows_
                )
              
              # update database
              DB_add_rows(new_rows_, "df_Eintritt", con, batch_size = 1)
              # system reply message
              paste0("Es wurde folgendes der Tabelle df_Eintritt hinzugefügt:\n",
                     paste0(print(new_rows_), collapse = "\n"), 
                     c_message
              )|>
                ausgabe_text()
            } else {
              c_message|>
                ausgabe_text()
            }
          } else {
            new_rows <- 
              bind_cols(ID = 1:nrow(new_rows),
                        new_rows)
            DB_copy_table(new_rows, DB_con(), "df_Eintritt")
            # system reply message
            paste0("Es wurde folgendes der Tabelle df_Eintritt hinzugefügt:\n",
                   paste0(print(new_rows_), collapse = "\n"), 
                   c_message
            )|>
              ausgabe_text()
          }
          c_raw <- DB_get_file(con, file_name, "Eintritt files")$`file content`|>
            str_split("\n")|>
            unlist()

          return(list(type = "txt", data = c_raw))
          
        } else if (str_detect(file_name, pattern = "Kiosk")){
          # upload file to database 
          c_message <- ""
          tryCatch({
            DB_upload_file(con, file_path = file_path, file_name, table_name = "Kiosk files", overwrite = FALSE)
          }, warning = function(w) {
            c_message <<- paste0("\nWarnung bein Hochladen der Datei:\n", file_name, "\n", conditionMessage(w), "\n")
          }, error = function(e) {
            c_message <<- 
              paste0("\nFehler beim Hochladen der Datei:\n", file_name, "\n",conditionMessage(e), "\n",
                     c_message, "\n")
          })
          
          # read data an create new rows from it
          tryCatch({
            # read data an create new rows from it
            new_rows <- convert_data_kiosk_txt(file_name, DB_con())
          # }, warning = function(w) {
          #   c_message <<- 
          #     paste0("\nWarnung bei der Datei konvertierung:\n", file_name, "\n", conditionMessage(w), "\n", 
          #            c_message, "\n")
          }, error = function(e) {
            c_message <<- 
              paste0("\nFehler bei der Datei konvertierung:\n", file_name, "\n", c_message, "\n", 
                     c_message ,"\n")
          })
          
          if(table_exists(DB_con(),"df_Kiosk")){
            # test if entries already exists
            test <- DB_get_table("df_Kiosk", DB_con(), download = FALSE)|>
              select(-ID)|>
              filter(`Event ID` %in% new_rows$`Event ID`)|>
              collect()|>
              convert_to_template_types(l_template$df_Kiosk)|>
              mutate(`Gewinn [CHF]` = round(`Gewinn [CHF]`,2))
            
            new_rows <- new_rows|>
              select(-ID)|>
              mutate(`Gewinn [CHF]` = round(`Gewinn [CHF]`,2))
            
            c_test <- identical(new_rows, test)
            c_test <- identical(dim(new_rows), dim(test))

            # what needs to be updated?
            new_rows_ <- 
              anti_join(
                new_rows,
                test,
                by = join_by(`Event ID`, Datum, ID_Kioskartikel,
                             `Artikelname-Kassensystem`, Verkaufsartikel, `Verkaufspreis [CHF]`, Menge,
                             `Einkaufspreis [CHF]`, Lieferant, `Gültig ab Datum`, `Einzelpreis [CHF]`,
                             Anzahl, `Umsatz [CHF]`, `Gewinn [CHF]`, `Überschuss / Manko [CHF]`)
              )
            
            if(nrow(new_rows_) > 0){
              c_ID <- DB_get_table("df_Kiosk", DB_con(), download = FALSE)|>
                select(ID)|>
                collect()|>
                pull()|>
                as.integer()|>
                max()
              c_ID <- c_ID + 1L
              
              new_rows_ <- 
                bind_cols(ID = c_ID:(c_ID + nrow(new_rows_) - 1),
                          new_rows_
                )
              
              # update database
              DB_add_rows(new_rows_, "df_Kiosk", con, batch_size = 1)
              # system reply message
              paste0("Es wurde folgendes der Tabelle df_Kiosk hinzugefügt:\n",
                     paste0(print(new_rows_), collapse = "\n"), 
                     c_message
              )|>
                ausgabe_text()
            } else {
              c_message|>
                ausgabe_text()
            }
          } else {
            new_rows <- new_rows|>
              mutate(ID = row_number())
            DB_copy_table(new_rows, DB_con(), "df_Kiosk")
            # system reply message
            paste0("Es wurde folgendes der Tabelle df_Kiosk hinzugefügt:\n",
                   paste0(print(new_rows_), collapse = "\n"), 
                   c_message
            )|>
              ausgabe_text()
          }
          
          c_raw <- DB_get_file(con, file_name, "Kiosk files")$`file content`|>
            str_split("\n")|>
            unlist()
          
          return(list(type = "txt", data = c_raw))
        }
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
        "\" wurde im Verzeichniss \n.../Kinoklub",
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
  
  ## Reder: Update table with all the dates in the selected range #####
  output$dateTable <-  DT::renderDT({
    df_temp <- DB_get_table("Programm", con)|>
      convert_to_template_types(l_template$Programm)|>
      distinct(`Event ID`, .keep_all = T)|>
      filter(between(Datum, START_date_choose(), End_date_choose()), `Verleiher Angefragt?` == "Bestätigt") |>
      arrange(desc(Datum), desc(Zeit)) |>
      mutate(Datum = format(Datum, "%d.%m.%Y"),
             Zeit = format(Zeit, "%H%M")) 
    
    Verleiher <- DB_get_table("Verleiher", con)|>
      convert_to_template_types(l_template$Verleiher )|>
      select(Verleihername, `Kinoförderer gratis?`)
    
    df_temp <- left_join(df_temp, Verleiher, by = c(Verleiher = "Verleihername"))
    
    df_temp <- df_temp|>
      select(`Event ID`, Filmtitel, Datum, Zeit, Suisanummer, Verleiher,`Kinoförderer gratis?`)
    
    current_data(df_temp)
    
    datatable(
      df_temp,
      filter = "top",
      rownames = FALSE,
      class = 'datatables',
      options = list(
        pageLength = 5,
        lengthMenu = c_lengthMenu,
        dom = 'lftip',
        initComplete = JS(
          "function(settings, json) {",
          "// One-time header/body styles",
          "$(this.api().table().header()).css({",
          "'background-color': '#2d3e50',",
          "'color': '#ffffff'",
          "});",
          "$(this.api().table().body()).css({",
          "'background-color': '#34495e',",
          "'color': '#ecf0f1'",
          "});",
          "// One-time search/length styling",
          "$('div.dataTables_filter input').css({",
          "'background-color': '#2c3e50',",
          "'color': '#ecf0f1',",
          "'border': '1px solid #7f8c8d'",
          "});",
          "$('div.dataTables_length select').css({",
          "'background-color': '#2c3e50',",
          "'color': '#ecf0f1',",
          "'border': '1px solid #7f8c8d'",
          "});",
          "}"
        ),
        drawCallback = JS(
          "function(settings) {",
          "$('a.paginate_button').css({",
          "'background-color': '#7898b6',",
          "'color': '#ffffff',",
          "'border': '1px solid #7f8c8d',",
          "'padding': '5px 10px',",
          "'margin': '0 2px',",
          "'border-radius': '4px',",
          "'text-decoration': 'none'",
          "});",
          
          "$('a.paginate_button.current').css({",
          "'background-color': '#e67e22',",
          "'color': '#ffffff',",
          "'font-weight': 'bold'",
          "});",
          
          "$('a.paginate_button').hover(",
          "function() {",
          "if (!$(this).hasClass('current')) {",
          "$(this).css('background-color', '#5d7d9a');",
          "}",
          "},",
          "function() {",
          "if (!$(this).hasClass('current')) {",
          "$(this).css('background-color', '#7898b6');",
          "}",
          "}",
          ");",
          "}"
        )
      )
    )
    
  })
  
  ## Render: txt file rendering ####
  output$text_output <- shiny::renderPrint({
    shiny::req(file_data()$type %in% c("txt", "csv"))
    if(is.null(file_data())){
      ""|>
        writeLines()
    } else {
      c_raw <- file_data()$data
      c_raw|>
        writeLines()
    }
    
  })
  
  ## Render: Systemrückmeldungen aktualisieren #####
  output$ausgabe <- renderText({
    ausgabe_text()
  })
  
  ## Render: Dynamically update the input panel content #####
  output$dynamicContent_input_panel <- shiny::renderUI({
    shiny::tagList(
      # Abrechnungsjahr
      shiny::numericInput("c_Abrechnungsjahr","Abrechnungsjahr", 
                          value = Abrechungsjahr(),
                          min = 2023, max = 3000, step = 1),
      
      # File input handler
      shiny::fileInput(
        "file",
        "Datei hochladen:",
        accept = c(".csv", ".txt"),
        multiple = FALSE,
        buttonLabel = "Datei auswählen",
        placeholder = "Drag & drop file"
      ),
      
      # Button Daten Einlesen
      shiny::actionButton("calculate", "Berechnen"),
      shiny::tags$hr(),
      
      # Datumsbereich auswählen für die Abrechnung Filmvorführungen
      shiny::dateRangeInput(
        inputId = "dateRange",
        label = "Wählen Sie einen Datumsbereich aus:",
        start = START_date_choose(),
        # Default start date (one week ago)
        end = End_date_choose(),
        # Default end date (last show)
        min = START_date_choose(),
        # Earliest selectable date
        max = End_date_choose(),
        # Latest selectable date
        format = "dd.mm.yyyy",
        # Set input format to German (DD.MM.YYYY)
        separator = " bis ", # Separator for the two dates in German
        language = "de",
        weekstart = 1
      ),
      
      # Button zum Ausführen von Code Filmabrechnunge(n) erstellen
      shiny::actionButton("Abrechnung", "Filmabrechnung(en) erstellen"),
      shiny::actionButton("Verleiherrechnung", "Verleiherrechnung(en) erstellen"),


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
      shiny::actionButton("ErstelleAbrechnung", "Alles neu erstellen")
    )
  })
  
  ## Render: Dynamically update the output panel content #####
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      shiny::actionButton("launch_app", "Input Daten editieren"),
      shiny::actionButton("stop_app", "Input Daten editieren stoppen"),
      if (file_exists()) {
        shiny::tags$h4("Berichte:")
      },
      if (file_exists()) {
        shiny::tags$a(href = "reports/index.html", "Site-map",
                      target = "_blank",
                      style = "font-size: 24px;")
      },
      shiny::tags$h4("Filme im gewählten Abrechnungsjahr"),
      if(!startup_error)DT::DTOutput("dateTable"),
      shiny::tags$h4("Systemrückmeldungen"),
      shiny::verbatimTextOutput("ausgabe"),
      shiny::tags$hr(),
      shiny::tags$h4("Inhalt der hochgeladen Datei:"),
      shiny::tableOutput("table_output"),
      shiny::verbatimTextOutput("text_output")
    )
    
  })

  ## launch the Dateien editieren App #####
  observeEvent(input$launch_app, {
    # Execution time 
    c_time <- Sys.time()
    ausgabe_text("Input Dateien editieren gestartet.")
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Step", 1, "of 2"))
      # Path to edit input data app
      second_app_path <- "edit_input_data.R"
      # If a process already exists, don't start a new one
      if (!is.null(second_app_process()) && second_app_process()$is_alive()) {
        showNotification("Dateinen editiern ist bereis geöffnet\n", type = "message")
        return()
      }else{
        print("Starting second app...")
        proc <- processx::process$new("Rscript", 
                                      args = c("-e", paste0("shiny::runApp('", second_app_path, "', port = 5001, launch.browser = TRUE)")), 
                                      stdout = "|", stderr = "|"
        )
        shiny::incProgress(1 / 2, detail = paste("Step", 1, "of 2"))
        second_app_process(proc)  # Store the process
        # calculate execution time
        c_time <- c(c_time,end = Sys.time())|>
          diff()
        shiny::incProgress(1 / 5, detail = paste("Step", 5, "of 5"))
        
        paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
          ausgabe_text()
      }
    })
  })
  
  ## stop input data edit app #####
  observeEvent(input$stop_app, {
    # Execution time 
    c_time <- Sys.time()
    ausgabe_text("Input Dateien editieren stoppen")
    shiny::withProgress(message = "Running script...", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Step", 1, "of 2"))
      if (!is.null(second_app_process()) && second_app_process()$is_alive()) {
        print("Stopping second app...")
        second_app_process()$kill()
        second_app_process(NULL)  # Clear the reference
      }
      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      shiny::incProgress(1 / 5, detail = paste("Step", 5, "of 5"))
      
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",ausgabe_text())|>
        ausgabe_text()
    })
  }) 
}

# shiny::shinyApp(ui = ui, server = server)

# Run the app
shiny::runApp(
  shiny::shinyApp(ui = ui, server = server),
  port = 5000,
  # Replace 8080 with your desired port
  launch.browser = TRUE, # Automatically open in the system's default browser
  host = "0.0.0.0"
)
