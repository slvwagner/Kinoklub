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
# Export render template
data_env$my_template <- my_template

# Serve the custom_styles directory
shiny::addResourcePath("custom_styles", "source")

# Map the URL path "custom" to the local directory "output"
# Webserver root directory
if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}
shiny::addResourcePath("reports", "output")

# Constants ####
c_lengthMenu = c(5:20, 50, 100) # page length drop down options

# Data table in german ####
DT_language <- list(
  lengthMenu = "Zeige _MENU_ Zeile(n) pro Seite", # Text für das Dropdown-Menü
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
  
  ### Abrechnungen mapping erstellen ####
  Abrechnung_mapping <- function(Abrechnung) {
    # Soll die Verleiherabrechnung erzeugt werden?
    df_mapping <- Abrechnung |>
      select(`Event ID`, Datum , Zeit, Suisanummer, Filmtitel, `Kinoförderer gratis?`)|>
      mutate(user_Datum = format(Datum, "%d.%m.%Y"))
    
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
    
    if(nrow(df_mapping) == 1){
      render_single_file(
        df_mapping$fileName_RMD[1],
        df_mapping$fileName_html[1],
        data_env
      )
    } else {
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
    }
    # remove temp files RMD files
    file.remove(df_mapping$fileName_RMD)
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
    
    # change title 
    c_raw[str_detect(c_raw, "Statistik Kinoklub")] <- paste0("title: \"Statistik ",Abrechungsjahr(),"\"")
    
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
    
    # change title 
    c_raw[str_detect(c_raw, "Jahresabrechnung Kinoklub")] <- paste0("title: \"Jahresrechnung ",Abrechungsjahr(),"\"")
    
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
  
  ### Update Film table and date range to choose from ####
  Update_Film_table <- function() {
    
    # Update date range to choose from
    df_temp <- DB_get_table("Programm", DB_con())|>
      filter(year(Datum) == input$c_Abrechnungsjahr)
    if(nrow(df_temp) == 0){
      warning("Es gibt noch keine Vorführung für das Jahr ", input$c_Abrechnungsjahr)
      START_date_choose(paste0(input$c_Abrechnungsjahr,"-01-01")|>as.Date())
      End_date_choose(paste0(input$c_Abrechnungsjahr,"-12-31")|>as.Date())
    }else{
      START_date_choose(paste0(min(df_temp$Datum),"-01-01")|>as.Date())
      End_date_choose(paste0(max(df_temp$Datum),"-12-31")|>as.Date())
    }
    
    # creat content to render 
    df_temp <- data_env$l_abrechnung|>
      lapply(function(x){
        x$Abrechnung
      })|>
      bind_rows()
    
    if(nrow(df_temp) == 0) {
      paste0("Es wurden keine Datensätze für das Abrechnungsjahr: ", Abrechungsjahr(), " gefunden.",
             "\nBitte Dateinen hochladen!")|>
        ausgabe_text()
      req(NULL) # early stop if no data available
    }
    
    df_temp <- df_temp|>
      filter(between(Datum, START_date_choose(), End_date_choose()))|>
      arrange(desc(Datum), desc(Zeit)) |>
      mutate(Datum = format(Datum, "%d.%m.%Y"),
             Zeit = format(Zeit, "%H%M")) 
    
    df_temp <- df_temp|>
      select(`Event ID`, `Link to Event ID`, Filmtitel, Datum, Zeit, Suisanummer, Verleiher,`Kinoförderer gratis?`)
    
    
    
    # Render
    current_data(df_temp)
    Report_links()
    
  }
  
  ### Create report links in datatable ####
  Report_links <- function(){
    
    df_Abrechnungen <- 
      tibble(
        Abrechnung = list.files(path = "output", pattern = "Abrechnung")
      )

    p <- "([\\d]+)\\.html"
    
    df_Abrechnungen <- df_Abrechnungen|>
      mutate(
        url = URLencode(paste0("reports/",Abrechnung)),
        Abrechnung = paste0("<a href='", url, "' target='_blank'>Abrechnung</a>"),
        ID = str_match(df_Abrechnungen$Abrechnung,p)[,2]|>as.integer())
    df_Abrechnungen
    
    df_temp <- current_data()
    
    if("Abrechnung" %in% names(df_temp)){ 
      df_temp <- df_temp|>
        select(-Abrechnung)|>
        left_join(df_Abrechnungen|>
                    select(ID, Abrechnung),
                  by = c(`Event ID` = "ID")
        )
      
    } else { # First time run
      df_temp <- df_temp|>
        left_join(df_Abrechnungen|>
                    select(ID, Abrechnung),
                  by = c(`Event ID` = "ID")
        )
    }
    # Render
    current_data(df_temp)
    
  }
  
  ## Shiny reactive variables ####
  ### DB connection ####
  DB_con <- shiny::reactiveVal(con)
  
  ### render modla 1 ####
  df_temp_1 <- shiny::reactiveVal(NULL)
  
  ### render modla 2 ####
  df_temp_2 <- shiny::reactiveVal(NULL)
  
  ### last uploaded filename ####
  last_uploaded_file <- shiny::reactiveVal(NULL)

  ### last uploaded file path #### 
  last_uploaded_file_path <- shiny::reactiveVal(NULL)
  
  ### last uploaded file path #### 
  last_uploaded_table_name <- shiny::reactiveVal(NULL)
  
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
  
  ### Does the Statistik.html file exist ####
  file_exists_statistk <- shiny::reactiveVal(file.exists("output/Statistik.html"))
  
  ### Does the Jahresrechnung.html file exist ####
  file_exists_jahhresrechnung <- shiny::reactiveVal(file.exists("output/Jahresrechnung.html"))
  
  ### Does the Archiv.html file exist ####
  file_exists_archiv <- shiny::reactiveVal(file.exists("output/Archiv.html"))
  
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

  ### Page length of data table ####
  page_length_var <- shiny::reactiveVal(5L)
  
  ### Selected rows in data table ####
  last_selected_rows <- shiny::reactiveVal(NULL)

  ## Button: Datenbank backup ####
  shiny::observeEvent(input$DB_backup,{
    # Execution time 
    c_time <- Sys.time()
    
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host, DB_name, DB_user, DB_pw)|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }

    shiny::withProgress(message = "Datenbank backup", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Datenbank backup", 1, "of 3"))
      ausgabe_text("Dateien wurden eingelesen.\n")
      calculate_warnings("")
      
      # read data
      tryCatch({
        # Fehler abfangen
        ausgabe_text(capture.output({
          withCallingHandlers(
            {
              source("source/SQL/SQL_backup_data.R")
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

      shiny::incProgress(1 / 2, detail = paste("Datenbank backup", 3, "of 3"))
      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",
             "Datenbank-Backup durchgeführt!\n",
             "Um die Daten auf git zu Speichern bitte mit Git commiten und pushen!",
             calculate_warnings())|>
        ausgabe_text()
    })
  })
  
  ##  Button: Abrechnungsjahr #####
  shiny::observeEvent(input$c_Abrechnungsjahr,{
    req(input$c_Abrechnungsjahr)
    
    # Execution time 
    c_time <- Sys.time()
    
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host, DB_name, DB_user, DB_pw)|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    
    
    # Export Abrechnungsjahr 
    Abrechungsjahr((input$c_Abrechnungsjahr)) # used to choose start and end date 
    data_env$c_Abrechnungsjahr <- as.integer(input$c_Abrechnungsjahr) # export to date_env used by Statistik and Jahresrechnung
    
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
      
      Update_Film_table()
      last_selected_rows(NULL)
      shiny::incProgress(1 / 3, detail = paste("step", 3, "of 3"))
      # calculate execution time
      c_time <- c(c_time,end = Sys.time())|>
        diff()
      paste0("Ausführungszeit: ",r_signif(c_time),"\n",
             "Berechnung für das Jahr ", Abrechungsjahr()," durchgeführt\n",
             calculate_warnings())|>
        ausgabe_text()
    })
  })
  
  ##  Button: Berechnen #####
  shiny::observeEvent(input$calculate, {
    # Execution time 
    c_time <- Sys.time()
    
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host, DB_name, DB_user, DB_pw)|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    
    # Export Abrechnungsjahr 
    Abrechungsjahr((input$c_Abrechnungsjahr)) # used to choose start and end date 
    data_env$c_Abrechnungsjahr <- as.integer(input$c_Abrechnungsjahr) # export to date_env used by Statistik and Jahresrechnung
    
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
      
      Update_Film_table()

      
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
  
  ## Button: Spezialpreise neu Einlesen ####
  shiny::observeEvent(input$spezpreis_recalc,{
    
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host, DB_name, DB_user, DB_pw)|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }

    shiny::withProgress(message = "Neu einlesen", value = 0, {
      shiny::incProgress(1 / 5, detail = paste("Text-Dateien Konvertieren ", 1, "of 5"))
      ausgabe_text("Dateien werden eingelesen.\n")
      calculate_warnings("")
      
      # get all entries for this Abrechnungsjahr()
      df_Kiosk <- DB_get_table("df_Kiosk", DB_con(), download = FALSE)|>
        left_join(DB_get_table("Programm", DB_con(), download = FALSE)|>
                    select(`Event ID`, Datum),
                  by = join_by(`Event ID`)
                  )|>
        collect()|>
        # filter(lubridate::year(Datum) == (Abrechungsjahr()))|>
        select(-Datum)|>
        convert_to_template_types(l_template$df_Kiosk)
      df_Kiosk
      
      IDs <- df_Kiosk|>
        distinct(`Event ID`, .keep_all = TRUE)|>
        select(`Event ID`)|>
        pull()
      
      # get file names
      files <- DB_get_table("Kiosk files", DB_con(), download = FALSE)|>
        # filter(ID %in% IDs)|>
        select(filename)|>
        pull()
      files
      
      # Extract data from file 
      df_converted <- convert_kiosk_txt(files,con, l_template)
      df_converted
      
      # extract data from kiosk files
      shiny::incProgress(1 / 5, detail = paste("Spezialpreise ", 2, "of 5"))
      df_extracted <- Spezialpreisekiosk(df_converted, con, l_template)
      
      # look up Einkaufspreise
      shiny::incProgress(1 / 5, detail = paste("Einkaufspreise ", 3, "of 5"))
      df_joined <- Einkaufspreise(df_extracted, con, l_template)
      
      # check data base for changed Spezialpreise entries 
      shiny::incProgress(1 / 5, detail = paste("identical ", 4, "of 5"))
      df_spez_preis_na_DB <- df_Kiosk|>
        filter(is.na(ID_Spezialpreisekiosk) & is.na(ID_Kioskartikel))|>
        mutate(`Einzelpreis [CHF]` = round(`Einzelpreis [CHF]`,6))
      df_spez_preis_na_DB
      
      df_spez_preis_na <- df_joined|>
        filter(is.na(ID_Spezialpreisekiosk) & is.na(ID_Kioskartikel))|>
        mutate(`Einzelpreis [CHF]` = round(`Einzelpreis [CHF]`,6))|>
        convert_to_template_types(l_template$df_Kiosk)
      df_spez_preis_na
      
      # identical(str(df_spez_preis_na_DB), str(df_spez_preis_na))  
      shiny::incProgress(1 / 5, detail = paste("Update DB ", 5, "of 5"))
      c_test <- identical(df_spez_preis_na_DB, df_spez_preis_na)  
      
      if(c_test) {
        paste0("Es sind keine neuen Spezialpreis-Definitionen vorhanden.\n",
               "Bitte in der Tabelle Spezialpreisekiosk nachtragen fall nötig und dann nochmals einlesen.\n",
               "Nach dem `Spezialpreise neu einlesen` muss `Berechnen` nochmals ausgeführt werden!"
               )|>
          ausgabe_text()
      } else {
        print("here")
        
        # update so rendering can take place
        df_temp_1(df_spez_preis_na_DB)
        df_temp_2(df_spez_preis_na)
        
        # Calculate modal size based on number of columns
        num_cols <- ncol(test)
        modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
        modal_height <- ifelse(nrow(test) <= 5, "auto", "600px")
        
        showModal(
          modalDialog(
            title = paste0("Die Datensäze sind nicht gleich wie in der Datenbank!"),
            tagList(
              renderText("Daten aus der Datenbank:"),
              shiny::hr(),
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table_1")),
              shiny::hr(),
              renderText("Daten Sätze die aus der Datei extrahiert wurden:"),
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table_2")),
            ),
            easyClose = FALSE,
            footer = tagList(
              actionButton("update_entries", "Datensätze schreiben"),
              actionButton("abort", "Abbrechen")
            )
          )
        )
      }
    })
  })
  
  ## Button: Filmabrechnung(en) erstellen #####
  shiny::observeEvent(input$Abrechnung, {
    # Execution time 
    c_time <- Sys.time()
    
    shiny::withProgress(message = "Script running... ", value = 0, {
      shiny::incProgress(1 / 4, detail = paste("Filmabrechnungen", 1, "of 4"))
    
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
        df_mapping <- current_data()[input$dateTable_rows_selected,]
        last_selected_rows(input$dateTable_rows_selected)
      }
      
      # get data for reports
      l_temp <- df_mapping$`Event ID`|>
        lapply( function(ii){
          try({data_env$l_abrechnung[[as.character(ii)]]})
        })
      
      # Check for data  
      c_select <- l_temp|>
        lapply(is.null)|>
        unlist()
      
      # remove from list if NULL
      l_temp <- l_temp[!c_select]
      
      # no data do not create reports
      if(length(l_temp) == 0) {
        # user information
        paste0(
          "Es sind keine Daten für diese Filmvorführung vorhanden.\n",
          "Wird dieser Film gemeinsam abgerechnet?, Zeigt eine `Link ID` auf diesen Film?\n",
          "Bitte den Filmtitel wählen der die erste `Link ID` enthält und dann die Filmabrechnung erstellen. "
        )|>
          ausgabe_text()
        req(NULL)
      } 
      
      # Data to render
      df_temp <- l_temp|>
        lapply(function(x){
          x$Abrechnung
        })|>
        bind_rows()
      df_temp
    
      # remove reports that have a `Event ID` link(s) 
      if(is.na(df_temp$`Link to Event ID`)|>sum() < nrow(df_temp)){
        ausgabe_text("")
        ID_to_remove <- l_temp|>
          lapply(function(x){
            IDs <- x$IDs|>
              arrange(IDs)
            # only remove IDs if more than one can be found so a linked ID is found
            if(nrow(IDs) > 1){
              ID_to_remove <- x$Abrechnung|>
                filter(is.na(`Link to Event ID`))|>
                select(`Event ID`)|>
                pull()
              
              # user information
              paste0(ausgabe_text(),
                     "Removed ID: ", ID_to_remove,
                     "\n"
              )|>
                ausgabe_text()
              
              # return
              return(ID_to_remove)
              
            } else {
              return(NULL)
            }
          })|>
          unlist()
        
        df_temp <- df_temp|>
          filter(!(`Event ID` %in% ID_to_remove))
        
        if(nrow(df_temp) == 0) {
          # user information
          paste0(ausgabe_text(),
                 "Es wird nur der haupt bericht erstellt", ID_to_remove,
                 "\n"
          )|>
            ausgabe_text()
          req(NULL) # early stop
        }
        
      } else {
        ausgabe_text("")
      }
      
      # Filmabrechnungen erstellen
      tryCatch({
        df_mapping__ <- 
          Abrechnung_mapping(
            df_temp
          )

        shiny::incProgress(1 / 4, detail = paste("Abrechnung: ", 2, "of 4"))
        AbrechnungErstellen(
          df_mapping__,
          df_temp
        )
        paste0(
          ausgabe_text(),
          "\nDie Filmabrechnungen ID `", df_mapping__$`Event ID`, "` für den Film `" , df_mapping__$Filmtitel,
          "` am ", format(df_mapping__$Datum, "%d.%m.%Y"),
          " wurden erstellt."
          )|>
          ausgabe_text( )

      }, error = function(e) {
        paste0(
          ausgabe_text(),
          "Filmabrechnungen erstellen, Fehler beim Bericht erstellen:\n",
          e$message
          )|>
          ausgabe_text()
      })
      
      Report_links()

      shiny::incProgress(1 / 4, detail = paste("Step", 4, "of 4"))
    })
  })
  
  ## Button: Verleiherabrechnung(en) erstellen #####
  shiny::observeEvent(input$Verleiherrechnung, {
    # Execution time 
    c_time <- Sys.time()
    
    Report_links()
    
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
      } else{
        ausgabe_text(
          "Statistik kann nicht erstellte werden.\nKeine Daten vorhanden bitte neu einlesen!!!!"
        )
      }
      shiny::incProgress(1 / 5, detail = paste("Step", 4, "of 5"))

      file_exists_statistk(file.exists("output/Statistik.html"))
      
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
      file_exists_jahhresrechnung(file.exists("output/Jahresrechnung.html"))
      
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
      }, error = function(e) {
        ausgabe_text(paste(
          "Filmvorschläge, Fehler beim Bericht erstellen:\n",
          e$message
        ))
      })
      
      file_exists_archiv(file.exists("output/Archiv.html"))
      
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
          shiny::incProgress(1 / 10, detail = paste("step", 9, "of 10"))
          
        }, error = function(e) {
          ausgabe_text(paste(
            "Alles neu erstellen Fehlermeldung:\n",
            e$message
          ))
        })
      }
      
      End_date_choose(max(data_env$df_Abrechnung$Datum))
      START_date_choose(min(data_env$df_Abrechnung$Datum))
      
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
    
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host, DB_name, DB_user, DB_pw)|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    
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
    
    ### txt #####
    if (file_ext == "txt") {
      #### procinema #####
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
      } 
      #### Eintritte #####
      else{
        if(str_detect(file_name, pattern = "Eintritte")){
          # upload file to database capturing message, warnings and errors
          df_file_upload <- Run_capture_error_warnings(
            DB_upload_file, con, file_path = file_path, file_name, table_name = "Eintritt files", overwrite = FALSE
            )
          
          # Message 
          c_message <- df_file_upload$messages
          
          # update last uploaded file name for later use
          last_uploaded_file(file_name)
          
          # check if the file already exists
          test <- str_detect(c_message,"already exists")
          if(test){

            last_uploaded_file_path(file_path)
            last_uploaded_table_name("Eintritt files")
            
            showModal(
              modalDialog(
                title = paste0("Achtung die Datei: ",file_name," ist schon auf der Datenbank gespeichert."),
                tagList(
                  renderText("Soll die Datei überschrieben werden?")
                ),
                easyClose = FALSE, 
                footer = tagList(
                  actionButton("upload_file", "Überschreiben"),
                  actionButton("abort", "Abbrechen")
                )
              )
            )
            # system reply message
            paste0(c_message)|>
              ausgabe_text()
            return(list(type = "txt", data = df_file_upload$results))
          } else {
            showModal(
              modalDialog(
                title = paste0("Datei: `",file_name,"` wird auf die Datenbank gespeichert."),
                tagList(
                  renderText("Soll die Datei gespeichert werden?")
                ),
                easyClose = FALSE, 
                footer = tagList(
                  actionButton("upload_file_eintritt", "Speichern"),
                  actionButton("abort", "Abbrechen")
                )
              )
            )
            # system reply message
            paste0(c_message)|>
              ausgabe_text()
            return(list(type = "txt", data = df_file_upload$results))
          } 
          
          
        } 
        #### Kiosk #####
        else if (str_detect(file_name, pattern = "Kiosk")){
          # upload file to database 
          df_file_upload <- Run_capture_error_warnings(            
            DB_upload_file, con, file_path = file_path, file_name, table_name = "Kiosk files", 
            overwrite = FALSE
            )
          # Message 
          c_message <- df_file_upload$messages
          
          # update last uploaded file name for later use
          last_uploaded_file(file_name)
          
          last_uploaded_file_path(file_path)
          last_uploaded_table_name("Kiosk files")
          
          # check if the file already exists
          test <- str_detect(c_message,"already exists")
          if(test){
            showModal(
              modalDialog(
                title = paste0("Achtung die Datei: ",file_name," ist schon auf der Datenbank gespeichert."),
                tagList(
                  renderText("Soll die Datei überschrieben werden?")
                ),
                easyClose = FALSE, 
                footer = tagList(
                  actionButton("upload_file_kiosk", "Überschreiben"),
                  actionButton("abort", "Abbrechen")
                )
              )
            )
            
            # system reply message
            paste0(c_message)|>
              ausgabe_text()
            
            return(list(type = "txt", data = df_file_upload$results))
          } 
          else { # upload file
            showModal(
              modalDialog(
                title = paste0("Soll die Datei: ",file_name," auf der Datenbank gespeichert werden?"),
                easyClose = FALSE, 
                footer = tagList(
                  actionButton("upload_file", "Speichern"),
                  actionButton("abort", "Abbrechen")
                )
              )
            )
            
            # system reply message
            paste0(c_message)|>
              ausgabe_text()
            
            return(list(type = "txt", data = df_file_upload$results))
          }
        } 
      }
    } 
    ### csv #####
    else if (file_ext == "csv") {
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
    } 
    ### not yet implemented #####
    else {
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

  
  ## Button: Upload file already exists ####
  shiny::observeEvent(input$upload_file, {
    removeModal()
    c_message <- paste0("Datei `",last_uploaded_file(),"` wurde überschrieben.")
    
    file_content <- Run_capture_error_warnings(
      DB_upload_file, con, last_uploaded_file_path(), last_uploaded_file(), last_uploaded_table_name(), 
      overwrite = TRUE
    )
    
    # check wich file type 
    c_test <- last_uploaded_file()|>
      str_detect("Kiosk")
    
    # Kiosk upload
    if(c_test){
      showModal(
        modalDialog(
          title = paste0("Sollen aus der Datei: `", last_uploaded_file(), 
                         "` die Datensätze extrahiert werden?"),
          tagList(
            renderText(paste0(c_message, "\n",
                              file_content$messages
                              ))
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton("upload_file_kiosk", "Datensätze extrahieren"),
            actionButton("abort", "Abbrechen")
          )
        )
      )
    } # Eintritt upload 
    else {
      showModal(
        modalDialog(
          title = paste0("Sollen aus der Datei: `", last_uploaded_file(), 
                         "` die Datensätze extrahiert werden?"),
          tagList(
            renderText(paste0(c_message, "\n",
                              file_content$messages
            ))
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton("upload_file_eintritt", "Datensätze extrahieren"),
            actionButton("abort", "Abbrechen")
          )
        )
      )
    }

    paste0(c_message, "\n",file_content$message)|>
      ausgabe_text()
    
    
    
  })
  
  
  ## Button: Upload Eintritt ####
  shiny::observeEvent(input$upload_file_eintritt, {
    removeModal()
    c_message <- paste0("Datei ",last_uploaded_file()," wurde überschrieben.")
    
    
    file_content <- Run_capture_error_warnings(
      DB_upload_file, con, last_uploaded_file_path(), last_uploaded_file(), last_uploaded_table_name(), 
                     overwrite = TRUE
    )
    
    # convert file 
    results <- Run_capture_error_warnings(
      convert_data_Film_txt, last_uploaded_file(), DB_con()
    )
    # new rows
    new_rows <- results$result
    
    # message handling
    c_message <- paste0(c_message, "\n", results$messages, "\n", file_content$messages)
    
    
    if(DB_table_exists(DB_con(),"df_Eintritt")){
      # test if entries already exists
      test <- DB_get_table("df_Eintritt", DB_con(), download = FALSE)|>
        filter(`Event ID` %in% new_rows$`Event ID`)|>
        collect()|>
        convert_to_template_types(l_template$df_Eintritt)

      # test if data is identical
      c_test <- identical(new_rows, test|>select(-ID))
      
      if(c_test){
        paste0("Die Datensätze von der Datei: ",last_uploaded_file(), " sind indentisch mit den Datensätzen der Datenbank!\n",
               "Es wurde nichts geändert.\n",
               c_message)|>
          ausgabe_text()
        req(NULL)
      } else {
        if(DB_nrow(DB_con(), "df_Eintritt") > 0){ # append data
          
          # check the number of rows in database table
          c_ID <- DB_get_max_pk(DB_con(),"df_Eintritt")
          c_ID <- c_ID + 1L
          
          new_rows_ <-
            bind_cols(ID = c_ID:(c_ID + nrow(new_rows) - 1),
                      new_rows
            )
          
          # update so rendering can take place
          df_temp_1(test)
          df_temp_2(new_rows_)
          last_uploaded_table_name("df_Eintritt")
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(test)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(test) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Die Datensäze aus der Datei: ",last_uploaded_file(), " sind nicht gleich wie in der Datenbank!"),
              tagList(
                renderText("Daten aus der Datenbank:"),
                shiny::hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table_1")),
                shiny::hr(),
                renderText("Daten Sätze die aus der Datei extrahiert wurden:"),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table_2")),
              ),
              easyClose = FALSE,
              footer = tagList(
                actionButton("update_entries", "Datensätze schreiben"),
                actionButton("abort", "Abbrechen")
              )
            )
          )
          
          # system reply message
          paste0(c_message)|>
            ausgabe_text()
          
          req(NULL)
          
        } else { 
          # copy data DB_nrow(con,"df_Eintritt") == 0
          new_rows <- 
            bind_cols(ID = 1:nrow(new_rows),
                      new_rows)
          # upload to database
          test <- Run_capture_error_warnings(
            DB_copy_table, new_rows, DB_con(), "df_Eintritt"
          )
          # system reply message
          paste0("Es wurde folgendes der Tabelle df_Eintritt hinzugefügt:\n",
                 paste0(paste(names(new_rows),"=",new_rows), collapse = "\n"),"\n", 
                 test$message,
                 c_message
          )|>
            ausgabe_text()
        }
      }
    } else {
      # copy data DB_nrow(con,"df_Eintritt") == 0
      new_rows <- 
        bind_cols(ID = 1:nrow(new_rows),
                  new_rows)
      # upload to database
      test <- Run_capture_error_warnings(
        DB_copy_table, new_rows, DB_con(), "df_Eintritt"
      )
      # system reply message
      paste0("Es wurde folgendes der Tabelle df_Eintritt hinzugefügt:\n",
             paste0(paste(names(new_rows),"=",new_rows), collapse = "\n"), 
             test$message,
             c_message
      )|>
        ausgabe_text()
    }
  })
  
  ## Button: Upload kiosk ####
  shiny::observeEvent(input$upload_file_kiosk, {
    removeModal()
    c_message <- paste0("Datei ",last_uploaded_file()," wurde überschrieben.")
    
    results <- Convert_Kiosk_files(last_uploaded_file(), DB_con(), l_template)

    # new rows
    new_rows <- results$result
    
    # message handling
    c_message <- paste0(c_message, "\n", results$messages, "\n", results$messages)

    
    if(DB_table_exists(DB_con(),"df_Kiosk")){
      # test if entries already exists
      test <- DB_get_table("df_Kiosk", DB_con(), download = FALSE)|>
        filter(`Event ID` %in% new_rows$`Event ID`)|>
        collect()|>
        convert_to_template_types(l_template$df_Kiosk)
      
      test <- test|>
        mutate(Lieferant = as.character(Lieferant),
               `Einzelpreis [CHF]` = round(`Einzelpreis [CHF]`,4),
               `Gewinn [CHF]` = round(`Gewinn [CHF]`)
               )
      
      new_rows <- new_rows|>
        select(-ID)|>
        convert_to_template_types(l_template$df_Kiosk)|>
        mutate(Lieferant = as.character(Lieferant),
               `Einzelpreis [CHF]` = round(`Einzelpreis [CHF]`,2),
               `Gewinn [CHF]` = round(`Gewinn [CHF]`)
               )
      
      # test if data is identical
      c_test <- all.equal(new_rows, test|>select(-ID))
      
      c_test <- identical(
        new_rows, 
        test|>select(-ID)
        )

      if(c_test){ # row entries are identical 
        paste0("Die Datensätze von der Datei: ",last_uploaded_file(), " sind indentisch mit den Datensätzen der Datenbank!\n",
               "Es wurde nichts geändert.")|>
          ausgabe_text()
        req(NULL)
      } else { # row entries are different
        
        if(DB_nrow(DB_con(), "df_Kiosk") > 0){ # append data 
          c_ID <- DB_get_max_pk(DB_con(), "df_Kiosk")
          c_ID <- c_ID + 1L
          
          new_rows_ <-
            bind_cols(ID = c_ID:(c_ID + nrow(new_rows) - 1),
                      new_rows
            )
          
          # update so rendering can take place
          df_temp_1(test)
          df_temp_2(new_rows_)
          last_uploaded_table_name("df_Kiosk")
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(test)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(test) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Die Datensäze aus der Datei: ",last_uploaded_file(), " sind nicht gleich wie in der Datenbank!"),
              tagList(
                renderText("Daten aus der Datenbank:"),
                shiny::hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table_1")),
                shiny::hr(),
                renderText("Daten Sätze die aus der Datei extrahiert wurden:"),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table_2")),
              ),
              easyClose = FALSE,
              footer = tagList(
                actionButton("update_entries", "Datensätze schreiben"),
                actionButton("abort", "Abbrechen")
              )
            )
          )
          req(NULL)
        } 
        else { # create new data
          # copy data DB_nrow(con,"df_Kiosk") == 0
          new_rows <- 
            bind_cols(ID = 1:nrow(new_rows),
                      new_rows)
          # upload to database
          test <- Run_capture_error_warnings(
            DB_copy_table, new_rows, DB_con(), "df_Kiosk"
          )
          # system reply message
          paste0("Es wurde folgendes der Tabelle df_Eintritt hinzugefügt:\n",
                 paste0(paste(names(new_rows),"=",new_rows), collapse = "\n"),"\n", 
                 test$message,
                 c_message
          )|>
            ausgabe_text()
        }
      }
    } else {
      # copy data DB_nrow(con,"df_Kiosk") == 0
      new_rows <- new_rows |>
        mutate(ID = row_number()
               )
      # upload to database
      test <- Run_capture_error_warnings(
        DB_copy_table, new_rows, DB_con(), "df_Kiosk"
      )
      # system reply message
      paste0("Es wurde folgendes der Tabelle df_Eintritt hinzugefügt:\n",
             paste0(paste(names(new_rows),"=",new_rows), collapse = "\n"),"\n", 
             test$message,
             c_message
      )|>
        ausgabe_text()
    }
  })
  
  ## Button: Abort, do nothing! ####
  observeEvent(input$abort,{
    removeModal()
  })
  
  ## Button: Dateien anzeigen ####
  observeEvent(input$explore_files,{
    os_name <- Sys.info()[["sysname"]]
    
    if(os_name == "Windows"){
      shell.exec(normalizePath("output"))
    } else if (os_name == "Darwin"){ # MAC OS
      path <- "output"
      system(paste("open", shQuote(path)))
    } else if (os_name == "Linux"){
      path <- "output"
      system(paste("xdg-open", shQuote(path)))
    } else {
      stop("Operating system: ", os_name, " was not implemented for shell actions")
    }
  })
  
  ## Button: Delete old entries and upload new entries to database ####
  shiny::observeEvent(input$update_entries, {
    removeModal()
    
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host, DB_name, DB_user, DB_pw)|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    
    # find primary kes to delete from table
    c_IDs <- df_temp_1()$ID

    # Delete old entries 
    l_temp <- c_IDs|>
      lapply(function(ID){
        Run_capture_error_warnings(        
          DB_delete_row,DB_con(), last_uploaded_table_name(), "ID", ID
          )
      })
    c_message <- l_temp|>
      lapply(function(x){
        x$messages
      })|>
      unlist()|>
      paste0(collapse = "")
    
    # update database
    test <- Run_capture_error_warnings(    
      DB_add_rows, df_temp_2(), last_uploaded_table_name(), con, batch_size = 1
      )
    
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
    
    # system reply message
    paste0("Es wurde folgendes der Tabelle ", last_uploaded_table_name(), " hinzugefügt:\n",
           paste0(names(df_temp_2()), " = ",df_temp_2(), collapse = "\n"),
           test$message, c_message)|>
      ausgabe_text()
    
  })

  ## Render modal table 1 ####
  output$modal_table_1 <- DT::renderDT({
    req(df_temp_1())  
    datatable(df_temp_1(), 
              rownames = FALSE,
              selection = "multiple",
              options = list(
                searching = FALSE,     # removes search box
                language = DT_language,
                pageLength = nrow(df_temp_1()),
                paging = FALSE        # disables pagination
              )
    )
  })
  
  ## Render modal table 1 ####
  output$modal_table_2 <- DT::renderDT({
    req(df_temp_1())  
    datatable(df_temp_2(), 
              rownames = FALSE,
              selection = "multiple",
              options = list(
                searching = FALSE,     # removes search box
                language = DT_language,
                pageLength = nrow(df_temp_1()),
                paging = FALSE        # disables pagination
              )
    )
  })
  
  ## Reder: Datatable Flim #####
  output$dateTable <-  DT::renderDT({
    writeLines("DT::renderDT")
    
    # Primary key as factor
    df_temp <- current_data()
    # Primary Key as factor
    df_temp[,1] <- pull(df_temp[,1])|>
      factor()
    # Link ID as factor
    df_temp[,2] <- pull(df_temp[,2])|>
      factor()
    
    datatable(
      df_temp,
      filter = "top",
      rownames = FALSE,
      escape = FALSE,
      extensions = c('FixedHeader'),
      options = list(
        fixedHeader = TRUE,  # This keeps headers visible
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = page_length_var(),  # Use the reactive value here
        lengthMenu = c_lengthMenu,
        dom = 'lftip',
        language = DT_language,
        initComplete = JS(
          "function(settings, json) {",
          "// One-time header/body styles",
          "  $(this.api().table().header()).css({",
          "    'background-color': '#2d3e50',",
          "    'color': '#ffffff'",
          "  });",
          "  $(this.api().table().body()).css({",
          "    'background-color': '#34495e',",
          "    'color': '#ecf0f1'",
          "  });",
          "  // One-time search/length styling",
          "  $('div.dataTables_filter input').css({",
          "    'background-color': '#2c3e50',",
          "    'color': '#ecf0f1',",
          "    'border': '1px solid #7f8c8d'",
          "  });",
          "  $('div.dataTables_length select').css({",
          "    'background-color': '#2c3e50',",
          "    'color': '#ecf0f1',",
          "    'border': '1px solid #7f8c8d'",
          "  });",
          "  // Signal that table has been rendered",
          "  Shiny.setInputValue('table_rendered', new Date().getTime());",
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
  
  ## Signal: Datatable has been rendered ####
  observeEvent(input$table_rendered, {
    writeLines("Signal: Datatable has been rendered")
    # select row and page if possible
    if(!is.null(last_selected_rows())){
      m <- last_selected_rows()
      dataTableProxy('dateTable')|>
        selectRows(last_selected_rows())
    }
  })
  
  ## Change in page length ####
  observeEvent(input$dateTable_state$length, {
    req(input$dateTable_state$length)
    writeLines(paste("Page length changed to:", input$dateTable_state$length))
    
    # Update page length
    as.integer(input$dateTable_state$length) |>
      page_length_var()
  })
  
  ## Render: txt file rendering ####
  output$text_output <- shiny::renderPrint({
    shiny::req(file_data()$type %in% c("txt", "csv"))
    if(is.null(file_data())){
      ""|>
        writeLines()
    } else {
      c_raw <- file_data()$type
      if(is.null(c_raw)){}
      else print(c_raw)
      c_raw <- file_data()$data
      if(is.null(c_raw)){}
      else writeLines(c_raw)
    }
    
  })
  
  ## Render: Systemrückmeldungen aktualisieren #####
  output$ausgabe <- renderText({
    ausgabe_text()
  })
  
  ## Render: Dynamically update the input panel content #####
  output$dynamicContent_input_panel <- shiny::renderUI({
    
    # Abrechnungsjahr
    choices_select <- Abrechungsjahr()
    choices <- 2023:lubridate::year(Sys.Date())
    
    shiny::tagList(
      # Abrechnungsjahr
      # shiny::numericInput("c_Abrechnungsjahr","Abrechnungsjahr", 
      #                     value = Abrechungsjahr(),
      #                     min = 2023, max = lubridate::year(Sys.Date()) , step = 1),
      shiny::radioButtons(inputId =  "c_Abrechnungsjahr", label ="Abrechnungsjahr",
                          choices, choices_select
      ),
      
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
      shiny::actionButton("spezpreis_recalc", "Spezialpeise neu einlesen"),
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
      shiny::actionButton("wordpress", "Filmvorschläge auswerten"),
      shiny::downloadButton("downloadWordPress", "Download Filmvorschläge"),
      shiny::tags$hr(),
      
      # Button zum Ausführen von Code Alles erstellen mit Webserver
      shiny::actionButton("ErstelleAbrechnung", "Alles neu erstellen")
    )
  })

  ## Render: Dynamically update the output panel content #####
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      shiny::actionButton("launch_app", "Input Daten editieren", class = "btn-success"),
      shiny::actionButton("stop_app", "Input Daten editieren stoppen",class = "btn-danger"),
      shiny::actionButton("DB_backup", "Datenbank backup",class = "btn-info"),
      shiny::hr(),
      shiny::div(
        style = "display: flex; gap: 20px; align-items: center;",
        if(file_exists_statistk()){
          shiny::tags$a(
            href = "reports/Statistik.html", "Statistik",
            target = "_blank",
            style = "font-size: 24px;"
            )
          },
        if(file_exists_jahhresrechnung()){
          shiny::tags$a(
            href = "reports/Jahresrechnung.html", "Jahresrechnung",
            target = "_blank",
            style = "font-size: 24px;"
            )
          },
        if(file_exists_archiv()){
          shiny::tags$a(
            href = "reports/Archiv.html", "Archiv",
            target = "_blank",
            style = "font-size: 24px;"
            )
          },
        shiny::actionButton("explore_files", "Dateien Anzeigen",class = "btn-info"), 
      ),
       
      shiny::hr(),
      if(!startup_error){
        shiny::div(
          DT::DTOutput("dateTable")
        )
      },
      shiny::hr(),
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
  
  ## Button stop input data edit app #####
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
  port = 5003,
  # Replace 8080 with your desired port
  launch.browser = TRUE, # Automatically open in the system's default browser
  host = "0.0.0.0"
)
