# Vorbereiten / Installieren
rm(list = ls())

source("user_settings.R")
# Load excel column definition database
col_env <- new.env()
load("col_env.RData", envir = col_env)

# Envirnoment for Data read in
data_env <- new.env()

# Daten einlesen
calculate_warnings <- ""
ausgabe_text <- "Alles eingelesen."
tryCatch({
  # Fehler abfangen
  ausgabe_text <<- capture.output({
    withCallingHandlers(
      {
        source("source/calculate.R", local = data_env)
      },
      warning = function(w) {
        # Capture warnings and store them in calculate_warnings
        calculate_warnings <<- paste(calculate_warnings, "Warning:", w$message, sep = "")
        invokeRestart("muffleWarning")  # Suppress the warning from being printed
      }
    )
  }, type = "message")
}, error = function(e) {
  ausgabe_text <<- paste0("Fehler beim Ausführen von 'source/calculate.R':\n",
                          e$message)
  ausgabe_text <<-
    paste0(
      "\n\n",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
      "! Es konnten nicht alle Daten einlesen werden. !\n",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",
      ausgabe_text,
      calculate_warnings,
      collapse = ""
    )
})

######################################################################################################
# Erstellen der Abrechnung pro Filmvorführung
AbrechnungRmd <- function(mapping, df_Abrechnung, toc) {
  lapply(mapping$index, function(ii) {
    # Template der Abrechnung einlesen
    c_raw <- readLines("source/Abrechnung.Rmd")
    c_raw
    
    # Ändern des Templates: Variable im Template ii wird gesetzt. c_Date[ii] wird verwendet um das korrekte Datum für die Bereichterstellung auszuwählen.
    index <- (1:length(c_raw))[c_raw |> str_detect("variablen")]
    c_raw[(index + 1)] <- c_raw[(index + 1)] |> str_replace(one_or_more(DGT), paste0(ii))
    
    # Ändern des Templates Titel Filmname
    index <- (1:length(c_raw))[c_raw |> str_detect("Abrechnung Filmvorführung")]
    c_temp1 <- df_Abrechnung |>
      filter(
        Datum == (mapping |> filter(index == ii) |> select(Datum) |> pull()),
        `Suisa Nummer` == (
          mapping |> filter(index == ii) |> select(Suisanummer) |> pull()
        )
      ) |>
      mutate(
        Anfang = paste0(
          lubridate::hour(Anfang),
          ":",
          lubridate::minute(Anfang) |> as.character() |> formatC(format = "0", width = 2) |> str_replace(SPC, "0")
        ),
        Datum = paste0(day(Datum), ".", month(Datum), ".", year(Datum))
      ) |>
      rename(`Total Gewinn [CHF]` = `Gewinn/Verlust Filmvorführungen [CHF]`) |>
      select(Filmtitel) |>
      pull()
    
    c_temp <- c_raw[(index)] |>
      str_split("\"", simplify = T) |>
      as.vector()
    
    c_temp <- c_temp[1:2]
    c_temp <- paste0(c(c_temp), collapse = "\"")
    c_temp <- paste0(c(c_temp, " "), collapse = "")
    c_temp <- paste0(c(c_temp, c_temp1), collapse = "")
    c_raw[(index)] <- paste0(c(c_temp, "\""), collapse = "")
    
    # Inhaltsverzeichnis
    if (toc) {
      # neues file schreiben mit toc
      c_raw |>
        r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis") |>
        writeLines(mapping$fileName_RMD[ii])
    } else {
      # neues file schreiben ohne toc
      c_raw |>
        writeLines(mapping$fileName_RMD[ii])
    }
    
    # Muss eine Verleiherrechnung erstellt werden?
    if ((mapping |> filter(index == ii) |> select(CreateReportVerleiherabrechnung) |> pull())) {
      # Einlesen template der Verleiherabrechnung
      c_raw <- readLines("source/Verleiherabrechnung.Rmd")
      c_raw
      
      # Ändern des Templates mit user eingaben (ii <- ??) verwendet für Datum
      index <- (1:length(c_raw))[c_raw |> str_detect("variablen")]
      index
      c_raw[(index + 1)] <- c_raw[(index + 1)] |> str_replace(one_or_more(DGT), paste0(ii))
      
      # neues file schreiben
      c_raw |>
        writeLines(mapping$fileName_RMD_Verleiher[ii])
    }
  })
  return(NULL)
}

######################################################################################################
# Index pro Suisa-Nummer und Datum erstellen
Reports_mapping <- function(data_env, start, end) {
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
    mutate(fileName_RMD            = paste0("source/",user_Datum," Abrechnung ", Suisanummer,".Rmd"),
           fileName_html           = paste0("source/",user_Datum," Abrechnung ", Suisanummer,".html"),
           fileName_RMD_Verleiher  = paste0("source/",user_Datum," Verleiherabrechnung ", Suisanummer,".Rmd"),
           fileName_html_Verleiher = paste0("source/",user_Datum," Verleiherabrechnung ", Suisanummer,".html")
           )|>
    left_join(data_env$df_show|>
                distinct(`Suisa Nummer`,.keep_all = T)|>
                select(`Suisa Nummer`, Filmtitel),
              by = c(Suisanummer = "Suisa Nummer")
    )|>
    arrange(index)
  return(df_mapping)
}

# Reports mapping
df_mapping <- 
  Reports_mapping(
    data_env, as.Date("2024-01-01"), as.Date("2025-05-01")
  )|>
  mutate(CreateReportVerleiherabrechnung = T)
df_mapping

# Ensure the output directory exists
if (!dir.exists("output")) {
  dir.create("output")
}

# create markdown files
AbrechnungRmd(
  df_mapping, 
  get("df_Abrechnung", envir = data_env), 
  toc = TRUE
)

######################################################################################################
# remove(
#   col_env, df_P_kat_verechnen, my_template, Abrechungsjahr, ausgabe_text, c_MWST, c_render_option,
#   calculate_warnings, clc, sommerpause, toc, x, c_script_version,
#   create_df, print.cleanup, r_toc_for_Rmd
# )

######################################################################################################
# Define a function to render a single RMarkdown file
render_single_file <- function(input, output, envir) {
  rmarkdown::render(
    input = input,
    output_file = output,
    output_dir = "output",
    envir = envir,
    quiet = TRUE  # Suppress output for cleaner logs
  )
}

######################################################################################################
ii <- 1
# # Render files
# lapply(1:nrow(df_mapping), function(ii){
#     render_single_file(c(df_mapping$fileName_RMD[ii] 
#                          # df_mapping$fileName_RMD_Verleiher[ii]
#                          ), 
#                        c(df_mapping$fileName_html[ii] 
#                          # df_mapping$fileName_html_Verleiher[ii]
#                          ),
#                        data_env
#                        )
#   })

# Load the parallel package
c_time <- Sys.time()

######################################################################################################
library(parallel)
# Determine the number of cores to use
num_cores <- detectCores() - 1  # Use all but one core to avoid overloading the system
if(num_cores > 4) num_cores <- 5
if(nrow(df_mapping) < num_cores) {
  num_cores <- nrow(df_mapping)
}

paste0("NB_cores: ", num_cores)|>
  writeLines()
ii <- 1
# Render files in parallel
if (.Platform$OS.type == "unix") {
  # Use mclapply for Unix-based systems (Linux/Mac)
  mclapply(1:nrow(df_mapping), function(ii) {
    render_single_file(
      c(df_mapping$fileName_RMD[ii], df_mapping$fileName_RMD_Verleiher[ii]), 
      c(df_mapping$fileName_html[ii],df_mapping$fileName_html_Verleiher[ii]), 
      data_env
      )
  }, mc.cores = num_cores)
} else {
  # Use parLapply for Windows
  cl <- makeCluster(num_cores)
  clusterExport(
    cl,
    c( # Export necessary variables to the cluster
      "data_env",
      "r_is.defined",
      "r_is.library_loaded",
      "r_signif",
      "render_single_file",
      "round5Rappen",
      "df_mapping"
    )
  )
  parLapply(cl, 1:nrow(df_mapping), function(ii){
    render_single_file(
      c(df_mapping$fileName_RMD[ii]),
      c(df_mapping$fileName_html[ii]), 
      data_env
      )
  })
  stopCluster(cl)  # Stop the cluster after rendering
}



render_single_file <- function(input, output, envir) {
  rmarkdown::render(
    input = input,
    output_file = output,
    output_dir = "output",
    envir = envir,
    quiet = TRUE  # Suppress output for cleaner logs
  )
}

######################################################################################################
# library(furrr)
# library(rmarkdown)
# 
# # Function needed by the markdown code
# data_env$r_is.defined <- r_is.defined
# data_env$round5Rappen <- round5Rappen
# 
# # Determine the number of cores to use
# num_cores <- availableCores() - 1  # Use all but one core to avoid overloading the system
# if(num_cores > 4) num_cores <- 4
# if(nrow(df_mapping) < num_cores) {
#   num_cores <- nrow(df_mapping)
# }
# paste("Number of cores:", num_cores)|>
#   writeLines()
# 
# # Set up parallel processing
# plan(multisession, workers = num_cores)  # Use all but one core
# 
# # Function to render a single document
# render_document <- function(ii, df_mapping, data_env) {
#   current_mapping <- df_mapping |> filter(index == ii)
#   rmarkdown::render(
#     input = current_mapping$fileName_RMD,
#     output_file = current_mapping$fileName_html,
#     envir = data_env,
#     output_dir = "output",
#     quiet = TRUE  # Suppress output for cleaner logs
#   )
# }
# # Apply the function in parallel
# future_map(df_mapping$index, ~render_document(.x, df_mapping, data_env))

######################################################################################################
# copy an remove files
file.remove(df_mapping$fileName_RMD)
file.remove(df_mapping$fileName_RMD_Verleiher)

c(c_time, Sys.time())|>
  diff()|>
  print()


