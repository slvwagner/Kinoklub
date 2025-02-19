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

# Index pro Suisa-Nummer und Datum erstellen
mapping <- function(c_Datum, c_suisa) {
  df_mapping <- tibble(Datum = c_Datum, Suisanummer = c_suisa) |>
    mutate(user_Datum = paste0(day(Datum), ".", month(Datum), ".", year(Datum)),
           index = row_number())
  
  # Soll die Verleiherabrechnung erzeugt werden?
  c_file <- "Input/Verleiherabgaben.xlsx"
  c_sheets <- readxl::excel_sheets(c_file)
  c_sheets
  
  df_verleiherabgaben <- readxl::read_excel(c_file, c_sheets[1]) |>
    mutate(Datum = as.Date(Datum)) |>
    left_join(readxl::read_excel(c_file, c_sheets[2]), by = "Verleiher")
  
  df_mapping <- df_verleiherabgaben |>
    select(Datum, `Kinoförderer gratis?`, Suisanummer) |>
    right_join(df_mapping, by = join_by(Datum, Suisanummer)) |>
    mutate(
      CreateReportVerleiherabrechnung = if_else(`Kinoförderer gratis?` == "ja", F, T),
      `Kinoförderer gratis?` = NULL
    ) |>
    arrange(index)
  df_mapping <- df_mapping |>
    distinct(Datum, Suisanummer, .keep_all = T)
  return(df_mapping)
}

# Erstellen der Abrechnung pro Filmvorführung
AbrechnungRmd <- function(mapping, df_Abrechnung, toc) {
  for (ii in mapping$index) {
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
  }
  return(mapping)
}

# Reports mapping
df_mapping <- 
  mapping(
    data_env$df_mapping$Datum,
    data_env$df_mapping$Suisanummer
  )|>
  filter(between(Datum, as.Date("2025-01-01"), as.Date("2025-05-01")))|>
  mutate(fileName_RMD = paste0("Abrechnung ", Suisanummer," ", user_Datum ,".Rmd"),
         fileName_html = paste0("output/Abrechnung ", Suisanummer," ", user_Datum,".html"))
df_mapping 

# Ensure the output directory exists
if (!dir.exists("output")) {
  dir.create("output")
}


# create markdown files
df_mapping <-  AbrechnungRmd(
  df_mapping, 
  get("df_Abrechnung", envir = data_env), 
  toc = TRUE
)

remove(
  col_env, df_P_kat_verechnen, my_template, Abrechungsjahr, ausgabe_text, c_MWST, c_render_option,
  calculate_warnings, clc, sommerpause, toc, x, c_script_version,
  create_df, mapping , print.cleanup, r_toc_for_Rmd
)

library(rmarkdown)
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

# Render files 
# lapply(1:nrow(df_mapping), function(ii){
#     render_single_file(df_mapping__$fileName_RMD[ii], df_mapping__$fileName_html[ii], data_env)
#   })

# Load the parallel package
c_time <- Sys.time()
library(parallel)

# Determine the number of cores to use
num_cores <- detectCores() - 1  # Use all but one core to avoid overloading the system
if(num_cores > 4) num_cores <- 4

paste0("NB_cores: ", num_cores)|>
  writeLines()

# Render files in parallel
if (.Platform$OS.type == "unix") {
  # Use mclapply for Unix-based systems (Linux/Mac)
  mclapply(1:nrow(df_mapping), function(ii) {
    render_single_file(df_mapping$fileName_RMD[ii], df_mapping$fileName_html[ii], data_env)
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
    render_single_file(df_mapping$fileName_RMD[ii], df_mapping$fileName_html[ii], data_env)
  }) 
  stopCluster(cl)  # Stop the cluster after rendering
}

file.remove(df_mapping$fileName_RMD)

c(c_time, Sys.time())|>
  diff()|>
  print()