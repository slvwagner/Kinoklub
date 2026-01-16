
source("source/functions.R")

if(!r_is.defined(sommerpause)){
  sommerpause <- 65
}

# Load required packages for parallelization
library(furrr)
library(purrr)

# Set up parallel plan
plan(multisession, workers = parallelly::availableCores() - 1)  # Leave one core free

# calculate data over all years
c_years <- 2023:lubridate::year(Sys.time())

# Define a function to process a single year
process_year <- function(year) {
  data_env_all <- new.env()
  data_env_all$c_Abrechnungsjahr <- year
  
  # Initialize output list
  result <- list(
    l_abrechnung = NULL,
    Einnahmen = NULL,
    Ausgaben = NULL,
    df_Eintritt = NULL,
    df_Kiosk = NULL
  )
  
  tryCatch({
    # Capture output and warnings
    ausgabe_text <- capture.output({
      withCallingHandlers({
        source("source/calculate.R", local = data_env_all)
        
        result$l_abrechnung <- data_env_all$l_abrechnung
        result$Einnahmen <- data_env_all$Einnahmen
        result$Ausgaben <- data_env_all$Ausgaben
        result$df_Eintritt <- data_env_all$df_Eintritt
        result$df_Kiosk <- data_env_all$df_Kiosk
      }, warning = function(w) {
        # Store warnings without printing
        message(paste("Warning for year", year, ":", w$message))
        invokeRestart("muffleWarning")
      })
    }, type = "message")
    
  }, error = function(e) {
    message(paste("Error processing year", year, ":", e$message))
  })
  
  return(result)
}

# Process all years in parallel
results <- future_map(c_years, process_year, 
                      .options = furrr_options(seed = TRUE))

# Extract results from the parallel processing
l_abrechnung <- map(results, ~ .x$l_abrechnung)
l_einnahmen <- map(results, ~ .x$Einnahmen)
l_ausgaben <- map(results, ~ .x$Ausgaben)
l_eintritte <- map(results, ~ .x$df_Eintritt)
l_kiosk <- map(results, ~ .x$df_Kiosk)

# Name the lists
names(l_abrechnung) <- c_years
names(l_einnahmen) <- c_years
names(l_ausgaben) <- c_years
names(l_eintritte) <- c_years
names(l_kiosk) <- c_years


# # calculate data over all years
# c_years <- 2023:lubridate::year(Sys.time())
# l_abrechnung <- list()
# l_einnahmen <- list()
# l_ausgaben <- list()
# l_eintritte <- list()
# l_kiosk <- list()


# ii <- 1
# for (ii in 1:length(c_years)) {
#   data_env_all <- new.env()
#   # set Abrechnungsjahr
#   data_env_all$c_Abrechnungsjahr <- c_years[ii]
#   # calculate data
#   tryCatch({
#     # Fehler abfangen
#     ausgabe_text <-(capture.output({
#       withCallingHandlers({
#         source("source/calculate.R", local = data_env_all)
#         l_abrechnung[[ii]] <- data_env_all$l_abrechnung
#         l_einnahmen[[ii]] <- data_env_all$Einnahmen
#         l_ausgaben[[ii]] <- data_env_all$Ausgaben
#         l_eintritte[[ii]] <- data_env_all$df_Eintritt
#         l_kiosk[[ii]] <- data_env_all$df_Kiosk
#       }, warning = function(w) {
#         # Capture warnings and store them in calculate_warnings
#         ausgabe_text <- paste("Warning:", w$message, sep = "")
#         invokeRestart("muffleWarning")  # Suppress the warning from being printed
#       })
#     }, type = "message"))
#   }, error = function(e) {
#     message(paste0(ausgabe_text, e$message, collapse = ""))
#   })
# }

# Tickets ####
df_tickets <- l_eintritte|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))

df_tickets <- df_tickets|>
  mutate(across(everything(), as.character),
       across(contains("ID"), as.integer),
       across(contains("datum"), as.Date),
       across(contains(c("[CHF]")), as.double),
       across(contains("abrechnungsjahr"), as.integer)
       )

s_df_tickets <- df_tickets|>
  group_by(Abrechnungsjahr, `Event ID`, Datum, Suisanummer, Filmtitel)|>
  reframe(`Ticketumsatz [CHF]` = sum(`Umsatz [CHF]`))
s_df_tickets
  
# Kiosk ####
df_Kiosk <- l_kiosk|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))|>
  rename(`Kioskumsatz [CHF]` = `Betrag [CHF]`)

df_Kiosk <- df_Kiosk|>
  mutate(across(everything(), as.character),
         across(contains("ID"), as.integer),
         across(contains("datum"), as.Date),
         across(contains(c("[CHF]", "Abrechnungsjahr")), as.double)
  )

s_df_Kiosk <- df_Kiosk|>
  group_by(Abrechnungsjahr, `Event ID`, Datum, Suisanummer, Filmtitel)|>
  reframe(`Kioskumsatz [CHF]` = sum(`Kioskumsatz [CHF]`),
          `Einkaufspreis [CHF]` = sum(`Einkaufspreis [CHF]`)
          )|>
  arrange(desc(Datum))

# Kasse ####
# Note: We need to get the data_env_all from the last successful year
# Find the last successful result
last_successful_year <- which(sapply(results, function(x) !is.null(x$l_abrechnung)))
if (length(last_successful_year) > 0) {
  # Get data from the last successful year
  data_env_all <- new.env()
  data_env_all$c_Abrechnungsjahr <- c_years[tail(last_successful_year, 1)]
  source("source/calculate.R", local = data_env_all)
} else {
  stop("No years were successfully processed")
}

df_temp <- left_join(
  data_env_all$Programm,
  data_env_all$df_manko_uerberschuss,
  by = join_by(`Event ID`)
)|>
  mutate(Abrechnungsjahr = as.integer(lubridate::year(Datum)),
         Semester = as.integer(get_semester(Datum)))

df_temp <- df_temp|>
  select(Abrechnungsjahr, Semester, Datum,`Event ID`, Suisanummer, Filmtitel,`Überschuss / Manko [CHF]`)

## Manko ####
df_manko <- df_temp|>
  filter(`Überschuss / Manko [CHF]`< 0)|>
  mutate(`Überschuss / Manko [CHF]` = -`Überschuss / Manko [CHF]`)
df_manko

## Überschuss ####
df_ueberschuss <- df_temp|>
  filter(`Überschuss / Manko [CHF]`>= 0)
df_ueberschuss

# Einnahmen ####
df_Einnahmen <- l_einnahmen|>
  bind_rows()
df_Einnahmen

df_Einnahmen <- df_Einnahmen|>
  mutate(across(where(is.factor), as.character),
         across(contains("ID"), as.integer),
         across(contains("abrechnungsjahr"), as.integer)
         )
df_Einnahmen

## Add tickets to Einnahmen ####
### Create an empty row ####
template_row <- df_Einnahmen[1,]|> 
  mutate(across(everything(), ~ NA),
         across(everything(), as.character),
         across(contains("ID"), as.integer),
         across(contains("datum"), as.Date),
         across(contains(c("[CHF]", "Abrechnungsjahr")), as.double)
         )
template_row

### Replicate the template row n times ####
df_temp <- template_row[rep(1, nrow(s_df_tickets)), ]


### populate  ####
add_tickets <- df_temp|>
  mutate(ID = row_number() + nrow(s_df_tickets),
         Kategorie = "Ticketumsatz",
         `Event ID` = s_df_tickets$`Event ID`,
         Bezeichnung = paste(s_df_tickets$Filmtitel),
         Datum = s_df_tickets$Datum,
         Abrechnungsjahr = s_df_tickets$Abrechnungsjahr,
         `Betrag [CHF]` = s_df_tickets$`Ticketumsatz [CHF]`,
         Firmennamen = "Theater am Bahnhof",
         Adresse = "Tunaustrasse 5, 5734 Reinach"
         )|>
  filter(`Betrag [CHF]` != 0)
add_tickets

## Add Kiosk to Einnahmen ####
### Replicate the template row n times ####
df_temp <- template_row[rep(1, nrow(s_df_Kiosk)), ]
df_temp

### populate  ####
add_kiosk <- df_temp|>
  mutate(ID = row_number() + nrow(s_df_Kiosk),
         Kategorie = "Kioskumsatz",
         `Event ID` = s_df_Kiosk$`Event ID`,
         Bezeichnung = paste("Tickets: ", s_df_Kiosk$Filmtitel),
         Datum = s_df_Kiosk$Datum,
         Abrechnungsjahr = s_df_Kiosk$Abrechnungsjahr,
         `Betrag [CHF]` = s_df_Kiosk$`Kioskumsatz [CHF]`,
         Firmennamen = "Theater am Bahnhof",
         Adresse = "Tunaustrasse 5, 5734 Reinach"
  )
add_kiosk

## Add Kasse Überschuss ####
### Replicate the template row n times ####
df_temp <- template_row[rep(1, nrow(df_ueberschuss)), ]
df_temp

### populate  ####
add_ueberschnuss <- df_temp|>
  mutate(ID = row_number() + nrow(df_ueberschuss),
         Kategorie = "Kinoklubkasse Überschuss",
         `Event ID` = df_ueberschuss$`Event ID`,
         Bezeichnung = paste("Tickets: ", df_ueberschuss$Filmtitel),
         Datum = df_ueberschuss$Datum,
         Abrechnungsjahr = df_ueberschuss$Abrechnungsjahr,
         `Betrag [CHF]` = df_ueberschuss$`Überschuss / Manko [CHF]`,
         Firmennamen = "Theater am Bahnhof",
         Adresse = "Tunaustrasse 5, 5734 Reinach"
  )
add_ueberschnuss

## combine Einnahmen ####
df_Einnahmen <- bind_rows(df_Einnahmen, add_tickets, add_kiosk, add_ueberschnuss)|>
  arrange(desc(Datum))|>
  mutate(Kategorie = factor(Kategorie), 
         `Event ID` = factor(`Event ID`),
         Abrechnungsjahr = factor(Abrechnungsjahr),
         Semester = factor(get_semester(Datum))
         )|>
  select(
    ID,"Abrechnungsjahr", "Semester","Kategorie", 
    "Event ID","Datum","Bezeichnung","Betrag [CHF]",
    "Firmennamen","Adresse","Rechnungsnummer",)
df_Einnahmen

# Ausgaben ####
df_Ausgaben <- l_ausgaben|>
  bind_rows()|>
  mutate(Abrechnungsjahr = as.character(Abrechnungsjahr)|>as.integer())
df_Ausgaben

df_Ausgaben <- df_Ausgaben|>
  mutate(across(where(is.factor), as.character),
         across(contains("ID"), as.integer),
         across(contains("abrechnungsjahr"), as.integer)
  )
df_Ausgaben

df_Ausgaben|>filter(is.na(`Betrag [CHF]`))


## Add Kiosk to Ausgaben ####
### Create an empty row ####
template_row <- df_Ausgaben[1,]|> 
  mutate(across(everything(), ~ NA),
         across(everything(), as.character),
         across(contains("ID"), as.integer),
         across(contains("datum"), as.Date),
         across(contains(c("[CHF]", "Abrechnungsjahr")), as.double)
  )
template_row

### Replicate the template row n times ####
df_temp <- template_row[rep(1, nrow(s_df_Kiosk)), ]
df_temp

### populate  ####
add_kiosk <- df_temp|>
  mutate(ID = row_number() + nrow(s_df_Kiosk),
         Kategorie = "Getränkeeinkauf",
         `Event ID` = s_df_Kiosk$`Event ID`,
         Bezeichnung = paste("Kiosk: ", s_df_Kiosk$Filmtitel),
         Datum = s_df_Kiosk$Datum,
         Abrechnungsjahr = s_df_Kiosk$Abrechnungsjahr,
         `Betrag [CHF]` = s_df_Kiosk$`Einkaufspreis [CHF]`, # Einkaufspreis 
         Firmennamen = "Theater am Bahnhof",
         Adresse = "Tunaustrasse 5, 5734 Reinach"
  )
add_kiosk

## Add Kassa Manko to Ausgaben ####

### Replicate the template row n times ####
df_temp <- template_row[rep(1, nrow(df_manko)), ]
df_temp

### populate  ####
add_manko <- df_temp|>
  mutate(ID = row_number() + nrow(df_manko),
         Kategorie = "Kinoklubkasse Manko",
         `Event ID` = df_manko$`Event ID`,
         Bezeichnung = paste("Kiosk: ", df_manko$Filmtitel),
         Datum = df_manko$Datum,
         Abrechnungsjahr = df_manko$Abrechnungsjahr,
         `Betrag [CHF]` = df_manko$`Überschuss / Manko [CHF]`, # Einkaufspreis 
         Firmennamen = "Theater am Bahnhof",
         Adresse = "Tunaustrasse 5, 5734 Reinach"
  )
add_manko

### combine Ausgaben ####
df_Ausgaben <- bind_rows(df_Ausgaben, add_kiosk, add_manko)|>
  arrange(desc(Datum))

r_get_colnames(df_Ausgaben)

df_Ausgaben <- df_Ausgaben|>
  mutate(ID = factor(ID),
         Abrechnungsjahr = factor(Abrechnungsjahr),
         Semester = factor(get_semester(Datum)),
         `Event ID` = factor(`Event ID`),
         Kategorie = factor(Kategorie)
         )

r_get_colnames(df_Ausgaben)

df_Ausgaben <- df_Ausgaben|>
  select(
    "ID", "Abrechnungsjahr", "Semester", "Kategorie","Event ID", "Datum",
    "Bezeichnung","Betrag [CHF]","Firmennamen","Adresse","Referenz","Rechnungsnummer","Buchungskonto")


# Export to Excel ####
list(Einnahmen = df_Einnahmen,
     Ausgaben = df_Ausgaben)|>
  openxlsx::write.xlsx("output/data/Statistik.xlsx", asTable = TRUE, overwrite = TRUE)

# tested solution
df_Eintritte <- l_abrechnung|>
  lapply(function(x){
    lapply(x, function(x){
      x$s_Eintritte
    })|>
      bind_rows(.id = "Event ID")|>
      mutate(`Event ID` = as.integer(`Event ID`))
  })
names(df_Eintritte) <- c_years
df_Eintritte <- df_Eintritte|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))
df_Eintritte

df_Eintritte |>
  reframe(n = n(), .by = c(Abrechnungsjahr, `Event ID`, `Umsatz [CHF]`, Zahlend)) |>
  filter(n > 1L)

df_temp <- left_join(
  df_Eintritte |>
    group_by(`Event ID`) |>
    reframe(Besucherzahl_tot = sum(Besucherzahl)),
  df_Eintritte |>
    filter(Zahlend) |>
    group_by(`Event ID`) |>
    reframe(`Besucherzahl zahlend` = sum(Besucherzahl)),
  join_by(`Event ID`)
)
df_temp

df_temp <- df_temp|>
  mutate(`Besucherzahl gratis` = Besucherzahl_tot -  `Besucherzahl zahlend`)|>
  group_by(`Event ID`)|>
  reframe(`Besucherzahl total` = sum(Besucherzahl_tot),
          `Besucherzahl zahlend` = sum(`Besucherzahl zahlend`),
          `Besucherzahl gratis` = sum(`Besucherzahl gratis`)
          )
df_temp

df_temp <- left_join(
  df_temp,
  df_Eintritte|>
    filter(Zahlend)|>
    select(`Event ID`, `Umsatz [CHF]`),
  by = join_by(`Event ID`)
  )

# Summary Eintritte
s_df_Eintritte <- df_Eintritte|>
  distinct(`Event ID`,.keep_all = TRUE)|>
  select(Abrechnungsjahr, `Event ID`)|>
  left_join(df_temp,
            by = join_by(`Event ID`)
            )
s_df_Eintritte


# Summary Abrechnung
df_s_Abrechnung <- l_abrechnung|>
  lapply(function(x){
    lapply(x, function(x){
      x$s_Abrechnung
    })|>
      bind_rows(.id = "Event ID")|>
      mutate(`Event ID` = as.integer(`Event ID`))
  })
names(df_s_Abrechnung) <- c_years
df_s_Abrechnung <- df_s_Abrechnung|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))
df_s_Abrechnung

# Abrechnung add
df_Abrechnung <- df_s_Abrechnung|>
  select(-`Umsatz [CHF]`)|>
  left_join(s_df_Eintritte, by = join_by(`Event ID`, Abrechnungsjahr))
df_Abrechnung

df_temp <- l_abrechnung|>
  lapply(function(x){
    lapply(x,function(x){
      x$Abrechnung[1,]
    })|>
      bind_rows()|>
      select(1:6)
  })
names(df_temp) <- c_years
df_temp <- df_temp|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))

# Abrechnung
df_Abrechnung <- df_Abrechnung|>
  left_join(df_temp, by = join_by(`Event ID`, Abrechnungsjahr))|>
  rename(`Ticketumsatz [CHF]` = `Umsatz [CHF]`)

# Kiosk
df_Kiosk <- l_abrechnung|>
  lapply(function(x){
    lapply(x, function(x){
      x$Kiosk
    })|>
      bind_rows()
  })
names(df_Kiosk) <- c_years

df_Kiosk <- df_Kiosk|>
  bind_rows(.id = "Abrechnungsjahr")
df_Kiosk

# Verkaufsartikel
s_df_Kiosk <- df_Kiosk|>
  filter(str_detect(`Artikel-Kassensystem`, "Spez"))|>
  group_by(`Event ID`)|>
  reframe(
    `Kioskumsatz-Spezialpreise [CHF]` = sum(`Umsatz [CHF]`, na.rm = TRUE),
    `Kioskgewinn-Spezialpreise [CHF]` = sum(`Gewinn [CHF]`, na.rm = TRUE)
  )
s_df_Kiosk

# Spezialpreise
s_df_Kiosk_spez <- df_Kiosk|>
  filter(!str_detect(`Artikel-Kassensystem`, "Spez"))|>
  group_by(`Event ID`)|>
  reframe(
    `Kioskumsatz [CHF]` = sum(`Umsatz [CHF]`, na.rm = TRUE),
    `Kioskgewinn [CHF]` = sum(`Gewinn [CHF]`, na.rm = TRUE)
  )
s_df_Kiosk_spez

# Manko / Überschuss
df_manko <- l_abrechnung|>
  lapply(function(x){
    lapply(x, function(x){
      x$manko
    })|>
      bind_rows()
  })|>
  bind_rows()|>
  group_by(`Event ID`)|>
  reframe(`Überschuss / Manko [CHF]` = sum(`Überschuss / Manko [CHF]`))
df_manko


# add information
df_Abrechnung <- df_Abrechnung|>
  left_join(s_df_Kiosk,
            by = join_by(`Event ID`)
            )|>
  left_join(s_df_Kiosk_spez,
            by = join_by(`Event ID`),
            )
df_Abrechnung

#
df_Abrechnung <- df_Abrechnung|>
  mutate(`Kioskgewinn-Spezialpreise [CHF]` = `Kioskumsatz-Spezialpreise [CHF]` - `Eventausgaben [CHF]` + `Überschuss / Manko [CHF]`,
         `Kioskumsatz pro Gast [CHF]` = (`Kioskumsatz [CHF]`) / `Besucherzahl total`,
         `Kioskumsatz pro zahlender Gast [CHF]` = (`Kioskumsatz [CHF]`) / (`Besucherzahl total` - `Besucherzahl gratis`)
         )
df_Abrechnung

remove(df_s_Abrechnung, df_temp, df_Eintritte, s_df_Eintritte, c_years,
       ii)

# r_get_colnames(df_Abrechnung)

df_Abrechnung <- df_Abrechnung|>
  select(
    "Abrechnungsjahr","Event ID","Link to Event ID","Suisanummer","Filmtitel","Datum","Zeit",
    "Verleiher","Abzug [%]","Minimal Abzug [CHF]","Abzug fix [CHF]","Kinoförderer gratis?",
    "SUISA-Vorabzug [%]","Umsatz für Netto3 [CHF]","Suisavorabzug [CHF]","Umsatz Netto 3 [CHF]",
    "Verleiherrechnungsbetrag [CHF]",
    "Eventeinnahmen [CHF]","Eventausgaben [CHF]",
    "Überschuss / Manko [CHF]",
    "Verleiherabzug [CHF]","MWST [CHF]",
    "Besucherzahl total","Besucherzahl zahlend","Besucherzahl gratis","Ticketumsatz [CHF]","Ticketgewinn [CHF]",
    "Kioskumsatz [CHF]","Kioskgewinn [CHF]",
    "Kioskumsatz-Spezialpreise [CHF]","Kioskgewinn-Spezialpreise [CHF]",
    "Gewinn aus Fimvorführung [CHF]"
    )|>
  distinct(`Event ID`, .keep_all = TRUE)

# Abrechnung ####
df_Kiosk <- l_kiosk|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))|>
  rename(`Kioskumsatz [CHF]` = `Betrag [CHF]`)

df_Kiosk <- left_join(
  df_Kiosk|>
    mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr)),
  df_Abrechnung |>
    select(
      Abrechnungsjahr ,
      `Event ID`,
      `Eventeinnahmen [CHF]`,
      `Eventausgaben [CHF]`
    ),
  by = join_by(Abrechnungsjahr, `Event ID`))|>
  mutate(Semester = get_semester(Datum))
df_Kiosk

message("all data converted")

