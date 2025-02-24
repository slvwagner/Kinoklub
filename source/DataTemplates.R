# Data templates

rm(list = ls())

library(tidyverse)

########################################################################
# find all columnnames in all excel spread sheets
########################################################################
find_col_names_excel <- function(c_file) {
  # Error handling
  stopifnot(file.exists(c_file))
  # Get sheet names
  c_sheets <- readxl::excel_sheets(c_file)
  # Read data from each sheet with specified column types
  df_temp <- 
    lapply(c_sheets, function(sheet_name) {
      readxl::read_excel(
        c_file,
        sheet = sheet_name,
        col_types = NULL # Apply column types
      )
    })
  names(df_temp) <- c_sheets
  df_temp|>
    lapply(names)
}

########################################################################
# Find all excel files to read in
c_file <- list.files("input", pattern = "xlsx", full.names = T)
c_file

c_cols <- c_file|>
  lapply(function(x){
    find_col_names_excel(x)
  })

names(c_cols) <- c_file

templateInput <- list(
  `Einkauf Kiosk` =
    tibble(
      "Artikel" = as.character(),
      "Artikelname-Kassensystem" = as.character(),
      "Verkaufspreis [CHF]" = as.numeric(),
      "Menge" = as.character(),
      "Einkaufspreis [CHF]" = as.numeric(),
      "Lieferant" = as.character(),
      "Gewinn [CHF]" = as.numeric()
    ),
  Einnahmen =
    tibble(
      "Kategorie" = as.character(),
      "Bezeichnung" = as.character(),
      "Datum" = as.Date(""),
      "Suisanummer" = as.character(),
      "Betrag [CHF]" = as.numeric(),
      "Firmennamen" = as.character(),
      "Adresse" = as.character(),
      "Rechnungsnummer" = as.character()
    ),
  Ausgaben =
    tibble(
      "Kategorie" = as.character(),
      "Spieldatum" = as.Date(""),
      "Suisanummer" = as.character(),
      "Bezeichnung" = as.character(),
      "Datum" = as.Date(""),
      "Betrag [CHF]" = as.numeric(),
      "Firmennamen" = as.character(),
      "Adresse" = as.character(),
      "Referenz" = as.character(),
      "Rechnungsnummer" = as.character(),
      "Buchungskonto" = as.character()
    ),
  Spezialpreisekiosk =
    tibble(
      "Datum" = as.Date(""),
      "Suisanummer" = as.character(),
      "Spezialpreis" = as.character(),
      "Artikelname" = as.character()
    ),
  Verleiherabgaben =
    tibble(
      "Datum" = as.Date(""),
      "Link Datum" = as.Date(""),
      "Suisanummer" = as.character(),
      "Minimal Abzug [CHF]" = as.numeric(),
      "Abzug [%]" = as.numeric(),
      "Abzug fix [CHF]" = as.numeric(),
      "Filmtitel" = as.numeric(),
      "Verleiher" = as.character(),
    ),
  Verleiher =
    tibble(
      "Verleiher" = as.character(),
      "Kinoförderer gratis?" = as.character(),
      "Adresse" = as.character(),
      "PLZ" = as.numeric(),
      "Ort" = as.character()
    ),
  Buchhaltungskonten =
    tibble(
      Buchungskonto =
        c(
          "4405 Einkauf Kioskwaren Kino",
          "4404 Filmmiete Kino",
          "4406 Werbung Kino",
          "4407 Unterhalt"
        )
    ),
  Kategorien =
    tibble(
      Auswahl = 
        c("Event",
          "Kiosk",
          "Personalaufwand",
          "Sonstiges",
          "Verleiher",
          "Vermietung",
          "Werbung"
        )
    ),
  JaNein = tibble(
    Auswahl = c("ja", "nein")
    )
)

# Einkaufspreise Kiosk
templateInput$`Einkauf Kiosk` <- readxl::read_excel("Input/Einkauf Kiosk  01.11.23.xlsx")|>
  mutate("Gültig ab Datum" =as.Date("2023-11-01"),
         "Gewinn" = NULL
         )|>
  rename("Artikelname-Kassensystem" = `Artikelname Kassensystem`,
         "Verkaufspreis [CHF]" = `Verkaufs-preis`,
         "Einkaufspreis [CHF]" = `Einkaufs- preis`,
         )|>
  mutate("Gewinn [CHF]" = `Verkaufspreis [CHF]`- `Einkaufspreis [CHF]`)

templateInput$`Einkauf Kiosk`


# Verleiher
templateInput$Verleiher <- readxl::read_excel("Input/Verleiherabgaben.xlsx", sheet = "Kinoförderer gratis")

templateInput$Verleiherabgaben <- 
  readxl::read_excel("Input/Verleiherabgaben.xlsx", sheet = "Verleiherabgaben")|>
  mutate(Datum = as.Date(Datum),
         `Link Datum` = as.Date(`Link Datum`),
         `Minimal Abzug` = as.double(`Minimal Abzug`),
         `Abzug [%]` = as.double(`Abzug [%]`),
         `Abzug fix [CHF]` = as.double(`Abzug fix [CHF]`)
         )
templateInput$Verleiherabgaben

c_file <- "Input/template.Rds"
# Create Data Source 
saveRDS(templateInput, c_file)

readRDS(c_file)

