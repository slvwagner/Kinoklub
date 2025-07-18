
source("source/functions.R")

if(!r_is.defined(sommerpause)){
  sommerpause <- 65
}

# calculate data over all years
c_years <- 2023:lubridate::year(Sys.time())
l_abrechnung <- list()
l_einnahmen <- list()
l_ausgaben <- list()
l_eintritte <- list()
l_kiosk <- list()

ii <- 1
for (ii in 1:length(c_years)) {
  data_env_all <- new.env()
  # set Abrechnungsjahr
  data_env_all$c_Abrechnungsjahr <- c_years[ii]
  # calculate data
  tryCatch({
    # Fehler abfangen
    ausgabe_text <-(capture.output({
      withCallingHandlers({
        source("source/calculate.R", local = data_env_all)
        l_abrechnung[[ii]] <- data_env_all$l_abrechnung
        l_einnahmen[[ii]] <- data_env_all$Einnahmen
        l_ausgaben[[ii]] <- data_env_all$Ausgaben
        l_eintritte[[ii]] <- data_env_all$df_Eintritt
        l_kiosk[[ii]] <- data_env_all$df_Kiosk
      }, warning = function(w) {
        # Capture warnings and store them in calculate_warnings
        ausgabe_text <- paste("Warning:", w$message, sep = "")
        invokeRestart("muffleWarning")  # Suppress the warning from being printed
      })
    }, type = "message"))
  }, error = function(e) {
    message(paste0(ausgabe_text, e$message, collapse = ""))
  })
}

# Tickets ####
names(l_eintritte) <- c_years
df_tickets <- l_eintritte|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))
df_tickets

df_tickets <- df_tickets|>
  mutate(across(everything(), as.character),
       across(contains("ID"), as.integer),
       across(contains("datum"), as.Date),
       across(contains(c("[CHF]")), as.double),
       across(contains("abrechnungsjahr"), as.integer)
       )
df_tickets

s_df_tickets <- df_tickets|>
  group_by(Abrechnungsjahr, `Event ID`, Datum, Suisanummer, Filmtitel)|>
  reframe(`Ticketumsatz [CHF]` = sum(`Umsatz [CHF]`))
s_df_tickets
  
# Kiosk ####
names(l_kiosk) <- c_years
df_Kiosk <- l_kiosk|>
  bind_rows(.id = "Abrechnungsjahr")|>
  mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))|>
  rename(`Kioskumsatz [CHF]` = `Betrag [CHF]`)
df_Kiosk

df_Kiosk <- df_Kiosk|>
  mutate(across(everything(), as.character),
         across(contains("ID"), as.integer),
         across(contains("datum"), as.Date),
         across(contains(c("[CHF]", "Abrechnungsjahr")), as.double)
  )
df_Kiosk

s_df_Kiosk <- df_Kiosk|>
  group_by(Abrechnungsjahr, `Event ID`, Datum, Suisanummer, Filmtitel)|>
  reframe(`Kioskumsatz [CHF]` = sum(`Kioskumsatz [CHF]`),
          `Einkaufspreis [CHF]` = sum(`Einkaufspreis [CHF]`)
          )|>
  arrange(desc(Datum))
s_df_Kiosk

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

## combine Einnahmen ####
df_Einnahmen <- bind_rows(df_Einnahmen, add_tickets, add_kiosk)|>
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


## Add Kiosk to Einnahmen ####
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
         Kategorie = "Kioskumsatz",
         `Event ID` = s_df_Kiosk$`Event ID`,
         Bezeichnung = paste("Kiosk: ", s_df_Kiosk$Filmtitel),
         Datum = s_df_Kiosk$Datum,
         Abrechnungsjahr = s_df_Kiosk$Abrechnungsjahr,
         `Betrag [CHF]` = s_df_Kiosk$`Einkaufspreis [CHF]`, # Einkaufspreis 
         Firmennamen = "Theater am Bahnhof",
         Adresse = "Tunaustrasse 5, 5734 Reinach"
  )
add_kiosk

## combine Ausgaben ####
df_Ausgaben <- bind_rows(df_Ausgaben, add_kiosk)|>
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

# # tested solution
# df_Eintritte <- l_abrechnung|>
#   lapply(function(x){
#     lapply(x, function(x){
#       x$s_Eintritte
#     })|>
#       bind_rows(.id = "Event ID")|>
#       mutate(`Event ID` = as.integer(`Event ID`))
#   })
# names(df_Eintritte) <- c_years
# df_Eintritte <- df_Eintritte|>
#   bind_rows(.id = "Abrechnungsjahr")|>
#   mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))
# df_Eintritte
# 
# df_Eintritte |>
#   reframe(n = n(), .by = c(Abrechnungsjahr, `Event ID`, `Umsatz [CHF]`, Zahlend)) |>
#   filter(n > 1L) 
# 
# df_temp <- left_join(
#   df_Eintritte |>
#     group_by(`Event ID`) |>
#     reframe(Besucherzahl_tot = sum(Besucherzahl)),
#   df_Eintritte |>
#     filter(Zahlend) |>
#     group_by(`Event ID`) |>
#     reframe(`Besucherzahl zahlend` = sum(Besucherzahl)),
#   join_by(`Event ID`)
# )
# df_temp
# 
# df_temp <- df_temp|>
#   mutate(`Besucherzahl gratis` = Besucherzahl_tot -  `Besucherzahl zahlend`)|>
#   group_by(`Event ID`)|>
#   reframe(`Besucherzahl total` = sum(Besucherzahl_tot),
#           `Besucherzahl zahlend` = sum(`Besucherzahl zahlend`),
#           `Besucherzahl gratis` = sum(`Besucherzahl gratis`)
#           )
# df_temp
# 
# df_temp <- left_join(
#   df_temp,
#   df_Eintritte|>
#     filter(Zahlend)|>
#     select(`Event ID`, `Umsatz [CHF]`),
#   by = join_by(`Event ID`)
#   )
# 
# # Summary Eintritte
# s_df_Eintritte <- df_Eintritte|>
#   distinct(`Event ID`,.keep_all = TRUE)|>
#   select(Abrechnungsjahr, `Event ID`)|>
#   left_join(df_temp,
#             by = join_by(`Event ID`)
#             )
# s_df_Eintritte
# 
# 
# # Summary Abrechnung
# df_s_Abrechnung <- l_abrechnung|>
#   lapply(function(x){
#     lapply(x, function(x){
#       x$s_Abrechnung
#     })|>
#       bind_rows(.id = "Event ID")|>
#       mutate(`Event ID` = as.integer(`Event ID`))
#   })
# names(df_s_Abrechnung) <- c_years
# df_s_Abrechnung <- df_s_Abrechnung|>
#   bind_rows(.id = "Abrechnungsjahr")|>
#   mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))
# df_s_Abrechnung
# 
# # Abrechnung add 
# df_Abrechnung <- df_s_Abrechnung|>
#   select(-`Umsatz [CHF]`)|>
#   left_join(s_df_Eintritte, by = join_by(`Event ID`, Abrechnungsjahr))
# df_Abrechnung
# 
# df_temp <- l_abrechnung|>
#   lapply(function(x){
#     lapply(x,function(x){
#       x$Abrechnung[1,]
#     })|>
#       bind_rows()|>
#       select(1:6)
#   })
# names(df_temp) <- c_years
# df_temp <- df_temp|>
#   bind_rows(.id = "Abrechnungsjahr")|>
#   mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr))
# 
# # Abrechnung
# df_Abrechnung <- df_Abrechnung|>
#   left_join(df_temp, by = join_by(`Event ID`, Abrechnungsjahr))|>
#   rename(`Ticketumsatz [CHF]` = `Umsatz [CHF]`)
# 
# # Kiosk 
# df_Kiosk <- l_abrechnung|>
#   lapply(function(x){
#     lapply(x, function(x){
#       x$Kiosk
#     })|>
#       bind_rows()
#   })
# names(df_Kiosk) <- c_years
# 
# df_Kiosk <- df_Kiosk|>
#   bind_rows(.id = "Abrechnungsjahr")
# df_Kiosk
# 
# # Verkaufsartikel
# s_df_Kiosk <- df_Kiosk|>
#   filter(str_detect(`Artikel-Kassensystem`, "Spez"))|>
#   group_by(`Event ID`)|>
#   reframe(
#     `Kioskumsatz-Spezialpreise [CHF]` = sum(`Umsatz [CHF]`, na.rm = TRUE),
#     `Kioskgewinn-Spezialpreise [CHF]` = sum(`Gewinn [CHF]`, na.rm = TRUE)
#   )
# s_df_Kiosk
# 
# # Spezialpreise
# s_df_Kiosk_spez <- df_Kiosk|>
#   filter(!str_detect(`Artikel-Kassensystem`, "Spez"))|>
#   group_by(`Event ID`)|>
#   reframe(
#     `Kioskumsatz [CHF]` = sum(`Umsatz [CHF]`, na.rm = TRUE),
#     `Kioskgewinn [CHF]` = sum(`Gewinn [CHF]`, na.rm = TRUE)
#   )
# s_df_Kiosk_spez
# 
# # Manko / Überschuss
# df_manko <- l_abrechnung|>
#   lapply(function(x){
#     lapply(x, function(x){
#       x$manko
#     })|>
#       bind_rows()
#   })|>
#   bind_rows()|>
#   group_by(`Event ID`)|>
#   reframe(`Überschuss / Manko [CHF]` = sum(`Überschuss / Manko [CHF]`))
# df_manko
# 
# 
# # add information
# df_Abrechnung <- df_Abrechnung|>
#   left_join(s_df_Kiosk, 
#             by = join_by(`Event ID`)
#             )|>
#   left_join(s_df_Kiosk_spez, 
#             by = join_by(`Event ID`),
#             )
# df_Abrechnung
# 
# # 
# df_Abrechnung <- df_Abrechnung|>
#   mutate(`Kioskgewinn-Spezialpreise [CHF]` = `Kioskumsatz-Spezialpreise [CHF]` - `Eventausgaben [CHF]` + `Überschuss / Manko [CHF]`,
#          `Kioskumsatz pro Gast [CHF]` = (`Kioskumsatz [CHF]`) / `Besucherzahl total`,
#          `Kioskumsatz pro zahlender Gast [CHF]` = (`Kioskumsatz [CHF]`) / (`Besucherzahl total` - `Besucherzahl gratis`)
#          )
# df_Abrechnung  
# 
# remove(df_s_Abrechnung, df_temp, df_Eintritte, s_df_Eintritte, c_years, 
#        data_env_all,
#        ii)
# 
# # r_get_colnames(df_Abrechnung)
# 
# df_Abrechnung <- df_Abrechnung|>
#   select(
#     "Abrechnungsjahr","Event ID","Link to Event ID","Suisanummer","Filmtitel","Datum","Zeit",
#     "Verleiher","Abzug [%]","Minimal Abzug [CHF]","Abzug fix [CHF]","Kinoförderer gratis?",
#     "SUISA-Vorabzug [%]","Umsatz für Netto3 [CHF]","Suisavorabzug [CHF]","Umsatz Netto 3 [CHF]",
#     "Verleiherrechnungsbetrag [CHF]",
#     "Eventeinnahmen [CHF]","Eventausgaben [CHF]",
#     "Überschuss / Manko [CHF]",
#     "Verleiherabzug [CHF]","MWST [CHF]",
#     "Besucherzahl total","Besucherzahl zahlend","Besucherzahl gratis","Ticketumsatz [CHF]","Ticketgewinn [CHF]",
#     "Kioskumsatz [CHF]","Kioskgewinn [CHF]",
#     "Kioskumsatz-Spezialpreise [CHF]","Kioskgewinn-Spezialpreise [CHF]",
#     "Gewinn aus Fimvorführung [CHF]"
#     )|>
#   distinct(`Event ID`, .keep_all = TRUE)
# 
# # 
# df_Kiosk <- left_join(
#   df_Kiosk|>
#     mutate(Abrechnungsjahr = as.integer(Abrechnungsjahr)),
#   df_Abrechnung |>
#     select(
#       Abrechnungsjahr ,
#       `Event ID`,
#       `Eventeinnahmen [CHF]`,
#       `Eventausgaben [CHF]`
#     ),
#   by = join_by(Abrechnungsjahr, `Event ID`)
# )  
# df_Kiosk
# 
# 



message("all data converted")

