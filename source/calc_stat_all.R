
source("source/functions.R")

if(!r_is.defined(sommerpause)){
  sommerpause <- 65
}

# calculate data over all years
c_years <- 2023:lubridate::year(Sys.time())
l_data <- list()

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
        l_data[[ii]] <- data_env_all$l_abrechnung
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

# Eintritte
df_Eintritte <- l_data|>
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
df_s_Abrechnung <- l_data|>
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

df_temp <- l_data|>
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
df_Kiosk <- l_data|>
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
df_manko <- l_data|>
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
       data_env_all,
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
    )

message("all data converted")