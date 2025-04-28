# Edit input data for Kinoklub ######
# Shiny app to edit all Kinoklub input data
# The data is stored on a SQL DB. The Password for the DB connection must be stored 
# in a environment variable: DB_PASSWORD_KINOKLUB 
# Find instruction in the readme to set it up for windows or Mac/Linux

library(shiny)
library(shinyjs)
library(shinyTime)
library(DT)
library(viridis)
library(tidyverse)

source("source/functions.R")
source("source/SQL/SQL_Functions.R")

# Constants ####
Email_col_names <- c("Allgemeine Infos erhalten","Kasse / Bar", "Programm") # Email Verteilerauswahl
c_pageLength = 5 # Initial page length
c_lengthMenu = c(5:10, 20, 50, 100) # page length drop down options

width_vectors <- list(# Define width vectors for specific tables
  "Filmvorschlag" = c("Filmtitel" = "200px", "Inhalt" = "700px", "actors" = "100px"),
  "Programm" = c("Filmtitel" = "200px"),
  "Einsatzplan" = c("Verantwortlich" = "150px", "Operateur*in" = "150px")
)

# Data templates (for data type conversion) ####
l_template <- readRDS("source/SQL/template.Rds")

# Split data to input and dropdown ####
c_select_input_data <- c("Filmvorschlag","Programm", "Einsatzplan", "Einnahmen", "Ausgaben", "Spezialpreisekiosk", "Einkauf Kiosk")
l_template[c_select_input_data]

c_select_dropdown_data <- c("Kinoklubmitglieder", "Verleiher", "Lieferanten", "Platzkategorien zum Verrechnen", "Buchhaltungskonten", "Spezialpreis", "MWST")
l_template[c_select_dropdown_data]

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

# Define UI ####
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # Input panel at top
  shiny::inputPanel(
    shiny::headerPanel("Input Kinoklub"),
    shiny::textInput("user", "Benutzer"),
    shiny::passwordInput("SQL_PW", "Datenbankpasswort"),
    shiny::actionButton("SQL_connect", "Mit Datenbank verbinden", class = "btn-success"),
    shiny::actionButton("SQL_disconnect", "Datenbankverbindung schliessen", class = "btn-danger")
  ),
  includeScript("source/JS/1.12.1_jquery-ui.js"),
  tags$head(
    tags$style(HTML("
      /* Add 10px padding */
      body {
        padding-left: 10px;
        margin: 0;
      }
      /* Main container styling */
      .main-container {
        width: calc(100% - 10px); /* Account for the padding */
        margin: 0;
        padding: 0;
      }
      /* Table container styling */
      .table-container {
        width: 100%;
        margin: 0;
        padding: 0;
        overflow-x: auto;
      }
      /* Floating panel adjustments */
      #floating-panel {
        position: fixed;
        top: 50px;
        right: 20px;
        width: 250px;
        background: #c7dbed;
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 10px;
        box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.2);
        z-index: 1000;
      }
      #floating-panel-header {
        background: #88e3a0;
        padding: 5px;
        cursor: grab;
        border-bottom: 1px solid #ddd;
        text-align: center;
        font-weight: bold;
      }
      /* DataTables specific adjustments */
      .dataTables_wrapper {
        width: 100% !important;
        margin: 0 !important;
      }
      .dataTables_scroll {
        width: 100% !important;
      }
    ")),
  ),
  # Main content area
  div(class = "main-container",
      div(class = "table-container",
          uiOutput("dynamicContent_output_panel")
      )
  )
)



# Define server ####
server <- function(input, output, session) {
  ## Reactive Values ####
  l_data_input <- reactiveVal(list())
  l_data_choices <- reactiveVal(list())
  l_data <- reactiveVal(list())
  column_choices <- reactiveVal(list())
  current_data <- reactiveVal(tibble())
  data_selection_ <- reactiveVal("")
  page_length_var <- reactiveVal(5L)
  ID_to_edit <- reactiveVal(1L)
  c_connected_to_db <- reactiveVal(FALSE)
  DB_con <- reactiveVal(NULL)
  df_temp_to_render <- reactiveVal(NULL)
  
  last_rendered_DT <- reactiveVal(NULL)
  lastEdited_data_set_name <- reactiveVal("")
  last_selected_row <- reactiveVal(1L)
  last_selected_page <- reactiveVal(1L)
  last_user_filter <- reactiveVal(NULL)
  
  
  ## helper functions ####
  
  ### update dropdowns ####
  update_choices <- function(l_data) {
    # Mitgliederauswahl für die Einsatzplanung
    Verantwortlich <- l_data$Kinoklubmitglieder|>
      filter(Koordination == pull(l_data$JaNein)[2])|>
      mutate(Mitglied = paste(Vorname, Nachname))|>
      select(Mitglied)
    Verantwortlich <- bind_rows(tibble(Mitglied = "..."),Verantwortlich)|>
      pull()
    
    `Operateur*in` <- l_data$Kinoklubmitglieder|>
      filter(`Operateurin` == pull(l_data$JaNein)[2])|>
      mutate(Mitglied = paste(Vorname, Nachname))|>
      select(Mitglied)
    `Operateur*in`  <- bind_rows(tibble(Mitglied = "..."),`Operateur*in` )|>
      pull()
    
    `Kasse/Bar` <- l_data$Kinoklubmitglieder|>
      filter(`Kasse / Bar` == pull(l_data$JaNein)[2])|>
      mutate(Mitglied = paste(Vorname, Nachname))|>
      select(Mitglied)
    `Kasse/Bar`   <- bind_rows(tibble(Mitglied = "..."),`Kasse/Bar`  )|>
      pull()
    
    # return updated choices list
    list(
      "Lieferant" = l_data$Lieferanten$Lieferantenname,
      "Kategorie" = l_data$Kategorie$Auswahl,
      "Buchungskonto" = l_data$Buchhaltungskonten$Buchungskontoname,
      "Verleiher" = l_data$Verleiher$Verleihername,
      "Kinoförderer gratis?" = l_data$JaNein$Auswahl,
      "Spezialpreis" = l_data$Spezialpreis$Spezialpreisname,
      "KDM ja oder nein" = l_data$JaNein$Auswahl,
      "Besucherzahlen an Verleiher gesendet" = l_data$JaNein$Auswahl,
      "Verleihervertrag abgelegt" = l_data$JaNein$Auswahl,
      "Rechnung bezahlt und abgelegt" = l_data$JaNein$Auswahl,
      "Verleiher Angefragt?" = l_data$`Status Filmliste`$`Status Filmliste`,
      "Verantwortlich" = Verantwortlich,
      "Operateur*in" = `Operateur*in`,
      "Kasse/Bar 1" = `Kasse/Bar`,
      "Kasse/Bar 2" = `Kasse/Bar`,
      "Back-up" = `Kasse/Bar`,
      "Allgemeine Infos erhalten" = l_data$JaNein$Auswahl,
      "Kasse / Bar" = l_data$JaNein$Auswahl,
      "Programm" = l_data$JaNein$Auswahl,
      "Sonderevents" = l_data$JaNein$Auswahl,
      "Marketing" = l_data$JaNein$Auswahl,
      "Finanzen" = l_data$JaNein$Auswahl,
      "Sponsoring" = l_data$JaNein$Auswahl,
      "Koordination" = l_data$JaNein$Auswahl,
      "Operateurin" = l_data$JaNein$Auswahl,
      "Event ID" = c("...",paste(l_data$Programm$`Event ID`, ":", l_data$Programm$Filmtitel)),
      "Link to Event ID" = c("...",paste(l_data$Programm$`Event ID`, ":", l_data$Programm$Filmtitel))
    )
  }
  
  ### Toolbox for the user to interact ####
  tool_box <- function(l_data_input, data_set_select , c_select_dropdown_data, choices_select = 1, choices = c("Inputdaten", "Dropdowns")) {
    if(data_set_select == "Programm"){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", "Werkzeuge"),
        selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)),
        # Function selection
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
        actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
        actionButton("duplicate_row", "Zeile duplizieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("archive_row", "Filmtitel ändern", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
      )
    } else if(data_set_select == "Einsatzplan"){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", "Werkzeuge"),
        selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)),
        # Function selection
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
      )
    } # Menue for drop downs 
    else if(data_set_select %in% names(l_data_choices())){
      if(lastEdited_data_set_name() == "Kinoklubmitglieder"){
        tags$div(
          id = "floating-panel",
          tags$div(id = "floating-panel-header", "Werkzeuge"),
          selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)),
          # Function selection
          shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                              choices = choices, selected = choices[choices_select]
          ),
          shiny::tags$hr(),
          actionButton("edit_row", "Zeile editieren", class = "btn-info"),
          shiny::tags$hr(),
          actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
          actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
          shiny::tags$hr(),
          actionButton("check_unique", "Prüfen", class = "btn-success"),
          shiny::tags$hr(),
          actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
          shiny::tags$hr(),
          actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        )
      } else {
        tags$div(
          id = "floating-panel",
          tags$div(id = "floating-panel-header", "Werkzeuge"),
          selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)),
          # Function selection
          shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                              choices = choices, selected = choices[choices_select]
          ),
          shiny::tags$hr(),
          actionButton("edit_row", "Zeile editieren", class = "btn-info"),
          shiny::tags$hr(),
          actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
          actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
          shiny::tags$hr(),
          actionButton("check_unique", "Prüfen", class = "btn-success"),
          shiny::tags$hr(),
          actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
          shiny::tags$hr(),
          actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        )
      }
    } else {
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", "Werkzeuge"),
        selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)),
        # Function selection 
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
        actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
        actionButton("duplicate_row", "Zeile duplizieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
      )
    }
  }
  
  ### get date type for each column from a data frame ####
  get_data_type <- function(df){
    1:ncol(df)|>
      lapply(function(x){
        c_temp <- df|>
          select(all_of(x))|>
          pull()
        class(c_temp)[1] # only use the first class
      })|>
      unlist()
  }
  
  ### Convert data frame columns to factors ####
  factor_handling <- function(df_temp, df_updated, select_row){
    # find class of column
    c_class <- get_data_type(df_temp)
    # convert factors to character
    for (ii in 1:length(c_class)) {
      if(c_class[ii] == "factor"){
        df_temp[,ii] <- df_temp[,ii]|>pull()|>as.character()
      }
    }
    
    for (ii in 1:nrow(df_updated)) {
      if(c_class[ii] == "factor"){
        df_updated[,ii] <- ifelse(df_updated[,ii] == "NA",NA,df_temp[,ii]) 
      }
    }
    # Update data 
    df_temp[select_row,] <- df_updated
    
    # convert to factor
    for (ii in 1:length(c_class)) {
      if(c_class[ii] == "factor"){
        df_temp[,ii] <- df_temp[,ii]|>pull()|>as.factor() 
      }
    }
    return(df_temp)
  }
  
  ### Update Einsatzplan (special handling) ####
  Update_Einsatzplan <- function(df_updated, c_class, new_row = FALSE) {
    # If the Programm changes Einsatzplan must be updated too
    if(nrow(df_updated) > 1) stop("Update_Einsatzplan shall only contain a single row")
    
    df_temp <- DB_get_table("Einsatzplan", DB_con())|> 
      filter(`Event ID` %in% df_updated$`Event ID`)|>
      select(-`Event ID`, -Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`)
    c_type <- c_class[(length(c_class) - ncol(df_temp) + 1):length(c_class)]
    
    if(nrow(df_temp) >= 1){
      df_temp <- bind_cols(DB_get_table("Programm", DB_con())|>
                             select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`)|> 
                             filter(`Event ID` %in% df_updated$`Event ID`),
                           df_temp
      )
    }else{
      df_temp <- DB_get_table("Einsatzplan", DB_con())|> 
        select(-`Event ID`, -Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`)|>
        slice(1)|>
        mutate(Verantwortlich = "", 
               `Operateur*in` = "",
               `Kasse/Bar 1` = "",
               `Kasse/Bar 2` = "",
               `Back-up` = "",
               Kommentar = "",
               Trailer = "")
      
      df_temp <- bind_cols(DB_get_table("Programm", DB_con())|>
                             select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`)|> 
                             filter(`Event ID` %in% df_updated$`Event ID`),
                           df_temp
      )
    }
    if (new_row) {
      DB_add_row(DB_con(), "Einsatzplan", df_temp)
    }
    else {
      DB_edit_row_in_table(DB_con(), "Einsatzplan", names(df_updated[,1]), df_updated[,1], df_temp, c_type)
    }
  }
  
  ### get luminance for a color ####
  get_luminance <- function(color) {
    rgb_val <- col2rgb(color) / 255
    luminance <- 0.2126 * rgb_val[1] + 0.7152 * rgb_val[2] + 0.0722 * rgb_val[3]
    luminance <- luminance * 0.8
    return(luminance)
  }
  
  ### create user modal input ####
  create_modal_input <- function(df_row, l_temp) {
    # Helper function to crate modla to edit a row
    cnt <- length(l_temp)
    for (ii in 1:ncol(df_row)) {
      col_name <- names(df_row)[ii]
      col_data_type <- class(pull(df_row[, ii]))[1]
      col_value <- df_row[, ii]|>pull()
      
      if (col_data_type == "Date") {
        l_temp[[ii + cnt]]  <- dateInput(
          inputId = as.character(ii),
          label = col_name,
          value = ifelse(is.na(col_value), as.Date(NA), col_value),
          format = "dd.mm.yyyy",
          language = "de",
          weekstart = 1
        )
      } else if (col_data_type == "hms") {
        l_temp[[ii + cnt]]  <- timeInput(
          inputId = as.character(ii),
          label = "Zeit",
          value = col_value,
          seconds = FALSE
        )
      } else if (col_data_type %in% c("numeric")) {
        l_temp[[ii + cnt]]  <- numericInput(
          inputId = as.character(ii),
          label = col_name,
          value = ifelse(is.na(col_value), NA, col_value),
          min = 0,
          step = 0.01
        )
      } else if (col_data_type %in% c("integer")) {
        l_temp[[ii + cnt]]  <- numericInput(
          inputId = as.character(ii),
          label = col_name,
          value = ifelse(is.na(col_value), NA, col_value),
          min = 0,
          step = 1
        )
      } else if (col_data_type == "factor") {
        col_value <- as.character(col_value)
        c_choices <- (column_choices()[names(column_choices()) == col_name])|>
          as_tibble()|>
          pull()
        
        if(col_name %in% c("Event ID","Link to Event ID")){
          c_select <- str_split(c_choices,":", simplify = T)[,1]|>
            as.integer()|>
            suppressWarnings()
          names(c_select) <- c_choices
          
          l_temp[[ii + cnt]]  <- selectInput(
            inputId = as.character(ii),
            label = col_name,
            choices = c_select,
            selected = ifelse(is.na(col_value), NA, col_value),
            selectize = TRUE
          )
        } else if (col_name %in% names(column_choices())) {
          l_temp[[ii + cnt]]  <- selectInput(
            inputId = as.character(ii),
            label = col_name,
            choices = c_choices,
            selected = ifelse(is.na(col_value), NA, col_value),
            selectize = TRUE
          )
        } else {
          stop("You should not end here: factor else")
        }
      } else if (col_data_type == "character") {
        if (col_name == "Suisanummer") {
          l_temp[[ii + cnt]]  <- textInput(
            inputId = as.character(ii),
            label = col_name,
            value = ifelse(is.na(col_value), "", col_value),
            placeholder = "xxxx.xxx"
          )
        } else {
          l_temp[[ii + cnt]]  <- textInput(
            inputId = as.character(ii),
            label = col_name,
            value = ifelse(is.na(col_value), "", col_value)
          )
        }
      }
    }
    
    l_temp <- base::Filter(function(x) !is.null(x) && length(x) > 0, l_temp)
    return(l_temp)
  }
  
  ### load all initially needed data before starting up ####
  load_initial_data <- function() {
    shiny::withProgress(message = "Loading data...", value = 0, {
      shiny::incProgress(1/3, detail = "Fetching from database")
      
      # 1. Get all data from DB using your template
      l_data_sql <- DB_get_Data(l_template, DB_con())
      
      # 2. Convert data types
      l_data_ready <- convert_DB_to_R(l_data_sql, l_template)
      l_data(l_data_ready)
      
      shiny::incProgress(1/3, detail = "Preparing data")
      
      # 3. Update choices for dropdowns
      update_choices(l_data()) |> 
        column_choices()
      
      # 4. Set up input and choice datasets
      l_data()[c_select_input_data] |> 
        l_data_input()
      
      l_data()[c_select_dropdown_data] |> 
        l_data_choices()
      
      
      shiny::incProgress(1/3, detail = "Finalizing")
      
      # 6. Set initial view to Programm data
      current_data(l_data()[["Programm"]] |> 
                     arrange(desc(Datum)))
      lastEdited_data_set_name("Programm")
      data_selection_("Inputdaten")
    })
  }
  
  ### Apply conditional formatting to a data table ####
  apply_conditional_formatting <- function(dt) {
    req(current_data())
    req(lastEdited_data_set_name())
    
    if (lastEdited_data_set_name() == "Programm") {
      dt <- dt |> 
        formatStyle(
          "Verleiher Angefragt?", 
          backgroundColor = styleEqual(
            c("Bestätigt", "Wird nicht gespielt", "Anfrage läuft"), 
            c('lightgreen', '#ed716d', '#FFFF97')
          )
        )
    }
    else if (lastEdited_data_set_name() == "Einsatzplan") {
      c_Kinoklubmitglied <- 
        l_data()[["Kinoklubmitglieder"]] |>
        mutate(Mitglied = paste(Vorname, Nachname)) |>
        select(Mitglied) |>
        pull()
      
      # Remove NA values and empty strings
      c_Kinoklubmitglied <- c_Kinoklubmitglied[!is.na(c_Kinoklubmitglied) & c_Kinoklubmitglied != "NA NA"]
      
      # Generate colors only for actual members (without the initial white color)
      member_colors <- viridis(n = length(c_Kinoklubmitglied), option = "turbo") |>
        colorspace::lighten(amount = 0.2)
      
      # Calculate text colors based on luminance
      text_colors <- ifelse(sapply(member_colors, get_luminance) < 0.5, "white", "black")
      
      # Apply formatting to each relevant column
      dt <- dt |>
        formatStyle(
          c("Verantwortlich", "Operateur*in", "Kasse/Bar 1", "Kasse/Bar 2", "Back-up"),
          target = "cell",
          backgroundColor = styleEqual(c_Kinoklubmitglied, member_colors),
          color = styleEqual(c_Kinoklubmitglied, text_colors)
        )
    } 
    return(dt)
  }

  
  ## Render data table ####
  output$table <- DT::renderDT({
    req(current_data())
    
    ### Create User-Readable "Datum" Columns ####
    df_temp <- current_data()
    
    # Step 1: Identify "datum" columns
    datum_cols <- names(df_temp)[stringr::str_detect(names(df_temp), regex("datum", ignore_case = TRUE))]
    
    # Step 2: Only proceed if there are any "datum" columns
    l_columnDefs <- list()
    
    if (length(datum_cols) > 0) {
      # Step 3: Format them as dd.mm.yyyy
      df_datum_user <- df_temp %>%
        dplyr::select(all_of(datum_cols)) %>%
        dplyr::mutate(across(everything(), ~ format(as.Date(.), "%d.%m.%Y")))
      
      # Step 4: Assign numeric names to the formatted columns
      names(df_datum_user) <- as.character(seq_len(ncol(df_datum_user)))
      
      # Step 5: Build new column list with formatted columns inserted
      new_cols <- list()
      formatted_index <- 1
      
      for (col_name in names(df_temp)) {
        new_cols[[length(new_cols) + 1]] <- df_temp[[col_name]]
        names(new_cols)[length(new_cols)] <- col_name
        
        if (col_name %in% datum_cols) {
          formatted_col <- df_datum_user[[as.character(formatted_index)]]
          new_cols[[length(new_cols) + 1]] <- formatted_col
          names(new_cols)[length(new_cols)] <- as.character(formatted_index)
          formatted_index <- formatted_index + 1
        }
      }
      
      df_temp <- tibble::as_tibble(new_cols)
      
      # Step 6: Build DT column definitions (hide original date, sort via original)
      col_names <- names(df_temp)
      display_col_indices <- suppressWarnings(which(!is.na(as.integer(col_names))))
      
      for (display_idx in display_col_indices) {
        original_idx <- display_idx - 1
        
        if (original_idx >= 1 && original_idx <= ncol(df_temp)) {
          l_columnDefs <- c(
            l_columnDefs,
            list(list(targets = original_idx - 1, visible = FALSE)),
            list(list(targets = display_idx - 1, orderData = original_idx - 1))
          )
          
          # Swap names to ensure user-readable label appears instead
          names(df_temp)[c(original_idx, display_idx)] <- names(df_temp)[c(display_idx, original_idx)]
        }
      }
    }
    
    ### Apply column widths from width_vector ####
    # Get the appropriate width vector for current table
    current_width_vector <- width_vectors[[lastEdited_data_set_name()]]
    
    # Apply width definitions if they exist
    if (!is.null(current_width_vector)) {
      for (colname in names(current_width_vector)) {
        idx <- which(names(df_temp) == colname)
        if (length(idx) == 1) {
          width_value <- current_width_vector[[colname]]
          # Ensure width has proper units
          if (!grepl("px$", width_value) && !grepl("%$", width_value)) {
            width_value <- paste0(width_value, "px")
          }
          l_columnDefs <- c(
            l_columnDefs,
            list(list(targets = idx - 1, width = width_value))
          )
        }
      }
    }
    
    ### Update last rendered DT ####
    stopifnot(is.data.frame(df_temp))
    last_rendered_DT(df_temp)
    
    ### Render Table ####
    datatable(
      df_temp,
      escape = FALSE,
      rownames = FALSE,
      editable = FALSE,
      selection = "single",
      filter = "top",
      width = NULL,  # Let the container handle width
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        autoWidth = TRUE,  # auto-width enable to controll columnwidth
        columnDefs = l_columnDefs,
        scrollCollapse = TRUE,  # Better scrolling behavior
        pageLength = page_length_var(),
        lengthMenu = c_lengthMenu,
        searchCols = last_user_filter(),
        drawCallback = JS("Shiny.setInputValue('table_rendered', new Date().getTime());"),
        initComplete = JS("
          function(settings, json) {
            var table = settings.oInstance.api();
            table.on('length.dt', function(e, settings, len) {
              Shiny.setInputValue('page_length', len);
            });
            // Adjust column widths after initialization
            table.columns.adjust().draw();
          }
        "),
        language = DT_language
      ),
      callback = JS("
        table.columns().every(function() {
          var column = this;
          $(column.header()).css('white-space', 'normal');
        });
      ")
    ) |> apply_conditional_formatting()
  }, server = TRUE)
  
  # Signal: Datatable has been rendered ####
  observeEvent(input$table_rendered, {
    if(!is.na(last_selected_row()) & !is.na(last_selected_page())){
      dataTableProxy('table')|>
        selectPage(last_selected_page())|>
        selectRows(last_selected_row())
    } else if (!is.na(last_selected_page())){
      dataTableProxy('table')|>
        selectPage(last_selected_page())
    }
  })
  
  ## Change in page length ####
  observeEvent(input$page_length, {
    req(input$page_length)
    page_length_var(input$page_length)
  })
  
  ## Select a row and finde page and update   ####
  observeEvent(input$table_rows_selected, {
    req(input$table_rows_selected)
    c_row <- as.integer(input$table_rows_selected)
    # map selected row to ID
    df_temp <- last_rendered_DT()
    pull(df_temp[c_row,1])|>
      ID_to_edit()
    writeLines(paste0("Selected row: ", c_row, " ID: ", ID_to_edit()," in table: ", lastEdited_data_set_name()))
    # handel user filters
    column_filters = input$table_search_columns
    column_filters <- column_filters|>
      str_remove_all("\"")|>
      str_remove_all("\\[")|>
      str_remove_all("\\]")
    column_filters <- str_split(column_filters,",")
    
    # Update last user filter
    c_test <- lapply(column_filters, function(x){
      nchar(x) > 0
    })|>
      unlist()
    # get column data type
    c_class <- get_data_type(df_temp)
    ##### apply all column filters ####
    for (ii in 1:length(column_filters)) {
      col_filter <- column_filters[[ii]]
      if(nchar(col_filter[1]) > 0){
        if(c_class[ii] %in% c("Date", "hms")){
          c_date <- pull(df_temp[,ii])|>
            as.character()
          df_temp <- df_temp[str_detect(c_date, col_filter),]
          df_temp <- df_temp[!is.na(pull(df_temp[,ii])),]
        } 
        else if(c_class[ii] == "integer"){
          # library(rebus)
          # p1 <- START%R%one_or_more(DGT)
          # p2 <- one_or_more(DGT)%R%END
          p1 <- "^[\\d]+"
          p2 <- "[\\d]+$"
          start <- str_extract(col_filter, p1)|>
            as.integer()
          end <- str_extract(col_filter, p2)|>
            as.integer()
          c_select <- start:end
          df_temp <- df_temp[pull(df_temp[,ii]) %in% c_select,]
        } else if (c_class[ii] == "factor"){
          if(length(col_filter) > 1){
            df_temp <- df_temp[pull(df_temp[,ii]) %in% col_filter,] 
          } else {
            c_select <- str_detect(pull(df_temp[,ii]), col_filter)
            c_select <- ifelse(is.na(c_select), FALSE, c_select)
            df_temp <- df_temp[c_select,]
          }
        }
        # character 
        else { 
          # filters for data tabel are not case sensitive so tolower() conversion is needed 
          df_temp <- df_temp[str_detect(pull(df_temp[,ii])|>tolower(), col_filter|>tolower()),] 
          df_temp <- df_temp[!is.na(pull(df_temp[,ii])),]
        }
      }
    }
    # map ID to selected row
    df_temp <- df_temp |>
      mutate(index = row_number())
    row_filtered <- df_temp[df_temp[,1] == ID_to_edit(),]$index
    # has the page lenght changed? 
    if(!is.null(input$page_length)){
      page_length_var(input$page_length)
    }
    if(!is_empty(row_filtered)){
      # Calculate page 
      c_page <-  ceiling(row_filtered / page_length_var())  
      writeLines(paste0("Selected page ", c_page,"\n"))
      
      if(c_page == 0) c_page <- 1
      last_selected_page(c_page)
      last_selected_row(c_row)
      
    } else {
      stop("Could not calculate page because row was empty, this is a BUG")
    }
    ##### if column filters are present update column filters #####
    if(sum(!c_test) != length(column_filters)) {
      x <- column_filters[[7]]
      column_filters_temp <- column_filters|>
        lapply(function(x){
          if(length(x) > 1){
            list(search = paste0("[",paste0("\"", x,"\"", collapse = ","),"]"))
          } else {
            if(nchar(x) > 0) {
              list(search = x)
            } 
            else {
              NULL
            }
          } 
        })
      # only update if changed
      test <- all.equal(last_user_filter(), column_filters_temp)|>is.logical()
      if(!test) {
        last_user_filter(column_filters_temp)
      }
    } else {
      last_user_filter(NULL)
    }
  })
  
  ## Database Connection ####
  observeEvent(input$SQL_connect, {
    tryCatch({
      # Connect to data base 
      DB_connect(pw = input$SQL_PW, DB_user = input$user  , con = DB_con())|>
        DB_con()
      
      # After successful connection
      c_connected_to_db(TRUE)
      
      # Initial data load
      load_initial_data()
      
    }, error = function(e) {
      showNotification(paste("load data from data base failed:", e$message), type = "error")
    })
  })
  
  ## Data set type selection ####
  observeEvent(input$data_selection,{
    shiny::withProgress(message = "data selection", value = 0, {
      shiny::incProgress(1 / 3, detail = paste("data selection", 1, "of 3"))
      
      data_selection_(input$data_selection)
      if(input$data_selection == "Dropdowns"){
        current_data(l_data()[["Kinoklubmitglieder"]])
        lastEdited_data_set_name("Kinoklubmitglieder")
        
        # Kinoklubmitgliederfarben
        c_Kinoklubmitglied <- l_data()[["Kinoklubmitglieder"]] |>
          mutate(Mitglied = paste(Vorname, Nachname)) |>
          select(Mitglied) |>
          pull()
        
        c_Kinoklubmitglied <- ifelse(c_Kinoklubmitglied == "NA NA", NA, c_Kinoklubmitglied)
        c_Kinoklubmitglied <- c_Kinoklubmitglied[!is.na(c_Kinoklubmitglied)]
        
        c_colors <- viridis(n = length(c_Kinoklubmitglied), option = "turbo") |>
          colorspace::lighten(amount = 0.2)
        
        c_colors <- c("#FFFFFFFF", c_colors)
        
      }else{
        current_data(l_data()[["Programm"]])
        lastEdited_data_set_name("Programm")
      }
      # get all data as defined in the template l_data
      l_data_sql <- DB_get_Data(l_template, DB_con())
      
      shiny::incProgress(1 / 3, detail = paste("data selection", 1, "of 3"))
      
      # Convert data types for each table
      convert_DB_to_R(l_data_sql,l_template)|>
        l_data()
      
      # update choices
      update_choices(l_data())|>
        column_choices()
      
      # Input data set
      l_data()[c_select_input_data]|>
        l_data_input()
      
      # Drop down data set
      l_data()[c_select_dropdown_data]|>
        l_data_choices()
      
      # remove row and page selection 
      last_selected_page(NA)
      last_selected_row(NA)
      # remove user filter 
      last_user_filter(NULL)
      
      shiny::incProgress(1 / 3, detail = paste("data selection", 1, "of 3"))
      
    })
  })
  
  ## Dataset Selection ####
  observeEvent(input$dataset, {
    req(input$dataset)
    req(DB_con())
    
    df_temp <- DB_get_table(input$dataset, DB_con()) |>
      convert_to_template_types(l_template[[input$dataset]])
    
    # Update reactive values
    l_temp <- l_data()
    
    if(input$dataset %in% c("Programm", "Einsatzplan")) {
      l_temp$Einsatzplan <- left_join(
        df_temp |> select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`),
        DB_get_table("Einsatzplan", DB_con()) |>
          convert_to_template_types(l_template[[input$dataset]]) |>
          select(-Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`),
        by = join_by(`Event ID`)
      ) |> arrange(desc(Datum))
      
      l_temp$Programm <- l_temp$Programm |> arrange(desc(Datum))
    } else {
      l_temp[[input$dataset]] <- df_temp
    }
    
    l_data(l_temp)
    update_choices(l_data()) |> 
      column_choices()
    lastEdited_data_set_name(input$dataset)
    
    if(input$dataset == "Einsatzplan") {
      l_temp[[input$dataset]]|>
        filter(`Verleiher Angefragt?` != "Wird nicht gespielt")|>
        current_data()
    }else {
      current_data(l_temp[[input$dataset]])
    }
    
    # remove row and page selection 
    last_selected_page(NA)
    last_selected_row(NA)
    # remove user filter 
    last_user_filter(NULL)
  })
  
  ## Disconnect from DB ####
  observeEvent(input$SQL_disconnect,{
    print("SQL_disconnect")
    dbDisconnect(DB_con())
    c_connected_to_db(FALSE)
    DB_con(NULL)
    # remove row and page selection 
    last_selected_page(NA)
    last_selected_row(NA)
  })
  
  ## Get email list ####
  observeEvent(input$get_email,{
    showModal(modalDialog(
      shiny::radioButtons("Verteiler", "Verteiler", 
                          choices = Email_col_names
      ),
      title = "Email-Verteiler wählen",
      footer = tagList(
        actionButton("get_email_verteiler","Email im Verteiler kopieren")
      )
    ))
  })
  
  ## Select email verteiler and copy emails to clipboard ####
  observeEvent(input$get_email_verteiler,{
    print("yes")
    generated_code <- paste0("l_data()[[\"Kinoklubmitglieder\"]]|>
        filter(\`",input$Verteiler,"\` == \"ja\")|>
        distinct(`E-Mail`)|>
        pull()", collapse =  "")
    
    C_verteiler <- sapply(generated_code, function(x) eval(parse(text = x)))|>
      paste0(collapse = ";")
    C_verteiler|>
      writeClipboard()
    showModal(modalDialog(
      modalButton("ok"),
      title = "Email-Verteiler wurde in die Zwischenablage kopiert",
      footer = NULL,
      easyClose = TRUE,
    ))
  })
  
  ## Abort changes and update ####
  observeEvent(input$abort_save, {
    current_data(l_data()[[input$dataset]])
    lastEdited_data_set_name(input$dataset)
    removeModal()
  })
  
  ## Abort: do nothing! ####
  observeEvent(input$abort,{
    removeModal()
  })
  
  #### Check unique ####
  observeEvent(input$check_unique, {
    # Find duplicates (keeping only duplicate rows)
    df_temp <- current_data() |>
      group_by(across(-ID)) |>
      mutate(duplicate_flag = n() > 1) |>
      ungroup() |>
      filter(duplicate_flag)|>
      select(-duplicate_flag)
    
    df_temp_to_render(df_temp)
    
    if(nrow(df_temp) > 1){
      # Calculate modal size based on number of columns
      num_cols <- ncol(df_temp)
      modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
      modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
      
      showModal(
        modalDialog(
          title = "Achtung die folgenden Zeilen sind nicht eindeutig.",
          size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
          tagList(
            renderText("Bitte Zeile selektieren und anpassen!"),
            hr(),
            div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                dataTableOutput("modal_table")
            )
          ),
          easyClose = FALSE, 
          footer = tagList(
            actionButton("modal_select_row", "Zeile editieren"),
            actionButton("abort", "Abbrechen")
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          title = "Daten sind eindeutig.",
          tagList(
          ),
          easyClose = TRUE, 
          footer = tagList(
            actionButton("abort", "Abbrechen")
          )
        )
      )
    }
    # # Trigger the edit_row button click
    # shinyjs::click("check_unique")
  })
  
  ## selected row modal data table ####
  observeEvent(input$modal_select_row, {
    if (!is.null(input$modal_table_rows_selected)){ # comming from add row top / bottom
      
      df_temp <- df_temp_to_render()
      c_ID <- df_temp[input$modal_table_rows_selected,]$ID
      # update latest ID 
      ID_to_edit(c_ID)
      # Store HTML elements
      l_temp <- list()
      # only display
      df_info <- current_data() |> 
        filter(ID == c_ID)|>
        select(1)
      # editable
      df_row <- current_data() |> 
        filter(ID == c_ID)|>
        select(2:ncol(current_data()))
      # Display the display columns (read-only)
      l_temp <- lapply(1:ncol(df_info), function(ii) {
        fluidRow(
          column(6, strong(paste(names(df_info)[ii], ":")), c_ID)
        )
      })
      
      l_temp <- create_modal_input(df_row, l_temp)
      
      removeModal()
      
      # User interaction to save
      showModal(
        modalDialog(
          title = "Zeile editieren",
          l_temp,
          actionButton("edit_row_value", "Werte übernehmen", class = "btn-info"),
          actionButton("abort_save", "Abbrechen"),
          easyClose = FALSE,
          footer = NULL
        )
      )
    } else {
      # User interaction
      showModal(
        modalDialog(
          title = "Bitte eine Zeile markieren",
          easyClose = TRUE,
          footer = modalButton("Abbrechen")
        )
      )
    }
  })
  
  #### Render modal table ####
  output$modal_table <- renderDataTable({
    req(df_temp_to_render())  
    datatable(df_temp_to_render(), 
              rownames = FALSE,
              selection = "single"
    )
  })
  
  ## Edit row ####
  ### Edit row modal Dialog ####
  observeEvent(input$edit_row, {
    if (!is.null(input$table_rows_selected)) {
      # Joined table handling 
      if (lastEdited_data_set_name() %in% c("Einsatzplan")) {
        df_temp <- current_data()
        # filter ID
        df_temp <- df_temp[df_temp[,1] == ID_to_edit(),]
        # Store HTML elements
        l_temp <- list()
        # only display
        df_info <- df_temp |> 
          select(1:6)
        # editable
        df_row <- df_temp|> 
          select(-(1:6))
        # Display the display columns (read-only)
        l_temp <- lapply(1:ncol(df_info), function(ii) {
          fluidRow(
            column(6, strong(paste(names(df_info)[ii], ":")), pull(df_info[, ii]))
          )
        })
      } else {
        df_temp <- current_data()
        # filter ID
        df_temp <- df_temp[df_temp[,1] == ID_to_edit(),]
        # Store HTML elements
        l_temp <- list()
        # only display
        df_info <- df_temp|> 
          select(1)
        df_info
        # editable
        df_row <- df_temp|> 
          select(2:ncol(current_data()))
        # Display the display columns (read-only)
        l_temp <- lapply(1:ncol(df_info), function(ii) {
          fluidRow(
            column(6, strong(paste(names(df_info)[ii], ":")), pull(df_info[, ii]))
          )
        })
      }
      
      if(r_is.defined(l_temp)) {
        cnt <- length(l_temp) + 1
      } else {
        # Store HTML elements
        l_temp <- list()
        cnt <- 0
      }
      # create Modal 
      l_temp <- create_modal_input(df_row, l_temp)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = "Zeile editieren",
          l_temp,
          actionButton("edit_row_value", "Werte übernehmen", class = "btn-info"),
          actionButton("abort_save", "Abbrechen"),
          easyClose = FALSE,
          footer = NULL
        )
      )
    } else {
      # User interaction
      showModal(
        modalDialog(
          title = "Bitte eine Zeile markieren",
          easyClose = TRUE,
          footer = modalButton("Abbrechen")
        )
      )
    }
  })
  
  ### Edit row value action button ####
  observeEvent(input$edit_row_value, {
    shiny::withProgress(message = "login... ", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("data selection", 1, "of 2"))
      
      # get actual data 
      df_temp <- current_data()
      df_temp_ <- current_data()
      
      ### Special user input handling #####
      if(lastEdited_data_set_name() == "Einsatzplan"){
        # select columns to be updated 
        c_select <- 7:ncol(df_temp)
        df_temp <- current_data()[,c_select]
        # input columns
        c_select_input <- 1:7
        # get the user input
        generated_code <- paste0("input$`",c_select_input, "`")
        c_input <- sapply(generated_code, function(x) eval(parse(text = x)))
        names(c_input) <- NULL
        c_input
      } 
      ### standard handling user input ####
      else{
        # get the user input
        generated_code <- paste0("input$`", 1:ncol(df_temp), "`")
        c_input <- sapply(generated_code, function(x) eval(parse(text = x)))
        names(c_input) <- NULL
        c_input
        
        df_temp <- current_data()[,2:ncol(current_data())]
      }
      removeModal()
      
      ### Coerce user input to correct data type ####
      l_input <- list()
      
      for (ii in 1:ncol(df_temp)) {
        c_input_class <- df_temp[input$table_rows_selected,ii]|>pull()|>class()
        c_table_name <- names(df_temp[,ii])
        
        if(length(c_input_class) > 1) c_input_class <- c_input_class[1]
        
        #### handle characters ####
        if(c_input_class == "character") {
          if (c_input[ii] == "" | c_input[ii] == "..."){
            l_input[[ii]] <- as.character(NA)
          } else {
            l_input[[ii]] <- as.character(c_input[ii])
          }
        } 
        #### handle dates ####
        else if (c_input_class == "Date") {
          if(is.na(c_input[ii])){
            l_input[[ii]] <- as.Date(NA)
          }else{
            l_input[[ii]] <- c_input[ii]|>as.integer()|>as.Date()
          }
        } 
        #### numeric inputs ####
        else if (c_input_class %in% c("double", "numeric")) {
          l_input[[ii]] <- as.numeric(c_input[ii])
        } 
        #### integer inputs####
        else if (c_input_class == "integer") {
          l_input[[ii]] <- as.integer(c_input[ii])
        } 
        #### factor or choices inputs ####
        else if (c_input_class == "factor"){
          c_input[ii] <- as.character(c_input[ii])
          if(names(df_temp[,ii]) == "Event ID"){
            if (c_input[ii] == "" | c_input[ii] == "..."){
              l_input[[ii]] <- as.integer(NA)
            } else {
              c_temp <- str_split(c_input[ii], ":")|>
                lapply(function(x){
                  x[[1]]
                })|>
                unlist()|>
                as.integer()
              
              l_input[[ii]] <- as.integer(c_temp)
            }
          }else{
            if ((c_input[ii] == "") | (c_input[ii] == "...") | (c_input[ii] == "NA")){
              l_input[[ii]] <- as.character(NA)
            } else {
              l_input[[ii]] <- as.character(c_input[ii])
            }
          }
          
        } 
        #### time inputs ####
        else if(c_input_class == "hms"){
          c_input[ii] <- as.character(c_input[ii])
          if (c_input[ii] == "" | c_input[ii] == "..."){
            l_input[[ii]] <- NA
          } else {
            # library(rebus)
            # p <- "min"%R%SPC%R%"="%R%SPC%R%capture(one_or_more(DGT))
            p <- "min\\s=\\s([\\d]+)"       
            # c_input[ii][[1]]|>
            #   str_view(pattern = p, html = T)
            c_minutes <- str_match_all(c_input[ii][[1]], pattern = p)|>unlist()
            c_minutes <- c_minutes[2]
            
            # p <- "hour"%R%SPC%R%"="%R%SPC%R%capture(one_or_more(DGT))
            p <- "hour\\s=\\s([\\d]+)"
            # c_input[ii][[1]]|>
            #   str_view(pattern = p, html = T)
            c_hours <- str_match_all(c_input[ii][[1]], pattern = p)|>unlist()
            c_hours <- c_hours[2]
            
            c_time <- paste0(c_hours, ":",c_minutes)
            # Add a leading zero to the minutes if necessary
            c_time <- format(as.POSIXct(c_time, format = "%H:%M"), format = "%H:%M")
            c_time
            l_input[[ii]] <- readr::parse_time(c_time)
          }
        } #### not yet implemented #### 
        else {
          stop(paste("Error\nData type format:", c_input_class, "is not yet implemented."))
        }
      }
      names(l_input) <- names(df_temp)
      df_updated <- l_input|>
        as_tibble()
      
      ### Handle columns containing `ID` in the column name ####
      c_col_is_factor <- df_updated|>
        select(contains("ID"))|>
        names()
      # Convert `ID` columns to character
      if(length(c_col_is_factor) > 0){
        for (ii in 1:length(c_col_is_factor)) {
          df_updated[,names(df_updated) == c_col_is_factor[ii]] <- as.character(df_updated[,names(df_updated) == c_col_is_factor[ii]])
        }    
      }
      
      ### map ID to row index ####
      df_index <- current_data()|>
        select(1)
      
      df_index <- df_index|>
        mutate(index = row_number(),
               select = (df_index|>select(1)|>pull() == ID_to_edit())
        )
      df_index
      
      select_row <- df_index|>
        filter(select == TRUE)|>
        select(index)|>
        pull()
      select_row
      
      ### Handel ID`s ####
      df_updated <- bind_cols(current_data()[select_row,1],
                              df_updated
      )
      df_temp <- bind_cols(current_data()[,1],
                           df_temp
      )
      
      ### check input E-Mail if correct #####
      df_Email <- df_updated[,names(df_temp) == "E-Mail"]
      if(ncol(df_Email) > 0){
        if(!is.na(df_Email$`E-Mail`)){
          # E-Mail regex pattern
          p <- "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}"
          p <- "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
          
          c_select <- df_Email$`E-Mail`|>
            str_detect(pattern = p)
          
          if(!c_select){
            # User interaction 
            removeModal()
            showModal(
              modalDialog(title = "Die E-Mailadresse ist nicht korrekt, bitte korrigieren!",
                          tagList(
                            renderText(df_Email$`E-Mail`),
                            hr(),
                            shiny::textInput("email","E-Mail korrigieren!",value = df_Email$`E-Mail`)
                          ),
                          easyClose = FALSE, 
                          footer = tagList(
                            actionButton("check_email","Speichern"),
                            actionButton("abort","Abbrechen")
                          )
              )
            )
          }
        }
      }
      
      ### check input Suisanummer if correct #####
      df_suisa <- df_updated[select_row,names(df_temp) == "Suisanummer"]
      if(ncol(df_suisa) > 0){
        if(!is.na(df_suisa$Suisanummer)){
          # Suisanummer regex pattern
          p <- "^\\d{4}\\.\\d{3}$"
          
          c_select <- df_suisa$Suisanummer|>
            str_detect(pattern = p)
          
          if(!c_select){
            # User interaction 
            showModal(
              modalDialog(title = "Suisanummer nicht korrekt, bitte korrigieren!",
                          tagList(
                            renderText(df_suisa$Suisanummer),
                            hr(),
                            shiny::textInput("suisa", "Suisanummer korrigieren!", value = df_suisa$Suisanummer)
                          ),
                          easyClose = FALSE, 
                          footer = tagList(
                            actionButton("check_suisa","Speichern"),
                            actionButton("abort","Abbrechen")
                          )
              )
            )
          }
        }
      }
      
      ### handle factors #####
      df_temp <- factor_handling(df_temp, df_updated, select_row)
      
      ### Handling uniqueness checks for Dropdowns ####
      if (data_selection_() == "Dropdowns") {
        # Find duplicates (keeping only duplicate rows)
        df_temp1 <- df_temp |>
          group_by(across(-ID)) |>
          mutate(duplicate_flag = n() > 1) |>
          ungroup() |>
          filter(duplicate_flag)|>
          select(-duplicate_flag)
        
        df_temp_to_render(df_temp1)
        
        if(nrow(df_temp1) > 1){
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_temp1)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_temp1) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = "Achtung die folgenden Zeilen sind nicht eindeutig.",
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Bitte Zeile selektieren und anpassen!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("modal_select_row", "Zeile editieren"),
                actionButton("abort", "Abbrechen")
              )
            )
          )
        }
      }
      
      ### check for changed data #####
      test <- is.logical(all.equal(df_temp, df_temp_))
      if( test ){
        # User interaction 
        showModal(
          modalDialog(title = "Es wurde nichts geändert!",
                      easyClose = TRUE, 
                      footer = actionButton("abort","Abbrechen")
          )
        )
      }else{
        # update data base 
        c_class <- get_data_type(df_temp)
        DB_edit_row_in_table(DB_con(), 
                             lastEdited_data_set_name(), names(df_updated[,1]), pull(df_updated[,1]), df_updated,
                             c_class
        )
        
        ### update joined data sets and choices ####
        if(lastEdited_data_set_name() == "Programm"){
          # Update the list
          l_temp <- l_data()
          l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con()) 
          
          # update all data
          l_data(l_temp)
          # update choices
          update_choices(l_data())|>
            column_choices()
          # update Einsatzplan
          Update_Einsatzplan(df_updated, c_class)
          
          df_temp|>
            current_data()
          
        } else if (lastEdited_data_set_name() == "Einsatzplan"){
          left_join(
            l_data()$Programm|>
              filter(`Verleiher Angefragt?` != "Wird nicht gespielt")|>
              select(1:8, -`Link to Event ID`,-Verleiher), 
            df_temp,
            by = join_by(`Event ID`)
          )|>
            current_data()
        } else {
          df_temp|>
            current_data()
        }
      }
      # Maintain selection 
      shiny::incProgress(1 , detail = paste("data selection", 2, "of 2"))
    })
  })
  
  ## Data checks ####
  ### Check Suisanummer Modal ####
  observeEvent(input$check_suisa,{
    print(input$suisa)
    DB_update_cell(DB_con(), lastEdited_data_set_name(), "Event ID", last_selected_row(), "Suisanummer", input$suisa)
    DB_update_cell(DB_con(), "Einsatzplan", "Event ID", last_selected_row(), "Suisanummer", input$suisa)
    df_temp <- current_data()
    df_temp[last_selected_row(),"Suisanummer"] <- input$suisa
    current_data(df_temp)
    removeModal()
  })
  
  ### Check E-Mail Modal ####
  observeEvent(input$check_email,{
    print(input$email)
    DB_update_cell(DB_con(), lastEdited_data_set_name(), "ID", last_selected_row(), "E-Mail", input$email)
    df_temp <- current_data()
    df_temp[last_selected_row(),"E-Mail"] <- input$email
    current_data(df_temp)
    removeModal()
  })
  
  ## Row Operations (Add/Delete/Duplicate/change title) ####
  ###  add row on top of selected row ####
  observeEvent(input$add_row_top, {
    if (is.null(input$table_rows_selected)) {
      # User interaction
      showModal(
        modalDialog(
          title = "Bitte eine Zeile markieren",
          easyClose = TRUE,
          footer = modalButton("Abbrechen")
        )
      )
    } else {
      # Create an empty row
      new_row <- current_data()[1, ] |> 
        mutate(across(everything(), ~ NA))|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      new_row[1,1] <- max(current_data()[,1]) + 1L
      
      pull(new_row[1,1])|>
        ID_to_edit()
      
      if (input$table_rows_selected == 1) {
        # add row on top
        updated_data <-
          bind_rows(new_row, current_data()[(input$table_rows_selected):nrow(current_data()), ])
      } else{
        updated_data <-
          bind_rows(current_data()[1:(input$table_rows_selected - 1), ], 
                    new_row, 
                    current_data()[(input$table_rows_selected):nrow(current_data()), ]
          )
      }
      
      # updata SQL DB and current data 
      DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
      
      # update joined data sets 
      if(input$dataset == "Programm"){
        c_class <- get_data_type(current_data())
        Update_Einsatzplan(new_row, c_class, new_row = TRUE)
      }
      
      ##### Handling uniqueness checks for Dropdowns #####
      if (data_selection_() == "Dropdowns") {
        # Find duplicates (keeping only duplicate rows)
        df_temp <- updated_data |>
          group_by(across(-ID)) |>
          mutate(duplicate_flag = n() > 1) |>
          ungroup() |>
          filter(duplicate_flag)|>
          select(-duplicate_flag)
        
        df_temp_to_render(df_temp)
        
        if(nrow(df_temp) > 1){
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_temp)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = "Achtung die folgenden Zeilen sind nicht eindeutig.",
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Bitte Zeile selektieren und anpassen!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("modal_select_row", "Zeile editieren"),
                actionButton("abort", "Abbrechen")
              )
            )
          )
        }
      }
      # Update the list
      l_temp <- l_data()
      l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con()) 
      # update all data
      l_data(l_temp)
      # update choices
      update_choices(l_data())|>
        column_choices()
      # update to render 
      current_data(updated_data)
      
      #### select last edited row and page ####
      last_selected_row(last_selected_row() + 1)
    }
  })
  
  ### add row below selected row ####
  observeEvent(input$add_row_bottom, {
    if(is.null(input$table_rows_selected)){ # add row on bottom 
      # User interaction 
      showModal(
        modalDialog(title = "Bitte eine Zeile markieren",
                    easyClose = TRUE, 
                    footer = modalButton("Abbrechen")
        )
      )
    } else {
      # Create an empty row
      new_row <- current_data()[1, ] |> 
        mutate(across(everything(), ~ NA))|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      new_row[1,1] <- max(current_data()[,1]) + 1L
      
      # updata SQL DB
      DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
      
      # update joined data sets 
      if(input$dataset == "Programm"){
        c_class <- get_data_type(current_data())
        Update_Einsatzplan(new_row, c_class, new_row = TRUE)
      }
      
      if(input$table_rows_selected == nrow(current_data())){
        updated_data <- 
          bind_rows(current_data()[1:input$table_rows_selected,],
                    new_row
          )
        
      }else {
        updated_data <- 
          bind_rows(current_data()[1:(input$table_rows_selected),],
                    new_row,
                    current_data()[(input$table_rows_selected + 1):nrow(current_data()),]
          )
        
      }
      ##### Handling uniqueness checks for Dropdowns #####
      if (data_selection_() == "Dropdowns") {
        # Find duplicates (keeping only duplicate rows)
        df_temp <- updated_data |>
          group_by(across(-ID)) |>
          mutate(duplicate_flag = n() > 1) |>
          ungroup() |>
          filter(duplicate_flag)|>
          select(-duplicate_flag)
        
        df_temp_to_render(df_temp)
        
        if(nrow(df_temp) > 1){
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_temp)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = "Achtung die folgenden Zeilen sind nicht eindeutig.",
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Bitte Zeile selektieren und anpassen!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("modal_select_row", "Zeile editieren"),
                actionButton("abort", "Abbrechen")
              )
            )
          )
        }
      }
      # Update the list
      l_temp <- l_data()
      l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con()) 
      # update all data
      l_data(l_temp)
      # update choices
      update_choices(l_data())|>
        column_choices()
      # update to render 
      current_data(updated_data)
    }
  })
  
  ### Duplicate selected row ####
  observeEvent(input$duplicate_row, {
    if(is.null(input$table_rows_selected)){ 
      # User interaction 
      showModal(
        modalDialog(title = "Bitte eine Zeile markieren",
                    easyClose = TRUE, footer = modalButton("Abbrechen")
        )
      )
    } else { 
      # Create duplicate row and add to table
      new_row <- current_data()[input$table_rows_selected, ] |> 
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      new_row
      new_row[1,1] <- max(current_data()[,1]) + 1L
      
      # Update "Gültig ab Datum" to the current system date
      if ("Gültig ab Datum" %in% colnames(new_row)) {
        new_row <- new_row |>
          mutate(`Gültig ab Datum` = Sys.Date())
      }
      # Add new row
      if(input$table_rows_selected == nrow(current_data())){
        updated_data <- 
          bind_rows(current_data()[1:input$table_rows_selected,],
                    new_row
          )
      }else {
        updated_data <- 
          bind_rows(current_data()[1:(input$table_rows_selected),],
                    new_row,
                    current_data()[(input$table_rows_selected + 1):nrow(current_data()),]
          )
      }
      
      # updata SQL DB
      DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
      
      # update joined data sets 
      if(input$dataset == "Programm"){
        c_class <- get_data_type(current_data())
        Update_Einsatzplan(new_row, c_class, new_row = TRUE)
      }
      # Update the list
      l_temp <- l_data()
      l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())
      # update all data
      l_data(l_temp)
      # update choices
      update_choices(l_data())|>
        column_choices()
      # update to render
      current_data(updated_data)
      
      #### select last edited row and page ####
      
    }
  })
  
  ### Duplicate Film and archive (Filmtitel ändern) ####
  observeEvent(input$archive_row,{
    if(!is.null(input$table_rows_selected)){
      req(input$table_rows_selected) # Ensure a row is selected
      new_row <- current_data()[input$table_rows_selected, ] |>
        mutate(`Verleiher Angefragt?` = column_choices()$`Verleiher Angefragt?`[length(column_choices()$`Verleiher Angefragt?`)])
      new_row[1,1] <- max(current_data()[,1]) + 1L
      
      if(nrow(current_data()) == 0){ 
        # Create an empty row
        new_row <- current_data()[1, ] |> 
          mutate(across(everything(), ~ NA))|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
        # updata SQL DB
        DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
        # update joined data sets 
        if(lastEdited_data_set_name() == "Programm"){
          c_class <- get_data_type(current_data())
          Update_Einsatzplan(new_row, c_class, new_row = TRUE)
        }
        
        # create new empty row with correct data type
        updated_data <- new_row
        current_data(updated_data)
      } else { # Add row to data  
        if(input$table_rows_selected == nrow(current_data())){
          updated_data <- 
            bind_rows(current_data()[1:input$table_rows_selected,],
                      new_row
            )|>
            convert_to_template_types(current_data())
        }else {
          updated_data <- 
            bind_rows(current_data()[1:(input$table_rows_selected),],
                      new_row,
                      current_data()[(input$table_rows_selected + 1L):nrow(current_data()),]
            )|>
            convert_to_template_types(current_data())
        }
        # update DB
        DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
        # update joined data sets 
        if(lastEdited_data_set_name() == "Programm"){
          c_class <- get_data_type(current_data())
          Update_Einsatzplan(new_row, c_class, new_row = TRUE)
        }
        # Update the list
        l_temp <- l_data()
        l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con()) 
        # update all data
        l_data(l_temp)
        # update choices
        update_choices(l_data())|>
          column_choices()
        # update to render 
        current_data(updated_data)
        
        #### select last edited row and page ####
        
      } 
    }else{
      # User interaction
      showModal(
        modalDialog(
          title = "Bitte eine Zeile markieren",
          easyClose = TRUE,
          footer = modalButton("Abbrechen")
        )
      )
    }
  })
  
  ### User interaction Delete selected row(s) ####
  #### Modal to delet row ####
  observeEvent(input$delete_row, {
    if(is.null(input$table_rows_selected)){
      
      showModal(modalDialog(
        title = "Bitte eine Zeile markieren!",
        footer = tagList(
          modalButton("Abbrechen")),
        easyClose = TRUE
      ))
    }else{
      showModal(modalDialog(
        title = "Selektierte Zeile löschen?",
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton("confirm_delete", "Löschen")
        ),
        easyClose = TRUE
      ))
    }
  })
  
  #### Delete selected row ####
  observeEvent(input$confirm_delete, {
    req(input$table_rows_selected)
    # if(nrow(current_data()) <= 1){
    #   showModal(modalDialog(
    #     title = "Die letzte Zeile kannn nicht gelöscht werden",
    #     footer = tagList(
    #       modalButton("Abbrechen")
    #     ),
    #     easyClose = TRUE
    #   ))
    # }
    # else {
      # Find ID to delete
      row <- current_data()[input$table_rows_selected, ]
      updated_data <- current_data()
      # Delete
      updated_data <- updated_data[updated_data[,1] !=  row[[1,1]],]
      current_data(updated_data)
      
      # Update SQL
      DB_delete_row(DB_con(), lastEdited_data_set_name(), names(updated_data[,1]), pull(row[,1]))
      if(lastEdited_data_set_name() == "Programm"){
        df_temp <- DB_get_table("Einsatzplan",DB_con())
        DB_delete_row(DB_con(), "Einsatzplan", names(df_temp[,1]), pull(row[,1]))
      }
      
      ##### select last edited page ####
      last_selected_row(NA)

      removeModal()
    # }
  })
  
  ## Dynamic UI ####
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      hr(),
      if(c_connected_to_db()) {
        div(
          style = "width: 100%; overflow-x: auto;",  # Container with scroll
          DTOutput("table", width = "100%")  # Table fills container
        )
      },
      if(c_connected_to_db()) {
        if(data_selection_() == "Inputdaten") {
          tool_box(l_data_input(), lastEdited_data_set_name(), c_select_dropdown_data)
        } else {
          tool_box(l_data_choices(), lastEdited_data_set_name(), c_select_dropdown_data, 2)
        }
      },
      tags$script(HTML("
        $(function() {
          $('#floating-panel').draggable({ handle: '#floating-panel-header' });
        });
      "))
    )
  })
}

# shinyApp(ui = ui, server = server)

# Run the shiny app ####
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5001,
  launch.browser = TRUE
)
