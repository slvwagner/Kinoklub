##### Edit input data for Kinoklub ######
# Shiny app to edit all Kinoklub input data
# The data is stored on a SQL DB. The Password for the DB connection must be stored 
# in a envirnonment variable: DB_PASSWORD_KINOKLUB 
# Find instroction in the readme to set it up for windows or Mac/Linux

library(shiny)
library(shinyjs)
library(shinyTime)
library(DT)
library(viridis)
library(colorspace)
library(tidyverse)

source("source/functions.R")
source("source/SQL/SQL_Functions.R")

# fuction to update all drop down menus choices 
update_choices <- function(l_data) {
  # Mitgliederauswahl für die Einsatzplanung
  Verantwortlich <- l_data$Kinoklubmitglieder|>
    filter(Koordination == pull(l_data$JaNein)[2])|>
    mutate(Mitglied = paste(Vorname, Nachname))|>
    select(Mitglied)
  Verantwortlich <- bind_rows(tibble(Mitglied = "..."),Verantwortlich)|>
    pull()
  
  `Operateur*in` <- l_data$Kinoklubmitglieder|>
    filter(`Operateur*in` == pull(l_data$JaNein)[2])|>
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
  
  # choices list
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
    "Koordination" = l_data$JaNein$Auswahl
  )
}

# Floating tool box function 
tool_box <- function(l_data_input, data_set_select , choices_select = 1, choices = c("Inputdaten", "Dropdowns")) {
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
        actionButton("save_edit", "Speichern", class = "btn-success"),
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
        actionButton("save_edit", "Speichern", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
      )
  }else {
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
      actionButton("save_edit", "Speichern", class = "btn-success"),
      shiny::tags$hr(),
      actionButton("get_email", "Email-Verteiler", class = "btn-info"),
    )
  }
}

# Regex validation function for Suisanummer
validate_suisanummer <- function(input) {
  p <- "^\\d{4}\\.\\d{3}$"
  grepl(p, input)
}
validate_suisanummer(c("1234.562","123.25"))

# factor handling Einsatzplan 
convert_Einsatzplan <- function(df_temp, convert_to){
  if(convert_to == "char"){
    bind_cols(df_temp|>
                select(1:5),
              df_temp|>
                select(6:10)|>
                mutate(across(everything(), as.character)),
              df_temp|>
                select(11:ncol(df_temp))
    )
  } else if(convert_to == "fact"){
    bind_cols(df_temp|>
                select(1:5),
              df_temp |>
                select(6:10) |>
                mutate(across(everything(), factor)), # Apply factor column-wise without coercing to a matrix
              df_temp|>
                select(11:ncol(df_temp))
    )
  }
}

# factor handling Programm 
convert_Programm <- function(df_temp, convert_to){
  if(convert_to == "char"){
    bind_cols(
      df_temp|>
        mutate(Verleiher = as.character(Verleiher),
               `Verleiher Angefragt?` = as.character(`Verleiher Angefragt?`)),
    )
  } else if(convert_to == "fact"){
    df_temp|>
      mutate(Verleiher = factor(Verleiher),
             `Verleiher Angefragt?` = factor(`Verleiher Angefragt?`)
      )
  }
}

# Update Einsatzpan 
Update_Einsatzplan <- function(df_updated, new_row = FALSE) {
  # If the Programm changes Einsatzplan must be updated too
  if(nrow(df_updated) > 1) stop("Update_Einsatzplan shall only contain a single row")
  
  df_temp <- DB_get_table("Einsatzplan", DB_con())|> 
    filter(ID %in% df_updated$ID)|>
    select(-ID, -Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`)
  
  if(nrow(df_temp) >= 1){
    df_temp <- bind_cols(DB_get_table("Programm", DB_con())|>
                            select(ID, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`)|> 
                            filter(ID %in% df_updated$ID),
                          df_temp
                          )
  }else{
    df_temp <- DB_get_table("Einsatzplan", DB_con())|> 
      select(-ID, -Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`)|>
      slice(1)|>
      mutate(Verantwortlich = "", 
             `Operateur*in` = "",
             `Kasse/Bar 1` = "",
             `Kasse/Bar 2` = "",
             `Back-up` = "",
             Kommentar = "",
             Trailer = "")
    
    df_temp <- bind_cols(DB_get_table("Programm", DB_con())|>
                           select(ID, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`)|> 
                           filter(ID %in% df_updated$ID),
                         df_temp
    )
  }
  if (new_row) {
    DB_add_row(DB_con(), "Einsatzplan", df_temp)
  }
  else {
    DB_edit_row_in_table(DB_con(), "Einsatzplan", "ID", df_updated$ID, df_temp)
  }
}

####################### Constants ############################
Email_col_names <- c("Allgemeine Infos erhalten","Kasse / Bar", "Programm") # Email Verteilerauswahl
c_pageLength = 5 # Initial page length
c_lengthMenu = c(5:10, 20, 50, 100) # page length drop down options

# read in data templates (for data type conversion)
l_template <- readRDS("source/SQL/template.Rds")

# Split data to input and dropdown
c_select_input_data <- c(1:3,5,16,14)
c_select_dropdown_data <- c(6:13, 15, 17)

DT_language <- list(
  lengthMenu = "Zeige _MENU_ Einträge pro Seite", # Text für das Dropdown-Menü
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

################# Define UI ################# 
ui <- 
  fluidPage(
    shiny::inputPanel(shiny::headerPanel("Input Kinoklub"),
                      shiny::textInput("user", "Benutzer"),
                      shiny::passwordInput("SQL_PW", "Datenbankpasswort"),
                      shiny::actionButton("SQL_connect", "Mit Datenbank verbinden", class = "btn-success"),
                      shiny::actionButton("SQL_disconnect", "Datenbankverbindung schliessen", class = "btn-danger")
    ),
    includeScript("source/JS/1.12.1_jquery-ui.js"),
    tags$head(
      tags$style(HTML("
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
      ")),
    ),
    # Render the main panel
    shiny::mainPanel(
      shiny::uiOutput("dynamicContent_output_panel"),
    )
  )


####################### Reactive variables ############################
# Reactive lists
l_data_input <- reactiveVal(list())
l_data_choices <- reactiveVal(list())
l_data <- reactiveVal(list())

# drop down choices list
column_choices <- reactiveVal(list())

# Reactive value to store the current data set
current_data <- reactiveVal(tibble())
# app behavior
table_edit <- reactiveVal("single")

# Edited data 
data_selection_ <- reactiveVal("")
# lastEdited_data_set <- reactiveVal(NULL)
lastEdited_data_set_name <- reactiveVal("")

# last edit 
last_selected_row <- reactiveVal(1L)
last_selected_page <- reactiveVal(1L)
page_length_var <- reactiveVal(6L)

# connected to db
c_connected_to_db <- reactiveVal(FALSE)
DB_con <- reactiveVal(NULL)

# System messages 
sys_msg <- reactiveVal("")

###################### server logic #############################
server <- function(input, output, session) {
  
  #### Data set type selection ####
  observeEvent(input$data_selection,{
    data_selection_(input$data_selection)
    if(input$data_selection == "Dropdowns"){
      current_data(l_data()[["Verleiher"]])
      lastEdited_data_set_name("Verleiher")
    }else{
      current_data(l_data()[["Ausgaben"]])
      lastEdited_data_set_name("Ausgaben")
    }
    last_selected_page(NA)
    last_selected_row(NA)
  })
  
  #### Data set selection ####
  observeEvent(input$dataset, {
    print("Data set selection")
    req(input$dataset)
    req(input$data_selection)
    c_input_dataset <- input$dataset
    paste0("change data set to \"", c_input_dataset, "\"")|>
      writeLines()
    
    # read data from data base
    df_temp <- DB_get_table(c_input_dataset,DB_con())|>
      convert_to_template_types(l_template[[c_input_dataset]])
    
    # temp data
    l_temp <- l_data()
    
    # data handling for Programm / Einsatzplan (joined tables)
    if (c_input_dataset %in% c("Programm", "Einsatzplan")){
      l_temp$Einsatzplan <- left_join(df_temp|>
                                        select(ID, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`),
                                      DB_get_table("Einsatzplan",DB_con())|>
                                        convert_to_template_types(l_template[[c_input_dataset]])|>
                                        select(-Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`),
                                      by = join_by(ID)
                                      )
    } 
    
    # Update the list
    l_temp[[c_input_dataset]] <- df_temp
    
    # update all data
    l_data(l_temp)
    
    # update choices
    update_choices(l_data())|>
      column_choices()
    
    if(lastEdited_data_set_name() == c_input_dataset & data_selection_() == input$data_selection){
      dataTableProxy("table")|>
        selectPage(last_selected_page())|>
        selectRows(last_selected_row())
    }else{
      last_selected_page(NA)
      last_selected_row(NA)
    }
    lastEdited_data_set_name(c_input_dataset)
    current_data(l_data()[[c_input_dataset]])
    print(current_data())
  })

  #### Connect to Datea base ####
  observeEvent(input$SQL_connect,{
    print("SQL_connect")
    shiny::withProgress(message = "login... ", value = 0, {
      shiny::incProgress(1 / 3, detail = paste("SQL login", 1, "of 3"))
      req(input$SQL_PW)
      req(input$user)
      tryCatch({
        # Connect to data base 
        DB_connect(pw = input$SQL_PW, DB_user = input$user  , con = DB_con())|>
          DB_con()
        shiny::incProgress(1 / 2, detail = paste("SQL login", 2, "of 3"))
        # get all data as defined in the template l_data
        l_data_sql <- DB_get_Data(l_template, DB_con())
        
        # Convert data types for each table
        convert_DB_to_R(l_data_sql,l_template)|>
          l_data()
        
        # update choices
        update_choices(l_data())|>
          column_choices()
        
        l_data()[c_select_input_data]|>
          l_data_input()
        l_data()[c_select_dropdown_data]|>
          l_data_choices()
        
        current_data(l_data()[["Ausgaben"]])
        lastEdited_data_set_name("Ausgaben")
        data_selection_("Inputdaten")
        c_connected_to_db(TRUE)
        
      },error =  function(e){
        writeLines(e$message)
        showModal(modalDialog(
          title = "Fehler beim Verbinden mit der Datenbank",
          renderText(e$message),
          footer = tagList(
            actionButton("abort","Abbrechen")
          )
        ))
      })
      shiny::incProgress(1 , detail = paste("SQL login", 3, "of 3"))
    })
  })
  
  #### Abort ####
  observeEvent(input$abort,{
    removeModal()
  })
  
  #### Disconnect from DB ####
  observeEvent(input$SQL_disconnect,{
    print("SQL_disconnect")
    dbDisconnect(DB_con())
    c_connected_to_db(FALSE)
    DB_con(NULL)
  })
  
  #### Get email list ####
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
  
  #### Select email verteiler and copy emails to clipboard ####
  observeEvent(input$get_email_verteiler,{
    print("yes")
    generated_code <- paste0("l_data()[[\"Kinoklubmitglieder\"]]|>
        filter(\`",input$Verteiler,"\` == \"ja\")|>
        distinct(Email)|>
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

  #### Abort changes and update ####
  observeEvent(input$abort_save, {
    current_data(l_data()[[input$dataset]])
    lastEdited_data_set_name(input$dataset)
    removeModal()
  })
  
  #### Update changes ####
  observeEvent(input$save_edit, {
    l_temp <- l_data() # get data list
    # joined tables 
    # specific data handling Programm / Einsatzplan
    if (lastEdited_data_set_name() == "Programm"){
      df_temp <- current_data()
      # Update the list with current edits
      l_temp[[lastEdited_data_set_name()]] <- df_temp 
      l_temp$Einsatzplan <- left_join(df_temp|>
                  select(ID, Suisanummer, Filmtitel, Datum, Zeit, `Verleiher Angefragt?`),
                l_temp[["Einsatzplan"]]|>
                  select(-Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`)
                )
    } # anything else
    else { 
      l_temp[[lastEdited_data_set_name()]] <- current_data() # Update the list with current edits

    }
    # create and update tables on SQL
    update_DB_all(l_temp, DB_con())
    
    # get all data as defined in the template l_data
    l_data_sql <- get_Data(l_template, DB_con())
    
    # Convert data types for each table
    l_temp <- convert_DB_to_R(l_data_sql,l_template)
    
    # update data
    l_data(l_temp) 
    
    # update choices
    Verantwortlich <- l_data()$Kinoklubmitglieder|>
      filter(Koordination == pull(l_data()$JaNein)[2])|>
      select(Mitglied)
    Verantwortlich <- bind_rows(tibble(Mitglied = "..."),Verantwortlich)|>
      pull()
    
    `Operateur*in` <- l_data()$Kinoklubmitglieder|>
      filter(`Operateur*in` == pull(l_data()$JaNein)[2])|>
      select(Mitglied)
    `Operateur*in`  <- bind_rows(tibble(Mitglied = "..."),`Operateur*in` )|>
      pull()
    
    `Kasse/Bar` <- l_data()$Kinoklubmitglieder|>
      filter(`Kasse / Bar` == pull(l_data()$JaNein)[2])|>
      select(Mitglied)
    `Kasse/Bar`   <- bind_rows(tibble(Mitglied = "..."),`Kasse/Bar`  )|>
      pull()
    
    list(  
      "Lieferant" = l_data()$Lieferanten$Lieferantenname,
      "Kategorie" = l_data()$Kategorie$Auswahl,
      "Buchungskonto" = l_data()$Buchhaltungskonten$Buchungskontoname,
      "Verleiher" = l_data()$Verleiher$Verleihername,
      "Kinoförderer gratis?" = l_data()$JaNein$Auswahl,
      "Spezialpreis" = l_data()$Spezialpreis$Spezialpreisname,
      "KDM ja oder nein" = l_data()$JaNein$Auswahl,
      "Besucherzahlen an Verleiher gesendet" = l_data()$JaNein$Auswahl,
      "Verleihervertrag abgelegt" = l_data()$JaNein$Auswahl,
      "Verleiher Angefragt?" = l_data()$`Status Filmliste`$`Status Filmliste`,
      "Verantwortlich" = Verantwortlich,
      "Operateur*in" = `Operateur*in`,
      "Kasse/Bar 1" = `Kasse/Bar`,
      "Kasse/Bar 2" = `Kasse/Bar`,
      "Back-up" = `Kasse/Bar`,
      "Allgemeine Infos erhalten" = l_data()$JaNein$Auswahl,
      "Kasse / Bar" = l_data()$JaNein$Auswahl,
      "Programm" = l_data()$JaNein$Auswahl,
      "Sonderevents" = l_data()$JaNein$Auswahl,
      "Marketing" = l_data()$JaNein$Auswahl,
      "Finanzen" = l_data()$JaNein$Auswahl,
      "Sponsoring" = l_data()$JaNein$Auswahl,
      "Koordination" = l_data()$JaNein$Auswahl
    )|>
      column_choices()
    showNotification("Changes saved successfully!", type = "message")
    
    lastEdited_data_set_name(input$dataset)
    current_data(l_data()[[input$dataset]])
    removeModal()
    if(lastEdited_data_set_name() == input$dataset){
        dataTableProxy("table")|>
        selectPage(last_selected_page())|>
        selectRows(last_selected_row())
    }else{
      last_selected_page(NA)
      last_selected_row(NA)
    }

  })

  #### Edit row ####
  observeEvent(input$edit_row, {
    if (!is.null(input$table_rows_selected)) {
      # Joined table handling
      if (lastEdited_data_set_name() %in% c("Einsatzplan")) {
        # Store HTML elements
        l_temp <- list()
        # only display
        df_info <- current_data() |> select(1:6)
        # editable
        df_row <- current_data() |> select(-(1:6))
        
        # Display the display columns (read-only)
        l_temp <- lapply(1:ncol(df_info), function(ii) {
          fluidRow(
            column(6, strong(paste(names(df_info)[ii], ":")), pull(df_info[input$table_rows_selected, ii]))
          )
        })
      } else {
        
        # Store HTML elements
        l_temp <- list()
        # only display
        df_info <- current_data() |> select(1)
        # editable
        df_row <- current_data() |> select(2:ncol(current_data()))
        
        # Display the display columns (read-only)
        l_temp <- lapply(1:ncol(df_info), function(ii) {
          fluidRow(
            column(6, strong(paste(names(df_info)[ii], ":")), pull(df_info[input$table_rows_selected, ii]))
          )
        })
        
        # # Get actual data
        # df_row <- current_data()
      }
      
      if(r_is.defined(l_temp)) {
        cnt <- length(l_temp) + 1
      } else {
        # Store HTML elements
        l_temp <- list()
        cnt <- 0
        }
      
      for (ii in 1:ncol(df_row)) {
        col_name <- names(df_row)[ii]
        col_data_type <- class(pull(df_row[, ii]))[1]
        col_value <- df_row[input$table_rows_selected, ii]|>pull()
        
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
            step = 0.01
          )
        } else if (col_data_type %in% c("integer")) {
          l_temp[[ii + cnt]]  <- numericInput(
            inputId = as.character(ii),
            label = col_name,
            value = ifelse(is.na(col_value), NA, col_value),
            step = 1
          )
        } else if (col_data_type == "factor") {
          col_value <- as.character(col_value)
          c_choices <- unlist(column_choices()[names(column_choices()) == col_name])
          names(c_choices) <- NULL
          
          if (col_name %in% names(column_choices())) {
            l_temp[[ii + cnt]]  <- selectInput(
              inputId = as.character(ii),
              label = col_name,
              choices = c_choices,
              selected = ifelse(is.na(col_value), NA, col_value)
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
  
  #### Edit row value action button ####
  observeEvent(input$edit_row_value, {
    # filter for selected data by user
    df_temp <- current_data()
    df_temp_ <- current_data()
    
    # Special user input handling
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
    # standard handling user input
    else{
      # get the user input
      generated_code <- paste0("input$`", 1:ncol(df_temp), "`")
      c_input <- sapply(generated_code, function(x) eval(parse(text = x)))
      names(c_input) <- NULL
      c_input
      
      df_temp <- current_data()[,2:ncol(current_data())]
    }
    
    # Coerce user input to correct data type 
    l_input <- list()

    for (ii in 1:ncol(df_temp)) {
      c_input_class <- df_temp[input$table_rows_selected,ii]|>pull()|>class()

      if(length(c_input_class) > 1) c_input_class <- c_input_class[1]
      
      # handle characters
      if(c_input_class == "character") {
        if (c_input[ii] == "" | c_input[ii] == "..."){
          l_input[[ii]] <- as.character(NA)
        } else {
          l_input[[ii]] <- as.character(c_input[ii])
        }
      } # handle dates
      else if (c_input_class == "Date") {
        if(is.na(c_input[ii])){
          l_input[[ii]] <- as.Date(NA)
        }else{
          l_input[[ii]] <- c_input[ii]|>as.integer()|>as.Date()
        }
      } # numeric inputs
      else if (c_input_class %in% c("double", "numeric")) {
        l_input[[ii]] <- as.numeric(c_input[ii])
      } # integer inputs 
      else if (c_input_class == "integer") {
        l_input[[ii]] <- as.integer(c_input[ii])
      } # factor or choices inputs
      else if (c_input_class == "factor"){
        c_input[ii] <- as.character(c_input[ii])
        if (c_input[ii] == "" | c_input[ii] == "..."){
          l_input[[ii]] <- as.character(NA)
        } else {
          l_input[[ii]] <- as.character(c_input[ii])
        }
      } # time inputs h:m 00:00
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
      } else {
        stop(paste("Error\nData type format:", c_input_class, "is not yet implemented."))
      }
    }
    names(l_input) <- names(df_temp)
    df_updated <- l_input|>
      as_tibble()
    
    # Handel ID`s
    df_updated <- bind_cols(current_data()[input$table_rows_selected,1],df_updated)
    df_temp <- bind_cols(current_data()[,1],df_temp)
    
    # handle factors 
    if(lastEdited_data_set_name() == "Einsatzplan"){
      df_updated <- 
        bind_cols(
          current_data()[input$table_rows_selected,1:(min(c_select)-1)], 
          df_updated
        )|>
        convert_Einsatzplan(convert_to = "char")
      df_temp <- current_data()|>
        convert_Einsatzplan(convert_to = "char")
      df_temp[input$table_rows_selected,] <- df_updated
      df_temp <- convert_Einsatzplan(df_temp, "fact")
    } else if (lastEdited_data_set_name() == "Programm"){
      df_temp <- convert_Programm(df_temp, "char")
      df_temp[input$table_rows_selected,] <- df_updated
      df_temp <- convert_Programm(df_temp, "fact")
    } else { # anything else 
      df_temp[input$table_rows_selected,] <- df_updated
    }

    # check for changed data 
    if(is.logical(all.equal(df_temp[input$table_rows_selected,], df_temp_[input$table_rows_selected,]))){
      # User interaction 
      showModal(
        modalDialog(title = "Es wurde nichts geändert!",
                    easyClose = TRUE, 
                    footer = actionButton("abort","Abbrechen")
        )
      )
    }else{
      # update data base 
      DB_edit_row_in_table(DB_con(), lastEdited_data_set_name(), "ID", df_updated$ID, df_updated)
      
      # update joined data sets 
      if(input$dataset == "Programm"){
        Update_Einsatzplan(df_updated)
      }
      # update data 
      current_data(df_temp)
      removeModal()
    }
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
  })
  
  #### Abort: Es wurde nichts geändert! ####
  observeEvent(input$abort,{
    removeModal()
  })
  
  #### Add a new row top of selected ####
  observeEvent(input$add_row_top, {
    if (nrow(current_data()) == 0) {
      template <- l_template[[lastEdited_data_set_name()]]
      if (is.null(template))
        stop("could not finde template data to create a new row")
      else {
        current_data(template)
      }
    } else {
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
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])|>
          mutate(ID = max(current_data()$ID) + 1L)

        # updata SQL DB
        DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
        
        # update joined data sets 
        if(input$dataset == "Programm"){
          Update_Einsatzplan(new_row, new_row = TRUE)
        }
        
        if (input$table_rows_selected == 1) {
          # add row on top
          updated_data <-
            bind_rows(new_row, current_data()[(input$table_rows_selected):nrow(current_data()), ])
          current_data(updated_data)
        } else{
          updated_data <-
            bind_rows(current_data()[1:(input$table_rows_selected - 1), ], new_row, current_data()[(input$table_rows_selected):nrow(current_data()), ])
          current_data(updated_data)
        }
      }
      dataTableProxy("table") |>
        selectRows(last_selected_row() + 1) |>
        selectPage(last_selected_page())
    }
  })
  
  #### Add a new row bottom of selected ####
  observeEvent(input$add_row_bottom, {
    if(nrow(current_data()) == 0){ 
      template <- l_template[[lastEdited_data_set_name()]]
      if (is.null(template))
        stop("could not finde template data to create a new row")
      else {
        current_data(template)
      }
    } else { # Add row to data  
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
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])|>
          mutate(ID = max(current_data()$ID) + 1L)
        
        # updata SQL DB
        DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
        
        # update joined data sets 
        if(input$dataset == "Programm"){
          Update_Einsatzplan(new_row, new_row = TRUE)
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
        current_data(updated_data)
      }
    }
    dataTableProxy("table")|>
      selectPage(last_selected_page())|>
      selectRows(last_selected_row())
      
  })
  
  #### Duplicate selected row ####
  observeEvent(input$duplicate_row, {
    if(nrow(current_data()) == 0){ 
      template <- l_template[[lastEdited_data_set_name()]]
      if (is.null(template))
        stop("could not finde template data to create a new row")
      else {
        current_data(template)
      }
    } else { 
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
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])|>
          mutate(ID = max(current_data()$ID) + 1L)

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
          Update_Einsatzplan(new_row, new_row = TRUE)
        }
        current_data(updated_data)
      }
    }
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
    
  })
  
  #### Duplicate Film and archive ####
  observeEvent(input$archive_row,{
    print("here")
    if(!is.null(input$table_rows_selected)){
      req(input$table_rows_selected) # Ensure a row is selected
      new_row <- current_data()[input$table_rows_selected, ] |>
        mutate(`Verleiher Angefragt?` = column_choices()$`Verleiher Angefragt?`[length(column_choices()$`Verleiher Angefragt?`)])|>
        mutate(ID = max(current_data()$ID) + 1L)
      
      if(nrow(current_data()) == 0){ 
        # Create an empty row
        new_row <- current_data()[1, ] |> 
          mutate(across(everything(), ~ NA))|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
        # updata SQL DB
        DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
        # update joined data sets 
        if(lastEdited_data_set_name() == "Programm"){
          Update_Einsatzplan(new_row, new_row = TRUE)
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
          Update_Einsatzplan(new_row, new_row = TRUE)
        }
        
        current_data(updated_data)
      } 
      dataTableProxy("table")|>
        selectRows(last_selected_row())|>
        selectPage(last_selected_page())
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
  
  #### User interaction Delete selected row(s) ####
  observeEvent(input$delete_row, {
    showModal(modalDialog(
      title = "Selektierten Zeile löschen?",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_delete", "Löschen")
      )
    ))
  })
  
  #### Delete selected row ####
  observeEvent(input$confirm_delete, {
    req(input$table_rows_selected)
    if(nrow(current_data()) <= 1){
      showModal(modalDialog(
        title = "Die letzte Zeile kannn nicht gelöscht werden",
        footer = tagList(
          modalButton("Abbrechen")
        )
      ))
    }
    else {
      req(input$table_rows_selected)
      # Find ID to delete
      row <- current_data()[input$table_rows_selected, ]
      
      # Update data
      updated_data <- current_data()|>
        filter(ID != row$ID)
      current_data(updated_data)
      
      # Update SQL
      DB_delete_row(DB_con(), lastEdited_data_set_name(), "ID", row$ID)
      if(lastEdited_data_set_name() == "Programm"){
        DB_delete_row(DB_con(), "Einsatzplan", "ID", row$ID)
      }
      removeModal()
    }
  })

  #### Select a row and finde page ####
  observeEvent(input$table_rows_selected, {
    req(input$table_rows_selected)
    row <- input$table_rows_selected
    writeLines(paste0("Selected row ", row, " in table: ", input$dataset))
    
    # has the page lenght changed? 
    if(!is.null(input$page_length)){
      page_length_var(input$page_length)
    }
    
    # find page
    if(lastEdited_data_set_name() == "Einsatzplan") page <-  round(row / page_length_var()) # don`t know why I need to calculate it differently for Einsatzplan 
    else page <-  ceiling(row / page_length_var())
    writeLines(paste0("page ", page, " in table: ", input$dataset))
    
    if(page == 0) page <- 1
    last_selected_page(page)
    last_selected_row(input$table_rows_selected)
    # select last row
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
  })
  
  #### Change in page length ####
  observeEvent(input$page_length, {
    # update last selected row  
    req(input$table_rows_selected)
    row <- input$table_rows_selected
    # update 
    req(input$page_length)
    page_length_var(input$page_length)
    # update 
    page <-  ceiling(row / page_length_var())
    last_selected_page(page)
    last_selected_row(input$table_rows_selected)
    # select last row
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
  })
  
  #### Render: Dynamically update the floating tool box ####
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      hr(),
      if(c_connected_to_db())  {
        DTOutput("table")
      },
      # Dynamically change Floating tool box to edit data
      if (c_connected_to_db() && data_selection_() == "Inputdaten") {
        if (lastEdited_data_set_name() == "Programm") {
          tool_box(l_data_input(), "Programm")
        } else if (lastEdited_data_set_name() == "Einsatzplan") {
          tool_box(l_data_input(), "Einsatzplan")
        } else if (lastEdited_data_set_name() == "Einnahmen") {
          tool_box(l_data_input(), "Einnahmen")
        } else if (lastEdited_data_set_name() == "Ausgaben") {
          tool_box(l_data_input(), "Ausgaben")
        } else if (lastEdited_data_set_name() == "Spezialpreisekiosk") {
          tool_box(l_data_input(), "Spezialpreisekiosk")
        } else if (lastEdited_data_set_name() == "Einkauf Kiosk") {
          tool_box(l_data_input(), "Einkauf Kiosk")
        } else if (lastEdited_data_set_name() == "Ausgaben") {
          tool_box(l_data_input(), "Ausgaben")
        }else {
          stop("tool_box not yet implemented")
        }
      } else if (c_connected_to_db() && data_selection_() == "Dropdowns") {
        if(lastEdited_data_set_name() == "Verleiher"){
          tool_box(l_data_choices(), "Verleiher",2)
        } else if (lastEdited_data_set_name() == "Buchhaltungskonten"){
          tool_box(l_data_choices(), "Buchhaltungskonten",2)
        } else if (lastEdited_data_set_name() == "Kategorie"){
          tool_box(l_data_choices(), "Kategorie",2)
        } else if (lastEdited_data_set_name() == "Spezialpreis"){
          tool_box(l_data_choices(), "Spezialpreis", 2)
        } else if (lastEdited_data_set_name() == "JaNein"){
          tool_box(l_data_choices(), "JaNein",2)
        } else if (lastEdited_data_set_name() == "Lieferanten"){
          tool_box(l_data_choices(), "Lieferanten",2)
        } else if (lastEdited_data_set_name() == "Platzkategorien zum Verrechnen"){
          tool_box(l_data_choices(), "Platzkategorien zum Verrechnen",2)
        } else if (lastEdited_data_set_name() == "MWST"){
          tool_box(l_data_choices(), "MWST",2)
        } else if (lastEdited_data_set_name() == "Status Filmliste"){
          tool_box(l_data_choices(), "Status Filmliste",2)
        } else if (lastEdited_data_set_name() == "Kinoklubmitglieder"){
          tool_box(l_data_choices(), "Kinoklubmitglieder",2)
        } else {
          stop("tool_box not yet implemented dataset")
        }
      } else {
        
      },
      
      # JavaScript to make the floating panel draggable
      tags$script(HTML("
        $(function() {
          $('#floating-panel').draggable({ handle: '#floating-panel-header' });
        });
      "))
    )
  })
  
  #### Render: data table output ####
  output$table <- DT::renderDT({
    if(lastEdited_data_set_name() != ""){
      # rendering the datatable depens on the input data 
      # for certain input data sets other renderings may be needed
      if(data_selection_() == "Inputdaten") { # for all Input date change to user readable "Datum"
        print("Render Inputdaten")
        # get current data
        df_temp <- current_data()
        # find all column names containing "Datum"
        df_Date <- current_data()|>
          select(contains("datum"))
        
        # create option list for datatable function
        if(ncol(df_Date) > 0){
          # create user readable Datum
          df_Date_user <-
            df_Date|>
            as.matrix()|>
            apply(2, function(x){
              x <- as.Date(x)
              x <- format(x, "%d.%m.%Y")
              return(x)
            })
          df_Date_user <- df_Date_user|>
              as_tibble()
  

          names(df_Date_user) <-  paste0(as.character(1:ncol(df_Date_user)))
          # Insert user readable Datum 
          run <- TRUE
          ii <- 1
          while(run){
            if(names(df_temp)[ii] %in% names(df_Date)){
              for (jj in 1:ncol(df_Date)) {
                if(names(df_temp)[ii] == names(df_Date)[jj]){
                  if(ncol(df_temp) == ii){
                    df_temp <-
                      bind_cols(
                        df_temp[,1:ii],
                        df_Date_user[, jj]
                      )
                  }else{
                    df_temp <-
                      bind_cols(
                        df_temp[,1:ii],
                        df_Date_user[, jj],
                        df_temp[,(ii+1):ncol(df_temp)]
                      )
                  }
                  ii <- ii + 1
                }
              }
            }
            if(ncol(df_temp) <= ii) run <- FALSE
            ii <- ii + 1
          }
          # Select user readable datum columns
          c_select <- names(df_temp)|>as.integer()|>
            suppressWarnings()
          c_select
          
          # create option list for datatable function
          l_columnDefs <- list()
          cnt <- 1
          for (ii in 1:length(c_select)) {
            if(!is.na(c_select)[ii]){
              l_columnDefs <- append(l_columnDefs, list(
                list(targets = ii - 1, visible =  FALSE),   # Hide the 'Datum' column
                list(targets = ii , orderData = ii-1)     # Use the 'Datum' column for sorting 'Datum_display'
              ))
              names(df_temp)[c(ii - 1,ii)] <- names(df_temp)[c(ii ,ii-1)]
              cnt <- cnt + 2
            }
          }
        }else {
          # create empty option list for datatable function
          l_columnDefs <- list()
        }
        
        # custom search pre set 
        if(lastEdited_data_set_name() == "Einsatzplan"){
          
          c_choices <- DB_get_table("Programm",DB_con())|>
            filter(`Verleiher Angefragt?` != "Wird nicht gespielt")|>
            distinct(`Verleiher Angefragt?`)|>
            pull()
          c_choices
          
          if(length(c_choices) == 1){
            c_choices <- paste0("[\"",c_choices,"\"]")
          }else{
            c_choices <- paste0("[",paste0("\"", c_choices,"\"", collapse = ","),"]")
          }
          writeLines(c_choices)
          
          c_select <- names(df_temp) == "Verleiher Angefragt?"
          c_col <- tibble(column = c_select)|>
            mutate(index = row_number())|>
            filter(column == TRUE)|>
            select(index)|>
            pull()
          # offset needed becaus datatable starts at index 0
          c_select <- c(FALSE,c_select)
          l_filter <- list(NULL)
          # create filters for data table
          for (ii in 1:(length(c_select))) {
            if(c_select[ii]) {
              l_filter[[ii]] <- list(search = c_choices)
              } 
            else {
              l_filter[[ii + 1]] <- NULL
              }
          }
          
          }else { # empty list if no filter needs to be applyed
            l_filter <- list()
          }
        
          # Create the DataTable
          dt <- datatable(
            df_temp,
            editable = FALSE, # Nicht bearbeitbar
            selection = "single", # only select sinle row
            filter = "top", # Filter oben
            options = list(
              columnDefs = l_columnDefs, # Spaltendefinitionen
              pageLength = page_length_var(), # Anzahl der Zeilen pro Seite
              lengthMenu = c_lengthMenu, # Dropdown-Menü für Zeilenanzahl
              searchCols = l_filter, # custom filtering
              # observe the page lenght from data table
              initComplete = JS(
                "function(settings, json) {",
                "  var table = settings.oInstance.api();",
                "  table.on('length.dt', function(e, settings, len) {",
                "    Shiny.setInputValue('page_length', len);",
                "  });",
                "}"
              ),
              language = DT_language
            )
          )
          req(input$dataset)
        
          # Apply conditional formatting for different data sets
          if (!is.null(input$dataset) && input$dataset == "Programm") {
            tryCatch({
              dt <- dt |>
                formatStyle(
                  "Verleiher Angefragt?",  # Ensure this column name matches exactly
                  backgroundColor = styleEqual(
                    levels = c("Bestätigt", "Wird nicht gespielt", "Anfrage läuft"),  # Exact values from your column
                    values = c('lightgreen', '#ed716d', '#FFFF97')  # Corresponding colors
                  )
                )
              
            }, error = function(e) {
              paste0(
                "Conditionall formating error:\n",
                e$message
              )|>sys_msg()
              
            })
          } else if (!is.null(input$dataset) & input$dataset == "Einsatzplan"){
            c_Kinoklubmitglied <- 
              l_data()[["Kinoklubmitglieder"]]|>
              filter(!is.na(`Kasse / Bar`))|>
              mutate(Mitglied = paste(Vorname, Nachname))|>
              select(Mitglied)|>
              pull()
            
            c_Kinoklubmitglied <- ifelse(c_Kinoklubmitglied == "NA NA", NA, c_Kinoklubmitglied)
            c_Kinoklubmitglied <- c_Kinoklubmitglied[!is.na(c_Kinoklubmitglied)]
            # Generate the magma color palette s
            magma_colors <- viridis(length(c_Kinoklubmitglied), option = "turbo")
            
            # Lighten the colors to create a pastel effect
            pastel_magma <- lighten(magma_colors, amount = 0.6)  # Adjust `amount` for more/less pastel effect
            
            # Apply conditional formatting to columns
            tryCatch({
              dt <- dt |>
                formatStyle(
                  "Verantwortlich",  # Ensure this column name matches exactly
                  backgroundColor = styleEqual(
                    levels = c_Kinoklubmitglied,  # Exact values from your column
                    values = pastel_magma  # Corresponding colors
                  )
                )|>
                formatStyle(
                  "Kasse/Bar 1",  # Ensure this column name matches exactly
                  backgroundColor = styleEqual(
                    levels = c_Kinoklubmitglied,  # Exact values from your column
                    values = pastel_magma  # Corresponding colors
                  )
                )|>
                formatStyle(
                  "Kasse/Bar 2",  # Ensure this column name matches exactly
                  backgroundColor = styleEqual(
                    levels = c_Kinoklubmitglied,  # Exact values from your column
                    values = pastel_magma  # Corresponding colors
                  )
                )|>
                formatStyle(
                  "Operateur*in",  # Ensure this column name matches exactly
                  backgroundColor = styleEqual(
                    levels = c_Kinoklubmitglied,  # Exact values from your column
                    values = pastel_magma  # Corresponding colors
                  )
                )|>
                formatStyle(
                  "Back-up",  # Ensure this column name matches exactly
                  backgroundColor = styleEqual(
                    levels = c_Kinoklubmitglied,  # Exact values from your column
                    values = pastel_magma  # Corresponding colors
                  )
                )
              
            }, error = function(e) {
              showModal(modalDialog(
                title = "Fehler beim Verbinden mit der Datenbank",
                renderText(e$message),
                footer = tagList(
                  actionButton("abort","Abbrechen")
                )
              ))
            })
          }
      } 
      else {
        print("Render Dropdowns")
        # Create the DataTable for all other data sets
        dt <- datatable(
          current_data(),
          editable = FALSE, # Nicht bearbeitbar
          selection = "single", # only select sinle row
          filter = "top", # Filter oben
          options = list(
            # columnDefs = l_columnDefs, # Spaltendefinitionen
            pageLength = page_length_var(), # Anzahl der Zeilen pro Seite
            lengthMenu = c_lengthMenu, # Dropdown-Menü für Zeilenanzahl,
            # observe the page lenght
            initComplete = JS( 
              "function(settings, json) {",
              "  var table = settings.oInstance.api();",
              "  table.on('length.dt', function(e, settings, len) {",
              "    Shiny.setInputValue('page_length', len);",
              "  });",
              "}"
            ),
            language = DT_language
          )
        )
      }
      # render dt (data table)
      dt
    }
  })
}

shinyApp(ui = ui, server = server)
 
# #### Run the shiny app ####
# shiny::runApp(
#   host = "0.0.0.0",
#   shiny::shinyApp(ui = ui, server = server),
#   port = 5001,
#   launch.browser = TRUE
# )
