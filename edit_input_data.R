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

# Mapping Verleiher-Procinema zu Verleiher
dict_env <- new.env()

# Constants ####
Email_col_names <- c("Allgemeine Infos erhalten","Kasse / Bar", "Programm") # Email Verteilerauswahl
c_pageLength = 5 # Initial page length
c_lengthMenu = c(5,10,15,20, 50, 100) # page length drop down options

width_vectors <- list(# Define width vectors for specific tables
  "Filmvorschlag" = c("Filmtitel" = "200px", "Inhalt" = "700px", "actors" = "100px"),
  "Programm" = c("Filmtitel" = "200px"),
  "Einsatzplan" = c("Verantwortlich" = "150px", "Operateur*in" = "150px"),
  "Eintritt files" = c("file content" = "800px"),
  "Kiosk files" = c("file content" = "800px")
)

# Data templates (for data type conversion) ####
l_template <- readRDS("source/SQL/template.Rds")

# Split data ####
## Input tables ####
c_select_input_data <- 
  c("Filmvorschlag","Programm", "Einsatzplan", "Einnahmen", "Ausgaben", "Spezialpreisekiosk", "Einkauf Kiosk")
l_template[c_select_input_data]

## Avanced tickets files ####
c_select_input_advanced_tickets <- c("Eintritt files", "df_Eintritt", "Kiosk files","df_Kiosk")
l_template[c_select_input_advanced_tickets]

## Drop down data and calculation definitions ####
c_select_dropdown_data <- 
  c("Kinoklubmitglieder", "Verleiher", "Verleiher mapping", "Lieferanten", 
    "Platzkategorien zum Verrechnen", "Buchhaltungskonten", "Spezialpreis", "MWST")
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

# Serve the custom_styles directory for css files
shiny::addResourcePath("custom_styles", "source/www")

# Define UI ####
ui <- fluidPage(
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles/Kinoklub_dark_edit.css"),
    tags$script(src="https://code.jquery.com/ui/1.14.1/jquery-ui.js",
                integrity="sha256-9zljDKpE/mQxmaR4V2cGVaQ7arF3CcXxarvgr7Sj8Uc=",
                crossorigin="anonymous"
                ),
    tags$style(HTML("
      .custom-select .selectize-dropdown-content {
        background-color: #330937;
        color: #f4eacc;
      }
      
      .selectize-input.full {
        background-color: #330937 !important;
        color: #f4eacc !important;
      }
      
      .custom-select .selectize-dropdown .active {
        background-color: #330937 ;
        color: #f4eacc ;
      }
      
      #floating-panel {
        position: fixed;
        right: 20px;
        top: 20px;
        width: 300px;
        height: auto; /* Start with auto height */
        border: 3px solid #000;
        border-radius: 5px;
        padding: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        z-index: 1001;
        transition: height 0.2s ease;
        overflow: hidden; /* Hide content when collapsed */
      }
      
      #floating-panel.collapsed {
        height: 38px; /* Just enough for the header */
      }
      
      #floating-panel-header {
        cursor: move;
        padding: 10px;
        margin: -10px -10px 10px -10px;
        border-bottom: 1px solid #ddd;
        font-weight: bold;
        border-radius: 5px 5px 0 0;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      #floating-panel.collapsed .panel-content {
        display: none;
      }
      
      #floating-panel.collapsed #floating-panel-header {
        margin-bottom: -10px; /* Adjust for collapsed state */
        border-bottom: none; /* Remove border when collapsed */
      }
      
      .toggle-panel {
        cursor: pointer;
        float: right;
      }
  
      #login-panel {
        background: #330937;
        position: absolute;
        left: 350px;
        top: 20px;
        width: 300px;
        height: auto;
        border: 3px solid #000;
        border-radius: 5px;
        padding: 10px;
        color: #f4eacc;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        z-index: 1000;
        transition: height 0.2s ease;
        overflow: hidden;
      }
      
      #login-panel.collapsed {
        height: 35px;
      }
      
      #login-panel-header {
        cursor: move;
        background: #46267d;
        padding: 8px;
        margin: -10px -10px 10px -10px;
        border-bottom: 1px solid #ddd;
        font-weight: bold;
        border-radius: 5px 5px 0 0;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      #login-panel.collapsed .panel-content {
        display: none;
      }
      
      #login-panel.collapsed #login-panel-header {
        margin-bottom: -10px;
        border-bottom: none;
      }
      
      .login-toggle-panel {
        cursor: pointer;
        float: right;
      }
  "))),
  
  # Functions
  shiny::tags$head(
    # Initialize drag and collapse functionality for the login panel
    tags$script(HTML("
    $(function() {
      // Wait for Shiny to be ready
      $(document).on('shiny:connected', function() {
        // Make login panel draggable
        $('#login-panel').draggable({ handle: '#login-panel-header' });
        
        // Toggle login panel collapse/expand
        $('#login_togglePanel').click(function(e) {
          e.stopPropagation();
          $('#login-panel').toggleClass('collapsed');
          if ($('#login-panel').hasClass('collapsed')) {
            $('#login_togglePanel').html('<i class=\"fa fa-plus\"></i>');
          } else {
            $('#login_togglePanel').html('<i class=\"fa fa-minus\"></i>');
          }
        });
      });
    });
    // Function to collapse login panel
    function collapseLoginPanel() {
      $('#login-panel').addClass('collapsed');
      $('#login_togglePanel').html('<i class=\"fa fa-plus\"></i>');
    }
    
    // Make this function available to Shiny
    Shiny.addCustomMessageHandler('collapseLoginPanel', function(message) {
      collapseLoginPanel();
    });
  ")),
  ),
  


  # Login Panel UI
  shiny::titlePanel(paste0("Input Daten Kinoklub")),
  div(
    id = "login-panel",
    tags$div(id = "login-panel-header", 
             "Login",
             span(class = "toggle-panel", id = "login_togglePanel", icon("minus"))
    ),
    div(class = "panel-content",
        # Input panel at top
        shiny::textInput("DB_host", "Datenbank Host", value = "lx51.hoststar.hosting"),
        shiny::textInput("DB_name", "Datenbank Name", value = "ch367079_gui"),
        shiny::textInput("DB_user", "Datenbank Benutzer"),
        shiny::passwordInput("DB_pw", "Datenbankpasswort"),
        shiny::actionButton("SQL_connect", "Mit Datenbank verbinden", class = "btn-success"),
        shiny::actionButton("SQL_disconnect", "Datenbankverbindung schliessen", class = "btn-danger")
        ),
  ),

  # Main content area
  div(class = "table-container",
      uiOutput("dynamicContent_output_panel")
  )
)

# Define server ####
server <- function(input, output, session) {
  ## Reactive Values ####
  l_data_input <- reactiveVal(list())
  l_data_advance_tickets <- reactiveVal(list())
  l_data_dropdown <- reactiveVal(list())
  l_data <- reactiveVal(list())
  column_choices <- reactiveVal(list())
  ### data frame to render ####
  current_data <- reactiveVal(tibble())
  data_selection_ <- reactiveVal("")
  ### render helping infromation ####
  help_information <- reactiveVal("")
  ### last page length from datatable ####
  page_length_var <- reactiveVal(5L)
  ### last selected ID in datatable ####
  ID_to_edit <- reactiveVal(1L)
  ### Is the database connection available ####
  c_connected_to_db <- reactiveVal(FALSE)
  
  ### Database connection ####
  DB_con <- shiny::reactiveVal(NULL)
  ### Database host ####
  DB_host <- shiny::reactiveVal(NULL)
  ### Database name ####
  DB_name <- shiny::reactiveVal(NULL)
  ### Database user ####
  DB_user <- shiny::reactiveVal(NULL)
  ### Database password ####
  DB_pw <- shiny::reactiveVal(NULL)
  
  ### temp datatable to render to render modal ####
  df_temp_to_render <- reactiveVal(NULL)
  ### last date frame that has been rendered ####
  last_rendered_DT <- reactiveVal(NULL)
  ### last date frame name ####
  lastEdited_data_set_name <- reactiveVal("")
  ### last selected row in datatable ####
  last_selected_row <- reactiveVal(1L)
  ### last selected page in datatable ####
  last_selected_page <- reactiveVal(1L)
  ### last set user filer in datatable ####
  last_user_filter <- reactiveVal(NULL)
  ### last set sorting in datatable ####
  last_sorting <- reactiveVal(NULL)
  
  ### temp_01 ####
  temp_01 <- reactiveVal(NULL)
  ### temp_02 ####
  temp_02 <- reactiveVal(NULL)
  # new entry use case ####
  new_entry <- reactiveVal(NULL)
  
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
      "Verleihername" = l_data$Verleiher$Verleihername,
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
      "Link to Event ID" = c("...",paste(l_data$Programm$`Event ID`, ":", l_data$Programm$Filmtitel)),
      "Abrechnungsjahr" = l_data$MWST$Abrechnungsjahr[length(l_data$MWST$Abrechnungsjahr):1],
      "Firmennamen" = c("...",l_data$Verleiher$Verleihername),
      "Personal" = 
        c("...", l_data$Kinoklubmitglieder|>
            filter(Personal)|>
            mutate(Personal = paste(Vorname, Nachname,", ", `E-Mail`))|>
            select(Personal)|>
            pull()
          )
    )
  }
  
  ### Toolbox for the user to interact ####
  tool_box <- function(l_data_input, data_set_select , c_select_dropdown_data, choices_select = 1, choices = c("Inputdaten", "Advance-Tickets", "Dropdowns")) {
    #### Filmvorschlag ####
    if(data_set_select == "Filmvorschlag"){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectizeInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
                        )
            ),
        # Function selection
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("add_row", "Eintrag hinzufügen", class = "btn-info"),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        # actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
        # actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("procinema_search", "Procinema-Suche", class = "btn-info"),
        actionButton("add_to_programm", "ins Programm übernehmen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("check_unique", "Prüfen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      ) 
    } 
    #### Programm ####
    else if (data_set_select == "Programm"){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectizeInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("add_row", "Eintrag hinzufügen", class = "btn-info"),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        # actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
        # actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
        actionButton("duplicate_row", "Zeile duplizieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("archive_row", "Filmtitel ändern", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("check_unique", "Prüfen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    } 
    #### Einsatzplan ####
    else if(data_set_select == "Einsatzplan"){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    } 
    #### Ausgaben ####
    else if (data_set_select == "Ausgaben"){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectizeInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("add_row_ausgaben", "Eintrag hinzufügen", class = "btn-info"),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        # actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
        # actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
        actionButton("duplicate_row", "Zeile duplizieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("archive_row", "Filmtitel ändern", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("check_unique", "Prüfen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    } 
    ### Spezialpreisekiosk ####
    else if (data_set_select == "Spezialpreisekiosk") {
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection 
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("add_row", "Eintrag hinzufügen", class = "btn-info"),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("check_unique", "Prüfen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    }
    ##### Kinoklubmitglieder ####
    else if(lastEdited_data_set_name() == "Kinoklubmitglieder"){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("add_row", "Eintrag hinzufügen", class = "btn-info"),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("check_unique", "Prüfen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    }
    ##### df_Eintritt df_Kiosk ####
    else if(lastEdited_data_set_name() %in% c("df_Eintritt", "df_Kiosk","Eintritt files", "Kiosk files")){
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection 
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    }
    ### anything else ####
    else if (lastEdited_data_set_name() == "Einkauf Kiosk") {
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection 
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("add_row", "Eintrag hinzufügen", class = "btn-info"),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        # actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
        # actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
        actionButton("duplicate_row", "Neuer Einkaufpreis", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("check_unique", "Prüfen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    }
    ### anything else ####
    else {
      tags$div(
        id = "floating-panel",
        tags$div(id = "floating-panel-header", 
                 "Werkzeuge",
                 span(class = "toggle-panel", id = "togglePanel", icon("minus"))
        ),
        div(class = "custom-select",
            selectInput("dataset", "Datensatz zum Editieren", selected = data_set_select, choices = names(l_data_input)
            )
        ),
        # Function selection 
        shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                            choices = choices, selected = choices[choices_select]
        ),
        shiny::tags$hr(),
        actionButton("add_row", "Eintrag hinzufügen", class = "btn-info"),
        actionButton("edit_row", "Zeile editieren", class = "btn-info"),
        shiny::tags$hr(),
        # actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
        # actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
        actionButton("duplicate_row", "Zeile duplizieren", class = "btn-info"),
        shiny::tags$hr(),
        actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
        shiny::tags$hr(),
        actionButton("check_unique", "Prüfen", class = "btn-success"),
        shiny::tags$hr(),
        actionButton("get_email", "Email-Verteiler", class = "btn-info"),
        shiny::downloadButton("table_export", "Tabelle herunterladen")
      )
    }
  }
  

  
  ### Convert data frame columns to factors ####
  factor_handling <- function(df_temp, df_updated){
    
    if(ncol(df_temp) != ncol(df_updated)) stop("ncol(df_temp): " , ncol(df_temp), " ncol(df_updated): ", ncol(df_updated))
    
    # find class of column
    c_class <- get_data_type(df_temp)
    # convert factors to character
    for (ii in 1:length(c_class)) {
      if(c_class[ii] == "factor"){
        df_temp[,ii] <- df_temp[,ii]|>pull()|>as.character()
        df_updated[,ii] <- df_updated[,ii]|>pull()|>as.character()
      }
    }
    # find row
    select_row <- pull(df_temp[,1]) == pull(df_updated[1,1])
    
    # Update selected row 
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
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
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
        select(Verantwortlich, `Operateur*in`, `Kasse/Bar 1`, `Kasse/Bar 2`, `Back-up`, Kommentar)|>
        slice(1)|>
        mutate(across(everything(), ~ as.factor(NA)))
      df_temp
      
      df_temp <- bind_cols(DB_get_table("Programm", DB_con())|>
                             select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, Procinema, Trailer, `Verleiher Angefragt?`)|> 
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
  
  ### Create user modal input ####
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
      }  else if (col_data_type == "logical"){
        l_temp[[ii + cnt]]  <- shiny::checkboxInput(
          inputId = as.character(ii),
          label = col_name,
          value = ifelse(is.na(col_value), FALSE, col_value)
        )
      } else if (col_data_type == "hms") {
        l_temp[[ii + cnt]]  <- timeInput(
          inputId = as.character(ii),
          label = col_name,
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
        c_choices <- (column_choices()[names(column_choices()) == col_name])
        
        if(length(c_choices) == 0) stop("could not find the column choice for the column: ", col_name)
        
        c_choices <- c_choices|>
          as_tibble()|>
          pull()
        
        if(col_name %in% c("Event ID","Link to Event ID")){
          c_select <- str_split(c_choices,":", simplify = T)[,1]|>
            as.integer()|>
            suppressWarnings()
          names(c_select) <- c_choices
          
          l_temp[[ii + cnt]]  <-
            # div(
            #   class = "custom-select",
              selectInput(
                inputId = as.character(ii),
                label = col_name,
                choices = c_select,
                selected = ifelse(is.na(col_value), NA, col_value),
                selectize = TRUE
              )
            # )
        } else if (col_name %in% names(column_choices())) {
          l_temp[[ii + cnt]]  <-
            # div(
            #   class = "custom-select",
              selectInput(
                inputId = as.character(ii),
                label = col_name,
                choices = c_choices,
                selected = ifelse(is.na(col_value), NA, col_value),
                selectize = TRUE
              )
            # )
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
  
  ### get data from user modal input ####
  get_data_from_modal <- function(df_temp) {
    
    # get the user input
    generated_code <- paste0("input$`", 1:ncol(temp_02()), "`")
    c_input <- sapply(generated_code, function(x) eval(parse(text = x)))
    names(c_input) <- NULL

    #### Coerce user input to correct data type ####
    l_input <- list()
    
    for (ii in 1:ncol(df_temp)) {
      c_input_class <- df_temp[input$table_rows_selected,ii]|>pull()|>class()
      c_table_name <- names(df_temp[,ii])
      
      if(length(c_input_class) > 1) c_input_class <- c_input_class[1]
      
      ##### handle characters ####
      if(c_input_class == "character") {
        l_input[[ii]] <- as.character(c_input[ii])
      } 
      ##### handle dates ####
      else if (c_input_class == "Date") {
        if(is.na(c_input[ii])){
          l_input[[ii]] <- as.Date(NA)
        }else{
          l_input[[ii]] <- c_input[ii]|>as.integer()|>as.Date()
        }
      } 
      ##### numeric inputs ####
      else if (c_input_class %in% c("double", "numeric")) {
        l_input[[ii]] <- as.numeric(c_input[ii])
      } 
      ##### integer inputs ####
      else if (c_input_class == "integer") {
        l_input[[ii]] <- as.integer(c_input[ii])
      } 
      ##### factor or choices inputs ####
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
          if(is.na(c_input[ii])){
            l_input[[ii]] <- NA
          } else {
            if ((c_input[ii] == "") | (c_input[ii] == "...") | (c_input[ii] == "NA")){
              l_input[[ii]] <- NA
            } else {
              l_input[[ii]] <- as.character(c_input[ii])
            }
          } 
        }
      } 
      ##### time inputs ####
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
      } ##### logical inputs ####
      else if (c_input_class == "logical"){
        l_input[[ii]] <- as.logical(c_input[ii]|>unlist())
      }
      #### not yet implemented #### 
      else {
        stop(paste("Error\nData type format:", c_input_class, "is not yet implemented."))
      }
    }
    names(l_input) <- names(df_temp)
    df_updated <- l_input|>
      as_tibble()
    return(df_updated)
  }
  
  ### load all initially needed data before starting up ####
  load_initial_data <- function() {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    
    shiny::withProgress(message = "Loading data...", value = 0, {
      shiny::incProgress(1/3, detail = "Fetching from database")
      
      # initialize dictionary Verleiher to Procinema-Verleiher
      df_mapping <- DB_get_table("Verleiher mapping", DB_con()) |>
        select(-ID)
      dict_env <<- dict_from_data.frame(df_mapping)
      
      # Get all data from DB using your template
      l_data_sql <- DB_get_Data(l_template, DB_con())
      
      shiny::incProgress(1/3, detail = "Preparing data")
      
      # Convert data types for each table
      l_data_ready <- convert_DB_to_R(l_data_sql, l_template)
      
      # Update ALL reactive values at once to prevent multiple triggers
      isolate({
        l_data(l_data_ready)
        update_choices(l_data_ready) |> column_choices()
        l_data_input(l_data_ready[c_select_input_data])
        l_data_dropdown(l_data_ready[c_select_dropdown_data])
        current_data(l_data_ready[["Programm"]] |> arrange(desc(Datum)))
        lastEdited_data_set_name("Programm")
        data_selection_("Inputdaten")
      })
      
      shiny::incProgress(1/3, detail = "Finalizing")
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
          ),
          color = styleEqual(
            c("Bestätigt", "Wird nicht gespielt", "Anfrage läuft"), 
            c('black', 'black', 'black')
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
      # Verleiher anfrage
      dt <- dt |> 
        formatStyle(
          "Verleiher Angefragt?", 
          backgroundColor = styleEqual(
            c("Bestätigt", "Wird nicht gespielt", "Anfrage läuft"), 
            c('lightgreen', '#ed716d', '#FFFF97')
          ),
          color = styleEqual(
            c("Bestätigt", "Wird nicht gespielt", "Anfrage läuft"), 
            c('black', 'black', 'black')
          )
        )
    } 
    return(dt)
  }
  


  ## Render data table ####
  output$table <- DT::renderDT({
    writeLines("renderDT")

    if(is.null(current_data())){
      print("here")
    }
    req(current_data())
    
    # Create User-Readable "Datum" Columns 
    df_temp <- current_data()
    stopifnot(is.data.frame(df_temp))
    
    # Step 1: Identify "datum" columns
    datum_cols <- names(df_temp)[stringr::str_detect(names(df_temp), regex("datum", ignore_case = TRUE))]
    
    # Step 2: Only proceed if there are any "datum" columns
    l_columnDefs <- list()
    
    if (length(datum_cols) > 0) {
      # Step 3: Format them as dd.mm.yyyy
      df_datum_user <- df_temp |>
        dplyr::select(all_of(datum_cols)) |>
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

    # Update last rendered DT 
    last_rendered_DT(df_temp)

    # links to render in html
    if(lastEdited_data_set_name() %in% c("Programm","Filmvorschlag","Einsatzplan")){
      df_temp <- df_temp|>
        mutate(Procinema = if_else(is.na(Procinema) | Procinema == "", NA, paste0("<a href='", Procinema, "' target='_blank'>Link</a>")),
               Trailer   = if_else(is.na(Trailer) | Trailer == "", NA, paste0("<a href='", Trailer, "' target='_blank'>Link</a>"))
               )

      if("Eintritte eingespielt" %in% names(df_temp)){
        df_temp$`Eintritte eingespielt` <- df_temp$`Eintritte eingespielt`|>
          prettyNum(big.mark = "`")
      }
    } else if(lastEdited_data_set_name() %in% c("Verleiher")){
      # mailto render in html
      df_temp$Kontakt <-
        ifelse(is.na(df_temp$`Kontakt`),
               NA,
               paste0(sprintf('<a href="mailto:%s">%s</a>', df_temp$`Kontakt`, df_temp$`Kontakt`))
        )

      df_temp$Besucherzahlen <-
        ifelse(is.na(df_temp$`Besucherzahlen`),
               NA,
               paste0(sprintf('<a href="mailto:%s">%s</a>', df_temp$`Besucherzahlen`, df_temp$`Besucherzahlen`))
        )
    } else if(lastEdited_data_set_name() %in% c("Kinoklubmitglieder")){
      # mailto render in html
      df_temp$`E-Mail` <-
        ifelse(is.na(df_temp$`E-Mail`),
               NA,
               paste0(sprintf('<a href="mailto:%s">%s</a>', df_temp$`E-Mail`, df_temp$`E-Mail`))
        )
    } else if (lastEdited_data_set_name() %in% c("df_Eintritt")){
      df_temp <- df_temp|>
        mutate(`Event ID` = factor(`Event ID`))
    } else if (lastEdited_data_set_name() %in% c("df_Kiosk")){
      df_temp <- df_temp|>
        mutate(`Event ID` = factor(`Event ID`),
               ID_Kioskartikel = factor(ID_Kioskartikel),
               ID_Spezialpreisekiosk = factor(ID_Spezialpreisekiosk))
    } else if (lastEdited_data_set_name() %in% c("Eintritt files", "Kiosk files")){
      df_temp <- df_temp|>
        mutate(`Event ID` = factor(`Event ID`))
    } 
    
    # Render primary Key always as factor
    df_temp[,1] <- pull(df_temp[,1])|>
      factor()
      
  
    # Render Table
    datatable(
      df_temp,
      escape = FALSE,
      rownames = FALSE,
      editable = FALSE,
      selection = "single",
      filter = "top",
      # width = NULL,  # Let the container handle width
      extensions = c('FixedHeader'),
      options = list(
        fixedHeader = TRUE,  # This keeps headers visible
        scrollX = TRUE,  # Enable horizontal scrolling
        # scrollY = "500px",
        autoWidth = TRUE,  # auto-width enable to controll columnwidth
        columnDefs = l_columnDefs,
        scrollCollapse = TRUE,  # Better scrolling behavior
        pageLength = page_length_var(),
        lengthMenu = c_lengthMenu,
        searchCols = last_user_filter(),
        order = last_sorting(),
        initComplete = JS(
          "function(settings, json) {",
          "  // One-time header/body styles",
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
          "  $('a.paginate_button').css({",
          "    'background-color': '#7898b6',",
          "    'color': '#ffffff',",
          "    'border': '1px solid #7f8c8d',",
          "    'padding': '5px 10px',",
          "    'margin': '0 2px',",
          "    'border-radius': '4px',",
          "    'text-decoration': 'none'",
          "  });",
          "  $('a.paginate_button.current').css({",
          "    'background-color': '#e67e22',",
          "    'color': '#ffffff',",
          "    'font-weight': 'bold'",
          "  });",
          "  $('a.paginate_button').hover(",
          "    function() {",
          "      if (!$(this).hasClass('current')) {",
          "        $(this).css('background-color', '#5d7d9a');",
          "      }",
          "    },",
          "    function() {",
          "      if (!$(this).hasClass('current')) {",
          "        $(this).css('background-color', '#7898b6');",
          "      }",
          "    }",
          "  );",
          "}"
        )
      )
    ) |> apply_conditional_formatting()

  }, server = FALSE)
  
  
  ## Signal: Datatable has been rendered ####
  observeEvent(input$table_rendered, {
    writeLines("Signal: Datatable has been rendered")
    # select row and page if possible
    if(!is.na(last_selected_row()) & !is.na(last_selected_page())){
      dataTableProxy('table')|>
        selectPage(last_selected_page())|>
        selectRows(last_selected_row())
    } else if (!is.na(last_selected_page())){
      dataTableProxy('table')|>
        selectPage(last_selected_page())
    }
  })
  
  ## last user filter ####
  last_filter <- reactiveVal(NULL)
  filter_state_cleared <- reactiveVal(TRUE)
  
  ## Check if last user filter has been cleared ####
  observeEvent(input$table_search_columns,{
    c_filters <- input$table_search_columns
    c_filters[c_filters == ""] <- NA
    c_filters
    
    # run after startup
    if(is.null(last_filter())){
      last_filter(c_filters)
      filter_state_cleared(TRUE)
    }
    
    if (!identical(last_filter(), c_filters)) {
      # detect filters are all cleared 
      if(sum(is.na(c_filters)) == length(c_filters)){
        filter_state_cleared(TRUE)
      } else {
        filter_state_cleared(FALSE)
      }
      last_filter(c_filters)
    }
  })
  
  ## Signal if last user filter has been cleared ####
  observeEvent(filter_state_cleared(),{
    if(filter_state_cleared()){
      message("last filter has been cleard")
      last_user_filter(NULL)
    } else {
      message("Filter is still active: ", paste(last_filter(), collapse = ", "))
    }
  })
  
  
  ## Change in page length ####
  observeEvent(input$page_length, {
    writeLines("page_length")
    req(input$page_length)
    req(input$table_rows_selected)
    
    # Update page length
    as.integer(input$page_length)|>
      page_length_var()
    
    # calculate page got an early stop if no rows have been selected 
    l_temp <- find_page(input$table_rows_selected, input$table_search_columns,
              last_rendered_DT(), 
              lastEdited_data_set_name(), input$page_length
              )
    
    l_temp$ID_to_edit|>
      ID_to_edit()
    l_temp$last_user_filter|>
      last_user_filter()
    l_temp$last_selected_page|>
      last_selected_page()
    l_temp$last_selected_row|>
      last_selected_row()
    
    # select row and page if possible
    if(!is.na(last_selected_row()) & !is.na(last_selected_page())){
      dataTableProxy('table')|>
        selectPage(last_selected_page())|>
        selectRows(last_selected_row())
    } else if (!is.na(last_selected_page())){
      dataTableProxy('table')|>
        selectPage(last_selected_page())
    }
    
  })
  
  ## Select a row and find page ####
  observeEvent(input$table_rows_selected, {
    req(input$table_rows_selected)
    # find page 
    l_temp <- find_page(input$table_rows_selected, input$table_search_columns,
              last_rendered_DT(), 
              lastEdited_data_set_name(), page_length_var()
              )
    
    l_temp$ID_to_edit|>
      ID_to_edit()
    l_temp$last_selected_page|>
      last_selected_page()
    l_temp$last_selected_row|>
      last_selected_row()
    # only update if it is not NULL to prevent infinite loop (This will trigger rendering)
    if(!is.null(l_temp$last_user_filter)){
      l_temp$last_user_filter|>
        last_user_filter()
    }
    
  })
  
  ## Database Connection ####
  observeEvent(input$SQL_connect, {
    writeLines("Database connection")
    shiny::withProgress(message = "Database connection", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Database connection", 1, "of 2"))
      req(input$DB_host)
      req(input$DB_name)
      req(input$DB_user)
      req(input$DB_pw)
      DB_host(input$DB_host)
      DB_name(input$DB_name)
      DB_user(input$DB_user)
      DB_pw(input$DB_pw)
      
      tryCatch({
        # Connect to data base 
        DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
          DB_con()
        
        # After successful connection
        c_connected_to_db(TRUE)
        
        # Collapse the login panel
        session$sendCustomMessage(type = "collapseLoginPanel", message = list())
        
        # Initial data load
        load_initial_data()
        
      }, error = function(e) {
        showNotification(paste("load data from data base failed:", e$message), type = "error")
      })
      
      shiny::incProgress(1 / 2, detail = paste("Database connection", 2, "of 2"))
      
    })
  })

  ## Datensatz zum Editieren ####
  observeEvent(input$dataset, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    
    writeLines(paste0("Datensatz zum Editieren: `Dataset` changed from `", lastEdited_data_set_name(), "` to `", input$dataset,"`"))
    
    req(input$dataset)
    req(DB_con())
    
    shiny::withProgress(message = "Datensatz", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("Datensatz", 1, "of 2"))
      
      # get data from database
      df_temp <- DB_get_table(input$dataset, DB_con()) |>
        convert_to_template_types(l_template[[input$dataset]])
      
      # Debug print
      print(df_temp[order(pull(df_temp[,1]), decreasing = TRUE),])
      
      # get local coppy of data 
      l_temp <- l_data()
      
      # update data
      l_temp[[input$dataset]] <- df_temp
      
      # Special handling for joind data
      if(input$dataset == "Programm") {
        # Programm
        l_temp$Programm <- l_temp$Programm |> 
          arrange(desc(`Event ID`))
        
        # Einsatzplan
        l_temp$Einsatzplan <- 
          left_join(
            df_temp |> 
              select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, Procinema, Trailer, `Verleiher Angefragt?`),
            DB_get_table("Einsatzplan", DB_con()) |>
              convert_to_template_types(l_template[[input$dataset]]) |>
              select(-Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`, -Procinema, -Trailer),
            by = join_by(`Event ID`)
            ) |> 
          arrange(desc(`Event ID`))
        
        # render 
        l_temp$Programm|>
          current_data()
        
      } else if(input$dataset == "Einsatzplan") {
        # Einsatzplan
        l_temp$Einsatzplan <- 
          left_join(
            DB_get_table("Programm", DB_con()) |>
              convert_to_template_types(l_template$Programm) |> 
              select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, Procinema, Trailer, `Verleiher Angefragt?`),
            DB_get_table("Einsatzplan", DB_con()) |>
              convert_to_template_types(l_template[[input$dataset]]) |>
              select(-Suisanummer, -Filmtitel, -Datum, -Zeit, -`Verleiher Angefragt?`, -Procinema, -Trailer),
            by = join_by(`Event ID`)
          ) |> 
          arrange(desc(`Event ID`))
        
        # render 
        l_temp$Einsatzplan|>
          current_data()
        
      } else {
        # render 
        l_temp[[input$dataset]]|>
          arrange(desc(ID))|>
          current_data()
      }
      
      # initialize dictionary Verleiher to Procinema-Verleiher
      df_mapping <- DB_get_table("Verleiher mapping", DB_con()) |>
        select(-ID)
      dict_env <<- dict_from_data.frame(df_mapping)
      
      # User Information 
      if(input$dataset == "Kinoklubmitglieder"){
        help_information("Diese Tabelle liefert die Werte für das Dropdown im `Einsatzplan`")
      } else if (input$dataset == "Verleiher"){
        help_information("Diese Tabelle liefert die Werte für das Dropdown in `Filvorschlag` und 'Programm'")
      } else if (input$dataset == "Verleiher mapping"){
        help_information("Diese Tabelle ersetzt den Verleihernamen von ProCinema (Bsp.: 'WB') 
                         durch den offiziellen Verleihernamen (Bsp.: Warner Bros Entertainment Switzerland GmbH), 
                         welcher in der Dropdown-Liste 'Verleiher' festgelegt wurde.")
      } else if (input$dataset == "Lieferanten"){
        help_information("Diese Tabelle liefert die Werte für das Dropdown im `Einkauf Kiosk`")
      } else if (input$dataset == "Platzkategorien zum Verrechnen"){
        help_information("Die Werte in dieser Tabelle werden ausschliesslich beim Erstellen der
                         Verleiherabrechnung berücksichtigt ('Verleiherabrechnung erstellen‘ ), 
                         da einige Verleiher auch Gratiseintritte (Förderer) als verrechenbare Eintritte anerkennen.")
      } else if (input$dataset == "Buchhaltungskonten"){
        help_information("Diese Tabelle liefert die Werte für das Dropdown in `Einnahmen' und 'Ausgaben'")
      } else if (input$dataset == "Spezialpreis"){
        help_information("Diese Tabelle liefert die Werte für das Dropdown in `Spezialpreiskiosk`")
      } else if (input$dataset == "MWST"){
        help_information("Diese Tabelle liefert die Werte für die Dropdowns in `MWST` und 'Abrechnugnsjahr' in 'Einnamhen' und 'Ausgaben'")
      } else if (input$dataset == "Programm"){
        help_information("Die Spalte `Link to Event ID` wird verwendet um gemeinsam über mehrere `Event ID`s abzurechnen.")
      } else {
        help_information(" ")
      }
      
      shiny::isolate({
        # updata data
        l_data(l_temp)
        update_choices(l_data()) |> 
          column_choices()
        lastEdited_data_set_name(input$dataset)
        
        # remove row and page selection 
        last_selected_page(NA)
        last_selected_row(NA)
        # remove user filter 
        last_user_filter(NULL)
        # remove temp render
        df_temp_to_render(NULL)
      })

      shiny::incProgress(1 / 2, detail = paste("Datensatz", 2, "of 2"))
    })
  })

  ## Inputdaten / Advance-Tickets / Dropdowns ####
  observeEvent(input$data_selection,{
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    writeLines(paste("Inputdaten /Dropdowns: `Data_selection` changed from", data_selection_(), "to", input$data_selection," **** ",
                     "`Dataset` changed from `", lastEdited_data_set_name(), "` to `", input$dataset,"`"))
    
    # Only proceed if selection actually changed
    req(input$data_selection != data_selection_())
    
    shiny::withProgress(message = "Inputdaten / Dropdown", value = 0, {
      shiny::incProgress(1 / 3, detail = paste("Inputdaten / Dropdown", 1, "of 3"))
      
      # set data selection
      data_selection_(input$data_selection)
      
      # get all data as defined in the template l_data
      l_data_sql <- DB_get_Data(l_template, DB_con())
      # Convert data types for each table
      convert_DB_to_R(l_data_sql,l_template)|>
        l_data()
      
      
      if(input$data_selection == "Dropdowns"){
        # lastEdited_data_set_name("Kinoklubmitglieder")
        
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
        
      }

      shiny::incProgress(1 / 3, detail = paste("Inputdaten / Dropdown", 2, "of 3"))
      
      shiny::isolate({
        # update choices
        update_choices(l_data())|>
          column_choices()
        
        # Input data set
        l_data()[c_select_input_data]|>
          l_data_input()
        
        # Advace-Tickets
        l_data()[c_select_input_advanced_tickets]|>
          l_data_advance_tickets()
        
        # Drop down data set
        l_data()[c_select_dropdown_data]|>
          l_data_dropdown()
        
        # remove row and page selection 
        last_selected_page(NA)
        last_selected_row(NA)
        # remove user filter 
        last_user_filter(NULL)
        # remove temp render
        df_temp_to_render(NULL)
      })

      shiny::incProgress(1 / 3, detail = paste("Inputdaten / Dropdown", 3, "of 3"))
      
    })
  })
    
  ## Disconnect from DB ####
  observeEvent(input$SQL_disconnect,{
    print("SQL_disconnect")
    dbDisconnect(DB_con())
    c_connected_to_db(FALSE)

    # remove row and page selection 
    last_selected_page(NA)
    last_selected_row(NA)
  })
  
  ## Get email list ####
  ### user Modal ####
  observeEvent(input$get_email,{
    showModal(modalDialog(
      shiny::radioButtons("Verteiler", "Verteiler", 
                          choices = Email_col_names
      ),
      title = "E-Mail Verteiler wählen",
      footer = tagList(
        actionButton("get_email_verteiler","Email im Verteiler kopieren")
      )
    ))
  })
  
  ### Select email Verteiler and copy emails to clipboard ####
  observeEvent(input$get_email_verteiler,{
    print("E-Mail Verteiler")
    generated_code <- paste0("l_data()[[\"Kinoklubmitglieder\"]]|>
        filter(\`",input$Verteiler,"\` == \"ja\")|>
        distinct(`E-Mail`)|>
        pull()", collapse =  "")
    
    C_verteiler <- sapply(generated_code, function(x) eval(parse(text = x)))|>
      paste0(collapse = ";")
    C_verteiler|>
      writeClipboard()
    
    C_verteiler <- paste0(str_split(C_verteiler, ";")|>unlist(), collapse = ";\n")
    
    showModal(modalDialog(
      title = "Email-Verteiler wurde in die Zwischenablage kopiert",
      shiny::renderText(C_verteiler),
      footer = modalButton("ok"),
      easyClose = TRUE,
    ))
  })
  
  ## Button: Download Handler #####
  output$table_export <- downloadHandler(
    filename = function() {
      paste0(lastEdited_data_set_name(), " ", Sys.time(),".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(
        current_data(),
        file = file,
        asTable = TRUE,
        overwrite = TRUE
      )
    }
  )
  
  ## Abort: do nothing! ####
  observeEvent(input$abort,{
    removeModal()
  })
  
  ## Render modal table ####
  output$modal_table <- renderDataTable({
    req(df_temp_to_render())  
    datatable(df_temp_to_render(), 
              rownames = FALSE,
              selection = "single",
              options = list(
                searching = FALSE,     # removes search box
                language = DT_language,
                pageLength = nrow(df_temp_to_render()),
                paging = FALSE        # disables pagination
              )
    )
  })
  
  ## Edit row ####
  ### Edit row modal Dialog ####
  observeEvent(input$edit_row, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
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
          select(1:8)
        # editable
        df_row <- df_temp|> 
          select(-(1:8))
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
      
      # save info for later use
      temp_01(df_info)
      temp_02(df_row)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = "Zeile editieren",
          l_temp,
          actionButton("edit_row_value", "Werte übernehmen", class = "btn-info"),
          actionButton("abort", "Abbrechen"),
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
      removeModal()

      # get actual data 
      df_temp <- current_data()
      df_temp_ <- current_data()
            
      # Extract data from user input
      df_updated <- get_data_from_modal(temp_02())
      
      #### Data to update ####
      df_updated <- bind_cols(temp_01(), df_updated)|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])

      #### handle factors #####
      df_temp <- factor_handling(df_temp, df_updated)

      #### check input E-Mail if correct #####
      df_Email <- df_updated[,names(df_updated) == "E-Mail"]
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
      
      #### check input Suisanummer if correct #####
      df_suisa <- df_updated[,names(df_updated) == "Suisanummer"]
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
      
      #### Handling uniqueness checks for Dropdowns ####
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
      
      #### check for changed data #####
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
        # check DB connection
        if (!dbIsValid(DB_con())) {
          showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
          DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
            DB_con()
          showNotification(paste("Database connection recovered"), type = "message")
        }
        # update data base 
        c_class <- get_data_type(df_temp)
        DB_edit_row_in_table(DB_con(), 
                             lastEdited_data_set_name(), names(df_updated[,1]), pull(df_updated[,1]), df_updated,
                             c_class
                             )
        
        # Update the list
        l_temp <- l_data()
        l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
        # update all data
        l_data(l_temp)
        # update choices
        update_choices(l_data())|>
          column_choices()
        
        ##### update joined data sets and choices ####
        if(lastEdited_data_set_name() == "Programm"){
          # render 
          df_temp|>
            convert_to_template_types(l_template[[lastEdited_data_set_name()]])|>
            arrange(desc(`Event ID`))|>
            current_data()
          
          # update Einsatzplan
          Update_Einsatzplan(df_updated, c_class)
          
        } else if (lastEdited_data_set_name() == "Einsatzplan"){
          # update 
          df_temp2 <- l_data()$Programm|>
            filter(`Verleiher Angefragt?` != "Wird nicht gespielt")|>
            select(`Event ID`, Suisanummer, Filmtitel, Datum, Zeit, Procinema, Trailer, `Verleiher Angefragt?`)
          
          df_temp3 <- 
            left_join(
              df_temp2,
              df_temp,
              by = join_by(`Event ID`)
              )|>
            arrange(desc(`Event ID`))
          
          # render
          df_temp3|>
            convert_to_template_types(l_template[[lastEdited_data_set_name()]])|>
            current_data()

        } else {
          # render 
          df_temp|>
            convert_to_template_types(l_template[[lastEdited_data_set_name()]])|>
            arrange(desc(ID))|>
            current_data()
        }
      }
      
      shiny::incProgress(1 , detail = paste("data selection", 2, "of 2"))
    })
  })
  
  ## Data checks ####
  ### Check Suisanummer Modal ####
  observeEvent(input$check_suisa,{
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
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
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    print(input$email)
    DB_update_cell(DB_con(), lastEdited_data_set_name(), "ID", last_selected_row(), "E-Mail", input$email)
    df_temp <- current_data()
    df_temp[last_selected_row(),"E-Mail"] <- input$email
    current_data(df_temp)
    removeModal()
  })
  
  ## Check unique ####
  observeEvent(input$check_unique, {
    if(lastEdited_data_set_name() == "Programm"){
      # Find duplicates (keeping only duplicate rows)
      df_temp <- current_data() |>
        group_by(across(-`Event ID`)) |>
        mutate(duplicate_flag = n() > 1) |>
        ungroup() |>
        filter(duplicate_flag)|>
        select(-duplicate_flag)
      
    } else {
      # Find duplicates (keeping only duplicate rows)
      df_temp <- current_data() |>
        group_by(across(-ID)) |>
        mutate(duplicate_flag = n() > 1) |>
        ungroup() |>
        filter(duplicate_flag)|>
        select(-duplicate_flag)
      
    }
    
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
            actionButton("delete_row_modal", "Zeile Löschen", class = "btn-danger"),
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
  })
  
  ## selected row modal data table ####
  observeEvent(input$modal_select_row, {
    if (!is.null(input$modal_table_rows_selected)){ # comming from add row top / bottom
      removeModal()
      if(lastEdited_data_set_name() == "Programm"){
        df_temp <- df_temp_to_render()
        
        c_ID <- df_temp|>
          slice(input$modal_table_rows_selected)|>
          select(`Event ID`)|>
          pull()
        
        # update latest ID 
        ID_to_edit(c_ID)
        
        # Store HTML elements
        l_temp <- list()
        # only display
        df_info <- current_data() |> 
          filter(`Event ID` == c_ID)|>
          select(1)
        # editable
        df_row <- current_data() |> 
          filter(`Event ID` == c_ID)|>
          select(2:ncol(current_data()))
        # Display the display columns (read-only)
        l_temp <- lapply(1:ncol(df_info), function(ii) {
          fluidRow(
            column(6, strong(paste(names(df_info)[ii], ":")), c_ID)
          )
        })
      } else {
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
      }
      
      l_temp <- create_modal_input(df_row, l_temp)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = "Zeile editieren",
          l_temp,
          actionButton("edit_row_value", "Werte übernehmen", class = "btn-info"),
          actionButton("abort", "Abbrechen"),
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
  
  ## Row Operations (Add/Delete/Duplicate/change title/takeover) ####
  ###  add row ####
  observeEvent(input$add_row, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }

    # Create an empty row
    new_row <- current_data()[1, ] |> 
      mutate(across(everything(), ~ NA))|>
      convert_to_template_types(l_template[[lastEdited_data_set_name()]])
    new_row[1,1] <- max(current_data()[,1]) + 1L

    # add row on top
    updated_data <-
      bind_rows(new_row, 
                current_data()
      )|>
      convert_to_template_types(l_template[[lastEdited_data_set_name()]])
    
    # updata SQL DB and current data 
    DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
    if(lastEdited_data_set_name() == "Programm"){
      Update_Einsatzplan(new_row ,get_data_type(new_row),new_row = TRUE)
    }
    
    # Update the list
    l_temp <- l_data()
    l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())|>
      convert_to_template_types(l_template[[lastEdited_data_set_name()]])
    # update all data
    l_data(l_temp)
    # update choices
    update_choices(l_data())|>
      column_choices()
    
    # update to render 
    current_data(updated_data)
    
    #### select last edited row and page ####
    last_selected_row(1)
    last_selected_page(1)
    
  })
  
  
  ###  add row new entry Ausgaben ####
  observeEvent(input$add_row_ausgaben, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }

    c_select <- tbl(DB_con(), "Kategorie", downlaod = FALSE)|>
      filter(Ausgaben)|>
      select(Auswahl)|>
      pull()
    
    showModal(modalDialog(
      title = "Was für ein Eintrag soll erstellt werden",
      tagList(
        div(class = "custom-select",
            selectizeInput("Kategorie", "Bitte Kategorie wählen", selected = c_select[1], choices = c_select
            )
        ),
      ),
      footer = tagList(
        actionButton("add_row_new_entry","Eintrag erstellen"),
        actionButton("abort","Abbrechen")
      )
    ))
    req(NULL)
  })
  
  ###  add row new entry ####
  observeEvent(input$add_row_new_entry, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }    
    
    removeModal()
    
    # Create an empty row
    new_row <- current_data()[1, ] |> 
      mutate(across(everything(), ~ NA))|>
      convert_to_template_types(l_template[[lastEdited_data_set_name()]])
    new_row[1,1] <- max(current_data()[,1]) + 1L
    
    #### use case Ausgaben Kategorie Verleiher ####
    if((input$Kategorie == "Verleiher") & (lastEdited_data_set_name() == "Ausgaben")){
      
      # remember use case
      new_entry("ausgaben_verleiher")
      
      # populate with Kategorie
      new_row <- new_row|>
        mutate(Kategorie = input$Kategorie
        )
      
      # Store HTML elements
      l_temp <- list()
      # only display
      df_info <- new_row|> 
        select(1:2)
      # editable
      df_row <- new_row|> 
        select(3:(ncol(new_row) - 1),
               -Firmennamen, -Adresse, -Buchungskonto)
      # Display the display columns (read-only)
      l_temp <- lapply(1:ncol(df_info), function(ii) {
        fluidRow(
          column(6, strong(paste(names(df_info)[ii], ":")), df_info[1,ii])
        )
      })
      
      # create Modal input 
      l_temp <- create_modal_input(df_row, l_temp)

      # save for later use
      temp_01(df_info)
      temp_02(df_row)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = "Verleiherrechnung erstellen",
          l_temp,
          actionButton("edit_row_modal", "Werte übernehmen", class = "btn-info"),
          actionButton("abort", "Abbrechen"),
          easyClose = FALSE,
          footer = NULL
        )
      )
    } 
    #### use case Ausganen Kategorie Event ####
    else if ((input$Kategorie == "Event") & (lastEdited_data_set_name() == "Ausgaben")){
      # remember use case
      new_entry("ausgaben_event")
      
      # populate with Kategorie
      new_row <- new_row|>
        mutate(Kategorie = input$Kategorie
        )
      
      # Store HTML elements
      l_temp <- list()
      # only display
      df_info <- new_row|> 
        select(1:2)
      # editable
      df_row <- new_row|> 
        select(3:(ncol(new_row))
               )
      # Display the display columns (read-only)
      l_temp <- lapply(1:ncol(df_info), function(ii) {
        fluidRow(
          column(6, strong(paste(names(df_info)[ii], ":")), df_info[1,ii])
        )
      })
      
      # create Modal input 
      l_temp <- create_modal_input(df_row, l_temp)
      
      # save for later use
      temp_01(df_info)
      temp_02(df_row)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = "Ausgabe für Event erfassen",
          l_temp,
          actionButton("edit_row_modal", "Werte übernehmen", class = "btn-info"),
          actionButton("abort", "Abbrechen"),
          easyClose = FALSE,
          footer = NULL
        )
      )
    } 
    #### use case Ausganen Kategorie Kiosk ####
    else if ((input$Kategorie == "Kiosk") & (lastEdited_data_set_name() == "Ausgaben")){
      # remember use case
      new_entry("ausgaben_kiosk")
      
      # populate with Kategorie
      new_row <- new_row|>
        mutate(Kategorie = input$Kategorie
        )
      
      # Store HTML elements
      l_temp <- list()
      # only display
      df_info <- new_row|> 
        select(1:2)
      # editable
      df_row <- new_row|> 
        rename(Lieferant = Firmennamen)|>
        mutate(Lieferant = as.factor(Lieferant))|>
        select(3:(ncol(new_row)),
               -`Event ID`,-Buchungskonto,
               -Adresse)
      # Display the display columns (read-only)
      l_temp <- lapply(1:ncol(df_info), function(ii) {
        fluidRow(
          column(6, strong(paste(names(df_info)[ii], ":")), df_info[1,ii])
        )
      })
      
      # create Modal input 
      l_temp <- create_modal_input(df_row, l_temp)
      
      # save for later use
      temp_01(df_info)
      temp_02(df_row)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = "Kiosk Einkauf erfassen",
          l_temp,
          actionButton("edit_row_modal", "Werte übernehmen", class = "btn-info"),
          actionButton("abort", "Abbrechen"),
          easyClose = FALSE,
          footer = NULL
        )
      )
    }
    #### use case Ausgaben Kategorie Personalaufwand ####
    else if ((input$Kategorie == "Personalaufwand") & (lastEdited_data_set_name() == "Ausgaben")){
      # remember use case
      new_entry("ausgaben_personalaufwand")
      
      # populate with Kategorie
      new_row <- new_row|>
        mutate(Kategorie = input$Kategorie
        )
      
      # Store HTML elements
      l_temp <- list()
      # only display
      df_info <- new_row|> 
        select(1:3)
      df_info[1,2] <- input$Kategorie # Kategorie
      df_info[1,3] <- NA # Event ID
      # editable
      df_row <- new_row|> 
        select(4:ncol(new_row),
               -Buchungskonto)|>
        rename(Personal = Firmennamen)|>
        mutate(Personal = factor(Personal))
      
      # Display the display columns (read-only)
      l_temp <- lapply(1:ncol(df_info), function(ii) {
        fluidRow(
          column(6, strong(paste(names(df_info)[ii], ":")), df_info[1,ii])
        )
      })
      
      # create Modal input 
      l_temp <- create_modal_input(df_row, l_temp)
      
      # save for later use
      temp_01(df_info)
      temp_02(df_row)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = paste0("Ausgabe für die Kategorie: ",input$Kategorie," erfassen"),
          l_temp,
          actionButton("edit_row_modal", "Werte übernehmen", class = "btn-info"),
          actionButton("abort", "Abbrechen"),
          easyClose = FALSE,
          footer = NULL
        )
      )
    }
    #### not yet implemented ####
    else {
      # remember use case
      new_entry("ausgaben_generic")
      
      # populate with Kategorie
      new_row <- new_row|>
        mutate(Kategorie = input$Kategorie
        )
      
      # Store HTML elements
      l_temp <- list()
      # only display
      df_info <- new_row|> 
        select(1:3)
      df_info[1,2] <- input$Kategorie # Kategorie
      df_info[1,3] <- NA # Event ID
      # editable
      df_row <- new_row|> 
        select(4:ncol(new_row))
      # Display the display columns (read-only)
      l_temp <- lapply(1:ncol(df_info), function(ii) {
        fluidRow(
          column(6, strong(paste(names(df_info)[ii], ":")), df_info[1,ii])
        )
      })
      
      # create Modal input 
      l_temp <- create_modal_input(df_row, l_temp)
      
      # save for later use
      temp_01(df_info)
      temp_02(df_row)
      
      # User interaction to save
      showModal(
        modalDialog(
          title = paste0("Ausgabe für die Kategorie: ",input$Kategorie," erfassen"),
          l_temp,
          actionButton("edit_row_modal", "Werte übernehmen", class = "btn-info"),
          actionButton("abort", "Abbrechen"),
          easyClose = FALSE,
          footer = NULL
        )
      )
    }
  })  
  
  ### edit row modal ####
  observeEvent(input$edit_row_modal, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    } 
    
    removeModal()
    
    # Extract data from user input
    df_updated <- get_data_from_modal(temp_02())
    
    # create new row
    new_row <- bind_cols(temp_01(), df_updated)
    
    #### use case Ausgaben Kategorie Verleiher #### 
    if(new_entry() == "ausgaben_verleiher"){
      if(is.na(new_row$`Event ID`)){
        # User interaction to save
        showModal(
          modalDialog(
            title = "Es muss einen Event ID angegeben werden um eine Verleiherrechnung zu erfassen!",
            actionButton("abort", "Abbrechen"),
            easyClose = FALSE,
            footer = NULL
          )
        )
        req(NULL) # early exit
      }
      # find Verleiher 
      Verleiher <- tbl(DB_con(), "Programm")|>
        filter(`Event ID` == new_row$`Event ID`)|>
        select(Verleiher)|>
        pull()
      
      Verleiher <- tbl(DB_con(), "Verleiher")|>
        filter(Verleihername == Verleiher)|>
        collect()
      Verleiher

      # fill in Verleiher info
      new_row <- new_row|>
        mutate(Firmennamen = Verleiher$Verleihername,
               Adresse = paste0(Verleiher$Adresse, ", ",Verleiher$PLZ, " ", Verleiher$Ort),
               Buchungskonto = "4404 Filmmiete Kino")|>
        select("ID", "Kategorie", "Event ID", "Bezeichnung", "Datum", "Abrechnungsjahr", "Betrag [CHF]", 
               "Firmennamen", "Adresse", "Referenz", "Rechnungsnummer", "Buchungskonto")|>
        convert_to_template_types(l_template$Ausgaben)
    }
    #### use case Ausgaben Kategorie Event ####
    else if (new_entry() == "ausgaben_event"){
      if(is.na(new_row$`Event ID`)){
        # User interaction to save
        showModal(
          modalDialog(
            title = "Es muss einen Event ID angegeben werden um eine Verleiherrechnung zu erfassen!",
            actionButton("abort", "Abbrechen"),
            easyClose = FALSE,
            footer = NULL
          )
        )
        req(NULL) # early exit
      }
      # fill in Buchungskonto
      new_row <- new_row|>
        select("ID", "Kategorie", "Event ID", "Bezeichnung", "Datum", "Abrechnungsjahr", "Betrag [CHF]", 
               "Firmennamen", "Adresse", "Referenz", "Rechnungsnummer", "Buchungskonto")|>
        convert_to_template_types(l_template$Ausgaben)
    } 
    #### use case Ausgaben Kategorie Kiosk ####
    else if (new_entry() == "ausgaben_kiosk"){
      c_Lieferant <- new_row$Lieferant
      if(is.na(c_Lieferant)){
        # User interaction to save
        showModal(
          modalDialog(
            title = "Es muss ein Lieferant angegeben werden um einen Kioskeinkauf zu erfassen!",
            actionButton("abort", "Abbrechen"),
            easyClose = FALSE,
            footer = NULL
          )
        )
        req(NULL) # early exit
      }

      # find Verleiher 
      Lieferant <- tbl(DB_con(), "Lieferanten")|>
        filter(Lieferantenname == c_Lieferant)|>
        collect()
      
      # fill in Verleiher info
      new_row <- new_row|>
        rename(Firmennamen = Lieferant)|>
        mutate(`Event ID` = NA,
               Firmennamen = Lieferant$Lieferantenname ,
               Adresse = paste0(Lieferant$Adresse, ", ",Lieferant$PLZ, " ", Lieferant$Ort),
               Buchungskonto = "4405 Einkauf Kioskwaren Kino")|>
        select("ID", "Kategorie", "Event ID", "Bezeichnung", "Datum", "Abrechnungsjahr", "Betrag [CHF]", 
               "Firmennamen", "Adresse", "Referenz", "Rechnungsnummer", "Buchungskonto")|>
        convert_to_template_types(l_template$Ausgaben)
    } 
    #### use case Ausgaben Kategorie Personalaufwand ####
    else if (new_entry() == "ausgaben_personalaufwand"){
      c_temp <- new_row$Personal
      if(is.na(c_temp)){
        # User interaction to save
        showModal(
          modalDialog(
            title = "Es muss eine Personalresource angegeben werden.",
            actionButton("abort", "Abbrechen"),
            easyClose = FALSE,
            footer = NULL
          )
        )
        req(NULL) # early exit
      }

      # fill in Verleiher info
      new_row <- new_row|>
        rename(Firmennamen = Personal)|>
        mutate(`Event ID` = NA,
               Firmennamen = c_temp,
               Adresse = NA,
               Buchungskonto = "5000 Personalaufwand")|>
        select("ID", "Kategorie", "Event ID", "Bezeichnung", "Datum", "Abrechnungsjahr", "Betrag [CHF]", 
               "Firmennamen", "Adresse", "Referenz", "Rechnungsnummer", "Buchungskonto")|>
        convert_to_template_types(l_template$Ausgaben)
    } 


    # add row on top
    updated_data <-
      bind_rows(new_row, 
                current_data()
      )|>
      convert_to_template_types(l_template[[lastEdited_data_set_name()]])
    
    # updata SQL DB and current data 
    DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
    
    # Update the list
    l_temp <- l_data()
    l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())|>
      convert_to_template_types(l_template[[lastEdited_data_set_name()]])
    # update all data
    l_data(l_temp)
    # update choices
    update_choices(l_data())|>
      column_choices()
    
    # update to render 
    current_data(updated_data)
    
    # select last edited row and page 
    last_selected_row(1)
    last_selected_page(1)
    
  })
    
  
  ###  add row on top of selected row ####
  observeEvent(input$add_row_top, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
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
          bind_rows(new_row, 
                    current_data()[(input$table_rows_selected):nrow(current_data()), ]
                    )|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      } else{
        updated_data <-
          bind_rows(current_data()[1:(input$table_rows_selected - 1), ], 
                    new_row, 
                    current_data()[(input$table_rows_selected):nrow(current_data()), ]
                    )|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      }
      
      # updata SQL DB and current data 
      DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
      
      # update joined data sets 
      if(input$dataset == "Programm"){
        c_class <- get_data_type(current_data())
        Update_Einsatzplan(new_row, c_class, new_row = TRUE)
      }
      
      #### Handling uniqueness checks for Dropdowns #####
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
      l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
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
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
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
          )|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
        
        #### select last edited row and page ####
        last_selected_row(last_selected_row() + 1)
        
      }else {
        updated_data <- 
          bind_rows(current_data()[1:(input$table_rows_selected),],
                    new_row,
                    current_data()[(input$table_rows_selected + 1):nrow(current_data()),]
          )|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
        
      }
      #### Handling uniqueness checks for Dropdowns #####
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
      l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      
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
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
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
          )|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      }else {
        updated_data <- 
          bind_rows(current_data()[1:(input$table_rows_selected),],
                    new_row,
                    current_data()[(input$table_rows_selected + 1):nrow(current_data()),]
          )|>
          convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      }
      # update the selected row
      (input$table_rows_selected + 1)|>
        last_selected_row()
      
      # updata SQL DB
      DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
      
      # update joined data sets 
      if(input$dataset == "Programm"){
        c_class <- get_data_type(current_data())
        Update_Einsatzplan(new_row, c_class, new_row = TRUE)
      }
      # Update the list
      l_temp <- l_data()
      l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
        
      # update all data
      l_data(l_temp)
      # update choices
      update_choices(l_data())|>
        column_choices()
      # update to render
      current_data(updated_data)
    }
  })
  
  ### Duplicate Film and archive (Filmtitel ändern) ####
  observeEvent(input$archive_row,{
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
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
  
  ### Delete selected row(s) ####
  #### Delete row ####
  observeEvent(input$delete_row, {
    # get selected row
    selected_row <- input$table_rows_selected
    selected_row|>
      delete_row()
  })
  
  #### Delete row from Modal input ####
  observeEvent(input$delete_row_modal, {
    # get selected row
    selected_row <- input$modal_table_rows_selected
    selected_row <- pull(df_temp_to_render()[selected_row,][1,1])
    
    # get row to delete 
    df_temp <- current_data()
    selected_row <- pull(df_temp[,1]) == selected_row
    
    selected_row|>
      delete_row()
  })
  
  #### Delete row helper function ####
  delete_row <- function(selected_row) {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }

    # get row to delete 
    df_temp <- current_data()[selected_row,]
    
    # row to render for user information
    df_temp|>
      df_temp_to_render()
    
    # Calculate modal size based on number of columns
    num_cols <- ncol(df_temp)
    modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
    modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
    
    ##### no row has been selected ####
    if( !(sum( (!is.null(input$table_rows_selected)) | (!is.null(input$modal_table_rows_selected)) ) > 0)){
      showModal(modalDialog(
        title = "Bitte eine Zeile markieren!",
        footer = tagList(
          modalButton("Abbrechen")),
        easyClose = TRUE
      ))
      req(NULL) # early exit
    } 
    ##### row has been selected ####
    else {
      ##### Programm ####
      if(lastEdited_data_set_name() == "Programm"){
        showModal(
          modalDialog(
            title = "Zeile löschen?",
            size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
            tagList(
              renderText("Achtung der Eintrag wird auch aus dem Einsatzplan gelöscht!"),
              hr(),
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table")
              )
            ),
            easyClose = FALSE, 
            footer = tagList(
              actionButton("confirm_delete", "Löschen"),
              modalButton("Abbrechen")
            ),
          )
        )
        
      } ##### Spezialpreisekiosk #### 
      else if (lastEdited_data_set_name() == "Spezialpreisekiosk"){
        
        df_Kiosk <- tbl(DB_con(), "df_Kiosk")|>
          filter(`Event ID` == df_temp$`Event ID`,
                 Artikelname == df_temp$Artikelname
          )|>
          collect()
        
        programm <- DB_get_table("Programm", DB_con())
        
        df_Kiosk <- df_Kiosk|>
          left_join(
            programm
          )|>
          select(ID, `Event ID`, `Artikel-Kassensystem`, Artikelname, Filmtitel, Suisanummer, Datum)
        
        if(nrow(df_Kiosk) > 0){
          # to render for modal 
          df_temp_to_render(df_Kiosk)
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_Kiosk)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_Kiosk) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Achtung die Spezialpreisdefinition ID = ", df_temp$ID,", `", df_temp$Spezialpreis,"` kann nicht gelöscht werden verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Die Definition wird in der Tabelle `df_Kiosk` verwendet und muss da zuerst gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
        } else {
          showModal(
            modalDialog(
              title = "Zeile löschen?",
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("confirm_delete", "Löschen"),
                modalButton("Abbrechen")
              ),
            )
          )
        }
      } 
      ##### Kinoklubmitglieder #### 
      else if (lastEdited_data_set_name() == "Kinoklubmitglieder"){
        
        c_ID <- df_temp[1,1]|>pull()
        
        c_search <- paste(df_temp$Vorname, df_temp$Nachname)
        
        df_Einsatzplan <- tbl(DB_con(), "Einsatzplan")|>
          filter((Verantwortlich %in% c_search) |
                   (`Operateur*in` %in% c_search) |
                   (`Kasse/Bar 1` %in% c_search) |
                   (`Kasse/Bar 2` %in% c_search) |
                   `Back-up` %in% c_search
          )|>
          collect()
        
        if(nrow(df_Einsatzplan) > 0){
          # to render for modal 
          df_temp_to_render(df_Einsatzplan)
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_Einsatzplan)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_Einsatzplan) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Achtung das Kinoklubmitglied \"",c_search,"\" wird im Einsatzplan verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Kinoklubmitglied muss zuerst im Einsatzplan gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
        } else {
          showModal(modalDialog(
            title = "Zeile löschen?",
            tagList(
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table")
              )
            ),
            footer = tagList(
              modalButton("Abbrechen"),
              actionButton("confirm_delete", "Löschen")
            ),
            easyClose = TRUE
          ))
        }
        
      } 
      ##### Verleiher ####
      else if (lastEdited_data_set_name() == "Verleiher"){
        
        c_ID <- df_temp[1,1]|>pull()
        
        df_temp <- df_temp|>
          filter(ID == c_ID)
        
        c_search <- df_temp$Verleihername
        
        df_Programm <- tbl(DB_con(), "Programm")|>
          filter(Verleiher == c_search)|>
          collect()
        df_Programm
        
        df_Filmvorschlag <- tbl(DB_con(), "Filmvorschlag")|>
          filter(Verleiher == c_search)|>
          collect()
        df_Filmvorschlag
        
        df_VerleiherMapping <- tbl(DB_con(), "Verleiher mapping")|>
          filter(Verleihername == c_search)|>
          collect()
        df_VerleiherMapping
        
        if(nrow(df_Programm) > 0){
          # to render for modal 
          df_temp_to_render(df_Programm)
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_Programm)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_Programm) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Achtung der Verleiher \"",c_search,"\" wird im Programm verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Verleihereinträge müssen zuerst im Programm gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
        } else if(nrow(df_Filmvorschlag) > 0){
          # to render for modal 
          df_temp_to_render(df_Filmvorschlag)
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_Filmvorschlag)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_Filmvorschlag) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Achtung der Verleiher \"",c_search,"\" wird im Filmvorschlag verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Verleihereinträge müssen zuerst im Filmvorschlag gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
          
        } else if(nrow(df_VerleiherMapping) > 0){
          # to render for modal 
          df_temp_to_render(df_VerleiherMapping)
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_VerleiherMapping)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_VerleiherMapping) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Achtung das Kinoklubmitglied \"",c_search,"\" wird im Einsatzplan verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Verleihereinträge muss zuerst im `Verleiher mapping` gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
          
        } else {
          showModal(modalDialog(
            title = "Selektierte Zeile löschen?",              
            tagList(
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table")
              )
            ),
            footer = tagList(
              modalButton("Abbrechen"),
              actionButton("confirm_delete", "Löschen")
            ),
            easyClose = TRUE
          ))
        }
      } 
      ##### Lieferanten #### 
      else if (lastEdited_data_set_name() == "Lieferanten"){
        
        c_ID <- df_temp[1,1]|>pull()
        
        df_temp <- df_temp|>
          filter(ID == c_ID)
        
        c_search <- df_temp$Lieferantenname
        
        df_temp <- tbl(DB_con(), "Einkauf Kiosk")|>
          filter(Lieferant == c_search)|>
          collect()
        df_temp
        
        if(nrow(df_temp) > 0){
          # to render for modal 
          df_temp_to_render(df_temp)
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_temp)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Achtung der Lieferant \"",c_search,"\" wird in `Einkauf Kiosk` verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Lieferant muss zuerst in `Einkauf Kiosk`  gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
        } else {
          showModal(modalDialog(
            title = "Selektierte Zeile löschen?",
            tagList(
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table")
              )
            ),
            footer = tagList(
              modalButton("Abbrechen"),
              actionButton("confirm_delete", "Löschen")
            ),
            easyClose = TRUE
          ))
        }
        
      } 
      ##### Buchhaltungskonten #### 
      else if (lastEdited_data_set_name() == "Buchhaltungskonten"){
        
        c_ID <- df_temp[1,1]|>pull()
        
        df_temp <- df_temp|>
          filter(ID == c_ID)
        
        c_search <- df_temp$Buchungskontoname
        
        df_temp <- tbl(DB_con(), "Ausgaben")|>
          filter(Buchungskonto == c_search)|>
          collect()
        df_temp
        
        if(nrow(df_temp) > 0){
          showModal(
            modalDialog(
              title = paste0("Achtung der Lieferant \"",c_search,"\" wird in `Einkauf Kiosk` verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Lieferant muss zuerst in `Einkauf Kiosk`  gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
        } else {
          showModal(modalDialog(
            title = "Selektierte Zeile löschen?",
            tagList(
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table")
              )
            ),
            footer = tagList(
              modalButton("Abbrechen"),
              actionButton("confirm_delete", "Löschen")
            ),
            easyClose = TRUE
          ))
        }
        
      } 
      ##### Buchhaltungskonten #### 
      else if (lastEdited_data_set_name() == "Spezialpreis"){
        
        c_ID <- df_temp[1,1]|>pull()
        
        df_temp <- df_temp|>
          filter(ID == c_ID)
        
        c_search <- df_temp$Spezialpreisname
        
        df_temp <- tbl(DB_con(), "Spezialpreisekiosk")|>
          filter(Spezialpreis == c_search)|>
          collect()
        df_temp
        
        if(nrow(df_temp) > 0){
          # to render for modal 
          df_temp_to_render(df_temp)
          
          # Calculate modal size based on number of columns
          num_cols <- ncol(df_temp)
          modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
          modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
          
          showModal(
            modalDialog(
              title = paste0("Achtung der Spezialpreis \"",c_search,"\" wird in Spezialpreisekiosk verwendet!"),
              size = modal_width,  # "s" (small), "m" (medium), "l" (large), or "xl" (extra large)
              tagList(
                renderText("Spezialpreis muss zuerst in Spezialpreisekiosk gelöscht werden!"),
                hr(),
                div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                    dataTableOutput("modal_table")
                )
              ),
              easyClose = FALSE, 
              footer = tagList(
                actionButton("abort", "Abbrechen")
              )
            )
          )
        } else {
          showModal(modalDialog(
            title = "Selektierte Zeile löschen?",
            tagList(
              div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                  dataTableOutput("modal_table")
              )
            ),
            footer = tagList(
              modalButton("Abbrechen"),
              actionButton("confirm_delete", "Löschen")
            ),
            easyClose = TRUE
          ))
        }
      } 
      ##### anything else ####
      else {
        showModal(modalDialog(
          title = "Selektierte Zeile löschen?",
          tagList(
            div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                dataTableOutput("modal_table")
            )
          ),
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton("confirm_delete", "Löschen")
          ),
          easyClose = TRUE
        ))
      }
    }
  }

  #### Delete selected row ####
  observeEvent(input$confirm_delete, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    if(nrow(current_data()) <= 1){
      removeModal()
      showModal(modalDialog(
        title = "Die letzte Zeile kannn nicht gelöscht werden",
        footer = tagList(
          modalButton("Abbrechen")
        ),
        easyClose = TRUE
      ))
    }
    else {
      # this is the ID to delete
      selected_ID <- pull(df_temp_to_render()[1,1])

      # find row to delete
      selected_row <- pull(current_data()[,1]) == selected_ID
      
      # Find ID to delete
      row <- current_data()[selected_row, ]
      updated_data <- current_data()
      
      # Delete in current data 
      updated_data <- updated_data[updated_data[,1] !=  row[[1,1]],]
      # ensure correct data type
      updated_data|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      
      # update to render
      current_data(updated_data)
      
      # Update SQL
      DB_delete_row(DB_con(), lastEdited_data_set_name(), names(updated_data[,1]), pull(row[,1]))
      
      # Update the list
      l_temp <- l_data()
      l_temp[[lastEdited_data_set_name()]] <- DB_get_table(lastEdited_data_set_name(), DB_con())|>
        convert_to_template_types(l_template[[lastEdited_data_set_name()]])
      
      # joined tables 
      if(lastEdited_data_set_name() == "Programm"){
        df_temp <- DB_get_table("Einsatzplan",DB_con())
        DB_delete_row(DB_con(), "Einsatzplan", names(row[,1]), pull(row[,1]))
        df_temp <- DB_get_table("Einsatzplan",DB_con())
        l_temp[["Einsatzplan"]] <- df_temp
      }
      
      # update all data
      l_data(l_temp)
      
      ##### select last edited page ####
      last_selected_row(NA)
      
      removeModal()
    }
    # Remove table render modal dialogs
    df_temp_to_render(NULL)
  })
  
  ### Takeover Filmvorschlag to Programm ####
  #### user modal ####
  observeEvent(input$add_to_programm,{
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    req(input$add_to_programm)
    
    if(is.null(input$table_rows_selected)){
      showModal(modalDialog(
        title = "Bitte eine Zeile markieren!",
        footer = tagList(
          modalButton("Abbrechen")),
        easyClose = TRUE
      ))
    }else{
      req(input$table_rows_selected)
      # Find selected data
      row <- current_data()[input$table_rows_selected, ]
      
      # get biggest ID from Programm
      Last_Event_ID <- DB_get_max_pk(DB_con(), "Programm")
      
      # paste0("\"",tbl(DB_con(), "Programm")|>
      #   colnames(),"\"")|>
      #   writeLines()
      
      df_newrow <- tibble("Event ID" = Last_Event_ID + 1L,
                          "Suisanummer" = row$Suisanummer,
                          "Filmtitel" = row$Filmtitel,
                          "Procinema" = row$Procinema,
                          "Trailer" = row$Trailer
      )
      df_newrow
      
      # Check if Suisanumber can be found in Programm
      df_temp <- tbl(DB_con(), "Programm")|>
        filter(Suisanummer == df_newrow$Suisanummer)|>
        collect()
      
      # to render for modal dialog
      df_temp_to_render(df_temp)
      
      if(nrow(df_temp) > 0){
        # Calculate modal size based on number of columns
        num_cols <- ncol(df_temp)
        modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
        modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
        
        showModal(modalDialog(
          title = paste0("Film \"", df_newrow$Filmtitel[1],"\" wurde bereits gezeigt."),
          tagList(
            div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                dataTableOutput("modal_table"))
          ),
          footer = tagList(
            actionButton("Film_takover","Film dennoch übernehmen", class = "btn-danger"),
            actionButton("abort","Abbrechen")
          )
        ))
      } else {
        showModal(modalDialog(
          title = paste0("Film: \"", df_newrow$Filmtitel[1], "\" ins Programm übernehmen"),
          footer = tagList(
            actionButton("Film_takover","Film übernehmen", class = "btn-success"),
            actionButton("abort","Abbrechen")
          )
        ))
      }
    }
  })
  
  #### define date ####
  observeEvent(input$Film_takover,{
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    req(input$table_rows_selected)
    removeModal()

    # Find selected data
    row <- current_data()[input$table_rows_selected, ]
    
    # get biggest ID from Programm
    Last_Event_ID <- DB_get_max_pk(DB_con(), "Programm")

    newrow <- tibble(
      "Event ID" = Last_Event_ID + 1L,
      "Suisanummer" = row$Suisanummer,
      "Filmtitel" = row$Filmtitel,
      "Datum" = NA,
      "Zeit" = NA,
      "Link to Event ID" = NA,
      "Verleiher" = row$Verleiher,
      "Verleiher Angefragt?" = "Anfrage läuft",
      "Procinema" = row$Procinema,
      "Trailer" = row$Trailer,
      "Produktionsland" = row$Produktionsland,
      "Abzug [%]" = 30,
      "Minimal Abzug [CHF]" = 150,
      "Abzug fix [CHF]" = NA,
      "Verleihervertrag abgelegt" = NA,
      "Anzahl bestellter Poster und Flyer" = NA,
      "Poster und Flyer erhalten?" = NA,
      "Art der Filmlieferung" = NA,
      "Besucherzahlen an Verleiher gesendet" = NA,
      "Rechnung bezahlt und abgelegt" = NA,
      "KDM ja oder nein" = NA,
    )
    
    # to render for modal dialog
    df_temp_to_render(newrow)
    
    # Calculate modal size based on number of columns
    num_cols <- ncol(newrow)
    modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
    modal_height <- ifelse(nrow(newrow) <= 5, "auto", "600px")
    
    showModal(modalDialog(
      title = paste0("Der Film `",newrow$Filmtitel, "` wird ins Programm übernommen."),
      tagList(
        shiny::dateInput(
          "Modal_Date","Spieldatum",
          format = "dd.mm.yyyy",
          language = "de",
          weekstart = 1,
          value = as.Date(Sys.Date())
          ),
        timeInput(
          inputId = "Modal_time",
          label = "Zeit",
          value = "20:00:00",
          seconds = FALSE
        )
      ),
      footer = tagList(
        actionButton("Film_takover_with_data","Übernehmen", class = "btn-danger"),
        actionButton("abort","Abbrechen")
      )
    ))
  })  
  
  #### take over ####
  observeEvent(input$Film_takover_with_data,{
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    removeModal()
    req(input$Modal_Date)
    req(input$Modal_time)
    c_time <- input$Modal_time
    c_time <- format(as.POSIXct(input$Modal_time, format = "%H:%M"), format = "%H:%M:%S")
    
    # change date and time according to user input
    newrow <- df_temp_to_render()
    newrow <- newrow|>
      mutate(Datum = as.Date(input$Modal_Date),
             Zeit = c_time)|>
    convert_to_template_types(l_template$Programm)
    
    # Add new row to Programm and update Einsatzplan
    DB_add_row(DB_con(),"Programm", newrow)

    # update joined data
    c_class <- get_data_type(newrow)
    Update_Einsatzplan(newrow, c_class, new_row = TRUE)
    
    # update data
    l_temp <- l_data()
    l_temp[["Programm"]] <- DB_get_table("Programm", DB_con())
    l_temp[["Einsatzplan"]] <- DB_get_table("Einsatzplan", DB_con()) 
    
    # Convert to R data type
    l_temp <- convert_DB_to_R(l_temp, l_template)
    # update data 
    l_data(l_temp)
    # update choices
    update_choices(l_data())|>
      column_choices()
  })
  
  ### Procinema search ####
  #### user modal ####
  observeEvent(input$procinema_search,{
    req(input$procinema_search)
    showModal(modalDialog(
      title = "Suisanummer auf Procinema suchen",
      # Input panel at top
      shiny::inputPanel(
        shiny::textInput("suisa", "Suisanummer", placeholder = "xxxx.xxx" )
      ),
      tagList(
        div(dataTableOutput("modal_table")
            )
      ),
      easyClose = FALSE, 
      footer = tagList(
        actionButton("procinema","Suchen", class = "btn-success"),
        actionButton("abort","Abbrechen")
      )
    ))
  })
  
  #### search and take over ####  
  observeEvent(input$procinema, {
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    req(input$suisa)
    if(str_detect(input$suisa, pattern = "\\d{4}\\.\\d{3}")){
      shiny::withProgress(message = "Procinema", value = 0, {
        shiny::incProgress(1 / 2, detail = paste("step", 1, "of 2"))
        tryCatch(
          {
            df_temp <- search_procinema_by_suisa(input$suisa)
            
          }, error = function(e){
            showNotification(paste("Es konnten kein Details für diesen Film geladen werden:\n", e$message), type = "error")
            removeModal()
          }
        )
        # only go on if df_temp is defined 
        if(r_is.defined(df_temp)){
          # only go on if a suisa number match can be found
          if((nrow(df_temp) > 0)){
            df_temp <- df_temp|>
              filter(Suisanummer == input$suisa)
            tryCatch(
              {
                # get correct Verleiher from dictionary
                df_temp <- df_temp|>
                  mutate(Verleiher = dict_get_values(df_temp$Verleiher, dict_env))
              }, error = function(e){
                showNotification(paste("Für den Verleiher von Procinema",df_temp$Verleiher," Fehlermeldung: ", e$message), type = "error")
                removeModal()
              })
            # render table 
            df_temp_to_render(df_temp)  
            removeModal()
            
            if(df_temp$Suisanummer %in% current_data()$Suisanummer){
              showModal(modalDialog(
                title = "Filmvorschlag exisiert bereits",
                easyClose = TRUE, 
                footer = tagList(
                  actionButton("abort","Abbrechen")
                )
              ))
              df_temp_to_render(NULL)
            } else {
              # Calculate modal size based on number of columns
              num_cols <- ncol(df_temp)
              modal_width <- ifelse(num_cols <= 3, "s", ifelse(num_cols <= 5, "m", "l"))
              modal_height <- ifelse(nrow(df_temp) <= 5, "auto", "600px")
              
              showModal(modalDialog(
                title = "Filmvorschlag übernehmen",
                tagList(
                  div(style = paste0("max-height: ", modal_height, "; overflow-y: auto;"),
                      dataTableOutput("modal_table")
                  )
                ),
                easyClose = FALSE, 
                footer = tagList(
                  actionButton("takeover_suisa","Selektierte Zeile übernehmen", class = "btn-success"),
                  actionButton("abort","Abbrechen")
                )
              ))
            }
          } else {
            removeModal()
            showModal(modalDialog(
              title = paste0("Die Suisanummer: \"",input$suisa, "\" wurde auf Procinema nicht gefunden." ),
              footer = tagList(
                actionButton("abort","Abbrechen")
              )
            ))
          }
        }
        shiny::incProgress(1 / 2, detail = paste("search procinema website", 1, "of 2"))
      })
      
    } else {
      removeModal()
      showModal(modalDialog(
        title = "Die Suisanummer ist nicht korrekt",
        footer = tagList(
          actionButton("abort","Abbrechen")
        )
      ))
    }
  })
  
  #### takeover Film to Filmvorschlag by suisanummer ####
  observeEvent(input$takeover_suisa,{
    # check DB connection
    if (!dbIsValid(DB_con())) {
      showNotification(paste("Database connection got lost, try to reconnect."), type = "warning")
      DB_connect(DB_host(), DB_name(), DB_user(), DB_pw())|>
        DB_con()
      showNotification(paste("Database connection recovered"), type = "message")
    }
    req(input$takeover_suisa)
    removeModal()
    shiny::withProgress(message = "Procinema", value = 0, {
      shiny::incProgress(1 / 2, detail = paste("step", 1, "of 2"))
      tryCatch(
        {
          # search detail on procinema
          df_temp <- df_temp_to_render()$link|>
            film_details()|>
            rename(Inhalt = synopsis,
                   Filmtitel = title
            )|>
            mutate(across(contains("release"), 
                          ~ as.Date(., format = "%d.%m.%Y")))
        }, error = function(e){
          showNotification(paste("Error:", e$message), type = "error")
        }
      )
      shiny::incProgress(1 / 2, detail = paste("search procinema website", 1, "of 2"))
    })

    # update date if details have been found
    if(r_is.defined(df_temp)){
      # create new row
      new_row <- df_temp_to_render()|>
        rename(Procinema = link,
               `Start-Datum` = release_date,
               `Eintritte eingespielt` = admissions)|>
        mutate(`Start-Datum` = dmy(`Start-Datum`))
      new_row

      # check if Verleiher mapping is available 
      df_Verleiher_mapping <- DB_get_table("Verleiher mapping", DB_con())
      tail(df_Verleiher_mapping)

      
      if(is.null(names(new_row$Verleiher))){
        showModal(modalDialog(
          title = paste0("Es gibt keinen Procinema Verleihernamen `", new_row$Verleiher, "` in der Tabelle `Verleiher mapping`."),
          renderText("Bitte einen Eintrag erfassen in der Tabelle `Verleiher mapping` erfassen und dann nochmals probieren!"),
          easyClose = FALSE, 
          footer = tagList(
            actionButton("abort","Abbrechen")
          )
        ))
        req(NULL) # early exit
      }
      
      # Create new row entry      
      new_row <- new_row|>
        bind_cols(Inhalt = df_temp$Inhalt,
                  director = df_temp$director,
                  Regie = df_temp$producer,
                  Schauspieler = df_temp$actors,
                  Kategorie = "",
                  Trailer = "",
                  Produktionsland = df_temp$Produktionsland,
                  Genre = df_temp$Genre
        )
      new_row <- bind_cols(ID = max(current_data()$ID) + 1,
                           new_row
      )|>
        select("ID", "Suisanummer", "Filmtitel", "Start-Datum", "Verleiher", "Inhalt", "Regie", 
               "Schauspieler", "Produktionsland", "Genre", "Eintritte eingespielt", "Procinema", "Trailer", "Kategorie")
      
      # updata SQL DB
      DB_add_row(DB_con(), lastEdited_data_set_name(), new_row)
      
      # update to render
      DB_get_table("Filmvorschlag", DB_con())|>
        convert_to_template_types(l_template$Filmvorschlag)|>
        arrange(desc(ID))|>
        current_data()
      # select the fist row in datatable
      last_selected_row(1L)
    }
    df_temp_to_render(NULL)
  })
  
  ## Timer to trigger every 5 seconds ####
  poll_timer <- reactiveTimer(5000)
  
  ## Reactive that checks DB connection ####
  db_connection_status <- reactive({
    poll_timer()  # Triggered every 5s
    if(dbIsValid(DB_con())){
      DB_get_max_pk(DB_con(), "MWST")
      writeLines(paste0("✅ Database connection is valid. Time: ", poll_timer()))
      return(TRUE)
    } else FALSE
  })
  
  ## Render: Database connection status ####
  output$db_status <- renderText({
    if (db_connection_status()) {
      paste0("✅ Database connection is valid. Time: ", poll_timer())
    } else {
      paste0("❌ Database connection is NOT valid! Time", poll_timer())
    }
  })
  
  ## Render: user help informaton text ####
  output$help_info <- renderText({
    help_information()
  })
  
  ## Dynamic content output panel ####
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      if(c_connected_to_db()) {
        div(
          shiny::uiOutput("db_status"),
          style = "width: 100%; overflow-x: auto;",
          DTOutput("table", width = "100%"),
          # Make panel draggable
          tags$script(HTML("
            $(function() {
              // Make panel draggable
              $('#floating-panel').draggable({ handle: '#floating-panel-header' });
              
              // Toggle collapse/expand
              $('#togglePanel').click(function() {
                $('#floating-panel').toggleClass('collapsed');
                if ($('#floating-panel').hasClass('collapsed')) {
                  $(this).html('<i class=\"fa fa-plus\"></i>');
                } else {
                  $(this).html('<i class=\"fa fa-minus\"></i>');
                }
              });
            });
          ")),
          
          # Toolbox floating and dragable and page length capture 
          tags$script(HTML(
            "$(function() {
              $('#floating-panel').draggable({ handle: '#floating-panel-header' });
            });",
                      "
            $(document).on('change', '.dataTables_length select', function() {
              Shiny.setInputValue('page_length', $(this).val());
            });
            "
          ))
        )
      },
      if(c_connected_to_db()) {
        if(data_selection_() == "Inputdaten") {
          tool_box(l_data_input(), lastEdited_data_set_name(), c_select_dropdown_data)
        } else if (data_selection_() == "Advance-Tickets"){
          tool_box(l_data_advance_tickets(), lastEdited_data_set_name(), c_select_dropdown_data, 2)
        } else {
          tool_box(l_data_dropdown(), lastEdited_data_set_name(), c_select_dropdown_data, 3)
        }
      },
      if(c_connected_to_db()){
        shiny::uiOutput("help_info")
      }
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
