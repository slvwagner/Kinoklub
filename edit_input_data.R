library(shiny)
library(shinyjs)
library(DT)
library(viridis)
library(colorspace)
library(tidyverse)

# Load the data
c_file <- "Input/Data.Rds"
if(file.exists(c_file)){
  l_data <- readRDS(c_file)
  c_backup_number <- length(list.files(path = "Input/backup", pattern = "backup"))
  if(!dir.exists("Input/backup")) dir.create("Input/backup")
  saveRDS(l_data, paste0("Input/backup/Data_backup",c_backup_number + 1,".Rds")) # Save the updated list to the file
}else{ # or load template date 
  c_file <- "Input/template.Rds"
  l_data <- readRDS(c_file)
  c_file <- "Input/Data.Rds"
}

# Format choices as factors
l_data$Einnahmen <- l_data$Einnahmen|>
  mutate(Kategorie = factor(Kategorie))

l_data$Ausgaben <- l_data$Ausgaben|>
  mutate(Kategorie = factor(Kategorie))

l_data$Verleiherabgaben  <- l_data$Verleiherabgaben|>
  mutate(Verleiher = factor(Verleiher))

l_data$Spezialpreisekiosk <- l_data$Spezialpreisekiosk |>
  mutate(Spezialpreis = factor(Spezialpreis) )

l_data$`Einkauf Kiosk` <- l_data$`Einkauf Kiosk`|>
  mutate(Lieferant = factor(Lieferant))

l_data$Einsatzplan <- l_data$Einsatzplan|>
  mutate(Verantwortlich = factor(Verantwortlich),
         `Operateur*in` = factor(`Operateur*in`),
         `Kasse/Bar 1` = factor(`Kasse/Bar 1`),
         `Kasse/Bar 2` = factor(`Kasse/Bar 2`),
         `Back-up` = factor(`Back-up`)
  )

l_data$Programm <- l_data$Programm|>
  mutate(Verleiher = factor(Verleiher),
         `Verleiher Angefragt?` = factor(`Verleiher Angefragt?`)
  )

l_data$Einsatzplan
l_data$Programm

# choices list
column_choices <- list(
  "Lieferant" = l_data$Lieferanten$Lieferantenname,
  "Kategorie" = l_data$Kategorie$Auswahl,
  "Buchungskonto" = l_data$Buchhaltungskonten$Buchungskontoname,
  "Verleiher" = l_data$Verleiher$Verleihername,
  "Kinoförderer gratis?" = l_data$JaNein$Auswahl,
  "Spezialpreis" = l_data$Spezialpreis$Spezialpreisname,
  "KDM ja oder nein" = l_data$JaNein$Auswahl,
  "Besucherzahlen an Verleiher gesendet" = l_data$JaNein$Auswahl,
  "Verleihervertrag abgelegt" = l_data$JaNein$Auswahl,
  "Verleiher Angefragt?" = l_data$`Status Filmliste`$`Status Filmliste`,
  "Verantwortlich" = ifelse(is.na(l_data$Kinoklubmitglieder$Vorname),"...",paste(l_data$Kinoklubmitglieder$Vorname, l_data$Kinoklubmitglieder$Nachname)),
  "Operateur*in" = ifelse(is.na(l_data$Kinoklubmitglieder$Vorname),"...",paste(l_data$Kinoklubmitglieder$Vorname, l_data$Kinoklubmitglieder$Nachname)),
  "Kasse/Bar 1" = ifelse(is.na(l_data$Kinoklubmitglieder$Vorname),"...",paste(l_data$Kinoklubmitglieder$Vorname, l_data$Kinoklubmitglieder$Nachname)),
  "Kasse/Bar 2" = ifelse(is.na(l_data$Kinoklubmitglieder$Vorname),"...",paste(l_data$Kinoklubmitglieder$Vorname, l_data$Kinoklubmitglieder$Nachname)),
  "Back-up" = ifelse(is.na(l_data$Kinoklubmitglieder$Vorname),"...",paste(l_data$Kinoklubmitglieder$Vorname, l_data$Kinoklubmitglieder$Nachname)),
  "Allgemeine Infos erhalten" = l_data$JaNein$Auswahl,
  "Kasse / Bar" = l_data$JaNein$Auswahl,
  "Programm" = l_data$JaNein$Auswahl,
  "Sonderevents" = l_data$JaNein$Auswahl,
  "Marketing" = l_data$JaNein$Auswahl,
  "Finanzen" = l_data$JaNein$Auswahl,
  "Sponsoring" = l_data$JaNein$Auswahl,
  "Koordination" = l_data$JaNein$Auswahl
)

# Floating tool box function 
tool_box_floating <- function(l_data_input, c_select = 1, page_length_var = NA) {
  tags$div(
    id = "floating-panel",
    tags$div(id = "floating-panel-header", "Werkzeuge"),
    selectInput("dataset", "Datensatz zum Editieren", selected = names(l_data_input)[c_select], choices = names(l_data_input)),
    # shiny::numericInput("page_lenght", "Wieviele Zeilen sollen angezeigt werden?", value = page_length_var),
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

# Regex validation function for Suisanummer
validate_suisanummer <- function(input) {
  p <- "^\\d{4}\\.\\d{3}$"
  grepl(p, input)
}
validate_suisanummer(c("1234.562","123.25"))

# handel joined tables  
convert_Einsatzplan <- function(df_temp, convert_to){
  if(nrow(df_temp) == 1 & convert_to == "char"){
    bind_cols(df_temp|>
                select(1:4),
              df_temp|>
                select(5:9)|>
                apply(2, as.character)|>
                t()|>
                as_tibble(),
              df_temp|>
                select(10:11)
    )
  } else if(nrow(df_temp) == 1 & convert_to == "fact"){
    bind_cols(df_temp|>
                select(1:4),
              df_temp|>
                select(5:9)|>
                apply(2, factor)|>
                t()|>
                as_tibble(),
              df_temp|>
                select(10:11)
    )
  }
  else if(convert_to == "char"){
    bind_cols(df_temp|>
                select(1:4),
              df_temp|>
                select(5:9)|>
                apply(2, as.character)|>
                as_tibble(),
              df_temp|>
                select(10:11)
    )
  } else if(convert_to == "fact"){
    bind_cols(df_temp|>
                select(1:4),
              df_temp |>
                select(5:9) |>
                mutate(across(everything(), factor)), # Apply factor column-wise without coercing to a matrix
              df_temp|>
                select(10:11)
    )
  }
}

# Define UI
ui <- function(){
  fluidPage(
    shiny::headerPanel("Input Kinoklub"),
    # Function selection 
    shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                        choices = c("Inputdaten", "Dropdowns")
                        ),
    # Ensure jQuery UI is available for dragable tool box
    # includeScript("https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    includeScript("source/1.12.1_jquery-ui.js"),
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
}

###################################################
# Konstanten
Email_col_names <- c("Allgemeine Infos erhalten","Kasse / Bar", "Programm") # Email Verteilerauswahl
c_pageLength = 5 # Initial page length
c_lengthMenu = c(5:10, 20, 50, 100) # page length drop down options

################################################
# Einsatzplan temporary save

join_Einsatzplan <- function(l_data){
  # back up Einsatzplan
  l_data$Einsatzplan_ <- l_data$Einsatzplan
  # Einsatzplan to work with 
  l_data$Einsatzplan <- l_data$Programm|>
    select(1:5)|>
    left_join(
      l_data$Einsatzplan,
      by = "ID")|>
    select(-ID)
  return(l_data)
}
l_data <- join_Einsatzplan(l_data)

###################################################
# Split data to input and dropdown
c_select_input_data <- c(1:5,16,14)
c_select_dropdown_data <- c(6:13, 15, 17)

###################################################
# Reactive lists
l_data_input <- reactiveVal(l_data[c_select_input_data])
l_data_choices <- reactiveVal(l_data[c_select_dropdown_data])
l_data <- reactiveVal(l_data)

# drop down choices list
column_choices <- reactiveVal(column_choices)

# Reactive value to store the current data set
current_data <- reactiveVal(tibble())
# app behavior
table_edit <- reactiveVal("single")

# Edited data 
startup <- reactiveVal(TRUE)
lastEdited_data_set <- reactiveVal(NULL)
lastEdited_data_set_name <- reactiveVal("")

# last edit 
last_selected_row <- reactiveVal(1L)
last_selected_page <- reactiveVal(1L)
page_length_var <- reactiveVal(6L)

# Debug 
c_debug <- reactiveVal(0)

# System messages 
sys_msg <- reactiveVal("")

###################################################
# server logic
server <- function(input, output, session) {
  
  # observe event get email list 
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
  
  # select email verteiler and copy emails to clipboard 
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
    removeModal()
  })

  # Observe dataset selection and update current_data
  observeEvent(input$dataset, {
    if(startup()){ # only run on app start up
      current_data(l_data()[[input$dataset]])
      lastEdited_data_set(l_data()[[input$dataset]])
      lastEdited_data_set_name(input$dataset)
      startup(FALSE)
    }else{ # run on changing the data set
      if(all.equal(current_data(),lastEdited_data_set()) |>class() == "logical"){ 
        # only ask to save if there is something to save  
        current_data(l_data()[[input$dataset]])
        lastEdited_data_set(l_data()[[input$dataset]])
        lastEdited_data_set_name(input$dataset)
        return()
      } else { 
        # If a change has been made ask the user to save 
        showModal(modalDialog(
          title = paste0("Achtung ungespeicherte Änderungen in Input \"", lastEdited_data_set_name(), "\""),
          footer = tagList(
            actionButton("abort_save","Abrechen"),
            actionButton("save_edit","Speichern")
            )
        ))
      }
    }
  })
  
  # Abort changes and update 
  observeEvent(input$abort_save, {
    current_data(l_data()[[input$dataset]])
    lastEdited_data_set(l_data()[[input$dataset]])
    lastEdited_data_set_name(input$dataset)
    removeModal()
  })
  
  # Save changes and update 
  observeEvent(input$save_edit, {
    l_temp <- l_data() # get data list
    
    # specific data handling 
    if (lastEdited_data_set_name() == "Einsatzplan") {
      print("here")
      l_temp$Programm
      l_temp$Einsatzplan_ <- NULL
      l_temp$Einsatzplan <- bind_cols(tibble(ID = 1:nrow(current_data())), 
                current_data()|>
                  select(-(1:4))
                )
    } else { # anything else
      l_temp[[lastEdited_data_set_name()]] <- current_data() # Update the list with current edits

    }
    saveRDS(l_temp, c_file) # Save the updated list into file
    readRDS(c_file)
    l_data() # update data
    list(  # update choices
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
      "Verantwortlich" = ifelse(is.na(l_data()$Kinoklubmitglieder$Vorname),"...",paste(l_data()$Kinoklubmitglieder$Vorname, l_data()$Kinoklubmitglieder$Nachname)),
      "Operateur*in" = ifelse(is.na(l_data()$Kinoklubmitglieder$Vorname),"...",paste(l_data()$Kinoklubmitglieder$Vorname, l_data()$Kinoklubmitglieder$Nachname)),
      "Kasse/Bar 1" = ifelse(is.na(l_data()$Kinoklubmitglieder$Vorname),"...",paste(l_data()$Kinoklubmitglieder$Vorname, l_data()$Kinoklubmitglieder$Nachname)),
      "Kasse/Bar 2" = ifelse(is.na(l_data()$Kinoklubmitglieder$Vorname),"...",paste(l_data()$Kinoklubmitglieder$Vorname, l_data()$Kinoklubmitglieder$Nachname)),
      "Back-up" = ifelse(is.na(l_data()$Kinoklubmitglieder$Vorname),"...",paste(l_data()$Kinoklubmitglieder$Vorname, l_data()$Kinoklubmitglieder$Nachname)),
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
    lastEdited_data_set(l_data()[[input$dataset]])
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

  # Create Modal form to Edit selected row  
  observeEvent(input$edit_row, {
    if(!is.null(input$table_rows_selected)){
      # get actual data
      df_row <- current_data()
      
      l_temp <- list()
      for (ii in 1:ncol(df_row)) {
        col_name <- names(df_row[,ii])
        col_data_type <- df_row[,ii]|>
          pull()|>
          class()
        col_value <- current_data()[input$table_rows_selected,ii]|>pull()
        
        # handel Date inputs
        if(col_data_type == "Date"){
          print(col_data_type)
          l_temp[[ii]] <- 
            dateInput(inputId =  as.character(ii), 
                      label = col_name, 
                      value = ifelse(is.na(col_value), as.Date(NA), col_value), 
                      format = "dd.mm.yyyy", 
                      language = "de", 
                      weekstart = 1
            )
          l_temp[[ii]]
        } # handle numeric inputs
        else if(col_data_type %in% c("numeric", "integer")){ 
          print(col_data_type)
          l_temp[[ii]] <- 
            numericInput(inputId =  as.character(ii), 
                         label = col_name, 
                         value =  ifelse(is.na(col_value), NA, col_value),
                         step = 0.01
            )
        } # handle factor inputs
        else if (col_data_type == "factor"){
          col_value <- as.character(col_value)
          print(col_data_type)
          column_choices()[names(column_choices()) == col_name]
          c_choices <- column_choices()[names(column_choices()) == col_name]|>unlist()
          names(c_choices) <- NULL
          c_choices
          if(col_name %in% names(column_choices())){ # look up choices
            l_temp[[ii]] <- 
              shiny::selectInput(
                inputId = as.character(ii),
                label = col_name,
                choices = c_choices,
                selected = ifelse(is.na(col_value), NA, col_value)
              )
          }else {
            stop("you shoud not end here: factor else")
          }
        } else if (col_data_type == "character"){ # handle character inputs
          print(col_data_type)
          column_choices()[names(column_choices()) == col_name]
          c_choices <- column_choices()[names(column_choices()) == col_name]|>unlist()
          names(c_choices) <- NULL
          c_choices
          if(col_name == "Suisanummer"){
            # create text input for Suisanummer
            if(is.na(col_value)){
              generated_code <-paste0(
                "textInput(inputId = \"", as.character(ii),"\", label = \"",col_name,"\",", 
                " placeholder = \"xxxx.xxx\")"
              )
            }else{
              generated_code <-paste0(
                "textInput(inputId = \"", as.character(ii),"\", label = \"",col_name,"\",", 
                " value = \"",col_value,"\")"
              )
            }
            writeLines(generated_code)
            eval(parse(text = generated_code))
            l_temp[[ii]] <- eval(parse(text = generated_code))
            
          } else if (col_data_type == class(T)){
            l_temp[[ii]] <- 
              shiny::textInput(
                inputId = as.character(ii),
                label = col_name,
                value = ifelse(is.na(col_value), NA, col_value)
                )
          } else {
            l_temp[[ii]] <- 
              shiny::textInput(
                inputId = as.character(ii),
                label = col_name,
                value = ifelse(is.na(col_value), NA, col_value)
              )
          }
        }
      }
      # User interaction to save 
      showModal(
        modalDialog(title = "Zeile editieren",
                    l_temp,
                    actionButton("edit_row_value", "Werte übernehmen", class = "btn-info"),
                    actionButton("abort_save", "Abrechen"),
                    easyClose = FALSE, footer = NULL
        )
      )
    } else {
      # User interaction 
      showModal(
        modalDialog(title = "Bitte eine Zeile markieren",
                    easyClose = TRUE, footer = modalButton("Abbrechen")
        )
      )
    }
  })
  
  # Observe edit row value button
  observeEvent(input$edit_row_value, {
    # filter for selected data by user
    df_temp <- current_data()
    df_temp_ <- current_data()
    # get the user input
    generated_code <- paste0("input$`", 1:ncol(df_temp), "`")
    c_input <- sapply(generated_code, function(x) eval(parse(text = x)))
    names(c_input) <- NULL
    
    # Coerce user input to correct data type 
    l_input <- list()
    for (ii in 1:ncol(df_temp)) {
      c_input_class <- l_data()[[input$dataset]][,ii]|>pull()|>class()
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
      } else if (c_input_class %in% c("double", "numeric")) {
        l_input[[ii]] <- as.numeric(c_input[ii])
      }
      else if (c_input_class == "integer") {
        l_input[[ii]] <- as.integer(c_input[ii])
      } else if (c_input_class == "factor"){
        c_input[ii] <- as.character(c_input[ii])
        if (c_input[ii] == "" | c_input[ii] == "..."){
          l_input[[ii]] <- as.character(NA)
        } else {
          l_input[[ii]] <- as.character(c_input[ii])
        }
      } else {
        stop("should not end here")
      }
    }
    names(l_input) <- names(df_temp)
    df_updated <- l_input|>
      as_tibble()
    # check for changed data 
    if(is.logical(all.equal(df_temp[input$table_rows_selected,], df_updated))){
      # User interaction 
      showModal(
        modalDialog(title = "Es wurde nichts geändert!",
                    easyClose = TRUE, 
                    footer = actionButton("abort","Abbrechen")
        )
      )
    }else{
      # Specific data handling to store user input 
      if(lastEdited_data_set_name() == "Einsatzplan"){
        df_temp <- convert_Einsatzplan(df_temp, "char")
        df_temp[input$table_rows_selected,] <- df_updated
        df_temp <- convert_Einsatzplan(df_temp, "fact")
      } else { # anything else 
        df_temp[input$table_rows_selected,] <- df_updated
      }
      current_data(df_temp)
      removeModal()
    }
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
  })
  
  # abort: Es wurde nichts geändert! 
  observeEvent(input$abort,{
    removeModal()
  })
  
  # Add a new row top of selected
  observeEvent(input$add_row_top, {
    if(nrow(current_data()) == 0){ # get template data if no current data is available
      updated_data <- l_data()[[lastEdited_data_set_name()]][1, ]
      current_data(updated_data)
    } else { 
      if(is.null(input$table_rows_selected)){
        # User interaction 
        showModal(
          modalDialog(title = "Bitte eine Zeile markieren",
                      easyClose = TRUE, footer = modalButton("Abbrechen")
          )
        )
      } else {
        # get actuall data 
        new_row <- current_data()[1, ]|>mutate(across(everything(), ~ NA)) # Create an empty row
        if(input$table_rows_selected == 1){ # add row on top
          updated_data <- 
            bind_rows(new_row,
                      current_data()[(input$table_rows_selected):nrow(current_data()),]
            )
          current_data(updated_data)
        }else{
          updated_data <- 
            bind_rows(current_data()[1:(input$table_rows_selected - 1),],
                      new_row,
                      current_data()[(input$table_rows_selected):nrow(current_data()),]
            )
          current_data(updated_data)
        }
      }
    }
    dataTableProxy("table")|>
      selectRows(last_selected_row() + 1)|>
      selectPage(last_selected_page())
  })
  
  # Add a new row bottom of selected
  observeEvent(input$add_row_bottom, {
    if(nrow(current_data()) == 0){ 
      # create new empty row with correct data type
      updated_data <- l_data()[[lastEdited_data_set_name()]][1, ]
      current_data(updated_data)
    } else { # Add row to data  
      if(is.null(input$table_rows_selected)){ # add row on bottom 
        # User interaction 
        showModal(
          modalDialog(title = "Bitte eine Zeile markieren",
                      easyClose = TRUE, footer = modalButton("Abbrechen")
          )
        )
      } else {
        # create new empty row with correct data type 
        new_row <- current_data()[1, ]|>mutate(across(everything(), ~ NA)) # Create an empty row
        if(input$table_rows_selected == nrow(current_data())){
          updated_data <- 
            bind_rows(current_data()[1:input$table_rows_selected,],
                      new_row
            )
          current_data(updated_data)
        }else {
          updated_data <- 
            bind_rows(current_data()[1:(input$table_rows_selected),],
                      new_row,
                      current_data()[(input$table_rows_selected + 1):nrow(current_data()),]
            )
          current_data(updated_data)
        }
      }
    }
    dataTableProxy("table")|>
      selectPage(last_selected_page())|>
      selectRows(last_selected_row())
      
  })
  
  # Duplicate selected row(s) and update "Gültig ab Datum"
  observeEvent(input$duplicate_row, {
    req(input$table_rows_selected) # Ensure a row is selected
    new_row <- current_data()[input$table_rows_selected, ]
    # Update "Gültig ab Datum" to the current system date
    if ("Gültig ab Datum" %in% colnames(new_row)) {
      new_row <- new_row |>
        mutate(`Gültig ab Datum` = Sys.Date())
    }

    if(nrow(current_data()) == 0){ 
      # create new empty row with correct data type
      updated_data <- l_data()[[lastEdited_data_set_name()]][1, ]
      current_data(updated_data)
    } else { # Add row to data  
      if(is.null(input$table_rows_selected)){ # add row on bottom 
        # User interaction 
        showModal(
          modalDialog(title = "Bitte eine Zeile markieren",
                      easyClose = TRUE, footer = modalButton("Abbrechen")
          )
        )
      } else {
        if(input$table_rows_selected == nrow(current_data())){
          updated_data <- 
            bind_rows(current_data()[1:input$table_rows_selected,],
                      new_row
            )
          current_data(updated_data)
        }else {
          updated_data <- 
            bind_rows(current_data()[1:(input$table_rows_selected),],
                      new_row,
                      current_data()[(input$table_rows_selected + 1):nrow(current_data()),]
            )
          current_data(updated_data)
        }
      }
    }
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
    
  })
  
  # User interaction Delete selected row(s) 
  observeEvent(input$delete_row, {
    showModal(modalDialog(
      title = "Möchten sie die selektierten Zeile(n) löschen?",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_delete", "Löschen")
      )
    ))
  })
  
  # Delete selected row 
  observeEvent(input$confirm_delete, {
    req(input$table_rows_selected)
    updated_data <- current_data()[-input$table_rows_selected, ]
    current_data(updated_data)
    removeModal()
  })
  
  # Render: Dynamically update the floating tool box
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      hr(),
      DTOutput("table"),
      if(input$data_selection == "Inputdaten"){
        # Floating tool box to edit input data 
        tool_box_floating(l_data_input(),2, page_length_var = page_length_var())
      } else {
        # Floating tool box for editing choices
        tool_box_floating(l_data_choices(), page_length_var = page_length_var())
      },
      
      # JavaScript to make the floating panel draggable
      tags$script(HTML("
        $(function() {
          $('#floating-panel').draggable({ handle: '#floating-panel-header' });
        });
      "))
    )
  })
  
  # observe Event select a row 
  observeEvent(input$table_rows_selected, {
    c_debug(c_debug()+1)
    # update last selected row  
    req(input$table_rows_selected)
    row <- input$table_rows_selected

    # has the page lenght changed? 
    if(!is.null(input$page_length)){
      page_length_var(input$page_length)
    }

    # update 
    page <-  ceiling(row / page_length_var())
    last_selected_page(page)
    last_selected_row(input$table_rows_selected)
    
    # Debug
    cat(
      "\n**************************\n",
      "Debug =", c_debug(),
      "\nObserve Event select a row:",
      "\nrow = ", last_selected_row(),
      "\npage = ", last_selected_page(),
      "\nlenght = ", page_length_var(),
      "\n**************************\n",
      sep = ""
    )
    
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
  })
  
  # Observe the change in page length
  observeEvent(input$page_length, {
    c_debug(c_debug()+1)
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
    
    # Debug
    cat(
      "\n**************************\n",
      "Debug =", c_debug(),
      "\nObserve Event page length:",
      "\nrow = ", last_selected_row(),
      "\npage = ", last_selected_page(),
      "\nlenght = ", page_length_var(),
      "\n**************************\n",
      sep = ""
    )
    
    dataTableProxy("table")|>
      selectRows(last_selected_row())|>
      selectPage(last_selected_page())
  })
  
  # Render data table output
  output$table <- renderDataTable({
    # rendering the datatable depens on the input data 
    # for certain input data sets other renderings may be needed
    if(input$data_selection == "Inputdaten") { # for all Input date change to user readable "Datum"
      print("Render data table output")
      # get crrent data
      df_temp <- current_data()
      # find all column names containing "Datum"
      df_Date <- current_data()|>
        select(contains("datum"))
      # create user readable Datum
      df_Date_user <-
        df_Date|>
        as.matrix()|>
        apply(2, function(x){
          x <- as.Date(x)
          x <- format(x, "%d.%m.%Y")
          return(x)
        })|>
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
          # observe the page lenght from data table
          initComplete = JS(
            "function(settings, json) {",
            "  var table = settings.oInstance.api();",
            "  table.on('length.dt', function(e, settings, len) {",
            "    Shiny.setInputValue('page_length', len);",
            "  });",
            "}"
          ),
          language = list(
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
        )
      )
    } else {
      # Create the DataTable for all other data sets
      dt <- datatable(
        df_temp,
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
          language = list(
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
        )
      )
    }
    
    # Apply conditional formatting for different data sets
    if (!is.null(input$dataset) && input$dataset == "Programm") {
      
      # Apply conditional formatting to columns
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
      names(l_data()[["Kinoklubmitglieder"]])
      c_Kinoklubmitglied <- 
        l_data()[["Kinoklubmitglieder"]]|>
        mutate(Kinoklubmitglied = paste(Vorname, Nachname))|>
        select(Kinoklubmitglied)|>
        pull()
      
      c_Kinoklubmitglied <- ifelse(c_Kinoklubmitglied == "NA NA", NA, c_Kinoklubmitglied)
      
      # Generate the magma color palette s
      magma_colors <- viridis(length(c_Kinoklubmitglied), option = "turbo")
      
      # Lighten the colors to create a pastel effect
      pastel_magma <- lighten(magma_colors, amount = 0.5)  # Adjust `amount` for more/less pastel effect
      
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
          paste0(
            "Conditionall formating error:\n",
            e$message
          )|>sys_msg()
        
      })
    }
    sys_msg()|>
      writeLines()
    return(dt)
  })
  
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5001,
  launch.browser = TRUE
)
