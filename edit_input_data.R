library(shiny)
library(DT)
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

# l_data$Kinoklubmitglieder <-
#   readxl::read_excel("C:/Users/slvwa/Downloads/Kopie von Kinoklub Mitglieder.xlsx")|>
#   mutate(Kinoklubmitglied = paste(Vorname, Nachname))|>
#   mutate(`Helferfest 2024` = NULL) |>
#   mutate("Weiss nicht" = NULL)
# 
# l_data$Kinoklubmitglieder
# 
# saveRDS(l_data,c_file)

###################################################
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
  "Verantwortlich" = l_data$Kinoklubmitglieder$Kinoklubmitglied,
  "Operateur*in" = l_data$JaNein$Auswahl,
  "Kasse/Bar 1" = l_data$Kinoklubmitglieder$Kinoklubmitglied,
  "Kasse/Bar 2" = l_data$Kinoklubmitglieder$Kinoklubmitglied,
  "Back-up" = l_data$Kinoklubmitglieder$Kinoklubmitglied,
  "Allgemeine Infos erhalten" = l_data$JaNein$Auswahl,
  "Kasse / Bar" = l_data$JaNein$Auswahl,
  "Programm" = l_data$JaNein$Auswahl,
  "Sonderevents" = l_data$JaNein$Auswahl,
  "Marketing" = l_data$JaNein$Auswahl,
  "Finanzen" = l_data$JaNein$Auswahl,
  "Sponsoring" = l_data$JaNein$Auswahl,
  "Koordination" = l_data$JaNein$Auswahl
)

###################################################
# Split data to input and dropdown
c_select_input_data <- c(1:5,16,14)
c_select_dropdown_data <- c(6:13, 15, 17)

l_data_input <- l_data[c_select_input_data]
l_data_choices <- l_data[c_select_dropdown_data]

l_data_input
l_data_choices

# Floating tool box function 
tool_box_floating <- function(l_data_input, c_select = 1) {
  tags$div(
    id = "floating-panel",
    tags$div(id = "floating-panel-header", "Werkzeuge"),
    selectInput("dataset", "Datensatz zum Editieren", selected = names(l_data_input)[c_select], choices = names(l_data_input)),
    shiny::tags$hr(),
    actionButton("edit_row", "Zeile editieren", class = "btn-info"),
    shiny::tags$hr(),
    actionButton("add_row_top", "Zeile oben hinzufügen", class = "btn-info"),
    actionButton("add_row_bottom", "Zeile unten hinzufügen", class = "btn-info"),
    actionButton("duplicate_row", "Zeile duplizieren", class = "btn-info"),
    shiny::tags$hr(),
    actionButton("delete_row", "Zeile Löschen", class = "btn-danger"),
    shiny::tags$hr(),
    actionButton("save", "Speichern", class = "btn-success")
  )
}

# Regex validation function for Suisanummer
validate_suisanummer <- function(input) {
  # library(rebus)
  # pattern <- DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT
  p <- "\\d\\d\\d\\d\\.\\d\\d\\d"
  grepl(p, input)
}
validate_suisanummer(c("1234.562","123.25"))

# Define UI
ui <- 
  fluidPage(
    shiny::headerPanel("Input Kinoklub"),
    # Function selection 
    shiny::radioButtons(inputId =  "data_selection", label ="Welche Dateien sollen editiert werden?",
                        choices = c("Inputdaten", "Dropdowns")
                        ),
    # Ensure jQuery UI is available for dragable tool box
    includeScript("https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
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

# Reactive choices list
l_data <- reactiveVal(l_data)
column_choices <- reactiveVal(column_choices)
# Reactive value to store the current dataset
current_data <- reactiveVal(tibble())
# app behavior
table_edit <- reactiveVal("single")
# table_select <- reactiveVal(TRUE)

# Edited data 
startup <- reactiveVal(TRUE)
lastEdited_data_set <- reactiveVal(NULL)
lastEdited_data_set_name <- reactiveVal("")

# server logic
server <- function(input, output, session) {
  
  # Render: Dynamically update the floating tool box
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      hr(),
      DTOutput("table"),
      if(input$data_selection == "Inputdaten"){
        # Floating tool box to edit input data 
        tool_box_floating(l_data_input,2)
      } else {
        # Floating tool box for editing choices
        tool_box_floating(l_data_choices)
      },

      # JavaScript to make the floating panel draggable
      tags$script(HTML("
        $(function() {
          $('#floating-panel').draggable({ handle: '#floating-panel-header' });
        });
      "))
    )
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

  # Create Modal form to Edit selected row  
  observeEvent(input$edit_row, {
    if(!is.null(input$table_rows_selected)){
      df_row <- l_data()[[input$dataset]][input$table_rows_selected,]|>
        as_tibble()
      
      l_temp <- list()
      for (ii in 1:ncol(df_row)) {
        col_name <- names(df_row[,ii])
        col_data_type <- df_row[,ii]|>
          pull()|>
          class()
        col_value <- df_row[,ii]|>pull()
        
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
        else if(col_data_type == "numeric"){ 
          print(col_data_type)
          l_temp[[ii]] <- 
            numericInput(inputId =  as.character(ii), 
                         label = col_name, 
                         value =  ifelse(is.na(col_value), NA, col_value),
                         step = 0.01
            )
        }  # handle character inputs
        else if (col_data_type == "character"){
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
          }else{
            if(col_name == "Suisanummer"){
              # create text input for Suisanummer
              generated_code <- 
                paste0(
                  "textInput(inputId = \"", as.character(ii),"\", ", label = "\"",col_name,"\",", 
                  "value = ", ifelse(is.na(col_value),
                                    paste0("\"", "", "\"", collapse = ""), 
                                    paste0("\"",col_value, "\"", collapse = "")
                                   ),
                  ifelse(!is.na(col_value), 
                         "", 
                         ", placeholder = \"xxxx.xxx\""),
                  ")"
                  )
              generated_code
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
      }
      # User interaction to save 
      showModal(
        modalDialog(title = "Zeile editieren",
                    l_temp,
                    actionButton("edit_row_value", "Werte übernehmen", class = "btn-info"),
                    actionButton("abort_save", "Abrechen"),
                    easyClose = TRUE, footer = NULL
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
  
  # Observe edit row button
  observeEvent(input$edit_row_value, {
    # filter for selected data by user
    df_temp <- current_data()
    
    # get the user input
    generated_code <- paste0("input$`", 1:ncol(df_temp), "`")
    c_input <- sapply(generated_code, function(x) eval(parse(text = x)))
    names(c_input) <- NULL
    
    # Coerce user input to correct data type 
    l_input <- list()
    for (ii in 1:ncol(df_temp)) {
      c_input_class <- l_data()[[input$dataset]][,ii]|>pull()|>class()
      if(c_input_class == "character") {
        if (c_input[ii] == ""){
          l_input[[ii]] <- as.character(NA)
        } else {
          l_input[[ii]] <- as.character(c_input[ii])
        }
      }
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
      }else {
        stop("should not end here")
      }
    }
    names(l_input) <- names(df_temp)
    df_updated <- l_input|>
      as_tibble()
    # check for changed data 
    if(is.logical(all.equal(df_temp, df_updated))){
      # User interaction 
      showModal(
        modalDialog(title = "Speichern nicht möglich, es wurde nichts geändert!",
                    easyClose = TRUE, 
                    footer = modalButton("Abbrechen")
        )
      )
    }else{
      df_temp[input$table_rows_selected,] <- df_updated
      current_data(df_temp)
    }
    shiny::removeModal()
  })

  # Abort changes and update 
  observeEvent(input$abort_save, {
    lastEdited_data_set(l_data()[[input$dataset]])
    lastEdited_data_set_name(input$dataset)
    current_data(l_data()[[input$dataset]])
    removeModal()
  })
    
  # Save changes and update 
  observeEvent(input$save_edit, {
    l_data()[[lastEdited_data_set_name()]] <<- current_data() # Update the list
    saveRDS(l_data(), c_file) # Save the updated list to the file
    l_data(readRDS(c_file)) # update data
    # update choices
    list(
      "Lieferant" = l_data()$Lieferanten$Lieferantenname,
      "Kategorie" = l_data()$Kategorie$Auswahl,
      "Buchungskonto" = l_data()$Buchhaltungskonten$Buchungskontoname,
      "Verleiher" = l_data()$Verleiher$Verleihername,
      "Kinoförderer gratis?" = l_data()$JaNein$Auswahl,
      "Spezialpreis" = l_data()$Spezialpreis$Spezialpreisname
    )|>
      column_choices()
    showNotification("Changes saved successfully!", type = "message")
    lastEdited_data_set(l_data()[[input$dataset]])
    lastEdited_data_set_name(input$dataset)
    current_data(l_data()[[input$dataset]])
    removeModal()
  })
  
  # Render the DT table
  output$table <- renderDataTable({
    datatable(
      current_data(),
      editable = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = nrow(current_data())
      )
    )
  })
  
  # Add a new row top of selected
  observeEvent(input$add_row_top, {
    new_row <- current_data()[1, ] |> mutate(across(everything(), ~ NA)) # Create an empty row
    if(input$table_rows_selected != 1){
      updated_data <- 
        bind_rows(current_data()[1:(input$table_rows_selected-1),],
                  new_row,
                  current_data()[input$table_rows_selected:nrow(current_data()),]
        )
    }else{
      updated_data <- 
        bind_rows(new_row,
                  current_data()[input$table_rows_selected:nrow(current_data()),]
                  )
    }
    current_data(updated_data)
  })
  
  # Add a new row bottom of selected
  observeEvent(input$add_row_bottom, {
    new_row <- current_data()[1, ] |> mutate(across(everything(), ~ NA)) # Create an empty row
    if(input$table_rows_selected != ncol(current_data())){
      updated_data <- 
        bind_rows(current_data()[1:(input$table_rows_selected),],
                  new_row,
                  current_data()[(input$table_rows_selected + 1):nrow(current_data()),]
        )
    }else{
      updated_data <- 
        bind_rows(current_data()[1:input$table_rows_selected,],
                  new_row
                  )
    }
    current_data(updated_data)
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
  
  # Duplicate selected row(s) and update "Gültig ab Datum"
  observeEvent(input$duplicate_row, {
    req(input$table_rows_selected) # Ensure a row is selected
    selected_rows <- current_data()[input$table_rows_selected, ]
    # Update "Gültig ab Datum" to the current system date
    if ("Gültig ab Datum" %in% colnames(selected_rows)) {
      selected_rows <- selected_rows |>
        mutate(`Gültig ab Datum` = Sys.Date())
    }
    # Append the duplicated rows to the dataset
    updated_data <- bind_rows(current_data(), selected_rows)
    current_data(updated_data)
  })
  
  # Save changes and update 
  observeEvent(input$save, {
    l_temp <- l_data()
    l_temp[[input$dataset]] <- current_data() # Update the list
    saveRDS(l_temp, c_file) # Save the updated list to the file
    l_data(readRDS(c_file)) # update data
    # update choices
    list(
      "Lieferant" = l_data()$Lieferanten$Lieferantenname,
      "Kategorie" = l_data()$Kategorie$Auswahl,
      "Buchungskonto" = l_data()$Buchhaltungskonten$Buchungskontoname,
      "Verleiher" = l_data()$Verleiher$Verleihername,
      "Kinoförderer gratis?" = l_data()$JaNein$Auswahl,
      "Spezialpreis" = l_data()$Spezialpreis$Spezialpreisname
    )|>
      column_choices()
    showNotification("Changes saved successfully!", type = "message")
    lastEdited_data_set(l_data()[[input$dataset]])
    lastEdited_data_set_name(input$dataset)
    current_data(l_data()[[input$dataset]])
  })
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5001,
  launch.browser = TRUE
)
