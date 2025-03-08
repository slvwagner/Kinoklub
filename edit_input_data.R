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

# l_data$Einsatzplan <- l_data$Einsatzplan|>
#   mutate(Verleiher = NULL)
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
  "Operateur*in" = l_data$Kinoklubmitglieder$Kinoklubmitglied,
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



###################################################
# Kombinierte Dateinen

update_combinde_tables <- function(l_data){
  l_data$Einsatzplan <- l_data$Programm|>
    filter(`Verleiher Angefragt?` == pull(l_data$`Status Filmliste`[3,]))|>
    select(1:4,6)|>
    left_join(l_data$Einsatzplan)|>
    select(-`Verleiher Angefragt?`)
  # Create Kinoklubmitglied
  l_data$Kinoklubmitglieder <- l_data$Kinoklubmitglieder|>
    mutate(Kinoklubmitglied = if_else(is.na(Kinoklubmitglied), "...", paste(Nachname, Vorname))
    )
  return(l_data)
}
l_data <- update_combinde_tables(l_data)
saveRDS(l_data,c_file)

###################################################
# Floating tool box function 
tool_box_floating <- function(l_data_input, c_select = 1, pageLenght_var = NA) {
  tags$div(
    id = "floating-panel",
    tags$div(id = "floating-panel-header", "Werkzeuge"),
    selectInput("dataset", "Datensatz zum Editieren", selected = names(l_data_input)[c_select], choices = names(l_data_input)),
    shiny::numericInput("page_lenght", "Wieviele Zeilen sollen angezeigt werden?", value = 5),
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
  # library(rebus)
  # pattern <- DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT
  p <- "\\d\\d\\d\\d\\.\\d\\d\\d"
  grepl(p, input)
}
validate_suisanummer(c("1234.562","123.25"))

###################################################
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

###################################################
# Konstanten
Email_col_names <- c("Allgemeine Infos erhalten","Kasse / Bar", "Programm")

###################################################
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

last_selected_row <- reactiveVal(1)
last_selected_page <- reactiveVal(1)
pageLenght_var <- reactiveVal(5)

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
    l_temp[[lastEdited_data_set_name()]] <- current_data() # Update the list with current edits
    l_temp <- update_combinde_tables(l_temp) # update joined tables
    saveRDS(l_temp, c_file) # Save the updated list into file
    l_data(readRDS(c_file)) # update data
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
      "Verantwortlich" = l_data()$Kinoklubmitglieder$Kinoklubmitglied,
      "Operateur*in" = l_data()$Kinoklubmitglieder$Kinoklubmitglied,
      "Kasse/Bar 1" = l_data()$Kinoklubmitglieder$Kinoklubmitglied,
      "Kasse/Bar 2" = l_data()$Kinoklubmitglieder$Kinoklubmitglied,
      "Back-up" = l_data()$Kinoklubmitglieder$Kinoklubmitglied,
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
    if(lastEdited_data_set_name() != input$dataset){
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
      df_row <- l_data()[[input$dataset]][input$table_rows_selected,]|>
        as_tibble()
      
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
  
  # Observe edit row value button
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
        if (c_input[ii] == "" | c_input[ii] == "..."){
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
    if(is.logical(all.equal(df_temp[input$table_rows_selected,], df_updated))){
      # User interaction 
      showModal(
        modalDialog(title = "Es wurde nichts geändert!",
                    easyClose = TRUE, 
                    footer = actionButton("abort","Abbrechen")
        )
      )
    }else{
      df_temp[input$table_rows_selected,] <- df_updated
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
  
  # Change number or rows to be displayed by datatable
  observeEvent(input$page_lenght,{
    pageLenght_var(input$page_lenght)
    last_selected_row(input$table_rows_selected)
    row_num <- last_selected_row()
    if(!is.null(row_num)){ # only update if row is selected
      if (row_num > 0 && row_num <= nrow(current_data())) {
        # Calculate the page number where the row is located
        page_length <- pageLenght_var()  # Same as pageLength in datatable options
        page_num <- ceiling(row_num / page_length)
        last_selected_page(page_num)
      }
      dataTableProxy("table")|>
        selectPage(last_selected_page())|>
        selectRows(last_selected_row())
    }
  })
  
  # update last selected row and page in current datatable
  observeEvent(input$table_rows_selected,{
    last_selected_row(input$table_rows_selected)
    row_num <- last_selected_row()
    if (row_num > 0 && row_num <= nrow(current_data())) {
      # Calculate the page number where the row is located
      page_length <- pageLenght_var()  # Same as pageLength in datatable options
      page_num <- ceiling(row_num / page_length)
      last_selected_page(page_num)
    }
    dataTableProxy("table")|>
      selectPage(last_selected_page())|>
      selectRows(last_selected_row())
  })
  
  # Render: Dynamically update the floating tool box
  output$dynamicContent_output_panel <- shiny::renderUI({
    shiny::tagList(
      hr(),
      DTOutput("table"),
      if(input$data_selection == "Inputdaten"){
        # Floating tool box to edit input data 
        tool_box_floating(l_data_input,2, pageLenght_var = pageLenght_var())
      } else {
        # Floating tool box for editing choices
        tool_box_floating(l_data_choices, pageLenght_var = pageLenght_var())
      },
      
      # JavaScript to make the floating panel draggable
      tags$script(HTML("
        $(function() {
          $('#floating-panel').draggable({ handle: '#floating-panel-header' });
        });
      "))
    )
  })
  
  # Render the DT table
  output$table <- renderDataTable({
    datatable(
      current_data(),
      editable = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = pageLenght_var()
      )
    )
  })
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5001,
  launch.browser = TRUE
)
