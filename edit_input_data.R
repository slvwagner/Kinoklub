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

column_choices <- reactiveVal(list(
  "Lieferant" = l_data$Lieferanten$Lieferantenname,
  "Kategorie" = l_data$Kategorie$Auswahl,
  "Buchungskonto" = l_data$Buchhaltungskonten$Buchungskontoname,
  "Verleiher" = l_data$Verleiher$Verleihername,
  "Kinoförderer gratis?" = l_data$JaNein$Auswahl,
  "Spezialpreis" = l_data$Spezialpreis$Spezialpreisname
))

# Html input choices
generate_html_inputs <- function(row, row_index) {
  l <- list()
  for (ii in names(row)) {
    value <- as.character(row[[ii]])  # Ensure consistent character conversion
    
    if (ii %in% names(column_choices())) {
      choices <- column_choices()[[ii]]
      options_html <- paste0(
        '\t<option value="', choices, '" ', ifelse(choices == ifelse(is.na(value),"", value), 'selected', ''), '>', choices, '</option>',
        collapse = "\n"
      )
      l[[ii]] <- paste0(
        '<select class="new_input" data-row="', row_index, '" data-col="', ii, '">', "\n", options_html, '</select>'
      )
      # writeLines(l[[ii]])
      
    } else {
      l[[ii]] <- value
      # writeLines(value)
    }
  }
  return(l)
}

# Generate html output table
create_datatable <- function(data, table_edit, table_select) {
  # create a row_index 
  temp <- data |>
    mutate(row_index = row_number()) |>
    apply(1, function(row) generate_html_inputs(row, row["row_index"])) |>
    bind_rows()
  temp
  
  # Finde columns containing a Date
  date_col <- names(temp)|>
    str_detect(rebus::or("datum", "Datum"))
  for (ii in 1:length(date_col)) {
    if(date_col[ii]) temp[,ii] <- temp[,ii]|>pull()|>as.Date()
  }
  
  # Finde columns containing numeric values 
  numeric_col <- names(temp)|>
    str_detect(rebus::OPEN_BRACKET)
  for (ii in 1:length(numeric_col)) {
    if(numeric_col[ii]) temp[,ii] <- temp[,ii]|>pull()|>as.numeric()
  }
  
  # create the datatable 
  temp |>
    datatable(
      editable = table_select,
      options = list(
        columnDefs = list(
          list(targets = ncol(current_data()) + 1, visible = FALSE)  # Hide column
        ),
        dom = 't',
        ordering = FALSE,
        scrollX = TRUE,
        pageLength = nrow(data)
      ),
      selection = table_edit,
      escape = FALSE
    )
}

# Define UI
ui <- function(){
  fluidPage(
    titlePanel("Dateien editieren"),
    tags$head(
      tags$script(HTML("
        $(document).on('change', '.new_input', function() {
          var row = $(this).data('row');
          var col = $(this).data('col');
          var value = $(this).val();
          Shiny.setInputValue('select_change', {row: row, col: col, value: value}, {priority: 'event'});
        });
      "))
    ),
    mainPanel(
      selectInput("dataset", "Wähle ein Datensatz zum Editieren", choices = names(l_data)),
      shiny::radioButtons("table_edit", "Funktion", choices = c("Zeilenauswahl", "Werte editieren")),
      DTOutput("table"),
      actionButton("add_row", "Zeile hinzufügen"),
      actionButton("delete_row", "Löschen der selektierenen Zeile(n)"),
      actionButton("duplicate_row", "Dublizieren der selektierten Zeile(n)"), 
      actionButton("save", "Speichern")
    )
    
  )
}

# Reactive value to store the current dataset
current_data <- reactiveVal(tibble())
# app behaivior
table_edit <- reactiveVal("multiple")
table_select <- reactiveVal(TRUE)
# Edited data 
l_temp <-reactiveVal(list())

# Helper function to update a table
update_table <- function(data,row_index, col_index, value) {
  updated_data <- data
  # Get the column types of the current dataset
  c_class <- sapply(updated_data, class)
  # Convert the edited value to the appropriate type
  updated_value <- switch(
    c_class[col_index],
    "numeric" = as.numeric(value),
    "integer" = as.integer(value),
    "Date" = as.Date(value),
    "character" = as.character(value),
    value # Default: keep as it is
  )
  # Handle conversion errors
  if (is.na(updated_value)) {
    showNotification("Invalid input: Value could not be converted to the required type.", type = "error")
    return()
  }
  # Update the dataset
  updated_data[row_index, col_index] <- updated_value
  return(updated_data)
}

# Define server logic
server <- function(input, output, session) {
  # Observe dataset selection and update current_data
  observeEvent(input$dataset, {
    current_data(l_data[[input$dataset]])
  })
  
  # Edit cell values or select rows
  observeEvent(input$table_edit, {
    if (input$table_edit == "Zeilenauswahl") {
      table_edit("multiple")
      table_select(FALSE)
    } else {
      table_edit("none")
      table_select(TRUE)
      l_temp(current_data())
    }
  })
  
  # Render the DT table
  output$table <- renderDataTable({
    if (!table_select()) {
      datatable(
        current_data(),
        editable = table_select(),
        filter = "top",
        options = list(
          # dom = 't',
          # ordering = FALSE,
          # scrollX = TRUE,
          pageLength = nrow(current_data())
        )
      )
    } else {
      if(nrow(current_data()) > 0) {
        create_datatable(current_data(), table_edit(), table_select())
      }
      else{
        datatable(
          current_data(),
          editable = table_select(),
          filter = "top",
          options = list(
            # dom = 't',
            # ordering = FALSE,
            # scrollX = TRUE,
            pageLength = nrow(current_data())
          )
        )
      }
    }
  })
  
  # Handle cell edits with choices to <select> elements
  observeEvent(input$select_change, {
    req(input$select_change)
    # Extract the row and column from the event
    row_index <- input$select_change$row
    col_name <- input$select_change$col
    # Find the column index
    col_index <- which(names(current_data()) == col_name)
    # Update the table
    updated_data <- update_table(l_temp(),row_index, col_index, input$select_change$value)
    l_temp(updated_data)
  })
  
  # Handle any other cell edits 
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    if(info$value == "") {
      showNotification("Empty cell will not be updated", type = "message")
    }
    else{
      updated_data <- update_table(l_temp(),info$row, info$col, info$value)
      l_temp(updated_data) 
    }
  })
  
  # Add a new row
  observeEvent(input$add_row, {
    new_row <- current_data()[1, ] |> mutate(across(everything(), ~ NA)) # Create an empty row
    updated_data <- bind_rows(current_data(), new_row)
    current_data(updated_data)
  })
  
  # Delete selected row(s)
  observeEvent(input$delete_row, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete the selected row(s)?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete")
      )
    ))
  })
  # Confirm delete
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
    current_data(l_temp())
    l_data[[input$dataset]] <<- current_data() # Update the list
    saveRDS(l_data, c_file) # Save the updated list to the file
    l_data <- readRDS(c_file) # update data
    # update choices
    list(
      "Lieferant" = l_data$Lieferanten$Lieferantenname,
      "Kategorie" = l_data$Kategorie$Auswahl,
      "Buchungskonto" = l_data$Buchhaltungskonten$Buchungskontoname,
      "Verleiher" = l_data$Verleiher$Verleihername,
      "Kinoförderer gratis?" = l_data$JaNein$Auswahl,
      "Spezialpreis" = l_data$Spezialpreis$Spezialpreisname
    )|>
      column_choices()
    showNotification("Changes saved successfully!", type = "message")
  })
}


# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5001,
  launch.browser = TRUE
)