library(shiny)
library(DT)
library(tidyverse)
library(shinysky)

# Load the data
c_file <- paste0(getwd(),"/Input/template.Rds")
l_templates <- readRDS(c_file)

l_templates$Kategorien

# Define UI
ui <- fluidPage(
  titlePanel("Edit List Entries"),
  mainPanel(
    selectInput("dataset", "Choose a dataset:", choices = names(l_templates)),
    shiny::radioButtons("table_edit", "Funktion", choices = c("Zeilenauswahl", "Werte editieren")),
    DTOutput("table"),
    actionButton("add_row", "Add Row"),
    actionButton("delete_row", "Delete Selected Row(s)"),
    actionButton("duplicate_row", "Duplicate Selected Row(s)"), 
    actionButton("save", "Save Changes")
  )
)

# Reactive value to store the current dataset
current_data <- reactiveVal(tibble())
table_edit <- reactiveVal("multiple")
table_select <- reactiveVal(TRUE)

# Define server logic
server <- function(input, output, session) {
  # Observe dataset selection and update current_data
  observeEvent(input$dataset,{
    current_data(l_templates[[input$dataset]])
    
  })
  
  # Edit values or select rows
  observeEvent(input$table_edit, {
    if (input$table_edit == "Zeilenauswahl") {
      table_edit("multiple")
      table_select(FALSE)
    } else {
      table_edit("none")
      table_select(TRUE)
    }
  })
  
  # Render the DT table
  output$table <- renderDT({
    
    switch (
      input$dataset,
      "Einkauf Kiosk" = print("Einkauf Kiosk"),
      "Einnahmen" = print("Einnahmen"),
      "Ausgaben" = print("Ausgaben"),
      "Spezialpreisekiosk" = print("Spezialpreisekiosk"),
      "Verleiherabgaben" = print("Verleiherabgaben"),
      "Verleiher" = print("Verleiher"),
      "Buchhaltungskonten" = print("Buchhaltungskonten"),
      "Kategorie" = print("Kategorie"),
      "JaNein" = print("JaNein"),
      "Lieferanten" = print("Lieferanten"),
      paste0("Anything else: ",input$dataset)|>print()
    )

    datatable(
      current_data(),
      editable = table_select(),
      options = list(pageLength = nrow(current_data())),
      selection = table_edit()
    )
  })
  
  # Add a new row
  observeEvent(input$add_row, {
    new_row <- current_data()[1, ] |> mutate(across(everything(), ~ NA)) # Create an empty row
    updated_data <- bind_rows(current_data(), new_row)
    current_data(updated_data)
  })
  
  # Delete selected row(s)
  observeEvent(input$delete_row, {
    req(input$table_rows_selected) # Ensure a row is selected
    updated_data <- current_data()[-input$table_rows_selected, ]
    current_data(updated_data)
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
  
  # Save changes back to the list
  observeEvent(input$save, {
    l_templates[[input$dataset]] <<- current_data() # Update the list
    saveRDS(l_templates, c_file) # Save the updated list to the file
    showNotification("Changes saved successfully!", type = "message")
  })
  
  # Handle cell edits in the data
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    # Get the column types of the current dataset
    c_class <- sapply(current_data(), class)
    # Convert the edited value to the appropriate type
    updated_value <- switch(
      c_class[info$col],
      "numeric" = as.numeric(info$value),
      "integer" = as.integer(info$value),
      "Date" = as.Date(info$value),
      "character" = as.character(info$value),
      info$value # Default: keep as it is
    )
    # Handle conversion errors
    if (is.na(updated_value)) {
      showNotification("Invalid input: Value could not be converted to the required type.", type = "error")
      return()
    }
    # Update the dataset
    updated_data <- current_data()
    updated_data[info$row, info$col] <- updated_value 
    current_data(updated_data)
  })
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5001,
  launch.browser = TRUE
)