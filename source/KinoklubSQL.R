# Install necessary packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("DBI")) install.packages("DBI")
if (!require("RPostgres")) install.packages("RPostgres")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DT")) install.packages("DT")  # Install DT package
if (!require("shinysky")) install.packages("shinysky")  # Install DT package

library(shiny)
library(DBI)
library(RPostgres)
library(tidyverse)
library(DT)  # Load DT package
# library(shinysky)

# Function to establish a database connection
db_connect <- function(password, user = "db_admin") {
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "kinoklub",
      host = "localhost",
      port = 5432,
      user = user,
      password = password
    )
    return(con)
  }, error = function(e) {
    message("Failed to connect to the database: ", e$message)
    return(NULL)
  })
}

# Function to fetch the Kiosk table
fetch_kiosk_table <- function(con) {
  if (!dbIsValid(con)) {
    stop("Database connection is not valid.")
  }
  tbl(con, "Kiosk") |>
    collect() |>  # Fetch the entire table into memory
    mutate(ID = as.character(ID) |> as.integer()) |>
    arrange(desc(ID)) |>
    select(ID, Verkaufsartikel, Verkaufspreis, Anzahl, Kassiert, Lieferant, Gewinn)
}

# Global variables
c_pageLength <- 15
column_definitions <- tibble(
  Lieferant = c("Schüwo", "Migros", "Nadia Wagner", "Stefan Jablonski")
  )
column_definitions

# Define UI for application
ui <- function(){
  fluidPage(
    titlePanel("Kiosk Table Management"),
    sidebarLayout(
      sidebarPanel(
        textInput("DB_User","DB User", placeholder = "db_admin"),
        passwordInput("Passwort", "Passwort"),  # Password input
        actionButton("connect", "Connect to Database"),  # Button to connect
        shiny::tags$hr(),
        shinysky::select2Input("select2_Lieferant","Lieferant",choices = column_definitions$Lieferant),
        numericInput("filter_id", "Filter by ID", value = NULL),  # Filter by ID
        actionButton("filter", "Filter Row"),  # Button to filter row
        dateInput("datum", "Datum", value = Sys.Date()),
        textInput("verkaufsartikel", "Verkaufsartikel"),
        numericInput("verkaufspreis", "Verkaufspreis", value = 0),
        numericInput("anzahl", "Anzahl", value = 0),
        numericInput("kassiert", "Kassiert", value = 0),
        textInput("lieferant", "Lieferant"),
        numericInput("gewinn", "Gewinn", value = 0),
        actionButton("submit", "Add New Row"),  # Button to add a new row
        actionButton("save", "Save Changes"),  # Button to save changes,
        actionButton("del_rows", "Delete Selected Rows")  # Button to delete selected rows
      ),
      mainPanel(
        textOutput("message"),
        DTOutput("kiosk_table"),  # Use DTOutput instead of tableOutput
        actionButton("refresh", "Refresh Table")  # Button to refresh the table
      )
    )
  )
}

# Reactive value to store the database connection
con <- reactiveVal(NULL)

# Reactive value to store the currently filtered row
filtered_row <- reactiveVal(NULL)

# Define server logic
server <- function(input, output, session) {

  # Establish the database connection when the "Connect" button is clicked
  observeEvent(input$connect, {
    req(input$Passwort)  # Ensure the password is provided
    con(db_connect(input$Passwort))
    if (is.null(con())) {
      output$message <- renderText("Failed to connect to the database. Please check your credentials.")
    } else {
      output$message <- renderText("Connected to the database successfully.")
    }
  })
  
  # Render the Kiosk table using DT with row selection enabled
  output$kiosk_table <- renderDT({
    req(con())  # Ensure the connection is valid
    fetch_kiosk_table(con())
  }, options = list(pageLength = c_pageLength), selection = 'multiple')  # Enable multiple row selection
  
  # Refresh the table when the "Refresh" button is clicked
  observeEvent(input$refresh, {
    output$kiosk_table <- renderDT({
      req(con())
      fetch_kiosk_table(con())
    }, options = list(pageLength = c_pageLength), selection = 'multiple')
  })
  
  # Delete selected rows when the "Delete Selected Rows" button is clicked
  observeEvent(input$del_rows, {
    req(con())  # Ensure the connection is valid
    # Get the selected rows
    selected_rows <- input$kiosk_table_rows_selected
    if (is.null(selected_rows)) {
      output$message <- renderText("No rows selected.")
      return()
    }
    # Fetch the current table data
    table_data <- fetch_kiosk_table(con())
    # Get the IDs of the selected rows
    selected_ids <- table_data[selected_rows, "ID"]
    # Delete the selected rows from the database
    tryCatch({
      pull(selected_ids)|>
        lapply(function(ID){
          dbExecute(con(), "DELETE FROM \"Kiosk\" WHERE \"ID\" = $1",
                    params = list(ID))
        })
      output$message <- renderText(paste("Deleted", length(selected_ids), "rows successfully."))
    }, error = function(e) {
      output$message <- renderText(paste("Failed to delete rows:", e$message))
    })
    # Refresh the Kiosk table display
    output$kiosk_table <- renderDT({
      fetch_kiosk_table(con())
    }, options = list(pageLength = c_pageLength), selection = 'multiple')
  })
  
  # Filter row by ID
  observeEvent(input$filter, {
    req(con(), input$filter_id)  # Ensure the connection and filter ID are valid
    
    row <- tbl(con(), "Kiosk") |>
      filter(ID == input$filter_id) |>
      collect()
    
    if (nrow(row) == 0) {
      output$message <- renderText(paste("No row found with ID:", input$filter_id))
      filtered_row(NULL)
    } else {
      output$message <- renderText(paste("Row with ID", input$filter_id, "filtered successfully."))
      filtered_row(row)
      
      # Populate the form fields with the filtered row's data
      updateDateInput(session, "datum", value = row$Datum)
      updateTextInput(session, "verkaufsartikel", value = row$Verkaufsartikel)
      updateNumericInput(session, "verkaufspreis", value = row$Verkaufspreis)
      updateNumericInput(session, "anzahl", value = row$Anzahl)
      updateNumericInput(session, "kassiert", value = row$Kassiert)
      updateTextInput(session, "lieferant", value = row$Lieferant)
      updateNumericInput(session, "gewinn", value = row$Gewinn)
    }
  })
  
  # Handle form submission for adding a new row
  observeEvent(input$submit, {
    req(con())  # Ensure the connection is valid
    
    if (is.na(input$verkaufsartikel) || input$verkaufsartikel == "") {
      output$message <- renderText("Verkaufsartikel is required.")
      return()
    }
    
    # Get the maximum ID from the Kiosk table
    max_ID <- tbl(con(), "Kiosk") |>
      select(ID) |>
      pull() |>
      max() |>
      as.integer()
    
    # Create a new row from the form inputs
    new_row <- tibble(
      Datum = input$datum,
      Verkaufsartikel = input$verkaufsartikel,
      Verkaufspreis = input$verkaufspreis,
      Anzahl = input$anzahl,
      Kassiert = input$kassiert,
      Lieferant = input$lieferant,
      Gewinn = input$gewinn,
      ID = max_ID + 1
    )
    
    # Append the new row to the Kiosk table
    tryCatch({
      dbAppendTable(con(), "Kiosk", new_row)
      output$message <- renderText("New row added successfully.")
      
      # Clear the form fields
      updateDateInput(session, "datum", value = Sys.Date())
      updateTextInput(session, "verkaufsartikel", value = "")
      updateNumericInput(session, "verkaufspreis", value = 0)
      updateNumericInput(session, "anzahl", value = 0)
      updateNumericInput(session, "kassiert", value = 0)
      updateTextInput(session, "lieferant", value = "")
      updateNumericInput(session, "gewinn", value = 0)
    }, error = function(e) {
      output$message <- renderText(paste("Failed to append to the database:", e$message))
    })
    
    # Refresh the Kiosk table display
    output$kiosk_table <- renderDT({
      fetch_kiosk_table(con())
    }, options = list(pageLength = c_pageLength), selection = 'multiple')
  })
  
  # Handle saving changes to the filtered row
  observeEvent(input$save, {
    req(con(), filtered_row())  # Ensure the connection and filtered row are valid
    
    # Create an updated row from the form inputs
    updated_row <- tibble(
      Datum = input$datum,
      Verkaufsartikel = input$verkaufsartikel,
      Verkaufspreis = input$verkaufspreis,
      Anzahl = input$anzahl,
      Kassiert = input$kassiert,
      Lieferant = input$lieferant,
      Gewinn = input$gewinn,
      ID = filtered_row()$ID
    )
    
    # Update the row in the database
    tryCatch({
      dbExecute(con(), "UPDATE \"Kiosk\" SET
                \"Datum\" = $1, \"Verkaufsartikel\" = $2, \"Verkaufspreis\" = $3, \"Anzahl\" = $4,
                \"Kassiert\" = $5, \"Lieferant\" = $6, \"Gewinn\" = $7
                WHERE \"ID\" = $8",
                params = list(
                  updated_row$Datum, updated_row$Verkaufsartikel, updated_row$Verkaufspreis, updated_row$Anzahl, 
                  updated_row$Kassiert, updated_row$Lieferant, updated_row$Gewinn, 
                  updated_row$ID
                  )
                )
      output$message <- renderText("Row updated successfully.")
    }, error = function(e) {
      output$message <- renderText(paste("Failed to update the row:", e$message))
    })
    
    # Refresh the Kiosk table display
    output$kiosk_table <- renderDT({
      fetch_kiosk_table(con())
    }, options = list(pageLength = c_pageLength), selection = 'multiple')
  })
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5000,
  launch.browser = TRUE
)