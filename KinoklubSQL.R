# Install necessary packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("DBI")) install.packages("DBI")
if (!require("RPostgres")) install.packages("RPostgres")
if (!require("tidyverse")) install.packages("tidyverse")

library(shiny)
library(DBI)
library(RPostgres)
library(tidyverse)

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

# Function to establish a database connection
db_connect <- function(password) {
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "kinoklub",
      host = "localhost",
      port = 5432,
      user = "db_admin",
      password = password
    )
    return(con)
  }, error = function(e) {
    message("Failed to connect to the database: ", e$message)
    return(NULL)
  })
}

# Serve the custom_styles directory
shiny::addResourcePath("custom_styles", "source")

# Reactive value to store the database connection
con <- reactiveVal(NULL)

### Define UI for application
ui <- fluidPage(
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles/Kinoklub_dark_gui.css")
  ),
  titlePanel("Kiosk Table Management"),
  sidebarLayout(
    sidebarPanel(
      passwordInput("Passwort", "Passwort"),  # Password input
      actionButton("connect", "Connect to Database"),  # Button to connect
      shiny::tags$hr(),
      numericInput("filter_id", "Filter by ID", value = NULL),  # Filter by ID
      actionButton("filter", "Filter Row"),  # Button to filter row
      shiny::tags$hr(),
      dateInput("datum", "Datum", value = Sys.Date()),
      textInput("verkaufsartikel", "Verkaufsartikel"),
      numericInput("verkaufspreis", "Verkaufspreis", value = 0),
      numericInput("anzahl", "Anzahl", value = 0),
      numericInput("kassiert", "Kassiert", value = 0),
      textInput("lieferant", "Lieferant"),
      numericInput("gewinn", "Gewinn", value = 0),
      actionButton("submit", "Add New Row"),  # Button to add a new row
      actionButton("save", "Save Changes")  # Button to save changes
    ),
    mainPanel(
      textOutput("message"),
      tableOutput("kiosk_table"),  # Render the Kiosk table here
      actionButton("refresh", "Refresh Table")  # Button to refresh the table
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the currently filtered row
  filtered_row <- reactiveVal(NULL)
  
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
  
  # Render the Kiosk table
  output$kiosk_table <- renderTable({
    req(con())  # Ensure the connection is valid
    fetch_kiosk_table(con())
  })
  
  # Refresh the table when the "Refresh" button is clicked
  observeEvent(input$refresh, {
    output$kiosk_table <- renderTable({
      req(con())
      fetch_kiosk_table(con())
    })
  })
  
  # Filter row by ID
  observeEvent(input$filter, {
    req(con(), input$filter_id)  # Ensure the connection and filter ID are valid
    
    row <- tbl(con(), "Kiosk") |>
      filter(ID == input$filter_id) |>
      collect()
    
    if (nrow(row) == 0) {
      output$message <- renderText("No row found with the specified ID.")
      filtered_row(NULL)
    } else {
      output$message <- renderText("Row filtered successfully.")
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
    }, error = function(e) {
      output$message <- renderText(paste("Failed to append to the database:", e$message))
    })
    
    # Refresh the Kiosk table display
    output$kiosk_table <- renderTable({
      fetch_kiosk_table(con())
    })
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
                  updated_row$Datum, updated_row$Verkaufsartikel, updated_row$Verkaufspreis,
                  updated_row$Anzahl, updated_row$Kassiert, updated_row$Lieferant,
                  updated_row$Gewinn, updated_row$ID
                ))
      output$message <- renderText("Row updated successfully.")
    }, error = function(e) {
      output$message <- renderText(paste("Failed to update the row:", e$message))
    })
    
    # Refresh the Kiosk table display
    output$kiosk_table <- renderTable({
      fetch_kiosk_table(con())
    })
  })
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5000,
  launch.browser = TRUE
)