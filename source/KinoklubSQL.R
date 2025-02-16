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
db_connect <- function() {
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "kinoklub",
      host = "localhost",
      port = 5432,
      user = "db_admin",
      password = rstudioapi::askForPassword("Database password")
    )
    return(con)
  }, error = function(e) {
    message("Failed to connect to the database: ", e$message)
    return(NULL)
  })
}

# Serve the custom_styles directory
shiny::addResourcePath("custom_styles", "source")

### Define UI for application
ui <- fluidPage(
  titlePanel("Add New Row to Kiosk Table"),
  sidebarLayout(
    sidebarPanel(
      textInput("Passwort", "Passwort"),
      shiny::tags$hr(),
      dateInput("datum", "Datum", value = Sys.Date()),
      textInput("verkaufsartikel", "Verkaufsartikel"),
      numericInput("verkaufspreis", "Verkaufspreis", value = 0),
      numericInput("anzahl", "Anzahl", value = 0),
      numericInput("kassiert", "Kassiert", value = 0),
      # textInput("suisanummer", "Suisanummer"),
      # textInput("artikel", "Artikel"),
      # numericInput("verkaufspreis2", "Verkaufspreis", value = 0),
      # textInput("menge", "Menge"),
      # numericInput("einkaufspreis", "Einkaufspreis", value = 0),
      textInput("lieferant", "Lieferant"),
      numericInput("gewinn", "Gewinn", value = 0),
      # numericInput("anzahl_bestellung", "Anzahl Bestellung Feb 2024", value = NA),
      # numericInput("einkaufspreis_gesamt", "Einkaufspreis gesamt", value = 0),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      textOutput("message"),
      tableOutput("kiosk_table")  # Render the Kiosk table here
    )
  )
)

# Reactive value to store the database connection
con <- reactiveVal(NULL)

# Define server logic
server <- function(input, output, session) {
  # Establish the database connection when the app starts
  observe({
    con(db_connect())
    if (is.null(con())) {
      output$message <- renderText("Failed to connect to the database. Please check your credentials.")
    }
  })
  
  # Render the Kiosk table
  output$kiosk_table <- renderTable({
    req(con())  # Ensure the connection is valid
    fetch_kiosk_table(con())
  })
  
  # input submit 
  observeEvent(input$submit, {
    req(con())  # Ensure the connection is valid else stop
    
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
      Suisanummer = input$suisanummer,
      Artikel = input$artikel,
      `Verkaufs-preis` = input$verkaufspreis2,
      Menge = input$menge,
      Einkaufspreis = input$einkaufspreis,
      Lieferant = input$lieferant,
      Gewinn = input$gewinn,
      `Anzahl Bestellung Feb 2024` = input$anzahl_bestellung,
      `Einkaufspreis gesamt` = input$einkaufspreis_gesamt,
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
}

# Run the app
shiny::runApp(
  host = "0.0.0.0",
  shiny::shinyApp(ui = ui, server = server),
  port = 5000,
  # Replace 8080 with your desired port
  launch.browser = TRUE # Automatically open in the system's default browser
)