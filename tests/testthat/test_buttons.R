library(shiny)
library(testthat)
library(shinytest2)
library(RSQLite)

# connection to Database
DB_connect <- function(pw, DB_user = "ch367079_flo", con = NULL) {
  # Database credentials
  host <- "lx51.hoststar.hosting"
  DB_name <- "ch367079_gui"
  
  # Check if connection already exists and is valid
  if (!is.null(con)) {
    return(con)
  } else {
    # Create a new connection
    con <- tryCatch({
      dbConnect(
        MySQL(),
        host = host,
        user = DB_user,
        password = pw,
        dbname = DB_name,
        port = 3306
      )
    }, error = function(e) {
      stop("Failed to connect to the database: ", e$message)
    })
    return(con)
  }
}

# Helper function to create test database
create_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Create simplified schema matching your app
  dbExecute(con, "CREATE TABLE Kinoklubmitglieder (
    ID INTEGER PRIMARY KEY,
    Vorname TEXT,
    Nachname TEXT,
    `E-Mail` TEXT,
    `Kasse / Bar` TEXT,
    Koordination TEXT,
    Operateurin TEXT,
    `Allgemeine Infos erhalten` TEXT
  )")
  
  dbExecute(con, "CREATE TABLE Programm (
    `Event ID` INTEGER PRIMARY KEY,
    Suisanummer TEXT,
    Filmtitel TEXT,
    Datum DATE,
    Zeit TEXT,
    Verleiher TEXT,
    `Verleiher Angefragt?` TEXT,
    `Link to Event ID` INTEGER
  )")
  
  dbExecute(con, "CREATE TABLE Einsatzplan (
    `Event ID` INTEGER,
    Verantwortlich TEXT,
    `Operateur*in` TEXT,
    `Kasse/Bar 1` TEXT,
    `Kasse/Bar 2` TEXT,
    `Back-up` TEXT,
    FOREIGN KEY(`Event ID`) REFERENCES Programm(`Event ID`)
  )")
  
  # Insert test data
  dbExecute(con, "INSERT INTO Kinoklubmitglieder VALUES 
    (1, 'Max', 'Mustermann', 'max@example.com', 'ja', 'ja', 'nein', 'ja'),
    (2, 'Erika', 'Musterfrau', 'erika@example.com', 'ja', 'nein', 'ja', 'ja')")
  
  dbExecute(con, "INSERT INTO Programm VALUES 
    (1, '1234.567', 'Test Film 1', '2023-01-01', '20:00', 'Verleiher A', 'Bestätigt', NULL),
    (2, '2345.678', 'Test Film 2', '2023-01-02', '19:30', 'Verleiher B', 'Anfrage läuft', NULL)")
  
  dbExecute(con, "INSERT INTO Einsatzplan VALUES 
    (1, 'Max Mustermann', 'Erika Musterfrau', 'Max Mustermann', 'Erika Musterfrau', ''),
    (2, '', '', '', '', '')")
  
  return(con)
}

library(shinytest2)
library(testthat)

test_that("Kinoklub action buttons work", {
  # Skip tests if not in interactive mode (for CI/CD you'll need proper setup)
  skip_on_ci()
  skip_if_not(interactive())
  
  # Start the app from the parent directory
  app <- AppDriver$new(
    name = "kinoklub_test",
    app_dir = ".",  # Points to directory containing app.R
    load_timeout = 20000  # Give more time for app to load
  )
  
  # Test database connection
  test_that("Database connection works", {
    app$set_inputs(user = "test_user", SQL_PW = "test_pw")
    app$click("SQL_connect")
    
    # Verify connection by checking if table appears
    expect_true(app$get_js("$('#table').length > 0"))
  })
  
  # Test edit button
  test_that("Edit button works", {
    app$set_inputs(`table_rows_selected` = 1)
    app$click("edit_row")
    
    # Verify modal appears
    expect_true(app$get_js("$('.modal').is(':visible')"))
    
    # Close modal
    app$click("abort_save")
  })
  
  # Test add row buttons
  test_that("Add row buttons work", {
    initial_count <- app$get_js("$('#table tbody tr').length")
    
    app$set_inputs(`table_rows_selected` = 1)
    app$click("add_row_top")
    expect_equal(app$get_js("$('#table tbody tr').length"), initial_count + 1)
    
    app$click("add_row_bottom")
    expect_equal(app$get_js("$('#table tbody tr').length"), initial_count + 2)
  })
  
  # Clean up
  app$stop()
})