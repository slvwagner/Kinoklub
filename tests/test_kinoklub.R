library(shiny)
library(testthat)
library(RSQLite)
library(shinytest2)

# Helper function to create a test database
create_test_db <- function() {
  # Create an in-memory SQLite database for testing
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Create tables with simplified schema for testing
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
    `Verleiher Angefragt?` TEXT
  )")
  
  dbExecute(con, "CREATE TABLE Einsatzplan (
    `Event ID` INTEGER,
    Suisanummer TEXT,
    Filmtitel TEXT,
    Datum DATE,
    Zeit TEXT,
    `Verleiher Angefragt?` TEXT,
    Verantwortlich TEXT,
    `Operateur*in` TEXT,
    `Kasse/Bar 1` TEXT,
    `Kasse/Bar 2` TEXT,
    `Back-up` TEXT,
    Kommentar TEXT,
    Trailer TEXT,
    FOREIGN KEY(`Event ID`) REFERENCES Programm(`Event ID`)
  )")
  
  # Insert test data
  dbExecute(con, "INSERT INTO Kinoklubmitglieder VALUES 
    (1, 'Max', 'Mustermann', 'max@example.com', 'ja', 'ja', 'nein', 'ja'),
    (2, 'Erika', 'Musterfrau', 'erika@example.com', 'ja', 'nein', 'ja', 'ja')")
  
  dbExecute(con, "INSERT INTO Programm VALUES 
    (1, '1234.567', 'Test Film 1', '2023-01-01', '20:00', 'Verleiher A', 'Bestätigt'),
    (2, '2345.678', 'Test Film 2', '2023-01-02', '19:30', 'Verleiher B', 'Anfrage läuft')")
  
  dbExecute(con, "INSERT INTO Einsatzplan VALUES 
    (1, '1234.567', 'Test Film 1', '2023-01-01', '20:00', 'Bestätigt', 'Max Mustermann', 'Erika Musterfrau', 'Max Mustermann', 'Erika Musterfrau', '', '', ''),
    (2, '2345.678', 'Test Film 2', '2023-01-02', '19:30', 'Anfrage läuft', '', '', '', '', '', '', '')")
  
  return(con)
}

# Test Suite
test_that("Kinoklub App Tests", {
  # Setup test database
  test_con <- create_test_db()
  on.exit(dbDisconnect(test_con))
  
  # Mock the DB connection function
  with_mock(
    DB_connect = function(pw, DB_user, con) {
      return(test_con)
    },
    {
      # Launch the app
      app <- AppDriver$new(app_dir = ".", name = "kinoklub_test")
      
      # Test 1: Database Connection
      test_that("Database connection works", {
        # Set credentials and connect
        app$set_inputs(user = "test_user", SQL_PW = "test_pw")
        app$click("SQL_connect")
        
        # Verify connection status
        expect_true(app$get_value(input = "SQL_connect") > 0)
        
        # Verify dynamic content appears
        expect_true(app$is_visible(selector = "#table"))
      })
      
      # Test 2: Data Table Rendering
      test_that("Data tables render correctly", {
        # Check initial table is Programm
        expect_equal(app$get_value(input = "dataset"), "Programm")
        
        # Verify table has expected columns
        table_cols <- app$get_html(selector = ".dataTable thead th") %>% 
          rvest::html_text()
        expect_true("Event ID" %in% table_cols)
        expect_true("Filmtitel" %in% table_cols)
        
        # Verify conditional formatting for Programm
        cell_color <- app$get_html(selector = "td[data-title='Verleiher Angefragt?']") %>% 
          rvest::html_attr("style")
        expect_true("background-color" %in% cell_color)
      })
      
      # Test 3: Row Editing
      test_that("Row editing works", {
        # Select first row
        app$set_inputs(`table_rows_selected` = 1)
        
        # Click edit button
        app$click("edit_row")
        
        # Verify modal appears
        expect_true(app$is_visible(selector = ".modal"))
        
        # Change a value and save
        app$set_inputs(`1` = "Modified Film Title")  # Assuming Filmtitel is first input
        app$click("edit_row_value")
        
        # Verify change was made
        updated_title <- app$get_html(selector = "td[data-title='Filmtitel']") %>% 
          rvest::html_text()
        expect_equal(updated_title[1], "Modified Film Title")
      })
      
      # Test 4: Adding Rows
      test_that("Adding rows works", {
        # Select first row
        app$set_inputs(`table_rows_selected` = 1)
        
        # Add row at top
        app$click("add_row_top")
        
        # Verify new row exists
        rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text()
        expect_length(rows, 3)  # Original 2 + 1 new
        
        # Verify the new row is at position 1
        new_row_id <- app$get_html(selector = "td[data-title='Event ID']") %>% 
          rvest::html_text()
        expect_equal(new_row_id[1], "3")  # Assuming auto-increment
      })
      
      # Test 5: Row Deletion
      test_that("Row deletion works", {
        # Select first row
        app$set_inputs(`table_rows_selected` = 1)
        
        # Click delete button
        app$click("delete_row")
        
        # Confirm deletion
        app$click("confirm_delete")
        
        # Verify row was deleted
        rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text()
        expect_length(rows, 2)  # Back to original count
      })
      
      # Test 6: Email Distribution List
      test_that("Email distribution works", {
        # Switch to Kinoklubmitglieder table
        app$set_inputs(dataset = "Kinoklubmitglieder")
        app$set_inputs(`data_selection` = "Dropdowns")
        
        # Click email button
        app$click("get_email")
        
        # Verify modal appears
        expect_true(app$is_visible(selector = ".modal"))
        
        # Select a distribution list
        app$set_inputs(Verteiler = "Kasse / Bar")
        app$click("get_email_verteiler")
        
        # Verify clipboard contains emails
        # Note: Actual clipboard testing may require additional setup
      })
      
      # Test 7: Database Disconnection
      test_that("Database disconnection works", {
        app$click("SQL_disconnect")
        
        # Verify table disappears
        expect_false(app$is_visible(selector = "#table"))
      })
    }
  )
})

# Run tests
test_dir("tests/")