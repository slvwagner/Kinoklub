library(shiny)
library(testthat)
library(shinytest2)
library(RSQLite)

test_buttons

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

test_that("All action buttons work correctly", {
  # Setup test database
  test_con <- create_test_db()
  on.exit(dbDisconnect(test_con))
  
  with_mock(
    DB_connect = function(pw, DB_user, con) {
      return(test_con)
    },
    {
      app <- AppDriver$new(app_dir = ".", name = "kinoklub_buttons_test")
      
      # Connect to database
      app$set_inputs(user = "test_user", SQL_PW = "test_pw")
      app$click("SQL_connect")
      
      # Test 1: Edit Row Button
      test_that("Edit row button works", {
        app$set_inputs(`table_rows_selected` = 1)
        app$click("edit_row")
        expect_true(app$is_visible(selector = ".modal"))
        
        # Test editing a field
        app$set_inputs(`3` = "Modified Title")  # Assuming Filmtitel is 3rd input
        app$click("edit_row_value")
        
        # Verify change
        new_title <- app$get_html(selector = "td[data-title='Filmtitel']") %>% 
          rvest::html_text()
        expect_equal(new_title[1], "Modified Title")
      })
      
      # Test 2: Add Row Top Button
      test_that("Add row top button works", {
        initial_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        
        app$set_inputs(`table_rows_selected` = 1)
        app$click("add_row_top")
        
        new_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        expect_equal(new_rows, initial_rows + 1)
      })
      
      # Test 3: Add Row Bottom Button
      test_that("Add row bottom button works", {
        initial_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        
        app$set_inputs(`table_rows_selected` = 1)
        app$click("add_row_bottom")
        
        new_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        expect_equal(new_rows, initial_rows + 1)
      })
      
      # Test 4: Duplicate Row Button
      test_that("Duplicate row button works", {
        initial_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        
        app$set_inputs(`table_rows_selected` = 1)
        app$click("duplicate_row")
        
        new_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        expect_equal(new_rows, initial_rows + 1)
      })
      
      # Test 5: Delete Row Button
      test_that("Delete row button works", {
        initial_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        
        app$set_inputs(`table_rows_selected` = 1)
        app$click("delete_row")
        app$click("confirm_delete")
        
        new_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        expect_equal(new_rows, initial_rows - 1)
      })
      
      # Test 6: Check Unique Button (for dropdown tables)
      test_that("Check unique button works", {
        # Switch to dropdown data
        app$set_inputs(dataset = "Kinoklubmitglieder")
        app$set_inputs(`data_selection` = "Dropdowns")
        
        app$click("check_unique")
        expect_true(app$is_visible(selector = ".modal"))
        app$click("abort")  # Close modal
      })
      
      # Test 7: Email Distribution Button
      test_that("Email distribution button works", {
        app$click("get_email")
        expect_true(app$is_visible(selector = ".modal"))
        
        # Test email list generation
        app$set_inputs(Verteiler = "Kasse / Bar")
        app$click("get_email_verteiler")
        
        # Verify modal shows success message
        success_msg <- app$get_html(selector = ".modal") %>% 
          rvest::html_text()
        expect_true(grepl("kopiert", success_msg))
      })
      
      # Test 8: Archive/Filmtitel ändern Button
      test_that("Archive/Filmtitel ändern button works", {
        # Switch back to Programm
        app$set_inputs(dataset = "Programm")
        app$set_inputs(`data_selection` = "Inputdaten")
        
        initial_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        
        app$set_inputs(`table_rows_selected` = 1)
        app$click("archive_row")
        
        # Verify new row was added with status changed
        new_rows <- app$get_html(selector = ".dataTable tbody tr") %>% 
          rvest::html_text() %>% 
          length()
        expect_equal(new_rows, initial_rows + 1)
        
        # Verify status changed for the new row
        statuses <- app$get_html(selector = "td[data-title='Verleiher Angefragt?']") %>% 
          rvest::html_text()
        expect_true("Wird nicht gespielt" %in% statuses)
      })
      
      # Test 9: Database Disconnect Button
      test_that("Disconnect button works", {
        app$click("SQL_disconnect")
        expect_false(app$is_visible(selector = "#table"))
      })
    }
  )
})