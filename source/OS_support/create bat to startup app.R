create_shortcut <- function(
    target_bat,
    shortcut_path,
    icon_path = NULL,
    working_dir = NULL,
    description = NULL
) {
  # Ensure all paths are normalized
  target_bat <- normalizePath(target_bat, winslash = "\\", mustWork = TRUE)
  shortcut_path <- normalizePath(shortcut_path, winslash = "\\", mustWork = FALSE)
  
  if (!grepl("\\.lnk$", shortcut_path, ignore.case = TRUE)) {
    shortcut_path <- paste0(shortcut_path, ".lnk")
  }
  
  icon_line <- if (!is.null(icon_path)) {
    icon_path <- normalizePath(icon_path, winslash = "\\", mustWork = TRUE)
    paste0("shortcut.IconLocation = \"", icon_path, "\"")
  } else {
    ""
  }
  
  working_dir_line <- if (!is.null(working_dir)) {
    working_dir <- normalizePath(working_dir, winslash = "\\", mustWork = TRUE)
    paste0("shortcut.WorkingDirectory = \"", working_dir, "\"")
  } else {
    ""
  }
  
  description_line <- if (!is.null(description)) {
    paste0("shortcut.Description = \"", description, "\"")
  } else {
    ""
  }
  
  vbs_script <- paste(
    "Set WshShell = CreateObject(\"WScript.Shell\")",
    paste0("Set shortcut = WshShell.CreateShortcut(\"", shortcut_path, "\")"),
    paste0("shortcut.TargetPath = \"", target_bat, "\""),
    icon_line,
    working_dir_line,
    description_line,
    "shortcut.Save",
    sep = "\n"
  )
  
  # Write VBS script
  vbs_file <- tempfile(fileext = ".vbs")
  writeLines(vbs_script, vbs_file)
  
  # Run the VBS script
  system(paste("cscript //nologo", shQuote(vbs_file)))
  
  # Cleanup
  unlink(vbs_file)
}






# crate bat 

r_path <- function(x) {
  x <- chartr("\\", "/", x)
  return(x)
}

r_win_path <- function(x){
  x <- chartr("/","\\", x)
  return(x)
}

r_exe <- Sys.which("Rscript")|>
  normalizePath()
r_exe

var_name <- "Kinoklub_wd"
r_wd <- Sys.getenv(var_name)|>
  normalizePath()

if((nchar(r_wd) == 0) | (r_wd != r_win_path(getwd()))) {
  # Define variable name and value

  var_value <- getwd()|>
    r_win_path()
  
  # Build the command
  cmd <- sprintf('setx %s "%s"', var_name, var_value)
  
  # Execute (use shell() on Windows for better behavior)
  shell(cmd)
  
  message("Systemvarible `Kinoklub_wd` wurde erstellt.")
}

r_file <- paste0(r_wd, "/Start_Input_data_edit.R")|>
  r_win_path()
r_file

c_raw <- readLines("source/OS_support/Kinoklub.template")
c_raw


c_raw[5] <- paste0("\"",r_exe,"\""," ","\"", r_file, "\"")
c_raw

# Write bat file
writeLines(c_raw, "source/OS_support/Kinoklub_input.bat")

# create shortcut
create_shortcut(
  target_bat = paste0(getwd(),"/source/OS_support/Kinoklub_input.bat"),
  shortcut_path = paste0(getwd(),"/source/OS_support/Kinoklub input"),
  icon_path = paste0(getwd(),"/source/OS_support/wagnius.ico"),
  working_dir = getwd(),
  description = "Kinoklub Input Tabellen"
)
message("Die Datei: ",getwd(),"`source/OS_support/Kinoklub_input.bat` wurde erstellt.")


r_file <- paste0(r_wd, "/Start_GUI.R")|>
  r_win_path()
r_wd

c_raw <- readLines("source/OS_support/Kinoklub.template")
c_raw

c_raw[5] <- paste0("\"",r_exe,"\""," ","\"", r_file, "\"")
c_raw

# Write bat file
writeLines(c_raw, "source/OS_support/Kinoklub_GUI.bat")

# create shortcut
create_shortcut(
  target_bat = paste0(getwd(),"/source/OS_support/Kinoklub_GUI.bat"),
  shortcut_path = paste0(getwd(),"/source/OS_support/Kinoklub GUI"),
  icon_path = paste0(getwd(),"/source/OS_support/wagnius.ico"),
  working_dir = getwd(),
  description = "Kinoklub GUI"
)

message("Die Datei: ",getwd(),"`source/OS_support/Kinoklub_GUI.bat` wurde erstellt.")



