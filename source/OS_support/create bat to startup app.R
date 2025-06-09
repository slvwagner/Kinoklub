# crate bat 

r_path <- function(x) {
  x <- chartr("\\", "/", x)
  return(x)
}

r_win_path <- function(x){
  x <- chartr("/","\\", x)
  return(x)
}

Sys.which("R")


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

r_wd <- paste0(r_wd, "/Start_Input_data_edit.R")|>
  r_win_path()
r_wd

r_wd|>
  r_win_path()
r_wd

c_raw <- readLines("source/OS_support/Kinoklub.template")
c_raw


c_raw[3] <- paste0("\"",r_exe,"\""," ","\"", r_wd, "\"")
c_raw

writeLines(c_raw, "source/OS_support/Kinoklub.bat")
message("Die Datei: ",getwd(),"`source/OS_support/Kinoklub.bat` wurde erstellt.")
