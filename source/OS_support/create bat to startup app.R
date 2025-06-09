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

r_wd <- Sys.getenv("Kinoklub_wd")|>
  normalizePath()

if(nchar(r_wd) == 0) stop("Systemvarible `Kinoklub_wd` wurde nicht gefunden.")

r_wd <- paste0(r_wd, "/Start_Input_data_edit.R")|>
  r_win_path()
r_wd

r_wd|>
  r_win_path()
r_wd

c_raw <- readLines("source/OS_support/Kinoklub.bat")
c_raw


c_raw[3] <- paste0("\"",r_exe,"\""," ","\"", r_wd, "\"")

writeLines(c_raw, "source/OS_support/Kinoklub.bat")
