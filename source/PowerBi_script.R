library(tidyverse)

# read system environment variables
c_env_var <- Sys.getenv()
c_path <- Sys.getenv()[str_detect(names(c_env_var), "KINOKLUB")]
setwd(c_path)
source("user_settings.R")
source("source/calculate.R")