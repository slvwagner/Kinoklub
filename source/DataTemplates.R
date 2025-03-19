# Data templates

library(tidyverse)

# read in data templates (for data type conversion)
l_data <- readRDS("Input/Data.Rds")


l_template <- l_data |> 
  lapply(function(x) {
    x |> 
      slice(1)
  })

l_template

saveRDS(l_template,file = "Input/template.Rds")
