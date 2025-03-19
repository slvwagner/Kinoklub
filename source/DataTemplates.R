# Data templates
library(lubridate)
library(hms)

l_template <- l_data |> 
  lapply(function(x) {
    x |> 
      slice(1) |> 
      mutate(
        across(where(is.character), ~NA_character_),
        across(where(is.double), ~NA_real_),
        across(where(is.integer), ~NA_integer_),
        across(where(is.factor), ~factor(NA, levels = levels(.))),
        across(where(lubridate::is.Date), ~as.Date(NA)),
        across(where(lubridate::is.POSIXct), ~as.POSIXct(NA, origin = "1970-01-01")),
        across(where(hms::is.hms), ~hms::as_hms(NA))
      )
  })

saveRDS(l_template,file = "Input/template.Rds")