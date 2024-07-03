library(readr)

##########################################################################################
# read in csv export from wordpress homepage 

Filmvorschlaege_wordpress_export <- Filmvorschlaege_wordpress_export <-
  read_csv(
    "Input/WordPress/Filmvorschlaege_wordpress_export.csv",
    col_types = cols(
      ID = col_integer(),
      Excerpt = col_skip(),
      Date = col_datetime(format = "%Y-%m-%d"),
      `Post Type` = col_skip(),
      `Image Title` = col_skip(),
      `Image Caption` = col_skip(),
      `Image Description` = col_skip(),
      `Image Alt Text` = col_skip(),
      `Attachment URL` = col_skip(),
      Kategorien = col_skip(),
      altersfreigabe = col_integer(),
      bildupload = col_skip(),
      review = col_skip(),
      playdate = col_skip(),
      `_fpsm_form_alias` = col_skip(),
      rmp_vote_count = col_integer(),
      rmp_rating_val_sum = col_integer(),
      rmp_avg_rating = col_integer(),
      `_wp_trash_meta_status` = col_skip(),
      `_wp_trash_meta_time` = col_skip(),
      `_wp_desired_post_slug` = col_skip(),
      Status = col_skip(),
      `Author ID` = col_integer(),
      Slug = col_skip(),
      Format = col_skip(),
      Template = col_skip(),
      Parent = col_skip(),
      `Parent Slug` = col_skip(),
      Order = col_skip(),
      `Post Modified Date` = col_datetime(format = "%Y-%m-%d")
    )
  )

Filmvorschlaege_wordpress_export <- Filmvorschlaege_wordpress_export|>
  select(-starts_with("Author"),
         -ends_with("Status"))

##########################################################################################
# clean up Content column

p <- or("<"%R%one_or_more(WRD)%R%">",
        "</"%R%one_or_more(WRD)%R%">",
        "<"%R%one_or_more(WRD)%R%one_or_more(DGT)%R%">",
        "</"%R%one_or_more(WRD)%R%one_or_more(DGT)%R%">",
        "\\r",
        "\\n"
        )

Filmvorschlaege_wordpress_export$Content <- Filmvorschlaege_wordpress_export$Content|>
  lapply(function(x){
    str_trim(x)|>
      str_remove_all(p)
  })|>
  unlist()

Filmvorschlaege_wordpress_export$Content[6]

p <- or("\"",
        "\\\"")

for (ii in 1:nrow(Filmvorschlaege_wordpress_export)) {
  x <- Filmvorschlaege_wordpress_export$Content[ii]|>
    str_remove_all(p)
  run <- TRUE
  while (run) {
    m_pos <- x|>
      str_locate_all(or("<",
                        ">"
                        )
      )
    m_pos <- m_pos[[1]]
    
    if(nrow(m_pos)>1){
      x <- x|>
        substring(m_pos[,"start"][2]+1,nchar(x))
    }else{
      Filmvorschlaege_wordpress_export$Content[ii] <- x
      break
    }
  }
}

##########################################################################################
# Auswertung 

Filmvorschlaege_wordpress_export|>
  mutate(`Durchschnittliche-\nbewertung` = factor(rmp_avg_rating))|>
  ggplot(aes(Title, (rmp_vote_count), fill = `Durchschnittliche-\nbewertung`))+ 
  scale_y_continuous(breaks = 0:max(Filmvorschlaege_wordpress_export$rmp_vote_count, na.rm = T))+
  geom_col(na.rm = TRUE)+
  labs(y = "Anzahl Bewertungen",
       x = "Filmtitel",
       color = "Durchschnittlichebewertung")+
  coord_flip()|>
  suppressMessages()

c_fileName <- "output/Filmvorschläge.jpg"

ggplot2::ggsave(c_fileName, width = 25, height = 3 + nrow(Filmvorschlaege_wordpress_export) * 0.35, units = "cm")

##########################################################################################
# Write xlsx

library(openxlsx)

# Define the style for text wrapping
wrap_text <- createStyle(
  wrapText = TRUE
)

# Create the workbook and build it with the data
wb_xlsx <- buildWorkbook(Filmvorschlaege_wordpress_export, asTable = T, tableStyle = "TableStyleMedium2")
c_sheet_name <- "Filmvorschläge"
names(wb_xlsx) <- c_sheet_name

# Add style to the desired cells
addStyle(wb_xlsx, sheet = c_sheet_name, style = wrap_text, rows = 1:nrow(Filmvorschlaege_wordpress_export) + 1, cols = 3)
setColWidths(wb_xlsx, sheet = c_sheet_name, cols = 2, widths = 28)
setColWidths(wb_xlsx, sheet = c_sheet_name, cols = 3, widths = 80)
setColWidths(wb_xlsx, sheet = c_sheet_name, cols = 4, widths = 21)
setColWidths(wb_xlsx, sheet = c_sheet_name, cols = 28, widths = 26)
setColWidths(wb_xlsx, sheet = c_sheet_name, cols = 33, widths = 21)

## External Hyperlink
x <- Filmvorschlaege_wordpress_export$Permalink
class(x) <- "hyperlink"
writeData(wb_xlsx, c_sheet_name,, x = x, startRow = 2,startCol = 5)

x <- Filmvorschlaege_wordpress_export$`Image URL`
class(x) <- "hyperlink"
writeData(wb_xlsx, c_sheet_name,, x = x, startRow = 2,startCol = 6)

x <- Filmvorschlaege_wordpress_export$`Image Featured`
class(x) <- "hyperlink"
writeData(wb_xlsx, c_sheet_name,, x = x, startRow = 2,startCol = 7)

x <- Filmvorschlaege_wordpress_export$trailerlink
class(x) <- "hyperlink"
writeData(wb_xlsx, c_sheet_name,, x = x, startRow = 2,startCol = 9)

x <- Filmvorschlaege_wordpress_export$youtubeembed
class(x) <- "hyperlink"
writeData(wb_xlsx, c_sheet_name,, x = x, startRow = 2,startCol = 21)

x <- Filmvorschlaege_wordpress_export$Blinkverleih
class(x) <- "hyperlink"
writeData(wb_xlsx, c_sheet_name,, x = x, startRow = 2,startCol = 25)


##########################################################################################
# insert plots to work book
addWorksheet(wb_xlsx, "Plot")

insertImage(
  wb_xlsx,
  sheet = "Plot",
  file = c_fileName,
  startRow = 1,
  startCol = 1,
  width = 25,
  height = 3 + nrow(Filmvorschlaege_wordpress_export) * 0.35,
  units = "cm"
)

##########################################################################################
# Save the workbook to a file
saveWorkbook(wb_xlsx, file = "output/Filmvorschläge.xlsx", overwrite = TRUE )