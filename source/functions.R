#######################################################################################
# spez. Round for Swiss currency "CHF"
#######################################################################################

round5Rappen <- function(zahl) {
  result <- lapply(zahl, function(zahl){
    if(is.na(zahl)){
      return(NA)
    }
    else{
      x <- round(zahl-as.integer(zahl*10)/10,6)
      if(x>=0.075){
        return((as.integer(zahl*10)/10)+0.1)
      }else {
        if(x>=0.025){
          return((as.integer(zahl*10)/10)+0.05)
        }else{
          return((as.integer(zahl*10)/10)+0.0)
        }
      }  
    }
  })
  result|>
    unlist()
}

#######################################################################################
# variable is present in global environment
#######################################################################################
r_is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}

# library is loaded in global environment
r_is.library_loaded <- function(package_name) {
  is_loaded <- FALSE
  tryCatch({
    is_loaded <- requireNamespace(package_name, quietly = TRUE)
  }, error = function(e) {
    is_loaded <- FALSE
  })
  return(is_loaded)
}

##  --------------------------------------------------------
##  Cleanup R console: clc
##  Version 1.0
##  by Hubert Ronald
##  --------------------------------------------------------
##  check first if you have "clc.R" in your currently root
##  sum(list.files() %in% "clc.R")

##  if TRUE then
##  Write in console: source("clc.R")
##  else
##  setwd() or source("yourPath/clc.R")

##  after write "clc" in console or when 
##  you need clean it

##  Console or Terminal'll be clean

##  --------------------------------------------------------
##  Reference:

##  English
##  https://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r-and-rstudio

##  Espaniol - Spanish
##  https://es.stackoverflow.com/questions/237112/c%C3%B3mo-limpiar-la-consola-de-rstudio-y-los-terminales-rstudio-y-macos
##  --------------------------------------------------------

##  The MIT License
##  Copyright 2019 Hubert Ronald

##  Permission is hereby granted, free of charge, to any person obtaining a copy
##  of this software and associated documentation files (the "Software"), to deal
##  in the Software without restriction, including without limitation the rights
##  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
##  copies of the Software, and to permit persons to whom the Software is furnished 
##  to do so, subject to the following conditions:

##  The above copyright notice and this permission notice shall be included in 
##  all copies or substantial portions of the Software.

##  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
##  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
##  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
##  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
##  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
##  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
##  IN THE SOFTWARE.


##  ------------------------------------------------------------
##  Detect RStudio Terminal or RStudio Console or Terminal macOS
##  --------------------------------------------------------------
if (commandArgs()[1]=='RStudio'){
  ##  method print: \f: Form Feed
  print.cleanup <- function(cleanupObject) cat("\f")     
  
  
}else if(substr(commandArgs()[1], nchar(commandArgs()[1]), nchar(commandArgs()[1])) == "R"){        
  
  ##  -------------------------------------------------------------
  ##  }else if(tail(strsplit(commandArgs()[1], split = "")[[1]], n=length(commandArgs()[1])))       
  ##  -------------------------------------------------------------
  ##  Reference
  ##  Rapp defaul configuration in macOS:
  ##  "/Library/Frameworks/R.framework/Resources/bin/exec/R"
  ##  --------------------------------------------------------------
  ##  http://ascii-table.com/ansi-escape-sequences.php                                                (check)
  ##  https://invisible-island.net/ncurses/man/clear.1.html                                           (check)
  ##  https://stackoverflow.com/questions/1348563/clearing-output-of-a-terminal-program-linux-c-c     (check)
  print.cleanup <- function(cleanupObject) cat(c("\033[2J","\033[H"))
  
}else{print(paste0("not support: ",commandArgs()[1]))}                                                                         


##  ----------------------------------------
##  About 'clc'
##  ----------------------------------------
##  Can replace 'clc' by:

##  'cls' command DOS
##  'clear' terminal macOS / Linux / Unix
##  or whatever you choose

##  Directly in this code or in console or terminal

##  For example change clc by clear
##  source("clc.R") or source("yourPath/clc.R")
##  clear <- 0
##  class(clear) <- 'cleanup'
##  ----------------------------------------                                                                     
clc <- 0                                        ##  variable from class numeric
class(clc) <- 'cleanup'                         ##  class cleanup
print(clc)                                      ##  when you load this source,
##  it cleans all console


################################################################################
#
################################################################################
create_df <- function(c_Rmd) {
  p <- "^```"
  df_data <- data.frame(
    index = 1:length(c_Rmd),
    c_Rmd,
    code_sections = lapply(c_Rmd, function(x)
      stringr::str_detect(x, p)) |> unlist(),
    is.heading = stringr::str_detect(c_Rmd, "^#")
  )
  
  # search and exclude code sections
  c_start_ii <- 0
  for (ii in 1:nrow(df_data)) {
    if (df_data$code_sections[ii] &  (c_start_ii != 0)) {
      df_data$code_sections[c_start_ii:ii] <-
        rep(TRUE, length(c_start_ii:ii))
      c_start_ii <- 0
    } else if (df_data$code_sections[ii]) {
      c_start_ii <- ii
    }
  }
  
  # remove heading in code section
  df_data$is.heading <- ifelse(df_data$code_sections, FALSE, df_data$is.heading)
  
  # Store headings
  df_data$`#` <- stringr::str_detect(df_data$c_Rmd, "^#\\s") |> ifelse(1, 0)
  df_data$`##` <-  stringr::str_detect(df_data$c_Rmd, "^##\\s") |> ifelse(1, 0)
  df_data$`###` <- stringr::str_detect(df_data$c_Rmd, "^###\\s") |> ifelse(1, 0)
  df_data$`####` <- stringr::str_detect(df_data$c_Rmd, "^####\\s") |> ifelse(1, 0)
  df_data$`#####` <- stringr::str_detect(df_data$c_Rmd, "^#####\\s") |> ifelse(1, 0)
  df_data$`######` <- stringr::str_detect(df_data$c_Rmd, "^######\\s") |> ifelse(1, 0)
  return(df_data)
}

################################################################################
#
################################################################################
r_toc_for_Rmd <- function(
    c_Rmd,
    toc_heading_string = "Table of Contents" ,
    create_nb = TRUE, create_top_link = TRUE , nb_front = TRUE, set_first_heading_level = FALSE,
    pagebreak_level = "non"
)
{
  ##########################################################################
  # create data frame to work with
  df_data <- create_df(c_Rmd)
  
  ##########################################################################
  # Headings
  m <- df_data[df_data$is.heading, 5:ncol(df_data)]
  
  ##########################################################################
  # Analyze heading structure
  heading_struct <- m|>
    apply(2, function(x) {
      sum(x)>0
    })
  
  # highest order heading column index
  for (ii in 1:ncol(m)) {
    if(heading_struct[ii]) {
      highest_order_jj <- ii
      break
    }
  }
  
  # highest order heading row index
  for (ii in 1:nrow(m)) {
    if(m[ii,highest_order_jj]) {
      highest_order_ii <- ii
      break
    }
  }
  
  # find first heading
  for (ii in 1:6) {
    if(m[1,ii]>0){
      first_heading_column <- ii
    }
  }
  
  ##########################################################################
  # correct heading structure
  c_names <- c("#","##","###","####","#####","######")
  
  if(highest_order_jj != first_heading_column){
    # correct structure
    temp <- m[1:(highest_order_ii-1),first_heading_column:6]
    temp
    temp <- switch (first_heading_column ,
                    temp = temp,
                    temp = cbind(temp,p1 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp)),p5 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp)),p5 = rep(0,nrow(temp)),p6 = rep(0,nrow(temp))),
    )
    temp
    
    temp1 <- m[highest_order_ii:nrow(m),highest_order_jj:6]
    temp1 <- switch (highest_order_jj,
                     temp1 = temp1,
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1)),p5 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1)),p5 = rep(0,nrow(temp1)),p6 = rep(0,nrow(temp1))),
    )
    names(temp1) <- c_names[highest_order_jj:6]
    names(temp) <- c_names[highest_order_jj:6]
    m_ <- rbind(temp,temp1)
  }else{
    if(highest_order_jj>0){ # remove not populated columns
      m_ <- switch (highest_order_jj,
                    m_ = m,
                    m_ = cbind(m[,2:6],p1 = rep(0,nrow(m))),
                    m_ = cbind(m[,3:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m))),
                    m_ = cbind(m[,4:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m))),
                    m_ = cbind(m[,5:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m)),p4 = rep(0,nrow(m))),
                    m_ = cbind(m[,6:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m)),p4 = rep(0,nrow(m)),p5 = rep(0,nrow(m)))
      )
    }else{
      m_ <- m
    }
  }
  m_
  
  ##########################################################################
  # create structure number system
  # Heading structure counts
  heading_cnt <- rep(0, 6)
  heading_cnt_ <- rep(0, 6)
  last_heading_edited <- 0
  
  # Structure string
  c_add <- c("* ",
             "    + ",
             "        + ",
             "            + ",
             "                + ",
             "                    + ")
  
  c_add_structure <- 1:nrow(m_)
  column_cnt <- 0
  m__ <- m_
  c_Heading_level <- 1:nrow(m_)
  for (ii in 1:nrow(m_)) {
    for (jj in 1:6)
      if (m_[ii, jj] > 0) {
        heading_cnt[jj] <- heading_cnt[jj] + 1
        if (last_heading_edited > jj) {
          # if heading order changes to higher order clear heading_cnt accordingly
          heading_cnt[(jj + 1):length(heading_cnt)] <- 0
          
        }
        last_heading_edited <- jj
        break
      }
    m__[ii, 1:6] <- heading_cnt
    heading_cnt_ <- heading_cnt
    if(set_first_heading_level){
      c_Heading_level[ii] <- c_names[jj]
    }else{
      c_Heading_level[ii] <- c_names[jj + (highest_order_jj-1)]
    }
    c_add_structure[ii] <- c_add[jj]
    
  }
  
  ##########################################################################
  # create structure number
  c_nb <- m__ |>
    apply(1, function(x) {
      temp <- x[x > 0]
      paste0(temp, collapse = ".")
    })
  
  ##########################################################################
  # create link link to table of contents
  c_top_link <-  paste0("\n[", toc_heading_string, "](#", toc_heading_string, ")\n")
  c_top_link
  
  ##########################################################################
  c_Heading <- c_Rmd[df_data$is.heading]|>stringr::str_remove_all("#")|>stringr::str_trim()
  c_Heading
  
  ##########################################################################
  # create anchor
  if (create_nb) {
    if (nb_front) { # number system in front of heading
      c_anchor <- paste0(
        c_Heading_level," " , c_nb, " ", c_Heading ,
        "<a name=\"",
        "A_", # add some characters to ensure html links will work
        c_nb, "_", c_Heading ,
        "\"></a>",
        if(create_top_link) c_top_link
      )
      c_toc <- paste0("[", c_nb,  " ", c_Heading,"](#A_", c_nb,"_", c_Heading, ")")
    } else {  # heading flowed by number system
      c_anchor <- paste0(
        c_Heading_level, " " , c_Heading, " ", c_nb,
        "<a name=\"",
        "A_", # add some characters to ensure html links will work
        c_Heading, " ", c_nb,
        "\"></a>",
        if(create_top_link) c_top_link
      )
      c_toc <- paste0("[", c_Heading, " ",c_nb,"](#A_", c_Heading," ",c_nb,")")
    }
  } else { # No numbering system / Do not Include number system
    c_anchor <- paste0(
      c_Heading_level, " ", c_Heading,
      "<a name=\"",
      "A_", # add some characters to ensure html links will work
      c_Heading,
      "\"></a>",
      if(create_top_link) c_top_link
    )
    c_toc <- paste0("[", c_Heading, "](#A_", c_Heading, ")")
  }
  
  # format toc according to found heading structure
  c_toc <- paste0(c_add_structure, c_toc)
  
  #########################################################################
  # Enhance headings
  df_data_ <- dplyr::left_join(df_data[, 1:4],
                               data.frame(index = rownames(m__) |> as.integer(),
                                          c_anchor),
                               by = "index")
  
  df_data_$c_Rmd_ <-  ifelse(!is.na(df_data_$c_anchor), df_data_$c_anchor, c_Rmd)
  
  
  
  #########################################################################
  # create TOC
  highest_order_jj <- ifelse(set_first_heading_level, 1, highest_order_jj)
  c_toc_link <- switch(highest_order_jj,
                       paste0(c_names[1]," ",toc_heading_string),
                       paste0(c_names[2]," ",toc_heading_string),
                       paste0(c_names[3]," ",toc_heading_string),
                       paste0(c_names[4]," ",toc_heading_string),
                       paste0(c_names[5]," ",toc_heading_string),
                       paste0(c_names[6]," ",toc_heading_string)
  )
  
  c_toc_link <- ifelse(create_top_link,
                       paste0(c_toc_link, "<a name=\"", toc_heading_string, "\"></a>"),
                       c_toc_link)
  
  #########################################################################
  # find position to insert table of contents
  check <- stringr::str_detect(c_Rmd, "---")
  c_start <- 1
  cnt <- 0
  
  for (ii in 1:length(c_Rmd)) {
    if (check[ii]) {
      c_start <- ii
      cnt <- cnt + 1
      if(cnt == 2) break
    }
  }
  
  #########################################################################
  # Insert table of contents
  c_Rmd <- c(df_data_$c_Rmd_ [1:(c_start)],
             c_toc_link,
             c_toc,
             "\n",
             df_data_$c_Rmd_[(c_start+1):nrow(df_data)]
  )
  
  #########################################################################
  # Insert page breaks
  
  #create data frame to work with
  df_data <- create_df(c_Rmd)
  
  # Headings
  m <- df_data[df_data$is.heading, 5:ncol(df_data)]
  
  # Analyze heading structure
  heading_struct <- m|>
    apply(2, function(x) {
      sum(x)>0
    })
  
  # highest order heading column index
  for (ii in 1:ncol(m)) {
    if(heading_struct[ii]) {
      highest_order_jj <- ii
      break
    }
  }
  
  if(highest_order_jj > 1 & pagebreak_level != "non") pagebreak_level <- (pagebreak_level|>as.integer() +  highest_order_jj - 1)|>as.character()
  
  m_pb <- switch (
    pagebreak_level,
    "non" = FALSE,
    "1" = m[, 1:1]|>matrix(dimnames =list(row.names(m),"#"))|>as.data.frame(),
    "2" = m[, 1:2],
    "3" = m[, 1:3],
    "4" = m[, 1:4],
    "5" = m[, 1:5],
    "6" = m[, 1:6],
  )
  
  # add html page break tag
  if(is.data.frame(m_pb)) {
    for (ii in 2:nrow(m_pb)) {
      for (jj in 1:ncol(m_pb)) {
        if (m_pb[ii, jj] > 0) {
          index <- row.names(m_pb)[ii] |> as.integer()
          c_Rmd[index] <-
            paste0("\n",
                   "\\newpage",
                   # "<div style=\"page-break-after: always\"></div>",
                   "\n",
                   c_Rmd[index])
        }
      }
    }
  }
  
  return(c_Rmd)
}
