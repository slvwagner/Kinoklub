c_xml_file <- "input/WordPress/Wordpress_Beitraege.xml"

WordPress <- read_xml(c_xml_file)

xml_list <- as_list(WordPress)

names(xml_list$rss$channel) == "item"

items <- xml_list$rss$channel[names(xml_list$rss$channel) == "item"]

items[[1]]$title[[1]]

c_raw <- xml_list$rss$channel$item[[1]][[1]]
c_raw

length(xml_list$rss$channel$item)
ii <- 1

c_raw <- 1:length(items)
c_raw

for (ii in c_raw) {
  c_raw[ii] <- items[[ii]]$title[[1]]
  c_raw
}

c_raw
