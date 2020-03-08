library(tidyverse)
library(rvest)

# which url to scrape
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
page <- read_html(url)

# get the first table on the page
table <- page %>% 
  html_node(css = "table:nth_of_type(1)") %>% 
  html_table() %>% 
  rename(cases = 2) %>% 
  # exclude totals
  filter(Bundesland != "Gesamt")

# save and keep historical data
if(!exists("rki_germany")) {
  rki_germany <- list()
} else {
  rki_germany <- read_rds()
}

rki_germany <- append(rki_germany, list(list(retrieve_date = Sys.Date(), table = table)))

write_rds(rki_germany, "output/rki_germany.RData")


#https://web.archive.org/web/*/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html