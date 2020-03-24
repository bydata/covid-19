library(tidyverse)
library(rvest)

# which url to scrape
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
page <- read_html(url)

# Update to table structure (2020-03-18)

table_raw <- page %>%
  html_node(css = "table:nth_of_type(1)")

# remove colgroup and table head from table doc
xml_remove(xml_child(table_raw, "colgroup"))
xml_remove(xml_child(xml_child(table_raw, "thead"), 1))

table <- table_raw %>% 
  html_table(dec = ",", fill = TRUE) 

colnames(table) <- c("Bundesland", "confirmed_cases", "confirmed_cases_diff", "cases_per_100k", "deaths", "risk_areas")
  
# table format has changed as of 2020-03-12
rki_germany <- table %>% 
  filter(Bundesland != "Gesamt") %>% 
  mutate(confirmed_cases = str_replace(confirmed_cases, "\\.", "") %>% as.numeric(),
         confirmed_cases_diff = str_replace_all(confirmed_cases_diff, "[+.]", "") %>% as.numeric())

write_rds(rki_germany, "output/rki_germany.RData")


#https://web.archive.org/web/*/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html