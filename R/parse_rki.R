library(tidyverse)
library(rvest)

# which url to scrape
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
page <- read_html(url)

# get the first table on the page
table <- page %>% 
  html_node(css = "table:nth_of_type(1)") %>% 
  html_table() %>% 
  # exclude totals
  filter(Bundesland != "Gesamt")

# table format has changed as of 2020-03-12
rki_germany <- table %>% 
  select(1, 2, 4) %>% 
  rename(cases = 2, risk_areas = 3) %>% 
  separate(cases, sep = " ", into = c("confirmed_cases", "deaths")) %>% 
  mutate(confirmed_cases = as.numeric(confirmed_cases),
         deaths = str_replace_all(deaths, "[\\(\\)]", "") %>% as.numeric())

write_rds(rki_germany, "output/rki_germany.RData")


#https://web.archive.org/web/*/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html