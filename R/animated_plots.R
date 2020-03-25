library(tidyverse)
library(gganimate)
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
library(tmap)

data("coronavirus")
#coronavirus <- read_rds("../input/coronavirus.RData") 
coronavirus <- coronavirus %>% 
  # merge China
  mutate(Country.Region = ifelse(
    Country.Region %in% c("Mainland China", "Macau", "Hong Kong", "Macau SAR", "Hong Kong SAR"),
    "China", Country.Region))


max_date <- max(coronavirus$date)
corona_cases_country_day <- coronavirus %>% 
  group_by(Country.Region, type, date) %>% 
  summarize(cases = sum(cases)) %>% 
  arrange(date, .by_group = TRUE) %>% 
  mutate(cumul_cases = cumsum(cases)) %>%
  # add missing days by country (missing day = no new cases reported)- start with min(date) per country
  complete(date = seq.Date(min(date), max_date, by = "day")) %>% 
  # fill empty values for newly created days with the succeeding number of cases
  fill(cumul_cases) %>% 
  # replace NA with 0
  mutate(cumul_cases = replace_na(cumul_cases, 0)) %>% 
  ungroup() 

data("World")

continents <- World %>% 
  mutate(name = as.character(name),
         name2 = case_when(name == "United States" ~ "US",
           name == "Czech Rep." ~ "Czech Republic",
           name == "Dominican Rep." ~ "Dominican Republic",
           name == "Macedonia" ~ "North Macedonia",
           name == "Korea" ~ "South Korea",
           name == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
           name == "United Kingdom" ~ "UK",
           TRUE ~ name)) %>% 
  select(name, name2, continent)

p <- corona_cases_country_day %>% 
  filter(type == "confirmed") %>% 
  group_by(Country.Region, date) %>% 
  summarize(cumul_cases = sum(cumul_cases)) %>% 
  ungroup() %>% 
  left_join(continents, by = c("Country.Region" = "name2")) %>% 
  mutate(continent = as.character(continent),
         continent = case_when(
           Country.Region == "Bahrain" ~ "Asia",
           Country.Region == "San Marino" ~ "Europe",
           Country.Region == "Singapore" ~ "Asia",
           TRUE ~ continent
         )) %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(forcats::fct_rev(Country.Region), cumul_cases, fill = continent)) +
  geom_col() +
  coord_flip() +
  scale_y_log10(labels = scales::number_format(accuracy = 1)) +
  labs(title = "{frame_time}", 
       x = NULL, y = "Confirmed Cases (log10)", fill = "Continent") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, color = "grey70"),
        legend.position = "bottom")

p <- corona_cases_country_day %>% 
  filter(type == "confirmed") %>% 
  group_by(Country.Region, date) %>% 
  summarize(cumul_cases = sum(cumul_cases)) %>% 
  ungroup() %>% 
  left_join(continents, by = c("Country.Region" = "name2")) %>% 
  mutate(continent = as.character(continent),
         continent = case_when(
           Country.Region == "Bahrain" ~ "Asia",
           Country.Region == "San Marino" ~ "Europe",
           Country.Region == "Singapore" ~ "Asia",
           TRUE ~ continent
         )) %>% 
  filter(!is.na(continent)) %>% 
  # show only countries with at least 10 confirmed cases
  filter(cumul_cases >= 10) %>% 
  ggplot(aes(Country.Region, cumul_cases, fill = continent)) +
  geom_point(aes(col = continent), size = 4) +
  geom_segment(aes(x = Country.Region, xend = Country.Region, y = 1, yend = cumul_cases, col = continent))+
  geom_text(aes(label = scales::number(cumul_cases, accuracy = 1)), size = 6, col = "grey40", vjust = -0.7) +
  scale_y_log10(labels = scales::number_format(accuracy = 1)) +
  labs(title = "{frame_time}", 
       x = NULL, y = "Confirmed Cases (log10)", fill = "Continent", col = "Continent") +
  theme_minimal() +
  theme(plot.title = element_text(size = 32, color = "grey70"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_blank())

p_anim <- p + transition_time(date) + ease_aes("cubic-in-out") 
animate(p_anim, nframes = 200, width = 1400, height = 900, end_pause = 20)
anim_save("plots/corona_cases_over_time.gif")
