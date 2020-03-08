devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
library(tidyverse)
library(lubridate)
library(osmdata)
library(sf)
library(ggmap)
library(maps)
library(mapproj)

data("coronavirus")
glimpse(coronavirus)

world_coords <- c(left = -180, bottom = -65, right = 179.9999, top = 80)

world_map <- get_stamenmap(
  bbox = world_coords, 
  zoom = 3, maptype = "toner-lite")

corona_cases <- coronavirus %>% 
  filter(type == "confirmed") %>% 
  group_by(Country.Region) %>% 
  summarize(total_confirmed = sum(cases),
            lat = first(Lat), lon = first(Long))
  
# get borders (https://github.com/thomasp85/gganimate/issues/261)
borders <- borders("world", xlim = c(world_coords[1], world_coords[3]), ylim = c(world_coords[2], world_coords[4]))
borders_shape <- borders$data[, 1:5]

# check if any regions couldn't be matched
corona_cases %>% 
  anti_join(borders_shape, by = c("Country.Region" = "region")) %>%
  pull(Country.Region)

#[1] "Hong Kong"       "Macau"           "Mainland China"  "North Macedonia" "Others"          "US"   

# there should be more non-matching cases in borders_shape (i.e. countries w/o confirmed Corona cases)
borders_shape %>% 
  anti_join(corona_cases, by = c("region" = "Country.Region")) %>%
  distinct(region) %>% 
  pull(region)

# rename regions in borders_shape
borders_shape <- borders_shape %>% 
  mutate(region2 = case_when(
    region == "China" ~ "Mainland China",
    region == "USA" ~ "US",
    region == "Macedonia" ~ "North Macedonia",
    TRUE ~ region
  ))

# merge with Corona data
corona_cases_shapes <- corona_cases %>% 
  left_join(borders_shape, by = c("Country.Region" = "region2")) %>% 
  # order by region and order variable
  arrange(group, order) %>% 
  filter(!is.na(group))


x11()
ggmap(world_map) +
  geom_point(data = corona_cases, aes(x = lon, y = lat, size = sqrt(total_confirmed))) +    
  theme_void()

x11()
ggmap(world_map) +
  geom_polygon(data = corona_cases_shapes, aes(x = long, y = lat.y, fill = log(total_confirmed), alpha = log(total_confirmed), group = group)) +    
  coord_map(projection = "mercator",
            xlim = c(world_coords[1], world_coords[3]), ylim = c(world_coords[2], world_coords[4])) +
  geom_text(data = subset(corona_cases, corona_cases$total_confirmed), aes(x = lon, y = lat, label = total_confirmed), size = 5, family = "Georgia") +
  scale_fill_gradient(low = "yellow", high = "red") +
  guides(fill = FALSE, alpha = FALSE) +
  labs(title = "Confirmed cases of COVID-19 as of 2020-03-01") +
  theme_minimal()


min_date <- min(coronavirus$date)
max_date <- max(coronavirus$date)
corona_cases_day <- coronavirus %>% 
  filter(type == "confirmed") %>% 
  group_by(Country.Region, date) %>% 
  summarize(cases = sum(cases)) %>% 
  arrange(date, .by_group = TRUE) %>% 
  mutate(cumul_confirmed = cumsum(cases)) %>%
  # add missing days by country (missing day = no new cases reported)- start with min(date) per country
  complete(date = seq.Date(min(date), max_date, by = "day")) %>% 
  # fill empty values for newly created days with the succeeding number of cases
  fill(cumul_confirmed) %>% 
  # replace NA with 0
  mutate(cumul_confirmed = replace_na(cumul_confirmed, 0)) %>% 
  ungroup() 

# merge with Corona data
corona_cases_day_shapes <- corona_cases_day %>% 
  left_join(borders_shape, by = c("Country.Region" = "region2")) %>% 
  # order by region and order variable
  arrange(group, date, order) %>% 
  filter(!is.na(group)) %>% 
  # transform date to factor
  mutate(date2 = factor(date))



x11()
ggmap(world_map) +
  geom_polygon(data = corona_cases_day_shapes, aes(x = long, y = lat, fill = log(cumul_confirmed), alpha = log(cumul_confirmed), group = group)) +    
  coord_map(projection = "mercator",
            xlim = c(world_coords[1], world_coords[3]), ylim = c(world_coords[2], world_coords[4])) +
  #geom_text(data = subset(corona_cases, corona_cases$cumul_confirmed 50), aes(x = lon, y = lat, label = cumul_confirmed), size = 5, family = "Georgia") +
  scale_fill_gradient(low = "yellow", high = "red") +
  guides(fill = FALSE, alpha = FALSE) +
  labs(title = "Confirmed cases of COVID-19 as of ...") +
  theme_minimal() +
  facet_wrap(vars(date))


## TMAP

library(tmap)
data("World")

World_corona <- World %>% 
  mutate(name = as.character(name),
         name2 = case_when(
           name == "China" ~ "Mainland China",
           name == "United States" ~ "US",
           name == "Macedonia" ~ "North Macedonia",
           name == "United Kingdom" ~ "UK",
           TRUE ~ name)) %>% 
  left_join(corona_cases, by = c("name2" = "Country.Region")) %>% 
  sf::st_sf()


tmap_mode("view")
tm_shape(World_corona, name = "geometry") +
  tm_view(text.size.variable = TRUE) +
  tm_polygons("total_confirmed", textNA = "No cases", breaks = c(1, 10, 100, 1000, 80000, Inf)) +
  tm_text("total_confirmed", size = "AREA")


