library(tidyverse)
library(ggtext)
library(lubridate)
library(myggthemes)

devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)

data(coronavirus)
glimpse(coronavirus)


# select only cases from Germany
germany <- coronavirus %>% 
  filter(country == "Germany", type == "confirmed") %>% 
  filter(date >= as_date("2020-06-15"))

max(germany$date)

theme_set(theme_clean_lightbg())

# plot development of daily cases

loess_bw <- 0.25
colors <- colorspace::diverge_hcl(2)

p <- germany %>% 
  ggplot(aes(date, cases)) +
  geom_col(alpha = 0.2, width = 0.35, fill = "grey10") +
  geom_smooth(aes(col = "Gleitender Durchschnitt"), method = "loess", 
              span = loess_bw, se = FALSE,
              size = 1.2, show.legend = FALSE) +
  annotate("text", label = "Bestätigte Fälle", 
           x = as_date("2020-10-01"), y = 5000,
           hjust = 1, family = "Inconsolata",
           col = colors[1]) +
  scale_x_date(date_breaks = "1 month", labels = scales::date_format("%B"), limits = c(as_date(NA), as_date("2020-12-31")) )
  

#' Angela Merkels estimation
#' "Wenn das in den nächsten drei Monaten Oktober, November, Dezember weiter so wäre, 
#' dann würden wir von 2.400 auf 4.800, auf 9.600, auf 19.200 kommen."
#' Source: https://www.zeit.de/wissen/gesundheit/2020-09/angela-merkel-corona-infektionszahlen-weihnachten-prognose-berechnung-realistisch-einschaetzung
#' 
#' doubling time: 1 month
#' Sep: 2400
#' Oct: 4800
#' Nov: 9600
#' Dec: 19200


merkel_monthly <- tribble(
  ~date, ~cases,
  "2020-06-30", 300,
  "2020-07-31", 600,
  "2020-08-31", 1200,
  "2020-09-30", 2400,
  "2020-10-31", 4800,
  "2020-11-30", 9600,
  "2020-12-31", 19200
) %>% 
  mutate(date = as_date(date))

merkel_monthly <- merkel_monthly %>% 
  mutate(group = ifelse(date > as_date("2020-10-01"), "2", "1")) %>%
  bind_rows(tibble(
    date = as_date("2020-09-30"),
    cases = 2400,
    group = "2"
  ))

# average number of days per month for projection of cases
avg_num_days_per_month = mean(c(31, 31, 30, 31, 30, 31))

# fill the missing days and interpolate the daily cases
merkel_daily <- merkel_monthly %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  mutate(days_elapsed = difftime(date, min(date), units = "day") %>% as.numeric()) %>%
  mutate(cases_proj = 2 ^ (days_elapsed / avg_num_days_per_month) * min(cases, na.rm = TRUE)) %>%
  fill(group)

plot_subtitle <- "Am 29.09.2020 stellte Angela Merkel ein Rechenbeispiel
für eine mögliche Entwicklung<br>der Coronavirus-Fallzahlen auf.
Das wurde als Worst-Case-Szenario verstanden. Nun..."

plot_caption <- glue::glue("Bestätigte Fälle geglättet (loess, bandwidth: {loess_bw}).<br>
                           Quelle: @4nsgarW. Daten: Johns Hopkins University.")


# place arrow based on max date in the dataset
max_date <- max(germany$date)
arrow_position_x <- max_date + period(4, "days")
# define y position from projected cases from Merkel's estimation
arrow_position_y <- merkel_daily$cases_proj[merkel_daily$date == arrow_position_x] + 600

p <- p + 
  # draw Merkel's projection
  geom_line(data = merkel_daily, 
              aes(date, cases_proj, lty = group, col = "Merkel"),
              size = 1.2, show.legend = FALSE) +
  # direct label for Merkel
  annotate("text", label = "Prognose Merkel", 
           x = as_date("2020-11-15"), y = 5500,
           hjust = 0, family = "Inconsolata",
           col = colors[2]) +
  # draw an arrow to highlight the difference between projection and actuals
  # move this as 
  annotate("segment", x = arrow_position_x, xend = arrow_position_x,
           y = arrow_position_y, yend = 12000, col = "grey20",
           arrow = arrow(angle = 90, ends = "both", type = "open", length = unit(1.5, "mm"))) +
  colorspace::scale_color_discrete_diverging() +
  guides(lty = FALSE) +
  labs(title = sprintf("Die <span style='color:%s'>Realität</span> überholt
                       <span style='color:%s'>Merkels Prognose</span>", colors[1], colors[2]),
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = NULL, y = "Tägliche bestätigte Fälle") +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(size = 10))

p
ggsave(file.path("plots", "merkel_corona_prognose.png"), type = "cairo", dpi = 320, width = 6, height = 5)

