library(shinydashboard)
library(shiny)
library(r2d3)
library(tidyverse)
library(DT)
#devtools::install_github("RamiKrispin/coronavirus")
#library(coronavirus)
library(tidyverse)
library(lubridate)
# library(sf)
# library(maps)
# library(mapproj)
library(leaflet)
library(plotly)
library(shinyWidgets)
library(formattable)

country_names <- read_tsv("input/country_name_mapping.tsv")
country_population <- read_tsv("input/country_population.tsv")


body <- dashboardBody(
  tags$style(type="text/css", ".recalculating { opacity: 1.0; }
             * {font-family: Nunito Sans}
             "),
  fluidRow(
    tabBox(title = NULL,
           width = 12,
           tabPanel(title = "World",
                    fluidRow(
                      column(width = 3,
                             sliderInput("date", label = "Select Date",
                                         min = min(coronavirus$date), max = max(coronavirus$date), 
                                         value = max(coronavirus$date),
                                         animate = animationOptions(interval = 100, loop = FALSE)
                             ),
                      ),
                      column(width = 3,
                             selectInput("type", "Size of Points",
                                         choices = c("Confirmed Cases" = "confirmed", "Deaths" = "death"), 
                                         selected = "Confirmed Cases"),
                             shinyWidgets::materialSwitch("relative_to_pop", "Per 100,000 Inhabitants", 
                                                          value = FALSE, status = "primary")
                      )
                    ), 
                    fluidRow(
                      tags$head(tags$style(type = "text/css", "#world_map {height:600px !important;}
                                                  #raw_data {height:600px !important;}
                                                  ")),
                      tabBox(title = NULL,
                             width = 12,
                             tabPanel(title = "Map",
                                      # show the world map
                                      leafletOutput("world_map")
                                      
                             ),
                             
                             tabPanel(title = "Raw Data",
                                      DTOutput("raw_data")
                                      
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             valueBoxOutput(width = 3, "valueBox_confirmed_cases"),
                             valueBoxOutput(width = 3, "valueBox_deaths"),
                             valueBoxOutput(width = 3, "valueBox_increase_outside_china"),
                             valueBoxOutput(width = 3, "valueBox_increase_highest_country")
                      )
                    )
           ),
           
           tabPanel(title = "Countries",
                    tags$head(tags$style(type = "text/css", "#country_compare {height:700px !important;}")),
                    fluidRow(
                      column(width = 4,
                             sliderInput("date_2", label = "Select Date",
                                         min = min(coronavirus$date), max = max(coronavirus$date), 
                                         value = max(coronavirus$date),
                                         animate = animationOptions(interval = 500, loop = FALSE)
                             ),
                      ),
                      column(width = 4,
                             fluidRow(
                               selectInput("type_2", "Choose Metric",
                                           choices = c("Confirmed Cases" = "confirmed", "Deaths" = "death"), 
                                           selected = "Confirmed Cases"),
                               selectInput("scale_2", "Scale",
                                           choices = c("Linear", "Logarithmic"),
                                           selected = "Linear")
                             )
                      ),
                      column(width = 4,
                             fluidRow(
                               selectInput("country_2", "Select Country",
                                           choices = "",
                                           selected = "Germany"),
                               selectInput("country_2_compare", "Compare to",
                                           choices = "",
                                           selected = "Italy")
                             )
                      )
                    ),        
                    
                    plotOutput("country_compare")
           ),
           
           tabPanel(title = "Mortality",
                    sliderInput("date_3", label = "Select Date",
                                min = min(coronavirus$date), max = max(coronavirus$date), 
                                value = max(coronavirus$date),
                                animate = animationOptions(interval = 200, loop = FALSE)
                    ),
                    plotlyOutput("mortality_graph")
           )
           
    )
  ),
  fluidRow(title = "Footer",
    column(width = 12,
           "Data Source: Johns Hopkins University (github.com/CSSEGISandData/COVID-19).",
           "Using coronavirus package by Rami Krispin to pull data (https://ramikrispin.github.io/coronavirus/)")
  )
)


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Coronavirus Dashboard"),
  dashboardSidebar(disable = TRUE),
  body = body
)



server <- function(input, output, session) {
  
  # get data
  # check time elapsed since last update
  # coronavirus <- reactive({
  #   filename <- "input/coronavirus.RData"
  #   update_threshold <- 60 * 60 * 6 # check for update after 6 hours (in seconds)
  #   update_threshold <- 10
  #   file_mtime <- file.info(filename)$mtime
  #   current_time <- Sys.time()
  #   # if threshold time has elapsed since last file update refresh data
  #   if (is.na(file_mtime) | as.numeric(difftime(current_time, file_mtime)) > update_threshold) {
  #     print("Update data source")
  #     source("pulling raw data.R", verbose = FALSE)
  #   }
  #   readr::read_rds(filename)
  # })

  update_threshold <- 60 * 60 * 1000 # check for update after 1 hour (in milliseconds)
  filename <- "input/coronavirus.RData"
  coronavirus <- reactivePoll(update_threshold, session,
                       checkFunc = function() {
                         if (file.exists(filename))
                           file.info(filename)$mtime[1]
                         else
                           ""
                        },
                       valueFunc = function() {
                         source("pulling raw data.R", verbose = FALSE)
                         readr::read_rds(filename)
                       }
                      )

  
  
  # calculate doubling time
  # @param r growth rate in interval t (fractions of 1)
  # @param t time in days (default: 1 day)
  calculate_doubling_time <- function(r, t = 1) {
    Td = t * (log(2) / log(1 + r))
    Td
  }
  
  max_date <- reactive({max(coronavirus()$date)})
  corona_cases_country_day <- reactive({
    coronavirus() %>% 
      inner_join(country_names, by = c("Country.Region" = "region_coronavirus")) %>% 
      group_by(region = region_world, continent, type, date) %>% 
      summarize(cases = sum(cases), 
                long = mean(Long),
                lat = mean(Lat)) %>%
      arrange(date, .by_group = TRUE) %>% 
      mutate(cumul_cases = cumsum(cases)) %>%
      # add missing days by country (missing day = no new cases reported)- start with min(date) per country
      complete(date = seq.Date(min(date), max_date(), by = "day")) %>% 
      # fill empty values for newly created days with the succeeding number of cases
      fill(cumul_cases) %>% 
      # replace NA with 0
      mutate(cumul_cases = replace_na(cumul_cases, 0)) %>% 
      ungroup() %>% 
      # change coordinates for a few countries
      mutate(
        lat = case_when(
          region == "France" ~ 46.2276,
          region == "Netherlands" ~ 52.1326,
          region == "Denmark" ~ 56.2639,
          region == "UK" ~ 	55.3781,
          TRUE ~ lat
        ),
        long = case_when(
          region == "France" ~ 2.2137,
          region == "Netherlands" ~	5.2913,
          region == "Denmark" ~ 9.5018,
          region == "UK" ~ 	-3.4360,
          TRUE ~ long
        )
      ) %>% 
      left_join(country_population, by = "region") %>% 
      mutate(cases_100k = 100 * cases /  pop,
             cumul_cases_100k = 100 * cumul_cases / pop
      ) %>% 
      # calculate increase from previous day and doubling time based on 3d-interval
      group_by(region, continent, type) %>% 
      mutate(
        increase_prop = cases / (cumul_cases - cases),
        increase_rate_3days = cumul_cases / lag(cumul_cases, 3) - 1,
        doubling_time = calculate_doubling_time(increase_rate_3days, t = 3),
        doubling_time = ifelse(doubling_time %in% c(0, NaN, Inf), NA, doubling_time)
      ) %>% 
      ungroup()
  })
  
  
  # value boxes world
  corona_world_summary <- reactive( {
    coronavirus() %>% 
      filter(date <= as_date(input$date)) %>% 
      group_by(type) %>% 
      summarize(cases = sum(cases))
  })
  
  output$valueBox_confirmed_cases <- renderValueBox({
    value <- corona_world_summary() %>% filter(type == "confirmed") %>% pull(cases)
    valueBox(scales::number(value), "Confirmed Cases", icon = icon("file-medical"))
  })
  output$valueBox_deaths <- renderValueBox({
    value <- corona_world_summary() %>% filter(type == "death") %>% pull(cases)
    valueBox(scales::number(value), "Deaths", icon = icon("cross"), color = "orange")
  })
  output$valueBox_increase_worldwide <- renderValueBox({
    increase_worldwide <- corona_cases_country_day() %>% 
      filter(type == "confirmed") %>% 
      filter(date >= max(date) - 1) %>% 
      group_by(date) %>% 
      summarize(cumul_cases = sum(cumul_cases)) %>% 
      mutate(prev_day = lag(cumul_cases),
             diff = cumul_cases - prev_day,
             increase = diff / prev_day) %>% 
      filter(date == max(date)) %>% 
      pull(increase)
    valueBox(scales::percent(increase_worldwide, accuracy = 1.1), "Increase Cases Worldwide", icon = icon("chart-line"))
  })
  output$valueBox_increase_outside_china <- renderValueBox({
    increase_worldwide <- corona_cases_country_day() %>% 
      filter(type == "confirmed", region != "China") %>% 
      filter(date >= max(date) - 1) %>% 
      group_by(date) %>% 
      summarize(cumul_cases = sum(cumul_cases)) %>% 
      mutate(prev_day = lag(cumul_cases),
             diff = cumul_cases - prev_day,
             increase = diff / prev_day) %>% 
      filter(date == max(date)) %>% 
      pull(increase)
    valueBox(scales::percent(increase_worldwide, accuracy = 1.1), "Increase Cases Outside China", icon = icon("chart-line"))
  })
  output$valueBox_increase_highest_country <- renderValueBox({
    cases_threshold <- 20
    increase_country_max <- corona_cases_country_day() %>% 
      filter(type == "confirmed") %>% 
      filter(date >= max(date) - 1) %>%
      # include only cases with a certain amount of cases on prev day
      filter(cumul_cases >= cases_threshold) %>% 
      group_by(region, date) %>% 
      summarize(cumul_cases = sum(cumul_cases)) %>% 
      # keep only if 2 previous and current day have at least {cases_threshold} cases
      filter(n() > 1) %>% 
      mutate(prev_day = lag(cumul_cases),
             diff = cumul_cases - prev_day,
             increase = diff / prev_day) %>% 
      filter(date == max(date)) %>% 
      ungroup() %>% 
      arrange(-increase) %>% 
      slice(1)
    
    country_highest_increase_confirmed <- pull(increase_country_max, region)
    
    valueBox(scales::percent(pull(increase_country_max, increase), accuracy = 1.0), sprintf("%s (Highest Increase)", country_highest_increase_confirmed), icon = icon("chart-line"))
  })
  
  # filter the dataset for world map
  corona_cases_country_day_data <- reactive({
    df <- corona_cases_country_day() %>% 
      filter(date == input$date, type == input$type) %>% 
      mutate(label = sprintf("<b>%s</b>\n%d", region, cumul_cases) %>% map(htmltools::HTML))
    
    if (input$relative_to_pop) {
      df <- df %>%
        mutate(label = sprintf("<b>%s</b>\n%9.2f", region, cumul_cases_100k) %>% map(htmltools::HTML))
    }
    
    df
  })
  
  corona_cases_country_day_historical_data <- reactive({
    df <- corona_cases_country_day() %>% 
      filter(date <= input$date_2, type == input$type_2, region == input$country_2) %>% 
      mutate(label = sprintf("<b>%s</b>\n%d", region, cases) %>% map(htmltools::HTML))
  })
  
  corona_cases_country_day_historical_data_compare <- reactive({
    df <- corona_cases_country_day() %>% 
      filter(date <= input$date_2, type == input$type_2, region %in% c(input$country_2, input$country_2_compare)) %>% 
      mutate(label = sprintf("<b>%s</b>\n%d", region, cases) %>% map(htmltools::HTML),
             # create a factor variable to order countries consistent with input
             region = factor(region, levels = c(input$country_2, input$country_2_compare))
      )
  })
  
  
  # Table view
  output$raw_data <- renderDT({
    df <- corona_cases_country_day_data() %>% 
      select(region, cumul_cases, cumul_cases_100k, cases, doubling_time) %>% 
      arrange(-cumul_cases) %>% 
      mutate(increase_prop = cases / (cumul_cases - cases),
             # for formatting color bars, limit max value to 1
             increase_prop_2 = ifelse(increase_prop > 1, 1, increase_prop),
             # ... and min value to 0
             increase_prop_2 = ifelse(increase_prop < 0, 0, increase_prop),
             # cover negative increases
             doubling_time = ifelse(doubling_time < 0, NA, doubling_time)
      )
    
    if (input$relative_to_pop) {
      df <- df %>% 
        select(`Country` = region, `Total Cases`= cumul_cases_100k, `Increase (n)` = cases, 
               `Increase (%)` = increase_prop,
               `Doubling Time (Days)` = doubling_time, increase_prop_2)
    } else {
      df <- df %>% 
      select(`Country` = region, `Total Cases`= cumul_cases, `Increase (n)` = cases, 
             `Increase (%)` = increase_prop,
             `Doubling Time (Days)` = doubling_time, increase_prop_2)
    }
    df %>% 
      select(-increase_prop_2) %>% 
      datatable() %>% 
      formatPercentage("Increase (%)") %>% 
      formatRound(c("Total Cases", "Increase (n)"), digits = 0) %>% 
      formatRound(c("Doubling Time (Days)"), digits = 1) %>% 
      formatStyle(
        "Increase (%)",
        background = styleColorBar(df$increase_prop_2, 'lightblue'),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>% 
      formatStyle(
        "Doubling Time (Days)",
        color = styleInterval(c(7, 14), c("red", "black", "green"))
      ) %>% 
      formatStyle(
        "Total Cases",
        background = styleColorBar(df$`Total Cases`, 'lightblue'),
        backgroundSize = "100% 10%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center bottom"
      )
  })
  
  output$world_map <- renderLeaflet({
    leaflet(
      options = leafletOptions(worldCopyJump = FALSE)
    ) %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(lng = 2, lat = 30, zoom = 2)
  })
  
  observe({
    
    # color points based on type of data
    if (input$type == "confirmed") {
      fill_color <- "#03F"
      point_size_factor <- 10
    } else if (input$type == "death") {
      fill_color <- "orangered"
      point_size_factor <- 60
    } else if (input$type == "recovered") {
      fill_color <- "darkgreen"
      point_size_factor <- 10
    }
    color <- fill_color
    
    if (input$relative_to_pop) {
      leafletProxy("world_map", session = session, data = corona_cases_country_day_data()) %>% 
        clearShapes() %>%
        addCircles(lng = ~long, lat = ~lat,
                   fillColor = fill_color, color = color,
                   label = ~label, weight = 1, radius = ~cumul_cases_100k * 200 * point_size_factor)
    } else {
      leafletProxy("world_map", session = session, data = corona_cases_country_day_data()) %>% 
        clearShapes() %>%
        addCircles(lng = ~long, lat = ~lat,
                   fillColor = fill_color, color = color,
                   label = ~label, weight = 1, radius = ~cumul_cases * point_size_factor)
      
    }
    
  })
  region_choices <- reactive({corona_cases_country_day() %>% distinct(region) %>% pull(region)})
  region_choices <- c("Germany", "Italy")
  updateSelectInput(session = session, "country_2", 
                    choices = region_choices,
                    selected = "Germany")
  
  updateSelectInput(session = session, "country_2_compare", 
                    choices = region_choices,
                    selected = "Italy")
  
  reactive_country_plot <- reactive({
    # color points based on type of data
    if (input$type_2 == "confirmed") {
      fill_color <- "#0033FF"
      point_size_factor <- 10
    } else if (input$type_2 == "death") {
      fill_color <- "orangered"
      point_size_factor <- 60
    } else if (input$type_2 == "recovered") {
      fill_color <- "darkgreen"
      point_size_factor <- 10
    }
    color <- fill_color
    
    corona_cases_country_day_historical_data() %>% 
      ggplot(aes(date)) +
      geom_col(aes(y = cases, 
                   #text = sprintf("<b>New Cases</b>\n%s\n%d", date, cases)
      ),
      alpha = 0.7) +
      geom_line(aes(y = cumul_cases, 
                    #text = sprintf("<b>Cumulative Cases</b>\n%s\n%d", date, cumul_cases)
      ), 
      col = fill_color, size = 1.3) +
      labs(x = NULL, y = NULL) +
      theme_minimal()
  })
  
  reactive_country_compare_plot <- reactive({
    # color points based on type of data
    if (input$type_2 == "confirmed") {
      fill_color <- "#0033FF"
      point_size_factor <- 10
    } else if (input$type_2 == "death") {
      fill_color <- "orangered"
      point_size_factor <- 60
    } else if (input$type_2 == "recovered") {
      fill_color <- "darkgreen"
      point_size_factor <- 10
    }
    color <- fill_color
    
    p <- corona_cases_country_day_historical_data_compare() %>% 
      ggplot(aes(date)) +
      geom_col(aes(y = cases,
                   #text = sprintf("<b>New Cases</b>\n%s\n%d", date, cases)
      ),
      alpha = 0.5) +
      geom_line(aes(y = cumul_cases, lty = region,
                    #text = sprintf("<b>Cumulative Cases</b>\n%s\n%d", date, cumul_cases)
      ), 
      col = fill_color, size = 1.3) +
      labs(x = NULL, y = NULL) +
      guides(lty = FALSE) +
      facet_wrap(vars(region), ncol = 1) +
      theme_minimal() +
      theme(strip.text = element_text(size = 20))
    
    if (input$scale_2 == "Logarithmic") {
      p <- p + scale_y_log10(labels = scales::number)
    }
    p
  })
  
  
  observe({
    output$country_view <- renderPlot({
      reactive_country_plot()
    })
    
    output$country_compare <- renderPlot({
      reactive_country_compare_plot()
    })
  })
  
  # plot of
  
  data_for_mortality_graph <- reactive({
    corona_cases_country_day %>%
      filter(type %in% c("confirmed", "death")) %>%
      select(region, continent, date, type, cumul_cases) %>%
      spread(key = type, value = cumul_cases) %>%
      mutate(death = replace_na(death, 0),
             mortality = death / confirmed) %>%
      # at least n cases
      mutate(
        # mute small values
        confirmed = ifelse(confirmed < 10, 0, confirmed),
        mortality = ifelse(is.nan(mortality), 0, mortality),
        mortality = ifelse(confirmed < 10, 0, mortality)
        ) %>%
      arrange(region, date)
  })
    
  output$mortality_graph <- renderPlotly({
    p <- data_for_mortality_graph() %>% 
      filter(date == input$date_3) %>% 
      ggplot(aes(confirmed, mortality)) +
      geom_point(aes(size = death, col = continent,
                     text = sprintf("<b>%s</b>\nMortality Rate: %s\nConfirmed Cases: %d\nDeaths: %d", region, scales::percent(mortality, accuracy = 0.1), confirmed, death))) +
      scale_x_log10(labels = scales::number, limits = c(1, 10^5)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.15)) +
      labs(x = "Confirmed Cases", y = "Deaths / Confirmed Cases", col = "Continent") +
      theme_minimal()
    ggplotly(p, tooltip = c("text"))
  })
  
}

shinyApp(ui, server)