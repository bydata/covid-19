library(shinydashboard)
library(shiny)
library(r2d3)
library(tidyverse)
library(DT)
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
library(tidyverse)
library(lubridate)
# library(sf)
# library(maps)
# library(mapproj)
library(leaflet)
library(plotly)
library(shinyWidgets)

body <- dashboardBody(
  tags$style(type="text/css", ".recalculating { opacity: 1.0; }"),
  fluidRow(
    tabBox(title = NULL,
           width = 12,
           tabPanel(title = "World Map",
                    fluidRow(
                      column(width = 4,
                             sliderInput("date", label = "Select Date",
                                         min = min(coronavirus$date), max = max(coronavirus$date), 
                                         value = max(coronavirus$date),
                                         animate = animationOptions(interval = 100, loop = FALSE)
                                         ),
                      ),
                      column(width = 4,
                             selectInput("type", "Size of Points",
                                         choices = c("Confirmed Cases" = "confirmed", "Deaths" = "death", "Recovered" = "recovered"), 
                                         selected = "Confirmed Cases")
                      ),
                      column(width = 2,
                             shinyWidgets::materialSwitch("relative", "Relative to population", 
                                                          value = FALSE, status = "primary")
                      )
                    ),  
                    
              # show the world map
              leafletOutput("world_map")
           ),
           
           tabPanel(title = "Mortality",
                    plotlyOutput("mortality_graph")
        )
    )
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
  data("coronavirus")
  country_names <- read_tsv("../input/country_name_mapping.tsv")
  
  max_date <- max(coronavirus$date)
  corona_cases_country_day <- coronavirus %>% 
    inner_join(country_names, by = c("Country.Region" = "region_coronavirus")) %>% 
    group_by(region = region_world, type, date) %>% 
    summarize(cases = sum(cases), 
              long = mean(Long),
              lat = mean(Lat)) %>% 
    arrange(date, .by_group = TRUE) %>% 
    mutate(cumul_cases = cumsum(cases)) %>%
    # add missing days by country (missing day = no new cases reported)- start with min(date) per country
    complete(date = seq.Date(min(date), max_date, by = "day")) %>% 
    # fill empty values for newly created days with the succeeding number of cases
    fill(cumul_cases) %>% 
    # replace NA with 0
    mutate(cumul_cases = replace_na(cumul_cases, 0)) %>% 
    ungroup()
  
  
  # filter the dataset for world map
  corona_cases_country_day_data <- reactive({
    df <- corona_cases_country_day %>% 
      filter(date == input$date, type == input$type) %>% 
      mutate(label = sprintf("<b>%s</b>\n%d", region, cumul_cases) %>% map(htmltools::HTML))
  })
  
  output$world_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
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
      fill_color <- "green"
      point_size_factor <- 10
    }
    color <- fill_color
    
    leafletProxy("world_map", session = session, data = corona_cases_country_day_data()) %>% 
      clearShapes() %>%
      addCircles(lng = ~long, lat = ~lat,
                 fillColor = fill_color, color = color,
                  label = ~label, weight = 1, radius = ~cumul_cases * point_size_factor)
  })
  
  
  # plot of 
  output$mortality_graph <- renderPlotly({
    # p <- corona_cases_country_day %>%
    #   filter(type %in% c("confirmed", "death")) %>%
    #   #filter(date == max(date)) %>%
    #   select(-cases) %>%
    #   spread(key = type, value = cumul_cases) %>%
    #   mutate(death = replace_na(death, 0),
    #          mortality = death / confirmed) %>%
    #   # at least n cases
    #   filter(confirmed >= 100) %>%
    #   ggplot(aes(confirmed, mortality, group = region)) +
    #   geom_point(alpha = 0.5,
    #              aes(
    #                #col = continent, 
    #                size = death,
    #                text = sprintf("<b>%s</b>\nConfirmed Cases: %d\nDeaths: %s",
    #                                   region, confirmed, death))) +
    #   scale_x_log10(label = scales::number_format(accuracy = 1)) +
    #   scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    #   labs(caption = "Only countries with at least 100 confirmed cases displayed.",
    #        x = "Confirmed Cases (log10)", y = "Mortality", size = "Deaths"
    #        #col = "Continent"
    #        ) +
    #   theme_minimal()
    # 
    # ggplotly(p, tooltip = c("text"), frame = ~date) %>%
    #   # layout(title = list(text = str_c("US State Population and Life Expectancy",
    #   #                                   "<br>",
    #   #                                   "<sup>",
    #   #                                   "Instead of actual mortality rates this plot at least partly indicates\nhow extensively testing is available in each country.",
    #   #                                   "</sup>")))
    #   # ggplotly ignores text-alignment from ggplot object
    #   style(textposition = "right")
    # 
    df <- corona_cases_country_day %>%
      filter(type %in% c("confirmed", "death")) %>%
      #filter(date == max(date)) %>%
      select(-cases) %>%
      spread(key = type, value = cumul_cases) %>%
      mutate(death = replace_na(death, 0),
             mortality = death / confirmed) %>%
      # at least n cases
      mutate(confirmed = ifelse(confirmed < 100, 0, confirmed),
             mortality = ifelse(is.nan(mortality), 0, mortality),
             mortality = ifelse(confirmed < 100, 0, mortality)) %>%
      arrange(region, date)

    df %>% plot_ly(
      x = ~confirmed, 
      y = ~mortality, 
      size = ~death, 
      #group = ~region,
      #connectgaps = TRUE,
      #color = ~continent, 
      frame = ~date,
      text = ~region, 
      hoverinfo = "text",
      type = "scatter",
      mode = "markers"
      ) %>% 
      layout(
        xaxis = list(
          type = "log"
        )
      )
    
  })
  
}

shinyApp(ui, server)