Readme
================

## Dashboard

Visit [COVID-19 dashboard](dashboard/flexdashboard.html) based on
flexdashboard R package

## R Scripts

  - [animated\_plots.R](R/animated_plots.R) creates any animated plot of
    how the number of confirmed cases per country developed over time.
    The resulting GIF file will be included in the dashboard as an
    image.
  - [parse\_rki.R](R/parse_rki.R) parses the latest figures for German
    federal status from the website of the Robert-Koch-Institute. Saved
    output will be loaded in the dashboard and used to create the German
    map.

## Sources

  - Daily updated statistics by country:
    <https://www.github.com/RamiKrispin/coronavirus>, Johns Hopkins
    University Center for Systems Science and Engineering (JHU CCSE)
    Coronavirus repository
  - Daily updated statistics for German federal states: [Robert Koch
    Institut](https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html)
    (RKI)
  - Shape files World: tmap R package
  - Shape files Germany: <https://gadm.org/download_country_v3.html>
