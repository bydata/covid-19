COVID-19 Dashboard
================

## Dashboard

A [COVID-19
dashboard](https://bydata.github.io/covid-19/dashboard/flexdashboard.html)
based on the flexdashboard R package. Updates to Corona virus figures
are fed into the dashboard via the [coronavirus R
package](https://www.github.com/RamiKrispin/coronavirus) by [Rami
Krispin](https://www.github.com/RamiKrispin/).

## Motivation

It’s my first experiment with
[flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/index.html),
an R package for creating interactive dashboards using RMarkdown. The
dashboard contains charts and widgets based on R packages like
[plotly](https://plot.ly/r/) for interactive plots (e.g. filtering by
clicking on legend elements, zooming),
[leaflet](https://rstudio.github.io/leaflet/) for interactive maps, and
[gganimate](https://github.com/thomasp85/gganimate) for animated plots.

## Additional R Scripts

  - [animated\_plots.R](R/animated_plots.R) creates any animated plot of
    how the number of confirmed cases per country developed over time.
    The resulting GIF file will be included in the dashboard as an
    image.
  - [parse\_rki.R](R/parse_rki.R) parses the latest figures for German
    federal status from the website of the Robert-Koch-Institut (RKI),
    Germany’s central scientific institution in the field of
    biomedicine. Saved output will be loaded in the dashboard and used
    to create the map of German federal states.

## Sources

  - Daily updated statistics by country:
    <https://www.github.com/RamiKrispin/coronavirus>, Johns Hopkins
    University Center for Systems Science and Engineering (JHU CCSE)
    Coronavirus repository
  - Update-to-date statistics from German federal states: [Robert Koch
    Institut](https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html)
    (RKI)
  - Shape files World: tmap R package
  - Shape files Germany: <https://gadm.org/download_country_v3.html>

## Latest Update

9 March 2020
