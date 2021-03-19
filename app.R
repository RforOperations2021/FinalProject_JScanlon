#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(acs)
library(tigris)
library(stringr)
library(httr)
library(sf)
library(choroplethr)
library(tidycensus)
library(rgdal)
library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(shinydashboard)

census_api_key("ae0adfeb544b3e9ff4472500a625e6e9c8d97cd1")

CA_tracts <- get_acs(geography = "tract", 
                     variables = "B19001_001", 
                     state = "CA",
                     geometry = TRUE)

pal <- colorNumeric(palette = "viridis", 
                    domain = CA_tracts$estimate)

CA_tracts <- CA_tracts %>%
  st_transform(crs = "+init=epsg:4326")

res <- GET("https://developer.nrel.gov/api/alt-fuel-stations/v1.json?&state=CA&access=public&fuel_type=ELEC&api_key=0odqc8Jlse5m02yD2aUykpQ53pHlowIseRUvkKa8")

stations <- jsonlite::fromJSON(content(res, "text"), flatten=TRUE)

stations <- stations$fuel_stations

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(fluidRow(
      column(width = 9,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("map", height = 500))
             ))
))

server <- function(input, output) {

  output$map <- renderLeaflet({
      leaflet() %>%
      setView(-120.8, 37, 5.5) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
      addPolygons(data = CA_tracts, popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.8,
                  color = ~ pal(estimate),
                  group = "Median Monthly Income") %>%
      addCircles(data = stations, lng = ~longitude, lat = ~latitude, weight = 1, color="red",
                 group = "Charging Stations") %>%
      addHeatmap(data = stations, lng= ~longitude, lat= ~latitude, max=100, radius=20, blur=10,
                 group = "Station Heatmap") %>%
      addLegend("bottomright", 
                pal = pal, 
                values = CA_tracts$estimate,
                title = "Income",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1) %>%
      addLayersControl(
        baseGroups = c("Positron"),
        overlayGroups = c("Charging Stations", "Station Heatmap", "Median Monthly Income"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Station Heatmap")
    
  }) 

}

shinyApp(ui, server)
