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

states = list("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin","Wyoming")

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      menuItem(selectizeInput("states", "Select State(s)", choices = states, selected="California", multiple=TRUE))
    ),
    dashboardBody(fluidRow(
      column(width = 9,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("map", height = 500))
             ))
))

server <- function(input, output) {
  
  st.abbs <- reactive ({
    st.abbs = c()
    for (i in 0:length(input$states)){
      st.abbs <- c(st.abbs, state.abb[which(state.name == input$states[i])])
    }
    st.abbs
  })
  
  tracts <- reactive ({
    census_api_key("ae0adfeb544b3e9ff4472500a625e6e9c8d97cd1")
  
  st_tracts <- get_acs(geography = "tract", 
                       variables = "B19001_001", 
                       state = st.abbs(),
                       geometry = TRUE)
  
  pal <- colorNumeric(palette = "viridis", 
                      domain = st_tracts$estimate)
  
  st_tracts <- st_tracts %>%
    st_transform(crs = "+init=epsg:4326")
  
  })
  
  NREL_states <- reactive({
    noquote(paste(st.abbs(), collapse = ','))
  })
  
  stations <- reactive({
  res <- GET(paste0("https://developer.nrel.gov/api/alt-fuel-stations/v1.json?&state=",NREL_states(),"&access=public&fuel_type=ELEC&api_key=0odqc8Jlse5m02yD2aUykpQ53pHlowIseRUvkKa8"))
  stations <- jsonlite::fromJSON(content(res, "text"), flatten=TRUE)
  stations <- stations$fuel_stations
})

  output$map <- renderLeaflet({
      leaflet() %>%
      setView(-120.8, 37, 5.5) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
      addPolygons(data = tracts(), popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.8,
                  color = ~ pal(estimate),
                  group = "Median Monthly Income") %>%
      addCircles(data = stations(), lng = ~longitude, lat = ~latitude, weight = 1, color="yellow",
                 group = "Charging Stations") %>%
      addHeatmap(data = stations(), lng= ~longitude, lat= ~latitude, max=100, radius=20, blur=10,
                 group = "Station Heatmap") %>%
      addLegend("bottomright", 
                pal = pal, 
                values = tracts()$estimate,
                title = "Median Monthly Income",
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
