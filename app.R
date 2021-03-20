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
library(ggplot2)
library(plotly)
library(dplyr)
library(ggforce)

census_api_key("ae0adfeb544b3e9ff4472500a625e6e9c8d97cd1")

race_vars <- c("Median Monthly Income" = "B19001_001", "Percent White" = "B03002_003", "Percent Black" = "B03002_004", "Percent Native American" = "B03002_005", "Percent Asian" = "B03002_006","Percent Hawaiian/P.I." = "B03002_007", "Hispanic" = "B03002_012")

states = list("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin","Wyoming")

racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      menuItem(selectizeInput("states", "Select State(s)", choices = states, selected="California", multiple=TRUE),
               menuItem(downloadButton("download1", "Download Data"), br()))
    ),
    dashboardBody(fluidRow(
      column(width = 7,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("map", height = 600))),
      column(width= 5,
             box(width=NULL, solidHeader = TRUE,
                 plotlyOutput("pie", height=275)),
             box(width = NULL, solidHeader = TRUE,
                 plotlyOutput("barchart", height=275)))
    
)))

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
  res <- GET(paste0("https://developer.nrel.gov/api/alt-fuel-stations/v1.json?&state=",NREL_states(),"&access=all&fuel_type=all&api_key=0odqc8Jlse5m02yD2aUykpQ53pHlowIseRUvkKa8"))
  stations <- jsonlite::fromJSON(content(res, "text"), flatten=TRUE)
  stations <- stations$fuel_stations
})
  
  tab1 <- reactive({
    dtable <- datatable(stations())
  })

  output$map <- renderLeaflet({
      leaflet() %>%
      setView(-115.8, 37, 5) %>%
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
      hideGroup("Station Heatmap")}) 

  output$download1 <- downloadHandler(
    filename = function() {
      paste("CharingStations.csv", sep="")
    },
    content = function(file) {
      write.csv(tab1(), file)
    })  
  
  bar <- reactive({
    stations() %>%
    count(facility_type)%>%
    filter(n>10 & n<3000)})
  
  output$barchart <- renderPlotly({
    ggplotly(
    ggplot(data = bar(), aes(x=reorder(facility_type, n), y=n, fill=facility_type)) +
      geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=90), axis.title.x = element_blank(), legend.position='none') +
      labs(y="Total Number", title = "Frequency of Charging Station Facility Types"))})
  
  pie <- reactive({
    stations() %>%
    count(access_code)})
  
  output$pie <- renderPlotly({
    fig <- plot_ly(pie(), labels = ~access_code, values = ~n, type = 'pie')
    fig <- fig %>% layout(title = 'Frequncy of Charging Station Types',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
}


shinyApp(ui, server)
