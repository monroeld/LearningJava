library(lubridate)
library(tidyverse)
library(padr)
library(ggmap)
library(leaflet)
library(htmltools)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(REdaS)

# Read in pre-prepared data
summTable <- read.csv("summCSV.csv", stringsAsFactors = F)
Counts <- read.csv("CountCSV.csv", stringsAsFactors = F)

ui <- dashboardPage(
  dashboardHeader(title = "Spot on CML"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Header area
    fluidRow(
      valueBox(paste(Sys.Date() - as.Date("2017-09-22", format = "%Y-%m-%d"),
                                          "days"), "Since Project Start Date",
               width = 4, color = "light-blue"),
      valueBox(summTable[, 2], "Samples Received", width = 4, color = "light-blue"),
      valueBox(summTable[, 3], "Total Miles Travelled by Samples", width = 4, color = "light-blue"),
      align = "center"
    ),
    # Map
    fluidRow(align = "center",
      helpText("Click the markers for information about Spot On CML samples from that country"),
      br(),
      leafletOutput("worldmap", height = 600)
    )
  )
)

server <- shinyServer(function(input, output, session) {
  output$worldmap <- renderLeaflet({
  
    # Render map, fix zoom, add markers based on institutionCounts
    test <- leaflet(Counts,
                    options = leafletOptions(zoomControl = FALSE,
                                             minZoom = 2, maxZoom = 2)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 popup = ~Popup) %>%
      setView(lng = 0, lat = -30, zoom=2) %>%
     fitBounds(-180, -80, 180, 90) %>% setMaxBounds(-180, -80, 180, 90)
    
  })

})

shinyApp(ui, server)
