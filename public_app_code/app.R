library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(tidytext)
library(ggplot2)
library(janitor)
library(tidyverse)
library(shiny)
library(leaflet)
library(shinyjs)
library(dplyr)
library(sp)
library(data.table)
library(shinyWidgets)

vessel_data <- read_rds("vessels.rds")

# Define UI
ui <- fluidPage(
  
  # Implementing CSS as used here: https://shiny.rstudio.com/gallery/superzip-example.html.
  # Will need to clean up and look at only relevant portions for when final app is published.
  tags$head(
    tags$style(HTML("input[type='number'] {
                    max-width: 80%;
                    }
                    
                    div.outer {
                    position: fixed;
                    top: 0;
                    left: 0;
                    right: 0;
                    bottom: 0;
                    overflow: hidden;
                    padding: 0;
                    }
                    
                    /* Customize fonts */
                    body, label, input, button, select, checkbox { 
                    font-family: 'Helvetica Neue', Helvetica;
                    font-weight: 200;
                    }
                    h1, h2, h3, h4 { font-weight: 400; }
                    
                    #controls {
                    /* Appearance */
                    background-color: white;
                    padding: 0 20px 20px 20px;
                    cursor: move;
                    /* Fade out while not hovering */
                    opacity: 0.65;
                    zoom: 0.9;
                    transition: opacity 500ms 1s;
                    }
                    #controls:hover {
                    /* Fade in while hovering */
                    opacity: 0.95;
                    transition-delay: 0;
                    }
                    
                    /* Position and style citation */
                    #cite {
                    position: absolute;
                    bottom: 10px;
                    left: 10px;
                    font-size: 12px;
                    }
                    
                    /* If not using map tiles, show a white background */
                    .leaflet-container {
                    background-color: white !important;
                    }
                    "))
    ),
  div(class="outer",
      
      # Set leaflet output map to fill whole screen
      leafletOutput("map", width='100%', height='100%'),
      
      # Create panel for selecting options. TSS defined a lot but define a few of the important
      # features.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("Map Options"),
                    
                    # Select options for all of the maps. These are mostly basemaps so I think I could have
                    # done this in an easier way but it's good to have the formatting set up for any map options
                    # I can find online. The rest of the options are mostly just simple selects of whether or not to turn
                    # on certain features. I manipulated the data in my rmd to make things pretty straightforward at this stage.
                    selectInput("maptype", "Map Type", 
                                choices = c("Base" = "https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png",
                                            "Greyscale" = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
                                            "Dark" = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png",
                                            "Voyager" = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png",
                                            "Relief" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}",
                                            "Satellite" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}")),
                    checkboxInput("labels", "Geo Labels", value = TRUE),
                    checkboxInput("tss", "2014 TSS", value = TRUE),
                    checkboxInput("vessels", "Vessels", value = TRUE),
                    checkboxInput("circles", "Data Points", value = FALSE),
                    checkboxInput("speeding", "Speeding Points", value = FALSE),
                    prettyCheckboxGroup("era", label = "Policy Filter", choices = c("Pre-TSS", "Post-TSS"), shape = "square", inline = TRUE, select = c("Pre-TSS", "Post-TSS")),
                    sliderInput("draught", label = ("Draught"), min = 0, 
                                max = 20, value = c(0, 20)),
                    selectInput("vesseltype", "Vessel Type", multiple = TRUE,
                                choices = c("Reserved" = 1, 
                                            "Wing in Ground" = 2,
                                            "Special Category" = c(3, 5),
                                            "High Speed Craft" = 4,
                                            "Passenger" = 6,
                                            "Cargo" = 7,
                                            "Tanker" = 8,
                                            "Other" = 9)),
                    checkboxInput("clean", "Filter Inaccurate Tracks", value = TRUE)
      )
  )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create the map
  # Render a base so that we can toggle the different basemap options.
  output$map <- renderLeaflet({
    leaflet()
  })
  
  # This step is important so that I can clear any map that is not currently being used.
  # Prior to implementation of this list maps would just stack on top of one another indefinitely
  # (rather inefficient).
  sources = c("https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png",
              "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
              'https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png',
              'https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png',
              'https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}',
              'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')
  
  # When maptype selection changes, we want to add to our map the selected tile and, depending on which tile was selected,
  # update the attribution on the bottom of the map. Once the tile has been loaded in, we remove any tile that is not currently selected
  # so that we don't waste memory. The addLayersControl lets R know that we want the map to always be below anything else we add.
  observeEvent(input$maptype, {
    leafletProxy("map") %>%
      addTiles(
        urlTemplate = input$maptype,
        if (input$maptype == "https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png") {
          attribution = 'Tiles courtesy of <a href="http://openstreetmap.se/" target="_blank">OpenStreetMap Sweden</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
        }
        else if (input$maptype == "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png") {
          attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="https://carto.com/attributions">CARTO</a>'
        }
        else if (input$maptype == 'https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png') {
          attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="https://carto.com/attributions">CARTO</a>'
        }
        else if (input$maptype == 'https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png') {
          attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="https://carto.com/attributions">CARTO</a>'
        }
        else if (input$maptype == 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}') {
          attribution = 'Tiles &copy; Esri &mdash; Source: Esri'
        }
        else if (input$maptype == 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') {
          attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community'
        }, layerId = input$maptype, group = "base") %>% 
      setView(lng = -79.2, lat = 8.14, zoom = 9) %>% 
      removeTiles(layerId = c(setdiff(sources, input$maptype))) %>% 
      addLayersControl(baseGroups = "base", overlayGroups = c("Geo Labels", "tss", "vessels", "circles"))
  })
  
  # Toggle label options when selected or deselected
  observeEvent(input$labels, {
    labelstrue <- input$labels
    
    # Whenever toggled we hide labels adn if it is TRUE then we add them back in
    leafletProxy("map") %>% 
      hideGroup("Geo Labels")
    
    if (labelstrue) {
      leafletProxy("map") %>% 
        addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}{r}.png', group = "Geo Labels") %>% 
        showGroup("Geo Labels")
    }
  })
  
  # Build in the traffic separation scheme. Need to define a list of the coordinates of each line / polygon at the top
  # and then build them in using addPolygons. Similar "hide" and "show" logic as above. "Show" is only done after th
  # polygons have been drawn so that they all load in as one.
  observeEvent(input$tss, {
    mid_lon <- c(-79.45000000, -79.43333333, -79.43333333, -79.38333333, -79.38333333, -79.42888889, -79.45000000)
    mid_lat <- c(8.75277778, 8.58333333, 7.75000000, 7.75000000, 8.58333333, 8.76166667, 8.75277778)
    
    west_lon <- c(-80.19805556, -79.47222222, -79.47222222, -79.46666667, -79.75416667)
    west_lat <- c(7.06111111, 7.75000000, 8.58333333, 8.73333333, 8.73916667)
    
    east_lon <- c(-78.98111111, -79.41722222, -79.35000000, -79.35000000, -78.22333333)
    east_lat <- c(8.94666667, 8.76666667, 8.58333333, 7.75000000, 7.56666667)
    
    tsstrue <- input$tss
    
    leafletProxy("map") %>% 
      hideGroup("tss")
    
    if (tsstrue) {
      leafletProxy("map") %>% 
        addPolygons(lng = mid_lon, lat = mid_lat, 
                    group = "tss", weight = 1) %>% 
        addPolylines(lng = west_lon, lat = west_lat,
                     group = "tss", weight = 1) %>% 
        addPolylines(lng = east_lon, lat = east_lat,
                     group = "tss", weight = 1) %>% 
        showGroup("tss")
    }
  })
  
  # whenever anything that is not tss, label, or map is changed, we need to filter our vessel data accordingly
  # and then plot it. I had a lot of issues getting R to interpret the input$s as "TRUE" so I had to declare them above
  # in order to get them to work.
  observe({
    vesseltype <- input$vesseltype
    draughts <- input$draught
    eras <- input$era
    clean <- input$clean
    vessels <- input$vessels
    speeding <- input$speeding
    
    # If the user has selected only clean tracks (the default) we filter to only the tracks which have datapoints in the middle of the gulf
    # because otherwise we get a lot of straight lines from the mouth of the canal to the far end of the gulf.
    if (clean) {
      vessel_data <- vessel_data %>% 
        filter(clean == 1)
    }
    
    # If there is a vessel type selected, filter to only show the selected vessel types.
    if (!is.null(vesseltype)) {
      vessel_data <- vessel_data %>% 
        filter(vessel_type %in% vesseltype)
    }
    
    # Filter according to the selected timespans. "era" refers to pre- and post- implementation
    # of the TSS.
    vessel_data <- vessel_data %>% 
      filter(era %in% eras) %>% 
      filter(draught >= draughts[1] & draught <= draughts[2])
    
    leafletProxy("map") %>% 
      hideGroup("vessels")
    
    # Need to clear these so that the old versions of the filtered data don't stick around
    leafletProxy("map") %>% 
      clearGroup("vessels")
    
    # Using the filtered data set, we build out the vessel tracks.
    # We arrange according to date_time so that tracks are built int the right order
    if (vessels) {
      vessel_data <- vessel_data %>% 
        arrange(date_time_utc)
      # Loop over each vessel trip, building up a polyline through all of the points
      for (i in unique(vessel_data$trip)) {
        leafletProxy("map") %>% 
          addPolylines(data = vessel_data[vessel_data$trip == i, ], 
                       lng = ~longitude, 
                       lat = ~latitude, weight = 1, color = "red", group = "vessels")
      } %>% showGroup("vessels") }
    
    # true/false input for datapoint locations
    circles <- input$circles
    
    leafletProxy("map") %>% 
      hideGroup("circles")
    
    leafletProxy("map") %>% 
      clearGroup("circles")
    
    # If circles have been selected, we want to do the same thing we did before to add in datapoints
    # We loop over each track adding in datapoints.
    if (circles) {
      for (i in unique(vessel_data$trip)) {
        leafletProxy("map") %>%
          addCircleMarkers(data = vessel_data[vessel_data$trip == i, ], 
                           lng = ~longitude, 
                           lat = ~latitude, radius = 1, opacity = 1, color = "speed", group = "circles")
      } %>%  showGroup("circles")}
    
    # Here we create an option to view vessels that are breaking speed laws according to TSS standards.
    leafletProxy("map") %>% 
      clearGroup("speeders")
    
    # Filter to relevant months and latitude, then speed to get only the speeding points
    vessel_data <- vessel_data %>% 
      mutate(month = as.numeric(str_sub(day, 6, 7))) %>% 
      filter(month > 7, month < 12) %>%
      filter(latitude > 8) %>% 
      filter(speed > 10)
    
    # Iterate over speeding points to show vessels in relevant range going over 10 knots.
    if (speeding) {
      for (i in unique(vessel_data$trip)) {
        leafletProxy("map") %>%
          addCircleMarkers(data = vessel_data[vessel_data$trip == i, ], 
                           lng = ~longitude, 
                           lat = ~latitude, radius = 1, opacity = 1, color = "yellow", group = "speeders")
      } %>%  showGroup("speeders")}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




