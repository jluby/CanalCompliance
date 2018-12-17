# Load in a bunch of libraries. Some of these might no longer be used. There was a lot
# of trial and error in making this app.
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
library(bsplus)
library(htmltools)

# Read in our data.
vessel_data <- read_rds("vessels.rds")

# Define UI
ui <- fluidPage(
  
  # Implementing CSS as used here: https://shiny.rstudio.com/gallery/superzip-example.html.
  # The last few style tags were implemented to solve a few problems that popped up.
  # The leaflet-top style helps to get an unwanted select menu off screen.
  # The navbar option makes the navbar take up the full screen (for some reason was not doing initially)
  # The popover option helps to format our popovers by putting white space breaks wherever "\n".
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
                    body, label, input, button, select { 
                    font-family: 'Helvetica Neue', Helvetica;
                    font-weight: 200;
                    }
                    h1, h2, h3, h4, h5 { font-weight: 400; }
                    
                    #controls {
                    /* Appearance */
                    background-color: white;
                    padding: 0 0 20px 20px;
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
                    
                    .leaflet-top {
                    top: -100px;
                    }
                    
                    .navbar {
                    position: absolute;
                    top: 0;
                    right: 0;
                    width: 100% !important;
                    left: 0;
                    z-index: 20;
                    }
                    
                    .popover {
                    white-space: pre-line;    
                    }
                    "))
    ),
  
  # Create navbar up top
  navbarPage("Visualizing the Effects of and Compliance with the 2014 Gulf of Panama IMO TSS Regulations", id="nav",
             
             # Create map tabpanel. There are no other tabpanels but I liked the look.
             tabPanel("Interactive Map",
                      div(class="outer",  
                          
                          # Set leaflet output map to fill whole screen
                          leafletOutput("map", width='100%', height='100%'),
                          
                          # Create panel for selecting options. TSS defined a lot but define a few of the important
                          # features. Also adopted from https://shiny.rstudio.com/gallery/superzip-example.html.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 59, left = "auto", right = 2, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h3("Map Options"),
                                        
                                        # Select options for all of the maps. These are mostly basemaps so I think I could have
                                        # done this in an easier way but it's good to have the formatting set up for any map options
                                        # I can find online. The rest of the options are mostly just simple selects of whether or not to turn
                                        # on certain features. I manipulated the data in my rmd to make things pretty straightforward at this stage.
                                        # Div(style = height:" calls were put in to compress things a bit - helped to create more space for the plot below.
                                        div(style = "height: 52px", 
                                            selectInput("maptype", "Map Aesthetics", 
                                                        choices = c("Base" = "https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png",
                                                                    "Greyscale" = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
                                                                    "Dark" = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png",
                                                                    "Voyager" = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png",
                                                                    "Relief" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}",
                                                                    "Satellite" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}")) %>% 
                                              # Embed icon label within the selectInput and embed popover with the text we want. "sep = '\n\n'" allows us to create two line breaks (also uses CSS above)
                                              # I tried a few other ways to get popovers but this was the only one I could really get to work.
                                              shinyInput_label_embed(icon("info-sign", lib = "glyphicon") %>% 
                                                                       bs_embed_popover(title = "Map Aesthetics", 
                                                                                        content = paste("Dropdown: Select a base map.", 
                                                                                                        "'Geo Labels' checkbox: Toggle geographic landmark labels.", 
                                                                                                        "'2014 TSS' checkbox: Toggle the visual representation of the new TSS (blue).", 
                                                                                                        sep = "\n\n"),
                                                                                        placement = "left"))),
                                        
                                        # Put in a splitLayout in order to save some space. Individual checkboxInputs were used because I needed simple
                                        # TRUE/FALSE outputs.
                                        div(style = "height: 40px", 
                                            splitLayout(cellWidths = c("35%", "35%"), 
                                                        checkboxInput("labels", "Geo Labels", value = TRUE),
                                                        checkboxInput("tss", "2014 TSS", value = TRUE))),
                                        
                                        # shinyInput_label_embed does not work for checkboxGroupInputs so I needed to create a splitLayout and enter in an
                                        # icon "manually," so to speak. This required me to declare the "class" of the icon as "form-group shiny-input-container"
                                        # as this is the only class to which a bs_embed_popover can be added. The same logic followed for the next three checkboxGroupInputs
                                        # as well. Through trial and error I found that 91.5% was the width which put the icon directly inline with the more systematically embedded
                                        # icon above. Heights were also decided through trial and error. The "info-sign" icon was decided because the circle was easily
                                        # clickable and intuitive as being clickable. 
                                        # We use the paste() function within the content options of bs_embed_popover so that we can more easily declare
                                        # distinct statements and put in white space using the "sep" argument.
                                        div(style = "height: 52px", 
                                            splitLayout(cellWidths = c("91.5%", "5.5%"),
                                                        checkboxGroupInput("markers",
                                                                           label = "Track Aesthetics",
                                                                           choices = c("Data Points", "Speeding Points"), 
                                                                           inline = TRUE),
                                                        div(class = "form-group shiny-input-container", icon("info-sign", lib = "glyphicon") %>%
                                                              bs_embed_popover(title = "Track Aesthetics",
                                                                               content = paste("'Data Points' checkbox: Toggle visual representation of individual data points. Helpful in viewing geographic distribution of data.", 
                                                                                               "'Speeding Points' checkbox: Toggle visual representations of observations whose speed exceeded the 10 knot limit above 8ºN between August 1st and November 30th, for tracks before and after implementation of the regulations (for comparison).", 
                                                                                               sep = "\n\n"), 
                                                                               placement = "left")))),
                                        
                                        div(style = "height: 52px", 
                                            splitLayout(cellWidths = c("91.5%", "5.5%"),
                                                        checkboxGroupInput("era", 
                                                                           label = "Policy Filters", 
                                                                           choices = c("Pre-TSS", "Post-TSS", "10kn Limit"), 
                                                                           inline = TRUE, 
                                                                           select = c("Pre-TSS", "Post-TSS")),
                                                        div(class = "form-group shiny-input-container", icon("info-sign", lib = "glyphicon") %>% 
                                                              bs_embed_popover(title = "Policy Filters",
                                                                               content = paste("'Pre-TSS' checkbox: Filter observations prior to the institution of the new IMO policies. These trips are colored yellow.", 
                                                                                               "'Post-TSS' checkbox: Filter observations after the institution of the new IMO policies. These trips are colored red.", 
                                                                                               "'10kn Limit' checkbox: Filter trips to only those observed between August 1st and November 30th.", 
                                                                                               sep = "\n\n"), 
                                                                               placement = "left")))),
                                        
                                        div(style = "height: 52px", 
                                            splitLayout(cellWidths = c("91.5%", "5.5%"), 
                                                        checkboxGroupInput("direction", 
                                                                           label = "Direction of Travel", 
                                                                           choices = c("Inbound (North)" = "North", "Outbound (South)" = "South"), 
                                                                           inline = TRUE, 
                                                                           select = c("North", "South")),
                                                        div(class = "form-group shiny-input-container", icon("info-sign", lib = "glyphicon") %>% 
                                                              bs_embed_popover(title = "Direction of Travel", 
                                                                               content = paste("'Inbound (North)' checkbox: Filter Northbound trips (towards the Canal).", 
                                                                                               "'Outbound (South)' checkbox: Filter Southbound trips (away from the Canal).", 
                                                                                               sep = "\n\n"), 
                                                                               placement = "left")))),
                                        
                                        # Create a sliderInput so that users can select whatever draught range they want. Height was a bit higher on
                                        # this one just because the slider takes up a bit more space. Slider takes shinyInput_label_embed so addid the icon was a bit easier.
                                        div(style = "height: 85px", 
                                            sliderInput("draught", label = ("Vessel Draught"), min = 0, max = 20, value = c(0, 20)) %>% 
                                              shinyInput_label_embed(icon("info-sign", lib = "glyphicon") %>% 
                                                                       bs_embed_popover(title = "Vessel Draught", 
                                                                                        content = paste("Filter vessels according to draught.", 
                                                                                                        "Draught is the vertical distance between the waterline and the bottom of the hull.", 
                                                                                                        "Draught is a vital indicator of vessel size and cargo load.", 
                                                                                                        sep = "\n\n"), 
                                                                                        placement = "left"))),
                                        
                                        # Create a selectInput with multiple = TRUE so that users can select whatever combination of vessels
                                        # they want to. Took out a few types for which there were no observations. selectInput does take shinyInput_label_embed.
                                        div(style = "height: 55px", 
                                            selectInput("vesseltype", "Vessel Type", multiple = TRUE,
                                                        choices = c("Special Category" = c(3, 5),
                                                                    "Passenger" = 6,
                                                                    "Cargo" = 7,
                                                                    "Tanker" = 8,
                                                                    "Other" = 9)) %>% 
                                              shinyInput_label_embed(icon("info-sign", lib = "glyphicon") %>% 
                                                                       bs_embed_popover(title = "Vessel Type", 
                                                                                        content = paste("Filter vessels according to their type.", 
                                                                                                        "Ships which carry different types of cargo are structurally optimized accordingly and vary in shape, size, and draught.", 
                                                                                                        sep = "\n\n"), 
                                                                                        placement = "left"))),
                                        
                                        # Create an option for filtering inaccurate tracks. As of right now, as described earlier, tracks are by default only included if they have trustworthy
                                        # geographic tracks. Unfiltering for these tracks allows us to have more points to draw from in doing analysis of speed in the plots.
                                        div(style = "height: 20px", 
                                            checkboxInput("clean", "Filter Inaccurate Tracks", value = TRUE) %>% 
                                              shinyInput_label_embed(icon("info-sign", lib = "glyphicon") %>% 
                                                                       bs_embed_popover(title = "", 
                                                                                        content = paste("Tracks represented by default are only those which present an accurate representation of the geographic travel route, containing observations near the start and end of the new traffic lanes (between 7.75ºN-7.85ºN AND 8.483ºN-8.583ºN).", 
                                                                                                        "Unchecking this box is recommended in analyzing the plot below, as filtering to only accurate tracks heavily reduces the available speed data.", 
                                                                                                        "Check this box prior to toggling any other options in order to reduce computing time.", 
                                                                                                        sep = "\n\n"), 
                                                                                        placement = "left"))),
                                        
                                        # Add in our graph.
                                        div(style = "height: 250px", plotOutput("speeds", height = 270)),
                                        
                                        # Create a splitLayout once again so that we can implement an information icon for the graph.
                                        # Create a radioGroupButton so that users can choose between seeing a visualization of speed
                                        # means and seeing boxplots of the data. The means are nice for seeing trends but the boxplots
                                        # allow for a bit more of an analytical look and the policy's effectiveness.
                                        div(style = "height: 50px", 
                                            splitLayout(cellWidths = c("91.5%", "5.5%"),
                                                        radioGroupButtons("plottype", 
                                                                          choices = c("Means", "Boxplot"), 
                                                                          size = "xs"),
                                                        icon("info-sign", lib = "glyphicon") %>% 
                                                          bs_embed_popover(title = "", 
                                                                           content = paste("This plot represents the speeds of vessel groups, filtered according to the options above and grouped by latitude, direction, and policy (ie whether a trip occurred before or after the institution of the TSS).", 
                                                                                           "Perhaps the most interesting plot can be viewed by: \n(1) Unchecking the 'Pre-TSS' checkbox \n(2) Checking the '10kn Limit' checkbox (3) Unchecking the 'Filter Inaccurate Tracks' checkbox \n(4) Toggling the boxplot representation of the plot", "In this plot, we see that fewer than 25% of all outbound vessels (those with most incentive to speed), are actually following the speed limit between August 1st and November 30th. Even inbound vessels, who will likely need to wait upwards of 24 hours to enter the canal, do not adhere to the speed limit well. This is a concerning trend that has and will continue to limit the effectiveness of these policies in saving humpback whales as long as it is not addressed.", 
                                                                                           sep = "\n\n"), 
                                                                           placement = "left"))),
                                        # use_bs_popover() needs to be declared so that we can use the popover options above
                                        # useShinyjs() needs to be declared so that we can call a click() on the readme button upon loading.
                                        use_bs_popover(),
                                        useShinyjs()
                          )
                      )
             )
  ),
  # Create modal and button. Modal loads up readme and displays upon opening the app to help users.
  bs_modal(id = "modal", 
           title = "Visualizing the 2014 Gulf of Panama IMO TSS Regulations", 
           body = includeMarkdown("README.md"), 
           size = "large"),
  # Place the button using div(class) and set pointer-events:auto so that click() can function.
  # Attach the modal defined above using bs_attach_modal
  div(class = "leaflet-bottom leaflet-left", 
      style = "pointer-events: auto", 
      bs_button("Read Me", 
                button_type = "default", 
                id = "readme") %>%
        bs_attach_modal(id_modal = "modal")))


# Define server logic
server <- function(input, output, session) {
  
  # On load, click the readme button so that it displays right away.
  click("readme")
  
  # Create the map
  # Render a base so that we can toggle the different basemap options.
  output$map <- renderLeaflet({
    leaflet()
  })
  
  # This step is important so that I can clear any map that is not currently being used.
  # Prior to implementation of this list maps would just stack on top of one another indefinitely
  # (rather inefficient).
  # So this list just defines all maps and if any loaded map does not match the currently declared map,
  # it can be cleared.
  sources = c("https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png",
              "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
              'https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png',
              'https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png',
              'https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}',
              'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')
  
  # When maptype selection changes, we want to add to our map the selected tile and, depending on which tile was selected,
  # update the attribution on the bottom of the map. Once the tile has been loaded in, we remove any tile that is not currently selected
  # so that we don't waste memory. The addLayersControl lets R know that we want the map to always be below anything else we add.
  # addLayersControl does add unfortunate select menu on the top right, hence the top: -100px defined in the css.
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
        }, 
        layerId = input$maptype, 
        group = "base") %>% 
      # setView on initialization and then whenever map is changed. I just decided to put it here because I kind of liked the look of it
      # as it loads and shifts back into position.
      setView(lng = -79.2, lat = 8.2, zoom = 9) %>% 
      # Remove any tile that does not match our currently declared maptype
      removeTiles(layerId = c(setdiff(sources, input$maptype))) %>% 
      # Helps to clarify that the map is always the base. Other options can then be toggled individually.
      addLayersControl(baseGroups = "base", overlayGroups = c("Geo Labels", "tss", "markers"))
  })
  
  # Toggle label options when selected or deselected
  # A common thread throughout this part of the server is that we need to define
  # our input as a static object so that it can be used as a !NULL/NULL if statement.
  observeEvent(input$labels, {
    labelstrue <- input$labels
    
    # Whenever toggled we hide and clear labels (so that we don't stack indefinitely) and if it is TRUE then we add them back in
    leafletProxy("map") %>% 
      hideGroup("Geo Labels") %>% 
      clearGroup("Geo Labels")
    
    if (labelstrue) {
      leafletProxy("map") %>% 
        addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}{r}.png', group = "Geo Labels") %>% 
        showGroup("Geo Labels")
    }
  })
  
  # Build in the traffic separation scheme. Need to define a list of the coordinates of each line / polygon at the top
  # and then build them in using addPolygons. Similar "hide" and "show" logic as above. "Show" is only done after the
  # polygons have been drawn so that they all load in as one.
  observeEvent(input$tss, {
    mid_lon <- c(-79.45000000, -79.43333333, -79.43333333, -79.38333333, -79.38333333, -79.42888889, -79.45000000)
    mid_lat <- c(8.75277778, 8.58333333, 7.75000000, 7.75000000, 8.58333333, 8.76166667, 8.75277778)
    
    west_lon <- c(-80.19805556, -79.47222222, -79.47222222, -79.46666667, -79.75416667)
    west_lat <- c(7.06111111, 7.75000000, 8.58333333, 8.73333333, 8.73916667)
    
    east_lon <- c(-78.98111111, -79.41722222, -79.35000000, -79.35000000, -78.22333333)
    east_lat <- c(8.94666667, 8.76666667, 8.58333333, 7.75000000, 7.56666667)
    
    # Once again define as a static object.
    tsstrue <- input$tss
    
    # Hide and clear so that we don't stack forever
    leafletProxy("map") %>% 
      hideGroup("tss") %>% 
      clearGroup("tss")
    
    # Load in based on the coordinates declared above and then showGroup if button is clicked and is !NULL
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
    directions <- input$direction
    
    # If the user has selected only clean tracks (the default) we filter to only the tracks which have datapoints in the middle of the gulf
    # because otherwise we get a lot of straight lines from the mouth of the canal to the far end of the gulf.
    if (clean) {
      vessel_data <- vessel_data %>% 
        filter(clean == 2)
    }
    
    # If there is a vessel type selected, filter to only show the selected vessel types.
    if (!is.null(vesseltype)) {
      vessel_data <- vessel_data %>% 
        filter(vessel_type %in% vesseltype)
    }
    
    # Filter according to the selected timespans. "era" refers to pre- and post- implementation
    # of the TSS.
    vessel_data <- vessel_data %>% 
      filter(era %in% eras)
    
    # If the "10kn Limit" option has been selected, as defined in popover, we filter the data down to only
    # observations from August 1st to November 30th.
    if ("10kn Limit" %in% input$era) {
      vessel_data <- vessel_data %>% 
        mutate(month = as.numeric(str_sub(day, 6, 7))) %>% 
        filter(month > 7, month < 12)
    }
    
    # Filter the draughts down to those selected in the sliderInput in the ui.
    vessel_data <- vessel_data %>% 
      filter(draught >= draughts[1] & draught <= draughts[2])
    
    # Filter the direction to those selected.
    vessel_data <- vessel_data %>% 
      filter(direction %in% directions)
    
    # Declare a "speeds_data" group for use in the plot below.
    # We need to group according to lat_group, era, and direction for the map
    # We don't want these alterations to effect the tracks themselves so we define
    # as a new set.
    # Relevel the era so that it shows in the order we want.
    # Group_by direction for the plot
    speeds_data <- vessel_data %>% 
      group_by(lat_group, era, direction) %>%
      summarize(speed_mean = mean(speed)) %>% 
      ungroup() %>% 
      mutate(era = factor(era, levels = c("Pre-TSS", "Post-TSS"))) %>% 
      group_by(direction)
    
    # We also need to relevel our general vessel_data for the boxplot
    # Boxplot uses normal vessel_data because it integrates all observations automatically
    vessel_data <- vessel_data %>% 
      mutate(era = factor(era, levels = c("Pre-TSS", "Post-TSS")))
    
    # Define the plot mentioned in the ui.
    # If "means" is selected, we create a plot of the means by direction, policy, and latitude.
    # If "boxplot" is selected, we create a boxplot by the same groups.
    # Nearly all of the options are the same, though these plots use different data (mentioned above).
    output$speeds <- renderPlot({
      # If no data available based on the filters above, don't plot
      if (nrow(vessel_data) == 0)
        return(NULL)
      
      if (input$plottype == "Means") {
        ggplot(speeds_data, aes(y = speed_mean, 
                                x = lat_group, 
                                border = 'white')) + 
          geom_col(fill = '#00DD00') + 
          coord_flip() + 
          facet_grid(rows = vars(direction, era)) + 
          labs(y = "Average Speed (kn)", 
               x = "Latitude Group (ºN)", 
               title = "Average Speeds by Latitude, Direction, and Policy") + 
          theme(plot.title = element_text(hjust = 0.50)) + 
          scale_x_discrete(limits = c("7.4-7.6", 
                                      "7.6-7.8", 
                                      "7.8-8.0", 
                                      "8.0-8.2", 
                                      "8.2-8.4", 
                                      "8.4-8.6", 
                                      "8.6-8.8"))
      } else {
        ggplot(vessel_data, aes(y = speed, 
                                x = lat_group, 
                                border = 'white')) + 
          geom_boxplot(fill = '#00DD00') + 
          coord_flip() + 
          facet_grid(rows = vars(direction, era)) + 
          labs(y = "Speed (kn)", 
               x = "Latitude Group (ºN)", 
               title = "Vessel Speeds by Latitude, Direction, and Policy") + 
          theme(plot.title = element_text(hjust = 0.50)) + 
          scale_x_discrete(limits = c("7.4-7.6", 
                                      "7.6-7.8", 
                                      "7.8-8.0", 
                                      "8.0-8.2", 
                                      "8.2-8.4", 
                                      "8.4-8.6", 
                                      "8.6-8.8"))
      }
    })
    
    # Need to clear these so that the old versions of the filtered data don't stick around
    # Clear and hide logic same as above. The opacity of these tracks is not 1 so if we keep adding
    # on then the appearance of our tracks will change.
    
    # Vessels group refers to vessel tracks
    leafletProxy("map") %>% 
      hideGroup("vessels") %>% 
      clearGroup("vessels")
    
    # Circles group refers to data points
    leafletProxy("map") %>% 
      hideGroup("circles") %>% 
      clearGroup("circles")
    
    # Speeders group refers to speeding points
    leafletProxy("map") %>% 
      clearGroup("speeders")
    
    # Using the filtered data set, we build out the vessel tracks.
    # We arrange according to date_time so that tracks are built in the right order
    
    # We define two separate data sets, post_data and pre_data, which allows us to create
    # tracks of different colors for data after the TSS implementation and data before.
    # Building out the vessel tracks requires some funky logic but it is essentially done
    # iterating over the individual trips and using the addPolylines function.
    post_data <- vessel_data %>% 
      filter(era == "Post-TSS") %>% 
      arrange(date_time_utc)
    # Loop over each vessel trip, building up a polyline through all of the points
    for (i in unique(post_data$trip)) {
      leafletProxy("map") %>% 
        addPolylines(data = vessel_data[vessel_data$trip == i, ], 
                     lng = ~longitude, 
                     lat = ~latitude, weight = 1, color = "red", group = "vessels")
    } %>% showGroup("vessels")
    
    # Same as above. The decision to put the "pre" here was purely aesthetic as I wanted
    # the tracks to stay visible despire being far more sparse compared to the "post" observations.
    pre_data <- vessel_data %>% 
      filter(era == "Pre-TSS") %>% 
      arrange(date_time_utc)
    # Loop over each vessel trip, building up a polyline through all of the points
    for (i in unique(pre_data$trip)) {
      leafletProxy("map") %>% 
        addPolylines(data = vessel_data[vessel_data$trip == i, ], 
                     lng = ~longitude, 
                     lat = ~latitude, weight = 1, opacity = .8, color = "yellow", group = "vessels")
    } %>% showGroup("vessels")
    
    # true/false input for datapoint locations
    # Things are again a bit goofy because the input can't be used as a TRUE/FALSE input
    # The checkboxGroupInput also made things a bit harder but the logic works for both
    # the circles and speeders.
    circles <- if ("Data Points" %in% input$markers) {
      TRUE
    } else
    {
      FALSE
    }
    
    speeding <- if ("Speeding Points" %in% input$markers) {
      TRUE
    } else
    {
      FALSE
    }
    
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
    
    # Filter to relevant months and latitude, then speed to get only the speeding points
    # These are only the months for which the 10 knot speed limit is in place.
    speeder_data <- vessel_data %>% 
      mutate(month = as.numeric(str_sub(day, 6, 7))) %>% 
      filter(month > 7, month < 12) %>%
      filter(latitude > 8) %>% 
      filter(speed > 10)
    
    # Iterate over speeding points to show vessels in relevant range going over 10 knots.
    if (speeding) {
      for (i in unique(vessel_data$trip)) {
        leafletProxy("map") %>%
          addCircleMarkers(data = speeder_data[speeder_data$trip == i, ], 
                           lng = ~longitude, 
                           lat = ~latitude, radius = 1, opacity = 1, color = "yellow", group = "speeders")
      } %>%  showGroup("speeders")}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



