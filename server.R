library(shiny)
library(leaflet)
library(leaflet.extras)  # To add the "Home" button
library(arrow)  # To read the Parquet file
library(dplyr)  # For data manipulation

# Define server logic
shinyServer(function(input, output, session) {
  
  # Load the data from the Parquet file
  meta <- read_parquet("www/data/tabs/roclihom_meta_1901_2023.parquet")
  
  # Calculate the bounds of the data (min and max longitudes and latitudes)
  map_bounds <- list(
    lng_min = min(meta$Longitude, na.rm = TRUE),
    lng_max = max(meta$Longitude, na.rm = TRUE),
    lat_min = min(meta$Latitude, na.rm = TRUE),
    lat_max = max(meta$Latitude, na.rm = TRUE)
  )
  
  # Get the minimum and maximum altitude for the slider
  min_altitude <- min(meta$Altitude, na.rm = TRUE)
  max_altitude <- max(meta$Altitude, na.rm = TRUE)
  
  # Update the altitude slider dynamically based on the data
  observe({
    updateSliderInput(session, "altitudeRange", 
                      min = min_altitude, 
                      max = max_altitude, 
                      value = c(min_altitude, max_altitude))
  })
  
  # Reactive expression to filter the meta data based on altitude range
  filtered_data <- reactive({
    meta %>%
      filter(Altitude >= input$altitudeRange[1], Altitude <= input$altitudeRange[2])
  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    
    # Define the center of the map based on the bounds
    center_lat <- (map_bounds$lat_min + map_bounds$lat_max) / 2
    center_lng <- (map_bounds$lng_min + map_bounds$lng_max) / 2
    
    # Initial leaflet map with zoom limits set using leafletOptions
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 18)) %>%
      addTiles() %>%
      # Fit the map to the bounds of the meta data
      fitBounds(
        lng1 = map_bounds$lng_min, lat1 = map_bounds$lat_min,
        lng2 = map_bounds$lng_max, lat2 = map_bounds$lat_max
      ) %>%
      # Set the max bounds to prevent zooming out beyond the data extent
      setMaxBounds(
        lng1 = map_bounds$lng_min, lat1 = map_bounds$lat_min,
        lng2 = map_bounds$lng_max, lat2 = map_bounds$lat_max
      ) %>%
      # Add a "Home" button that will reset the map to the initial view
      addResetMapButton()
  })
  
  # Observe the filtered data and update the map accordingly
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, 
        popup = ~paste0("<strong>Name: </strong>", Name, 
                        "<br><strong>ID: </strong>", ID, 
                        "<br><strong>Altitude: </strong>", Altitude, " m"),
        radius = 5,  # Size of the markers
        color = "blue",  # Marker color
        fillOpacity = 0.7  # Opacity of the marker fill
      )
  })
})