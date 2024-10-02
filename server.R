# Define server logic
shinyServer(function(input, output, session) {
  
  # Join the data with meta based on id
  combined_data <- data %>%
    left_join(meta, by = "id")
  
  # Calculate the bounds of the data
  map_bounds <- list(
    lng_min = min(meta$longitude, na.rm = TRUE),
    lng_max = max(meta$longitude, na.rm = TRUE),
    lat_min = min(meta$latitude, na.rm = TRUE),
    lat_max = max(meta$latitude, na.rm = TRUE)
  )
  
 
  # Reactive expression to filter the combined data based on inputs
  filtered_data <- reactive({
    
    combined_data %>%
      dplyr::filter(altitude >= input$altitudeRange[1], altitude <= input$altitudeRange[2],
             variable == input$variable, 
             year == input$year, 
             month == input$month)
    
  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    center_lat <- (map_bounds$lat_min + map_bounds$lat_max) / 2
    center_lng <- (map_bounds$lng_min + map_bounds$lng_max) / 2
    
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 18)) %>%
      addTiles() %>%
      fitBounds(
        lng1 = map_bounds$lng_min, lat1 = map_bounds$lat_min,
        lng2 = map_bounds$lng_max, lat2 = map_bounds$lat_max
      ) %>%
      setMaxBounds(
        lng1 = map_bounds$lng_min, lat1 = map_bounds$lat_min,
        lng2 = map_bounds$lng_max, lat2 = map_bounds$lat_max
      ) %>%
      addResetMapButton()
  })
  
  # Observe the filtered data and update the map accordingly
  observe({
    
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude, 
        popup = ~paste0("<strong>Name: </strong>", name, 
                        "<br><strong>ID: </strong>", id, 
                        "<br><strong>Altitude: </strong>", altitude, " m",
                        "<br><strong>Value: </strong>", value),
        radius = 5,
        color = ~colorNumeric("viridis", value)(value),
        fillOpacity = 0.7
      )
  })
})
