shinyServer(function(input, output, session) {
  
  # Calculate the bounds of the data
  map_bounds <- list(
    lng_min = min(meta$longitude, na.rm = TRUE),
    lng_max = max(meta$longitude, na.rm = TRUE),
    lat_min = min(meta$latitude, na.rm = TRUE),
    lat_max = max(meta$latitude, na.rm = TRUE)
  )
  
  # Initialize a reactive value to store the clicked station ID
  selected_station_id <- reactiveVal(sample(combined_data$id, 1))
  
  # Reactive expression to filter the combined data based on inputs and calculate multi-annual monthly averages
  filtered_data <- reactive({
    year_range <- input$yearRange  # Get the selected year range
    
    combined_data %>%
      dplyr::filter(
        altitude >= input$altitudeRange[1], 
        altitude <= input$altitudeRange[2],
        variable == input$variable, 
        year >= year_range[1],  # Filter for the selected year range
        year <= year_range[2],
        month == input$month
      ) %>%
      group_by(id, name, latitude, longitude, altitude) %>%
      summarise(
        multi_annual_value = mean(value, na.rm = TRUE)  # Compute the multi-annual monthly average
      ) %>%
      ungroup()
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
                        "<br><strong>Multi-annual Value (", input$variable, "): </strong>", round(multi_annual_value, 2)),
        radius = 5,
        color = ~colorNumeric("viridis", multi_annual_value)(multi_annual_value),
        fillOpacity = 0.7,
        layerId = ~id  # Set the layer ID to the station ID for click events
      )
  })
  
  # Listen for click events on the map markers
  observeEvent(input$map_marker_click, {
    clicked_station <- input$map_marker_click$id
    
    if (!is.null(clicked_station)) {
      selected_station_id(clicked_station)
      
      # Optionally, print the station ID in the console for debugging
      print(paste("Clicked station ID:", clicked_station))
    }
  })
  
  # Display information about the selected station and update when input$variable changes
  observe({
    if (!is.null(selected_station_id())) {
      specific_data <- filtered_data() %>% filter(id == selected_station_id())
      
      output$station_name_output <- renderText({
        if (nrow(specific_data) > 0) {
          paste("Selected Station:", specific_data$name[1], 
                "<br>ID:", specific_data$id[1], 
                "<br>Altitude:", specific_data$altitude[1], "m", 
                "<br>Multi-annual Value (", input$variable, "):", round(specific_data$multi_annual_value[1], 2))
        } else {
          "No data available for the selected station and variable."
        }
      })
    }
  })
  
})