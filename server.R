shinyServer(function(input, output, session) {
  
  
  # Calculate the bounds of the data
  map_bounds <- list(
    lng_min = min(meta$longitude, na.rm = TRUE),
    lng_max = max(meta$longitude, na.rm = TRUE),
    lat_min = min(meta$latitude, na.rm = TRUE),
    lat_max = max(meta$latitude, na.rm = TRUE)
  )
  
  # Initialize a reactive value to store the clicked station ID
  # Start with a random station ID at the beginning
  selected_station_id <- reactiveVal(sample(combined_data$id, 1))
  
  # Reactive expression to filter the combined data based on inputs (but not based on station click)
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
        fillOpacity = 0.7,
        layerId = ~id  # Set the layer ID to the station ID for click events
      )
  })
  
  # Listen for click events on the map markers
  observeEvent(input$map_marker_click, {
    # Get the clicked station's ID
    clicked_station <- input$map_marker_click$id
    
    if (!is.null(clicked_station)) {
      # Update the reactive value with the clicked station ID
      selected_station_id(clicked_station)
      
      # Optionally, print the station ID in the console for debugging
      print(paste("Clicked station ID:", clicked_station))
    }
  })
  
  # Display information about the selected station (random or clicked) and update when input$variable changes
  observe({
    if (!is.null(selected_station_id())) {
      # Get the data for the selected station and the selected variable
      specific_data <- combined_data %>% 
        filter(id == selected_station_id(), variable == input$variable, month == input$month)  # Add variable filtering
      
      # Update the output with the station details, including the value for the selected variable
      output$station_name_output <- renderText({
        if (nrow(specific_data) > 0) {
          paste("Selected Station:", specific_data$name[1], 
                "<br>ID:", specific_data$id[1], 
                "<br>Altitude:", specific_data$altitude[1], "m", 
                "<br>Value (", input$variable, "):", specific_data$value[1])
        } else {
          "No data available for the selected station and variable."
        }
      })
    }
  })
})