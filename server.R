shinyServer(function(input, output, session) {
  
  # Populate the selectInput with station names dynamically
  observe({
    updateSelectInput(session, "stationSelect", choices = sort(unique(combined_data$name)))
  })
  
  # Calculate the bounds of the data
  map_bounds <- list(
    lng_min = min(meta$longitude, na.rm = TRUE),
    lng_max = max(meta$longitude, na.rm = TRUE),
    lat_min = min(meta$latitude, na.rm = TRUE),
    lat_max = max(meta$latitude, na.rm = TRUE)
  )
  
  # Initialize a reactive value to store the clicked or selected station ID
  selected_station_id <- reactiveVal(NULL)
  
  # Update the selected station ID based on the dropdown selection
  observeEvent(input$stationSelect, {
    selected_station <- combined_data %>%
      filter(name == input$stationSelect) %>%
      pull(id)
    
  
    # Ensure we select only the first ID if multiple values are returned
    if (length(selected_station) > 0) {
      selected_station_id(selected_station[1])  # Select the first ID
    }
  })
  
  # Listen for click events on the map markers
  observeEvent(input$map_marker_click, {
    clicked_station <- input$map_marker_click$id
    
    if (!is.null(clicked_station)) {
      selected_station_id(clicked_station)
      
      # Also update the dropdown to reflect the selected station
      selected_station_name <- combined_data %>%
        filter(id == clicked_station) %>%
        pull(name)
      
      updateSelectInput(session, "stationSelect", selected = selected_station_name)
    }
  })
  
  # Display the selected station name in the sidebar
  output$selected_station_name <- renderText({
    # Check if a station is selected, if not display a default message
    if (is.null(selected_station_id())) {
      return("No station selected")
    }
    
    station_data <- combined_data %>% filter(id == selected_station_id())
    
    if (nrow(station_data) > 0) {
      return(paste("Selected Station:", station_data$name[1]))
    } else {
      return("No station selected")
    }
  })
  
  # Reactive expression to filter the combined data and calculate multi-annual means based on the selected aggregation
  filtered_data <- reactive({
    # Ensure a station ID is selected before filtering
    #req(selected_station_id())
    
    year_range <- input$yearRange  # Get the selected year range
    agg_type <- input$aggregation  # Get the selected aggregation type (Monthly, Seasonal, Annual)
    
    data_filtered <- combined_data %>%
      filter(
        altitude >= input$altitudeRange[1],
        altitude <= input$altitudeRange[2],
        variable == input$variable,
        year >= year_range[1],  # Filter for the selected year range
        year <= year_range[2]
      )
    
    # Calculate multi-annual means based on the selected aggregation type
    if (agg_type == "Monthly") {
      # Filter by the selected month and compute monthly multi-annual means
      data_filtered <- data_filtered %>%
        filter(month == input$month) %>%
        group_by(id, name, latitude, longitude, altitude) %>%
        summarise(multi_annual_value = mean(value, na.rm = TRUE), .groups = "drop")
      
    } else if (agg_type == "Seasonal") {
      # Define the seasons (DJF, MAM, JJA, SON)
      data_filtered <- data_filtered %>%
        mutate(season = case_when(
          month %in% c(12, 1, 2) ~ "DJF",
          month %in% c(3, 4, 5) ~ "MAM",
          month %in% c(6, 7, 8) ~ "JJA",
          month %in% c(9, 10, 11) ~ "SON"
        )) %>%
        filter(season == input$season) %>%  # Filter by the selected season
        group_by(id, name, latitude, longitude, altitude) %>%
        summarise(multi_annual_value = mean(value, na.rm = TRUE), .groups = "drop")
      
    } else if (agg_type == "Annual") {
      # Compute annual multi-annual means
      data_filtered <- data_filtered %>%
        group_by(id, name, latitude, longitude, altitude) %>%
        summarise(multi_annual_value = mean(value, na.rm = TRUE), .groups = "drop")
    }
    
    return(data_filtered)
  })
  
  # Reactive expression to filter the time series data for the selected station, variable, and time aggregation
  time_series_data <- reactive({
    # Ensure that a station ID is selected before proceeding
    req(selected_station_id())
    
    year_range <- input$yearRange
    agg_type <- input$aggregation
    
    data_filtered <- combined_data %>%
      filter(
        id == selected_station_id(),
        variable == input$variable,
        year >= year_range[1],
        year <= year_range[2]
      )
    
    if (agg_type == "Monthly") {
      # Return raw monthly data for the selected station
      return(
        data_filtered %>%
          filter(month == input$month) |>
          select(year, month, value) %>%
          arrange(year, month)
      )
      
    } else if (agg_type == "Seasonal") {
      # Group by season and calculate seasonal averages
      data_filtered <- data_filtered %>%
        mutate(season = case_when(
          month %in% c(12, 1, 2) ~ "DJF",
          month %in% c(3, 4, 5) ~ "MAM",
          month %in% c(6, 7, 8) ~ "JJA",
          month %in% c(9, 10, 11) ~ "SON"
        )) %>%
        group_by(year, season) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        filter(season == input$season) %>%
        arrange(year)
      return(data_filtered)
      
    } else if (agg_type == "Annual") {
      # Group by year and calculate annual averages
      return(
        data_filtered %>%
          group_by(year) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          arrange(year)
      )
    }
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
  
  
  # Render the time series plot
  output$time_series_plot <- renderPlot({
    # Ensure that time series data is available
    req(time_series_data())
    
    # Get the filtered time series data
    ts_data <- time_series_data()
    
    # Generate the plot based on the selected aggregation type
    if (input$aggregation == "Monthly") {
      ggplot(ts_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = value)) +
        geom_line(color = "blue") +
        labs(title = paste("Monthly Time Series for", input$variable),
             x = "Date", y = paste(input$variable, "Value")) +
        scale_x_date(date_labels = "%Y-%m") +
        theme_minimal()
      
    } else if (input$aggregation == "Seasonal") {
      ggplot(ts_data, aes(x = year, y = value)) +
        geom_line(color = "green") +
        labs(title = paste("Seasonal Time Series (", input$season, ") for", input$variable),
             x = "Year", y = paste(input$variable, "Value")) +
        theme_minimal()
      
    } else if (input$aggregation == "Annual") {
      ggplot(ts_data, aes(x = year, y = value)) +
        geom_line(color = "red") +
        labs(title = paste("Annual Time Series for", input$variable),
             x = "Year", y = paste(input$variable, "Value")) +
        theme_minimal()
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