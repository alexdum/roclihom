shinyServer(function(input, output, session) {
  
  # Populate the selectInput with station names dynamically
  observe({
    updateSelectInput(session, "stationSelect", choices = sort(meta$name))
  })
  
  
  # Initialize a reactive value to store the clicked or selected station ID
  selected_station_id <- reactiveVal(NULL)
  
  # Update the selected station ID based on the dropdown selection
  observeEvent(input$stationSelect, {
    selected_station <- meta %>%
      filter(name == input$stationSelect) %>%
      pull(id)
    
    selected_station_id(selected_station) 
    
  })
  
  # Listen for click events on the map markers
  observeEvent(input$map_marker_click, {
    clicked_station <- input$map_marker_click$id
    
    if (!is.null(clicked_station)) {
      
      # Also update the dropdown to reflect the selected station
      selected_station_name <- meta %>%
        filter(id == clicked_station) %>%
        pull(name)
      
      updateSelectInput(session, "stationSelect", selected = selected_station_name)
    }
  })
  
  # Reactive expression to filter the combined data and calculate multi-annual means or sums
  filtered_data <- reactive({
    # Ensure a station ID is selected before filtering
    req(selected_station_id())
    
    # Add a progress bar
    withProgress(message = 'Processing data...', value = 0, {
      
      # Step 1: Get user inputs and initialize data filtering
      incProgress(0.2, detail = "Setting up filters...")  # Progress at 20%
      
      year_range <- input$yearRange  # Get the selected year range
      agg_type <- input$aggregation  # Get the selected aggregation type (Monthly, Seasonal, Annual)
      
      # Filter data based on user input
      data_filtered <- combined_data %>%
        filter(
          altitude >= input$altitudeRange[1],  # Filter by altitude range
          altitude <= input$altitudeRange[2],
          variable == input$variable,          # Filter by the selected variable
          year >= year_range[1],               # Filter by the selected year range
          year <= year_range[2]
        )
      
      # Step 2: Apply aggregation based on agg_type
      incProgress(0.4, detail = paste("Applying", agg_type, "aggregation..."))  # Progress at 40%
      
      if (agg_type == "Monthly") {
        # Compute multi-annual means for the selected month
        data_filtered <- data_filtered %>%
          filter(month == input$month) %>%
          group_by(id, name, latitude, longitude, altitude) %>%
          summarise(multi_annual_value = mean(value, na.rm = TRUE), .groups = "drop")
        
      } else if (agg_type == "Seasonal") {
        data_filtered <-
          data_filtered |>
          mutate(
            Date = as.Date(paste(year, month, "01", sep = "-")),
            season_id =  (12 * year + month) %/% 3,
            season = mkseas(Date, "DJF")
          ) |>
          group_by(season_id) %>%
          mutate(season_label = paste(max(year), season)) %>%
          filter(Date >= as.Date("1901-03-01"), Date <= as.Date("2023-11-30")) %>% #pentru calcul sezon corect trecere ani
          filter(season == input$season) %>%  # Filter by the selected season
          group_by(id, season_label, name, latitude, longitude, altitude) %>%
          summarise(value = if (input$variable == "PREC") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE), .groups = "drop") %>%
          separate(season_label, into = c("year", "season"), sep = " ") |>
          group_by(id, name, latitude, longitude, altitude) %>%
          summarise(multi_annual_value = mean(value, na.rm = TRUE), .groups = "drop") |>
          mutate(multi_annual_value = round(multi_annual_value, 1))
        
      } else if (agg_type == "Annual") {
        # Compute annual sums if PREC, otherwise mean
        data_filtered <- data_filtered %>%
          group_by(id, name, latitude, longitude, altitude, year) %>%
          summarise(
            annual_value = if (input$variable == "PREC") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          group_by(id, name, latitude, longitude, altitude) %>%
          summarise(multi_annual_value = mean(annual_value, na.rm = TRUE), .groups = "drop")
      }
      
      # Step 3: Finalizing the data processing
      incProgress(1, detail = "Finalizing...")  # Progress at 100%
      
      return(data_filtered)
    })
  })
  
  # Reactive expression to filter the time series data for the selected station, variable, and time aggregation
  time_series_data <- reactive({
    # Ensure that a station ID is selected before proceeding
    req(selected_station_id())
    
    # Common filtering for all aggregation types
    year_range <- input$yearRange
    data_filtered <- combined_data %>%
      filter(
        id == selected_station_id(),
        variable == input$variable,
        year >= year_range[1],
        year <= year_range[2]
      )
    
    # Determine the aggregation type
    agg_type <- input$aggregation
    
    if (agg_type == "Monthly") {
      return(
        data_filtered %>%
          filter(month == input$month) %>%
          dplyr::select(name, year, month, value) %>%
          arrange(year, month)
      )
      
    } else if (agg_type == "Seasonal") {
      # Add date and season information
      data_filtered <- data_filtered %>%
        mutate(
          Date = as.Date(paste(year, month, "01", sep = "-")),
          season_id = (12 * year + month) %/% 3,
          season = mkseas(Date, "DJF")  # Assuming mkseas is properly defined
        ) %>%
        group_by(season_id) %>%
        mutate(season_label = paste(max(year), season)) %>%
        filter(Date >= as.Date("1901-03-01"), Date <= as.Date("2023-11-30")) %>%
        group_by(id, season_label) %>%
        summarise(value = if (input$variable == "PREC") sum(value) else mean(value), .groups = "drop") %>%
        separate(season_label, into = c("year", "season"), sep = " ") %>%
        filter(season == input$season) %>%
        mutate(value = round(value, 1), year = as.numeric(year)) %>%
        arrange(year)
      print(summary(data_filtered))
      return(data_filtered)
      
    } else if (agg_type == "Annual") {
      return(
        data_filtered %>%
          group_by(name, year) %>%
          summarise(value = if (input$variable == "PREC") sum(value) else mean(value), .groups = "drop") %>%
          mutate(value = round(value, 1)) %>%
          arrange(year)
      )
    }
  })
  output$map <- renderLeaflet({
    center_lat <- 45
    center_lng <- 25
    
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 18)) %>%
      addTiles(group = "OpenStreetMap") %>%  # Default OpenStreetMap
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topo Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      fitBounds(
        lng1 = map_bounds$lng_min, lat1 = map_bounds$lat_min,
        lng2 = map_bounds$lng_max, lat2 = map_bounds$lat_max
      ) %>%
      setMaxBounds(
        lng1 = map_bounds$lng_min, lat1 = map_bounds$lat_min,
        lng2 = map_bounds$lng_max, lat2 = map_bounds$lat_max
      ) %>%
      addLayersControl(
        baseGroups = c("Esri World Topo Map", "OpenStreetMap", "Esri World Imagery"),
        options = layersControlOptions(collapsed = T) 
      ) %>%
      addResetMapButton()
  })
  
  # Observe changes and update markers accordingly
  observe({
    req(filtered_data())  # Ensure that filtered_data is available
    
    # Retrieve the current selected station ID
    selected_id <- selected_station_id()
    
    # Define color palettes
    color_pal <- get_color_palette(input$variable, domain = filtered_data()$multi_annual_value, reverse = FALSE)
    color_pal2 <- get_color_palette(input$variable, domain = filtered_data()$multi_annual_value, reverse = TRUE)
    
    # Update the markers on the map
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude,
        label = ~paste0(
          "<strong>Name: </strong>", name,
          "<br><strong>", input$variable, ": </strong>", round(multi_annual_value, 1),
          "<br><span style='color:red;'>click to update</span>"
        ) %>% lapply(htmltools::HTML),  # Ensure HTML format for label
        radius = ~ifelse(id == selected_id, 8, 5),  # Larger radius for selected station
        #color = ~ifelse(id == selected_id, "#808080", color_pal2(multi_annual_value)),  # Different color for selected
        # Border color (stroke) changes when selected, otherwise it's transparent or a default value
        color = ~ifelse(id == selected_id, "#FF0000", "#00000000"),  # Red border for selected, transparent for others
        
        # Inside color stays the same for all, based on the color palette
        fillColor = ~color_pal2(multi_annual_value), 
        stroke = ~ifelse(id == selected_id, TRUE, FALSE),  # Add a border stroke for selected
        weight = ~ifelse(id == selected_id, 2, 1),        # Thicker border for selected
        fillOpacity = 0.9,  # Increased opacity for better visibility
        layerId = ~id  # Ensure layerId is set for interactivity
      ) %>%
      # addLabelOnlyMarkers(
      #   lng = ~longitude[selected_id == id], 
      #   lat = ~latitude[selected_id == id],
      #   label = ~paste0(
      #     "<strong>Name: </strong>", name[selected_id == id],
      #     "<br><strong>", input$variable, ": </strong>", round(multi_annual_value[selected_id == id], 1)
      #   ) %>% lapply(htmltools::HTML),  # Ensure HTML format for label
      #   labelOptions = labelOptions(noHide = TRUE, direction = 'auto')
      # ) %>%
      clearControls() %>%
      addLegend(
        "bottomright",
        pal = color_pal,
        values = ~multi_annual_value,
        title = input$variable,
        opacity = 0.7,
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )
    
  })
  
   # Render the plot title dynamically
  output$plot_title <- renderText({
    
    paste(input$stationSelect, "station")
  })
  
  
  # Render the time series plot with Theil-Sen trend line and slope annotation
  output$time_series_plot <- renderPlotly({

    # Ensure that time series data is available
    # req(time_series_data())
    
    # Get the filtered time series data
    ts_data <- time_series_data()
    
    # Define the color based on the variable
    line_color <- if (input$variable == "PREC") {
      "blue"
    } else {
      "red"
    }
    
    # Define the y-axis label based on the variable
    y_axis_label <- switch(input$variable,
                           "PREC" = "mm",  # mm for precipitation
                           "Tavg" = "°C",  # °C for temperature
                           "Tmin" = "°C",  # °C for temperature
                           "Tmax" = "°C"   # °C for temperature
    )
    
    # Define the breaks for the x-axis
    x_lim <- range(ts_data$year)
    
    # Calculate Kendall's Tau and Theil-Sen slope
    kendall_test_result <- kendallTrendTest(ts_data$value ~ ts_data$year)
    theil_sen_slope <- kendall_test_result$estimate["slope"]
    p_value <-  kendall_test_result$p.value
    intercept <- mean(ts_data$value) - theil_sen_slope * mean(ts_data$year)
    trend_line <- intercept + theil_sen_slope * ts_data$year
    
    # Generate the plot with Theil-Sen trend line and slope annotation
    p <- ggplot(ts_data, aes(x = year, y = value)) +
      geom_line(color = line_color) +
      xlim(x_lim) +
      geom_line(aes(y = trend_line), color = "#808080") +  # Add Theil-Sen trend line
      labs(x = NULL, y = y_axis_label) +  # Remove title from here
      #scale_x_continuous(breaks = x_breaks) +  # Set x-axis breaks
      theme_minimal() +
      annotate("text", x = x_lim[1] + 24, y = max(ts_data$value) * 1.05,  # Adjust x and y for annotation positioning
               label = 
                 paste0("Theil-Sen slope: ", round(theil_sen_slope * 10, 3)," ", y_axis_label,"/decade  p.value:",round(p_value, 4)), 
               hjust = 0, vjust = 1, color = "black", size = 3, fontface = "italic")  # Add slope annotation
    
    # Convert the ggplot object to a Plotly object for interactivity
    ggplotly(p) %>%
      layout(autosize = TRUE, hovermode = "closest")
  })
  
  # Display map title
  output$map_title <- renderText({
    # Extract the variable name for display
    var_name <- switch(input$variable,
                       "PREC" = "Precipitation",
                       "Tavg" = "Average Temperature",
                       "Tmin" = "Minimum Temperature",
                       "Tmax" = "Maximum Temperature",
                       "Variable"  # Generic name for other types
    )
    
    # Extract the aggregation type
    agg_type <- input$aggregation
    
    # Extract the year range
    year_range <- paste(input$yearRange[1], "-", input$yearRange[2])
    
    # Initialize the title
    title_text <- paste(var_name, agg_type, "from", year_range)
    
    # If aggregation type is "Monthly", append the selected month
    if (agg_type == "Monthly") {
      month_name <- month.abb[input$month]  # Get month abbreviation
      title_text <- paste(var_name, agg_type, month_name, "from", year_range)
    }
    
    # If aggregation type is "Seasonal", append the selected season
    if (agg_type == "Seasonal") {
      title_text <- paste(var_name, agg_type, input$season, "from", year_range)
    }
    
    # Return the final title
    title_text
  })
  
})
