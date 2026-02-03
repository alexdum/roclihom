server <- function(input, output, session) {
  # Populate the selectInput with station names dynamically
  observe({
    updateSelectInput(session, "stationSelect", choices = sort(meta$name))
  })


  # Initialize a reactive value to store the clicked or selected station ID
  selected_station_id <- reactiveVal(NULL)

  # Reactive value to trigger station layer refresh after style change
  style_change_trigger <- reactiveVal(0)

  # Update the selected station ID based on the dropdown selection
  observeEvent(input$stationSelect, {
    print(paste("Dropdown Change - New Selection:", input$stationSelect))

    selected_station <- meta %>%
      filter(name == input$stationSelect) %>%
      pull(id)

    print(paste("Dropdown Change - Resolved ID:", selected_station))

    selected_station_id(selected_station)
  })

  # Listen for click events on the map markers
  observeEvent(input$map_feature_click, {
    clicked_data <- input$map_feature_click
    print("Feature Click Event:")
    print(str(clicked_data))

    # Check if the click was on the "stations" layer
    # Use isTRUE to handle NULLs safely, and check for "layer" or "layer_id"
    if (!is.null(clicked_data) && (isTRUE(clicked_data$layer_id == "stations") || isTRUE(clicked_data$layer == "stations"))) {
      # The ID is in the properties
      clicked_station <- clicked_data$properties$id

      if (!is.null(clicked_station)) {
        # Also update the dropdown to reflect the selected station
        selected_station_name <- meta %>%
          filter(id == clicked_station) %>%
          pull(name)

        print(paste("Map Click - Station ID:", clicked_station))
        print(paste("Map Click - Found Name:", selected_station_name))

        updateSelectInput(session, "stationSelect", selected = selected_station_name)
      }
    }
  })

  # Reactive expression to filter the combined data and calculate multi-annual means or sums
  filtered_data <- reactive({
    # Ensure a station ID is selected before filtering
    req(selected_station_id())

    # Add a progress bar
    withProgress(message = "Processing data...", value = 0, {
      # Step 1: Get user inputs and initialize data filtering
      incProgress(0.2, detail = "Setting up filters...") # Progress at 20%

      year_range <- input$yearRange # Get the selected year range
      agg_type <- input$aggregation # Get the selected aggregation type (Monthly, Seasonal, Annual)

      # Filter data based on user input (Common filters)
      data_filtered <- combined_data %>%
        filter(
          altitude >= input$altitudeRange[1], # Filter by altitude range
          altitude <= input$altitudeRange[2],
          variable == input$variable, # Filter by the selected variable
          year >= year_range[1], # Filter by the selected year range
          year <= year_range[2]
        )

      # Step 2: Apply aggregation based on agg_type
      incProgress(0.4, detail = paste("Applying", agg_type, "aggregation...")) # Progress at 40%

      if (agg_type == "Monthly") {
        # Compute multi-annual means for the selected month
        data_filtered <- data_filtered %>%
          filter(month == as.integer(input$month)) %>%
          group_by(id, name, latitude, longitude, altitude) %>%
          summarise(multi_annual_value = mean(value, na.rm = TRUE), .groups = "drop")
      } else if (agg_type == "Seasonal") {
        data_filtered <- data_filtered %>%
          add_seasonal_columns() %>%
          filter_seasonal_data(input$season) %>%
          aggregate_seasonal(input$variable) %>%
          group_by(id, name, latitude, longitude, altitude) %>%
          summarise(multi_annual_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
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
      incProgress(1, detail = "Finalizing...") # Progress at 100%

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
          filter(month == as.integer(input$month)) %>%
          dplyr::select(name, year, month, value) %>%
          arrange(year, month)
      )
    } else if (agg_type == "Seasonal") {
      # Use helper functions
      data_filtered <- data_filtered %>%
        add_seasonal_columns() %>%
        filter_seasonal_data(input$season) %>%
        aggregate_seasonal(input$variable) %>%
        mutate(value = round(value, 1)) %>%
        arrange(year)

      # print(summary(data_filtered))
      return(data_filtered)
    } else if (agg_type == "Annual") {
      return(
        data_filtered %>%
          group_by(name, year) %>%
          summarise(value = if (input$variable == "PREC") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE), .groups = "drop") %>%
          mutate(value = round(value, 1)) %>%
          arrange(year)
      )
    }
  })

  # Allow downloading the aggregated map data
  output$download_map_data <- downloadHandler(
    filename = function() {
      agg_label <- switch(input$aggregation,
        "Monthly" = sprintf("monthly_%02d", as.integer(input$month)),
        "Seasonal" = paste0("seasonal_", input$season),
        "Annual" = "annual"
      )
      paste0("roclihom_map_data_", tolower(input$variable), "_", agg_label, ".csv")
    },
    content = function(file) {
      map_data <- filtered_data()
      req(nrow(map_data) > 0)
      write.csv(map_data, file, row.names = FALSE)
    }
  )

  # Allow downloading the plotted time series as CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      station_slug <- if (is.null(input$stationSelect) || input$stationSelect == "") {
        "station"
      } else {
        gsub("[^A-Za-z0-9]+", "_", tolower(input$stationSelect))
      }

      agg_label <- switch(input$aggregation,
        "Monthly" = sprintf("monthly_%02d", as.integer(input$month)),
        "Seasonal" = paste0("seasonal_", input$season),
        "Annual" = "annual"
      )

      paste0("roclihom_", station_slug, "_", tolower(input$variable), "_", agg_label, ".csv")
    },
    content = function(file) {
      ts_data <- time_series_data()
      req(nrow(ts_data) > 0)

      export_df <- ts_data %>%
        mutate(
          station = input$stationSelect,
          variable = input$variable,
          aggregation = input$aggregation
        ) %>%
        relocate(station, variable, aggregation)

      write.csv(export_df, file, row.names = FALSE)
    }
  )

  output$map <- renderMaplibre({
    print("DEBUG: renderMaplibre called - Map is initializing/re-rendering")
    maplibre(
      style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json", # Base style (Positron Vector)
      center = c(25, 44),
      zoom = 6
    ) %>%
      add_navigation_control(show_compass = FALSE, visualize_pitch = FALSE, position = "top-left")
  })


  # Reactive value to trigger style updates
  style_change_trigger <- reactiveVal(0)

  # Flag to track if map has been initialized
  map_initialized <- reactiveVal(FALSE)

  # Initialize map bounds only once
  observe({
    req(!map_initialized())
    # Wait for map to be ready (zoom is reported)
    req(input$map_zoom)

    maplibre_proxy("map") %>%
      fit_bounds(
        c(map_bounds$lng_min, map_bounds$lat_min, map_bounds$lng_max, map_bounds$lat_max)
      )

    map_initialized(TRUE)
  })

  # Home Zoom Button Handler
  observeEvent(input$home_zoom, {
    req(map_bounds) # Ensure bounds are available
    maplibre_proxy("map") %>%
      fit_bounds(
        c(map_bounds$lng_min, map_bounds$lat_min, map_bounds$lng_max, map_bounds$lat_max),
        animate = TRUE
      )
  })

  # Observe changes and update markers accordingly
  # Also depends on input$basemap so stations are re-added after style change
  observe({
    req(filtered_data()) # Ensure that filtered_data is available

    # Add dependency on style_change_trigger to re-add layer after style change
    style_change_trigger()


    # Retrieve the current selected station ID
    selected_id <- selected_station_id()

    # Prepare data for mapgl
    # We need to add styling properties directly to the data frame for data-driven styling if strict interpolations are hard
    # For mapgl, we can use expressions, but computing colors in R is often simpler for dynamic palettes

    map_data <- filtered_data()

    # Define color palettes
    color_pal2 <- get_color_palette(input$variable, domain = map_data$multi_annual_value, reverse = TRUE)

    # Add styling columns
    map_data <- map_data %>%
      mutate(
        circle_color = color_pal2(multi_annual_value),
        circle_radius = ifelse(id == selected_id, 8, 5),
        circle_stroke_color = ifelse(id == selected_id, "#FF0000", "#00000000"),
        circle_stroke_width = ifelse(id == selected_id, 2, 1),
        # Sort to ensure selected is on top (if needed, though circle_sort_key might be used if fully supported, otherwise robust ordering in data usually works)
        is_selected = ifelse(id == selected_id, 1, 0),
        popup_content = paste0(
          "<strong>Name: </strong>", name,
          "<br><strong>", input$variable, ": </strong>", round(multi_annual_value, 1),
          "<br><span style='color:red;'>click to update</span>"
        )
      ) %>%
      arrange(is_selected) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    # Apply to map
    maplibre_proxy("map") %>%
      clear_layer("stations") %>% # Remove existing layer if any
      add_circle_layer(
        id = "stations",
        source = map_data,
        circle_color = get_column("circle_color"),
        circle_radius = get_column("circle_radius"),
        circle_stroke_color = get_column("circle_stroke_color"),
        circle_stroke_width = get_column("circle_stroke_width"),
        circle_opacity = 0.9,
        # Create a tooltip for hover
        tooltip = get_column("popup_content"),
        before_id = stations_before_id()
      )
  })

  # Track the layer ID that stations should be drawn BEFORE
  stations_before_id <- reactiveVal(NULL)

  # Track IDs of dynamically added raster layers for explicit removal
  current_raster_layers <- reactiveVal(character(0))

  # Observe changes in the basemap selection and update the map style
  observeEvent(input$basemap, {
    print(paste("Basemap Change - Selection:", input$basemap))

    proxy <- maplibre_proxy("map")

    # Explicitly remove any previously added raster layers
    old_layers <- isolate(current_raster_layers())
    if (length(old_layers) > 0) {
      for (layer_id in old_layers) {
        proxy %>% clear_layer(layer_id)
      }
      current_raster_layers(character(0)) # Reset
    }

    if (input$basemap %in% c("carto_positron", "carto_voyager", "esri_imagery", "mapbox_satellite")) {
      # VECTOR LOGIC (Carto-based styles + Mapbox)
      # For esri_imagery, we use Voyager style but insert satellite raster below labels
      print("Setting Vector Style for Carto...")

      style_url <- switch(input$basemap,
        "carto_positron" = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
        "carto_voyager" = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
        "esri_imagery" = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json", # Use Voyager for labels
        "mapbox_satellite" = paste0("https://api.mapbox.com/styles/v1/mapbox/satellite-streets-v12?access_token=", mapbox_token)
      )

      proxy %>%
        set_style(style_url)

      # For vector sandwich, we want stations below labels.
      stations_before_id("watername_ocean")

      # For Esri Imagery: Insert satellite raster layer below the vector style's features
      if (input$basemap == "esri_imagery") {
        session <- shiny::getDefaultReactiveDomain()
        selected_basemap <- input$basemap

        later::later(function() {
          shiny::withReactiveDomain(session, {
            # Race condition check
            current_basemap <- isolate(input$basemap)
            if (current_basemap != selected_basemap) {
              return()
            }

            unique_suffix <- as.numeric(Sys.time()) * 1000
            source_id <- paste0("esri_imagery_source_", unique_suffix)
            layer_id <- paste0("esri_imagery_layer_", unique_suffix)

            esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"

            # Insert raster layer BELOW labels but ABOVE water polygons
            # "watername_ocean" is the first label layer, so raster covers water but labels show on top
            maplibre_proxy("map") %>%
              add_raster_source(id = source_id, tiles = c(esri_url), tileSize = 256) %>%
              add_layer(
                id = layer_id,
                type = "raster",
                source = source_id,
                paint = list("raster-opacity" = 1),
                before_id = "watername_ocean" # Insert just below labels
              )

            current_raster_layers(c(layer_id))

            # Trigger station re-render
            style_change_trigger(isolate(style_change_trigger()) + 1)
          })
        }, delay = 0.5)
      } else {
        # Pure vector (Positron/Voyager) - trigger immediately
        style_change_trigger(isolate(style_change_trigger()) + 1)
      }
    } else {
      # RASTER LOGIC (Esri Topo, OSM)
      # These use native labels baked into the tiles

      tile_url <- if (input$basemap %in% c("osm", "osm_gray")) {
        "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
      } else {
        # Esri Topo (esri_topo)
        "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"
      }

      attribution_text <- if (input$basemap %in% c("osm", "osm_gray")) {
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      } else {
        "Tiles &copy; Esri"
      }

      # Determine paint properties (Saturation -1 for Gray style)
      paint_props <- list("raster-opacity" = 1)
      if (input$basemap == "osm_gray") {
        paint_props[["raster-saturation"]] <- -0.9 # Muted but not B&W
        paint_props[["raster-contrast"]] <- 0.3 # Add contrast
      }

      # Use blank style + raster layer
      blank_style <- list(
        version = 8,
        sources = list(),
        layers = list(),
        metadata = list(timestamp = as.numeric(Sys.time()))
      )
      json_blank <- jsonlite::toJSON(blank_style, auto_unbox = TRUE)
      blank_uri <- paste0("data:application/json,", URLencode(as.character(json_blank), reserved = TRUE))

      proxy %>%
        set_style(blank_uri)

      # Capture session and current selection for later callback
      session <- shiny::getDefaultReactiveDomain()
      selected_basemap <- input$basemap

      # Add Raster layer after style loads
      later::later(function() {
        shiny::withReactiveDomain(session, {
          # RACE CONDITION CHECK
          current_basemap <- isolate(input$basemap)
          if (current_basemap != selected_basemap) {
            print(paste("Basemap changed during delay - aborting."))
            return()
          }

          unique_suffix <- as.numeric(Sys.time()) * 1000
          source_id <- paste0("raster_source_", unique_suffix)
          layer_id <- paste0("raster_layer_", unique_suffix)

          maplibre_proxy("map") %>%
            add_raster_source(id = source_id, tiles = c(tile_url), tileSize = 256, attribution = attribution_text) %>%
            add_layer(
              id = layer_id,
              type = "raster",
              source = source_id,
              paint = paint_props
            )

          # For Raster maps, stations render ON TOP of native labels
          stations_before_id(NULL)
          current_raster_layers(c(layer_id))

          # Trigger station re-render
          style_change_trigger(isolate(style_change_trigger()) + 1)
        })
      }, delay = 0.5)
    }
  })

  # Toggle Labels visibility
  observeEvent(input$show_labels,
    {
      visibility <- if (input$show_labels) "visible" else "none"

      # List of actual Carto label layer IDs (from positron/voyager style.json)
      label_layers <- c(
        # Place labels
        "place_villages", "place_town", "place_country_2", "place_country_1",
        "place_state", "place_continent",
        "place_city_r6", "place_city_r5", "place_city_dot_r7", "place_city_dot_r4",
        "place_city_dot_r2", "place_city_dot_z7",
        "place_capital_dot_z7", "place_capital",
        # Road labels
        "roadname_minor", "roadname_sec", "roadname_pri", "roadname_major",
        "motorway_name",
        # Water labels
        "watername_ocean", "watername_sea", "watername_lake", "watername_lake_line",
        # POI labels
        "poi_stadium", "poi_park", "poi_zoo",
        # Airport
        "airport_label",

        # Mapbox Standard Label Layers (Satellite Streets v12)
        "country-label", "state-label", "settlement-major-label", "settlement-minor-label",
        "settlement-subdivision-label", "road-label", "waterway-label", "natural-point-label",
        "poi-label", "airport-label"
      )


      print(paste("Labels toggle - visibility:", visibility))
      proxy <- maplibre_proxy("map")

      for (layer_id in label_layers) {
        tryCatch(
          {
            proxy <- proxy %>% set_layout_property(layer_id, "visibility", visibility)
          },
          error = function(e) {
            # Layer may not exist in current style, ignore silently
          }
        )
      }
    },
    ignoreInit = TRUE
  )

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
    line_color <- if_else(input$variable == "PREC", "blue", "red")

    # Define the y-axis label based on the variable
    y_axis_label <- switch(input$variable,
      "PREC" = "mm", # mm for precipitation
      "Tavg" = "°C", # °C for temperature
      "Tmin" = "°C", # °C for temperature
      "Tmax" = "°C" # °C for temperature
    )

    # Define the breaks for the x-axis
    x_lim <- range(ts_data$year)

    # Calculate Kendall's Tau and Theil-Sen slope
    kendall_test_result <- kendallTrendTest(ts_data$value ~ ts_data$year)
    theil_sen_slope <- kendall_test_result$estimate["slope"]
    p_value <- kendall_test_result$p.value
    intercept <- mean(ts_data$value) - theil_sen_slope * mean(ts_data$year)
    trend_line <- intercept + theil_sen_slope * ts_data$year

    # Generate the plot with Theil-Sen trend line and slope annotation
    p <- ggplot(ts_data, aes(x = year, y = value)) +
      geom_line(color = line_color) +
      xlim(x_lim) +
      geom_line(aes(y = trend_line), color = "#808080") + # Add Theil-Sen trend line
      labs(x = NULL, y = y_axis_label) + # Remove title from here
      # scale_x_continuous(breaks = x_breaks) +  # Set x-axis breaks
      theme_minimal() +
      annotate("text",
        x = x_lim[1] + 24, y = max(ts_data$value) * 1.05, # Adjust x and y for annotation positioning
        label =
          paste0("Theil-Sen slope: ", round(theil_sen_slope * 10, 3), " ", y_axis_label, "/decade  p.value:", round(p_value, 4)),
        hjust = 0, vjust = 1, color = "black", size = 3, fontface = "italic"
      ) # Add slope annotation

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
      "Variable" # Generic name for other types
    )

    # Extract the aggregation type
    agg_type <- input$aggregation

    # Extract the year range
    year_range <- paste(input$yearRange[1], "-", input$yearRange[2])

    # Initialize the title
    title_text <- paste(var_name, tolower(agg_type), "from", year_range)

    # If aggregation type is "Monthly", append the selected month
    if (agg_type == "Monthly") {
      month_name <- month.abb[as.integer(input$month)] # Get month abbreviation
      title_text <- paste(var_name, tolower(agg_type), month_name, "from", year_range)
    }

    # If aggregation type is "Seasonal", append the selected season
    if (agg_type == "Seasonal") {
      title_text <- paste(var_name, tolower(agg_type), input$season, "from", year_range)
    }

    # Return the final title
    title_text
  })
}
