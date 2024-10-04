page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  
  sidebar = sidebar(
    # Dropdown to select station by name
    selectInput(
      inputId = "stationSelect",
      label = "Select Station by Name:",
      choices = NULL,  # Choices will be populated dynamically
      selected = NULL
    ),
    
    sliderInput(
      inputId = "altitudeRange",
      label = "Altitude Range (m):",
      min = 0, max = 2504, 
      value = c(0, 2504),
      step = 1
    ),
    
    selectInput(
      inputId = "variable",
      label = "Variable:",
      choices = c("PREC", "Tavg", "Tmin", "Tmax"),
      selected = "PREC"
    ),
    
    # Add a slider input to select the year range
    sliderInput(
      inputId = "yearRange",
      label = "Year Range:",
      min = 1901, max = 2023,
      value = c(1901, 2023),
      step = 1
    ),
    
    # Add a selectInput to choose the aggregation level (Monthly, Seasonal, Annual)
    selectInput(
      inputId = "aggregation",
      label = "Aggregation:",
      choices = c("Monthly", "Seasonal", "Annual"),
      selected = "Monthly"
    ),
    
    # Numeric input for the month (only shown for monthly aggregation)
    conditionalPanel(
      condition = "input.aggregation == 'Monthly'",
      numericInput(
        inputId = "month",
        label = "Month:",
        value = 1,
        min = 1,
        max = 12,
        step = 1
      )
    ),
    
    # Select input for selecting season (only shown for seasonal aggregation)
    conditionalPanel(
      condition = "input.aggregation == 'Seasonal'",
      selectInput(
        inputId = "season",
        label = "Season:",
        choices = c("DJF", "MAM", "JJA", "SON"),
        selected = "DJF"
      )
    )
  ),
  
  # Adding the tabs
  navset_card_underline(
    id = "tabs",
    
    # First tab for Data Explorer
    nav_panel(
      "Data Explorer",
      card(
        full_screen = T,
        h5(textOutput("map_title")), 
        leafletOutput("map", height = 300),
        plotlyOutput("time_series_plot", height = "150px") # Add time series plot below the map
      )
    ),
    
    # Second tab for Info
    nav_panel(
      "Info",
      card(
        h5("Information"),   # Title for the Info card
        
        p("The primary input data consists of daily data extracted from the database of the National
Meteorological Administration of Romania."),
        p("For 1901–1960, the daily Tavg was derived as the
average of three climatological observations taken at 06, 12, and 18 UTC, then a Köppen
coefficient was applied, calculated based on the Tmin and month of the year. For 1961-2023,
the daily Tavg was calculated as the arithmetic average of four climatological observations
recorded at 00, 06, 12, and 18 UTC."),
p("For the entire period of analysis, daily PREC was the total accumulation from 18 UTC on the
previous day to 18 UTC on the current day, with the timestamp corresponding to the end of
the 24-hour accumulation period."),
p("The quality control, gap-filling, and homogenization were performed using the methodology implemented in Climatol.")
        # Add more content as needed for the Info tab
      )
    )
  )
)