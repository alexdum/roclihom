library(shiny)
library(bslib)
library(leaflet)

# Define the UI using bslib for a modern layout
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
  card(
    card_header(
      h5(textOutput("station_name_output"))  # Display selected station details
    ), 
    leafletOutput("map", height = 300),
    plotOutput("time_series_plot", height = 150)  # Add time series plot below the map
  )
)