library(shiny)
library(bslib)
library(leaflet)

# Define the UI using bslib for a modern layout
shinyUI(fluidPage(
  
  # Use bslib for a modern Bootstrap 5 theme
  theme = bs_theme(version = 5, bootswatch = "flatly"),  # You can choose different bootswatch themes
  
  # Title of the application
  titlePanel("Interactive Map with Altitude Filter"),
  
  # Sidebar layout with a slider input for altitude range
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      # Slider input for filtering by altitude (initialized without min/max)
      sliderInput(
        inputId = "altitudeRange",
        label = "Altitude Range (m):",
        min = 0, max = 500, 
        value = c(0, 500),  # Default range, will be updated dynamically
        step = 1
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Leaflet output to display the map
      leafletOutput("map", height = 600)
    )
  )
))