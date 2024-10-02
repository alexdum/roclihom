library(shiny)
library(bslib)
library(leaflet)

# Define the UI using bslib for a modern layout
shinyUI(fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Interactive Map with Altitude and Data Filters"),
  
  sidebarLayout(
    sidebarPanel(
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
      numericInput(
        inputId = "year",
        label = "Year:",
        value = 1901,
        min = 1901,
        max = 2023,
        step = 1
      ),
      numericInput(
        inputId = "month",
        label = "Month:",
        value = 1,
        min = 1,
        max = 12,
        step = 1
      )
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
))
