page_navbar(
  theme = bs_theme(version = 5),
  navbar_options = navbar_options(collapsible = TRUE),

  # Add the canonical link and noindex meta tag inside the head tag
  header = tags$head(
    tags$link(rel = "canonical", href = "https://climate-insights.netlify.app/roclihom")
  ),
  fillable_mobile = T,

  # First tab for Data Explorer
  nav_panel(
    "Data Explorer",
    layout_sidebar(
      sidebar = sidebar(
        open = list(desktop = "open", mobile = "always-above"),
        # Dropdown to select station by name
        selectInput(
          inputId = "stationSelect",
          label = "Select Station by Name:",
          choices = NULL, # Choices will be populated dynamically
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
        full_screen = TRUE,
        card_header(h6(textOutput("map_title"))),
        leafletOutput("map", height = "300px"),
        div(
          class = "d-flex justify-content-between align-items-center gap-2 flex-wrap",
          h6(textOutput("plot_title"), class = "mb-0"),
          downloadButton("download_csv", "Download CSV", class = "btn btn-primary btn-sm")
        ),
        plotlyOutput("time_series_plot", height = "150px")
      )
    )
  ),

  # Second tab for Info
  nav_panel(
    "Info",
    card(
      includeMarkdown("www/md/roclihom_info.md") # Load external HTML file
    )
  )
)
