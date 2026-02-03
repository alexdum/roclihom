page_navbar(
  title = "RoCliHom Explorer",
  theme = bs_theme(version = 5),
  navbar_options = navbar_options(collapsible = TRUE),

  # Add the canonical link and noindex meta tag inside the head tag
  header = tags$head(
    tags$link(rel = "canonical", href = "https://climate-insights.netlify.app/roclihom"),
    tags$script(src = "js/fullscreen.js"),
    # Auto-collapse navbar on mobile when a nav item is selected
    tags$script(HTML("
      $(document).ready(function() {
        $('.navbar-nav .nav-link').on('click', function() {
          var navbarCollapse = $('.navbar-collapse');
          if (navbarCollapse.hasClass('show')) {
            navbarCollapse.collapse('hide');
          }
        });
      });
    ")),
    tags$style(HTML("
      /* Collapsible Map Control Styling */
      .map-layer-control {
        background-color: white;
        border-radius: 4px;
        box-shadow: 0 0 5px rgba(0,0,0,0.3);
        padding: 5px;
        width: 36px;
        height: 36px;
        overflow: hidden;
        transition: width 0.3s ease, height 0.3s ease;
        cursor: pointer;
        display: flex;
        flex-direction: column;
      }
      .map-layer-control:hover {
        width: 200px;
        height: auto;
        padding: 10px;
      }
      .control-icon {
        width: 26px;
        height: 26px;
        text-align: center;
        margin-bottom: 5px;
        font-size: 18px;
        color: #333;
        flex-shrink: 0;
      }
      .control-content {
        opacity: 0;
        transition: opacity 0.3s ease;
        margin-top: 5px;
        white-space: nowrap; /* Prevent wrapping during transition */
      }
      .map-layer-control:hover .control-content {
        opacity: 1;
        white-space: normal;
      }
      .map-layer-control:hover .control-icon {
        display: none !important;
      }
    "))
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


        # Select input for the month (only shown for monthly aggregation)
        conditionalPanel(
          condition = "input.aggregation == 'Monthly'",
          selectInput(
            inputId = "month",
            label = "Month:",
            choices = setNames(1:12, month.name),
            selected = 1
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
        ),

        # Download button for Map Data (Aggregated Spatial Data)
        hr(),
        downloadButton("download_map_data", "Download Map Data", class = "btn btn-secondary btn-sm w-100")
      ),
      card(
        full_screen = TRUE,
        card_header(h6(textOutput("map_title"))),
        div(
          style = "position: relative;",
          maplibreOutput("map", height = "400px"),
          absolutePanel(
            top = 130, right = 10,
            class = "map-layer-control",
            style = "z-index: 1000;",
            div(class = "control-icon", icon("layer-group")),
            div(
              class = "control-content",
              radioButtons(
                inputId = "basemap",
                label = "Basemap",
                choices = c(
                  "Carto Positron" = "carto_positron",
                  "Carto Dark Matter" = "carto_dark_matter",
                  "Carto Voyager" = "carto_voyager",
                  "Esri Imagery" = "esri_imagery"
                ),
                selected = "carto_positron"
              )
            )
          ),
          absolutePanel(
            top = 80, right = 10,
            style = "z-index: 1000;",
            actionButton(
              inputId = "home_zoom",
              label = NULL,
              icon = icon("house"),
              style = "background-color: white; border: none; border-radius: 4px; box-shadow: 0 0 5px rgba(0,0,0,0.3); width: 36px; height: 36px; padding: 0; color: #333;"
            )
          )
        ),
        div(
          class = "d-flex justify-content-between align-items-center gap-2 flex-wrap",
          h6(textOutput("plot_title"), class = "mb-0"),
          downloadButton("download_csv", "Download CSV", class = "btn btn-primary btn-sm")
        ),
        plotlyOutput("time_series_plot", height = "250px")
      )
    )
  ),

  # Second tab for Info
  nav_panel(
    "Info",
    card(
      includeMarkdown("www/md/roclihom_info.md") # Load external HTML file
    )
  ),
  nav_spacer(),
  nav_item(
    tooltip(
      tags$a(
        id = "fullscreen_toggle",
        href = "#",
        onclick = "toggleFullScreen(); return false;",
        style = "display: flex; align-items: center; color: rgba(0,0,0,0.55); padding: 0.5rem 1rem; text-decoration: none;",
        bsicons::bs_icon("arrows-fullscreen", size = "1.2rem"),
        span("Fullscreen", style = "margin-left: 5px; display: none;")
      ),
      "Toggle Fullscreen",
      placement = "bottom"
    )
  )
)
