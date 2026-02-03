library(shiny)
library(bslib)
library(bsicons)
library(mapgl)
library(sf)
library(jsonlite)
library(arrow)
library(dplyr)
library(ggplot2)
library(plotly)
library(EnvStats)
library(tidyr)
library(seas)
library(markdown)
source("utils/get_color_palette.R", local = T)
source("utils/data_processing.R", local = T)

# Load the meta data
meta <- read_parquet("www/data/tabs/roclihom_meta_1901_2023.parquet")

# Load the additional data
data <- read_parquet("www/data/tabs/roclihom_data_1901_2023.parquet")

# Join the data with meta based on id
combined_data <- data %>% left_join(meta, by = "id")

# Calculate the bounds of the data
map_bounds <- list(
  lng_min = min(meta$longitude - 1, na.rm = TRUE),
  lat_min = min(meta$latitude - 1, na.rm = TRUE),
  lng_max = max(meta$longitude + 1, na.rm = TRUE),
  lat_max = max(meta$latitude + 1, na.rm = TRUE)
)

# Define custom raster styles for OSM and Esri
osm_style <- list(
  version = 8,
  sources = list(
    osm = list(
      type = "raster",
      tiles = list("https://a.tile.openstreetmap.org/{z}/{x}/{y}.png"),
      tileSize = 256,
      attribution = "&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors"
    )
  ),
  layers = list(
    list(
      id = "osm",
      type = "raster",
      source = "osm"
    )
  )
)

esri_style <- list(
  version = 8,
  sources = list(
    esri = list(
      type = "raster",
      tiles = list("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"),
      tileSize = 256,
      attribution = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
    )
  ),
  layers = list(
    list(
      id = "esri",
      type = "raster",
      source = "esri"
    )
  )
)
