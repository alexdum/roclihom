library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(leaflet.extras)
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
  lng_max = max(meta$longitude + 1, na.rm = TRUE),
  lat_min = min(meta$latitude - 1, na.rm = TRUE),
  lat_max = max(meta$latitude + 1, na.rm = TRUE)
)
