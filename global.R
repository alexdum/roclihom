library(shiny)
library(leaflet)
library(leaflet.extras)
library(arrow)
library(dplyr)


# Load the meta data
meta <- read_parquet("www/data/tabs/roclihom_meta_1901_2023.parquet")

# Load the additional data
data <- read_parquet("www/data/tabs/roclihom_data_1901_2023.parquet")