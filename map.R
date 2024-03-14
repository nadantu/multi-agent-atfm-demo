library(leaflet)
library(sf)
library(jsonlite)
library(readr)

plot_geographical_map <- function(geojson_file, csv_file) {
  
  geojson_data <- st_read(geojson_file)
  #capacity_data <- read_csv(csv_file)
  
  # Create a map object, with initial location set to the Asia-Pacific region
  m <- leaflet() %>%
    setView(lng = 115, lat = 8, zoom = 4) %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron")
  
}

plot_map <- function(geojson_file) {
  geojson_data <- st_read(geojson_file)
  plot(geojson_data)
}

