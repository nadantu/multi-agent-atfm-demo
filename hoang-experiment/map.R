library(leaflet)
library(geojsonio)
library(sf)
library(jsonlite)
library(readr)

source("sector.R")

plot_geographical_map <- function(geojson_file, csv_file) {
  
  geojson_data = geojson_read(geojson_file)
  capacity_data = read_csv(csv_file)
  
  # Get color list for coloring sectors
  index = 1
  color_list = c()
  for (sector in capacity_data$name) {
    color = get_sector_color(sector, capacity_data)
    color_list[[index]] = color
    index = index + 1
  }
  
  # Create a map object, with initial location set to the Asia-Pacific region
  m = leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 115, lat = 8, zoom = 4)
  
  # Loop through the list of objects to plot sectors one by one
  color_index = 1
  for (sector in geojson_data$features) {
    
    color = color_list[[color_index]]
    color_index = color_index + 1
    
    m = addGeoJSON(
      m,
      geojson = sector,
      stroke = TRUE,
      color = color,
      fillOpacity = 0.3,
      opacity = 0.4,
      data = sector
    )
  }
  
  m
}

plot_sector <- function(geojson_file) {
  geojson_data = st_read(geojson_file)
  plot(geojson_data)
}

