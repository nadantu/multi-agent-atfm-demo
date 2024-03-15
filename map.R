library(leaflet)
library(geojsonio)
library(sf)
library(jsonlite)
library(readr)

source("sector.R")

plot_geographical_map <- function(geojson_file, csv_file, geo_map) {
  
  geojson_data = geojson_read(geojson_file)
  capacity_data = read_csv(csv_file)
  color_list = get_color_list(capacity_data)
  
  # Create a map object, with initial location set to the Asia-Pacific region
  m = leaflet() %>%
      setView(lng = 115, lat = 8, zoom = 4)
  
  if (geo_map == TRUE) {
    m = addTiles(m) %>% 
      addProviderTiles("CartoDB.Positron")
  }
  
  # Loop through the list of objects to plot sectors one by one
  color_index = 1
  for (sector in geojson_data$features) {
    
    m = addGeoJSON(
      m,
      geojson = sector,
      stroke = TRUE,
      weight = 3,
      color = color_list[[color_index]],
      fillOpacity = 0.3,
      opacity = 0.2,
      data = sector
    )
    color_index = color_index + 1
  }
  # return(m)
  m
}

plot_sector <- function(geojson_file) {
  geojson_data = st_read(geojson_file)
  plot(geojson_data)
}

?sf::plot

# highlightOptions(stroke = TRUE, 
#                  color = color,
#                  fillOpacity = 0.6,
#                  opacity = 0.4,
#                  weight = 2, 
#                  bringToFront = TRUE)
