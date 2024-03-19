library(leaflet)
library(geojsonio)
library(sf)
library(jsonlite)
library(readr)

source("sector.R")

sector_geo_file = "data/sectors.geojson"
sector_info_file = "data/mockup_capacity.csv"

# Read GeoJSON file
geojson_data = geojson_read(sector_geo_file)
sector_info_data = read.csv(sector_info_file)


plot_geographical_map <- function(sector_data = geojson_data, 
                                  dcb_data = sector_info_data,
                                  volume = 'current',
                                  routing = 'ats',
                                  display_geo_map = TRUE,
                                  time_step = 0) {
  
  # Create a map object, with initial location set to the Asia-Pacific region
  m = leaflet() %>%
      setView(lng = 115, lat = 8, zoom = 4)
  
  if (display_geo_map == TRUE) {
    m = addTiles(m) %>% 
      addProviderTiles("CartoDB.Positron")
  }
  
  #TODO: implement the logic to get the color list based time_step and scenarios from the DCB data
  
  color_list = get_color_list(dcb_data)
  
  # Loop through the list of objects to plot sectors one by one
  color_index = 1
  for (sector in sector_data$features) {
    
    m = addGeoJSON(
      m,
      geojson = sector,
      stroke = TRUE,
      weight = 3,
      color = color_list[[color_index]],
      fillOpacity = 0.3,
      opacity = 0.2,
      data = sector,
      
    )
    color_index = color_index + 1
  }
  # return(m)
  m
}

plot_sector <- function(geosjon_data = geojson_data) {
  plot(geojson_data)
}

# highlightOptions(stroke = TRUE, 
#                  color = color,
#                  fillOpacity = 0.6,
#                  opacity = 0.4,
#                  weight = 2, 
#                  bringToFront = TRUE)
