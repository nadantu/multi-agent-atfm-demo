library(leaflet)
library(sf)
library(jsonlite)
library(readr)

generate_map <- function(geojson_file, csv_file) {
  # Read the GeoJSON data
  geojson_data <- fromJSON(geojson_file)
  
  # Read the capacity data from CSV
  capacity_data <- read_csv(csv_file)
  
  # Create a map object, with initial location set to the Asia-Pacific region
  m <- leaflet() %>%
    setView(lng = 120, lat = 5, zoom = 5) %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron")
  
  # Iterate over GeoJSON features
  for (feature in geojson_data$features) {
    index <- feature$properties$index
    
    # Generate content for the popup
    iframe_content <- sector_data(index, capacity_data)
    
    # Add GeoJSON layer to the map
    m <- addGeoJSON(
      m,
      data = feature,
      stroke = TRUE,
      color = get_color(index, capacity_data),
      fillOpacity = 0.3,
      opacity = 0.4,
      highlight = highlightOptions(
        color = get_color(index, capacity_data),
        fillOpacity = 0.7,
        opacity = 0.8
      ),
      popup = paste0('<iframe style="width:300px; height:150px;" src="data:text/html;base64,', htmltools::htmlEscape(b64encode(iframe_content)), '"></iframe>')
    )
    
    cat(paste("Processed", index, get_color(index, capacity_data), "\n"))
  }
  
  # Save the map as an HTML file
  #saveWidget(m, "map.html", selfcontained = TRUE)
}

generate_map
