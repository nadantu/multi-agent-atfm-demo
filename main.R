library(leaflet)
library(sf)
------------------------------------
source("map.R")
------------------------------------

# Read GeoJSON file
geojson_data <- st_read("data/sectors.geojson")
plot(geojson_data)


# Create Leaflet map
m <- leaflet() %>%
    setView(lng = 115, lat = 8, zoom = 4) %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron")

# for (sector in geojson_data$geometry) {
#     sector
#     m <- addGeoJSON(
#         m,
#         geojson = sector,
#         stroke = TRUE,
#         color = "green",
#         fillOpacity = 0.7,
#         opacity = 0.6
#         )
#}

plot_map(geojson_file = "data/sectors.geojson")
