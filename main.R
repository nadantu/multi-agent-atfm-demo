library(leaflet)
library(sf)

source("map.R")


sector_geo_file = "data/sectors.geojson"
sector_info_file = "data/mockup_capacity.csv"

# Read GeoJSON file
geojson_data <- st_read(sector_geo_file)


#plot_sector(geojson_file = sector_geo_file)


plot_geographical_map(geojson_file = sector_geo_file, csv_file = sector_info_file, geo_map=TRUE)
