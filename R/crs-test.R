library(rgdal)

# ogrInfo(dsn = "data/tracts.geojson")

# Source: "data/tracts.geojson", layer: "OGRGeoJSON"
# Driver: GeoJSON; number of rows: 801 
# Feature type: wkbPolygon with 2 dimensions
# Extent: (-87.94025 41.64429) - (-87.52366 42.02392)
# CRS: +proj=longlat +datum=WGS84 +no_defs  

# ogrInfo(dsn = "data/Buildings.geojson")

# Source: "data/Buildings.geojson", layer: "OGRGeoJSON"
# Driver: GeoJSON; number of rows: 820606 
# Feature type: wkbPolygon with 2 dimensions
# Extent: (-87.93981 41.6446) - (-87.52421 42.023)
# Null geometry IDs: 406569, 406570, 409030, 409031, 511591, 820219 
# CRS: +proj=longlat +datum=WGS84 +no_defs 

# ogrInfo(dsn = "data/parcels.geojson")

# Source: "data/parcels.geojson", layer: "OGRGeoJSON"
# Driver: GeoJSON; number of rows: 1414707 
# Feature type: wkbPolygon with 2 dimensions
# Extent: (-88.26363 41.46971) - (-87.52453 42.15429)
# CRS: +proj=longlat +datum=WGS84 +no_defs 

tracts <- readOGR("data/tracts.geojson", stringsAsFactors = FALSE)
buildings <- readOGR("data/buildings.geojson", stringsAsFactors = FALSE)
taxShapes <- readOGR("data/parcels.geojson", stringsAsFactors = FALSE)

proj4string(tracts)
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(buildings)
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(taxShapes)
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

popupCoords <- violations$address
popup <- paste0(buildings$f_add1, "-", buildings$t_add1, " ", buildings$pre_dir1,
                " ", buildings$st_name1, " ", buildings$st_type1)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = buildings, weight = 1, popup = popup) %>%
  addCircles(data = violations, radius = 3, stroke = FALSE, fillOpacity = 1, fillColor = '#ff0000', popup = popupCoords)
