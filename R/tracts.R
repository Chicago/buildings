tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON",
                  stringsAsFactors = FALSE)
tracts <- spTransform(tracts, CRS("+init=epsg:4326"))
