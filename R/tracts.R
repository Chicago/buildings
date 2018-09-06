tracts <- readOGR(dsn="data/geojson/tracts.geojson", layer="tracts",
                  stringsAsFactors = FALSE)
tracts <- spTransform(tracts, CRS("+init=epsg:4326"))
