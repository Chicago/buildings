# ---------------------------------------------------------------------------- #
#  Import all Chicago building data
# ---------------------------------------------------------------------------- #

# buildings <- readOGR(dsn="data/Buildings.geojson", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------- #
#  DRILL DOWN TO A FEW CENSUS TRACTS FOR FASTER COMPUTATION 
# ---------------------------------------------------------------------------- #

# tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON",
#                   stringsAsFactors = FALSE)
# # tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432",
# #                 "2433")
# tractsList <- c("2431")
# tracts <- tracts[tracts$name10 %in% tractsList,]
# buildings_df <- over(buildings, tracts)
# buildings_df <- buildings_df[complete.cases(buildings_df),]
# rowNums <- as.numeric(rownames(buildings_df))
# buildings <- buildings[c(rowNums),]
# writeOGR(buildings, "data/buildings-small.geojson",layer="OGRGeoJSON",
#          driver="GeoJSON")

# ---------------------------------------------------------------------------- #
#  Read small geojson file
# ---------------------------------------------------------------------------- #

buildings <- readOGR("data/buildings-small.geojson", layer="OGRGeoJSON",
                     stringsAsFactors = FALSE)
buildings <- spTransform(buildings, CRS("+init=epsg:4326"))
