# ---------------------------------------------------------------------------- #
#  Import all Cook County tax parcel data
# ---------------------------------------------------------------------------- #

# taxShapes <- readOGR(dsn="data/parcels.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------- #
#  DRILL DOWN TO A FEW CENSUS TRACTS FOR FASTER COMPUTATION 
# ---------------------------------------------------------------------------- #

# tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# # tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432",
# #                 "2433")
# tractsList <- c("2431")
# tracts <- tracts[tracts$name10 %in% tractsList,]
# taxShapes_df <- over(taxShapes, tracts)
# taxShapes_df <- taxShapes_df[complete.cases(taxShapes_df),]
# rowNums <- as.numeric(rownames(taxShapes_df))
# taxShapes <- taxShapes[c(rowNums),]
# writeOGR(taxShapes, "data/taxShapes-small.geojson",layer="OGRGeoJSON",
#          driver="GeoJSON")

# ---------------------------------------------------------------------------- #
#  Read small geojson file
# ---------------------------------------------------------------------------- #

taxShapes <- readOGR("data/taxShapes-small.geojson", layer="OGRGeoJSON",
                     stringsAsFactors = FALSE)
taxShapes <- spTransform(taxShapes, CRS("+init=epsg:4326"))

