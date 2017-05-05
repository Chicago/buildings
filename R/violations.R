# ---------------------------------------------------------------------------- #
#  Import all building violations
# ---------------------------------------------------------------------------- #

# violations <-
#   read.socrata("https://data.cityofchicago.org/resource/ucdv-yd74.json")

# ---------------------------------------------------------------------------- #
#  CONVERT VIOLATIONS TO SPATIAL DATA FRAME
# ---------------------------------------------------------------------------- #

# violations$longitude <- as.numeric(violations$longitude)
# violations$latitude <- as.numeric(violations$latitude)
# violations <- violations[complete.cases(violations[c("address","longitude",
#                                                      "latitude")]),]
# rownames(violations) <- seq(length=nrow(violations))
# violations <- SpatialPointsDataFrame(coords =
#                                        SpatialPoints(violations[,c("longitude",
#                                                                    "latitude")]),
#                                      data = as.data.frame(violations,
#                                                           stringsAsFactors = FALSE))
# proj4string(violations) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ---------------------------------------------------------------------------- #
#  DRILL DOWN TO A FEW CENSUS TRACTS FOR FASTER COMPUTATION 
# ---------------------------------------------------------------------------- #

# tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# # tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432",
# #                 "2433")
# tractsList <- c("2431")
# tracts <- tracts[tracts$name10 %in% tractsList,]
# violations_df <- over(violations, tracts)
# violations_df <- violations_df[complete.cases(violations_df),]
# rowNums <- as.numeric(rownames(violations_df))
# violations <- violations[c(rowNums),]
# saveRDS(violations, "data/violations-small.Rds")

# ---------------------------------------------------------------------------- #
#  Read small Rds file
# ---------------------------------------------------------------------------- #

violations <- readRDS("data/violations-small.Rds")
violations <- spTransform(violations, CRS("+init=epsg:4326"))

