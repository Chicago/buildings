# ---------------------------------------------------------------------------- #
#  Import all 311-reported building vacancies
# ---------------------------------------------------------------------------- #

# vacant <-
#   read.socrata("https://data.cityofchicago.org/resource/yama-9had.json")

# ---------------------------------------------------------------------------- #
#  CONVERT VIOLATIONS TO SPATIAL DATA FRAME
# ---------------------------------------------------------------------------- #

# vacant$longitude <- as.numeric(vacant$longitude)
# vacant$latitude <- as.numeric(vacant$latitude)
# vacant <- vacant[complete.cases(vacant[c("address_street_name","longitude",
#                                          "latitude")]),]
# rownames(vacant) <- seq(length=nrow(vacant))
# vacant <- SpatialPointsDataFrame(coords =
#                                    SpatialPoints(vacant[,c("longitude",
#                                                            "latitude")]),
#                                  data = as.data.frame(vacant,
#                                                       stringsAsFactors = FALSE))
# proj4string(vacant) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ---------------------------------------------------------------------------- #
#  DRILL DOWN TO A FEW CENSUS TRACTS FOR FASTER COMPUTATION 
# ---------------------------------------------------------------------------- #

# tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# # tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432",
# #                 "2433")
# tractsList <- c("2608")
# tracts <- tracts[tracts$name10 %in% tractsList,]
# vacant_df <- over(vacant, tracts)
# vacant_df <- vacant_df[complete.cases(vacant_df),]
# rowNums <- as.numeric(rownames(vacant_df))
# vacant <- vacant[c(rowNums),]
# saveRDS(vacant, "data/vacant-small.Rds")

# ---------------------------------------------------------------------------- #
#  Read small Rds file
# ---------------------------------------------------------------------------- #

vacant <- readRDS("data/vacant-small.Rds")
vacant <- spTransform(vacant, CRS("+init=epsg:4326"))

