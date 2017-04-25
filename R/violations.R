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
# proj4string(violations) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

# ---------------------------------------------------------------------------- #
#  DRILL DOWN TO A FEW CENSUS TRACTS FOR FASTER COMPUTATION 
# ---------------------------------------------------------------------------- #

# violations_df <- over(violations, tracts)
# violations_df <- violations_df[complete.cases(violations_df),]
# rowNums <- as.numeric(rownames(violations_df))
# violations <- violations[c(rowNums),]
# saveRDS(violations, "data/violations-small.Rds")

violations <- readRDS("data/violations-small.Rds")
