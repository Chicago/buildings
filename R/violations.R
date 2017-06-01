# ---------------------------------------------------------------------------- #
#  Import all building violations
# ---------------------------------------------------------------------------- #

## read in CSV downloaded from data portal

# violations <- read.csv("data/Building_Violations.csv", stringsAsFactors = FALSE)

## remove prior geocoding

# violations$LATITUDE <- NULL
# violations$LONGITUDE <- NULL
# violations$LOCATION <- NULL

## export for geocoding (geocoding is manual step)

# write.csv(violations, "building-violations-to-geocode-data-science.csv", row.names = FALSE)

## import geocoded file

# violations <- read.csv("Results_Job74347_building-violations-to-geocode-data-science.csv", stringsAsFactors = FALSE)

# violations$VIOLATION.DATE <- as.Date(violations$VIOLATION.DATE, format="%m/%d/%Y")

# ---------------------------------------------------------------------------- #
#  CONVERT VIOLATIONS TO SPATIAL DATA FRAME
# ---------------------------------------------------------------------------- #

# violations <- toSpatial(violations)

# ---------------------------------------------------------------------------- #
#  DRILL DOWN TO A FEW CENSUS TRACTS FOR FASTER COMPUTATION 
# ---------------------------------------------------------------------------- #

# tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# # tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432",
# #                 "2433")
# tracts <- spTransform(tracts, CRS("+init=epsg:4326"))
# tractsList <- c("2608")
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
