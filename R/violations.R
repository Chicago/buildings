# ---------------------------------------------------------------------------- #
#  Import all building violations
# ---------------------------------------------------------------------------- #

## read in CSV downloaded from data portal
## tractToPass (incoming variable) 

# violations <- read.csv("data/Building_Violations.csv", stringsAsFactors = FALSE)

## remove prior geocoding

# violations$LATITUDE <- NULL
# violations$LONGITUDE <- NULL
# violations$LOCATION <- NULL

## export for geocoding (geocoding is manual step)

# write.csv(violations, "building-violations-to-geocode-data-science.csv", row.names = FALSE)

## import geocoded file

# violations <- read.csv("Results_Job74347_building-violations-to-geocode-data-science.csv", stringsAsFactors = FALSE)
# 
# violations$VIOLATION.DATE <- as.Date(violations$VIOLATION.DATE, format="%m/%d/%Y")
# violations <- violations[!is.na(violations$HOUSE_LOW),]
# 
# # ---------------------------------------------------------------------------- #
# #  CONVERT VIOLATIONS TO SPATIAL DATA FRAME
# # ---------------------------------------------------------------------------- #
# 
# violations <- toSpatial(violations)

# ---------------------------------------------------------------------------- #
#  Create a geojson file for each census tract 
# ---------------------------------------------------------------------------- #

# writeTracts(spatialData = violations,
#             tracts = tracts, 
#            description = "violations")

# ---------------------------------------------------------------------------- #
#  Read file
# ---------------------------------------------------------------------------- #

path <- paste0("data/byTract/violations/", tractToPass,".geojson")
violations <- readOGR(path, layer="OGRGeoJSON",
                      stringsAsFactors = FALSE)
violations <- spTransform(violations, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))
