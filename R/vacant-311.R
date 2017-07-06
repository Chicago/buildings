# ---------------------------------------------------------------------------- #
#  Import all 311 call for vacant buildings
# ---------------------------------------------------------------------------- #

## read in CSV downloaded from data portal
## ALREADY GEOCODED
## tractToPass (incoming variable) 

# vacant311 <- read.csv("Results_Job75263_CSR-311-calls.csv", stringsAsFactors = FALSE)

## remove prior lat/long

# vacant311$LATITUDE <- NULL
# vacant311$LONGITUDE <- NULL
# vacant311$Location <- NULL
# vacant311$X.COORDINATE <- NULL
# vacant311$Y.COORDINATE <- NULL
# 
# vacant311$DATE.SERVICE.REQUEST.WAS.RECEIVED <- as.Date(vacant311$DATE.SERVICE.REQUEST.WAS.RECEIVED, format="%m/%d/%Y")
# vacant311 <- vacant311[!is.na(vacant311$HOUSE_LOW),]

# ---------------------------------------------------------------------------- #
#  CONVERT TO SPATIAL DATA FRAME
# ---------------------------------------------------------------------------- #

# vacant311 <- toSpatial(vacant311)

# ---------------------------------------------------------------------------- #
#  Create a geojson file for each census tract 
# ---------------------------------------------------------------------------- #

# writeTracts(spatialData = vacant311,
#             tracts = tracts,
#             description = "vacant311")

# ---------------------------------------------------------------------------- #
#  Read file
# ---------------------------------------------------------------------------- #

path <- paste0("data/byTract/vacant311/", tractToPass,".geojson")
vacant <- try(readOGR(path, layer="OGRGeoJSON",
                      stringsAsFactors = FALSE), silent = TRUE)
if (class(vacant) == "try-error") {
  warning(paste0("Could not read ", path))
} else {
  vacant <- spTransform(vacant, 
                        CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))
}
