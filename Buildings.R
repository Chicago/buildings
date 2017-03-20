# load Chicago building polygons
# load building-related datasets with geo coordinates
# match datasets with building polygons

# library(data.table) # use later for data manipulation
library(devtools)
install_github("Chicago/RSocrata", ref = "dev")
library(rgdal)
library(RSocrata)

## READ IN DATA (use RDS files unless you want fresh data (fresh will take much longer))

#Read fresh data

buildingPolygons <- readOGR(dsn="data/Buildings.geojson", layer="OGRGeoJSON")
buildingViolations <- read.socrata("https://data.cityofchicago.org/resource/ucdv-yd74.json")

#Save/Read RDS

# saveRDS(buildingPolygons, file = "data/building_polygons.Rds")
# saveRDS(buildingViolations, file = "data/building_violations.Rds")
# buildingPolygons <- readRDS(file = "data/building_polygons.Rds")
# buildingViolations <- readRDS(file = "data/building_violations.Rds")

## EXTRACT COORDINATES FROM DATA

bvCoords <- data.frame("longitude" = as.numeric(buildingViolations$longitude),
                     "latitude" = as.numeric(buildingViolations$latitude))
bvCoords <- bvCoords[complete.cases(bvCoords),]
bvCoords <- SpatialPoints(bvCoords)
proj4string(bvCoords) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## MATCH DATA TO BUILDING POLYGONS

# test <- over(bvCoords, buildingPolygons)
# saveRDS(test, file = "data/test.Rds")
test <- readRDS(file = "data/test.Rds")

# calcuate match rate
match <- !is.na(test$bldg_id)
noMatch <- is.na(test$bldg_id)
matchRate <- sum(match) / (sum(match) + sum(noMatch))
matchRate <- round(matchRate * 100, digits = 0)
print(paste0("Tried to match ", (sum(match) + sum(noMatch)), " rows to building polygons"))
# "Tried to match 1650323 rows to building polygons"
print(paste0("Succesfully matched ", sum(match), " rows"))
# "Succesfully matched 350648 rows"
print(paste0("Success rate was ", matchRate, "%"))
# "Success rate was 21%"
