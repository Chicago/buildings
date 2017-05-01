# ---------------------------------------------------------------------------- #
#  A script to add features and aggregate data for Chicago buildings
# 
#  Author: Nick Lucius
# ---------------------------------------------------------------------------- #

library(data.table)
library(rgdal)
library(leaflet)
library(RSocrata)
library(plyr)
library(raster)

# ---------------------------------------------------------------------------- #
#  A function for matching incoming addresses to the building "footprint"      
#  address
# ---------------------------------------------------------------------------- #

matchAddress <- function(footprintAddresses, addNum, preDir, street, postType) {
  addNum <- as.numeric(addNum)
  odd <- footprintAddresses[(footprintAddresses$label_hous %% 2) == 1,]
  even <- footprintAddresses[(footprintAddresses$label_hous %% 2) == 0,]
  if ((addNum %% 2 == 1)) {
    addressMatch <- odd[odd$label_hous <= addNum &
                          odd$t_add1 >= addNum &
                          toupper(odd$pre_dir1) == toupper(preDir) &
                          toupper(odd$st_name1) == toupper(street) &
                          toupper(odd$st_type1) == toupper(postType)
                        ,]
  } else {
    addressMatch <- even[even$label_hous <= addNum &
                           even$t_add1 >= addNum &
                           toupper(even$pre_dir1) == toupper(preDir) &
                           toupper(even$st_name1) == toupper(street) &
                           toupper(even$st_type1) == toupper(postType)
                         ,]    
  }
  return(addressMatch)
}

# ---------------------------------------------------------------------------- #
#  A function for calling Datamade's python address parser
# ---------------------------------------------------------------------------- #

parsedAddress <- function(addr) {
  addr <- gsub(" ", ",", addr)
  args <- c('py/address-parser.py',addr)
  system2("python3.5", args, stdout=TRUE) #macbook
  # system2("python", args, stdout=TRUE)
  parsed <- read.csv("parsed.csv", header = FALSE)
  parsed$V1 <- gsub("]","",parsed$V1)
  parsed$V1 <- gsub(",","",parsed$V1)
  parsed$V1 <- gsub("\'","",parsed$V1)
  parsed$V1 <- gsub("[.]","",parsed$V1)
  parsed <- data.table(parsed$V2, parsed$V1)
  parsed <- dcast(parsed, . ~ V1, value.var = "V2")
  parsed <- parsed[,2:ncol(parsed)]
  if (file.exists("parsed.csv")) file.remove("parsed.csv") 
  return(as.data.frame(parsed))
}

# ---------------------------------------------------------------------------- #
#  Import Data
# ---------------------------------------------------------------------------- #

source("R/buildings.R")
source("R/violations.R")
source("R/tax-parcels.R")

# ---------------------------------------------------------------------------- #
#  Clean addresses using DataMade's python parser (very slow)
# ---------------------------------------------------------------------------- #

addresses <- violations$address
addresses <- unique(addresses)
lookupTable <- c()
for(row in c(1:length(addresses))) {
  newRow <- cbind("address" = addresses[row], parsedAddress(addresses[row]))
  lookupTable <- rbind.fill(lookupTable, newRow)
}
lookupTable <- lookupTable[,c(1:5)]
saveRDS(lookupTable, "data/lookupTable.Rds")
lookupTable <- readRDS("data/lookuptable.Rds")

# ---------------------------------------------------------------------------- #
#  First try at adding a feature to building.geojson
# ---------------------------------------------------------------------------- #

## still need to deal with multiple houses on one property (merge error)
## Demo is being added as total violations, not inspections

# extract addresses from building.geojson
footprintAddresses <- buildings@data[,c("bldg_id","label_hous","t_add1","pre_dir1","st_name1","st_type1")]
footprintAddresses$label_hous <- as.numeric(footprintAddresses$label_hous)
footprintAddresses$t_add1 <- as.numeric(footprintAddresses$t_add1)
footprintAddresses <- footprintAddresses[complete.cases(footprintAddresses),]

# match the cleaned addresses from violations to the extracted building addresses
matches <- c()
for (row in c(1:nrow(lookupTable))) {
  newMatch <- matchAddress(footprintAddresses,
                           lookupTable$AddressNumber[row],
                           lookupTable$StreetNamePreDirectional[row],
                           lookupTable$StreetName[row],
                           lookupTable$StreetNamePostType[row])
  if (nrow(newMatch) == 0) next
  newRow <- cbind(lookupTable[row,],
                  newMatch,
                  row.names = NULL)
  matches <- rbind(matches, newRow)
  rm(list = c("newRow","newMatch","row"))
}

# remove coach houses, etc.
matches <- matches[!duplicated(matches$address),c("address","bldg_id")] 

# add building identifier to the  violations dataset
violations <- merge(violations, matches, by = "address")

# create the feature and add it to building.geojson
demo_last24mos <- violations@data[violations@data$department_bureau == "DEMOLITION" &
                               as.Date(violations@data$violation_date) > as.Date("2015-04-24"),]
demo_last24mos <- count(demo_last24mos, "bldg_id")
names(demo_last24mos)[2] <- "demo_last24mos"
buildings <- merge(buildings, demo_last24mos, by="bldg_id")

# map out "bad buildings" using the new feature
bad_buildings <- buildings[!is.na(buildings$demo_last24mos) & buildings$demo_last24mos > 0,]
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = bad_buildings, weight = 1, fillColor = "red")

# ---------------------------------------------------------------------------- #
#  Link Tax Shapes
# ---------------------------------------------------------------------------- #

matches <- data.table(matches)
lookupTable <- data.table(lookupTable)
unmatched <- fsetdiff(lookupTable[,"address"], matches[,"address"])
unmatched <- violations[violations$address %in% unmatched$address,]

pipMatches <- over(unmatched, buildings)
pipMatches <- pipMatches[!is.na(pipMatches$st_name1),]
pipMatches <- unique(pipMatches)
pipMatches <- violations[violations$address %in% pipMatches$address,]

popupCoords <- unmatched$address

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = taxShapes, weight = 1) %>%
  addPolygons(data = buildings, weight = 1, fillColor = "red") %>%
  addCircles(data = unmatched, radius = 3, stroke = FALSE, fillOpacity = 1, fillColor = '#ff0000', popup = popupCoords)


overlap <- cbind("pin" = taxShapes@data$pin10, 
                 over(taxShapes, 
                      buildings,
                      minDimension = 2))
referenceTable <- overlap[,c("pin",
                             "bldg_id", 
                             "harris_str",
                             "t_add1", 
                             "pre_dir1", 
                             "st_name1", 
                             "st_type1")]
referenceTable <- referenceTable[!(referenceTable$t_add1 == 0),]
referenceTable <- referenceTable[complete.cases(referenceTable),]

# if PIN has multiple buildings, only one is being returned
# small, unintentional overlaps need to be removed

# 1150 N Cleaver
# 1635 W Division

# plot a few different bins of areas
# have PIN dataset and building dataset
# give all other datasets a building ID and PIN as possible
# tidy each dataset

test <- intersect(taxShapes, buildingShapes)
test$area <- sapply(slot(test, "polygons"), slot, "area")
summary(test$area)

#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000e+00 2.369e-09 7.786e-09 1.144e-08 1.382e-08 8.797e-07 

hist(test$area)
hist(test$area[test$area < 1.382e-08])
hist(test$area[test$area < 1.144e-08])
hist(test$area[test$area < 7.786e-09], xlab = "Area", main = "Histogram of Building Shape Area")
hist(test$area[test$area < 2.369e-09])

# min to 1st quartile
min_first <- test[test$area < 2.369e-09,]

# 1st quartile to median
first_median <- test[test$area >= 2.369e-09 & test$area < 7.786e-09,]

# median to 3rd quartile
median_plus <- test[test$area >= 7.786e-09,]

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = test, weight = 1)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = min_first, weight = 1, fillColor = "red") %>%
  addPolygons(data = first_median, weight = 1, fillColor = "blue") %>%
  addPolygons(data = median_plus, weight = 1, fillColor = "yellow") 

