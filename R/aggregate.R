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
library(stringr)

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
source("R/tax-delinquencies.R")

# ---------------------------------------------------------------------------- #
#  Clean addresses using DataMade's python parser (very slow)
# ---------------------------------------------------------------------------- #

# addresses <- violations$address
# addresses <- unique(addresses)
# lookupTable <- c()
# for(row in c(1:length(addresses))) {
#   newRow <- cbind("address" = addresses[row], parsedAddress(addresses[row]))
#   lookupTable <- rbind.fill(lookupTable, newRow)
# }
# lookupTable <- lookupTable[,c(1:5)]
# saveRDS(lookupTable, "data/lookupTable.Rds")
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
violations <- merge(violations, matches, by = "address", all.x = T)

# create the feature and add it to building.geojson
demo_last24mos <- violations@data[violations@data$department_bureau == "DEMOLITION" &
                                    as.Date(violations@data$violation_date) > as.Date("2015-04-24"),]
demo_last24mos <- count(demo_last24mos, "bldg_id")
names(demo_last24mos)[2] <- "demo_last24mos"
buildings <- merge(buildings, demo_last24mos, by="bldg_id")

# ---------------------------------------------------------------------------- #
#  Link Tax Shapes
# ---------------------------------------------------------------------------- #

unmatched <- is.na(violations$bldg_id)
newMatches <- over(violations[unmatched,], taxShapes)
violations[unmatched,"pin"] <- newMatches$pin10

unmatched <- violations[unmatched,]
unmatchedPIP <- violations[is.na(violations$bldg_id) & is.na(violations$pin),]

popupCoords <- unmatched$address
popupCoords1 <-paste0(buildings$f_add1, "-", buildings$t_add1, " ", buildings$pre_dir1,
                      " ", buildings$st_name1, " ", buildings$st_type1)
popupCoords2 <- unmatchedPIP$address

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = taxShapes, weight = 1) %>%
  addPolygons(data = buildings, weight = 1, fillColor = "red", popup = popupCoords1) %>%
  addCircles(data = unmatched, radius = 3, stroke = FALSE, fillOpacity = 1, fillColor = '#ff0000', popup = popupCoords) %>%
  addCircles(data = unmatchedPIP, radius = 3, stroke = FALSE, fillOpacity = 1, fillColor = "yellow", popup = popupCoords2)

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

test <- intersect(taxShapes, buildings)
test$area <- sapply(slot(test, "polygons"), slot, "area")
summary(test$area)

#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000e+00 3.815e-09 8.667e-09 1.163e-08 1.290e-08 3.626e-07  

hist(test$area)
hist(test$area[test$area < summary(test$area)[5]], 
     xlab = "Area",
     main = "Histogram of Building Shape Area")

# min to 1st quartile
min_first <- test[test$area < summary(test$area)[2],]

# 1st quartile to median
first_median <- test[test$area >= summary(test$area)[2] & 
                       test$area < summary(test$area)[3],]

# median to 3rd quartile
median_plus <- test[test$area >= summary(test$area)[3],]

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = test, weight = 1)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = min_first, weight = 1, fillColor = "red") %>%
  addPolygons(data = first_median, weight = 1, fillColor = "blue") %>%
  addPolygons(data = median_plus, weight = 1, fillColor = "yellow") 


## ASSIGN PINS TO BUILDINGS
## PUT PINS IN THE BUILDINGS GEOJSON FILE

# (1) For a given building, identify how many tax parcels it overlaps
# (2) For each tax parcel, find the area of that overlap
# (3) Take the area of the overlap from the parcel with the largest overlap. 
#       Find the area of all of the other parcels' overlaps as a percentage of that 
#       largest one. Then set an arbitrary threshold. If the percentage falls under 
#       the threshold, assume they are unrelated.

bldg_ids <- unique(test$bldg_id)
bldgList <- list()
buildings$pins <- NA
for (i in c(1:length(bldg_ids))) {
  bldgList[[i]] <- list(bldg_id = bldg_ids[i])
  pins <- test@data[test$bldg_id == bldg_ids[i],
                    "pin10"]
  bldgList[[i]]$pins <- pins
  area <- test@data[test$pin10 %in% pins & 
                      test$bldg_id == bldg_ids[i],
                    "area"]
  bldgList[[i]]$area <- area
  areaSum <- sum(area)
  areaRatio <- area / areaSum
  bldgList[[i]]$areaRatio <- areaRatio
  pinIndex <- which(areaRatio > .05)
  pinsFinal <- pins[pinIndex]
  bldgList[[i]]$pinsFinal <- pinsFinal
  buildings@data[buildings$bldg_id == bldg_ids[i],]$pins <- list(t(pinsFinal))
}

# ---------------------------------------------------------------------------- #
#  Add tax delinquencies
#  Condos are getting counted multiple times (Need to address, but OK now)     
# ---------------------------------------------------------------------------- #

# create the feature and add it to building.geojson
# functionalize - add feature
# createFeature(data) # columns: bldg_id or pin, date of event
# addFeature(buildings, feature, matchType = "bldg_id" or "pin", name)

taxsales_last24mos <- tax_sales[tax_sales$tax_sale_year %in% c(2014,2015),]
taxsales_last24mos <- count(taxsales_last24mos, "pin") 
names(taxsales_last24mos)[2] <- "taxsales_last24mos"
buildings$taxsales_last24mos <- 0
for (i in c(1:length(bldgList))) {
  pins <- bldgList[[i]]$pinsFinal
  n <- 0
  for (j in 1:length(pins)) {
    found <- taxsales_last24mos[taxsales_last24mos$pin == pins[j],
                                "taxsales_last24mos"] 
    if (length(found) > 0) {
      n <- n + found
    }
  } 
  bldgList[[i]]$taxsales_last24mos <- n
  bid <- bldgList[[i]]$bldg_id
  buildings@data[buildings$bldg_id == bid,]$taxsales_last24mos <- n
}

# map out "bad buildings" using the new feature
bad_buildings <- buildings[!is.na(buildings$demo_last24mos) & buildings$demo_last24mos > 0,]
tax_sales <- buildings[buildings$taxsales_last24mos > 0,]
popup1 <- paste0(bad_buildings$f_add1, "-", bad_buildings$t_add1, " ", bad_buildings$pre_dir1,
                 " ", bad_buildings$st_name1, " ", bad_buildings$st_type1)
popup2 <- paste0(tax_sales$f_add1, "-", tax_sales$t_add1, " ", tax_sales$pre_dir1,
                 " ", tax_sales$st_name1, " ", tax_sales$st_type1)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = bad_buildings, weight = 1, fillColor = "red", popup = popup1) %>%
  addPolygons(data = tax_sales, weight = 1, fillColor = "yellow", popup = popup2)
