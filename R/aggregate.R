rm(list=ls())
dev.off(dev.list()["RStudioGD"])

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
  odd <- footprintAddresses[(footprintAddresses$f_add1 %% 2) == 1,]
  even <- footprintAddresses[(footprintAddresses$f_add1 %% 2) == 0,]
  if ((addNum %% 2 == 1)) {
    addressMatch <- odd[odd$f_add1 <= addNum &
                          odd$t_add1 >= addNum &
                          toupper(odd$pre_dir1) == toupper(preDir) &
                          toupper(odd$st_name1) == toupper(street) &
                          toupper(odd$st_type1) == toupper(postType)
                        ,]
  } else {
    addressMatch <- even[even$f_add1 <= addNum &
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
  # system2("python3.5", args, stdout=TRUE) #macbook
  system2("python", args, stdout=TRUE)
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

stateplane2latlon <- function(X, Y, metric=FALSE){
  # latlon <- data.table(longitude = c(1148703.5804669, 1148721.69534794,
  #                                    1148718.58006945, 1148719.92031838,
  #                                    1148722.28519294),
  #                      latitude = c(1938916.16105645, 1938209.79458671,
  #                                   1938315.99708976, 1938270.3270949,
  #                                   1938189.60711051))
  # browser()
  latlon <- data.table(longitude = X, latitude = Y)
  ii <- apply(latlon, 1, function(x) !any(is.na(x)))
  latlon <- latlon[ii]
  coordinates(latlon) <- c("longitude", "latitude")
  if(metric){
    proj4string(latlon) <- CRS("+init=epsg:3529 +units=ft")
  } else {
    proj4string(latlon) <- CRS("+init=epsg:3529 +units=us-ft")
  }
  latlon <- coordinates(spTransform(latlon, CRS("+proj=longlat +datum=WGS84")))
  ret <- data.table(X, Y)
  ret <- ret[ii, latitude := coordinates(latlon)[ , 'latitude']][]
  ret <- ret[ii, longitude := coordinates(latlon)[ , 'longitude']][]
  return(ret)
}

# ---------------------------------------------------------------------------- #
#  Import Data
# ---------------------------------------------------------------------------- #

source("R/buildings.R")
source("R/violations.R")
source("R/tax-parcels.R")
source("R/tax-delinquencies.R")
source("R/vacant-311.R")

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
footprintAddresses <- buildings@data[,c("bldg_id","f_add1","t_add1","pre_dir1","st_name1","st_type1")]
footprintAddresses$f_add1 <- as.numeric(footprintAddresses$f_add1)
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

# match the cleaned addresses from vacant buildings to the extracted building addresses
matchesVacant <- c()
for (row in c(1:nrow(vacant))) {
  newMatch <- matchAddress(footprintAddresses,
                           vacant@data$address_street_number[row],
                           vacant@data$address_street_direction[row],
                           vacant@data$address_street_name[row],
                           vacant@data$address_street_suffix[row])
  if (nrow(newMatch) == 0) next
  newRow <- cbind(vacant@data[row,1:4],
                  newMatch,
                  row.names = NULL)
  matchesVacant <- rbind(matchesVacant, newRow)
  rm(list = c("newRow","newMatch","row"))
}

# remove coach houses, etc.
matches <- matches[!duplicated(matches$address),c("address","bldg_id")]

# remove duplicates
matchesVacant <- unique(matchesVacant[,c("address_street_number",
                                         "address_street_direction",
                                         "address_street_name",
                                         "address_street_suffix",
                                         "bldg_id")])

# add building identifier to the datasets
violations <- merge(violations, matches, by = "address", all.x = T)
vacant <- merge(vacant, matchesVacant,
                by.x = c("address_street_number",
                         "address_street_direction",
                         "address_street_name",
                         "address_street_suffix"),
                by.y = c("address_street_number",
                         "address_street_direction",
                         "address_street_name",
                         "address_street_suffix"), 
                
                all.x = T)

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
#  Add features
#    when doing PIN matching, Condos are getting counted multiple times (Need to address, but OK now)     
# ---------------------------------------------------------------------------- #

# create the feature and add it to building.geojson
# functionalize - add feature
# createFeature(data) # columns: bldg_id or pin, date of event
# addFeature(buildings, feature, matchType = "bldg_id" or "pin", name)


## createFeature expects a data.frame with a list of events, e.g. inspections
## the data needs to have two columns: 1) bldg_id or pin and 2) date of event
## returns a "feature" that can be fed into addFeature
createFeature <- function(df) {
  ## TODO write tests to make sure data is in proper format
  ## TODO handle errors with descriptions of the problem
  if ("pin" %in% names(df)) {
    result <- count(df, "pin") 
  }
  if ("bldg_id" %in% names(df)) {
    result <- count(df, "bldg_id") 
  }
  return(result)
}

addFeature <- function(bldgList, feature_df, feature_name) {
  ## match by bldg_id if available, pin if not
  if ("bldg_id" %in% names(feature_df)) {
    matchType <- "bldg_id" 
  } else if ("pin" %in% names(feature_df)) {
    matchType <- "pin" 
  } else stop("No bldg_id or pin found in feature_df, cannot match records to a building")
  
  if (matchType == "bldg_id") {
    for (i in c(1:length(bldgList))) {
      n <- 0
      bldg_id <- bldgList[[i]]$bldg_id
      found <- feature_df[feature_df$bldg_id == bldg_id,
                          "freq"] 
      if (length(found) > 0) {
        n <- n + found
      }
      index <- length(bldgList[[i]])
      bldgList[[i]][index+1] <- n
      names(bldgList[[i]])[index+1] <- feature_name
    }    
    result <- bldgList
  }
  
  if (matchType == "pin") {
    for (i in c(1:length(bldgList))) {
      pins <- bldgList[[i]]$pinsFinal
      n <- 0
      for (j in 1:length(pins)) {
        found <- feature_df[feature_df$pin == pins[j],
                            "freq"] 
        if (length(found) > 0) {
          n <- n + found
        }
      } 
      index <- length(bldgList[[i]])
      bldgList[[i]][index+1] <- n
      names(bldgList[[i]])[index+1] <- feature_name
    }    
    result <- bldgList
  }
  
  return(result)
}

taxsales_last24mos <- tax_sales[tax_sales$tax_sale_year %in% c(2014,2015),]
newFeature <- createFeature(taxsales_last24mos)
bldgList <- addFeature(bldgList, newFeature, "taxsales_last24mos")

demo_last24mos <- violations@data[violations@data$department_bureau == "DEMOLITION" &
                                    as.Date(violations@data$violation_date) > as.Date("2010-01-01") &
                                    !is.na(violations@data$bldg_id),
                                  c("bldg_id", "violation_date")]
newFeature <- createFeature(demo_last24mos)
bldgList <- addFeature(bldgList, newFeature, "demo_last24mos")

demo_last24mosPIN <- violations@data[violations@data$department_bureau == "DEMOLITION" &
                                       as.Date(violations@data$violation_date) > as.Date("2010-01-01") &
                                       !is.na(violations@data$pin),
                                     c("pin", "violation_date")]
newFeature <- createFeature(demo_last24mosPIN)
bldgList <- addFeature(bldgList, newFeature, "demo_last24mosPIN")

vacant311 <- vacant@data[as.Date(vacant@data$date_service_request_was_received) > as.Date("2010-01-01") &
                           !is.na(vacant@data$bldg_id),
                         c("bldg_id", "date_service_request_was_received")]
newFeature <- createFeature(vacant311)
bldgList <- addFeature(bldgList, newFeature, "vacant311")

## Add features back to buildings oj

extractedFeatures <- lapply(bldgList, function(x) {
  data.frame("bldg_id" = x$bldg_id, 
             "taxsales_last24mos" = x$taxsales_last24mos, 
             "demo_last24mos" = x$demo_last24mos,
             "demo_last24mosPIN" = x$demo_last24mosPIN,
             "vacant311" = x$vacant311,
             stringsAsFactors = FALSE)
})
extractedFeatures <- do.call("rbind", extractedFeatures)
buildings <- merge(buildings, extractedFeatures, by = "bldg_id", all.x = TRUE)

# map out "troubled buildings" using the new feature
bad_buildings <- buildings[(buildings$demo_last24mos > 0 | buildings$demo_last24mosPIN > 0),]
tax_sales <- buildings[buildings$taxsales_last24mos > 0,]
vacant_calls <- buildings[buildings$vacant311 > 0,]
popup1 <- paste0(bad_buildings$f_add1, "-", bad_buildings$t_add1, " ", bad_buildings$pre_dir1,
                 " ", bad_buildings$st_name1, " ", bad_buildings$st_type1, "<br>",
                 bad_buildings$pins)
popup2 <- paste0(tax_sales$f_add1, "-", tax_sales$t_add1, " ", tax_sales$pre_dir1,
                 " ", tax_sales$st_name1, " ", tax_sales$st_type1, "<br>",
                 tax_sales$pins)
popup3 <- paste0(vacant_calls$f_add1, "-", vacant_calls$t_add1, " ", vacant_calls$pre_dir1,
                 " ", vacant_calls$st_name1, " ", vacant_calls$st_type1, "<br>",
                 vacant_calls$pins)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = vacant_calls, weight = 0, opacity = 1, fillColor = "purple", popup = popup3) %>%
  addPolygons(data = bad_buildings, weight = 0, opacity = 1, fillColor = "red", popup = popup1) %>%
  addPolygons(data = tax_sales, weight = 0, opacity = .05, fillColor = "yellow", popup = popup2) 

