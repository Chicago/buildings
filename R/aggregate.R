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
  args <- c('address-parser.py',addr)
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

# ---------------------------------------------------------------------------- #
#  Import Data
# ---------------------------------------------------------------------------- #

source("R/buildings.R")
source("R/violations.R")

# ---------------------------------------------------------------------------- #
#  Clean addresses using DataMade's python parser (very slow)
# ---------------------------------------------------------------------------- #

# addresses <- b_vio$address
# addresses <- unique(addresses)
# lookupTable <- c()
# for(row in c(1:length(addresses))) {
#   newRow <- cbind("address" = addresses[row], parsedAddress(addresses[row]))
#   lookupTable <- rbind.fill(lookupTable, newRow)
# }
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
