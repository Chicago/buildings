library(data.table)
library(rgdal)
library(leaflet)
library(RSocrata)
library(plyr)

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

## READ IN DATA 

#Read fresh data

# footprints <- readOGR(dsn="data/Buildings.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# b_vio <- read.socrata("https://data.cityofchicago.org/resource/ucdv-yd74.json")
# tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432","2433")
# tracts <- tracts[tracts$name10 %in% tractsList,]

#Save/Read RDS

# saveRDS(footprints, file = "data/footprints.Rds")
# saveRDS(b_vio, file = "data/building_violations.Rds")
# footprints <- readRDS(file = "data/footprints.Rds")
# b_vio <- readRDS(file = "data/building_violations.Rds")

## CONVERT VIOLATIONS TO SPATIAL DATA FRAME

# b_vio$longitude <- as.numeric(b_vio$longitude)
# b_vio$latitude <- as.numeric(b_vio$latitude)
# b_vio <- b_vio[complete.cases(b_vio[c("address","longitude","latitude")]),]
# rownames(b_vio) <- seq(length=nrow(b_vio))
# b_vio <- SpatialPointsDataFrame(coords = SpatialPoints(b_vio[,c("longitude","latitude")]),
#                                 data = as.data.frame(b_vio,
#                                                      stringsAsFactors = FALSE))
# proj4string(b_vio) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## DRILL DOWN TO A FEW CENSUS TRACTS FOR FASTER COMPUTATION 

# footprints_df <- over(footprints, tracts)
# footprints_df <- footprints_df[complete.cases(footprints_df),]
# rowNums <- as.numeric(rownames(footprints_df))
# footprints <- footprints[c(rowNums),]
# b_vio_df <- over(b_vio, tracts)
# b_vio_df <- b_vio_df[complete.cases(b_vio_df),]
# rowNums <- as.numeric(rownames(b_vio_df))
# b_vio <- b_vio[c(rowNums),]

#saveRDS(footprints, "data/footprints_small.Rds")
#saveRDS(b_vio, "data/b_vioSmall.Rds")

# Read RDS files (comment out the above if you use these files)

footprints <- readRDS("data/footprints_small.Rds")
b_vio <- readRDS("data/b_vioSmall.Rds")
b_vio$address <- as.character(b_vio$address)

popup <- paste(footprints$label_hous, 
               footprints$t_add1,
               footprints$pre_dir1,
               footprints$st_name1,
               footprints$st_type1,
               sep = " ")

popupCoords <- b_vio$address

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = footprints, weight = 1, popup = popup) %>%
  addCircles(data = b_vio, radius = 3, stroke = FALSE, fillOpacity = 1, fillColor = '#ff0000', popup = popupCoords)

## LINK DATA
## 1. Link by address. Then, for unmatched points:
## 2. Explore using machine learning, point-in-polygon, and distance from polygon

# The following for loop is very slow due to command line python call
# Use lookupTable as a reference table for quicker reading
 
# addresses <- b_vio$address
# addresses <- unique(addresses)
# lookupTable <- c()
# for(row in c(1:length(addresses))) {
#   newRow <- cbind("address" = addresses[row], parsedAddress(addresses[row]))
#   lookupTable <- rbind.fill(lookupTable, newRow)
# }

# saveRDS(lookupTable, "data/lookupTable.Rds")

lookupTable <- readRDS("data/lookupTable.Rds")
lookupTable$address <- as.character(lookupTable$address)
footprintAddresses <- footprints@data[,c("label_hous","t_add1","pre_dir1","st_name1","st_type1")]
footprintAddresses$label_hous <- as.numeric(footprintAddresses$label_hous)
footprintAddresses$t_add1 <- as.numeric(footprintAddresses$t_add1)
footprintAddresses <- footprintAddresses[complete.cases(footprintAddresses),]

# function for matching addresses to building footprints

matchAddress <- function(footprintAddresses, addNum, preDir, street, postType) {
  addNum <- as.numeric(addNum)
  odd <- footprintAddresses[(footprintAddresses$label_hous %% 2) == 1,]
  even <- footprintAddresses[(footprintAddresses$label_hous %% 2) == 0,]
  if ((addNum %% 2 == 1)) {
    match <- odd[odd$label_hous <= addNum &
                   odd$t_add1 >= addNum &
                   toupper(odd$pre_dir1) == toupper(preDir) &
                   toupper(odd$st_name1) == toupper(street) &
                   toupper(odd$st_type1) == toupper(postType)
                 ,]
  } else {
    match <- even[even$label_hous <= addNum &
                    even$t_add1 >= addNum &
                    toupper(even$pre_dir1) == toupper(preDir) &
                    toupper(even$st_name1) == toupper(street) &
                    toupper(even$st_type1) == toupper(postType)
                  ,]    
  }
  return(match)
}

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

matches <- data.table(matches)
lookupTable <- data.table(lookupTable)
unmatched <- fsetdiff(lookupTable, matches[,1:5])
unmatched <- b_vio[b_vio$address %in% unmatched$address,]


popupCoords <- unmatched$address

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = footprints, weight = 1, popup = popup) %>%
  addCircles(data = unmatched, radius = 3, stroke = FALSE, fillOpacity = 1, fillColor = '#ff0000', popup = popupCoords)

# find nearest polygon to a point: 
# http://stackoverflow.com/questions/26308426/how-do-i-find-the-polygon-nearest-to-a-point-in-r
# What are the types of errors?
# 1. Corner buildings (point-in or distance OK)
# 2. Vacant lots or missing shapefile? (point-in or distance NOT OK) (HAVE A POINT VISUAL FOR PROBLEMS WITH NO MATCHING SHAPEFILE?)
# 3. Long / missing ranges (point-in or distance OK)
# 4. Wildcards (point-in or distance OK)
# 5. use building features against violation characteristics? (future possibility)
