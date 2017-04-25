library(data.table)
library(rgdal)
library(raster)
library(leaflet)
library(RSocrata)
library(plyr)

buildingShapes <- readRDS(file = "data/footprints_small.Rds")

# ogrInfo(dsn = "data/parcels.geojson")
# taxShapes <- readOGR(dsn="data/parcels.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# saveRDS(taxShapes, "data/taxShapes.Rds")
# taxShapes <- readRDS("data/taxShapes.Rds")

# tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432","2433")
# tracts <- tracts[tracts$name10 %in% tractsList,]

# taxShapes_df <- over(taxShapes, tracts)
# taxShapes_df <- taxShapes_df[complete.cases(taxShapes_df),]
# rowNums <- as.numeric(rownames(taxShapes_df))
# taxShapes <- taxShapes[c(rowNums),]
# saveRDS(taxShapes, "data/taxShapesSmall.Rds")
taxShapes <- readRDS("data/taxShapesSmall.Rds")

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
