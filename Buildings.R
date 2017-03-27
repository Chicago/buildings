library(rgdal)
library(leaflet)
# library(RSocrata)

## READ IN DATA (use RDS files unless you want fresh data (fresh will take much longer))

#Read fresh data

# footprints <- readOGR(dsn="data/Buildings.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
# b_vio <- read.socrata("https://data.cityofchicago.org/resource/ucdv-yd74.json")
#commAreas <- readOGR(dsn="data/community-areas.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
#westTown <- commAreas[commAreas$community == "WEST TOWN",]
tracts <- readOGR(dsn="data/tracts.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)
tractsList <- c("2424","2423","2422","2421","2420","2429","2430","2431","2432","2433")
tracts <- tracts[tracts$name10 %in% tractsList,]

#Save/Read RDS

# saveRDS(footprints, file = "data/building_polygons.Rds")
# saveRDS(b_vio, file = "data/building_violations.Rds")
footprints <- readRDS(file = "data/building_polygons.Rds")
b_vio <- readRDS(file = "data/building_violations.Rds")

## EXTRACT COORDINATES FROM DATA

b_vio <- data.frame("address" = b_vio$address,
                    "longitude" = as.numeric(b_vio$longitude),
                    "latitude" = as.numeric(b_vio$latitude),
                    stringsAsFactors = FALSE)
b_vio <- b_vio[complete.cases(b_vio),]
b_vio <- unique(b_vio)
rownames(b_vio) <- seq(length=nrow(b_vio))
b_vio <- SpatialPointsDataFrame(coords = SpatialPoints(b_vio[,c(2:3)]),
                                data = as.data.frame(b_vio$address,
                                                     stringsAsFactors = FALSE))
proj4string(b_vio) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## MATCH DATA TO BUILDING POLYGONS 

# test <- over(b_vio, footprints)
# saveRDS(test, file = "data/test.Rds")
# test <- readRDS(file = "data/test.Rds")
footprints_df <- over(footprints, tracts)
footprints_df <- footprints_df[complete.cases(footprints_df),]
rowNums <- as.numeric(rownames(footprints_df))
footprints <- footprints[c(rowNums),]
b_vio_df <- over(b_vio, tracts)
b_vio_df <- b_vio_df[complete.cases(b_vio_df),]
rowNums <- as.numeric(rownames(b_vio_df))
b_vio <- b_vio[c(rowNums),]

#saveRDS(footprints, "data/footprints.Rds")
#saveRDS(b_vio, "data/b_vioSmall.Rds")
footprints <- readRDS("data/footprints.Rds")
b_vio <- readRDS("data/b_vioSmall.Rds")

popup <- paste(footprints$label_hous, 
               footprints$t_add1,
               footprints$pre_dir1,
               footprints$st_name1,
               footprints$st_type1,
               sep = " ")

popupCoords <- b_vio$`b_vio$address`

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = footprints, weight = 1, popup = popup) %>%
  addCircles(data = b_vio, radius = 1, stroke = FALSE, fillOpacity = 1, fillColor = '#ff0000', popup = popupCoords)

