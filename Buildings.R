library(data.table)
library(rgdal)
library(leaflet)
library(RSocrata)

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
  if (file.exists("parsed.csv")) file.remove("parsed.csv") 
  return(parsed)
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
  addCircles(data = b_vio, radius = 1, stroke = FALSE, fillOpacity = 1, fillColor = '#ff0000', popup = popupCoords)

## LINK DATA
## 1. Link by address. Then, for unmatched points:
## 2. Explore using machine learning, point-in-polygon, and distance from polygon

#inspect data

b_vio$address[456]

parsedAddress(b_vio$address[456])

footprints@data[c(343),c("label_hous","t_add1","pre_dir1","st_name1","st_type1")]
