
initBldgList <- function(buildings, taxShapes) {
  overlap <- intersect(taxShapes, buildings)
  overlap$area <- sapply(slot(overlap, "polygons"), slot, "area")
  bldg_ids <- unique(overlap$bldg_id)
  bldgList <- list()
  for (i in c(1:length(bldg_ids))) {
    bldgList[[i]] <- list(bldg_id = bldg_ids[i])
    pins <- overlap@data[overlap$bldg_id == bldg_ids[i],
                         "pin10"]
    bldgList[[i]]$pins <- pins
    area <- overlap@data[overlap$pin10 %in% pins & 
                           overlap$bldg_id == bldg_ids[i],
                         "area"]
    bldgList[[i]]$area <- area
    areaSum <- sum(area)
    areaRatio <- area / areaSum
    bldgList[[i]]$areaRatio <- areaRatio
    pinIndex <- which(areaRatio > .05)
    pinsFinal <- pins[pinIndex]
    bldgList[[i]]$pinsFinal <- pinsFinal
  }
  return(bldgList)
}
  
matchAddress <- function(buildings, addNum, preDir, street, postType) {
  addresses <- buildings@data[,c("bldg_id","f_add1","t_add1","pre_dir1","st_name1","st_type1")]
  addresses$f_add1 <- as.numeric(addresses$f_add1)
  addresses$t_add1 <- as.numeric(addresses$t_add1)
  addresses <- addresses[complete.cases(addresses),]
  addNum <- as.numeric(addNum)
  odd <- addresses[(addresses$f_add1 %% 2) == 1,]
  even <- addresses[(addresses$f_add1 %% 2) == 0,]
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

linkAddress <- function(addresses, buildings) {
  matches <- c()
  for (row in c(1:nrow(addresses))) {
    newMatch <- matchAddress(buildings,
                             addresses$num[row],
                             addresses$pre[row],
                             addresses$street[row],
                             addresses$type[row])
    numMatches <- nrow(newMatch)
    if (numMatches == 1) {
      newRow <- cbind(addresses[row,],
                      "bldg_id" = newMatch$bldg_id,
                      row.names = NULL)
    } else {
      newRow <- cbind(addresses[row,],
                      "bldg_id" = NA,
                      row.names = NULL) 
    }
    matches <- rbind(matches, newRow)
    rm(list = c("newRow","newMatch","row"))
  }
  return(matches)
}

linkByPIP <- function(incoming, taxShapes) {
  unmatched <- is.na(incoming$bldg_id)
  newMatches <- over(incoming[unmatched,], taxShapes)
  incoming[unmatched,"pin"] <- newMatches$pin10
  return(incoming)
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

extractAddress <- function(num, pre, street, type) {
  address <- data.frame(num, pre, street, type)
  dedupedAddress <- unique(address)
  return(dedupedAddress)
}

mergeLinks <- function(spatialData, linkedAddresses, nameNum, namePre, nameStreet, nameType) {
  merged <- merge(spatialData, 
                  linkedAddresses, 
                  by.x = c(nameNum,
                           namePre,
                           nameStreet,
                           nameType),
                  by.y  = c("num",
                            "pre",
                            "street",
                            "type"),
                  all.x = TRUE)
  return(merged)
}

link <- function(data, buildings, taxShapes, nameNum, namePre, nameStreet, nameType) {
  addresses <- extractAddress(data@data[,nameNum],
                              data@data[,namePre],
                              data@data[,nameStreet],
                              data@data[,nameType])
  addresses$num <- as.numeric(as.character(addresses$num))
  linkedAddresses <- linkAddress(addresses, buildings)
  linked1 <- mergeLinks(data, 
                       linkedAddresses,
                       nameNum,
                       namePre,
                       nameStreet,
                       nameType)
  linked2 <- linkByPIP(linked1, taxShapes)
  return(linked2)
}

toSpatial <- function(incoming) {
  coords <- stateplane2latlon(incoming$XCOORD, incoming$YCOORD)
  incoming$longitude <- coords$longitude
  incoming$latitude <- coords$latitude
  incoming <- incoming[complete.cases(incoming[c("longitude",
                                                 "latitude")]),]
  rownames(incoming) <- seq(length=nrow(incoming))
  incomingSP <- SpatialPointsDataFrame(coords =
                                         SpatialPoints(incoming[,c("longitude",
                                                                   "latitude")]),
                                       data = as.data.frame(incoming,
                                                            stringsAsFactors = FALSE))
  proj4string(incomingSP) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  incomingSP <- spTransform(incomingSP, CRS("+init=epsg:4326"))
  return(incomingSP)
}

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
