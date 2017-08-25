
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
  rm(overlap)
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
  bldg_idNAs <- sum(is.na(linked1$bldg_id))
  if (bldg_idNAs > 0) {
    linked2 <- linkByPIP(linked1, taxShapes)
    return(linked2)
  } else {
    linked1$pin <- NA
    return(linked1)
  }
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
  incomingSP <- spTransform(incomingSP, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))
  return(incomingSP)
}

## createFeature expects a data.frame with a list of events, e.g. inspections
## the data needs to have two columns: 1) bldg_id or pin and 2) date of event
## returns a "feature" that can be fed into addFeature
createFeature <- function(df) {
  ## TODO write tests to make sure data is in proper format
  ## TODO handle errors with descriptions of the problem
  if ("pin" %in% names(df)) {
    result_all <- count(df, "pin")
    result_year <- count(df[df$feature_date > (Sys.Date() - 365),], "pin")
    result_90days <- count(df[df$feature_date > (Sys.Date() - 90),], "pin")
    result_30days <- count(df[df$feature_date > (Sys.Date() - 30),], "pin")
  }
  if ("bldg_id" %in% names(df)) {
    result_all <- count(df, "bldg_id")
    result_year <- count(df[df$feature_date > (Sys.Date() - 365),], "bldg_id") 
    result_90days <- count(df[df$feature_date > (Sys.Date() - 90),], "bldg_id")
    result_30days <- count(df[df$feature_date > (Sys.Date() - 30),], "bldg_id")
  }
  return(list(list("result_all" = result_all,
                   "result_year" = result_year,
                   "result_90days" = result_90days,
                   "result_30days" = result_30days),
              df))
}

addFeature <- function(bldgList, feature_list, feature_name) {
  df_summary_list <- feature_list[[1]]
  df_full <- feature_list[[2]]
  result_year <- df_summary_list$result_year
  result_all <- df_summary_list$result_all
  result_90days <- df_summary_list$result_90days
  result_30days <- df_summary_list$result_30days
  ## match by bldg_id if available, pin if not
  if ("bldg_id" %in% names(result_all)) {
    matchType <- "bldg_id" 
  } else if ("pin" %in% names(result_all)) {
    matchType <- "pin" 
  } else stop("No bldg_id or pin found, cannot match records to a building")
  
  if (matchType == "bldg_id") {
    for (i in c(1:length(bldgList))) {
      count_all <- 0
      count_year <- 0
      count_90days <- 0
      count_30days <- 0
      bldg_id <- bldgList[[i]]$bldg_id
      
      # add summary for all results 
      found_all <- result_all[result_all$bldg_id == bldg_id,
                                "freq"] 
      if (length(found_all) > 0) {
        count_all <- count_all + found_all
      }
      bldgList[[i]][[paste0(feature_name, ".alltime")]] <- count_all
      
      # add summary for results in the last year
      found_year <- result_year[result_year$bldg_id == bldg_id,
                          "freq"] 
      if (length(found_year) > 0) {
        count_year <- count_year + found_year
      }
      bldgList[[i]][[paste0(feature_name, ".pastyear")]] <- count_year
      
      # add summary for results in the 90 days 
      found_90days <- result_90days[result_90days$bldg_id == bldg_id,
                                "freq"] 
      if (length(found_90days) > 0) {
        count_90days <- count_90days + found_90days
      }
      bldgList[[i]][[paste0(feature_name, ".past90days")]] <- count_90days
      
      # add summary for results in the 30 days
      found_30days <- result_30days[result_30days$bldg_id == bldg_id,
                                "freq"] 
      if (length(found_30days) > 0) {
        count_30days <- count_30days + found_30days
      }
      bldgList[[i]][[paste0(feature_name, ".past30days")]] <- count_30days
      
      # add all matched rows to the building list
      rows <- df_full[df_full$bldg_id == bldg_id,]
      bldgList[[i]][[feature_name]] <- rows  
    }    
    result <- bldgList
  }
  
  if (matchType == "pin") {
    for (i in c(1:length(bldgList))) {
      count_all <- 0
      count_year <- 0
      count_90days <- 0
      count_30days <- 0
      pins <- bldgList[[i]]$pinsFinal
      rows <- c()
      for (j in 1:length(pins)) {
        
        # add summary for all results 
        found_all <- result_all[result_all$pin == pins[j],
                                  "freq"] 
        if (length(found_all) > 0) {
          count_all <- count_all + found_all
        }
        
        # add summary for results in the last year
        found_year <- result_year[result_year$pin == pins[j],
                            "freq"] 
        if (length(found_year) > 0) {
          count_year <- count_year + found_year
        }
        
        # add summary for results in the 90 days 
        found_90days <- result_90days[result_90days$bldg_id == pins[j],
                                      "freq"] 
        if (length(found_90days) > 0) {
          count_90days <- count_90days + found_90days
        }
        
        # add summary for results in the 30 days
        found_30days <- result_30days[result_30days$bldg_id == pins[j],
                                      "freq"] 
        if (length(found_30days) > 0) {
          count_30days <- count_30days + found_30days
        }
        
        newRow <- df_full[df_full$pin == pins[j],]
        rows <- rbind(rows, newRow)
      } 
      bldgList[[i]][[paste0(feature_name, ".alltime")]] <- count_all
      bldgList[[i]][[paste0(feature_name, ".pastyear")]] <- count_year
      bldgList[[i]][[paste0(feature_name, ".past90days")]] <- count_90days
      bldgList[[i]][[paste0(feature_name, ".past30days")]] <- count_30days
      
      # add all matched rows to the building list
      bldgList[[i]][[feature_name]] <- rows
    }    
    result <- bldgList
  }
  
  return(result)
}

writeTracts <- function(spatialData, tracts, description) {
  tractsList <- tracts$name10
  sapply(tractsList, function(x) {
    tract <- tracts[tracts$name10 == x,]
    spatialData_df <- over(spatialData, tract)
    spatialData_df <- spatialData_df[complete.cases(spatialData_df),]
    rowNums <- as.numeric(rownames(spatialData_df))
    spatialData <- spatialData[c(rowNums),]
    writeOGR(spatialData,
             paste0("data/byTract/", description, "/", x,".geojson"),
             layer = "OGRGeoJSON",
             driver = "GeoJSON")
  })
}
