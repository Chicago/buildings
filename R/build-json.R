## check out buildings in 7709.02, 8214.02 - seems to be missing buildings
## "Partially outside City Boundary"
## also broke: 3817, 605


rm(list=ls())

# ---------------------------------------------------------------------------- #
#  A script to add features and aggregate data for Chicago buildings
# 
#  Author: Nick Lucius
# ---------------------------------------------------------------------------- #

library(data.table)
library(rgeos)
library(rgdal)
library(leaflet)
library(RSocrata)
library(plyr)
library(raster)
library(stringr)
library(jsonlite)

# ---------------------------------------------------------------------------- #
#  Import Data
# ---------------------------------------------------------------------------- #

source("R/functions.R")
source("R/tracts.R")
source("R/tax-delinquencies.R")

# ---------------------------------------------------------------------------- #
#  Initialize bldgList (will become building.json)
# ---------------------------------------------------------------------------- #

which(tracts$name10 %in% c("7709.02", "8214.02", "3817"))
# [1] 181 313 415
# tractsList <- tracts$name10[c(1:180,182:312,314:414,416:724,726:801)]

# small focus in district 11
which(tracts$name10 == "2608")
tractsList <- tracts$name10[736]

for (i in 1:length(tractsList)) {
  tractToPass <- tractsList[i]
  source("R/buildings.R")
  source("R/violations.R")
  source("R/tax-parcels.R")
  source("R/vacant-311.R")
  
  bldgList <- initBldgList(buildings, taxShapes)
  
  # ---------------------------------------------------------------------------- #
  #  Link records to bldg_ID or PIN
  # ---------------------------------------------------------------------------- #
  
  violations <- link(violations, buildings, taxShapes, "HOUSE_LOW", "PRE", "STREET_NAME", "STREET_TYPE")
  if (!class(vacant) == "try-error") {
    vacant <- link(vacant, buildings, taxShapes, "ADDRESS.STREET.NUMBER", "ADDRESS.STREET.DIRECTION", "ADDRESS.STREET.NAME", "ADDRESS.STREET.SUFFIX")
  }
  # ---------------------------------------------------------------------------- #
  #  Add features to bldgList
  # ---------------------------------------------------------------------------- #
  
  ## add tax sales
  annual_sale$feature_date <- as.Date(annual_sale$tax_sale_year, format = "%Y")
  # annual tax sale is lagged, so pad with 3 years
  annual_sale$feature_date <- annual_sale$feature_date + 1095
  newFeature <- createFeature(annual_sale)
  bldgList <- addFeature(bldgList, newFeature, "annual_sale")
  
  scavenger_sale$feature_date <- as.Date(scavenger_sale$tax_sale_year, format = "%Y")
  # scavenger tax sale is lagged, so pad with 3 years
  scavenger_sale$feature_date <- scavenger_sale$feature_date + 730  
  newFeature <- createFeature(scavenger_sale)
  bldgList <- addFeature(bldgList, newFeature, "scavenger_sale")
  
  ## add demo inspections (by building)
  
  demoBldg <- violations@data[violations@data$DEPARTMENT.BUREAU == "DEMOLITION" &
                                !is.na(violations@data$bldg_id),]
  demoBldg$feature_date <- as.Date(demoBldg$VIOLATION.DATE)
  newFeature <- createFeature(demoBldg)
  bldgList <- addFeature(bldgList, newFeature, "demoBldg")
  
  ## add demo inspections (by PIN)
  
  demoPIN <- violations@data[violations@data$DEPARTMENT.BUREAU == "DEMOLITION" &
                               !is.na(violations@data$pin),]
  demoPIN$bldg_id <- NULL
  demoPIN$feature_date <- as.Date(demoPIN$VIOLATION.DATE)
  newFeature <- createFeature(demoPIN)
  bldgList <- addFeature(bldgList, newFeature, "demoPIN")
  
  ## add 311 vacant/abandoned building reports (by building)
  if (!class(vacant) == "try-error") {  
    vacant311Bldg <- vacant@data[!is.na(vacant@data$bldg_id),]
    vacant311Bldg$feature_date <- as.Date(vacant311Bldg$DATE.SERVICE.REQUEST.WAS.RECEIVED)
    newFeature <- createFeature(vacant311Bldg)
    bldgList <- addFeature(bldgList, newFeature, "vacant311Bldg")
  
    ## add 311 vacant/abandoned building reports (by PIN)
  
    vacant311PIN <- vacant@data[!is.na(vacant@data$pin),]
    vacant311PIN$bldg_id <- NULL
    vacant311PIN$feature_date <- as.Date(vacant311PIN$DATE.SERVICE.REQUEST.WAS.RECEIVED)
    newFeature <- createFeature(vacant311PIN)
    bldgList <- addFeature(bldgList, newFeature, "vacant311PIN")
  }
  bldgList <- toJSON(bldgList)
  write(bldgList,
        paste0("data/json/", tractToPass,".json"))
  rm(buildings)
  rm(taxShapes)
  rm(bldgList)
}

# combinedJSON <- c()
# for (i in 1:length(tractsList)) {
#   tractToPass <- tractsList[i]
#   path <- paste0("data/json/", tractToPass,".json")
#   singleJSON <- read_json(path,
#                           simplifyDataFrame = TRUE)
#   combinedJSON <- rbind.fill(combinedJSON, singleJSON)
# }
# 
# 
# 
# saveRDS(combinedJSON, "data/Rds/combinedJSON.Rds")
combinedJSON <- readRDS("data/Rds/combinedJSON.Rds")
## deal with missing values
combinedJSON[["vacant311PIN.alltime"]] <- sapply(combinedJSON[["vacant311PIN.alltime"]], function(x) {if (is.null(x)) 0 else x})
combinedJSON[["vacant311PIN.pastyear"]] <- sapply(combinedJSON[["vacant311PIN.pastyear"]], function(x) {if (is.null(x)) 0 else x})
combinedJSON[["vacant311PIN.past90days"]] <- sapply(combinedJSON[["vacant311PIN.alltime"]], function(x) {if (is.null(x)) 0 else x})
combinedJSON[["vacant311PIN.past30days"]] <- sapply(combinedJSON[["vacant311PIN.pastyear"]], function(x) {if (is.null(x)) 0 else x})
combinedJSON[["vacant311Bldg.alltime"]] <- sapply(combinedJSON[["vacant311Bldg.alltime"]], function(x) {if (is.null(x)) 0 else x})
combinedJSON[["vacant311Bldg.pastyear"]] <- sapply(combinedJSON[["vacant311Bldg.pastyear"]], function(x) {if (is.null(x)) 0 else x})
combinedJSON[["vacant311Bldg.past90days"]] <- sapply(combinedJSON[["vacant311Bldg.past90days"]], function(x) {if (is.null(x)) 0 else x})
combinedJSON[["vacant311Bldg.past30days"]] <- sapply(combinedJSON[["vacant311Bldg.past30days"]], function(x) {if (is.null(x)) 0 else x})
buildings <- readRDS("gitexclude/buildings.Rds")
buildings_df <- buildings@data
buildings_df$address <- paste0(buildings$label_hous, " ", buildings$pre_dir1,
                               " ", buildings$st_name1, " ", buildings$st_type1)
combinedJSON$bldg_id <- unlist(combinedJSON$bldg_id)
# combinedJSON$demo <- unlist(combinedJSON$demoBldg) + unlist(combinedJSON$demoPIN)
# combinedJSON$vacant311 <- unlist(combinedJSON$vacant311Bldg) + unlist(combinedJSON$vacant311PIN)
# combinedJSON$taxsale <- unlist(combinedJSON$taxsale)
# combinedJSON$demoBldg <- NULL
# combinedJSON$demoPIN <- NULL
# combinedJSON$vacant311Bldg <- NULL
# combinedJSON$vacant311PIN <- NULL
combinedJSON <- merge(combinedJSON, buildings_df, by = "bldg_id")
combinedJSON <- combinedJSON[!combinedJSON$address == "0   ",]
combinedJSON <- combinedJSON[!combinedJSON$address == "   ",]


## structure and reorder before saving as JSON
## more work needed here

combinedJSON <- combinedJSON[!duplicated(combinedJSON$bldg_id),]
write(toJSON(combinedJSON, pretty = TRUE), "notebooks/building_test.json")


buildings <- buildings[!duplicated(buildings$bldg_id),]
mergeBy <- intersect(names(combinedJSON), names(buildings))
combinedGeoJSON <- merge(buildings, combinedJSON, by = mergeBy, all = FALSE)

# writeOGR(combinedGeoJSON, "buildings-test.geojson", "OGRGeoJSON", driver="GeoJSON")
# the above errors out. make a non-nested version just to move forward?

# saveRDS(combinedGeoJSON, "notebooks/combinedGeoJSON.Rds")
combinedGeoJSON <- readRDS("notebooks/combinedGeoJSON.Rds")

## following is for creating final JSON prototype

testGeoJSONdf <- data.frame(Address = combinedGeoJSON$address)
coords <- combinedGeoJSON@data[,c("x_coord","y_coord")]
for (i in 1:nrow(coords)) {testGeoJSONdf$Coordinates[[i]] <- list(x_coord = coords$x_coord[i], y_coord = coords$y_coord[i])}
for (i in 1:nrow(combinedGeoJSON@data)) {testGeoJSONdf$Summary[[i]] <- 
  list(TaxSales = combinedGeoJSON@data[i,c("annual_sale.alltime", 
                                           "annual_sale.pastyear",
                                           "annual_sale.past90days",
                                           "annual_sale.past30days",
                                           "scavenger_sale.alltime",
                                           "scavenger_sale.pastyear",
                                           "scavenger_sale.past90days",
                                           "scavenger_sale.past30days")],
       VacancyReports = combinedGeoJSON@data[i,c("vacant311Bldg.alltime",
                                                 "vacant311Bldg.pastyear",
                                                 "vacant311Bldg.past90days", 
                                                 "vacant311Bldg.past30days",
                                                 "vacant311PIN.alltime",     
                                                 "vacant311PIN.pastyear",
                                                 "vacant311PIN.past90days",
                                                 "vacant311PIN.past30days")],
       DemolitionViolations = combinedGeoJSON@data[i,c("demoBldg.alltime",
                                                       "demoBldg.pastyear",
                                                       "demoBldg.past90days", 
                                                       "demoBldg.past30days",
                                                       "demoPIN.alltime",     
                                                       "demoPIN.pastyear",
                                                       "demoPIN.past90days",
                                                       "demoPIN.past30days")])}
for (i in 1:nrow(combinedGeoJSON@data)) {testGeoJSONdf$Metadata[[i]]$attributes <- 
  combinedGeoJSON@data[i,c( "bldg_id",
                            "no_stories",              
                            "suf_dir1",
                            "x_coord",                
                            "shape_area",
                            "year_built",               
                            "cdb_city_i",
                            "stories",                  
                            "y_coord",
                            "unit_name",                
                            "qc_date",
                            "edit_date",                
                            "non_standa",
                            "label_hous",               
                            "st_name1",
                            "t_add1",                   
                            "z_coord",                   
                            "qc_userid",                
                            "qc_source",                 
                            "orig_bldg_",               
                            "footprint_",                
                            "st_type1",                 
                            "bldg_statu",                
                            "condition_",               
                            "bldg_name2",                
                            "bldg_activ",               
                            "edit_useri",                
                            "bldg_sq_fo",               
                            "edit_sourc",                
                            "f_add1",                   
                            "vacancy_st",                
                            "bldg_creat",               
                            "no_of_unit",                
                            "bldg_end_d",               
                            "create_use",                
                            "demolished",               
                            "pre_dir1",                  
                            "bldg_condi",               
                            "bldg_name1",                
                            "comments",                 
                            "shape_len",                 
                            "harris_str",               
                            "pins",                      
                            "area",                     
                            "areaRatio",                 
                            "pinsFinal"   
  )]}
for (i in 1:nrow(combinedGeoJSON@data)) {testGeoJSONdf$TaxSales[[i]] <- 
  combinedGeoJSON@data[i,c("annual_sale", "scavenger_sale")]}
for (i in 1:nrow(combinedGeoJSON@data)) {testGeoJSONdf$DemoViolations[[i]] <- 
  combinedGeoJSON@data[i,c("demoBldg", "demoPIN")]}
for (i in 1:nrow(combinedGeoJSON@data)) {testGeoJSONdf$VacancyReports[[i]] <- 
  combinedGeoJSON@data[i,c("vacant311Bldg", "vacant311PIN")]}

toJSON(testGeoJSONdf, pretty = TRUE)
write(toJSON(testGeoJSONdf, pretty = TRUE), "test.json")


