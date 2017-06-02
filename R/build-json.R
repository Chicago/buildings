rm(list=ls())

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
#  Import Data
# ---------------------------------------------------------------------------- #

source("R/functions.R")
source("R/buildings.R")
source("R/violations.R")
source("R/tax-parcels.R")
source("R/tax-delinquencies.R")
source("R/vacant-311.R")

# ---------------------------------------------------------------------------- #
#  Initialize bldgList (will become building.json)
# ---------------------------------------------------------------------------- #

bldgList <- initBldgList(buildings, taxShapes)

# ---------------------------------------------------------------------------- #
#  Link records to bldg_ID or PIN
# ---------------------------------------------------------------------------- #

violations <- link(violations, buildings, taxShapes, "HOUSE_LOW", "PRE", "STREET_NAME", "STREET_TYPE")
vacant <- link(vacant, buildings, taxShapes, "address_street_number", "address_street_direction", "address_street_name", "address_street_suffix")

# ---------------------------------------------------------------------------- #
#  Add features to bldgList
# ---------------------------------------------------------------------------- #

## add tax sales

taxsale <- tax_sales[tax_sales$tax_sale_year %in% c(2014,2015),]
newFeature <- createFeature(taxsale)
bldgList <- addFeature(bldgList, newFeature, "taxsale")

## add demo inspections (by building)

demoBldg <- violations@data[violations@data$DEPARTMENT.BUREAU == "DEMOLITION" &
                          violations@data$VIOLATION.DATE > as.Date("2010-01-01") &
                          !is.na(violations@data$bldg_id),
                        c("bldg_id", "VIOLATION.DATE")]
newFeature <- createFeature(demoBldg)
bldgList <- addFeature(bldgList, newFeature, "demoBldg")

## add demo inspections (by PIN)

demoPIN <- violations@data[violations@data$DEPARTMENT.BUREAU == "DEMOLITION" &
                             violations@data$VIOLATION.DATE > as.Date("2010-01-01") &
                             !is.na(violations@data$pin),
                           c("pin", "VIOLATION.DATE")]
newFeature <- createFeature(demoPIN)
bldgList <- addFeature(bldgList, newFeature, "demoPIN")

## add 311 vacant/abandoned building reports (by building)

vacant311Bldg <- vacant@data[as.Date(vacant@data$date_service_request_was_received) > as.Date("2010-01-01") &
                           !is.na(vacant@data$bldg_id),
                         c("bldg_id", "date_service_request_was_received")]
newFeature <- createFeature(vacant311Bldg)
bldgList <- addFeature(bldgList, newFeature, "vacant311Bldg")

## add 311 vacant/abandoned building reports (by PIN)

vacant311PIN <- vacant@data[as.Date(vacant@data$date_service_request_was_received) > as.Date("2010-01-01") &
                              !is.na(vacant@data$pin),
                            c("pin", "date_service_request_was_received")]
newFeature <- createFeature(vacant311PIN)
bldgList <- addFeature(bldgList, newFeature, "vacant311PIN")

## put features into buildings spatial object (for mapping)

extractedFeatures <- lapply(bldgList, function(x) {
  data.frame("bldg_id" = x$bldg_id, 
             "taxsale" = x$taxsale, 
             "demoBldg" = x$demoBldg,
             "demoPIN" = x$demoPIN,
             "vacant311Bldg" = x$vacant311Bldg,
             "vacant311PIN" = x$vacant311PIN,
             stringsAsFactors = FALSE)
})
extractedFeatures <- do.call("rbind", extractedFeatures)
buildings <- merge(buildings, extractedFeatures, by = "bldg_id", all.x = TRUE)

# map out "troubled buildings" using the new feature
bad_buildings <- buildings[(buildings$demoBldg > 0 | buildings$demoPIN > 0),]
tax_sales <- buildings[buildings$taxsale > 0,]
vacant_calls <- buildings[(buildings$vacant311Bldg | buildings$vacant311PIN > 0),]
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
