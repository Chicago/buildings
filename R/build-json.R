args <- commandArgs(TRUE)
runNumber <- as.numeric(args[1])
runRange <- c(((runNumber - 1) * 100) + 1, runNumber * 100)

## check out buildings in 7709.02, 8214.02 - seems to be missing buildings
## "Partially outside City Boundary"
## also broke: 3817, 605


# ---------------------------------------------------------------------------- #
#  A script to add features and aggregate data for Chicago buildings
# 
#  Author: Nick Lucius
# ---------------------------------------------------------------------------- #

library(data.table)
library(rgeos, lib.loc = "/usr/lib64/R/library")
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

source("R/tracts.R")
source("R/functions.R")

# ---------------------------------------------------------------------------- #
#  Initialize bldgList (will become building.json)
# ---------------------------------------------------------------------------- #

which(tracts$name10 %in% c("7709.02", "8214.02", "3817"))
# [1] 181 313 415
tractsList <- tracts$name10[c(1:180,182:312,314:414,416:724,726:801)]
tractsList <- tractsList[runRange[1]:runRange[2]]

# small focus in district 11
# which(tracts$name10 == "2608")
# tractsList <- tracts$name10[736]

buildJSON(tracts, tractsList)
