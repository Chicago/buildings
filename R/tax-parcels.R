# ---------------------------------------------------------------------------- #
#  Import all Cook County tax parcel data
# ---------------------------------------------------------------------------- #

# import one large tax shapes file
# make 3 groupings of census tracts
# split tax shapes into 3 files based on census tract groups
# for each file, use over ONLY on that census tract grouping

## tractToPass (incoming variable) 

# taxShapes <- readOGR(dsn="gitexclude/parcels.geojson", layer="OGRGeoJSON", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------- #
#  Create a geojson file for each census tract 
# ---------------------------------------------------------------------------- #

# tracts1 <- tracts[c(1:266),]
# tracts2 <- tracts[c(267:532),]
# tracts3 <- tracts[c(533:801),]
 
# taxShapes1_df <- over(taxShapes, tracts1)
# taxShapes1_df <- taxShapes1_df[complete.cases(taxShapes1_df),]
# rowNums <- as.numeric(rownames(taxShapes1_df))
# taxShapes1 <- taxShapes[c(rowNums),]
# 
# taxShapes2_df <- over(taxShapes, tracts2)
# taxShapes2_df <- taxShapes2_df[complete.cases(taxShapes2_df),]
# rowNums <- as.numeric(rownames(taxShapes2_df))
# taxShapes2 <- taxShapes[c(rowNums),]
# 
# taxShapes3_df <- over(taxShapes, tracts3)
# taxShapes3_df <- taxShapes3_df[complete.cases(taxShapes3_df),]
# rowNums <- as.numeric(rownames(taxShapes3_df))
# taxShapes3 <- taxShapes[c(rowNums),]

# saveRDS(taxShapes1, "data/tax-shapes1.Rds")
# saveRDS(taxShapes2, "data/tax-shapes2.Rds")
# saveRDS(taxShapes3, "data/tax-shapes3.Rds")
# taxShapes1 <- readRDS("data/tax-shapes1.Rds")
# taxShapes2 <- readRDS("data/tax-shapes2.Rds")
# taxShapes3 <- readRDS("data/tax-shapes3.Rds")
                  
# writeTracts(spatialData = taxShapes1,
#             tracts = tracts1,
#             description = "taxShapes")
                
# ---------------------------------------------------------------------------- #
#  Read file
# ---------------------------------------------------------------------------- #

path <- paste0("data/byTract/taxShapes/", tractToPass,".geojson")
taxShapes <- readOGR(path, layer="OGRGeoJSON",
                     stringsAsFactors = FALSE)

taxShapes <- spTransform(taxShapes, 
                         CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))

# simplify the polgons a tad (tweak 0.00001 to your liking)
taxShapes_df <- taxShapes@data
taxShapes <- gSimplify(taxShapes, tol = 0.00001)
taxShapes <- SpatialPolygonsDataFrame(taxShapes, taxShapes_df)

# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
taxShapes <- gBuffer(taxShapes, byid=TRUE, width=0)

# any bad polys?
sum(gIsValid(taxShapes, byid=TRUE)==FALSE)
