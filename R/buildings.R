# ---------------------------------------------------------------------------- #
#  Import all Chicago building data
# ---------------------------------------------------------------------------- #

## tractToPass (incoming variable) 

# buildings <- readOGR(dsn="data/Buildings.geojson", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------- #
#  Create a geojson file for each census tract 
# ---------------------------------------------------------------------------- #

# writeTracts(spatialData = buildings, 
#             tracts = tracts, 
#             description = "buildings")

# ---------------------------------------------------------------------------- #
#  Read file
# ---------------------------------------------------------------------------- #

path <- paste0("data/byTract/buildings/", tractToPass,".geojson")
buildings <- readOGR(path, layer="OGRGeoJSON",
                     stringsAsFactors = FALSE)

# following fixes https://gis.stackexchange.com/questions/163445/r-solution-for-topologyexception-input-geom-1-is-invalid-self-intersection-er

buildings <- spTransform(buildings, 
                         CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))

# simplify the polgons a tad (tweak 0.00001 to your liking)
buildings_df <- buildings@data
buildings <- gSimplify(buildings, tol = 0.00001)
buildings <- SpatialPolygonsDataFrame(buildings, buildings_df)

# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
buildings <- gBuffer(buildings, byid=TRUE, width=0)

# any bad polys?
sum(gIsValid(buildings, byid=TRUE)==FALSE)
