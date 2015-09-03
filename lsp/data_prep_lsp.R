require(sp) # the classes and methods that make up spatial ops in R
require(maptools) # tools for reading and manipulating spatial objects
require(mapdata) # includes good vector maps of world political boundaries.
require(rgeos)
require(rgdal)
require(gpclib)
require(ggplot2)
require(scales)
gpclibPermit()

## Download the Ordnance Survey Boundary-Line data (large!) from this URL:
## https://www.ordnancesurvey.co.uk/opendatadownload/products.html
## then extract all the files to a local folder.
## Read the electoral division (ward) boundaries from the shapefile
dir_bc  <- setwd('~/github/ohibc')

source('~/github/ohiprep/src/R/common.R')

dir_rgn <- file.path(dir_bc, 'regions')

### Read OHIBC shapefiles: 1 km inland, 3 nm offshore.
rgn_3nm <- readOGR(dsn = dir_rgn, layer = 'ohi_offshore_3nm')
rgn_1km <- readOGR(dsn = dir_rgn, layer = 'ohi_inland_1km')

### Read most recent WDPA_MPA database from git-annex/globalprep
wdpa_prod <- 'v2015/raw/WDPA_Jan2015_Public/WDPA_Jan2015_Public.gdb'
dir_wdpa <- file.path(dir_neptune_data, 'git-annex/globalprep/WDPA_MPA', wdpa_prod)

wdpa_layer <- ogrListLayers(dir_wdpa) %>% 
  .[str_detect(., 'poly')]
#   [1] "WDPA_poly_Jan2015"   "WDPA_point_Jan2015"  "WDPA_Source_Jan2015"
#   attr(,"driver")
#   [1] "OpenFileGDB"
#   attr(,"nlayers")
#   [1] 3

poly_wdpa <- readOGR(dsn = dir_wdpa, layer = wdpa_layer)

### Filter down to just polygons within Canada
poly_wdpa_can <- poly_wdpa[poly_wdpa@data$PARENT_ISO == 'CAN' | poly_wdpa@data$ISO3 == 'CAN', ]


## We want a visual check of the map with the new polygon but
## ggplot requires a data frame, so use the fortify() function
mydf <- fortify(shp4, region = "NAME")


## Now plot
ggplot(mydf, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "black", size = 1, aes(fill = mydf$filltype)) +
  scale_fill_manual("Test", values = c(alpha("Red", 0.4), "white"), labels = c("a", "b"))

## Visual check, successful, so back to the original problem of finding intersections
overlaid.poly <- 6 # This is the index of the polygon we added
num.of.polys <- length(shp4@polygons)
all.polys <- 1:num.of.polys
all.polys <- all.polys[-overlaid.poly] # Remove the overlaid polygon - no point in comparing to self
all.polys <- all.polys[-1] ## In this case the visual check we did shows that the
## first polygon doesn't intersect overlaid poly, so remove

## Display example intersection for a visual check - note use of SpatialPolygons()
plot(gIntersection(SpatialPolygons(shp4@polygons[3]), SpatialPolygons(shp4@polygons[6])))

## Calculate and print out intersecting area as % total area for each polygon
areas.list <- sapply(all.polys, function(x) {
  my.area <- shp4@polygons[[x]]@Polygons[[1]]@area # the OS data contains area
  intersected.area <- gArea(gIntersection(SpatialPolygons(shp4@polygons[x]), SpatialPolygons(shp4@polygons[overlaid.poly])))
  print(paste(shp4@data$NAME[x], " (poly ", x, ") area = ", round(my.area, 1), ", intersect = ", round(intersected.area, 1), ", intersect % = ", sprintf("%1.1f%%", 100*intersected.area/my.area), sep = ""))
  return(intersected.area) # return the intersected area for future use
})