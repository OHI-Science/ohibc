library(sp) # the classes and methods that make up spatial ops in R
# require(maptools) # tools for reading and manipulating spatial objects
# require(mapdata) # includes good vector maps of world political boundaries.
library(rgeos)
library(rgdal)
library(ggplot2)
#library(gpclib)
library(raster)
# require(scales)
#gpclibPermit()

dir_bc  <- setwd('~/github/ohibc')                              ### set wd to work in Github OHIBC location

source('src/R/common.R')  ### an OHIBC specific version of common.R

dir_rgn <- file.path(dir_bc, 'regions')
rgn_3nm <- readOGR(dsn = dir_rgn, layer = 'ohibc_offshore_3nm')
rgn_1km <- readOGR(dsn = dir_rgn, layer = 'ohibc_inland_1km')
### NOTE: both files are EPSG:3005 NAD83/BC Albers

get_wdpa_poly <- function(reload = FALSE) {
  ### Time consuming... if Canada-specific WDPA shapefile does not yet exist, or 
  ### reload == TRUE, read the main WDPA database, filter to just Canada, and 
  ### save in the Git-Annex OHIBC directory.
  ### If the Canada-specific file already exists, read into memory and return it.
  
  dir_int   <- file.path(dir_neptune_data, 'git-annex/bcprep/lsp/intermediate')
  layer_can <- 'wdpa_canada'
  
  if(!file.exists(file.path(dir_int, paste(layer_can, '.shp', sep = ''))) | reload == TRUE) {
    ### Read most recent WDPA_MPA database from git-annex/globalprep
    wdpa_prod <- 'v2015/raw/WDPA_Jan2015_Public/WDPA_Jan2015_Public.gdb'
    dir_wdpa <- file.path(dir_neptune_data, 'git-annex/globalprep/WDPA_MPA', wdpa_prod)
    
    cat(sprintf('No OHIBC WDPA file.  Reading full WDPA shapefile from:\n  %s, ', dir_wdpa))
    
    wdpa_layer <- ogrListLayers(dir_wdpa) %>% 
      .[str_detect(., 'poly')]
    #   [1] "WDPA_poly_Jan2015"   "WDPA_point_Jan2015"  "WDPA_Source_Jan2015"
    #   attr(,"driver")
    #   [1] "OpenFileGDB"
    #   attr(,"nlayers")
    #   [1] 3
    # wdpa_layer <- 'WDPA_poly_Jan2015'
    cat(sprintf('layer = %s\n', wdpa_layer))
    
    poly_wdpa <- readOGR(dsn = path.expand(dir_wdpa), layer = wdpa_layer)
    ### NOTE: projection is EPSG:4326 WGS 84.
    
    ### Filter down to just polygons within Canada
    cat('Filtering full WDPA shapefile to Canadian regions...\n')
    poly_wdpa_can <- poly_wdpa[poly_wdpa@data$PARENT_ISO == 'CAN' | poly_wdpa@data$ISO3 == 'CAN', ]
    
    cat('Filtering to only STATUS == Designated...\n')
    poly_wdpa_can <- poly_wdpa_can[poly_wdpa@data$STATUS == 'Designated', ]
    
    
    writeOGR(poly_wdpa_can, dsn = dir_int, layer = layer_can, driver="ESRI Shapefile")
    ### NOTE: returns warning:   Field names abbreviated for ESRI Shapefile driver
#         > names(poly_wdpa@data) before writing OGR
#         "WDPAID"       "WDPA_PID"     "NAME"         "ORIG_NAME"    "SUB_LOC"      "DESIG"        "DESIG_ENG"    
#         "DESIG_TYPE"   "IUCN_CAT"     "INT_CRIT"     "MARINE"       "REP_M_AREA"   "GIS_M_AREA"   "REP_AREA"    
#         "GIS_AREA"     "STATUS"       "STATUS_YR"    "GOV_TYPE"     "MANG_AUTH"    "MANG_PLAN"    "NO_TAKE"      
#         "NO_TK_AREA"   "METADATAID"   "PARENT_ISO3"  "ISO3"         "Shape_Length" "Shape_Area"  
#         > names(poly_wdpa_can@data) after writing OGR
#         "WDPAID"  "WDPA_PI" "NAME"    "ORIG_NA" "SUB_LOC" "DESIG"   "DESIG_E" "DESIG_T" "IUCN_CA" 
#         "INT_CRI" "MARINE"  "REP_M_A" "GIS_M_A" "REP_ARE" "GIS_ARE" "STATUS"  "STATUS_" "GOV_TYP" 
#         "MANG_AU" "MANG_PL" "NO_TAKE" "NO_TK_A" "METADAT" "PARENT_" "ISO3"    "Shp_Lng" "Shap_Ar"
  } else {
    cat(sprintf('Reading OHIBC WDPA shapefile from: \n  %s, layer = %s\n', dir_int, layer_can))
    poly_wdpa_can <- readOGR(dsn = dir_int, layer = 'wdpa_canada')
    ### NOTE: projection is EPSG:4326 WGS 84.

    ### Replace original WDPA attribute names:
    names(poly_wdpa_can@data) <- c("WDPAID", "WDPA_PID", "NAME", "ORIG_NAME", "SUB_LOC", "DESIG", "DESIG_ENG",
                                   "DESIG_TYPE", "IUCN_CAT", "INT_CRIT", "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA",
                                   "GIS_AREA", "STATUS", "STATUS_YR", "GOV_TYPE", "MANG_AUTH", "MANG_PLAN", "NO_TAKE",
                                   "NO_TK_AREA", "METADATAID", "PARENT_ISO3", "ISO3", "Shape_Length", "Shape_Area")
  }
  return(poly_wdpa_can)
}

poly_wdpa_can <- get_wdpa_poly()

### project WDPA polygons into BC Albers (equal area projection) from WGS 84
poly_wdpa_bc <- spTransform(poly_wdpa_can, proj4string(rgn_3nm))


### inspect shapefiles
plot(rgn_3nm, col = 'light blue',  border = 'blue')
plot(rgn_1km, col = 'green', border = 'dark green', add = TRUE)
plot(poly_wdpa_bc, col = 'orange', border = 'red', add = TRUE)


prot_area_mar <- raster::intersect(rgn_3nm, poly_wdpa_bc)
prot_area_ter <- raster::intersect(rgn_1km, poly_wdpa_bc)
######### left off here #########

## We want a visual check of the map with the new polygon but
## ggplot requires a data frame, so use the fortify() function

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