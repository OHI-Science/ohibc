library(sp)        # the classes and methods that make up spatial ops in R
library(gdalUtils) # for gdal_rasterize() function
library(maptools)  # tools for reading and manipulating spatial objects
library(rgeos)
library(rgdal)
library(raster)


dir_bc  <- setwd('~/github/ohibc')                              ### set wd to work in Github OHIBC location

source('src/R/common.R')  ### an OHIBC specific version of common.R

###########################################################=
### Get rgn and WDPA polygons -----
###########################################################=
dir_rgn <- file.path(dir_bc, 'regions')

rgn_3nm <- readOGR(dsn = dir_rgn, layer = 'ohibc_offshore_3nm')
rgn_1km <- readOGR(dsn = dir_rgn, layer = 'ohibc_inland_1km')
### NOTE: both files are EPSG:3005 NAD83/BC Albers

### Check geometries:
#gIsValid(rgn_3nm); gIsValid(rgn_1km)

### NOTE: Can skip down to 'get_protected_poly' if this has already been done
get_wdpa_poly <- function(proj4string = NULL, reload = FALSE) {
  ### Time consuming... if BC-specific WDPA shapefile does not yet exist, or 
  ### reload == TRUE, read the main WDPA database, filter to just BC, and 
  ### save in the Git-Annex OHIBC directory.
  ### If the BC-specific file already exists, read into memory and return it.
  
  dir_int   <- file.path(dir_neptune_data, 'git-annex/bcprep/lsp/intermediate')
  layer_bc <- 'wdpa_bc'
  proj4s_wdpa <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  ### NOTE: projection is EPSG:4326 WGS 84 for the WDPA layers.
  
  if(!file.exists(file.path(dir_int, paste(layer_bc, '.shp', sep = ''))) | reload == TRUE) {
    ### Read most recent WDPA_MPA database from git-annex/globalprep
    wdpa_prod <- 'v2015/raw/WDPA_Jan2015_Public/WDPA_Jan2015_Public.gdb'
    dir_wdpa <- file.path(dir_neptune_data, 'git-annex/globalprep/WDPA_MPA', wdpa_prod)
    
    cat(sprintf('Creating new OHIBC WDPA file.  Reading full WDPA shapefile from:\n  %s, ', dir_wdpa))
    
    layer_wdpa <- ogrListLayers(dir_wdpa) %>% 
      .[str_detect(., 'poly')]
    #   [1] "WDPA_poly_Jan2015"   "WDPA_point_Jan2015"  "WDPA_Source_Jan2015"
    #   attr(,"driver")
    #   [1] "OpenFileGDB"
    #   attr(,"nlayers")
    #   [1] 3
    # layer_wdpa <- 'WDPA_poly_Jan2015'
    cat(sprintf('layer = %s\n', layer_wdpa))
    
    poly_wdpa <- readOGR(dsn = dir_int, layer = layer_wdpa)
    ### can't use readShapePoly on a .gdb database...
    
    ### Filter down to just polygons within Canada
    cat('Filtering full WDPA shapefile to Canadian regions...\n')
    poly_wdpa_bc <- poly_wdpa[poly_wdpa@data$PARENT_ISO == 'CAN' | poly_wdpa@data$ISO3 == 'CAN', ]
    poly_wdpa_bc <- poly_wdpa_bc[toupper(poly_wdpa_bc@data$SUB_LOC) %in% c('CA-BC', 'MARINE'), ]
    
    cat('Filtering to only STATUS == Designated...\n')
    poly_wdpa_bc <- poly_wdpa_bc[poly_wdpa@data$STATUS == 'Designated', ]
    
    writePolyShape(poly_wdpa_bc, fn = file.path(dir_int, layer_bc))
    
  } else {
    
    cat(sprintf('Reading OHIBC WDPA shapefile from: \n  %s, layer = %s\n', dir_int, layer_bc))
    poly_wdpa_bc <- readShapePoly(file.path(dir_int, layer_bc), proj4string = CRS(proj4s_wdpa))
    ### NOTE: projection is EPSG:4326 WGS 84 (+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0)
    
  }
  return(poly_wdpa_bc)
}

poly_wdpa_bc <- get_wdpa_poly()

### project WDPA polygons into BC Albers (equal area projection) from WGS 84
poly_wdpa_bc <- spTransform(poly_wdpa_bc, proj4string(rgn_3nm))


###########################################################=
### Get protected areas in offshore and inland buffers -----
###########################################################=
get_protected_poly <- function(wdpa_poly, region_poly, rgn_type, reload = FALSE) {
  ### rgn_type is either 'mar' or 'ter' for marine or terrestrial
  
  dir_int   <- file.path(dir_neptune_data, 'git-annex/bcprep/lsp/intermediate')
  prot_layer <- sprintf('prot_area_%s', rgn_type)
  if(!(rgn_type %in% c('mar', 'ter'))) {
    error('Unrecognized region type: either "mar" for marine or "ter" for terrestrial!\n')
  }
  rgn_fullname <- ifelse(rgn_type == 'mar', 'offshore 3nm', 'inland 1 km')
  
  if(!file.exists(sprintf('%s/%s.shp', dir_int, prot_layer))) {
    cat(sprintf('Creating offshore protected area shapefile by intersecting protected areas with %s buffer.\n', rgn_fullname))
    prot_area_poly <- raster::intersect(region_poly, wdpa_poly)
    writeOGR(prot_area_poly, dsn = dir_int, layer = prot_layer, driver="ESRI Shapefile")
    ### Warning: In writeOGR(prot_area_ter, dsn = dir_int, layer = "prot_area_ter",  : Field names abbreviated for ESRI Shapefile driver
    ### original field names:
    #   [1] "rgn_name"     "rgn_id"       "rgn_code"     "WDPAID"       "WDPA_PID"     "NAME"         "ORIG_NAME"    "SUB_LOC"      "DESIG"        "DESIG_ENG"    "DESIG_TYPE"   "IUCN_CAT"     "INT_CRIT"     "MARINE"      
    #   [15] "REP_M_AREA"   "GIS_M_AREA"   "REP_AREA"     "GIS_AREA"     "STATUS"       "STATUS_YR"    "GOV_TYPE"     "MANG_AUTH"    "MANG_PLAN"    "NO_TAKE"      "NO_TK_AREA"   "METADATAID"   "PARENT_ISO3"  "ISO3"        
    #   [29] "Shape_Length" "Shape_Area"  
  } else {
    cat(sprintf('Reading %s protected areas from: \n %s/%s\n', rgn_fullname, dir_int, prot_layer))
    prot_area_poly <- readOGR(dsn = dir_int, layer = prot_layer)
    
    ### restore full attribute names
    names(prot_area_poly@data) <- c("rgn_name", "rgn_id", "rgn_code", "WDPAID", "WDPA_PID", "NAME", "ORIG_NAME", "SUB_LOC",
                                   "DESIG", "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "INT_CRIT", "MARINE", "REP_M_AREA", 
                                   "GIS_M_AREA", "REP_AREA", "GIS_AREA", "STATUS", "STATUS_YR", "GOV_TYPE", "MANG_AUTH", 
                                   "MANG_PLAN", "NO_TAKE", "NO_TK_AREA", "METADATAID", "PARENT_ISO3", "ISO3", 
                                   "Shape_Length", "Shape_Area")
  }
  prot_area_poly@data <- prot_area_poly@data %>%
    select(rgn_name, rgn_id, ORIG_NAME, DESIG_ENG, STATUS_YR, IUCN_CAT)
  return(prot_area_poly)
}

prot_area_mar_orig <- get_protected_poly(poly_wdpa_bc, rgn_3nm, rgn_type = 'mar', reload = FALSE)
prot_area_ter_orig <- get_protected_poly(poly_wdpa_bc, rgn_1km, rgn_type = 'ter', reload = FALSE)

### Check geometries:
# gIsValid(prot_area_mar_orig); gIsValid(prot_area_ter_orig) ### should return TRUE TRUE but takes a long time


### make sure there are no NA occurrences in the status year field - each protected area has a year associated
if(sum(is.na(prot_area_mar_orig@data$STATUS_YR)) != 0 | sum(is.na(prot_area_ter_orig@data$STATUS_YR)) != 0) 
  warning('STATUS_YR contains NAs which should be addressed')

# plot(rgn_3nm, col = 'light blue',  border = 'blue')
# plot(rgn_1km, col = 'green',       border = NA, add = TRUE)
# plot(prot_area_mar_orig, col = rgb(1, .75, .5, .5), border = NA, add = TRUE)
# plot(prot_area_ter_orig, col = rgb(.75, .5, 1, .5), border = NA, add = TRUE)

###########################################################=
### Eliminate sliver polygons -----
###########################################################=
### Lots of tiny polygons result from inexact matches of WDPA boundaries and
### OHIBC buffer regions, especially along shorelines.  These slow down
### processing and may create issues with overlap; but total area will be
### inconsequential to score...
###
# sapply(prot_area_mar@polygons, function(x) length(x@Polygons))
# #       [1]    13    30    23     1    71    22    14     3     2   335     2     1     3     5     1    17     1     9     1     1     5     6    14   322   244     2     2     5     3     1     2     9     4    19     5
# #      [36]    10     6     4     1    28   506     2     3    44    15    23   113    76   139     7    37    26   140    44    39     3     2     1    38     5     2     7     2    45   131    46     2     1     1    68
# #      [71]     9     1     1     1    12     1     2     2    98     3    30     7     2    22     1     1    28     7     1    12     1     1     1     5    48     1     1     1     2     1     1   119     2    31     1
# #     [106]     1     1     2     5     2     3    10    21     5     5     5     1    94     2     1   139   239    25     1    75  1543     1     2     1     1     1     1     1     2     2     2    86     1     1     1
# #     [141]     2    29     1  1894     2    55     2    13    23     3  1657     8     1     5    98     1     2     1     4     1     2     1     1     1    60     4     2     2    25  1919 24146     2   121  1073   300
# #     [176]  8288    12     4   590    69    93   290   677  9491     2     1   892   295     8   913     1     2    11     3     1     1     2   135     1     3   290   327  1269   122    46    51    37  1774    26  2403
# #     [211]   135  7854   935  1309  1241    94   458  7618   121     1     1    10     1     1     1     1   594   187    32     1    18     1   601     5   753     1    13    78     3     1     2     2   254     1    13
# #     [246]     1     3     8    98     2     1     1     3    34     2     1    66    42    20  1213   209   224   358   152    63    47    82     6     3     2     2     3    27     1     6     3   387    10    34   264
# #     [281]   176    68   206
# length(prot_area_mar@polygons[[171]]@Polygons)
# #       [1] 24146
# 
# area <- lapply(prot_area_mar@polygons, function(x) sapply(x@Polygons, function(y) y@area))
# quantile(unlist(area))
# #          0%          25%          50%          75%         100% 
# #     9.847142e-13 7.251430e-01 2.234341e+01 8.530417e+01 2.740146e+09 
# ### Median value is a protected area of only 22 square meters!

cull_tiny_polys <- function(prot_area, threshold = 100, single = FALSE) {
### threshold is cutoff for what counts as a tiny polygon, in m^2
### single is flag for passing a single polygon rather than a polygons data frame
  area <- lapply(prot_area@polygons, function(x) sapply(x@Polygons, function(y) y@area))
  mainPolys <- lapply(area, function(x) which(x > threshold))
  cutPolys  <- lapply(area, function(x) which(x <= threshold))
  
  cat('Original breakdown of sub-polygon areas by quantile: \n')
  print(quantile(unlist(area)))

  prot_area_new <- prot_area
  
  if(!single) cat('Working on polygon ')
  
  for(i in 1:length(mainPolys)){
    if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
      cat(sprintf('%s ', i))
      prot_area_new@polygons[[i]]@Polygons <- prot_area_new@polygons[[i]]@Polygons[mainPolys[[i]]]
      prot_area_new@polygons[[i]]@plotOrder <- 1:length(prot_area_new@polygons[[i]]@Polygons)
      
      ### fix comment (for holes) for this polygon
      comment(slot(prot_area_new, "polygons")[[i]]) <- createPolygonsComment(prot_area_new@polygons[[i]])
      
    }
  }
  prot_area_new@plotOrder <- 1:length(prot_area_new@polygons)
  ### can't have gaps in plotOrder...
  
  if(!single) {
    ### If polygons data frame, check to make sure the total number of overall polygons has not changed (by year)
    if(nrow(setdiff(as.data.frame(table(prot_area@data$STATUS_YR)), as.data.frame(table(prot_area_new@data$STATUS_YR)))) != 0)
       warning('Total number of polygons has changed! check threshold maybe?')
  }

  cut_area <- 0
  cut_num  <- 0
  for(i in 1:length(cutPolys)){
    if(length(cutPolys[[i]]) >= 1 && cutPolys[[i]][1] >= 1){
      cut_area <- cut_area + sum(area[[i]][cutPolys[[i]]])
    }
    cut_num <- cut_num + length(cutPolys[[i]])
  }
  cat(sprintf('\nAt a min area threshold of %s m^2, eliminated %s polygons for a total exclusion of %.2f km^2.\n', threshold, cut_num, cut_area/1e6))

  ### Check new breakdown of areas
  area2 <- lapply(prot_area_new@polygons, function(x) sapply(x@Polygons, function(y) y@area))
  cat('New breakdown of sub-polygon areas by quantile: \n')
  print(quantile(unlist(area2)))
  if(!gIsValid(prot_area_new)) {
    warning(sprintf('\nPolygon %s contains invalid geometry.\n', i))
    return(NULL)
  }
  return(prot_area_new)
}


prot_area_mar <- cull_tiny_polys(prot_area_mar_orig)
prot_area_ter <- cull_tiny_polys(prot_area_ter_orig)
rgn_3nm       <- cull_tiny_polys(rgn_3nm)
rgn_1km       <- cull_tiny_polys(rgn_1km)


### WTF is wrong with Haida Gwaii and other regions.
rgn_hg <- rgn_3nm
rgn_hg@polygons <- rgn_hg@polygons[rgn_hg@data$rgn_id == 2]
rgn_hg@data <- rgn_hg@data[rgn_hg@data$rgn_id == 2, ]
rgn_hg@plotOrder <- 1:length(rgn_hg@polygons)
plot(rgn_hg)

length(rgn_hg@polygons[[1]]@Polygons)
quantile(sapply(rgn_hg@polygons[[1]]@Polygons, function(x) length(x@coords)))

sapply(rgn_hg@polygons[[1]]@Polygons, function(x) return(sprintf('Area %.2f m^2, hole? %s', x@area/1e6, x@hole)))
asdf <- sapply(rgn_hg@polygons[[1]]@Polygons, function(x) x@hole)
### for Haida Gwaii, only one non-hole polygon; the rest are holes.  Not showing up! 

###########################################################=
### DO IT POLY STYLE -----
###########################################################=
### Find intersections of polygons within the protected area layer...
### For intersecting polygons, use gDifference on pair
### NOT WORKING PROPERLY

elim_overlaps <- function(prot_area, threshold = 10) {
### threshold is allowable small overlaps - ignore these
  
  cat(sprintf('Checking overlaps among %s polygons...\n', length(prot_area@polygons)))
  overlaps <- gOverlaps(prot_area, byid = TRUE) ### gOverlaps returns a matrix of row/columns that have overlap.
  
  for (i in 1:(length(prot_area@polygons) - 1)) {
    if(!any(overlaps[i, ])) {
      ### No overlaps? skip this column
      cat(sprintf('Poly %s free of overlaps!\n', i))
    } else if(is.null(SpatialPolygons(prot_area@polygons[i]))) {
      ### Did this poly get eliminated in a previous round? skip this column
      cat(sprintf('Poly %s is NULL\n', i))
    } else {
      cat(sprintf('Check poly %s: ', i))
      for (j in (i + 1):length(prot_area@polygons)) {
        if(!overlaps[i, j]) {
          cat(sprintf('.', j))
        } else {
          cat(sprintf(' %s ', j))
          intsx <- gIntersection(SpatialPolygons(prot_area@polygons[i]), SpatialPolygons(prot_area@polygons[j]))
          intsx_area <- gArea(intsx)
          if (intsx_area > .1) {
            cat(sprintf('\n  Polygon %s vs %s:  area of overlap = %.2f m^2\n', i, j, intsx_area))
            cat(sprintf('%s (%s) vs %s (%s), ', prot_area@data$ORIG_NAME[i], prot_area@data$STATUS_YR[i], prot_area@data$ORIG_NAME[j], prot_area@data$STATUS_YR[j]))
            if(intsx_area > threshold) {
              if (prot_area@data$STATUS_YR[i] <= prot_area@data$STATUS_YR[j]) {
                early <- i; later <- j
              } else {
                early <- j; later <- i
              }
              cat(sprintf('subtracting overlap of %s from %s.\n ', prot_area@data$ORIG_NAME[early], prot_area@data$ORIG_NAME[later]))
              poly_early <- SpatialPolygons(prot_area@polygons[early])
              poly_later <- SpatialPolygons(prot_area@polygons[later])
              
              temp_poly <- gDifference(poly_later, poly_early)
              if(!gIsValid(temp_poly) & !is.null(temp_poly)) {
                cat('Invalid geometry detected.... fixing it?\n')
                temp_poly_clean <- clgeo_Clean(temp_poly, print.log = TRUE)
                cat(sprintf('Polygon geometry now valid? %s\n', gIsValid(temp_poly_clean)))
                prot_area@polygons[later] <- cull_tiny_polys(temp_poly_clean, threshold = threshold, single = TRUE)@polygons
              } else cat('Difference not properly accounted for.\n')

            } else {
            cat(sprintf('smaller than min threshold %s; ignoring overlap.\n', threshold))
            }
          }
        }
      }
      cat('\n')
    }
  }
}

### AAARRRGGGHGH: gDifference creates invalid polygons.  How to fix?
### * eliminate tiny polygons using cull_tiny_polys?  might catch some.
### * checking on an invalid polygon, some sub-polys are on the order
###   of a couple hundred m^2; one is on the order of 9 km^2.  Not good.
### 
prot_area_mar1 <- elim_overlaps(prot_area_mar)
prot_area_ter1 <- elim_overlaps(prot_area_ter)

prot_area_mar1@data <- prot_area_mar1@data[prot_area_mar1@polygons]
### Determine areas of all sub-polygons and sum to find polygon areas.
get_areas <- function(prot_area) {
  area_list <- lapply(prot_area@polygons, function(x) sapply(x@Polygons, function(y) y@area))
  area_vect <- sapply(area_list, sum)
  prot_area@data$area_m2 <- area_vect
  prot_area_sum <- prot_area@data %>%
    group_by(rgn_name, rgn_id, STATUS_YR) %>%
    summarize(area_tot = sum(area_m2, na.rm = TRUE))
}

area_mar_sum <- get_areas(prot_area_mar1)
area_ter_sum <- get_areas(prot_area_ter1)

###########################################################=
### DO IT RASTER STYLE -----
###########################################################=
### Similar to global analysis - rasterize to eliminate overlapping polygons
### Use fun = 'min' on STATUS_YR to assign cell value based on earliest park.

get_base_raster <- function(rgn_list, fn = NULL) {
### create a raster grid based upon extents of a list of SpatialPolygons
  ex_list <- lapply(rgn_list, function(x) extent(x))
  crs_vec <- sapply(rgn_list, function(x) proj4string(x))
  if(length(unique(crs_vec)) > 1) {
    cat('CRS of input polygons do not match: \n')
    cat(sprintf('%s\n', crs_vec))
    return(NULL)
  }
  
  ### establish extents to cover both offshore and inland, rounded to nearest km
  ex_xmin <- round(min(sapply(ex_list, function(x) x@xmin)) - 500, -3)
  ex_xmax <- round(max(sapply(ex_list, function(x) x@xmax)) + 500, -3)
  ex_ymin <- round(min(sapply(ex_list, function(x) x@ymin)) - 500, -3)
  ex_ymax <- round(max(sapply(ex_list, function(x) x@ymax)) + 500, -3)
  ext_rgn  <- extent(ex_xmin, ex_xmax, ex_ymin, ex_ymax)
  
  reso <- 500 ### 500 m cells
  
  xcol <- (ex_xmax - ex_xmin)/reso; yrow <- (ex_ymax - ex_ymin)/reso
  
  base_raster <- raster(ext_rgn, yrow, xcol, crs = proj4string(rgn_list[[1]]))
  
  if(!is.null(dir_rast))
    writeRaster(base_raster, fn, overwrite = TRUE)
  
  return(base_raster)
}

dir_rast <- file.path(dir_bc, 'lsp', 'spatial')

base_raster <- get_base_raster(list(rgn_3nm, rgn_1km, poly_wdpa_bc), fn = file.path(dir_rast, 'rast_base.tif'))

rast_wdpa_bc <- rasterize(poly_wdpa_bc, base_raster, field = 'STATUS_YR', fun = 'min', filename = file.path(dir_rast, 'rast_wdpa_bc.tif'), background = NA, overwrite = TRUE)
### looks OK


rast_prot_mar <- mask(rast_wdpa_bc, rgn_3nm, filename = file.path(dir_rast, 'rast_prot_mar.tif'), updatevalue = NA)

### based on readOGR version:
rgn_3nmx <- readOGR(dsn = dir_rgn, layer = 'ohibc_offshore_3nm')
rast_3nmx <- rasterize(rgn_3nm, base_raster, field = 'rgn_id', fun = 'last', filename = file.path(dir_rast, 'rast_3nmx.tif'), background = NA, overwrite = TRUE)
### problem raster.

### based on readShapePoly:
rgn3nma <- readShapePoly('regions/ohibc_offshore_3nm', proj4string = CRS(proj4string(rgn_3nm)))
ptm <- proc.time()
rast_3nma <- rasterize(rgn_3nm, base_raster, field = 'rgn_id', fun = 'last', filename = file.path(dir_rast, 'rast_3nma.tif'), background = NA, overwrite = TRUE)
proc.time() - ptm
### raster still has problem

rast_1km <- rasterize(rgn_1km, base_raster, field = 'rgn_id', fun = 'last', filename = file.path(dir_rast, 'rast_1km.tif'), background = NA, overwrite = TRUE)
rast_prot_area_mar <- rasterize(prot_area_mar, base_raster, field = 'STATUS_YR', fun = 'min', filename = file.path(dir_rast, 'rast_prot_area_mar.tif'), background = NA, overwrite = TRUE)
rast_prot_area_mar1 <- rasterize(prot_area_mar1, base_raster, field = 'STATUS_YR', fun = 'min', filename = file.path(dir_rast, 'rast_prot_area_mar1.tif'), background = NA, overwrite = TRUE)
rast_prot_area_ter <- rasterize(prot_area_ter, base_raster, field = 'STATUS_YR', fun = 'min', filename = file.path(dir_rast, 'rast_prot_area_ter.tif'), background = NA, overwrite = TRUE)

library(gdalUtils)
install.packages('gdalUtils')

ptm <- proc.time()
rast_3nm_gdal <- gdal_rasterize(src_datasource = file.path(dir_rgn, 'ohibc_offshore_3nm.shp'),
                                dst_filename = file.path(dir_rast, 'rast_3nm_gdal.tif'), 
                                a = 'rgn_id',
                                a_nodata = NA,
                                te = c(ex_xmin, ex_ymin, ex_xmax, ex_ymax), # extents for output file
                                tr = c(500, 500), # resolution for x and y
                                tap = TRUE, # target aligned pixels - align coords of extent of output to values of -tr, so aligned extent includes minimum extent.  I think I already have this covered by defining the extents?
                                q = FALSE, # suppress progress monitor and other non-error output
                                output_Raster = TRUE, # return output as a RasterBrick? 
                                ignore.full_scan = TRUE,
                                verbose = TRUE)
proc.time() - ptm
plot(rast_3nm_gdal)

rast_1km_gdal <- gdal_rasterize(src_datasource = file.path(dir_rgn, 'ohibc_inland_1km.shp'),
                                dst_filename = file.path(dir_rast, 'rast_1km_gdal.tif'), 
                                a = 'rgn_id',
                                a_nodata = NA,
                                te = c(ex_xmin, ex_ymin, ex_xmax, ex_ymax), # extents for output file
                                tr = c(500, 500), # resolution for x and y
                                tap = TRUE, # target aligned pixels - align coords of extent of output to values of -tr, so aligned extent includes minimum extent.  I think I already have this covered by defining the extents?
                                q = FALSE, # suppress progress monitor and other non-error output
                                output_Raster = TRUE, # return output as a RasterBrick? 
                                ignore.full_scan = TRUE,
                                verbose = TRUE)
plot(rast_1km_gdal, add = TRUE)

