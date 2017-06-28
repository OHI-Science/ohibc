### downres_polygons.R
### 20150914 - Casey O'Hara

### Simplifies geometry of polygons to create smaller shapefiles; the main
### idea is to use the simplified shapefiles exclusively for plotting
### global maps quickly at a scale where the simplified geometry is
### acceptable.


### From: http://stackoverflow.com/questions/20976449/r-simplify-shapefile

### There is still a lot of detail in the edges of the main polygons that is unecessary
### for my requirements. This detail can be simplified using the dp() function in the
### shapefiles package. Once again, we create a loop that goes through the object
### simplifying each of the polygons within it. dp() works only on dataframes, so we
### need to break each @coords matrix into a dataframe, run dp(), then convert it back
### into a matrix.

### Note: can also cut smaller polygons if necessary...
### Note: the dp() function in the shapefiles package looks similar to the
###   gSimplify() function in the rgeos package... didn't try that one out.

source('src/R/common.R')

library(sp)
library(rgdal)
library(maptools)
library(shapefiles)

dir_spatial <- path.expand("~/github/ohibc/prep/spatial")
shp <- 'ohibc_rgn'
dst <- 'ohibc_rgn_simple'
proj_units = 'm'


rgn_poly_orig <- readOGR(dir_spatial, shp)


### examine structure of region polygons ----
rgn_poly <- rgn_poly_orig # create safety copy  # rgn_poly <- rgn_poly_orig
str(rgn_poly, max.level = 3)

sapply(rgn_poly@polygons, function(x) length(x@Polygons))
# [1]  307 1491  521  219  537 1240   94    1

### filter out tiny sub-polygons -----
### create area list for filtering.  Area will be in square degrees...
area <- lapply(rgn_poly@polygons, function(x) sapply(x@Polygons, function(y) y@area))
quantile(unlist(area))
#           0%          25%          50%          75%         100%
# 3.988795e-08 3.237168e+01 3.394341e+04 1.091734e+05 3.158881e+11


### ditch the small polys;
area_threshold <- switch(proj_units,
                       'deg' = 0.001,
                       'm' = 5e7)

mainPolys <- lapply(area, function(x) which(x > area_threshold))

for(i in 1:length(mainPolys)){ ### i <- 2
  if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
    rgn_poly@polygons[[i]]@Polygons <- rgn_poly@polygons[[i]]@Polygons[mainPolys[[i]]]
    rgn_poly@polygons[[i]]@plotOrder <- 1:length(rgn_poly@polygons[[i]]@Polygons)
  }
}

rgn_poly@plotOrder <- 1:length(rgn_poly@polygons)
# is plotOrder necessary to be continuous?  that would explain this line.

plot(rgn_poly_orig, col = 'red', border = NA)
plot(rgn_poly, col = 'blue', border = NA, add = TRUE)

### The threshold for resolution in the dp() function:
### Appears to be basically in same units of shapefile - the minimum deviation of a point between two
### other points, to be saved rather than deleted in simplifying.
reso <- switch(proj_units,
                   'deg' = .005,
                   'm'   =  500)


num_poly <- length(rgn_poly@polygons)
# reso is the resolution for the dp() call
rgn_poly_temp <- rgn_poly
# set working rgn_poly_temp to original rgn_poly
for(i in 1:num_poly) { # i <- 1
  message(sprintf('Poly %s out of %s... res = %s...\n', i, num_poly, reso))
  for(j in 1:length(rgn_poly_temp@polygons[[i]]@Polygons)) { # j <- 1
    temp <- as.data.frame(rgn_poly_temp@polygons[[i]]@Polygons[[j]]@coords)
    # data frame of coordinates for subpoly j within poly i
    names(temp) <- c("x", "y")
    temp2 <- shapefiles::dp(temp, reso)
    # this is the function that actually performs the Douglas–Peucker algorithm
    rgn_poly_temp@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
    # put the simplified coordinate set values into the place of the original coordinate sets
  }
}

area <- lapply(rgn_poly_temp@polygons, function(x) sapply(x@Polygons, function(y) y@area))
quantile(unlist(area))
#       0%          25%          50%          75%         100%
# 50307407     78831709    152108457    340554288 315888121302

rgn_poly_temp@plotOrder <- 1:length(rgn_poly@polygons)

# writeOGR(rgn_poly_temp, dsn = dir_spatial, layer = dst, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
### writeOGR was crashing when saving... not sure why - no error message.
maptools::writePolyShape(rgn_poly_temp, file.path(dir_spatial, dst))
# Warning message:
#   use rgdal::readOGR or sf::st_read
