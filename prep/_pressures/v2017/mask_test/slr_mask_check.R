### checking SLR masks

library(sf)
library(rgdal)
library(raster)

source('~/github/ohibc/src/R/common.R')

dir_git <- path.expand('~/github/ohibc')

### The raster used as a mask for 2016 and 2017 SLR calcs:
s <- raster(file.path(dir_git,'../ohiprep/globalprep/prs_slr/v2016/int/rast_3nm_mask.tif'))

### Extents (in lat-long) to clip to BC for easier inspection
bc_ext <- extent(c('xmin' = -135, 'xmax' = -115, 'ymin' = 48, 'ymax' = 55))

s_bc <- s %>% crop(bc_ext)


### The global 3-nm poly used to create the mask:
# poly_3nm <- readOGR(dsn = file.path(dir_M, 'git-annex/globalprep/spatial/d2014/data'),
#                     layer = 'regions_offshore3nm_gcs')

# poly_3nm_df <- poly_3nm@data
### Canada is rgn_id 218
# poly_3nm_bc <- poly_3nm[poly_3nm@data$rgn_id == 218, ]
# writeOGR(poly_3nm_bc, file.path(dir_git, 'prep/_pressures/v2017/mask_test'), 'poly3nm_canada', driver = 'ESRI Shapefile')

poly_3nm_can <- readOGR(file.path(dir_git, 'prep/_pressures/v2017/mask_test'), 'poly3nm_canada')


### generate a raster of random values within BC extents, to test mask function
test_rast <- s_bc
values(test_rast) <- runif(length(values(test_rast)))

plot(test_rast)

### try mask

r_3nm_mask <- mask(test_rast, poly_3nm_can, progress = 'text')

plot(s_bc, col = 'red')
plot(r_3nm_mask, col = '#0000ff88', add = TRUE)

writeRaster(s_bc, file.path(dir_git, 'prep/_pressures/v2017/mask_test/slr_mask_bc.tif'))
writeRaster(r_3nm_mask, file.path(dir_git, 'prep/_pressures/v2017/mask_test/slr_mask_bc_new.tif'))

### Try creating a new global mask raster from scratch:
### * create global raster of values = 1
test_rast_global <- s
values(test_rast_global) <- rep(1, times = length(values(test_rast_global)))
### * mask that fucker:
r_3nm_mask_gl <- mask(test_rast_global, poly_3nm, progress = 'text')
writeRaster(r_3nm_mask_gl, file.path(dir_git, 'prep/_pressures/v2017/mask_test/slr_mask_gl_new.tif'))

### global still has issues filling in islands.

### Try fasterize!

library(sf)
library(fasterize)

sf_3nm <- sf::read_sf(dsn = file.path(dir_M, 'git-annex/globalprep/spatial/d2014/data'),
                      layer = 'regions_offshore3nm_gcs')
r_3nm_mask_gl_fasterize <- fasterize::fasterize(sf_3nm, s, field = 'rgn_id')

writeRaster(r_3nm_mask_gl_fasterize,
            file.path(dir_git, 'prep/_pressures/v2017/mask_test/slr_mask_gl_fasterize.tif'))
