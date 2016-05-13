### rasterize ohibc regions
library(dplyr)
library(tidyr)
library(gdalUtils)
library(raster)

### file path for ohibc_rgn and ohibc_land
rgn_shp_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn.shp'

### output for rasters
rgn_rast_tmp_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn_rast_tmp_1000m.tif'
rgn_raster_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn_raster_1000m.tif'

base_te <- c(154000,168000,1876000,1741000)
res_1km <- c(1000, 1000)

rast_rgn <- gdalUtils::gdal_rasterize(
  src_datasource = path.expand(rgn_shp_file),
  dst_filename   = path.expand(rgn_rast_tmp_file),
  a = 'rgn_id', # attribute to burn
  a_nodata = NA,
  # at = TRUE,
  te = base_te,
  tr = res_1km,
  output_Raster = TRUE)
### this saves an 20+ MB file; But raster::rasterize is really slow.

### writeRaster to write a compressed version in final location?

writeRaster(rast_rgn, rgn_raster_file, overwrite = TRUE)
unlink(rgn_rast_tmp_file) ### delete temp

rast_rgn <- raster(rgn_raster_file)
plot(rast_rgn)


### repeat for land raster
land_shp_file <- '~/github/ohibc/prep/spatial/ohibc_land.shp'
land_rast_tmp_file <- '~/github/ohibc/prep/spatial/ohibc_land_rast_tmp_1000m.tif'
land_raster_file <- '~/github/ohibc/prep/spatial/ohibc_land_raster_1000m.tif'

rast_land <- gdalUtils::gdal_rasterize(
  src_datasource = path.expand(land_shp_file),
  dst_filename   = path.expand(land_rast_tmp_file),
  a = 'rgn_id', # attribute to burn
  a_nodata = NA,
  # at = TRUE,
  te = base_te,
  tr = res_1km,
  output_Raster = TRUE)
### this saves an 20+ MB file; But raster::rasterize is really slow.

### writeRaster to write a compressed version in final location?

writeRaster(rast_land, land_raster_file, overwrite = TRUE)
unlink(land_rast_tmp_file) ### delete temp

rast_land <- raster(land_raster_file)
plot(rast_land)
