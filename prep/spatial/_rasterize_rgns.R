### rasterize ohibc regions
library(dplyr)
library(tidyr)
library(gdalUtils)
library(raster)

source('src/R/rast_tools.R')
### to use the gdal_rast2 function

### file path for ohibc_rgn and ohibc_land
# rgn_shp_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn.shp'
rgn_shp_file  <- '~/github/ohibc/prep/spatial/ohibc_inland_2km.shp'

x <- readOGR(dirname(rgn_shp_file) %>% path.expand(), basename(rgn_shp_file) %>% str_replace('.shp', ''))
x@data
x@data <- x@data %>% select(rgn_id = rgn_id_2, rgn_name = rgn_name_2, rgn_code = rgn_code_2)
writeOGR(x,
         dirname(rgn_shp_file) %>% path.expand(), basename(rgn_shp_file) %>% str_replace('.shp', ''),
         overwrite_layer = TRUE,
         driver = 'ESRI Shapefile')

rgn_base_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn_raster_500m.tif'
rgn_base <- raster(rgn_base_file)

# gdal_rast2(rgn_shp_file, rast_base, dst = rgn_rast_file)

### output for rasters
# rgn_rast_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn_raster_500m_1.tif'
# rgn_rast_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn_raster_250m.tif'
# rgn_rast_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn_raster_100m.tif'
# rgn_rast_file  <- '~/github/ohibc/prep/spatial/ohibc_inland_1km_raster_100m.tif'
# rgn_rast_file  <- '~/github/ohibc/prep/spatial/ohibc_inland_2km_raster_100m.tif'

base_te <- extent(rast_base)
base_tr <- res(rast_base)

rast_rgn <- gdalUtils::gdal_rasterize(
  src_datasource = path.expand(rgn_shp_file),
  dst_filename   = path.expand(rgn_rast_tmp_file),
  a = 'rgn_id', # attribute to burn
  a_nodata = NA,
  # at = TRUE,
  te = base_te,
  tr = res,
  output_Raster = TRUE)
### this saves an 20+ MB file; But raster::rasterize is really slow.

### writeRaster to write a compressed version in final location?

writeRaster(rast_rgn, rgn_rast_file, overwrite = TRUE)
unlink(rgn_rast_tmp_file) ### delete temp

rast_rgn <- raster(rgn_rast_file)
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
