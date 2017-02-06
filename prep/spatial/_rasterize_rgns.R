### rasterize ohibc regions

library(rgdal)
library(raster)

source('~/github/ohibc/src/R/common.R')  ### an OHIBC specific version of common.R
source('src/R/rast_tools.R')
### to use the gdal_rast2 function

dir_spatial <- '~/github/ohibc/prep/spatial'
### file path for ohibc_rgn and ohibc_land

### set up region to be rasterized
# rgn_shp_file  <- '~/github/ohibc/prep/spatial/ohibc_rgn.shp'
rgn_shp_file  <- '~/github/ohibc/prep/spatial/ohibc_inland_1km.shp'
# rgn_shp_file  <- '~/github/ohibc/prep/spatial/ohibc_offshore_3nm.shp'

### set up base raster
rast_base_file  <- file.path(dir_spatial, 'raster/ohibc_inland_1km_raster_500m.tif')
# rast_base_file  <- file.path(dir_goal_anx, 'raster/land_cover_1990_30m_crop.tif')
rast_base <- raster(rast_base_file)

### set destination file name
# rgn_rast_file <- file.path(dir_spatial, 'raster/ohibc_rgn_raster_500m.tif')
rgn_rast_file  <- file.path(dir_spatial, 'raster/ohibc_inland_2km_raster_500m.tif')
# rgn_rast_file <- file.path(dir_goal_anx, 'raster/ohibc_inland_1km_30m.tif')

### output for rasters
gdal_rast2(rgn_shp_file,
           rast_base,
           dst = rgn_rast_file,
           override_p4s = TRUE)

rast_rgn <- raster(rgn_rast_file)
plot(rast_rgn)

#############################################=
### Create Mollweide versions of regions -----
###   get Cumulative Impacts layer as base raster to reproject
###   reproject BC region polygons and visually inspect (?is this necessary?)
###   rasterize BC regions

rast_base_file <- file.path(dir_M, 'model/GL-NCEAS-Halpern2008/tmp/ocean.tif')
rast_base <- raster::raster(rast_base_file)

# rgn_shp_file     <- '~/github/ohibc/prep/spatial/ohibc_rgn.shp'
# rgn_shp_mol_file <- '~/github/ohibc/prep/spatial/ohibc_rgn_mol.shp'
# rgn_rast_file    <- '~/github/ohibc/prep/spatial/raster/ohibc_rgn_mol_934m.tif'

rgn_shp_file     <- '~/github/ohibc/prep/spatial/ohibc_offshore_3nm.shp'
rgn_shp_mol_file <- '~/github/ohibc/prep/spatial/ohibc_offshore_3nm_mol.shp'
rgn_rast_file    <- '~/github/ohibc/prep/spatial/raster/ohibc_offshore_3nm_934m.tif'

if(!file.exists(rgn_shp_mol_file)) {
  rgn_shp_bcalb <- readOGR(path.expand(dirname(rgn_shp_file)),
                           str_replace(basename(rgn_shp_file), '.shp', ''))
  rgn_shp_mol <- spTransform(rgn_shp_bcalb, crs(rast_base))

  writeOGR(rgn_shp_mol,
           dsn = path.expand(dirname(rgn_shp_mol_file)),
           layer = str_replace(basename(rgn_shp_mol_file), '.shp', ''),
           driver = 'ESRI Shapefile')
} else {
  message('Mollweide shapefile already exists: ', rgn_shp_mol_file)
  rgn_shp_mol <- readOGR(path.expand(dirname(rgn_shp_mol_file)),
                         str_replace(basename(rgn_shp_mol_file), '.shp', ''))
}

rast_base_crop <- crop(rast_base, rgn_shp_mol)

new_rast <- gdal_rast2(rgn_shp_mol_file, rast_base_crop, dst = rgn_rast_file, value = 'rgn_id')

#############################################=
### Create WGS84 versions of regions -----
###   manually create base raster

rast_base <- raster(x = extent(c(-180, 180, -90, 90)), resolution = 0.0333333,
                    crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

rgn_shp_file     <- '~/github/ohibc/prep/spatial/ohibc_rgn.shp'
rgn_shp_wgs_file <- '~/github/ohibc/prep/spatial/ohibc_rgn_wgs84.shp'
rgn_rast_file    <- '~/github/ohibc/prep/spatial/raster/ohibc_rgn_wgs84_02min.tif'

if(!file.exists(rgn_shp_wgs_file)) {
  rgn_shp_bcalb <- readOGR(path.expand(dirname(rgn_shp_file)),
                           str_replace(basename(rgn_shp_file), '.shp', ''))
  rgn_shp_wgs84 <- spTransform(rgn_shp_bcalb, crs(rast_base))

  writeOGR(rgn_shp_wgs84,
           dsn = path.expand(dirname(rgn_shp_wgs_file)),
           layer = str_replace(basename(rgn_shp_wgs_file), '.shp', ''),
           driver = 'ESRI Shapefile')
} else {
  message('WGS84 shapefile already exists: ', rgn_shp_wgs_file)
  rgn_shp_wgs84 <- readOGR(path.expand(dirname(rgn_shp_wgs_file)),
                         str_replace(basename(rgn_shp_wgs_file), '.shp', ''))
}

rast_base_crop <- crop(rast_base, rgn_shp_wgs84)

new_rast <- gdal_rast2(rgn_shp_wgs_file,
                       rast_base_crop,
                       dst = rgn_rast_file,
                       value = 'rgn_id', override_p4s = TRUE)
plot(new_rast)
