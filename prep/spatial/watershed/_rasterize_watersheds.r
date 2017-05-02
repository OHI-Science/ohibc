
library(sp)        # the classes and methods that make up spatial ops in R
# library(gdalUtils) # for gdal_rasterize() function
library(maptools)  # tools for reading and manipulating spatial objects
library(rgeos)
library(rgdal)
library(raster)


dir_git  <- '~/github/ohibc'         ### set wd to work in Github OHIBC location
source(file.path(dir_git, 'src/R/common.R'))  ### an OHIBC specific version of common.R


dir_spatial <- file.path(dir_git, 'prep/spatial') ### github: general buffer region shapefiles

dir_anx <- file.path(dir_M, 'git-annex/bcprep')
dir_spatial_anx <- file.path(dir_anx, 'spatial')
### provenance tracking
library(provRmd); prov_setup()

source(file.path(dir_git, 'src/R/rast_tools.R'))

### set up the default BC projection to be BC Albers
p4s_bcalb <- c('bcalb' = '+init=epsg:3005')


# Methods

## Set up coastal buffer rasters

# Buffer shapefiles are located in `github/ohibc/prep/spatial`.  Global LSP uses 1 km inland and 3nm offshore buffers, while resilience requires analysis over the entire EEZ.  For OHI Howe Sound, we used the inland extent of the entire Squamish watershed.  For OHIBC, we will compromise: include coastal sub-watersheds that intersect with the 1 km buffer.
#
# * Rasterize the watersheds using the value of watershed ID to 500 m
# * Crosstabulate with the 500 m 1 km inland buffer to identify watersheds that intersect with the coastline
# * create a raster of these features, with a rgn_id value to reflect the region ID with the greatest amount of overlap.


all_ws_gdb_file   <- file.path(dir_anx, '_raw_data/databc/FWA_BC.gdb')

### Set up all watersheds polygon from geodatabase
if(!file.exists(all_ws_shp)) {
  all_ws_poly <- readOGR(dsn = all_ws_gdb_file,
                         layer = 'FWA_ASSESSMENT_WATERSHEDS_POLY',
                         stringsAsFactors = FALSE)
  names(all_ws_poly@data) <- tolower(names(all_ws_poly@data))
  all_ws_poly@data <- all_ws_poly@data %>%
    select(ws_ftid = watershed_feature_id,
           ws_gpid = watershed_group_id,
           ws_ha   = area_ha)
  writeOGR(all_ws_poly, dsn = dirname(all_ws_shp),
           layer = basename(all_ws_shp %>% str_replace('.shp', '')),
           driver = 'ESRI Shapefile',
           overwrite_layer = TRUE)
  ### ws_ftid (WATERSHED_FEATURE_ID) is unique; rasterize to this.  Visual inspection
  ### looks like no overlapping polygons to worry about.
} else git_prov(all_ws_gdb_file, filetype = 'input')


### Set up 500-m raster of watersheds, to crosstab against 500-m inland shapefile
### This will be used to identify coastal watershed IDs

all_ws_shp <- file.path(dir_anx, '_raw_data/databc/FWA_BC_assessment_watersheds.shp')
all_ws_500m_tif <- file.path(dir_anx, '_raw_data/databc', 'FWA_BC_assessment_watersheds_500m.tif')
rast_base_500 <- raster(file.path(dir_spatial, 'raster/ohibc_rgn_raster_500m.tif'))

all_ws_rast_500 <- gdal_rast2(src = all_ws_shp,
                          rast_base = rast_base_500,
                          dst = all_ws_500m_tif,
                          value = 'ws_ftid',
                          override_p4s = TRUE)

coastal_rast_500 <- raster(file.path(dir_spatial, 'raster', 'ohibc_inland_1km_raster_500m.tif'))
coastal_ws_ids <- crosstab(coastal_rast_500, all_ws_rast_500, progress = 'text')

coastal_ws_df <- coastal_ws_ids %>%
  as.data.frame() %>%
  setNames(c('rgn_id', 'ws_id', 'freq')) %>%
  mutate(rgn_id = as.integer(as.character(rgn_id)),
         ws_id  = as.integer(as.character(ws_id)),
         freq   = as.integer(freq)) %>%
  filter(!is.na(rgn_id) & freq != 0 & !is.na(ws_id)) %>%
  group_by(ws_id) %>%
  arrange(desc(freq)) %>% ### the instance with highest frequency is on top
  summarize(rgn_id = first(rgn_id)) ### select the rgn_id with highest frequency for that watershed
write_csv(coastal_ws_df, file.path(dir_spatial, 'watershed/coastal_ws_ids.csv'))


### Save identified watersheds as a polygons shapefile (for reference)
all_ws_poly <- readOGR(dsn = path.expand(file.path(dir_anx, '_raw_data/databc')),
                       layer = 'FWA_BC_assessment_watersheds',
                       stringsAsFactors = FALSE)

coastal_ws_polys <- all_ws_poly[x <- all_ws_poly@data$ws_ftid %in% coastal_ws_df$ws_id, ]
writeOGR(coastal_ws_polys, dsn = path.expand(file.path(dir_spatial_anx, 'coastal_watershed')),
         layer = 'ohibc_coastal_watersheds',
         driver = 'ESRI Shapefile')


### Using the coastal watershed IDs, create a coastal watershed raster of OHIBC IDs.
rast_base_30 <- raster(file.path(dir_spatial_anx, 'raster/ohibc_offshore_3nm_30m.tif'))
coast_ws_30m_tif_file <- file.path(dir_spatial_anx, 'coastal_watershed/ohibc_coastal_watersheds_30m.tif')

if(!file.exists(coast_ws_30m_tif_file)) {
  ### is there a full watershed raster?
  all_ws_tif <- file.path(dir_anx, '_raw_data/databc', 'FWA_BC_assessment_watersheds_30m.tif')
  if(!file.exists(all_ws_tif)) {
    all_ws_rast <- gdal_rast2(src = all_ws_shp,
                              rast_base = rast_base,
                              dst = all_ws_tif,
                              value = 'ws_ftid',
                              override_p4s = TRUE)
  }

  ### from the all watershed raster, select only those whose ids match the coastal IDs
  coastal_ws_rast <- subs(all_ws_rast, coastal_ws_df, by = 'ws_id', which = 'rgn_id')
  writeRaster(coastal_ws_rast, coast_ws_tif_file, overwrite = TRUE)
}


