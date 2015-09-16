### data_prep_lsp.R
### 

library(sp)        # the classes and methods that make up spatial ops in R
library(gdalUtils) # for gdal_rasterize() function
library(maptools)  # tools for reading and manipulating spatial objects
library(rgeos)
library(rgdal)
library(raster)


dir_bc  <- setwd('~/github/ohibc')           ### set wd to work in Github OHIBC location
source('src/R/common.R')  ### an OHIBC specific version of common.R
source('lsp/R/lsp_fxns.R')

dir_anx <- file.path(dir_neptune_data, 'git-annex/ohibc/lsp') ### git-annex: goal-specific large files
dir_rgn <- file.path(dir_bc, 'regions')      ### github: general buffer region shapefiles
dir_rst <- file.path(dir_bc, 'lsp/spatial')  ### github: goal-specific spatial outputs

p4s_bcalb <- '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'

#####################################################################=
### Read in BC WDPA shapefile for rasterization -----
#####################################################################=
### NOTE: If BC WDPA file does not yet exist, get_wdpa_poly() creates 
###   it from the original WDPA-MPA file.  Takes a long time, due to
###   reading in the full WDPA-MPA shapefile.
### Need to load the WDPA poly into memory for two reasons:
### * we need the polygon to establish the extents for all the rasters
###   to follow.
### * Since we need to assign the oldest STATUS_YR value to the cell
###   value in the WDPA-MPA raster, we can't use gdal_rasterize();
###   instead we need raster::rasterize() which works from memory.

poly_wdpa_bc <- get_wdpa_poly()  ### defaults to BC Albers

x <- gIsValid(poly_wdpa_bc, byid = TRUE)
poly_wdpa_bc@data$NAME[x == FALSE]
### invalid polygons - fix with cleangeo?
# [1] Mud Lake Delta Park         Valhalla Park               Brackendale Eagles Park     Lazo Marsh-North East Comox
# [5] Cape Scott Park             Naikoon Park                Hesquiat Peninsula Park     Pacific Rim                
# [9] NEAH CONSERVANCY           


#####################################################################=
### Set up inland 1 km and offshore 3nm buffers -----
#####################################################################=
### * Read in buffer shapefiles to SpatialPolygons data frames; add
###   the WDPA-MPA Spatial Polygons data frame as well.
### * From these objects, get the extents, CRS (proj4string)
### * Create a base raster from extents, CRS, and resolution.

poly_3nm <- readOGR(dsn = dir_rgn, layer = 'ohibc_offshore_3nm')
poly_1km <- readOGR(dsn = dir_rgn, layer = 'ohibc_inland_1km')
rgn_list<- list(poly_3nm, poly_1km, poly_wdpa_bc)

ext_rgn <- get_extents(rgn_list)
p4s_rgn <- get_p4s(rgn_list)

reso <- 500 ### resolution for all rasters in this process

base_raster <- get_base_raster(ext = ext_rgn, reso = reso, p4s_base = p4s_rgn, fn = file.path(dir_rst, 'rast_base.tif'))


#####################################################################=
### Convert inland 1 km and offshore 3nm polygons to rasters -----
#####################################################################=
### Note: these rasters are currently not used in final analysis...

# Create rasters for regions in BC Albers projection (no reprojection):
rast_3nm <- gdal_rasterize(src_datasource = file.path(dir_rgn, 'ohibc_offshore_3nm.shp'), dst_filename = file.path(dir_rst, 'rast_offshore_3nm.tif'), 
                                a = 'rgn_id', a_nodata = NA,
                                te = c(ext_rgn@xmin, ext_rgn@ymin, ext_rgn@xmax, ext_rgn@ymax), tr = c(reso, reso), # extents and resolution for x and y
                                output_Raster = TRUE, # return output as a RasterBrick? 
                                ignore.full_scan = TRUE,
                                verbose = TRUE)
rast_1km <- gdal_rasterize(src_datasource = file.path(dir_rgn, 'ohibc_inland_1km.shp'), dst_filename = file.path(dir_rst, 'rast_inland_1km.tif'), 
                                a = 'rgn_id', a_nodata = NA,
                                te = c(ext_rgn@xmin, ext_rgn@ymin, ext_rgn@xmax, ext_rgn@ymax), tr = c(reso, reso), # extents and resolution for x and y
                                output_Raster = TRUE, # return output as a RasterBrick? 
                                ignore.full_scan = TRUE,
                                verbose = TRUE)
### Raster::extract requires raster layers rather than raster bricks.  Unstack
### the raster bricks and select the first element in each resulting list.
rast_3nm <- unstack(rast_3nm)[[1]]
rast_1km <- unstack(rast_1km)[[1]]

plot(rast_3nm)
plot(rast_1km, add = TRUE)

#####################################################################=
### Rasterize the BC WDPA-MPA shapefile -----
#####################################################################=

rast_wdpa_bc <- rasterize(poly_wdpa_bc, base_raster, field = 'STATUS_YR', fun = 'min', background = NA, 
                          filename = file.path(dir_rst, 'rast_wdpa_bc.tif'), overwrite = TRUE)

plot(rast_wdpa_bc, alpha = 0.5, add = TRUE)
### looks OK

#####################################################################=
### Extract WDPA values against buffer regions -----
#####################################################################=
### Extract the rasterized WDPA against the spatial polygons of
### the buffer regions.  Result is a list of raster cells within each
### region, and the associated WDPA value (earliest STATUS_YR) for
### that cell.

prot_mar_list <- raster::extract(rast_wdpa_bc, poly_3nm)
names(prot_mar_list) <- poly_3nm@data$rgn_id
prot_mar_df <- sum_prot_areas(prot_mar_list, reso = res(rast_wdpa_bc)[1])
### note: pull the resolution from the rast_wdpa_bc raster; 
### assume x and y resolution are identical

prot_ter_list <- raster::extract(rast_wdpa_bc, poly_1km)
names(prot_ter_list) <- poly_1km@data$rgn_id
prot_ter_df <- sum_prot_areas(prot_ter_list, reso = res(rast_wdpa_bc)[1])


#####################################################################=
### Calc status for each region for each year -----
#####################################################################=
ref_pt = .3

prot_mar_df <- prot_mar_df %>%
  mutate(status = (cum_a_km2/tot_a_km2)/ref_pt*100,
         status = ifelse(status > 100, 100, status))

prot_ter_df <- prot_ter_df %>%
  mutate(status = (cum_a_km2/tot_a_km2)/ref_pt*100,
         status = ifelse(status > 100, 100, status))

lsp_status <- prot_mar_df %>% 
  dplyr::select(rgn_id, year, mar_status = status) %>%
  left_join(prot_ter_df %>% 
              dplyr::select(rgn_id, year, ter_status = status),
            by = c('rgn_id', 'year')) %>%
  mutate(status = (mar_status + ter_status) / 2)
  

View(lsp_status %>% filter(year > 2010))

#####################################################################=
### TO DO:  -----
#####################################################################=

### sanity check by comparing vs polygon areas
### have rasters saved on Neptune instead of Github
