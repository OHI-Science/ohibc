### buffer_polygons.R
### 20170614 - Casey O'Hara

source('src/R/common.R')

library(sf)

dir_spatial <- path.expand("~/github/ohibc/prep/spatial")
buffer_seed_layer <- 'ohibc_eez'
### expand this basic EEZ file (1 region covering entire EEZ) outward
buffer_slice_layer <- 'ohibc_rgns_unclipped'
### intersect resulting buffer with this shapefile to slice it into regions
dst <- 'ohibc_inland_15km'
### save the result here...
proj_units  <- 'm'
buffer_dist <- 15000

buffer_seed <- read_sf(dir_spatial, layer = buffer_seed_layer) %>%
  select(geometry)

buffer_expanded <- st_buffer(buffer_seed, buffer_dist)

### Intersect the trimmed buffer with the 'slice' layer to divide it into
### OHIBC regions.
buffer_slice <- read_sf(dir_spatial, layer = buffer_slice_layer)
ptm <- proc.time()
buffer_intersected <- st_intersection(buffer_expanded, buffer_slice)
(proc.time() - ptm)[3]

### Subtract the original shape from the expanded shape to get just the
### buffer region.
ptm <- proc.time()
buffer_trimmed  <- st_difference(buffer_intersected, buffer_seed)
(proc.time() - ptm)[3]

### NOTE: Regardless of order (difference, then intersection, or intersection
### then difference), the first step seems to go very quickly then the next
### takes forever.

write_sf(buffer_trimmed, dsn = file.path(dir_spatial, 'tmp'), layer = dst, driver = 'ESRI Shapefile')

### Note: weird little remnants to deal with - do it in QGIS?
