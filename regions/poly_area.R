### poly_area.R

setwd('~/github/ohibc')
source('src/R/common.R')
library(rgeos)
library(rgdal)
library(maptools)

dir_spatial <- file.path(getwd(), 'regions')

### identify all shapefile sets, eliminate extension
shp_list <- list.files(dir_spatial)
shp_list <- shp_list[str_detect(shp_list, '.shp')] %>%
  str_replace('.shp', '')

for (shp in shp_list) {   # shp <- shp_list[1]
  rgn_poly <- readOGR(dsn = dir_spatial, layer = shp)
  print(head(rgn_poly@data))
  
  areas <- gArea(rgn_poly, byid = TRUE)
  areas_km2 <- areas/1e6
  areas_hec <- areas/1e4
  
  for (i in 1:nrow(rgn_poly@data)) {
    cat(sprintf('%s:   area from slot: %.1f km^2; area from gArea: %.1f km^2\n', 
                rgn_poly@data$rgn_code[i], rgn_poly@polygons[[i]]@area/1e6, areas_km2[i]))
  }
  
  rgn_poly@data$area_km2 <- areas_km2
  rgn_poly@data$area_hec <- areas_hec
  
  print(head(rgn_poly@data))
  
  writeOGR(rgn_poly, dsn = dir_spatial, layer = paste(shp, '_area', sep = ''), driver = 'ESRI Shapefile')
}
