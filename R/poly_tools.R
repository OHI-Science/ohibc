### poly_tools.R

### Utilities for working with polygons and shapefiles in OHI.  
### * The dir_spatial argument is a directory containing spatial files
###   (uses .shp basename to identify spatial files).
### * Spatial files should have rgn_name and rgn_id in attributes table -
###   not for important functionality, just to facilitate more helpful 
###   reporting.

require(dplyr)
require(tidyr)
require(rgeos)
require(rgdal)
require(maptools)
require(cleangeo) ### devtools::install_github('eblondel/cleangeo') 

# dir_spatial <- file.path(getwd(), 'regions')

calc_poly_area <- function(dir_spatial) {
### Given a directory that contains region shapefiles, opens each shapefile,
### calcs area and adds (or updates) attributes of area_km2 and area_hectare.
### Saves updated poly in same directory, with '_area' tag on filename.
  
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
    
    ### sanity check - the area from 'area' slot is area of the main poly;
    ### in most cases, the gArea will report smaller area b/c of holes.
    for (i in 1:nrow(rgn_poly@data)) {
      cat(sprintf('%s:   area from slot: %.1f km^2; area from gArea: %.1f km^2\n', 
                  rgn_poly@data$rgn_code[i], 
                  rgn_poly@polygons[[i]]@area/1e6, 
                  areas_km2[i]))
    }
    
    rgn_poly@data$area_km2 <- areas_km2
    rgn_poly@data$area_hectare <- areas_hec
    
    print(head(rgn_poly@data))
    
    writeOGR(rgn_poly, dsn = dir_spatial, 
             layer = paste(shp, '_area', sep = ''), 
             driver = 'ESRI Shapefile')
  }
}

fix_poly_geom <- function(dir_spatial) {
### Given a directory that contains shapefiles, opens each shapefile,
### checks for valid geometries, and uses tools from cleangeo to 
### attempt to repair invalid geometries
  
  ### identify all shapefile sets, eliminate extension
  shp_list <- list.files(dir_spatial)
  shp_list <- shp_list[str_detect(shp_list, '.shp')] %>%
    str_replace('.shp', '')
  
  for (shp in shp_list) {   # shp <- shp_list[1]
    
    cat(sprintf('\n\nReading shapefile at %s/%s.shp...\n', dir_spatial, shp))
    rgn_poly <- readOGR(dsn = dir_spatial, layer = shp)
    print(head(rgn_poly@data))
    
    cat(sprintf('Generating geometry report for %s.shp\n', shp))
    report  <- clgeo_CollectionReport(rgn_poly)
    if(any(!report$valid)) {
      report$rgn_name <- rgn_poly@data$rgn_name
      report$rgn_id <- rgn_poly@data$rgn_id
      issues  <- report %>% filter(valid == FALSE)
      cat(sprintf('Oh no! Found %s region(s) with invalid geometries.\n', nrow(issues)))
      print(issues)
      
      cat('Attempting to fix invalid geometries...\n')
      rgn_poly_clean <- clgeo_Clean(rgn_poly, print.log = TRUE)
      report_clean <- clgeo_CollectionReport(rgn_poly_clean)
      
      ### Check for success; if successful, save new shapefile.
      if(any(!report_clean$valid)) {
        report_clean$rgn_name <- rgn_poly@data$rgn_name
        report_clean$rgn_id <- rgn_poly@data$rgn_id
        issues_clean  <- report_clean %>% filter(valid == FALSE)
        cat(sprintf('Unable to repair all geometries!  Still %s instance(s) of invalid geometry.\n',
                    nrow(issues_clean)))
        print(issues_clean)
        cat(sprintf('No new file saved.  Please examine %s/%s.shp manually.\n', dir_spatial, shp))
      } else {
        cat('Cleaning successful!  All geometries valid.\n')
        writeOGR(rgn_poly, dsn = dir_spatial, 
                 layer = paste(shp, '_clean', sep = ''), 
                 driver = 'ESRI Shapefile',
                 )
      }
    } else {
      cat(sprintf('Hooray!  no invalid geometries identified in %s/%s.shp.\n', dir_spatial, shp))
    }  
  }
}
