### poly_tools.R

### Utilities for working with polygons and shapefiles in OHI.  
### * The dir_spatial argument is a directory containing spatial files
###   (uses .shp basename to identify spatial files).
### * Spatial files should have rgn_name and rgn_id in attributes table -
###   not for important functionality, just to facilitate more helpful 
###   reporting.

require(dplyr)
require(tidyr)
require(stringr)
require(rgeos)
require(rgdal)
require(maptools)
require(cleangeo)

# dir_spatial <- file.path(getwd(), 'regions')

calc_poly_area_dir <- function(dir_spatial) {
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

clean_poly_dir <- function(dir_spatial, clean_tag = '') {
### Given a directory that contains shapefiles, opens each shapefile,
### checks for valid geometries, and uses tools from cleangeo to 
### attempt to repair invalid geometries.
### * 'clean_tag' allows user to add a tag to the end of the filename of
###   the cleaned shapefile.
  
  ### identify all shapefile sets, eliminate extension
  shp_list <- list.files(dir_spatial)
  shp_list <- shp_list[str_detect(shp_list, '.shp')] %>%
    str_replace('.shp', '')
  
  for (shp in shp_list) {   # shp <- shp_list[4]
    
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
        cat(sprintf('Adding "_invalid" tag to filename. Please examine %s/%s_invalid.shp manually.\n', dir_spatial, shp))
        inv_flag <- '_invalid'
      } else {
        cat('Cleaning successful!  All geometries valid.\n')
        inv_flag <- ''
      }
      writeOGR(rgn_poly_clean, dsn = dir_spatial, 
               layer = paste(shp, clean_tag, inv_flag, sep = ''), 
               driver = 'ESRI Shapefile',
               overwrite_layer = TRUE
               )
    } else {
      cat(sprintf('Hooray!  no invalid geometries identified in %s/%s.shp.\n', dir_spatial, shp))
    }  
  }
}

reproj_poly_dir <- function(dir_spatial, p4s_new, crs_flag, crs_dir = 'crs_new') {
  ### Given a directory that contains shapefiles, opens each shapefile,
  ### reprojects into new defined projection, checks for geometry errors
  
  if(!substr(path.expand(crs_dir), 1, 1) == '\\' & !substr(path.expand(crs_dir), 1, 1) == '/')
    ### simple check for whether crs_dir is absolute (starts with / or \) or relative
    crs_dir <- file.path(dir_spatial, crs_dir)
  
  ### identify all shapefile sets, eliminate extension
  shp_list <- list.files(dir_spatial)
  shp_list <- shp_list[str_detect(shp_list, '.shp')] %>%
    str_replace('.shp', '')
  
  for (shp in shp_list) {   # shp <- shp_list[4]
    
    cat(sprintf('\n\nReading shapefile at %s/%s.shp...\n', dir_spatial, shp))
    rgn_poly <- readOGR(dsn = dir_spatial, layer = shp)
    print(head(rgn_poly@data))
    
    if(any(!gIsValid(rgn_poly, byid = TRUE))) {
      ### original shapefile has problems; skip to next
      cat('Invalid geometries detected... not reprojecting this shapefile.\n')
    } else {
      ### geometries OK - continue
      p4s_orig <- proj4string(rgn_poly)
      cat(sprintf('  proj4string orig: %s\n  proj4string new:  %s\n', p4s_orig, p4s_new))
      
      rgn_poly_crs <- spTransform(rgn_poly, CRS(p4s_new))
      
      if(any(!gIsValid(rgn_poly_crs, byid = TRUE))) {
        ### projected shapefile has problems; proceed with caution
        invalid_flag <- '_invalid'
        cat(sprintf('***Projected polygons show invalid geometry***\n'))
      } else 
        invalid_flag <- ''
        
      if(!dir.exists(crs_dir)) dir.create(crs_dir)
      
      lyr_new <- sprintf('%s%s%s', shp, crs_flag, invalid_flag)
      
      cat(sprintf('Writing projected shapefile to: \n  %s/%s.shp\n', crs_dir, lyr_new))
      writeOGR(rgn_poly_crs, 
               dsn = crs_dir, layer = lyr_new, 
               driver = 'ESRI Shapefile', 
               overwrite_layer = TRUE)
    }
  }
}
