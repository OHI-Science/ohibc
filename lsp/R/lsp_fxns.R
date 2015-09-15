### lsp_fxns.R
### 
### Support functions for the OHI BC data_prep_lsp.R

get_wdpa_poly <- function(p4s_def  = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
                          p4s_name = 'wgs84', reload = FALSE) {
  ### Time consuming due to OGR load times of original WDPA database... 
  ### If BC-specific WDPA shapefile does not yet exist, or 
  ### reload == TRUE, read the main WDPA database, filter to just BC, and 
  ### save in the Git-Annex OHIBC directory.
  ### If the BC-specific file already exists, read into memory and return it.
  
  dir_spatial   <- file.path(dir_neptune_data, 'git-annex/bcprep/lsp/spatial')
  layer_bc     <- sprintf('%s_%s', layer_bc, p4s_name)
  
  if(!file.exists(file.path(dir_spatial, paste(layer_bc, '.shp', sep = ''))) | reload == TRUE) {
    ### Read most recent WDPA_MPA database from git-annex/globalprep
    wdpa_prod <- 'v2015/raw/WDPA_Jan2015_Public/WDPA_Jan2015_Public.gdb'
    dir_wdpa  <- file.path(dir_neptune_data, 'git-annex/globalprep/WDPA_MPA', wdpa_prod)
    
    cat(sprintf('Creating new OHIBC WDPA file.  Reading full WDPA shapefile from:\n  %s, ', dir_wdpa))
    
    layer_wdpa <- ogrListLayers(dir_wdpa) %>% 
      .[str_detect(., 'poly')]
    #   [1] "WDPA_poly_Jan2015"   "WDPA_point_Jan2015"  "WDPA_Source_Jan2015"
    #   attr(,"driver")
    #   [1] "OpenFileGDB"
    #   attr(,"nlayers")
    #   [1] 3
    # layer_wdpa <- 'WDPA_poly_Jan2015'
    cat(sprintf('layer = %s\n', layer_wdpa))
    
    poly_wdpa <- readOGR(dsn = dir_int, layer = layer_wdpa)
    ### can't use readShapePoly on a .gdb database...
    
    ### Filter down to just polygons within BC
    cat('Filtering full WDPA shapefile to Canadian regions...\n')
    poly_wdpa_bc <- poly_wdpa[poly_wdpa@data$PARENT_ISO == 'CAN' | poly_wdpa@data$ISO3 == 'CAN', ]
    poly_wdpa_bc <- poly_wdpa_bc[toupper(poly_wdpa_bc@data$SUB_LOC) %in% c('CA-BC', 'MARINE'), ]
    
    cat('Filtering to only STATUS == Designated...\n')
    poly_wdpa_bc <- poly_wdpa_bc[poly_wdpa@data$STATUS == 'Designated', ]
    
    p4s_wdpa_orig <- proj4string(poly_wdpa_bc)
    cat(sprintf('Current shapefile CRS: %s\n', p4s_wdpa_orig))
    ### NOTE: projection is EPSG:4326 WGS 84 for the original WDPA shapefile
    
    if(p4s_wdpa_orig != p4s_def) {
      cat(sprintf('Reprojecting shapefile: \n  Original: %s \n  New:      %s'))
      poly_wdpa_bc <- spTransform(poly_wdpa_bc, p4s_def)
    }
    
    writePolyShape(poly_wdpa_bc, fn = file.path(dir_spatial, layer_bc))
    
  } else {
    
    cat(sprintf('Reading OHIBC WDPA shapefile from: \n  %s, layer = %s\n', dir_int, layer_bc))
    poly_wdpa_bc <- readOGR(dsn = dir_spatial, fn = layer_bc)
    p4s_wdpa_bc <- proj4string(poly_wdpa_bc)
    cat(sprintf('Current shapefile CRS: %s\n', p4s_wdpa_bc))
  }
  return(poly_wdpa_bc)
}


get_p4s <- function(rgn_list) {
  ### From a list of SpatialPolygonsDataFrames, determine whether all CRSs match.
  ### If so, return the CRS for all objects.
  
  p4s_opts <- c('EPSG:3005 NAD83/BC Albers' = '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0',
                'EPSG:4326 WGS 84'          = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  p4s_vec <- sapply(rgn_list, function(x) proj4string(x))
  if(length(unique(p4s_vec)) > 1) {
    cat('CRS of input polygons do not match! \n')
    cat(sprintf('%s: %s\n', names(p4s_opts[p4s_opts == p4s_vec]), p4s_vec))
    return(NULL)
  }
  p4s_rgn <- p4s_vec[1]
  names(p4s_rgn) <- names(p4s_opts[p4s_opts == p4s_rgn])
  cat(sprintf('All shapefiles: CRS = %s \n  (%s)\n', names(p4s_rgn), p4s_rgn))
  return(p4s_rgn)
}


get_extents <- function(rgn_list) {
  ### Determine the extents that fully include all regions in a list of
  ### SpatialPolygonsDataFrame objects.
  
  ex_list <- lapply(rgn_list, function(x) extent(x))
  
  ### establish extents to cover both offshore and inland, rounded to nearest km
  ex_xmin <- round(min(sapply(ex_list, function(x) x@xmin)) - 500, -3)
  ex_xmax <- round(max(sapply(ex_list, function(x) x@xmax)) + 500, -3)
  ex_ymin <- round(min(sapply(ex_list, function(x) x@ymin)) - 500, -3)
  ex_ymax <- round(max(sapply(ex_list, function(x) x@ymax)) + 500, -3)
  ext_rgn  <- extent(ex_xmin, ex_xmax, ex_ymin, ex_ymax)
}


get_base_raster <- function(ext, reso = 500, p4s_base, fn = NULL) {
  ### create a raster grid to establish the extents and resolution of raster objects.
  ### * ext is an raster::extent object
  ### * reso is resolution of cells
  ### * p4s_base is the CRS of the base raster, given as a proj.4 string
  ### * fn is an optional filename for saving the base raster.
  
  xcol <- (ext@xmax - ext@xmin)/reso; yrow <- (ext@ymax - ext@ymin)/reso
  
  base_raster <- raster(ext, yrow, xcol, crs = p4s_base)
  
  if(!is.null(fn))
    writeRaster(base_raster, fn, overwrite = TRUE)
  
  return(base_raster)
}


sum_prot_areas <- function(prot_area_list, reso = 500) {
  
  ### prot_area_list is a list, by region, of all cell values (STATUS_YR) within
  ### the region.  Turn the list into a data frame, by rgn_id and year.
  prot_area_df <- data.frame()
  for (rgn_id in names(prot_area_list)) {
    temp_df <- data.frame(rgn_id, year = unlist(prot_area_list[[rgn_id]]))
    prot_area_df <- rbind(prot_area_df, temp_df)
  }
  
  ### From prot_area_df, determine total area of protected marine areas.
  ### Count number of cells, including NAs.  If resolution is in meters,
  ### each cell has area of 1e6 m^2 / (resolution)^2
  
  a_per_cell <- 1e6/reso^2
  yr_min <- 1900
  yr_max <- 2015
  
  total_rgn_area <- prot_area_df %>%
    group_by(rgn_id) %>%
    summarize(tot_a_km2 = n()/a_per_cell) 
  
  ### Summarize # of protected cells by region and year 
  prot_area_years <- prot_area_df %>%
    filter(!is.na(year)) %>%
    group_by(rgn_id, year) %>%
    summarize(a_by_yr_km2 = n()/a_per_cell) 
  
  ### This creates a cross-joined table (CJ in data.table package) across all years and all region IDs
  yr_rgn_df <- as.data.frame(data.table::CJ(unique(prot_area_years$rgn_id), seq(yr_min, yr_max)))
  names(yr_rgn_df) <- c('rgn_id', 'year')
  
  prot_area_years <- prot_area_years %>%
    full_join(yr_rgn_df, by = c('rgn_id', 'year')) %>%
    group_by(rgn_id) %>%
    arrange(year) %>%
    mutate(a_by_yr_km2 = ifelse(is.na(a_by_yr_km2), 0, a_by_yr_km2),
           cum_a_km2 = cumsum(a_by_yr_km2))
  
  prot_area_years <- prot_area_years %>%
    full_join(total_rgn_area, by = 'rgn_id')
  
  return(prot_area_years)
}

