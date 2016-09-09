


good_rasterize <- function(in_shp, out_tif, field) {
  if(!exists('rast_rgn'))
    rast_rgn <- raster('~/github/ohibc/prep/spatial/ohibc_rgn_raster_1000m.tif')
  te <- rast_rgn@extent
  base_te <- c(te@xmin, te@ymin, te@xmax, te@ymax)
  base_tr <- raster::res(rast_rgn)

  ### check range of values in field:
  shp_df <- foreign::read.dbf(paste0(in_shp %>% str_replace('.shp', ''), '.dbf'))
  message(sprintf('Values in field "%s": %s - %s; NAs: %s', field,
                  min(shp_df[field], na.rm = TRUE),
                  max(shp_df[field], na.rm = TRUE),
                  sum(is.na(shp_df[field]))))

  if(!str_detect(in_shp, '.shp'))
    in_shp <- paste0(in_shp, '.shp')

  out_tif_tmp <- out_tif %>%
    str_replace('.tif', '_tmp.tif')
  if(!file.exists(out_tif_tmp))
    raster::writeRaster(rast_rgn, out_tif_tmp)
  ### need a file there for gdalUtils::gdal_rasterize to do its work

  message('Rasterizing:\n  ', in_shp, '\nto temp tif: \n  ', out_tif_tmp)
  rast_tmp <- gdalUtils::gdal_rasterize(
    src_datasource = path.expand(in_shp),
    dst_filename   = path.expand(out_tif_tmp),
    a = field, # attribute to burn
    a_nodata = NA,
    # at = TRUE,
    te = base_te,
    tr = base_tr,
    output_Raster = TRUE)

  ### rewrite for compression and unlink the temp raster
  message('Compressing: \n  ', out_tif_tmp, '\nto: \n  ', out_tif)
  raster::writeRaster(rast_tmp, out_tif, overwrite = TRUE)
  unlink(out_tif_tmp)
  return(invisible(raster::raster(out_tif)))
}


### two automation functions:
### * create_poly_list takes a directory name, identifies the shapefiles,
###   trims the filename ends if needed, and returns a named list of the SPDFs.
### * rasterize_list() takes the named SPDFs list, and a field name argument,
###   and rasterizes each SPDF to the fisheries spatial folder.
### UPDATE WITH BETTER RASTER FUNCTION (using gdal_rasterize)
create_poly_list <- function(dir_shp, trim_end = 16) {
  shp_list <- list.files(dir_shp, full.names = TRUE)

  shp_list <- shp_list[str_detect(shp_list, '.shp$')] %>%
    str_replace('.shp', '')
  shp <- list()
  for (j in 1:length(shp_list)) { ### j <- 1
    shp[[j]] <- rgdal::readOGR(dsn   = dirname(shp_list[j]),
                               layer = basename(shp_list[j]),
                               stringsAsFactors = FALSE)
    names(shp[[j]]@data) <- tolower(names(shp[[j]]@data))
    if(shp[[j]]@proj4string@projargs == p4s_bcalb) {
      message('projection for ', basename(shp_list[j]), ' is in BC Albers')
    } else {
      message('projection for ', basename(shp_list[j]), ' is not in BC Albers; transforming projection')
      shp[[j]] <- spTransform(shp[[j]], CRS(p4s_bcalb))
    }
  }

  names(shp) <- basename(shp_list) %>%
    tolower() %>%
    str_sub(1, -1 - trim_end)

  return(shp)
}

library(doParallel)
library(foreach)
registerDoParallel(12)


rasterize_list <- function(poly_list, fld_name, shp_area = 1.6e7) {
  foreach(j = 1:length(poly_list)) %dopar% {
#  for (j in 1:length(poly_list)) { # j = 2
    lyr_name <- names(poly_list)[j]
      ### single bracket returns name of the list item; double returns the names of the columns in the object
    poly <- poly_list[[j]]
      ### double bracket returns the single object held in that list slot
    names(poly@data)[names(poly@data) == fld_name] <- 'fld'
    if("shape_area" %in% names(poly@data)) { ### autodetect shape area from field
      poly@data <- poly@data %>%
        mutate(fld_per_km2 = fld/(shape_area/1e6)) ### returns field value per square kilometer!
    } else { ### assign area manually
      message('No shape_area field; assuming poly areas of ', shp_area)
      poly@data <- poly@data %>%
        mutate(fld_per_km2 = fld/(shp_area/1e6))
    }
    rast_item <- rasterize(poly, rast_rgn,
                           field = 'fld_per_km2',
                           fun   = 'last',
                           filename = file.path(dir_goal_anx, 'spatial',
                                                sprintf('%s_%s.tif', lyr_name, fld_name)),
                           overwrite = TRUE)
  }
  return(TRUE)
}



zonestats <- function(stock, value, yrs, fun = 'sum') {
  fis_df <- data.frame()
  for (yr in yrs) { # yr <- 2001
    tmp_rast <- raster(file.path(dir_rast, sprintf('%s%s_%s.tif', stock, yr, value)))
    tmp_zonal <- zonal(tmp_rast, rast_rgn, fun = fun, digits = 0, na.rm = TRUE) %>%
      as.data.frame() %>%
      mutate(year = yr)
    fis_df <- bind_rows(tmp_zonal, fis_df)
  }
names(fis_df) <- c('rgn_id', value, 'year')
return(fis_df)
}

