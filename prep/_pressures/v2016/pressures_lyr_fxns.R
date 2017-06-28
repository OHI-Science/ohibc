### Function to extract pressures, doing a number of calculations and
### manipulations... useful or not?
prs_extract <- function(prs_rast, rgn_poly, prs_rgn_file,
                        qtile = NULL,
                        xform = c('none', 'log')[1],
                        rescale = FALSE) {
  ### check libraries: raster, rgdal
  pkgs <- c('raster', 'rgdal', 'maptools')
  for (pkg in pkgs[!paste('package:', pkgs, sep = '') %in% search()]) {
    message('loading package: ', pkg)
    library(pkg, character.only = TRUE)
  }

  ### check that CRS match between pressure and polys; if not, reproject the raster(?)
  rgn_crs <- crs(rgn_poly)
  prs_crs <- crs(prs_rast)
  if(!is.na(rgn_crs) & !is.na(prs_crs)) {
    ### neither CRS is NA
    if(rgn_crs@projargs != prs_crs@projargs) {
      message('Reprojecting pressure raster to region CRS: ', rgn_crs@projargs)
      prs_rast1 <- raster::projectRaster(prs_rast, crs = rgn_crs, filename = )
    } else {
      message('Pressure raster and region polygons in same CRS: no reprojection')
      prs_rast1 <- prs_rast ### no need to reproject
    }
  } else {
    message('Missing coordinate reference system for pressure raster or region polygon.')
    return(NULL)
  }

  ### interpolate using python
  ### resample to 1 km

  ### mask raster with region polygons
  prs_rgn_rast <- mask(prs_rast1, rgn_poly) #, filename = prs_rgn_file)

  ### apply quantile
  if(!is.null(qtile)) {
    qvalue <- quantile(values(prs_rgn_rast), qtile/100, na.rm = TRUE)
    values(prs_rgn_rast)[!is.na(values(prs_rgn_rast)) & values(prs_rgn_rast) > qvalue] <- qvalue
  }

  ### apply transform
  if(xform == 'log') {
    prs_rgn_rast <- calc(prs_rgn_rast, fun = function(x) log(x + 1))
  }

  ### apply rescale
  if(rescale == TRUE) {
    min_val <- min(prs_rgn_rast %>% values, na.rm = TRUE)
    max_val <- max(prs_rgn_rast %>% values, na.rm = TRUE)

    prs_rgn_rast <- calc(prs_rgn_rast, fun = function(x) (x - min_val)/(max_val - min_val))
  }
  return(prs_rgn_rast)
}


### Function to calculate the annual mean for each cell in a raster,
### looking at monthly values; returns all years as a raster brick
annual_mean <- function(stressor_data, yr_init, yr_count = 10) {

  rast_list <- list() ### initialize list for raster brick
  for(i in seq(1, (12 * yr_count), by = 12)) { # i <- 1
    ### i is the first month of the year; skipping by 12 selects start of each year
    yr_index <- (i - 1)/12 + 1
    yr_num   <- yr_index - 1 + yr_init
    j <- i + 11 ### j is the last month of each year

    message('yr: ', yr_num, '; first month index = ', i, '; last month index = ', j)

    yr_data <- stressor_data[ , , i:j] #select the monthly layers for each year
    yr_mean <- apply(yr_data, 1:2, mean) # get the annual mean

    ### create an array with long, lat, aragonite mean data
    stressor_array  <- array(c(long, lat, yr_mean), dim = c(320, 384, 3))
    stressor_matrix <- apply(stressor_array, 3, cbind)

    ### lon=x, lat=y
    #     xlong <- stressor_matrix[ , 1]
    #     ylat  <- stressor_matrix[ , 2]

    stressor_df <- as.data.frame(stressor_matrix)
    names(stressor_df) <- c('x', 'y', 'value')

    ### set extent to lon/lat
    ext <- extent(stressor_df[ , 1:2])
    rast_empty <- raster(ext, ncol = 320, nrow = 384)

    ### rasterize the dataframe
    rast_out <- rasterize(x     = stressor_df[ , 1:2],
                          y     = rast_empty,
                          field = stressor_df[ , 3],
                          fun   = function(x, ...) mean(x),
                          progress = 'text')
    ### i had to create a mean function here for "multiple points in a cell"

    extent(rast_out) <- c(0, 360, -80, 90)
    ### data extent is 0, 360, -80, 90 (the -80 is not a typo)
    rast_out <- rotate(rast_out)
    ### shifts data from 0 - 360 to -180 - 180

    crs(rast_out) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
    ### set CRS to wgs84

    #     plot(rast_out1)
    #     library(maps)
    #     maps::map(add = TRUE, fill = TRUE, col = 'grey80')
    #     notice the empty cells, due to irregular grid, so plot using a different projection
    rast_list[yr_index] <- rast_out
    names(rast_list)[yr_index] <- paste('y', yr_num, sep = '')
  }

  message('Bricking stressor rasters for years: \n  ', paste(names(rast_list), collapse = ', '))

  rast_brick_out <- brick(rast_list)
  return(rast_brick_out)

}

### resample was giving errors when applying it to a raster brick.
### Wrapper around resample that loops over each layer in brick.
resample_brick <- function(rbrick, rbase,
                           method   = 'ngb',
                           progress = 'text',
                           filename,
                           overwrite = TRUE) {
  rbrick_list <- vector(mode = 'list', length = nlayers(rbrick))

  for (i in 1:nlayers(rbrick)) {
    message('Processing layer ', i, ' of ', nlayers(rbrick))
    rbrick_list[[i]] <- raster::resample(rbrick[[i]], rbase,
                                         method    = method,
                                         progress  = progress,
                                         filename  = str_replace(filename, '.tif', '_resample_tmp.tif'),
                                         overwrite = overwrite)
  }
  names(rbrick_list) <- names(rbrick)
  rbrick_resamp <- brick(rbrick_list)

  message('Writing raster brick to: ', filename)
  writeRaster(rbrick_resamp, filename, bandorder = 'BIL', overwrite = overwrite)

  unlink(str_replace(filename, '.tif', '_resample_tmp.tif'))
  return(rbrick_resamp)
}

### Function to run an interpolation model separately on each layer
### of a raster brick, e.g. independently for each year
interpolate_brick <- function(rbrick, subset_n = 500) {

  xy <- data.frame(xyFromCell(rbrick, 1:ncell(rbrick)))
  rbrick_list <- vector('list', length = nlayers(rbrick))  ### initialize list

  for (i in 1:nlayers(rbrick)) { # i <- 1
    message('Processing layer ', i, ' of ', nlayers(rbrick))
    v  <- getValues(rbrick[[i]])
    tmpdf <- cbind(xy, v) %>%
      sample_n(min(subset_n, nrow(xy)))
    xy1 <- tmpdf[ , 1:2]
    v1  <- tmpdf[ , 3]
    tps_model <- fields::Tps(xy1, v1)

    rbrick_list[[i]] <- interpolate(rbrick, tps_model)
  }
  names(rbrick_list) <- names(rbrick)
  rbrick_int <- brick(rbrick_list)

  return(rbrick_int)
}

