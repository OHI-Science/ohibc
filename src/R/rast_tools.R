filter_rast <- function(rast, vals, cut = TRUE, revalue = NA) {
  ### revalue = NA leaves original values for cells in 'vals';
  ###   otherwise set to a new number (e.g. 1)
  ### cut = TRUE sets all cells not in 'vals' to NA
  ### cut = FALSE and revalue = NA would return the original raster
  rast_filtered <- rast

  for(i in 1:nlayers(rast_filtered)) {
    message('processing layer ', names(rast_filtered)[i])
    if(cut) {
      message('Cell values not in ', paste(vals, collapse = ', '), ' are set to NA')
      values(rast_filtered[[i]])[!values(rast_filtered[[i]]) %in% vals] <- NA
    }
    if(!is.na(revalue)) {
      message('Cell values in ', paste(vals, collapse = ', '), ' are set to ', revalue)
      values(rast_filtered[[i]])[values(rast_filtered[[i]]) %in% vals] <- revalue
    }
  }

  return(rast_filtered)
}

plot_rast_map <- function(rast,
                      rgn_poly = NULL,
                      title = '',
                      scale_label  = '',
                      scale_limits = NULL,
                      rev_scale    = FALSE) {

  pkgs <- c('tmap', 'RColorBrewer')
  for(pkg in pkgs[!(paste('package:', pkgs, sep = '')) %in% search()]) {
    library(pkg, character.only = TRUE)
  }

  # if(!exists('World')) data(World)

  if(!exists('.poly_bc_continent')) {
    message('reading ohibc continent shapefile')
    bc_cont <- rgdal::readOGR(path.expand('~/github/ohibc/prep/_spatial'), 'ohibc_continent')
    assign('.poly_bc_continent', bc_cont, envir = .GlobalEnv)
    ### assign to global and hide it...
  }

  if(is.null(scale_limits)) scale_limits <- c(min(values(rast), na.rm = TRUE),
                                              max(values(rast), na.rm = TRUE))

  rast_pal <- ifelse(rev_scale, '-RdYlBu', 'RdYlBu')

  message('building raster map')
  rast_map  <- tm_shape(rast, is.master = TRUE) +
      tm_raster(palette = rast_pal,
                n = 10,
                auto.palette.mapping = FALSE,
                legend.show = TRUE,
                textNA = 'no data',
                showNA = FALSE,
                alpha = .8) +
    tm_shape(.poly_bc_continent) +
      tm_polygons(col = 'grey45', border.col = 'grey40', lwd = .25)

  if(!is.null(rgn_poly)) {
    rast_map <- rast_map +
      tm_shape(rgn_poly) +
        tm_borders(col = 'blue', lwd = .5, alpha = .3)
  }

  rast_map <- rast_map +
    tm_layout(title = title, title.size = .75,
              bg.color = '#ddeeff',
      # title.position = c('left', 'top'),
      frame = FALSE,
      legend.text.size = .6,
      legend.title.size = .7,
      legend.outside = FALSE,
      legend.position = c('right', 'top'),
      legend.bg.color = 'white',
      legend.bg.alpha = .9,
      attr.outside = TRUE)

  message('printing raster map')
  print(rast_map)

  return(invisible(rast_map))
}

# rast_base_1000 <- raster(file.path(dir_rgn, 'ohibc_base_raster_1000m.tif'))
# poly_rgn <- readOGR(dsn = path.expand(dir_rgn), layer = 'ohibc_rgn', stringsAsFactors = FALSE)
# rast_rgns_1000 <- rasterize(poly_rgn, rast_base_1000,
#                           field = 'rgn_id',
#                           filename = file.path(dir_rgn, 'ohibc_rgn_raster_1000m.tif'),
#                           overwrite = TRUE)

gdal_rast2 <- function(src, rast_base, dst = NULL, value = NULL, override_p4s = FALSE) {

  src <- path.expand(src)

  if(!str_detect(src, '.shp$'))
    src <- paste0(src, '.shp')
  ### add .shp if not present on src

  if(is.null(dst))
    dst <- src %>%
      stringr::str_replace('.shp$', '.tif')
  ### if no dst, save it in same place as src

  ### check projections
  message('Checking projections...')
  shp_prj <- rgdal::ogrInfo(dsn = dirname(src),
                     layer = basename(src) %>% str_replace('.shp$', '')) %>%
    .$p4s

  rst_prj <- rast_base@crs@projargs

  if(str_trim(shp_prj) != str_trim(rst_prj) & override_p4s == FALSE) {
    cat('Shapefile and raster file do not seem to have same proj4string:\n')
    cat('  shapefile: ', shp_prj, '\n')
    cat('  raster:    ', rst_prj, '\n')
    stop('Exiting process; please resolve projections or set override_p4s = TRUE')
  } else {
    message('Shapefile and raster file seem to have same proj4string, or override_p4s == TRUE:\n  shapefile: ',
            shp_prj, '\n  raster:    ', rst_prj)
  }

  if(is.null(value)) { ### default: choose first numeric column as value
    message('No "value" set...')
    tmp_dbf  <- foreign::read.dbf(str_replace(src, '.shp$', '.dbf'))
    num_cols <- sapply(tmp_dbf, class) %in% c('numeric', 'integer')
    if(sum(num_cols) == 0) {
      message('No numeric column found in source shapefile')
      stop()
    } else {
      value <- names(tmp_dbf)[num_cols][1]
      message('Using "', value, '" column')
    }
  }

  dst_tmp  <- dst %>% str_replace('.tif$', '_tmp.tif')

  base_tr  <- raster::res(rast_base)
  base_ext <- raster::extent(rast_base)
  base_te  <- c(base_ext[1], base_ext[3], base_ext[2], base_ext[4])

  message('Initializing temp raster file at final location: \n  ', dst_tmp)
  file.copy(rast_base@file@name, dst_tmp) ### set up a file at the temp location

  message('Using gdalUtils::gdal_rasterize to rasterize polygons to temp raster')
  rast_tmp <- gdalUtils::gdal_rasterize(
    src_datasource = path.expand(src),
    dst_filename   = path.expand(dst_tmp),
    a = value, # attribute to burn
    a_nodata = NA,
    # at = TRUE,
    te = base_te,
    tr = base_tr,
    output_Raster = TRUE)

  ### writeRaster to write a compressed version in final location
  message('Writing final raster file using raster::writeRaster (for compression)\n  ', dst)
  raster::writeRaster(rast_tmp, dst, overwrite = TRUE)

  ### unlink the temp raster because it's huge
  message('Unlinking temp raster')
  unlink(dst_tmp) ### delete temp

  ### reload raster from the compressed file and return it
  rast <- raster::raster(dst)

  if(exists('git_prov')) {
    git_prov(src, 'input')
    git_prov(dst, 'output')
  }

  return(invisible(rast))
}

animate_rast <- function(rast_stack, gif_file, scale_lim = NULL, rev_scale = FALSE) {
  library(animation)

  if(is.null(scale_lim)) {
    scale_lim <- c(min(minValue(rast_stack), na.rm = TRUE), max(maxValue(rast_stack), na.rm = TRUE))
  }

  colorscale <- colorRampPalette(brewer.pal(9, 'Spectral'))(255) # rainbow color scheme
  if(rev_scale) colorscale <- rev(colorscale)

  capture.output({
    saveGIF({
      for(i in 1:nlayers(rast_stack)) {
        # don't forget to fix the zlimits
        plot(rast_stack[[i]], zlim = scale_lim,
             axes = FALSE,
             col  = colorscale,
             main = names(rast_stack[[i]]))
      }
    }, movie.name = gif_file)
  })
}
