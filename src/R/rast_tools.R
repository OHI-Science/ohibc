plot_rast <- function(rast,
                      title = '',
                      scale_label  = '',
                      scale_limits = NULL,
                      rev_scale    = FALSE) {

  pkgs <- c('ggplot2', 'RColorBrewer', 'maptools')
  for(pkg in pkgs[!(paste('package:', pkgs, sep = '')) %in% search()]) {
    library(pkg, character.only = TRUE)
  }

  rast_pts <- raster::rasterToPoints(rast) %>%
    as.data.frame()
  names(rast_pts) <- c('x', 'y', 'layer')
  rast_pts <- rast_pts %>%
    mutate(group = 1) ### need 'group' variable to plot below...

  if(rev_scale == TRUE) {
    cols <- rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme, low = blue
  } else {
    cols <- colorRampPalette(brewer.pal(11, 'Spectral'))(255) # rainbow color scheme, low = red
  }

  ### set up polys for land and regions, using simplified shapes
  p4s_bcalb   <- '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'

  if(exists('git_prov')) poly_rgn <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_rgn_simple'),
                                 proj4string = CRS(p4s_bcalb),
                                 nogit = TRUE) ### see prov.R
  else poly_rgn <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_rgn_simple'),
                                 proj4string = CRS(p4s_bcalb))

  poly_rgn_df <- fortify(poly_rgn, region = 'rgn_id') %>%
    rename(x = long, y = lat, rgn_id = id) %>%
    mutate(rgn_id = as.integer(rgn_id))

  if(exists('git_prov')) poly_land <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_land_simple'),
                                  proj4string = CRS(p4s_bcalb),
                                  nogit = TRUE) ### from prov.R
  else poly_land <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_land_simple'),
                                  proj4string = CRS(p4s_bcalb),
                                  nogit = TRUE) ### from prov.R
  poly_land_df <- fortify(poly_land) %>%
    rename(x = long, y = lat)

  if(is.null(scale_limits))
    scale_limits <- c(min(rast_pts$layer, na.rm = TRUE),
                      max(rast_pts$layer, na.rm = TRUE))

  rast_plot <- ggplot(data = rast_pts, aes(x, y, group = group, fill = layer)) +
    ### omit ticks, axis text:
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    ### set text style, title size and position, and legend position:
    theme(text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
          plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
          legend.position = 'right') +
    ### Blank background and grid:
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    scale_fill_gradientn(colours = cols, na.value = 'gray80',
                         limits = scale_limits) +
    geom_raster(alpha = .8) +
    geom_polygon(data = poly_land_df, color = 'gray60', fill = 'gray75', size = 0.02) +
    geom_polygon(data = poly_rgn_df, color = 'gray20', fill = NA, size = 0.05) +
    labs(title = title,
         fill  = scale_label,
         x = NULL, y = NULL)

  print(rast_plot)
}

# rast_base_1000 <- raster(file.path(dir_rgn, 'ohibc_base_raster_1000m.tif'))
# poly_rgn <- readOGR(dsn = path.expand(dir_rgn), layer = 'ohibc_rgn', stringsAsFactors = FALSE)
# rast_rgns_1000 <- rasterize(poly_rgn, rast_base_1000,
#                           field = 'rgn_id',
#                           filename = file.path(dir_rgn, 'ohibc_rgn_raster_1000m.tif'),
#                           overwrite = TRUE)

gdal_rast2 <- function(src, rast_base, dst = NULL, value = NULL, override_p4s = FALSE) {

  src <- path.expand(src)

  if(!str_detect(src, '.shp$')) src <- paste0(src, '.shp')
  ### add .shp if not present on src

  if(is.null(dst)) dst <- src %>% str_replace('.shp$', '.tif')
  ### if no dst, save it in same place as src

  ### check projections
  message('Checking projections...')
  shp_prj <- ogrInfo(dsn = dirname(src),
                     layer = basename(src) %>% str_replace('.shp$', '')) %>%
    .$p4s
  rst_prj <- rast_base@crs@projargs
  if(str_trim(shp_prj) != str_trim(rst_prj) & override_p4s == FALSE) {
    message('Shapefile and raster file do not seem to have same proj4string:')
    message('  shapefile: ', shp_prj)
    message('  raster:    ', rst_prj)
    stop('Exiting process; please resolve projections or set override_p4s = TRUE')
  } else {
    message('Shapefile and raster file seem to have same proj4string:\n  ', shp_prj)
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
  rast_tmp <- gdal_rasterize(
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
  writeRaster(rast_tmp, dst, overwrite = TRUE)

  ### unlink the temp raster because it's huge
  message('Unlinking temp raster')
  unlink(dst_tmp) ### delete temp

  ### reload raster from the compressed file and return it
  rast <- raster(dst)
}

