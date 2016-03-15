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
  p4s_bcalb <- '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
  poly_rgn    <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_rgn_simple'),
                                proj4string = CRS(p4s_bcalb))
  poly_rgn_df <- fortify(rgn_poly, region = 'rgn_id') %>%
    rename(x = long, y = lat, rgn_id = id) %>%
    mutate(rgn_id = as.integer(rgn_id))

  poly_land    <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_land_simple'),
                                proj4string = CRS(p4s_bcalb))
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

# rast_base_500 <- raster(file.path(dir_rgn, 'ohibc_base_raster_500m.tif'))
# poly_rgn <- readOGR(dsn = path.expand(dir_rgn), layer = 'ohibc_rgn', stringsAsFactors = FALSE)
# rast_rgns_500 <- rasterize(poly_rgn, rast_base_500,
#                           field = 'rgn_id',
#                           filename = file.path(dir_rgn, 'ohibc_rgn_raster_500m'),
#                           overwrite = TRUE)

