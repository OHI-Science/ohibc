rasterize_layers <- function(tmp_hab_lyrs) {

  ### Set up temp directory to store rasters; we'll save the completed raster then get rid of these.
  dir_tmp <- file.path(dir_rast, 'tmp')
  dir.create(dir_tmp, showWarnings = FALSE)

  for (i in 1:nrow(tmp_hab_lyrs)) { # i = 2
    message('Reading layer: ', tmp_hab_lyrs$layer_fname[i])
    tmp_shp <- readOGR(dsn   = file.path(dir_anx, 'data', tmp_hab_lyrs$dir[i]),
                       layer = tmp_hab_lyrs$layer_fname[i],
                       stringsAsFactors = FALSE)
    if(tmp_shp@proj4string@projargs != p4s_bcalb) {
      message('Proj4string: ', tmp_shp@proj4string)
      message('Transforming to BC Albers')
      tmp_shp <- spTransform(tmp_shp, CRS(p4s_bcalb))
    }
    ### create a buffer around points and lines
    if(!is.na(tmp_hab_lyrs$buffer_m[i])) {
      message(sprintf('Buffering a %s layer, %s m buffer: %s',
                      tmp_hab_lyrs$layer_type[i], tmp_hab_lyrs$buffer_m[i], tmp_hab_lyrs$layer_fname[i]))
      system.time({
        tmp_poly <- rgeos::gBuffer(tmp_shp,
                                   byid     = TRUE,
                                   width    = tmp_hab_lyrs$buffer_m[i],
                                   capStyle = ifelse(tmp_hab_lyrs$layer_type[i] == 'line', 'FLAT', 'ROUND'))
      })
    } else {
      tmp_poly <- tmp_shp ### shp is already a polygon...
    }
    ### If no ID field, add one and populate it with sequential integers
    tmp_poly@data$raster_id <- c(1:nrow(tmp_poly@data))
    ### write poly to a temp file for the gdal_rasterize to access.
    writePolyShape(tmp_poly, file.path(dir_tmp, sprintf('tmp_poly_%s', i)))
  }


  tmp_rast_list <- list() ### initialize empty list to store rasters
  base_te <- c(rast_base@extent[1], rast_base@extent[3], rast_base@extent[2], rast_base@extent[4])
  base_tr <- raster::res(rast_base)

  for (i in 1:nrow(tmp_hab_lyrs)) { # i = 1
    ### Set up source and destination filepaths, and attribute ID to
    ### use as raster cell value (default to raster_id created above)
    src_file <- path.expand(file.path(dir_tmp, sprintf('tmp_poly_%s.shp', i)))
    dst_file <- path.expand(file.path(dir_tmp, sprintf('tmp_rast_%s.tif', i)))
    src_dbf  <- foreign::read.dbf(file.path(dir_tmp, sprintf('tmp_poly_%s.dbf', i)))
    a_id <- ifelse(is.na(tmp_hab_lyrs$attr_id[i]), 'raster_id', tmp_hab_lyrs$attr_id[i])

    message(sprintf('Rasterizing temporary polygon %s of %s\n  src  = %s \n  dest = %s\n  attr = %s, class = %s',
                    i, nrow(tmp_hab_lyrs), src_file, dst_file, a_id, class(src_dbf[[a_id]])))
    ### save a temp raster since gdal_rasterize seems to hate creating new files?
    # writeRaster(rast_base, path.expand(file.path(dir_tmp, sprintf('tmp_rast%s.tif', i))), overwrite = TRUE)
    system.time({
      tmp_rast_list[[i]] <- gdalUtils::gdal_rasterize(
        src_datasource = src_file,
        dst_filename   = dst_file,
        a = a_id, # attribute to burn
        a_nodata = NA,
        # at = TRUE,
        te = base_te,
        tr = base_tr,
        output_Raster = TRUE
      )
    })
    ### assign proj4string
    tmp_rast_list[[i]]@crs <- CRS(p4s_bcalb)
  }

  names(tmp_rast_list) <- tmp_hab_lyrs$layer_fname


  return(tmp_rast_list) ### A list object of temporary rasters
}


plot_raster <- function(rast,
                        title = '', scale_label = '',
                        scale_limits = c(0, max(values(rast), na.rm = TRUE)),
                        rev_scale = FALSE) {
  require(ggplot2)
  require(RColorBrewer)
  require(maptools)

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

  #   poly_rgn <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_rgn'), proj4string = CRS(p4s_bcalb))
  #   poly_rgn_df <- fortify(poly_rgn, region = 'rgn_id') %>%
  #     rename(rgn_id = id, x = long, y = lat) %>%
  #     mutate(rgn_id = as.integer(rgn_id))

  poly_land <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_land'), proj4string = CRS(p4s_bcalb))
  poly_land_df <- fortify(poly_land) %>%
    rename(x = long, y = lat)

  x <- c(159000, 1237700, 174000, 1223000) # from extent of poly_rgn; just hard code it.

  rast_plot <- ggplot(data = rast_pts, aes(x = x, y = y, group = group, fill = layer)) +
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
    geom_polygon(data = poly_land_df, color = 'gray70', fill = 'gray75', size = 0.1) +
    # geom_polygon(data = poly_rgn_df,  color = 'gray20', fill = NA,       size = 0.1) +
    coord_cartesian(xlim = c(x[1], x[2]), ylim = c(x[3], x[4]), expand = TRUE) +
    labs(title = title,
         fill  = scale_label,
         x = NULL, y = NULL)

  print(rast_plot)
}


#
# if (rgeosStatus()) {
#   nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
#                        proj4string=CRS("+proj=longlat +datum=NAD27"))
#   lps <- coordinates(nc1)
#   ID <- cut(lps[,1], quantile(lps[,1]), include.lowest=TRUE)
#   reg4 <- unionSpatialPolygons(nc1, ID)
#   row.names(reg4)
# }
