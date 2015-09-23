require(ggplot2)
require(RColorBrewer)

poly_plot_scores <- function(score_df, 
                        scale_label = element_blank(),
                        title = 'OHIBC status',
                        scale_limits = c(0, 100)) {
### This function takes a dataframe of scores and plots them to appropriate BC regions.
### * 'score_df' is a data frame with variables rgn_id and score (use rename in main script)
### * The other arguments are pretty self explanatory
  
  ### Fortify the poly_rgn and poly_land SpatialPolygonsDataFrames into a
  ### data frame that ggplot can work with.  
  dir_spatial <- '~/github/ohibc/regions'
  p4s <- '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
  
#   if(!('score' %in% names(score_df))) {
#     sc_index <- 
#   }
  
  poly_rgn <- readShapePoly(fn = file.path(dir_spatial, 'ohibc_rgn'), proj4string = CRS(p4s))
  poly_rgn_df <- fortify(poly_rgn, region = 'rgn_id') %>%
    rename(rgn_id = id) %>%
    mutate(rgn_id = as.integer(rgn_id)) 
  
  poly_rgn_df <- poly_rgn_df %>%
    left_join(score_df, by = 'rgn_id')
  
  head(poly_rgn_df)

  poly_land    <- readShapePoly(fn = file.path(dir_spatial, 'ohibc_land'), proj4string = CRS(p4s))
  poly_land_df <- fortify(poly_land)
  
  df_plot <- ggplot(data = poly_rgn_df, aes(x = long, y = lat, group = group, fill = score)) +  
    theme(axis.ticks = element_blank(), axis.text = element_blank(),
          text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
          plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
          legend.position = 'right') +
    scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), na.value = 'gray80',
                         limits = scale_limits) + 
    geom_polygon(color = 'gray80', size = 0.1) +
    geom_polygon(data = poly_land_df, color = 'gray40', fill = 'gray45', size = 0.25) +
    ### df_plot order: EEZ score polygons, then land polygons (dark grey).
    labs(title = title, 
         fill  = scale_label,
         x = NULL, y = NULL)
  
  
  print(df_plot)
}
