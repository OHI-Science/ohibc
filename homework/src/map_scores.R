require(tmap)
require(RColorBrewer)
require(maptools)

map_scores <- function(score_obj,
                       score_var   = 'score',           ### character or vector
                       scale_label = score_var,        ### character or vector
                       map_title   = 'OHIBC status') {  ### character or vector
### This function takes a dataframe of scores and applies them to a map of BC regions.
### * 'score_obj' is a data frame with variables rgn_id and one or more score variables.
### * score_vars is a vector of column names; tmap will print small
###   multiples - one map for each column. Default: "score"
### * scale_labels is a vector of scale names; this should either be a single
###   name (for all scales) or a vector of same length as score_vars (so each
###   map scale gets its own title).  Defaults to the same as score_vars.
### * map_titles is similar to scale_labels

  dir_poly <- '~/github/ohibc/prep/spatial'
  p4s <- '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'

  prov_loaded <- any(str_detect(search(), 'provRmd'))

  if(prov_loaded) {
    poly_rgn  <- readShapePoly(fn = file.path(dir_poly, 'ohibc_rgn_simple'),
                               proj4string = CRS(p4s),
                               nogit = TRUE) ### see prov.R
    poly_land <- readShapePoly(fn = file.path(dir_poly, 'ohibc_land_simple'),
                               proj4string = CRS(p4s),
                               nogit = TRUE)

  } else {
    poly_rgn  <- readShapePoly(fn = file.path(dir_poly, 'ohibc_rgn_simple'),
                               proj4string = CRS(p4s)) ### see prov.R
    poly_land <- readShapePoly(fn = file.path(dir_poly, 'ohibc_land_simple'),
                               proj4string = CRS(p4s))
  }

  poly_rgn@data <- poly_rgn@data %>%
    left_join(score_obj, by = 'rgn_id')

  score_map <- tm_shape(poly_rgn, is.master = TRUE) +
    tm_polygons(col     = score_var,
                title   = scale_label,
                palette = 'RdYlBu',
                border.col = 'grey50')
  score_map <- score_map +
    tm_shape(poly_land) +
      tm_polygons(col = 'grey40', border.col = 'grey30') +
    tm_layout(title = map_title)

  print(score_map)
  return(invisible(score_map))

}
