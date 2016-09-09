CW = function(layers){

  # layers
  lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash',
            'cw_pesticide_trend', 'cw_fertilizer_trend', 'cw_coastalpopn_trend', 'cw_pathogen_trend')

  d <-  SelectLayersData(layers, layers=lyrs)  %>%
    select(region_id = id_num, layer, value = val_num)

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }


  d_pressures <- d %>%
    filter(layer %in% grep('po_', lyrs, value=TRUE))  %>%
    mutate(pressure = 1 - value) %>%  # invert pressures
    group_by(region_id) %>%
    summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    ungroup()

  d_trends <- d %>%
    filter(layer %in% grep('_trend', lyrs, value=TRUE)) %>%
    mutate(trend = -1 * value)  %>%  # invert trends
    group_by(region_id) %>%
    summarize(score = mean(trend, na.rm = TRUE)) %>%
    mutate(dimension = "trend") %>%
    ungroup()


  # return scores
  scores = rbind(d_pressures, d_trends) %>%
    mutate(goal = "CW") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CW", method = "spatial: pressures scaled from 0-1 at raster level",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  return(scores)
}
