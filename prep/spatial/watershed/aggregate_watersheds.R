### Create watershed group polygon
library(sf)
source('~/github/ohibc/src/R/common.R')

ws_poly <- read_sf(dsn   = file.path(dir_M, 'git-annex/bcprep/_raw_data/databc/watersheds/d2017'),
                   layer = 'FWA_BC_assessment_watersheds')

ws_poly <- ws_poly %>%
  aggregate.sf(by = 'ws_gpid', do_union = TRUE, simplify = TRUE)
