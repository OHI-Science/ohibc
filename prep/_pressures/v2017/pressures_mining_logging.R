### data explore logging, mining, hatcheries

source('~/github/ohibc/src/R/common.R')
library(sf)
dir_data <- file.path(dir_M, 'git-annex/bcprep/_raw_data/databc')

list.files(dir_data)
[1] "2016 Census - CSD_control_79d4a796-2ccd-4732-a01c-1a8fd26d254a_1.csv" "Boundaries - Census Subdivisions 2011"                               
[3] "crown_tenures"                                                        "logging_cutblocks"                                                   
[5] "mining"                                                               "prot_ecol_areas"                                                     
[7] "readme.txt"                                                           "salmon_hatcheries"                                                   
[9] "shellfish_hatcheries"                                                 "watersheds"

### function to get data from shapefile, then unique vals for each column
get_jist <- function(shp) {
  df <- shp %>%
    as.data.frame() %>%
    select(-geometry)
  
  df_jist <- lapply(df, FUN = function(x) {
      y = data.frame(vals = as.character(unique(x)))
    }) %>%
    setNames(names(df)) %>%
    bind_rows(.id = 'attribute') %>%
    group_by(attribute) %>%
    mutate(n_vals = n()) %>%
    ungroup()
  
  return(df_jist)
}

### crown tenures
dir_ct <- file.path(dir_data, 'crown_tenures', 'TA_CROWN_TENURES_SVW')

ct_geom <- read_sf(file.path(dir_ct, 'TA_CRT_SVW_geom.shp'))
### no actual spatial info here

ct_poly <- read_sf(file.path(dir_ct, 'TA_CRT_SVW_polygon.shp'))

ct_jist <- get_jist(ct_poly)
ct_jist_short <- ct_jist %>%
  filter(n_vals < 1000)

### logging cutblocks CSV
lcc_df <- read_csv(file.path(dir_data, 'logging_cutblocks/FTEN_CUT_BLOCK_POLY_SVW', 'FTN_C_B_PL.csv'))
lcc_jist <- get_jist(lcc_df %>% mutate(geometry = NA))
lcc_jist_short <- lcc_jist %>%
  filter(n_vals < 1000)

### logging cutblocks poly
lc_poly <- read_sf(file.path(dir_data, 'logging_cutblocks/VEG_CONSOLIDATED_CUT_BLOCKS_SP/CNS_CUT_BL_polygon.shp'))
lcp_jist <- get_jist(lc_poly)
lcp_jist_short <- lcp_jist %>%
  filter(n_vals < 1000)

### mining
dir_mining <- file.path(dir_data, 'mining')
list.files(dir_mining)
mt_poly <- read_sf(file.path(dir_mining, 'mining_tenure/MTA_CROWN_GRANT_MIN_CLAIM_SVW/MTCRWNGRNT_polygon.shp'))
mt_jist <- get_jist(mt_poly)
mt_jist_short <- mt_jist %>%
  filter(n_vals < 1000)
mt2_poly <- read_sf(file.path(dir_mining, 'mining_tenure2/MTA_ACQUIRED_TENURE_SVW/MTA_ACQ_TE_polygon.shp'))
mt2_jist <- get_jist(mt2_poly)
mt2_jist_short <- mt2_jist %>%
  filter(n_vals < 1000)
mth_poly <- read_sf(file.path(dir_mining, 'mining_tenure_history/MTA_ACQUIRED_TENURE_HISTORY_SP/MTCQRDTNRH_polygon.shp'))
mth_jist <- get_jist(mth_poly)
mth_jist_short <- mth_jist %>%
  filter(n_vals < 1000)
