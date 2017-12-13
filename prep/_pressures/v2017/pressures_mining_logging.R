### data explore logging, mining, hatcheries

source('~/github/ohibc/src/R/common.R')
library(sf)
dir_data <- file.path(dir_M, 'git-annex/bcprep/_raw_data/databc')

list.files(dir_data)
# [1] "2016 Census - CSD_control_79d4a796-2ccd-4732-a01c-1a8fd26d254a_1.csv" "Boundaries - Census Subdivisions 2011"
# [3] "crown_tenures"                                                        "logging_cutblocks"
# [5] "mining"                                                               "prot_ecol_areas"
# [7] "readme.txt"                                                           "salmon_hatcheries"
# [9] "shellfish_hatcheries"                                                 "watersheds"

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

### mining
dir_mining <- file.path(dir_data, 'mining')
list.files(dir_mining)

# This geographic dataset (feature class) contains a subset of records
# (from the Tantalis Survey Parcels) that represent the spatial location
# of crown granted mineral claims issued between 1874 to 1957.

# Additional information about crown granted 2-post mineral claims can be
# found on the branch website:
# http://www2.gov.bc.ca/gov/content/industry/mineral-exploration-mining/mineral-titles/news-notices-announcements/notices-crown-granted-2-post-mineral-claims
mt_poly <- read_sf(file.path(dir_mining, 'mining_tenure/MTA_CROWN_GRANT_MIN_CLAIM_SVW/MTCRWNGRNT_polygon.shp'))
mt_jist <- get_jist(mt_poly)
mt_jist_short <- mt_jist %>%
  filter(n_vals < 1000)
mt_df <- mt_poly %>%
  as.data.frame() %>%
  select(-geometry)

# Holds data for mineral, placer claims and leases, as well as
# coal licence applications, licences and leases within the Province
# of British Columbia. This is the spatial view utilized by Mineral
# Titles Online tenure on the mineral, placer and coal viewers, respectively.
# It contains additional attributes related to each tenure.
mt2_poly <- read_sf(file.path(dir_mining, 'mining_tenure2/MTA_ACQUIRED_TENURE_SVW/MTA_ACQ_TE_polygon.shp'))
mt2_jist <- get_jist(mt2_poly)
mt2_jist_short <- mt2_jist %>%
  filter(n_vals < 1000)
mt2_df <- mt2_poly %>%
  as.data.frame() %>%
  select(-geometry)

# Holds historical data for mineral, placer claims and leases, as well as
# coal license applications, licenses and leases within the Province of
# British Columbia. This layer is utilized by Mineral Titles Online tenure
# history on the mineral, placer and coal viewers, respectively.
mth_poly <- read_sf(file.path(dir_mining, 'mining_tenure_history/MTA_ACQUIRED_TENURE_HISTORY_SP/MTCQRDTNRH_polygon.shp'))
mth_jist <- get_jist(mth_poly)
mth_jist_short <- mth_jist %>%
  filter(n_vals < 1000)
mth_df <- mth_poly %>%
  as.data.frame() %>%
  select(-geometry)

