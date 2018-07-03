library(ohicore) ### devtools::install_github('ohi-science/ohicore@dev')

source('~/github/ohibc/src/R/common.R')

dir_ohibc  <- '~/github/ohibc'
dir_calc   <- file.path(dir_ohibc, 'calc_ohibc')
dir_master <- file.path(dir_calc, 'master')

source(file.path(dir_calc, 'calc_scores_fxns.R'))

fis_from_jamie <- read_csv('https://raw.githubusercontent.com/OHI-Science/bc-fisheries/master/output/fis_status.csv') %>%
  left_join(get_rgn_names(), by = 'rgn_name') %>% ### attach rgn IDs
  select(rgn_id, rgn_name, year, status_ja = status)

fis_from_ohibc <- read_csv(file.path(dir_calc, 'scores_all.csv')) %>%
  filter(goal == 'FIS' & dimension == 'status') %>%
  select(rgn_id = region_id, year, status_bc = score)

combined <- full_join(fis_from_jamie, fis_from_ohibc, by = c('year', 'rgn_id')) %>%
  mutate(diff1 = status_ja - status_bc)

ok <- combined %>%
  filter(diff1 == 0) %>%
  group_by(rgn_id) %>%
  mutate(all_ok = n() == length(2007:2015)) %>%
  select(rgn_id, rgn_name, year, all_ok)
### SoG and Az Island are all the same scores.

not_ok <- combined %>%
  filter(diff1 != 0) %>%
  group_by(rgn_id) %>%
  mutate(all_wrong = n() == length(2007:2015))
### HG, WCVI, and Pac Offshore are all different scores.

### What is the cause of the differences?  Examine the layers from each repo...
dir_bc <- file.path(dir_ohibc, 'prep/fis/v2017/output')
dir_ja <- file.path('https://raw.githubusercontent.com/OHI-Science/bc-fisheries/master/output')

bc_files <- list.files(dir_bc)
# "dfo_catch.csv"         "ram_b_bmsy.csv"        "ram_f_fmsy.csv"        "rgn_catch_summary.csv"

f <- 'dfo_catch.csv'
dfo_ja <- read_csv(file.path(dir_ja, f)) %>%
  left_join(get_rgn_names(), by = 'rgn_name') %>%
  select(rgn_id, rgn_name, year, species, stockid, ja_value = rgn_ass_catch_prop)
dfo_bc <- read_csv(file.path(dir_bc, f)) %>%
  select(rgn_id, year, species, stockid, bc_value = rgn_ass_catch_prop)
dfo_both <- full_join(dfo_ja, dfo_bc, by = c('rgn_id', 'year', 'species', 'stockid')) %>%
  mutate(diff = bc_value - ja_value)

### OK, there are diffs between the dfo_catch values here... need to rerun the layer generation scripts?
