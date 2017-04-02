
library(ohicore) ### devtools::install_github('ohi-science/ohicore')

source('~/github/ohibc/src/R/common.R')

dir_ohibc  <- '~/github/ohibc'
dir_calc   <- file.path(dir_ohibc, 'calc_ohibc')
dir_master <- file.path(dir_calc, 'master')

source(file.path(dir_calc, 'calc_scores_fxns.R'))

##### Set up scenarios #####
### Set up scenarios by assigning years.  Each scenario (eg. 'region2015')
### will be tied to a specific data year, not floating as in global OHI.
scenario_years <- c(2005:2015) %>%
  setNames(paste0('region', .))

##### Prepare scenario folders #####
### * create scenario dirs and subdirs
### * copy config files from master to scenarios

prep_scenario_dirs(dir_calc)

##### Copy layers from prep to layers folder #####
### * Read in the layers.csv file with paths to the data files
### * Register layers based on include == TRUE and identify file locations
### * check that all data layers are available based on file paths
### * copy layers into current layers folder

message('Copying all layers to ', file.path(dir_calc, 'layers'))

layers_log <- read_csv(file.path(dir_master, 'layers_ohibc.csv')) %>%
  mutate(dir_prep = file.path(dir_ohibc, str_replace(dir_prep, 'ohibc:', '')))

lyrs <- register_layers(layers_log, dir_calc)

verify_layers(lyrs)

copy_layers_to_scenario(lyrs)

write_csv(lyrs %>% dplyr::select(-path_in, -path_in_exists, -path_out),
          file.path(dir_calc, 'layers.csv'))



##### Check that all layers match up to config #####
conf   <- Conf(file.path(dir_calc, 'conf'))

CheckLayers(layers.csv = file.path(dir_calc, 'layers.csv'),
            layers.dir = file.path(dir_calc, 'layers'),
            flds_id    = conf$config$layers_id_fields)




##### Iterate across scenario years #####
### * Initialize conf, layers; store working directory in
###   layers$data$dir_calc so it can be accessed by functions.R
### * Initialize scores_all dataframe to hold scores for all years
### * Calculate scores for each scenario and append to scores_all

conf   <- Conf(file.path(dir_calc, 'conf'))
layers <- Layers(layers.csv = file.path(dir_calc, 'layers.csv'),
                 layers.dir = file.path(dir_calc, 'layers'))
layers$data$dir_calc    <- dir_calc

scores_all <- data.frame() ### initialize scores

for (yr_i in seq_along(scenario_years)) {
  # yr_i <- 3

  scenario <- names(scenario_years)[yr_i]
  status_year <- scenario_years[yr_i]

  ### For each run through loop, assign status_year inside
  ###   the layers object/env't so it is accessible to functions.R
  layers$data$status_year <- status_year

  scores <- CalculateAll(conf, layers) %>%
    mutate(year = status_year)

  scores_all <- scores_all %>%
    bind_rows(scores)

}

write_csv(scores_all, file.path(dir_calc, 'scores_all.csv'))
