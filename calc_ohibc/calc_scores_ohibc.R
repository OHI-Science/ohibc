
library(ohicore) ### devtools::install_github('ohi-science/ohicore')
# library(zoo)

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

### Flags to do all the things (or not)
do_layercopy  <- TRUE
do_layercheck <- TRUE
do_calculate  <- TRUE

##### Prepare scenario folders #####

for (scenario in names(scenario_years)) {   # scenario <- names(scenario_years)[1]

  unlink(file.path(dir_calc, scenario), recursive = TRUE) ### clear out old files
  # prep_scen_dirs(dir_calc, scenario)

}


for (yr_i in seq_along(scenario_years)) {
  # yr_i <- 2

  scenario <- names(scenario_years)[yr_i]
  status_year <- scenario_years[yr_i]
  dir_scen <- file.path(dir_calc, scenario)

  message('Scenario ', scenario, ': ', dir_scen)

  ##### Copy layers from prep to layers folder #####
  if (do_layercopy) {
    message('Copying all layers to ', file.path(dir_scen, 'layers'))

    ### Read in the layers.csv file with paths to the data files
    layers_log <- read_csv(file.path(dir_master, 'layers_ohibc.csv')) %>%
      mutate(dir_prep = file.path(dir_ohibc, str_replace(dir_prep, 'ohibc:', '')))

    lyrs <- register_layers(layers_log, dir_scen)

    ### checks that all data layers are available based on file paths
    if (nrow(filter(lyrs, !path_in_exists)) != 0) {
      message('The following layers paths do not exist: \n  ')
      print(filter(lyrs, !path_in_exists) %>% select(layer, path_in), row.names = FALSE)
      stop('Data cannot be found - check file paths/names in layers.csv')
    }

    ### copy layers into current scenario/layers folder
    for (j in 1:nrow(lyrs)) { # j = 4
      layer_data <- lyrs$path_in[j]
      layer_copy <- lyrs$path_out[j]
      message('... copying ', layer_data, '\n         to ', layer_copy)
      stopifnot(file.copy(layer_data, layer_copy, overwrite = TRUE))
    }

    ### delete extraneous files
    files_extra <- setdiff(list.files(file.path(dir_scen, 'layers')),
                           as.character(lyrs$filename))
    unlink(file.path(dir_scen, 'layers', files_extra))

    ### layers registry in scenario folder
    write_csv(lyrs %>% dplyr::select(-path_in, -path_in_exists, -path_out),
              file.path(dir_scen, 'layers.csv'))
  }

  ##### Check that all layers match up to config #####
  if (do_layercheck){
    conf   <- Conf(file.path(dir_scen, 'conf'))

    CheckLayers(layers.csv = file.path(dir_scen, 'layers.csv'),
                layers.dir = file.path(dir_scen, 'layers'),
                flds_id    = conf$config$layers_id_fields)
  }

  ##### Calculate scores for this scenario #####
  if (do_calculate){

    ### load configuration and layers;
    ### For each run through loop, assign status_year and dir_scen inside
    ###   the layers object/env't so they are accessible to functions.R
    conf   <- Conf(file.path(dir_scen, 'conf'))
    layers <- Layers(layers.csv = file.path(dir_scen, 'layers.csv'),
                     layers.dir = file.path(dir_scen, 'layers'))
    layers$data$status_year <- status_year
    layers$data$dir_scen    <- dir_scen

    ### calculate scores
    scores <- CalculateAll(conf, layers)
    write_csv(scores, file.path(dir_scen, 'scores.csv'))

  }

  # if (do.other){
  #   # spatial
  #   for (f in scenarios[[scenario]][['f_spatial']]){ # f <- f_spatial[1]
  #     stopifnot(file.exists(f))
  #     file.copy(f, sprintf('%s/spatial/%s', scenario, basename(f)))
  #   }
  #
  #   # delete old shortcut files
  #   for (f in c('launchApp.bat','launchApp.command','launchApp_code.R','scenario.R')){
  #     path <- sprintf('%s/%s',scenario,f)
  #     if (file.exists(path)) unlink(path)
  #   }
  #
  #   # save shortcut files not specific to operating system
  #   write_shortcuts(scenario, os_files=0)
  #
  #   # launch on Mac # setwd('~/github/ohi-global/eez2013'); launch_app()
  #   #system(sprintf('open %s/launch_app.command', scenario))
  # }
}


# ### make a plot to compare different commits within a scenario
#
# change_plot(repo = 'ohi-global', scenario='eez2016', commit='previous',
#            fileSave='eez2016_new_fis_pressures')
#
# change_plot(repo = 'ohi-global', scenario='eez2012', commit='previous',
#             fileSave='eez2012_hd_sb_hab')
#
#
# new_health <- read_csv('../ohiprep/globalprep/hab_combined/v2016/output/habitat_health_2016.csv')
# filter(new_health, rgn_id==80)
#
# new_health <- read_csv('../ohiprep/globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/hd_sb_subtidal_v2010.csv')
# filter(new_health, rgn_id==67)
#
# new_health <- read_csv('../ohiprep/globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/hd_sb_subtidal_v2006.csv')
# filter(new_health, rgn_id==67)
#
#
#   filter(goal == 'ICO') %>%
#   filter(dimension == 'status') %>%
#   left_join(geos) %>%
#   filter(region_id != 0) %>%
#   arrange(r2)
#
# mod <- lm(score ~ as.factor(r2), data=ico)
# summary(mod)
# anova(mod)

# source('../ohiprep/src/R/VisGlobal.R')
# # looking within a goal:
# scatterPlot(repo='ohi-global', scenario='eez2015', commit='previous', goal='CP', dim='pressures', fileSave='CP_pressure_eez2015')
#
# goalHistogram(scenario='eez2016', goal='AO', dim='status', fileSave='AO_need_eez2016')

# ## make an interactive table
#
# library(hwriter)
# names <- read_csv('eez2013/layers/rgn_labels.csv') %>%
#   filter(type=='eez') %>%
#   select(region_id=rgn_id, label)
#
#
# data <- read_csv('eez2015/scores.csv')%>%
#   filter(goal=='FIS', dimension=='status') %>%
#   filter(region_id !=0) %>%
#   left_join(names) %>%
#   select(country=label, goal, dimension, score) %>%
#   arrange(score)
# hwrite(data, 'changePlot_figures/status_eez2015_FIS.html', br = TRUE, center = TRUE, border=0,
#        row.style=list(goal='text-align:center'))
#
#
# tmp <- read_csv('changePlot_figures/MAR_trend_compare_eez2016.csv')
# plot(tmp$old_score, tmp$score, xlab='old trends', ylab='new trends')
# abline(0,1, col='red')
