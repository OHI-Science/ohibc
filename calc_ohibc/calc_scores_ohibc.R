
library(ohicore) ### devtools::install_github('ohi-science/ohicore')
library(zoo)

source('~/github/ohibc/src/R/common.R')

# # new paths based on host machine
### !!! ALL TOOLBOX DATA IN OHIBC REPO - NONE ON MAZU
# dirs = list(
#   neptune_data  = dir_M,
#   neptune_local = dir_M,
#   ohiprep       = '../ohiprep',
#   ohicore       = '../ohicore')

### !!!CHANGE THESE TO do_BLAH
# do_layercopy  <- TRUE
# do_layercheck <- TRUE
# do_calculate  <- TRUE
# do_other      <- FALSE

# scenario list (need to add new scenarios here)
### !!!does this duplicate (and risk conflicting with) layers_eez.csv?
scenarios <- list(
  eez2016     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2016a',
    fld_fn       = 'fn_2016a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = TRUE),
  eez2015     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2015a',
    fld_fn       = 'fn_2015a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = TRUE) ,

  eez2014     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2014a',
    fld_fn       = 'fn_2014a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = TRUE),

  eez2013     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = TRUE),

  eez2012     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2012a',
    fld_fn       = 'fn_2012a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = TRUE)
)

### sync functions.R:
# overwrite eez2012, eez2013, eez2014, eez2015 with eez2016
### !!!SET UP TO DO THIS BASED ON scenarios LIST OR CSV
for (dir in c('eez2012','eez2013', 'eez2014', 'eez2015')){
  stopifnot(file.copy('eez2016/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite = TRUE))
}

scenario_years <- c(2005:2015) %>%
  setNames(paste0('region', .))

for (i in seq_along(scenario_years)){
  # i <- 2

  scenario   <- names(scenarios)[i]
  fld_dir    <- scenarios[[i]][['fld_dir']]
  fld_fn     <- scenarios[[i]][['fld_fn']]
  layer <- scenarios[[i]][['layer']]
  # do         <- scenarios[[i]][['do']]

    print(scenario)
    print(fld_dir)
    print(fld_fn)
    # print(do)


  # if (!do) next()

  message('Scenario: ', scenario)

  ### create dirs for each scenario
  dirs_scenario <- c(scenario, sprintf('%s/%s', scenario, c('layers', 'conf', 'spatial')))
  for (dir in dirs_scenario) {
    if (!file.exists(dir)) dir.create(dir, showWarnings = FALSE)
  }

  if (do.layercopy){


## Read in the layers.csv file with paths to the data files
### !!!FIRST: READ THE LAYERS_EEZ
 g <- read_csv(sprintf('~/github/ohibc/calc_ohibc/%s.csv', layer))

### !!!CAN THIS BE DONE WITH FILL (forward and backward)?
 # carry forward file paths and names when no data for 2014 and/or 2015
    if (as.numeric(gsub('[a-z]', '', scenario)) > 2013){
      g <- g %>%
        dplyr::mutate(
          dir_2014a = ifelse(is.na(dir_2014a), dir_2013a, dir_2014a),
          fn_2014a = ifelse(is.na(fn_2014a), fn_2013a, fn_2014a)) %>%
        dplyr::mutate(
          dir_2015a = ifelse(is.na(dir_2015a), dir_2014a, dir_2015a),
          fn_2015a = ifelse(is.na(fn_2015a), fn_2014a, fn_2015a))%>%
        dplyr::mutate(
          dir_2016a = ifelse(is.na(dir_2016a), dir_2015a, dir_2016a),
          fn_2016a = ifelse(is.na(fn_2016a), fn_2015a, fn_2016a))
      }

    # replaces 'ohiprep' and 'neptune_data' parts of the filepath with the full file paths
    # 'ohiprep' files are located here: https://github.com/OHI-Science/ohiprep
    # 'neptune_data' files are located on the NCEAS Neptune server
### !!!USES dirs FROM ABOVE; WHICH LINKS TO MAZU OR OHIPREP.
### !!!KEEP ALL DATA FOR TOOLBOX IN OHIBC REPO - NOT NEEDED HERE
    g$dir_in <- sapply(
      str_split(g[[fld_dir]], ':'),
      function(x){ sprintf('%s/%s', '~/github/ohibc', x[2])})

### !!!FIND FILE NAME BASED ON COLUMN FROM THIS SCENARIO
    g$fn_in <- g[[fld_fn]]

    # filters the data and determines whether the file is available, saves a copy to tmp folder
    lyrs <- g %>%
      filter(ingest == TRUE) %>%
      mutate(
        path_in        = file.path(dir_in, fn_in),
        path_in_exists = file.exists(path_in),
        filename = sprintf('%s.csv', layer),
        path_out = sprintf('%s/layers/%s', scenario, filename)) %>%
      select(
        targets, layer, name, description,
        fld_value=name_data_fld, units,
        path_in, path_in_exists, filename, path_out) %>%
      # select(
      #   targets, layer,
      #   fld_value=name_data_fld,
      #   path_in, path_in_exists, filename, path_out) %>%
      arrange(targets, layer)
    # write_csv(lyrs, sprintf('%s/temp/layers_1-ingest.csv', scenario))

    ### USE FILE.EXISTS INSTEAD OF A COLUMN PATH_IN_EXISTS?
    # checks that all data layers are available based on file paths
    if (nrow(filter(lyrs, !path_in_exists)) != 0){
      message('The following layers paths do not exist:\n')
      print(filter(lyrs, !path_in_exists) %>% select(layer, path_in), row.names = FALSE)
      stop('Data cannot be found - check file paths/names in layers.csv')
    }

    # copy layers into specific scenario / layers file
    for (j in 1:nrow(lyrs)){ # j=4
      stopifnot(file.copy(lyrs$path_in[j], lyrs$path_out[j], overwrite = TRUE))
    }

    # delete extraneous files
    files_extra <- setdiff(list.files(sprintf('%s/layers',scenario)), as.character(lyrs$filename))
    unlink(sprintf('%s/layers/%s', scenario, files_extra))

    # layers registry
    write_csv(dplyr::select(lyrs, -path_in, -path_in_exists, -path_out), sprintf('%s/layers.csv', scenario))
  }

  if (do.layercheck){
    # load conf
    conf   <- Conf(sprintf('%s/conf', scenario))

    # run checks on layers
    CheckLayers(layers.csv = sprintf('%s/layers.csv', scenario),
                layers.dir = sprintf('%s/layers', scenario),
                flds_id    = conf$config$layers_id_fields)
    # system(sprintf('open %s/layers.csv', scenario))
  }

  ### !!! HERE IS THE CALCULATE PART OF THE LOOP
  if (do.calculate){
    # calculate scores from directory of scenario
    setwd(sprintf('%s', scenario)) # load_all(dirs$ohicore)

    # load configuration and layers
    conf   <- Conf('conf')
    layers <- Layers(layers.csv = 'layers.csv', layers.dir = 'layers')


    # calculate scores
    #try({    })
    scores <- CalculateAll(conf, layers)
    write_csv(scores, 'scores.csv')

    # restore working directory
    setwd('..')

    # archive scores on disk (out of github, for easy retrieval later)
    csv <- sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores_%s_%s.csv',
                  dirs$neptune_data, scenario, format(Sys.Date(), '%Y-%m-%d'))
    write_csv(scores, csv)
  }

  if (do.other){
    # spatial
    for (f in scenarios[[scenario]][['f_spatial']]){ # f <- f_spatial[1]
      stopifnot(file.exists(f))
      file.copy(f, sprintf('%s/spatial/%s', scenario, basename(f)))
    }

    # delete old shortcut files
    for (f in c('launchApp.bat','launchApp.command','launchApp_code.R','scenario.R')){
      path <- sprintf('%s/%s',scenario,f)
      if (file.exists(path)) unlink(path)
    }

    # save shortcut files not specific to operating system
    write_shortcuts(scenario, os_files=0)

    # launch on Mac # setwd('~/github/ohi-global/eez2013'); launch_app()
    #system(sprintf('open %s/launch_app.command', scenario))
  }
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
