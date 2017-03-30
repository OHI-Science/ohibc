prep_scen_dirs <- function(dir_calc, scenario) {
  ### * unlink scenario folder
  ### * create scenario folder and scenario subfolders
  ### * sync the following from master file folder:
  ###   * functions.R, config.R
  ###   * goals.csv, pressures_matrix.csv, resilience_matrix.csv, resilience_weights.csv, pressure_categories.csv

  dir_scen   <- file.path(dir_calc, scenario)

  ### set up scenario and subdirs
  message('Creating scenario directory: ', dir_scen)
  dir.create(dir_scen)

  subdirs_scen <- file.path(dir_scen, c('layers', 'conf', 'spatial'))
  for (subdir in subdirs_scen) {
    message('In ', scenario, ', creating subdirectory: ', subdir)
    dir.create(subdir, showWarnings = FALSE)
  }

  ### copy configuration files
  files_to <- file.path(scenario, 'conf', c('functions.R', 'config.R', 'goals.csv',
                                            'pressures_matrix.csv', 'resilience_matrix.csv',
                                            'resilience_categories.csv', 'pressure_categories.csv'))
  files_from <- file.path('master', basename(files_to) %>%
                            str_replace('\\.', '_master\\.'))
  message('For scenario ', scenario, ', copying: \n  ', paste(files_from, files_to, sep = ' to ', collapse = '\n  '))
  stopifnot(file.copy(file.path(dir_calc, files_from),
                      file.path(dir_calc, files_to),
                      overwrite = TRUE))
}

register_layers <- function(layers_log, dir_scenario) {

  ### filter the data and determines whether the file is available,
  ### save a register of ingested layers to scenario folder
  lyrs <- layers_log %>%
    filter(ingest == TRUE) %>%
    mutate(path_in        = file.path(dir_prep, fn_data),
           path_in_exists = file.exists(path_in),
           filename = paste0(layer, '.csv'),
           path_out = file.path(dir_scenario, 'layers', filename)) %>%
    select(targets, layer, name, description,
           fld_value = name_data_fld, units,
           path_in, path_in_exists, filename, path_out) %>%
    arrange(targets, layer)
  write_csv(lyrs, file.path(dir_scenario, 'layers_ingest.csv'))

  return(lyrs)
}
