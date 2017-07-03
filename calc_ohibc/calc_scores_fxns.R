prep_scenario_dirs <- function(dir_calc,          ### destination location for scenario files
                               dir_master = NULL, ### source location of master files; NULL will default to <dir_calc>/master
                               master_flag = '_master', ### e.g. functions.R comes from functions_master.R
                               purge = TRUE) {    ### delete previous copies of layers, conf, and spatial
  ### * unlink subfolders if necessary
  ### * create subfolders
  ### * sync the following from master file folder:
  ###   * functions.R, config.R
  ###   * goals.csv, pressures_matrix.csv, resilience_matrix.csv, resilience_weights.csv, pressure_categories.csv

  if(is.null(dir_master)) dir_master <- file.path(dir_calc, 'master')

  subdirs_scen <- file.path(dir_calc, c('layers', 'conf', 'spatial'))
  for (subdir in subdirs_scen) {
    if(purge) {
      unlink(subdir)
    }
    message('...Creating subdirectory: ', subdir)
    dir.create(subdir, showWarnings = FALSE)
  }

  ### copy configuration files from masters to scenario folder
  files_to <- file.path(dir_calc, 'conf',
                        c('functions.R', 'config.R', 'goals.csv', 'status_year_matrix.csv',
                        'pressures_matrix.csv', 'resilience_matrix.csv',
                        'resilience_categories.csv', 'pressure_categories.csv'))
  files_from <- file.path(dir_master,
                          basename(files_to) %>%
                            str_replace('\\.', paste0(master_flag, '\\.')))
  message('Copying: \n  ', paste(files_from, files_to, sep = ' to ', collapse = '\n  '))
  stopifnot(file.copy(files_from, files_to, overwrite = TRUE))

}

register_layers <- function(layers_log, dir_calc) {

  ### filter the data and determines whether the file is available,
  ### save a register of ingested layers to scenario folder
  lyrs <- layers_log %>%
    filter(ingest == TRUE) %>%
    mutate(path_in        = file.path(dir_prep, fn_data),
           path_in_exists = file.exists(path_in),
           filename = paste0(layer, '.csv'),
           path_out = file.path(dir_calc, 'layers', filename)) %>%
    select(targets, layer, name, description,
           fld_value = name_data_fld, units,
           path_in, path_in_exists, filename, path_out) %>%
    arrange(targets, layer)
  write_csv(lyrs, file.path(dir_calc, 'layers_ingest.csv'))

  return(lyrs)
}


verify_layers <- function(lyrs) {
  if (nrow(filter(lyrs, !path_in_exists)) != 0) {
    x <- lyrs %>%
      filter(!path_in_exists) %>%
      select(layer, path_in)
    # message('The following layers paths do not exist:')
    # message('\n  ', paste(paste(x$layer, x$path_in, sep = ': '), collapse = '\n  '))
    stop('Data cannot be found - check file paths/names in layers.csv: ',
         '\n  ', paste(paste(x$layer, x$path_in, sep = ': '), collapse = '\n  '))
  } else {
    message('All registered layers exist')
  }
}

copy_layers_to_scenario <- function(lyrs) {
  for (j in 1:nrow(lyrs)) { # j = 4
    layer_data <- lyrs$path_in[j]
    layer_copy <- lyrs$path_out[j]
    message('... copying ', layer_data, '\n         to ', layer_copy)
    stopifnot(file.copy(layer_data, layer_copy, overwrite = TRUE))
  }

  ### delete extraneous files
  files_extra <- setdiff(list.files(file.path(dir_calc, 'layers')),
                         as.character(lyrs$filename))
  if(length(files_extra) > 0) {
    message('Deleting extraneous files: \n  ', paste(files_extra, collapse = '\n  '))
    unlink(file.path(dir_calc, 'layers', files_extra))
  }

}

