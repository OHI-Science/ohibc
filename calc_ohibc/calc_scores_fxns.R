prep_scenario_dirs <- function(dir_calc, purge = TRUE) {
  ### * unlink subfolders if necessary
  ### * create subfolders
  ### * sync the following from master file folder:
  ###   * functions.R, config.R
  ###   * goals.csv, pressures_matrix.csv, resilience_matrix.csv, resilience_weights.csv, pressure_categories.csv

  subdirs_scen <- file.path(dir_calc, c('layers', 'conf', 'spatial'))
  for (subdir in subdirs_scen) {
    if(purge) {
      unlink(subdir)
    }
    message('...Creating subdirectory: ', subdir)
    dir.create(subdir, showWarnings = FALSE)
  }

  ### copy configuration files
  files_to <- file.path('conf', c('functions.R', 'config.R', 'goals.csv',
                                            'pressures_matrix.csv', 'resilience_matrix.csv',
                                            'resilience_categories.csv', 'pressure_categories.csv'))
  files_from <- file.path('master', basename(files_to) %>%
                            str_replace('\\.', '_master\\.'))
  message('Copying: \n  ', paste(files_from, files_to, sep = ' to ', collapse = '\n  '))
  stopifnot(file.copy(file.path(dir_calc, files_from),
                      file.path(dir_calc, files_to),
                      overwrite = TRUE))
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
    message('The following layers paths do not exist:')
    message('\n  ', paste(paste(x$layer, x$path_in, sep = ': '), collapse = '\n  '))
    stop('Data cannot be found - check file paths/names in layers.csv')
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

### functions to support functions.R
write_ref_pts <- function(goal, method, ref_pt) {

  ref_pt_file <- file.path(layers$data$dir_calc, 'reference_pts.csv')

  if(!file.exists(ref_pt_file)) {
    warning('Reference point file does not exist! \n  ', ref_pt_file)
    return()
  }
  ref_pts <- read_csv(ref_pt_file)  %>%
    rbind(data.frame(year   = layers$data$status_year,
                     goal   = goal,
                     method = method,
                     reference_point = ref_pt))
  write_csv(ref_pts, ref_pt_file)

}

calc_trend <- function(status_year_df, years = NULL) {
  ### provide a dataframe of status by region by year; this will
  ### calculate the linear trend across the entire span, and then
  ### divide by the first value to get a proportional change. If
  ### a vector of years is provided, will calculate based on those

  if('score' %in% names(status_year_df) & !'status' %in% names(status_year_df)) {
    status_year_df <- status_year_df %>%
      rename(status = score)
  }

  if(!all(c('year', 'region_id', 'status') %in% names(status_year_df))) {
    stop('calc_trend() requires fields named "year", "region_id", and "status" or "score" - fix it!')
  }

  if(!is.null(years)) status_year_df <- status_year_df %>%
      filter(year %in% years)

  if(any(is.na(status_year_df$year))) {
    stop('calc_trend(): NA values for year not allowed')
  }

  max_year <- max(status_year_df$year)

  goalname <- status_year_df$goal[1]
  if(is.null(goalname)) goalname <- NA

  message('Calculating trend for goal ', goalname, ' for years ',
          paste(status_year_df$year %>% range(), collapse = ' - '))

  status_year_df <- status_year_df %>%
    arrange(region_id, year) %>%
    mutate(status_1 = first(status))

  trend <- status_year_df %>%
    group_by(region_id, status_1) %>%
    do(mdl = lm(status ~ year, data = . )) %>%
    summarize(
      region_id = region_id,
      score = 5 * coef(mdl)['year'] / status_1,
      score = min(max(score, -1), 1)) %>% # set boundaries so trend does not go below -1 or above 1
    ungroup() %>%
    mutate(year  = max_year,
           goal  = goalname,
           dimension = 'trend',
           score = round(score, 5))

  return(trend)

}

