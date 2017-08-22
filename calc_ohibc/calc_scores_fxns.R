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

  subdirs_scen <- file.path(dir_calc, c('layers', 'conf', 'spatial', 'gapfill'))
  for (subdir in subdirs_scen) {
    if(purge) {
      unlink(subdir, recursive = TRUE)
    }
    message('...Creating subdirectory: ', subdir)
    dir.create(subdir, showWarnings = FALSE)
  }

  ### copy configuration files from masters to scenario folder
  files_to <- file.path(dir_calc, 'conf',
                        c('functions.R', 'config.R', 'goals.csv', 'scenario_data_years.csv',
                        'pressures_matrix.csv', 'resilience_matrix.csv',
                        'resilience_categories.csv', 'pressure_categories.csv'))
  files_from <- file.path(dir_master,
                          basename(files_to) %>%
                            str_replace('\\.', paste0(master_flag, '\\.')))
  message('Copying: \n  ', paste(files_from, files_to, sep = ' to ', collapse = '\n  '))
  stopifnot(file.copy(files_from, files_to, overwrite = TRUE))

}

assemble_layers_csv <- function(dir_master) {

  l_files <- read_csv(file.path(dir_master, 'layers_files_master.csv'))
  l_meta  <- read_csv(file.path(dir_master, 'layers_meta_master.csv'))

  l_df <- left_join(l_files, l_meta, by = 'layer')

  return(l_df)
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

Setup <- function(){
  ref_pt_file <- file.path(layers$data$dir_calc, 'reference_pts.csv')
  unlink(ref_pt_file)

  ref_pts <- data.frame(year   = as.integer(),
                        goal   = as.character(),
                        method = as.character(),
                        reference_point = as.character())
  write_csv(ref_pts, ref_pt_file)
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

calc_trend <- function(scenario_df, years = NULL) {
  ### provide a dataframe of status by region by year; this will
  ### calculate the linear trend across the entire span, and then
  ### divide by the first value to get a proportional change. If
  ### a vector of years is provided, will calculate based on those

  if('score' %in% names(scenario_df) & !'status' %in% names(scenario_df)) {
    scenario_df <- scenario_df %>%
      rename(status = score)
  }

  if(!all(c('year', 'region_id', 'status') %in% names(scenario_df))) {
    stop('calc_trend() requires fields named "year", "region_id", and "status" or "score" - fix it!')
  }

  if(!is.null(years)) scenario_df <- scenario_df %>%
      filter(year %in% years)

  if(any(is.na(scenario_df$year))) {
    stop('calc_trend(): NA values for year not allowed')
  }

  max_year <- max(scenario_df$year)

  goalname <- scenario_df$goal[1]
  if(is.null(goalname)) goalname <- NA

  message('Calculating trend for goal ', goalname, ' for years ',
          paste(scenario_df$year %>% range(), collapse = ' - '))

  scenario_df <- scenario_df %>%
    filter(!is.na(region_id)) %>%
    arrange(region_id, year) %>%
    mutate(status_1 = first(status))

  trend <- scenario_df %>%
    group_by(region_id, status_1) %>%
    do(mdl = lm(status ~ year, data = . )) %>%
    summarize(
      region_id = region_id,
      score = 5 * coef(mdl)['year'] / status_1,
      score = ifelse(is.nan(score), NA, score),
      score = min(max(score, -1), 1)) %>% # set boundaries so trend does not go below -1 or above 1
    ungroup() %>%
    mutate(year  = max_year,
           goal  = goalname,
           dimension = 'trend',
           score = round(score, 5))

  trend <- trend %>%
    filter(!is.na(region_id))

  return(trend)

}

### Since for OHIBC all data will be lined up with status years, this is irrelevant
# get_data_year <- function(layernames, status_yr, layers) {
#
#   ### Returns a named vector of data years, name = layer name
#   data_year_df <- layers$data$scenario_data_years %>%
#     filter(layer %in% layernames & scenario_year == status_yr)
#
#   data_year <- data_year_df$data_year %>%
#     setNames(data_year_df$layer)
#
#   return(data_year)
#
# }

complete_years <- function(score_df, year_span,
                           method = c('carry',   'zero', 'none')[1],
                           dir    = c('forward', 'back', 'both')[3],
                           pad    = 5, ### pad the early part of the dataset by 5 years?
                           report_gaps = FALSE) {

  if('rgn_id' %in% names(score_df)) {
    message('The complete_years() function automagically renames "rgn_id" to "region_id" for your convenience.')
    score_df <- score_df %>%
      rename(region_id = rgn_id)
  }

  if(pad > 0) {
    message('Padding the year span by adding an additional ', pad, ' to the early part.')
    year_span <- (min(year_span) - pad) : (max(year_span))
  }

  data_range <- range(score_df$year, na.rm = TRUE)
  if(min(year_span) > data_range[1] | max(year_span) < data_range[2]) {
    min_yr <- min(min(year_span), data_range[1])
    max_yr <- max(max(year_span), data_range[2])
    message('Data year span (', data_range[1], ':', data_range[2],
            ') exceeds assigned year span (',
            min(year_span), ':', max(year_span),
            '); completing data sequence from ', min_yr, ' to ', max_yr, '.')
    year_span <- min_yr : max_yr
  }

  score_df <- score_df %>%
    mutate(gapfill = 0, gf_method = 'none') %>%
    complete(year = year_span, nesting(region_id), fill = list(gapfill = 1, gf_method = method)) %>%
    arrange(year)

  if(method == 'none') {
    if(report_gaps == FALSE) {
      score_df <- score_df %>% select(-gapfill, -gf_method)
    }
    return(score_df)
  }

  if(method == 'zero') {
    ### zero out numerics, though text fields will be carried
    ### in the next step
    score_df <- score_df %>%
      lapply(FUN = function(x) {
        if(!class(x) %in% c('character', 'factor')) x[is.na(x)] <- 0
        return(x)
      }) %>%
      data.frame()
  }

  if(dir %in% c('forward', 'both')) {
    score_df <- score_df %>%
      # group_by(region_id) %>%
      fill(-year, -region_id, .direction = 'down') # %>%
      # ungroup()
  }
  if(dir %in% c('back', 'both')) {
    score_df <- score_df %>%
      # group_by(region_id) %>%
      fill(-year, -region_id, .direction = 'up') # %>%
      # ungroup()
  }

  if(report_gaps == FALSE) {
    score_df <- score_df %>% select(-gapfill, -gf_method)
  }

  return(score_df)

}
