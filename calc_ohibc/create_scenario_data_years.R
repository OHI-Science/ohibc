### Create scenario_data_years matrix

lyrs_yrs <- lapply(lyrs_files, read_csv) %>%
  setNames(basename(lyrs_files %>% str_replace('.csv', ''))) %>%
  bind_rows(.id = 'layer_name') %>%
  select('layer_name', 'year') %>%
  distinct() %>%
  filter(!is.na(year)) %>%
  mutate(data_year = year,
         scenario_year = year) %>%
  filter(year <= 2016 & year >= 2000) %>%
  group_by(layer_name) %>%
  complete(scenario_year = 2000:2016) %>%
  fill(data_year, .direction = 'down') %>%
  fill(data_year, .direction = 'up') %>%
  select(-year) %>%
  mutate(data_year = as.integer(data_year),
         scenario_year = as.integer(scenario_year))

write_csv(lyrs_yrs, file.path(dir_calc, 'master/scenario_data_years_master.csv'))
