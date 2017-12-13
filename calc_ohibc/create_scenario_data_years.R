### Create scenario_data_years matrix
library(tidyverse)
library(stringr)

dir_calc <- '~/github/ohibc/calc_ohibc'

lyrs_master <- read_csv(file.path(dir_calc, 'master/layers_files_master.csv')) %>%
  mutate(file_path = file.path(dir_prep, fn_data),
         file_path = str_replace(file_path, 'ohibc:', '~/github/ohibc/'))

lyrs_files <- lyrs_master$file_path %>%
  setNames(lyrs_master$layer)

lyrs_yrs_all <- lapply(lyrs_files, read_csv) %>%
  setNames(names(lyrs_files)) %>%
  bind_rows(.id = 'layer_name') %>%
  select('layer_name', 'year') %>%
  distinct() %>%
  filter(!is.na(year))

lyrs_yrs <- lyrs_yrs_all %>%
  mutate(data_year = year,
         scenario_year = year) %>%
  filter(year <= 2016 & year >= 2001) %>%
  group_by(layer_name) %>%
  complete(scenario_year = 2001:2016) %>%
  fill(data_year, .direction = 'down') %>%
  fill(data_year, .direction = 'up') %>%
  select(-year) %>%
  mutate(data_year = as.integer(data_year),
         scenario_year = as.integer(scenario_year))

write_csv(lyrs_yrs, file.path(dir_calc, 'master/scenario_data_years_master.csv'))

write_csv(lyrs_yrs_all, file.path(dir_calc, 'master/all_data_years.csv'))
