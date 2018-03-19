### Create layer-to-target lookup from pressures, resilience
library(tidyverse)
library(stringr)

dir_calc <- '~/github/ohibc/calc_ohibc'

prs_matrix <- read_csv(file.path(dir_calc, 'master/pressures_matrix_master.csv'))

prs_df <- prs_matrix %>%
  gather(layer, value, -goal, -element) %>%
  filter(!is.na(value)) %>%
  select(layer, target = goal, target_element = element) %>%
  mutate(dimension = 'pressure')

res_matrix <- read_csv(file.path(dir_calc, 'master/resilience_matrix_master.csv'))

res_df <- res_matrix %>%
  gather(layer, value, -goal, -element) %>%
  filter(!is.na(value)) %>%
  select(layer, target = goal, target_element = element) %>%
  mutate(dimension = 'resilience')

status_df <- read_csv(file.path(dir_calc, 'master/layers_meta_master.csv')) %>%
  select(layer, target = targets) %>%
  mutate(target = str_replace_all(target, 'pressure[s]?|resilience', ''),
         target = str_split(target, ' ')) %>%
  unnest(target) %>%
  filter(str_detect(tolower(target), '[a-z]+')) %>%
  mutate(dimension = 'status')

lookup <- bind_rows(prs_df, res_df, status_df)

write_csv(lookup, file.path(dir_calc, 'explore/int/layers_targets_master.csv'))
