### Create source lookup from provenance
library(tidyverse)
library(stringr)

dir_prep <- '~/github/ohibc/prep'
dir_calc <- '~/github/ohibc/calc_ohibc'

prov_logs <- list.files(dir_prep,
                        pattern = 'rmd.csv$', ignore.case = TRUE,
                        recursive = TRUE, full.names = TRUE)

prov_df_all <- lapply(prov_logs, FUN = function(x) {
  # x <- prov_logs[1]
  y <- read_csv(x) %>%
    filter(run_id == max(run_id)) %>%
    filter(filetype == 'output') %>%
    filter(str_detect(file_loc, '/output/')) %>%
    filter(!str_detect(file_loc, '/vHS/')) %>%
    mutate(filename = basename(file_loc),
           date = ifelse(is.na(commit_date), run_date, commit_date),
           dir_prep = str_replace(dirname(file_loc), '.+github/ohibc/', 'ohibc:'),
           prep_script = str_replace(parent_fn, '.+github/ohibc/', 'ohibc:')) %>%
    select(filename,
           date,
           prep_script,
           dir_prep) %>%
    distinct()

}) %>%
  bind_rows()

layers <- read_csv(file.path(dir_calc, 'master/layers_files_master.csv')) %>%
  select(filename = fn_data, layer)

prov_df <- layers %>%
  inner_join(prov_df_all, by = 'filename')

write_csv(prov_df, file.path(dir_calc, 'explore/int/layers_source_master.csv'))
