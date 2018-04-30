dir_git <- '~/github/ohibc'
source(file.path(dir_git, 'src/R/common.R'))  ### an OHIBC specific version of common.R
dir_anx <- file.path(dir_M, 'git-annex/bcprep')

x <- list.files(file.path(dir_anx, '_raw_data/dfo_khunter/fisheries/d2018'),
                recursive = TRUE, full.names = TRUE, pattern = 'shp$')

df <- data.frame(basename = basename(x),
                 dirname  = basename(dirname(x)))

df_sum <- df %>%
  mutate(year = str_extract(basename, '[0-9]{4}') %>%
           as.integer()) %>%
  group_by(dirname) %>%
  mutate(yrs = paste0(min(year), '-', max(year)),
         all_yrs = (n() == max(year) - min(year) + 1)) %>%
  group_by(dirname, year)
