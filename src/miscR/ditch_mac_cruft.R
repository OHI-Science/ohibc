
# source('~/github/ohibc/src/R/common.R')  ### an OHIBC specific version of common.R;
  ### includes library(tidyverse); library(stringr)

all_files <- list.files(getwd(), all.files = TRUE,
                        recursive = TRUE, full.names = TRUE)

cruft <- all_files[stringr::str_detect(basename(all_files), pattern = '^\\._|^\\.DS_Store')]

unlink(cruft)
