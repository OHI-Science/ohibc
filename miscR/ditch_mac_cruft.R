
source('~/github/ohibc/src/R/common.R')  ### an OHIBC specific version of common.R;
  ### includes library(tidyverse); library(stringr)

all_files <- list.files(file.path('~/github/ohibc'), all.files = TRUE,
                        recursive = TRUE, full.names = TRUE)

cruft <- all_files[str_detect(basename(all_files), pattern = '^\\._|^.DS_Store')]

unlink(cruft)
