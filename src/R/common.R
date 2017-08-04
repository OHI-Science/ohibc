
# set the neptune data_edit share based on operating system
dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

dir_M <- path.expand(dir_M)

# install (if necessary) and load commonly used libraries
packages <- c('tidyverse')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  cat(sprintf("Installing %s\n", setdiff(packages, rownames(installed.packages()))))
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(tidyverse)
library(RColorBrewer)
library(stringr)
rm(packages)

### Set up some options
# options(scipen = "999")           ### Turn off scientific notation
options(stringsAsFactors = FALSE) ### Ensure strings come in as character types


### generic theme for all plots
ggtheme_plot <- function(base_size = 9) {
  theme(axis.ticks = element_blank(),
               text             = element_text(family = 'Helvetica', color = 'gray30', size = base_size),
               plot.title       = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
               panel.background = element_blank(),
               legend.position  = 'right',
               panel.border     = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_line(colour = 'grey90', size = .25),
               # panel.grid.major = element_blank(),
               legend.key       = element_rect(colour = NA, fill = NA),
               axis.line        = element_blank()) # element_line(colour = "grey30", size = .5))
}

show_dupes <- function(x, y, na.rm = FALSE) {
  if(na.rm)
    x <- x[!is.na(x[[y]]), ]

  # x is data frame, y is field (as character) within that dataframe
  z <- x[x[[y]] %in% x[[y]][duplicated(x[[y]])], ]
}

get_rgn_names <- function() {
  x <- foreign::read.dbf('~/github/ohibc/prep/_spatial/ohibc_rgn.dbf', as.is = TRUE) %>%
    dplyr::select(rgn_id, rgn_name, rgn_code) %>%
    bind_rows(data.frame(rgn_id   = 0,
                         rgn_name = 'British Columbia',
                         rgn_code = 'BC'))
}

clean_df_names <- function(df) {
  df <- df %>%
    setNames(tolower(names(.)) %>%
               str_replace_all('[^a-z0-9]+', '_') %>%
               str_replace_all('^_+|_+$', ''))
  return(df)
}
