
# set the neptune data_edit share based on operating system
dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

# install (if necessary) and load commonly used libraries
packages <- c('tidyverse')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  cat(sprintf("Installing %s\n", setdiff(packages, rownames(installed.packages()))))
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(tidyverse)
library(stringr)
rm(packages)

### generic theme for all plots
ggtheme_basic <- theme(axis.ticks = element_blank(),
                       text = element_text(family = 'Helvetica', color = 'gray30', size = 8),
                       plot.title = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
                       legend.position = 'right',
                       legend.key = element_rect(colour = NA, fill = NA))

ggtheme_plot <- ggtheme_basic +
  theme(panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey30"))

