
# set the neptune data_edit share based on operating system
dir_neptune_data = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
                     'Darwin'  = '/Volumes/data_edit',
                     'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# WARN rather than stop if directory doesn't exist
if (!file.exists(sprintf('%s/',dir_neptune_data))){
  warning(sprintf("The directory for variable dir_neptune_data set in src/R/common.R does not exist. Do you need to mount %s?", dir_neptune_data))

}

# install (if necessary) and load commonly used libraries
packages <- c('dplyr', 'tidyr', 'stringr', 'readr', 'ggplot2')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  cat(sprintf("Installing %s\n", setdiff(packages, rownames(installed.packages()))))
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
rm(packages)

### generic theme for all plots
ggtheme_basic <- theme(axis.ticks = element_blank(),
                       text = element_text(family = 'Helvetica', color = 'gray30', size = 8),
                       plot.title = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
                       legend.position = 'right')

ggtheme_plot <- ggtheme_basic +
  theme(panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey30"))

