
# set the neptune data_edit share based on operating system
dir_neptune_data = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
                     'Darwin'  = '/Volumes/data_edit',
                     'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# stop if directory doesn't exist
if (!file.exists(sprintf('%s/',dir_neptune_data))){
  stop(sprintf("The directory for variable dir_neptune_data set in src/R/common.R does not exist. Do you need to mount %s?", dir_neptune_data))
  
}

# install (if necessary) and load commonly used libraries
packages <- c('dplyr', 'tidyr', 'stringr', 'readr')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  cat(sprintf("Installing %s\n", setdiff(packages, rownames(installed.packages()))))
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
rm(packages)



git_prov <- function(git_file) {
### This function determines the most recent commit for a given file.
### The idea is to help promote provenance by identifying a particular
### version of a given file.
  suppressWarnings({
    git_info <- system(sprintf('git log --follow %s', git_file), intern = TRUE, ignore.stderr = TRUE)[1:3]
  })
  if(is.na(git_info[1])) {
    message(sprintf('File %s: git commit info unavailable.  Not version-tracked in Git?\n', git_file))
  } else {
    cat(sprintf('File %s most recent commit info:\n', git_file))
    cat(sprintf('  %s\n', git_info))
  }
  return(invisible(git_info))
}
