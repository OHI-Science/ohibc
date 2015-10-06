
# set the neptune data_edit share based on operating system
dir_neptune_data = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
                     'Darwin'  = '/Volumes/data_edit',
                     'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# stop if directory doesn't exist
if (!file.exists(sprintf('%s/',dir_neptune_data))){
  stop(sprintf("The directory for variable dir_neptune_data set in src/R/common.R does not exist. Do you need to mount %s?", dir_neptune_data))
  
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



git_prov <- function(git_file) {
### This function determines the most recent commit for a given file.
### The idea is to help promote provenance by identifying a particular
### version of a given file.
  suppressWarnings({
    git_info <- system(sprintf('git log --follow %s', git_file), intern = TRUE, ignore.stderr = TRUE)[1:3]
    git_loc  <- system(sprintf('git config --get remote.origin.url'), intern = TRUE, ignore.stderr = TRUE)
    git_commit_url <- sprintf('%s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), gsub('commit ', '', git_info[1]))
  })
  if(is.na(git_info[1])) {
    msg_git <- sprintf('File `%s`: git commit info unavailable.  Not version-tracked in Git?', git_file)
  } else {
    ### convert commit info to a hyperlinked commit info string.
    git_info[1] <- sprintf('commit [%s](%s)', gsub('commit ', '', git_info[1]), git_commit_url)
    msg_git <- sprintf('File `%s`: most recent commit info: %s', git_file, paste(git_info, collapse = '; '))
  }
  return(msg_git)
}

script_prov <- function(script_file_name) {
  sys <- Sys.info()
  ses <- sessionInfo()
  
  msg_sys <- sprintf('System: %s, Release: %s. Machine: %s. User: %s.', sys['sysname'], sys['release'], sys['machine'], sys['user'])
  msg_ses <- sprintf('R version: %s, Platform: %s, Running under: %s.', 
                     ses$R.version$version.string, ses$R.version$platform, ses$running)
  msg_base_pcks <- sprintf('Attached base packages: %s', paste(ses$basePkgs, 
                                                               collapse = ', '))
  msg_att_pcks <- sprintf('Other attached packages: %s', paste(sapply(ses$otherPkgs, 
                                                                      function(x) paste(x$Package, x$Version, sep = '_')), 
                                                               collapse = ', '))
  ### Gather git info using system calls.  Convert commit # and remote origin url into a url for that commit.
  msg_git <- git_prov(script_file_name)
  
  ### Return all message strings within a named list for convenient reference.
  return(list('msg_sys' = msg_sys, 'msg_ses' = msg_ses, 'msg_git' = msg_git, 'msg_base_pcks' = msg_base_pcks, 'msg_att_pcks' = msg_att_pcks))
}
