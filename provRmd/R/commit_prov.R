#' Set up a commit for new provenance-tracked files
#'
#' Files flagged as "outputs" from the provenance-tracked script
#' are given a Git commit ID and a commit message.  This script does not
#' push the files, only commits them.
#' @param git_file A valid file name (relative or absolute)
#' @param filetype The role of this file within this context: 'input', 'output', 'parent_script', or 'sourced_script'.  Defaults to 'input'.
#' @param nolog Should this git provenance information be omitted from the log file? Defaults to FALSE.
#' @examples
#' commit_prov()

commit_prov <- function(script_file, tag) {
  ### from prov_track, identify all output files with uncommitted changes;
  ### commit them and add new commit info to prov_track

  require('dplyr'); require('tidyr'); require('stringr'); require('readr'); require('knitr')


  ### Stage all new files in repository
  assign('prov_track', prov_track %>%
    mutate(uncommitted_changes = as.logical(uncommitted_changes),
           uncommitted_changes = ifelse(is.na(uncommitted_changes), TRUE, uncommitted_changes)),
    envir = .GlobalEnv)
  prov_staged <- prov_track %>%
    filter(str_detect(filetype, 'out') & uncommitted_changes == TRUE)
  if (nrow(prov_staged) > 0) {
    for (i in 1:nrow(prov_staged)) {
      git_add <- system2('git', args = sprintf('add %s', prov_staged$file_loc[i]), stderr = TRUE, stdout = TRUE)
      prov_staged$git_staged[i] <- (length(git_add) == 0)
    }
    assign('prov_track', prov_track %>%
             left_join(prov_staged %>%
                  dplyr::select(file_loc, git_staged) %>%
                  unique(),
                by = 'file_loc'),
           envir = .GlobalEnv)

  } else {
    assign('prov_track', prov_track %>%
             mutate(git_staged = NA),
           envir = .GlobalEnv)

  }

  git_commit <- system2('git', args = sprintf('commit -m "Running script %s; %s"', script_file, tag), stderr = TRUE, stdout = TRUE)
  message(sprintf('%s', paste(git_commit, collapse = '\n')))

  git_id <- system2('git', args = 'rev-parse HEAD', stderr = TRUE, stdout = TRUE)

  git_loc  <- system2('git', args = 'config --get remote.origin.url', stderr = TRUE, stdout = TRUE)
  git_commit_url <- sprintf('%s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), git_id)

  suppressWarnings({
    git_info <- system2('git', args = sprintf('show %s', git_id), stderr = FALSE, stdout = TRUE)[1:3]
  })

  assign('prov_track', prov_track %>%
           mutate(git_staged    = ifelse(is.na(git_staged), FALSE, git_staged),
           commit_url    = commit_url,
           commit_author = commit_author,
           commit_date   = commit_date),
         envir = .GlobalEnv)

  assign('prov_track', prov_track %>%
                  mutate(commit_url    = ifelse(git_staged == TRUE, sprintf('New commit: %s', git_commit_url), commit_url),
           commit_author = ifelse(git_staged == TRUE, sub('Author: ', '', git_info[2]), commit_author),
           commit_date   = ifelse(git_staged == TRUE, sub('Date: ', '', git_info[3]), commit_date),
           uncommitted_changes = ifelse(git_staged == TRUE, FALSE, uncommitted_changes)) %>%
         dplyr::select(-git_staged),
         envir = .GlobalEnv)

}

