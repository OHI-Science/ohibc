### prov.R
###
### This script sets up provenance tracking for a script.
### When sourced, initializes a global variable prov_track to be NULL.
###
### Two functions (so far):
### git_prov(git_file, type = 'input') takes filename and reads its
###   git log, strips to its most recent commit, adds a line to prov_track
###   for this file, and returns a git provenance dataframe including:
###   * file_loc
###   * type
###   * commit_url
###   * commit_author
###   * commit_date
###   * uncommitted_changes
###
### script_prov(script_file) is called at the end of a script.  It
###   calls git_prov() for this script; then calls Sys.info() and
###   sessionInfo() to get run information.  Then it appends (or creates)
###   a log for this run of this script, with all the info from the
###   prov_track variable as well as session/system info.

prov_track   <- NULL ### initialize the prov_track global variable when source()d
script_track <- NULL ### initialize the script_track global variable when source()d
prov_run_tag <- 'standard run' ### set up a default at the start; main script can change it
prov_start_time <- proc.time() ### initialize process timing

git_prov <- function(git_file, type = c('input', 'output', 'script', 'sourced_script')[1]) {
### This function determines the most recent commit for a given file.
### The idea is to help promote provenance by identifying a particular
### version of a given file.

  ### attempt to read git_info for script or input
  suppressWarnings({
    git_info <- system2('git', args = sprintf('log --follow %s', git_file), stderr = FALSE, stdout = TRUE)[1:3]
    #    git_diff <- system2('git', args = 'diff HEAD', stderr = TRUE, stdout = TRUE)
  })
  ### if git_info[1] is NA, commit info not found.
  if(is.na(git_info[1])) {
    message(sprintf('File `%s`: git commit info unavailable.  Not version-controlled in Git?', git_file))
    git_commit_url  <- 'no version control info found'
    git_uncommitted <- NA
  } else {
    ### git_info[1] is not NA, so commit info is available.
    ### find whether uncommitted differences in this file.
    ### in str_detect, '$' makes sure git_file string is at end of line.
    suppressWarnings({
      git_diff <- system2('git', args = 'diff HEAD', stderr = TRUE, stdout = TRUE)
    })
    git_diff_check <- which(str_detect(git_diff, sprintf('%s$', basename(git_file))) &
                              str_detect(git_diff, 'diff --git'))
    git_uncommitted <- length(git_diff_check) > 0

    ### convert commit info to a hyperlinked commit info string.
    git_loc  <- system2('git', args = 'config --get remote.origin.url', stderr = TRUE, stdout = TRUE)
    if(type == 'output')
      git_commit_url <- sprintf('Previous commit: %s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), gsub('commit ', '', git_info[1]))
    else
      git_commit_url <- sprintf('%s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), gsub('commit ', '', git_info[1]))
    git_link <- sprintf('commit [%s](%s)', gsub('commit ', '', git_info[1]), git_commit_url)
    message(sprintf('File `%s`: most recent commit info: %s; uncommitted changes = %s', git_file,
                    paste(git_info[1], git_info[2], git_info[3], collapse = '; '), git_uncommitted))
  }

  git_df <- data.frame('file_loc'      = as.character(git_file),
                       'type'          = tolower(as.character(type)),
                       'commit_url'    = as.character(git_commit_url),
                       'commit_author' = as.character(sub('Author: ', '', git_info[2])),
                       'commit_date'   = as.character(sub('Date: ', '', git_info[3])),
                       'uncommitted_changes' = as.logical(git_uncommitted))

  ### Binds git_df to the global prov_track variable, and reassigns it to the higher environment
  prov_track <<- prov_track %>%
    rbind(git_df)

  return(invisible(git_df))
}

script_prov <- function(script_file, tag = prov_run_tag, commit_outputs = TRUE) {
  # script_file <- this_script_file; tag = prov_run_tag
  if(commit_outputs) {
    commit_prov(script_file, tag)
  }

  sys <- Sys.info()
  ses <- sessionInfo()

  msg_sys <- sprintf('System: %s, Release: %s. Machine: %s. User: %s.', sys['sysname'], sys['release'], sys['machine'], sys['user'])
  msg_ses <- sprintf('R version: %s, Platform: %s, Running under: %s.',
                     ses$R.version$version.string, ses$R.version$platform, ses$running)
  msg_base_pkgs <- sprintf('Attached base packages: %s', paste(ses$basePkgs,
                                                               collapse = ', '))
  msg_att_pkgs <- sprintf('Other attached packages: %s', paste(sapply(ses$otherPkgs,
                                                                      function(x) paste(x$Package, x$Version, sep = '_')),
                                                               collapse = ', '))
  ### Gather git info using system calls.  Convert commit # and remote origin url into a url for that commit.
  msg_git <- git_prov(script_file, type = 'script')
  run_time  = (proc.time() - prov_start_time)[3]
  run_mem <- NA

  script_track <<- prov_track %>%
    mutate(elapsed_time  = run_time,
           memory_use    = run_mem,
           sys_info      = msg_sys,
           ses_info      = msg_ses,
           base_pkgs     = msg_base_pkgs,
           attached_pkgs = msg_att_pkgs,
           rdf_subject   = file_loc,
           rdf_object    = script_file,
           rdf_predicate = ifelse(str_detect(type, 'out'), 'was generated by',
                             ifelse(str_detect(type, 'in'), 'was used as input by',
                               ifelse(str_detect(type, 'source'), 'was sourced by',
                                 ifelse(path.expand(as.character(file_loc)) == path.expand(rdf_object),
                                   ifelse(uncommitted_changes, 'is modified from', 'is'),
                                   'has an undefined relation to'))))
           )

  if(!exists('dir_prov')) {
    warning('No provenance directory assigned - this run will not be logged.\n')
    run_id <- 'NOT LOGGED'
  } else {
    if(!dir.exists(dir_prov)) dir.create(dir_prov)
    prov_log_file <- path.expand(file.path(dir_prov, sprintf('%s.csv', basename(script_file))))
      ### takes full script file (including extension) and adds .csv extension
    if(!file.exists(prov_log_file)) {
      warning(sprintf('No log file found at %s - initializing new log file.\n', prov_log_file))
        ### no log found, so initialize log with run_id = 1 for all inputs and script.
      script_track <<- data.frame('run_id'   = rep(1,      length.out = nrow(script_track)),
                                  'run_tag'  = tag,
                                  'run_date' = rep(date(), length.out = nrow(script_track)),
                                                 script_track)
      run_id <- 1
      log_df <- script_track
    } else {
      log_df <- read.csv(prov_log_file, stringsAsFactors = FALSE)
      run_id_old <- max(log_df$run_id)
      run_id <- run_id_old + 1
      message(sprintf('Log file found at %s; last run_id = %s. Appending latest run.\n', prov_log_file, run_id_old))
      script_track <<- data.frame('run_id'   = rep(run_id, length.out = nrow(script_track)),
                                  'run_tag'  = tag,
                                  'run_date' = rep(date(), length.out = nrow(script_track)),
                                                     script_track)
      log_df <- log_df %>%
        bind_rows(script_track)
    }
    message(sprintf('Writing updated log file to %s.\n', prov_log_file))
    write.csv(log_df, prov_log_file, row.names = FALSE)
  }

  ### Return all message strings within a named list for convenient reference.
  return(invisible(list('run_id' = run_id, 'run_tag' = tag, 'elapsed_time' = run_time, 'memory_use' = run_mem,
                        'msg_sys' = msg_sys, 'msg_ses' = msg_ses,
                        'msg_git' = msg_git, 'msg_base_pkgs' = msg_base_pkgs, 'msg_att_pkgs' = msg_att_pkgs)))
}

commit_prov <- function(script_file, tag) {
  ### from prov_track, identify all output files with uncommitted changes;
  ### commit them and add new commit info to prov_track

  ### Stage all new files in repository
  prov_track <<- prov_track %>%
    mutate(uncommitted_changes = as.logical(uncommitted_changes),
           uncommitted_changes = ifelse(is.na(uncommitted_changes), TRUE, uncommitted_changes))
  prov_staged <- prov_track %>%
    filter(str_detect(type, 'out') & uncommitted_changes == TRUE)
  if (nrow(prov_staged) > 0) {
    for (i in 1:nrow(prov_staged)) {
      git_add <- system2('git', args = sprintf('add %s', prov_staged$file_loc[i]), stderr = TRUE, stdout = TRUE)
      prov_staged$git_staged[i] <- (length(git_add) == 0)
    }
    prov_track <<- prov_track %>%
      left_join(prov_staged %>%
                  dplyr::select(file_loc, git_staged) %>%
                  unique(),
                by = 'file_loc')
  } else {
    prov_track <<- prov_track %>%
      mutate(git_staged = NA)
  }

  git_commit <- system2('git', args = sprintf('commit -m "Running script %s; %s"', script_file, tag), stderr = TRUE, stdout = TRUE)
  message(sprintf('%s', paste(git_commit, collapse = '\n')))

  git_id <- system2('git', args = 'rev-parse HEAD', stderr = TRUE, stdout = TRUE)

  git_loc  <- system2('git', args = 'config --get remote.origin.url', stderr = TRUE, stdout = TRUE)
  git_commit_url <- sprintf('%s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), git_id)

  suppressWarnings({
    git_info <- system2('git', args = sprintf('show %s', git_id), stderr = FALSE, stdout = TRUE)[1:3]
  })

  prov_track <<- prov_track %>%
    mutate(git_staged    = ifelse(is.na(git_staged), FALSE, git_staged),
           commit_url    = as.character(commit_url),
           commit_author = as.character(commit_author),
           commit_date   = as.character(commit_date),
           commit_url    = ifelse(git_staged == TRUE, sprintf('New commit: %s', git_commit_url), commit_url),
           commit_author = ifelse(git_staged == TRUE, as.character(sub('Author: ', '', git_info[2])), commit_author),
           commit_date   = ifelse(git_staged == TRUE, as.character(sub('Date: ', '', git_info[3])), commit_date),
           uncommitted_changes = ifelse(git_staged == TRUE, FALSE, uncommitted_changes)) %>%
    dplyr::select(-git_staged)
}
