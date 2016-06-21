#' Assemble script provenance information
#'
#' This function gathers the information from all provenance tracking within
#' the script and adds system and session info, as well as RDF predicate info.
#' @param script_file This should be the name of the parent script.
#' @param tag An optional run tag; defaults to the run tag set in prov_setup().
#' @param commit_outputs Should a commit be created for the output files? Defaults to TRUE.
#' @export
#' @examples
#' script_prov()

script_prov <- function(script_file, tag = prov_run_tag, commit_outputs = TRUE) {
  # script_file <- prov_parent_script_file; tag = prov_run_tag
  require('dplyr'); require('tidyr'); require('stringr'); require('readr'); require('knitr')

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
  msg_git  <- git_prov(script_file, filetype = 'parent_script')
  run_time <- (proc.time() - prov_start_time)[3]
  run_mem  <- NA

  backwards_predicates <- c('output', 'sourced_script') ### for those annoying prov predicates that flip the subject/object
  assign('script_track', prov_track %>%
           mutate(elapsed_time  = run_time,
                  memory_use    = run_mem,
                  sys_info      = msg_sys,
                  ses_info      = msg_ses,
                  base_pkgs     = msg_base_pkgs,
                  attached_pkgs = msg_att_pkgs,
                  rdf_subject   = ifelse(filetype %in% backwards_predicates, parent_fn, file_loc),
                  rdf_object    = ifelse(filetype %in% backwards_predicates, file_loc, parent_fn)),
         envir = .GlobalEnv)

  assign('script_track', script_track %>%
           mutate(rdf_predicate = 'UNDEFINED', ### initialize value to default
           rdf_predicate = ifelse(str_detect(filetype, 'out'),
                                  'prov:wasGeneratedBy',
                                  rdf_predicate),
           rdf_predicate = ifelse(str_detect(filetype, 'in'),
                                  'prov:used',
                                  rdf_predicate),
           rdf_predicate = ifelse(str_detect(filetype, 'source'),
                                  'prov:wasExecutedBy',
                                  rdf_predicate),
           rdf_predicate = ifelse(path.expand(rdf_subject) == path.expand(rdf_object),
                                  ifelse(uncommitted_changes,
                                         'prov:wasDerivedFrom',
                                         'prov:(isPrettyMuchIdenticalTo)'),
                                  rdf_predicate)),
         envir = .GlobalEnv)


  run_hash <- digest::sha1(script_track)
  assign('script_track', script_track %>%
           mutate(run_hash = run_hash),
         envir = .GlobalEnv)


  if(!exists('prov_log_dir')) {
    warning('No provenance directory assigned - this run will not be logged.\n')
    run_id <- 'NOT LOGGED'
  } else {
    if(!dir.exists(prov_log_dir)) dir.create(prov_log_dir)
    prov_log_file <- file.path(prov_log_dir, sprintf('%s.csv', basename(script_file)))
      ### takes full script file (including extension) and adds .csv extension
    if(!file.exists(prov_log_file)) {
      warning(sprintf('No log file found at %s - initializing new log file.\n', prov_log_file))
        ### no log found, so initialize log with run_id = 1 for all inputs and script.
      assign('script_track', data.frame('run_id'   = rep(1,      length.out = nrow(script_track)),
                                  'run_tag'  = tag,
                                  'run_date' = rep(date(), length.out = nrow(script_track)),
                                  script_track,
                                  stringsAsFactors = FALSE),
             envir = .GlobalEnv)
      run_id <- 1
      log_df <- script_track
    } else {
      log_df <- read_csv(prov_log_file)
      run_id_old <- max(log_df$run_id)
      run_id <- run_id_old + 1
      message(sprintf('Log file found at %s; last run_id = %s. Appending latest run.\n', prov_log_file, run_id_old))
      assign('script_track', data.frame('run_id'   = rep(run_id, length.out = nrow(script_track)),
                                  'run_tag'  = tag,
                                  'run_date' = rep(date(), length.out = nrow(script_track)),
                                  script_track,
                                  stringsAsFactors = FALSE),
             envir = .GlobalEnv)
      log_df <- log_df %>%
        bind_rows(script_track)
    }
    message(sprintf('Writing updated log file to %s.\n', prov_log_file))
    write_csv(log_df, prov_log_file)
  }

  ### Return all message strings within a named list for convenient reference.
  return(invisible(list('run_id'        = run_id,
                        'run_hash'      = run_hash,
                        'run_tag'       = tag,
                        'elapsed_time'  = run_time,
                        'memory_use'    = run_mem,
                        'msg_sys'       = msg_sys,
                        'msg_ses'       = msg_ses,
                        'msg_git'       = msg_git,
                        'msg_base_pkgs' = msg_base_pkgs,
                        'msg_att_pkgs'  = msg_att_pkgs)))
}
