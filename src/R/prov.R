### prov.R
###
### This script sets up provenance tracking for a script.
### When sourced, initializes a global variable prov_track to be NULL.
###
### Two functions (so far):
### git_prov(git_file, filetype = 'input') takes filename and reads its
###   git log, strips to its most recent commit, adds a line to prov_track
###   for this file, and returns a git provenance dataframe including:
###   * parent (the script that operates on the file)
###   * file_loc
###   * filetype
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

## print knitr options

# [r - Need the filename of the Rnw when knitr runs in rStudio - Stack Overflow](http://stackoverflow.com/questions/20957129/need-the-filename-of-the-rnw-when-knitr-runs-in-rstudio)
# [Objects: Objects to manipulate options, patterns and hooks | knitr](http://yihui.name/knitr/objects/)

### set up current directory and file for knitted script.
### If not being knitted (e.g. run chunk at a time) the knitr::: call returns
### character(0) so set to a valid temp string.
prov_script_dir <- file.path(getwd(), knitr:::.knitEnv$input.dir) %>%
  str_sub(1, -3) %>%                            ### ditch annoying '/.' at the end
  str_replace(path.expand('~'), '~')            ### ditch specific home for generic home

if(length(prov_script_dir) == 0) {
  prov_script_dir  <- getwd()  ### default for non-knitted operations
}

prov_parent_script_file <- file.path(prov_script_dir, knitr:::knit_concord$get("infile"))
if(length(prov_parent_script_file) == 0) {
  prov_parent_script_file  <- 'Rmd_not_knitted'
}

### set the prov_parent_id variable to the parent script; this will be
### temporarily modified during a 'source' call so files operated on
### by sourced script will get a new parent.
prov_parent_id <- prov_parent_script_file

### set directory for provenance log .csv (for script_prov()):
prov_log_dir <- file.path(prov_script_dir, 'prov')

prov_track   <- NULL ### initialize the prov_track global variable when source()d
script_track <- NULL ### initialize the script_track global variable when source()d
if(!exists('prov_run_tag'))
  prov_run_tag  <- 'standard run' ### set up a default at the start; main script can change it before or after the source call.
prov_start_time <- proc.time() ### initialize process timing

options(stringsAsFactors = FALSE)



###############################=
git_prov <- function(git_file, filetype = c('input', 'output', 'parent_script', 'sourced_script')[1], nolog = FALSE) {
### This function determines the most recent commit for a given file.

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
    if(filetype == 'output')
      git_commit_url <- sprintf('Previous commit: %s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), gsub('commit ', '', git_info[1]))
    else
      git_commit_url <- sprintf('%s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), gsub('commit ', '', git_info[1]))
    git_link <- sprintf('commit [%s](%s)', gsub('commit ', '', git_info[1]), git_commit_url)
    message(sprintf('File `%s`: most recent commit info: %s; uncommitted changes = %s', git_file,
                    paste(git_info[1], git_info[2], git_info[3], collapse = '; '), git_uncommitted))
  }

  git_file <- git_file %>%
    str_replace(dir_M, 'Mazu:') # %>%
    # str_replace(dir_N, 'Neptune:')

  git_df <- data.frame('parent_fn'     = prov_parent_id,
                       'file_loc'      = git_file,
                       'filetype'      = tolower(filetype),
                       'commit_url'    = git_commit_url,
                       'commit_author' = sub('Author: ', '', git_info[2]),
                       'commit_date'   = sub('Date: ', '', git_info[3]),
                       'uncommitted_changes' = as.logical(git_uncommitted))

  ### Binds git_df to the global prov_track variable, and reassigns it to the higher environment.
  ### nolog argument to git_prov allows to check git info without logging it (for peek_csv() below)
  if(!nolog) {
    prov_track <<- prov_track %>%
      rbind(git_df)
  }

  return(invisible(git_df))
}

script_prov <- function(script_file, tag = prov_run_tag, commit_outputs = TRUE) {
  # script_file <- prov_parent_script_file; tag = prov_run_tag
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
  script_track <<- prov_track %>%
    mutate(elapsed_time  = run_time,
           memory_use    = run_mem,
           sys_info      = msg_sys,
           ses_info      = msg_ses,
           base_pkgs     = msg_base_pkgs,
           attached_pkgs = msg_att_pkgs,
           rdf_subject   = ifelse(filetype %in% backwards_predicates, parent_fn, file_loc),
           rdf_object    = ifelse(filetype %in% backwards_predicates, file_loc, parent_fn))

  script_track <<- script_track %>%
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
                                  rdf_predicate))

  run_hash = digest::sha1(script_track)
  script_track <<- script_track %>%
    mutate(run_hash = run_hash)


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
      script_track <<- data.frame('run_id'   = rep(1,      length.out = nrow(script_track)),
                                  'run_tag'  = tag,
                                  'run_date' = rep(date(), length.out = nrow(script_track)),
                                  script_track,
                                  stringsAsFactors = FALSE)
      run_id <- 1
      log_df <- script_track
    } else {
      log_df <- read_csv(prov_log_file)
      run_id_old <- max(log_df$run_id)
      run_id <- run_id_old + 1
      message(sprintf('Log file found at %s; last run_id = %s. Appending latest run.\n', prov_log_file, run_id_old))
      script_track <<- data.frame('run_id'   = rep(run_id, length.out = nrow(script_track)),
                                  'run_tag'  = tag,
                                  'run_date' = rep(date(), length.out = nrow(script_track)),
                                  script_track,
                                  stringsAsFactors = FALSE)
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

commit_prov <- function(script_file, tag) {
  ### from prov_track, identify all output files with uncommitted changes;
  ### commit them and add new commit info to prov_track

  ### Stage all new files in repository
  prov_track <<- prov_track %>%
    mutate(uncommitted_changes = as.logical(uncommitted_changes),
           uncommitted_changes = ifelse(is.na(uncommitted_changes), TRUE, uncommitted_changes))
  prov_staged <- prov_track %>%
    filter(str_detect(filetype, 'out') & uncommitted_changes == TRUE)
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
           commit_url    = commit_url,
           commit_author = commit_author,
           commit_date   = commit_date)
  prov_track <<- prov_track %>%
    mutate(commit_url    = ifelse(git_staged == TRUE, sprintf('New commit: %s', git_commit_url), commit_url),
           commit_author = ifelse(git_staged == TRUE, sub('Author: ', '', git_info[2]), commit_author),
           commit_date   = ifelse(git_staged == TRUE, sub('Date: ', '', git_info[3]), commit_date),
           uncommitted_changes = ifelse(git_staged == TRUE, FALSE, uncommitted_changes)) %>%
    dplyr::select(-git_staged)
}

plot_prov <- function(df, plot_dir = c('TB', 'LR')[1]) {
  # devtools::install_github("rich-iannone/DiagrammeR")
  # devtools::install_github("rich-iannone/DiagrammeRsvg")
  message('For now, gotta use the install_github version of DiagrammeR and DiagrammeRsvg')
  library(DiagrammeR, quietly = TRUE)
  library(DiagrammeRsvg, quietly = TRUE)

  df <- df %>%
    filter(run_id == max(run_id)) %>%
    mutate(from = rdf_subject,
           to   = rdf_object,
           rel  = rdf_predicate)
  ### NOTE: from subject to object is active; this will ensure that the
  ### sequence goes down the page.  But predicates are usually
  ### passive voice, so e.g. subject WASGENERATEDBY.  So: edges
  ### should have direction set to reverse?

  shapes_df <- data.frame(
    filetype  = c('input',         'output',        'parent_script', 'sourced_script'),
    shape     = c('oval',          'oval',          'rectangle',     'rectangle'),
    color     = c( hsv(.6, .5, .7), hsv(.3, .5, .7), hsv(.1, .5, .7), hsv(.1, .5, .7)),
    fillcolor = c( hsv(.6, .3, .9), hsv(.3, .4, .9), hsv(.1, .4, .9), hsv(.15, .2, 1)))
    # fontcolor, fontname
  nodes_df <- df %>%
    dplyr::select(file_loc, filetype, commit_url, uncommitted_changes) %>%
    mutate(nodes   = file_loc,
           label   = basename(file_loc),
           tooltip = commit_url,
           style   = 'filled',
           fontsize  = 6,
           fontcolor = 'grey20',
           fontname  = 'Helvetica',
           penwidth  = 2) %>%
    left_join(shapes_df, by = 'filetype') %>%
    # sides, distortion for different shapes!
    # style to differentiate script vs sourced? or alpha to differentiate source ins/outs?
    unique()

  ### special cases: no git tracking, or uncommitted changes
  nodes_df <- nodes_df %>%
    mutate(color    = ifelse(uncommitted_changes == TRUE, 'yellow', color),
           penwidth = ifelse(uncommitted_changes == TRUE, 3,        penwidth),
           color    = ifelse(str_detect(commit_url, 'no version control'), 'red', color),
           penwidth = ifelse(str_detect(commit_url, 'no version control'), 3,     penwidth))

  arrows_df <- data.frame(
    rel   = c('prov:used', 'prov:wasGeneratedBy', 'prov:wasExecutedBy'),
    color = c( hsv(.6, .5, .4),    hsv(.3, .5, .4),       hsv(.1, .5, .4)))

  edges_df <- df %>%
    dplyr::select(from, to, rel) %>%
    filter(from != to) %>%
    mutate(label = str_replace(rel, 'prov:', ''),
           #dir       = 'back',
           tooltip   = rel,
           fontsize  = 6,
           fontcolor = 'grey20',
           fontname  = 'Helvetica',
           penwidth  = 1,
           #arrowhead = 'diamond', # if dir is 'back', use arrowtail
           #arrowtail = 'box',
           arrowsize = .5) %>%
    left_join(arrows_df, by = 'rel') %>%
    unique()

  prov_gr <- create_graph(nodes_df = nodes_df,
                          edges_df = edges_df,
                          graph_attrs  = sprintf('rankdir = %s', plot_dir),
                          # node_attrs   = NULL,
                          # edge_attrs   = NULL,
                          # directed     = TRUE,
                          # graph_name   = NULL,
                          # graph_time   = NULL,
                          # graph_tz     = NULL,
                          generate_dot = TRUE)

  # render_graph(prov_gr) %>%
  #   print()

  return(invisible(prov_gr))

}


### organize better
### color code re: committed/unchanged = green, committed/changed = yellow, uncommitted = red?
### link to file on github or something?
### proper RDF predicates, shapes, etc
### info on plot: run tag, date, name; sys.info, session.info
### use md5 tags for unique run IDs (vs human readable)

### Redefine common read and write functions to include a call to git_prov.
### 'nogit = TRUE' to override call to git_prov()

### functions from base:
source <- function(source_fn, ..., nogit = FALSE) {
  ### prov_parent_id will change within this script to point to the sourced file.
  ### didn't seem to work with local change - so setting it globally

  ### save the current prov_parent_id value temporarily.
  prov_parent_id_temp <- prov_parent_id

  ### reset the prov_parent_id value to the sourced file
  prov_parent_id <<- source_fn

  base::source(file = source_fn, ...)

  ### reset prov_parent_id back to original value
  prov_parent_id <<- prov_parent_id_temp
  if(!nogit) git_prov(source_fn, filetype = 'sourced_script')
}

read.csv <- function(file, stringsAsFactors = FALSE, nogit = FALSE, ...) {
  x <- utils::read.csv(file, ..., stringsAsFactors = stringsAsFactors)
  if(!nogit) git_prov(file, filetype = 'input')
  return(x)
}

write.csv <- function(x, file, row.names = FALSE, nogit = FALSE, ...) {
  utils::write.csv(x, file = file, ..., row.names = row.names)
  if(!nogit) git_prov(file, filetype = 'output')
}

### functions from readr:
read_csv <- function(file, nogit = FALSE, ...) {
  x <- readr::read_csv(file, ...)
  if(!nogit) git_prov(file, filetype = 'input')
  return(x)
}

write_csv <- function(x, path, nogit = FALSE, ...) {
  readr::write_csv(x, path = path, ...)
  if(!nogit) git_prov(path, filetype = 'output')
}

### functions to read/write shapefiles:
readOGR <- function(dsn, layer, stringsAsFactors = FALSE, nogit = FALSE, ...) {
  x <- rgdal::readOGR(dsn = dsn, layer = layer, ..., stringsAsFactors = stringsAsFactors)
  if(!nogit) git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'input')
  return(x)
}

writeOGR <- function(obj, dsn, layer, driver = 'ESRI Shapefile', nogit = FALSE, ...) {
  rgdal::writeOGR(obj, dsn = dsn, layer = layer, ..., driver = driver)
  if(!nogit) git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'output')
}

readShapePoly <- function(fn, nogit = FALSE, ...) {
  x <- maptools::readShapePoly(fn, ...)
  if(!nogit) git_prov(paste(fn, '.shp', sep = ''), filetype = 'input')
  return(x)
}

writePolyShape <- function(x, fn, nogit = FALSE, ...) {
  maptools::writePolyShape(x, fn, ...)
  if(!nogit) git_prov(paste(fn, '.shp', sep = ''), filetype = 'output')
}

### functions to read/write rasters:
raster <- function(x, nogit = FALSE, ...) {
  if(is.character(x) & !nogit) {
    y <- raster::raster(x, ...)
    git_prov(x, filetype = 'input')
    return(y)
  } else {
    raster::raster(x, ...)
  }
}

brick <- function(x, nogit = FALSE, ...) {
  if(is.character(x) & !nogit) {
    y <- raster::brick(x, ...)
    git_prov(x, filetype = 'input')
    return(y)
  } else {
    raster::brick(x, ...)
  }
}

writeRaster <- function(x, filename, bylayer = FALSE, nogit = FALSE, ...) {
  raster::writeRaster(x, filename, ..., bylayer = bylayer)
  if(bylayer == TRUE & !nogit) {
    message('please run git_prov() manually on individual output layers')
  } else {
    if(!nogit) git_prov(filename, filetype = 'output')
  }
}

gdal_rasterize <- function(..., nogit = FALSE) {
  message("Don't forget to run git_prov() on the inputs and outputs...")
  gdalUtils::gdal_rasterize(...)
}

rasterize <- function(x, y, filename = '', nogit = FALSE, ...) {
  z <- raster::rasterize(x, y, ..., filename = filename)
  if(filename != '' & !nogit) {
    git_prov(filename, filetype = 'output')
  }
  return(z)
}

