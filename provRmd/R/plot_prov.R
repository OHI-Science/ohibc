#' Access git provenance information
#'
#' This function takes a filename and reads its git log, strips to its
#' most recent commit, adds a line to prov_trackfor this file, and returns
#' a dataframe with git provenance information.
#' @param df A dataframe of a provenance log file (from script_prov())
#' @param filetype The role of this file within this context: 'input', 'output', 'parent_script', or 'sourced_script'.  Defaults to 'input'.
#' @param nolog Should this git provenance information be omitted from the log file? Defaults to FALSE.
#' @export
#' @examples
#' plot_prov()

plot_prov <- function(df, plot_dir = c('TB', 'LR')[1]) {

  require('dplyr'); require('tidyr'); require('stringr'); require('readr'); require('knitr')

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
