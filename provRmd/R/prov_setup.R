#' Set up git provenance tracking process
#'
#' This function sets up a number of global variables used to track provenance
#' information during the run of the script.  If used within an R Markdown
#' script, uses the knitted file name and directory.  Sets up NULL variables that
#' will contain prov_track and script_track info, timing info, and other info
#' used in the final provenance output dataframe.
#' @param run_tag A character string that briefly describes this particular run. Default = 'standard run'.
#' @export
#' @examples
#' prov_setup()
#' prov_setup(run_tag = 'testing with new data set')

prov_setup <- function (run_tag = 'standard run') {

  require('dplyr'); require('tidyr'); require('stringr'); require('readr'); require('knitr')

  ### set up current directory and file for knitted script.
  ### If not being knitted (e.g. run chunk at a time) the knitr::: call returns
  ### character(0) so set to a valid temp string.

  prov_script_dir <- file.path(getwd(), knitr:::.knitEnv$input.dir) %>%
    str_sub(1, -3) %>%                            ### ditch annoying '/.' at the end
    str_replace(path.expand('~'), '~')            ### ditch specific home for generic home

  if(length(prov_script_dir) == 0) {
    prov_script_dir  <- getwd()  ### default for non-knitted operations
  }
  assign('prov_script_dir', prov_script_dir, envir = .GlobalEnv)

  prov_parent_script_file <- file.path(prov_script_dir, knitr:::knit_concord$get("infile"))
  if(length(prov_parent_script_file) == 0) {
    prov_parent_script_file  <- 'Rmd_not_knitted'
  }
  assign('prov_parent_script_file', prov_parent_script_file, envir = .GlobalEnv)

  ### set the prov_parent_id variable to the parent script; this will be
  ### temporarily modified during a 'source' call so files operated on
  ### by sourced script will get a new parent.
  assign('prov_parent_id', prov_parent_script_file, envir = .GlobalEnv)

  ### set directory for provenance log .csv (for script_prov()):
  assign('prov_log_dir', file.path(prov_script_dir, 'prov'), envir = .GlobalEnv)

  ### initialize the prov_track global variable
  assign('prov_track', NULL, envir = .GlobalEnv)

  ### initialize the script_track global variable
  assign('script_track', NULL, envir = .GlobalEnv)

  ### initialize prov_run_tag global variable based on input argument
  assign('prov_run_tag', run_tag, envir = .GlobalEnv)

  ### initialize process timing
  assign('prov_start_time', proc.time(), envir = .GlobalEnv)

  options(stringsAsFactors = FALSE) ### because factors are annoying
}

