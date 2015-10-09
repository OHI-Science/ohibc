# ico_fxn.R
# created Jun2015 by Casey O'Hara
# functions to support calculations of the Iconic Species subgoal.  Some 
# functions are also used from the spp_fxn.R script, so that must be loaded as well.

message('NOTE: spp_fxn.R requires that the following variables be set in the global environment (main script):')
message(sprintf('dir_anx:  currently set to \'%s\'', dir_anx))
message(sprintf('dir_anx_global: currently set to \'%s\'', dir_anx_global))
message(sprintf('scenario:       currently set to \'%s\'\n', scenario))



#############################################################################=
process_ico_rgn <- function(ico_rgn_list) {
  ### Summarize category and trend for each region.
  
  # to overall lookup table, join scores for population category and trend.
  popn_cat    <- data.frame(category  = c('LC', 'NT', 'VU', 'EN', 'CR', 'EX'), 
                            category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
  popn_trend  <- data.frame(trend=c('decreasing', 'stable', 'increasing'), 
                            trend_score=c(-0.5, 0, 0.5))
  ico_rgn_list <- ico_rgn_list %>%
    left_join(popn_cat,   by = 'category') %>%
    left_join(popn_trend, by = 'trend') %>%
    select(-category, -trend)
  
  ### This section aggregates category and trend for a single species sciname within a region,
  ### including parent and all subpopulations present in a region.
  ### Species, including parent and all subpops, is weighted same as species w/o parents and subpops.
  ico_rgn_list <- ico_rgn_list %>%
    group_by(rgn_id, sciname) %>%
    summarize(category_score = mean(category_score), trend_score = mean(trend_score, na.rm = TRUE))

  ico_rgn_sum <- ico_rgn_list %>%
    group_by(rgn_id) %>%
    summarize(mean_cat = mean(category_score), mean_trend = mean(trend_score, na.rm = TRUE))
  
  ico_rgn_sum_file <- file.path(dir_git, scenario, 'summary/ico_rgn_sum.csv')
  cat(sprintf('Writing file for iconic species summary by region: \n  %s\n', ico_rgn_sum_file))
  write_csv(ico_rgn_sum, ico_rgn_sum_file)
  
  return(invisible(ico_rgn_sum))
}

