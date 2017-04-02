Setup <- function(){
  ref_pt_file <- file.path(layers$data$dir_calc, 'reference_pts.csv')
  unlink(ref_pt_file)

  ref_pts <- data.frame(year   = as.integer(),
                        goal   = as.character(),
                        method = as.character(),
                        reference_point = as.character())
  write_csv(ref_pts, ref_pt_file)
}

write_ref_pts <- function(goal, method, ref_pt) {

  ref_pt_file <- file.path(layers$data$dir_calc, 'reference_pts.csv')

  if(!file.exists(ref_pt_file)) {
    warning('Reference point file does not exist! \n  ', ref_pt_file)
    return()
  }
  ref_pts <- read_csv(ref_pt_file)  %>%
    rbind(data.frame(year   = layers$data$status_year,
                     goal   = goal,
                     method = method,
                     reference_point = ref_pt))
  write_csv(ref_pts, ref_pt_file)

}

ICO <- function(layers, status_year) {

  ico_layer <- SelectLayersData(layers, layers=c('cat_label', 'ico_spp_risk_score')) %>%
    select(year, region_id = id_num, category, category_score = val_num, sciname, layer)

  if(is.null(status_year)) {
    ### in goals.csv, allow status year to be NULL, so it can be reassigned
    ### for each iteration through calculate loop
    status_year <- layers$data$status_year
  }
  # lookup for weights status
  #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  #  T  <- "THREATENED (T)" treat as "EN"
  #  VU <- "VULNERABLE (V)"
  #  EN <- "ENDANGERED (E)"
  #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  #  DD <- "INSUFFICIENTLY KNOWN (K)"
  #  DD <- "INDETERMINATE (I)"
  #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"

  ### calculate base status across all years
  # STEP 1: take mean of subpopulation scores if necessary
  rgn_ico_status_spp <- ico_layer %>%
    group_by(region_id, sciname, year) %>%
    summarize(spp_mean = mean(category_score, na.rm = TRUE)) %>%
    ungroup()

  # STEP 2: take mean of populations within regions
  rgn_ico_status <- rgn_ico_status_spp %>%
    group_by(region_id, year) %>%
    summarize(score = mean(spp_mean, na.rm = TRUE)) %>%
    ungroup()

  ### calculate trend for status_year
  trend_span       <- 10 ### trend based on 10 years of data, due to infrequency of IUCN assessments
  trend_yr_include <- c(status_year:(status_year - trend_span + 1))

  if(!all(trend_yr_include %in% rgn_ico_status$year)) {
    message('Calculating trend for ', status_year, ': status not available for ',
            paste(trend_yr_include[!trend_yr_include %in% rgn_ico_status$year], collapse = ', '))
  }

  rgn_ico_trend <- rgn_ico_status %>%
    filter(year %in% trend_yr_include) %>%
    group_by(region_id) %>%
    do(mdl = lm(score ~ year, data = . )) %>%
    summarize(region_id = region_id,
              score = coef(mdl)['year'] * 5) %>%
    ungroup() %>%
    mutate(score     = round(score, 5),
           year      = status_year,
           dimension = "trend")

  ### format status for reporting
  rgn_ico_status <- rgn_ico_status %>%
    mutate(score = round(score * 100, 5)) %>%
    mutate(dimension = "status") %>%
    select(year, region_id, score, dimension)

  ### write reference points
  write_ref_pts(goal   = "ICO",
                method = "scaled IUCN risk categories",
                ref_pt = NA)

  # return scores
  scores <- bind_rows(rgn_ico_status, rgn_ico_trend) %>%
    filter(year == status_year) %>%
    mutate('goal'='ICO') %>%
    select(year, goal, dimension, region_id, score) %>%
    data.frame()

  return(scores)

}

LSP <- function(layers, ref_pct_cmpa = 30, ref_pct_cp = 30, status_year){


  # select data ----
  tot_area_rgn  <- SelectLayersData(layers, layers = c('lsp_tot_area_inland_ws', 'lsp_tot_area_offshore3nm'))
  prot_area_rgn <- SelectLayersData(layers, layers = c('lsp_prot_area_offshore3nm', 'lsp_prot_area_inland_ws'))

  tot_area_rgn <- tot_area_rgn %>%
    select(region_id = id_num, val_num, layer) %>%
    distinct() %>%
    spread(layer, val_num) %>%
    select(region_id,
           area_inland   = lsp_tot_area_inland_ws,
           area_offshore = lsp_tot_area_offshore3nm)

  prot_area_rgn <- prot_area_rgn %>%
    select(region_id = id_num, year, val_num, layer) %>%
    spread(layer, val_num) %>%
    select(region_id, year,
           cmpa = lsp_prot_area_offshore3nm,
           cp   = lsp_prot_area_inland_ws)

  ### get percent of total area that is protected for inland (cp) and
  ### offshore (cmpa) per year, and calculate status score
  rgn_yrs <- prot_area_rgn %>%
    full_join(tot_area_rgn, by = "region_id") %>%
    mutate(pct_cp    = pmin(cp   / area_inland   * 100, 100),
           pct_cmpa  = pmin(cmpa / area_offshore * 100, 100),
           status    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
    filter(!is.na(status))

  ### extract status based on specified year
  if(is.null(status_year)) {
    status_year <- layers$data$status_year
  }

  rgn_status <- rgn_yrs %>%
    filter(year == status_year) %>%
    select(year, region_id, score = status) %>%
    mutate(score = round(score * 100, 5),
           goal  = 'LSP',
           dimension = 'status')

  ### calculate trend
  trend_years <- (status_year-4):status_year

  rgn_trend <-   rgn_yrs %>%
    filter(year %in% trend_years) %>%
    group_by(region_id) %>%
    do(mdl = lm(status ~ year, data = . )) %>%
    summarize(
      region_id = region_id,
      score = min(1, max(0, 5 * coef(mdl)['year']))) %>% # set boundaries so trend does not go below 0 or above 1
    ungroup() %>%
    mutate(year  = status_year,
           goal  = 'LSP',
           dimension = 'trend',
           score = round(score, 5))

  ### reference points
  write_ref_pts(goal   = "LSP",
                method = paste0(ref_pct_cmpa, "% marine protected area; ",
                                ref_pct_cp, "% coastal protected area"),
                ref_pt = "varies by area of region's eez and 1 km inland")

  ### return scores
  scores <- bind_rows(rgn_status, rgn_trend) %>%
    select(year, region_id, goal, dimension, score)

  return(scores)

}

SP <- function(scores, status_year) {

  ### to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    filter(goal %in% c('ICO','LSP') &
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    select(region_id, goal, dimension, score)

  ### return all scores
  scores <- bind_rows(scores, s)

  return(scores)
}

FinalizeScores <- function(layers, conf, scores){

  # get regions
  rgns <- SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d <- expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d <- subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores <- merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores <- arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score <- round(scores$score, 2)

  return(scores)
}
