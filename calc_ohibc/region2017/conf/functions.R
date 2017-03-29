Setup <- function(){
  if(file.exists('eez2013/temp/referencePoints.csv')){file.remove('temp/referencePoints.csv')}
  referencePoints <- data.frame(goal=as.character(),
                                method = as.character(),
                                reference_point = as.character())
  write_csv(referencePoints, 'temp/referencePoints.csv')
}

ICO <- function(layers, status_years){

  layers_data <- SelectLayersData(layers, layers=c('ico_spp_iucn_status'))

  ico_layer <- layers_data %>%
    select(region_id = id_num, sciname = category, iucn_cat=val_chr, year, layer) %>%
    mutate(iucn_cat = as.character(iucn_cat))

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
  ico_risk_cat <- data.frame(iucn_cat   = c('LC', 'NT', 'CD', 'VU', 'EN', 'CR', 'EX', 'DD'),
                                risk_score = c( 0.0,  0.2,  0.3,  0.4,  0.6,  0.8,  1.0,  NA)) %>%
    mutate(status_score = 1 - risk_score) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  ####### status
  # STEP 1: take mean of subpopulation scores
  rgn_ico_status_spp <- ico_layer %>%
    left_join(ico_risk_cat, by = 'iucn_cat') %>%
    group_by(region_id, sciname, year) %>%
    summarize(spp_mean = mean(status_score, na.rm = TRUE)) %>%
    ungroup()

  # STEP 2: take mean of populations within regions
  rgn_ico_status <- rgn_ico_status_spp %>%
    group_by(region_id, year) %>%
    summarize(score = mean(spp_mean, na.rm = TRUE)) %>%
    ungroup()

  ####### trend
  trend_span    <- 10 ### trend based on 10 years of data, due to infrequency of IUCN assessments
  rgn_ico_trend <- data.frame()

  for(yr in status_years) {
    # yr <- status_years[1]
    trend_yr_include <- c(yr:(yr - trend_span + 1))
    if(!all(trend_yr_include %in% rgn_ico_status$year)) {
      message('Calculating trend for ', yr, ': status not available for ',
              paste(trend_yr_include[!trend_yr_include %in% rgn_ico_status$year], collapse = ', '))
    }
    trend_tmp <- rgn_ico_status %>%
      filter(year %in% trend_yr_include) %>%
      group_by(region_id) %>%
      do(mdl = lm(score ~ year, data=.)) %>%
      summarize(region_id = region_id,
                score = coef(mdl)['year'] * 5) %>%
      ungroup() %>%
      mutate(year = yr,
             dimension = "trend")
    rgn_ico_trend <- bind_rows(rgn_ico_trend, trend_tmp)
  }


  ####### status
  rgn_ico_status <- rgn_ico_status %>%
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    select(year, region_id, score, dimension)

  ## reference points
  ref_pts <- read_csv('temp/referencePoints.csv') %>%
    rbind(data.frame(goal = "ICO", method = "scaled IUCN risk categories",
                     reference_point = NA))
  write_csv(ref_pts, 'temp/referencePoints.csv')


  # return scores
  scores <-  bind_rows(rgn_ico_status, rgn_ico_trend) %>%
    filter(year %in% status_years) %>%
    mutate('goal'='ICO') %>%
    select(year, goal, dimension, region_id, score) %>%
    data.frame()

  return(scores)

}

LSP <- function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_years){

  trend_years <- (status_years-4):status_year

  # select data ----
  tot_area_rgn  <- SelectLayersData(layers, layers=c('rgn_area_inland1km', 'rgn_area_offshore3nm'))
  prot_area_rgn <- SelectLayersData(layers, layers=c('lsp_prot_area_offshore3nm', 'lsp_prot_area_inland1km'))

  tot_area_rgn <- tot_area_rgn %>%
    select(region_id = id_num, val_num, layer) %>%
    spread(layer, val_num) %>%
    select(region_id, area_inland1km = rgn_area_inland1km,
           area_offshore3nm = rgn_area_offshore3nm)

  prot_area_rgn <- prot_area_rgn %>%
    select(region_id = id_num, year, val_num, layer) %>%
    spread(layer, val_num) %>%
    select(region_id, year,
           cmpa = lsp_prot_area_offshore3nm,
           cp = lsp_prot_area_inland1km)

  # fill in time series for all regions

rgn_yrs <- expand.grid(region_id = unique(prot_area_rgn$region_id),
                       year      = unique(prot_area_rgn$year)) %>%
  left_join(prot_area_rgn, by=c('region_id', 'year')) %>%
  arrange(region_id, year) %>%
  mutate(cp   = ifelse(is.na(cp),   0, cp),
         cmpa = ifelse(is.na(cmpa), 0, cmpa)) #  %>%
  # mutate(pa   = cp + cmpa)

  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
rgn_yrs <- rgn_yrs %>%
  full_join(tot_area_rgn, by="region_id") %>%
  mutate(pct_cp    = pmin(cp   / area_inland1km   * 100, 100),
         pct_cmpa  = pmin(cmpa / area_offshore3nm * 100, 100),
         prop_protected = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
  filter(!is.na(prop_protected))

# extract status based on specified year
  rgn_status <- rgn_yrs %>%
    filter(year %in% status_years) %>%
    select(year, region_id, status = prop_protected) %>%
    mutate(status = status * 100)

  # calculate trend
  rgn_trend <-   rgn_yrs %>%
    filter(year %in% trend_years) %>%
    group_by(region_id) %>%
    do(mdl = lm(prop_protected ~ year, data=.)) %>%
    summarize(
      region_id = region_id,
      trend = min(1, max(0, 5*coef(mdl)['year']))) %>% # set boundaries so trend does not go below 0 or above 1
    ungroup()

  ## reference points
  rp <- read_csv('temp/referencePoints.csv') %>%
    rbind(data.frame(goal = "LSP", method = paste0(ref_pct_cmpa, "% marine protected area; ",
                                                   ref_pct_cp, "% coastal protected area"),
                     reference_point = "varies by area of region's eez and 1 km inland"))
  write_csv(rp, 'temp/referencePoints.csv')


  # return scores
  scores <- bind_rows(
    within(rgn_status, {
      goal      = 'LSP'
      dimension = 'status'
      score     = status}),
    within(rgn_trend, {
      goal      = 'LSP'
      dimension = 'trend'
      score     = trend}))
  return(scores[,c('region_id','goal','dimension','score')])
}

SP <- function(scores){

  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    filter(goal %in% c('ICO','LSP') &
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
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
