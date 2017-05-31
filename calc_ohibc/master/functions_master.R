Setup <- function(){
  ref_pt_file <- file.path(layers$data$dir_calc, 'reference_pts.csv')
  unlink(ref_pt_file)

  ref_pts <- data.frame(year   = as.integer(),
                        goal   = as.character(),
                        method = as.character(),
                        reference_point = as.character())
  write_csv(ref_pts, ref_pt_file)
}

### functions to support functions.R
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

calc_trend <- function(scenario_df, years = NULL) {
  ### provide a dataframe of status by region by year; this will
  ### calculate the linear trend across the entire span, and then
  ### divide by the first value to get a proportional change. If
  ### a vector of years is provided, will calculate based on those

  if('score' %in% names(scenario_df) & !'status' %in% names(scenario_df)) {
    scenario_df <- scenario_df %>%
      rename(status = score)
  }

  if(!all(c('year', 'region_id', 'status') %in% names(scenario_df))) {
    stop('calc_trend() requires fields named "year", "region_id", and "status" or "score" - fix it!')
  }

  if(!is.null(years)) scenario_df <- scenario_df %>%
      filter(year %in% years)

  if(any(is.na(scenario_df$year))) {
    stop('calc_trend(): NA values for year not allowed')
  }

  max_year <- max(scenario_df$year)

  goalname <- scenario_df$goal[1]
  if(is.null(goalname)) goalname <- NA

  message('Calculating trend for goal ', goalname, ' for years ',
          paste(scenario_df$year %>% range(), collapse = ' - '))

  scenario_df <- scenario_df %>%
    arrange(region_id, year) %>%
    mutate(status_1 = first(status))

  trend <- scenario_df %>%
    group_by(region_id, status_1) %>%
    do(mdl = lm(status ~ year, data = . )) %>%
    summarize(
      region_id = region_id,
      score = 5 * coef(mdl)['year'] / status_1,
      score = min(max(score, -1), 1)) %>% # set boundaries so trend does not go below -1 or above 1
    ungroup() %>%
    mutate(year  = max_year,
           goal  = goalname,
           dimension = 'trend',
           score = round(score, 5))

  trend <- trend %>%
    filter(!is.na(region_id))

  return(trend)

}

get_data_year <- function(goal, status_yr, layers) {
  data_year <- layers$data$stat_yr_matrix %>%
    filter(goal_name == goal & status_year == status_yr) %>%
    .$data_year

  return(data_year)

}

complete_years <- function(score_df, year_span) {
  if('rgn_id' %in% names(score_df)) {
    message('The complete_years() function automagically renames "rgn_id" to "region_id" for your convenience.')
    score_df <- score_df %>%
      rename(region_id = rgn_id)
  }
  data_range <- range(score_df$year, na.rm = TRUE)
  if(min(year_span) > data_range[1] | max(year_span) < data_range[2]) {
    min_yr <- min(min(year_span), data_range[1])
    max_yr <- max(max(year_span), data_range[2])
    message('Data year span (', data_range[1], ':', data_range[2],
            ') exceeds assigned year span (',
            min(year_span), ':', max(year_span),
            '); completing data sequence from ', min_yr, ' to ', max_yr, '.')
    year_span <- min_yr : max_yr
  }
  score_df <- score_df %>%
    group_by(region_id) %>%
    complete(year = year_span) %>%
    arrange(year) %>%
    fill(-year, -region_id, .direction = 'down') %>% fill(-year, -region_id, .direction = 'up') %>%
    ungroup()

  return(score_df)
}

FIS <- function(layers) {


  ##### Gather parameters and layers #####
  ### * ram_b_bmsy, ram_f_fmsy, ram_catch
  ### * rgn_stock_wt_uniform, _saup, _dfo
  ### * ram_dfo_saup_lookup.csv

  status_year <- layers$data$status_year
  data_year <- get_data_year('FIS', status_year, layers)
  year_span <- layers$data$year_span

  ram_b_bmsy        <- layers$data[['fis_ram_b_bmsy']] %>%
    select(year, stock_id, b_bmsy = value)
  ram_f_fmsy        <- layers$data[['fis_ram_f_fmsy']] %>%
    select(year, stock_id, f_fmsy = value)
  ram_catch         <- layers$data[['fis_ram_catch']] %>%
    select(year, stock_id, catch = value)
  rgn_stock_wt_dfo  <- layers$data[['fis_stock_wt_dfo']] %>%
    select(region_id = rgn_id, stock_id = ram_stock_id, dfo_wt = catch_wt)
  rgn_stock_wt_saup <- layers$data[['fis_stock_wt_saup']] %>%
    select(region_id = rgn_id, stock_id = ram_stockid, saup_wt = catch_wt)
  stock_wt_src      <- layers$data[['fis_stock_wt_src']] %>%
    select(stock_id = ram_stock_id, priority)

  ### These parameters are based on conversation with Ian Perry, Karen Hunter,
  ### and Karin Bodtker on May 24 2017.
  b_bmsy_underexploit_penalty <- 0.25
  b_bmsy_underexploit_thresh  <- 3.00
  f_fmsy_underfishing_penalty <- 0.25
  f_fmsy_overfishing_thresh   <- 2.00

  stock_status_layers <- ram_b_bmsy %>%
    full_join(ram_f_fmsy, by = c('year', 'stock_id'))

  ########################################################.
  ##### run each fishery through the Kobe plot calcs #####
  ########################################################.
  ### * ram_b_bmsy, ram_f_fmsy

  ### Apply rolling mean to F/Fmsy
  ram_f_fmsy <- ram_f_fmsy %>%
    mutate(f_fmsy_raw = f_fmsy) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    filter(!is.na(f_fmsy)) %>%
    mutate(f_fmsy = zoo::rollmean(f_fmsy_raw, k = 4, align = 'right', fill = NA)) %>%
    ungroup() %>%
    select(-f_fmsy_raw)

  ### Function for converting B/Bmsy values into a 0 - 1 score
  rescale_bprime_crit <- function(fish_stat_df,
                                  bmax, bmax_val) {

    ### parameter from DFO harvest control rule:
    overfished_th  <- 0.8
    ### parameter from OHI California Current:
    underfished_th <- 1.5

    bmax_adj <- (bmax - underfished_th) / (1 - bmax_val) + underfished_th
      ### this is used to create a "virtual" B/Bmsy max where score drops
      ### to zero.  If bmax_val == 0, this is bmax; if bmax_val > 0, bmax_adj
      ### extends beyond bmax, to create a gradient where bmax_val occurs at bmax.

    fish_stat_df <- fish_stat_df %>%
      # group_by(stock) %>% ### grouping by stock will set b_max by max per stock, instead of max overall
      mutate(b_max     = max(b_bmsy, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(bPrime = NA,
             bPrime = ifelse(b_bmsy < overfished_th,  ### overfished stock
                             b_bmsy / overfished_th,
                             bPrime),
             bPrime = ifelse(b_bmsy >= overfished_th & b_bmsy < underfished_th,
                             1,                       ### appropriately fished stock
                             bPrime),
             bPrime = ifelse(b_bmsy >= underfished_th,
                             (bmax_adj - b_bmsy) / (bmax_adj - underfished_th), ### underfished stock
                             bPrime),
             bPrime = ifelse(bPrime < 0, 0, bPrime))
    return(fish_stat_df)
  }


  ### Function to create vertical gradient based on distance from
  ### ideal F/Fmsy value to actual F/Fmsy value
  f_gradient <- function(f, over_f, under_f, fmax, fmin_val) {
    x <- ifelse(f < over_f & f > under_f, 1, NA)
    x <- ifelse(f <= under_f, (f * (1 - fmin_val) / under_f + fmin_val), x)
    x <- ifelse(f >= over_f,  (fmax - f) / (fmax - over_f), x)
    x <- ifelse(f > fmax, NA, x)
    return(x)
  }

  ### Function to convert F/Fmsy values into 0 - 1 score
  rescale_fprime_crit <- function(fish_stat_df,
                                  fmax, fmin_val) {

    ### params from DFO harvest control rule:
    Bcrit <- 0.4; overfished_th <- 0.8
    ### params from OHI California Current:
    underfishing_th <- 0.8; overfishing_th  <- 1.2

    bcritslope = 1 / (overfished_th - Bcrit)
      ### connecting from (Bcrit, 0) to (overfished_th, 1)

    fish_stat_df <- fish_stat_df %>%
      mutate(fPrime = ifelse(b_bmsy < overfished_th & f_fmsy < fmax,
                             f_gradient(f_fmsy + (overfished_th - b_bmsy) * bcritslope,
                                        over_f = overfishing_th,
                                        under_f = underfishing_th,
                                        fmax = fmax,
                                        fmin_val = fmin_val),
                             NA),
             fPrime = ifelse(b_bmsy >= overfished_th & f_fmsy < fmax,
                             f_gradient(f_fmsy,
                                        over_f = overfishing_th,
                                        under_f = underfishing_th,
                                        fmax = fmax,
                                        fmin_val = fmin_val),
                             fPrime),
             fPrime = ifelse(is.na(fPrime), 0, fPrime)
      )
    return(fish_stat_df)
  }

  stock_status_df <- stock_status_layers %>%
    rescale_bprime_crit(bmax     = b_bmsy_underexploit_thresh,
                        bmax_val = b_bmsy_underexploit_penalty) %>%
    rescale_fprime_crit(fmax     = f_fmsy_overfishing_thresh,
                        fmin_val = f_fmsy_underfishing_penalty) %>%
    mutate(x_prod = (fPrime * bPrime)) %>%
    dplyr::select(year, stock_id,
                  score = x_prod,
                  b_bmsy, f_fmsy)  %>%
    group_by(stock_id) %>%
    complete(year = year_span) %>%
    arrange(year) %>%
    fill(score, .direction = 'down') %>%
    ungroup()

  ##############################################################.
  ##### calculate distribution of fishery catch to regions #####
  ##############################################################.
  ### * ram_catch, rgn_stock_wt_*

  stock_wt_df <- rgn_stock_wt_dfo %>%
    full_join(rgn_stock_wt_saup, by = c('stock_id', 'region_id')) %>%
    left_join(stock_wt_src, by = 'stock_id') %>%
    gather(key = src, value = catch_wt, dfo_wt:saup_wt) %>%
    filter(str_detect(src, priority) & !is.na(catch_wt)) %>%
    select(region_id, stock_id, catch_wt) %>%
    filter(catch_wt > 0)

  ### calculate weights within each region by regional catch
  catch_df <- ram_catch %>%
    left_join(stock_wt_df, by = 'stock_id') %>%
    group_by(region_id, stock_id) %>%
    complete(year = year_span) %>%
    arrange(year) %>%
    fill(catch, catch_wt, .direction = 'down') %>%
    ungroup() %>%
    mutate(rgn_catch = catch * catch_wt,
           rgn_catch = ifelse(is.na(rgn_catch), 0, rgn_catch))

  stock_score_df <- stock_status_df %>%
    group_by(stock_id) %>%
    arrange(stock_id, year) %>%
    fill(score, .direction = c('down')) %>%
    fill(score, .direction = c('up')) %>%
    ungroup() %>%
    full_join(catch_df, by = c('stock_id', 'year')) %>%
    select(region_id, year, stock_id, score, rgn_catch) %>%
    filter(year %in% year_span & !is.na(region_id))

  if(data_year == max(year_span)) {
    write_csv(stock_score_df, '~/github/ohibc/prep/fis/v2017/summary/fis_from_functions.csv')
  }

  ### plotting fishery catch weighting by region
  # stock_plot_df <- stock_score_df %>%
  #   group_by(region_id, year) %>%
  #   mutate(total_catch = sum(rgn_catch),
  #          rgn_catch_pct = rgn_catch / total_catch,
  #          total_score = sum(score * rgn_catch) / total_catch) %>%
  #   ungroup() %>%
  #   left_join(get_rgn_names(), by = 'region_id')
  #
  # for(rgn in 1:8) {
  #   # rgn <- 1
  #   rgn_plot_df <- stock_plot_df %>%
  #     filter(region_id == rgn)
  #   rgn_plot <- ggplot(rgn_plot_df, aes(x = year, y = rgn_catch_pct)) +
  #     geom_area(aes(group = stock_id, fill = stock_id)) +
  #     labs(title = first(rgn_plot_df$rgn_name),
  #          fill  = 'Stock ID',
  #          y     = 'Proportion of catch')
  #   print(rgn_plot)
  # }

  score_df <- stock_score_df %>%
    group_by(region_id, year) %>%
    mutate(stock_catch = sprintf('%s: %.2f', stock_id, rgn_catch)) %>%
    summarize(total_catch = sum(rgn_catch),
              total_score = sum(score * rgn_catch) / total_catch,
              n_stocks    = sum(rgn_catch > 0),
              stocks      = paste(tolower(stock_catch), collapse = '\n')) %>%
    ungroup()

  fis_status <- score_df %>%
    select(region_id = region_id, year, score = total_score) %>%
    mutate(score     = 100 * score,
           goal      = 'FIS',
           dimension = 'status')

  ## reference points
  write_ref_pts(goal   = "FIS",
                method = "XXXXXXXX",
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  fis_trend   <- calc_trend(fis_status, trend_years)

  fis_scores <- fis_status %>%
    filter(year == data_year) %>%
    filter(!is.na(region_id)) %>%
    bind_rows(fis_trend) %>%
    select(goal, dimension, region_id, score)

  return(fis_scores)

}

MAR <- function(layers) {
  # # layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score, mar_coastalpopn_inland25mi, mar_trend_years
  # harvest_tonnes <- SelectLayersData(layers, layers='mar_harvest_tonnes', narrow = TRUE) %>%
  #   select(region_id=id_num, species_code=category, year, tonnes=val_num)
  #
  # sustainability_score <- SelectLayersData(layers, layers='mar_sustainability_score', narrow = TRUE) %>%
  #   select(region_id=id_num, species_code=category, sust_coeff=val_num)
  #
  # popn_inland25mi <- SelectLayersData(layers, layers='mar_coastalpopn_inland25mi', narrow = TRUE) %>%
  #   select(region_id=id_num, year, popsum=val_num) %>%
  #   mutate(popsum = popsum + 1)
  #
  #
  # rky <-  harvest_tonnes %>%
  #   left_join(sustainability_score, by = c('region_id', 'species_code'))
  #
  # # fill in gaps with no data
  # rky <- spread(rky, year, tonnes)
  # rky <- gather(rky, "year", "tonnes", 4:dim(rky)[2])
  #
  #
  # # 4-year rolling mean of data
  # m <- rky %>%
  #   mutate(year = as.numeric(as.character(year))) %>%
  #   group_by(region_id, species_code, sust_coeff) %>%
  #   arrange(region_id, species_code, year) %>%
  #   mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
  #   ungroup()
  #
  # # smoothed mariculture harvest * sustainability coefficient
  # m <- m %>%
  #   mutate(sust_tonnes = sust_coeff * sm_tonnes)
  #
  #
  # # aggregate all weighted timeseries per region, and divide by coastal human population
  # ry <- m %>%
  #   group_by(region_id, year) %>%
  #   summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
  #   left_join(popn_inland25mi, by = c('region_id','year')) %>%
  #   mutate(mar_pop = sust_tonnes_sum / popsum) %>%
  #   ungroup()
  #
  #
  # # get reference quantile based on argument years
  # ref_95pct_data <- ry %>%
  #   filter(year <= scenario)
  #
  # ref_95pct <- quantile(ref_95pct_data$mar_pop, 0.95, na.rm=TRUE)
  #
  # # identify reference region_id
  # ry_ref = ref_95pct_data %>%
  #   arrange(mar_pop) %>%
  #   filter(mar_pop >= ref_95pct)
  # message(sprintf('95th percentile for MAR ref pt is: %s\n', ref_95pct)) # region_id 25 = Thailand
  # message(sprintf('95th percentile region_id for MAR ref pt is: %s\n', ry_ref$region_id[1])) # region_id 25 = Thailand
  #
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "MAR", method = "spatial 95th quantile",
  #                    reference_point = paste0("region id: ", ry_ref$region_id[1], ' value: ', ref_95pct)))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  #
  # ry = ry %>%
  #   mutate(status = ifelse(mar_pop / ref_95pct > 1,
  #                          1,
  #                          mar_pop / ref_95pct))
  # status <- ry %>%
  #   filter(year == scenario) %>%
  #   select(region_id, status) %>%
  #   mutate(status = round(status*100, 2))
  #
  # trend_years <- (scenario-4):(scenario)
  # first_trend_year <- min(trend_years)
  #
  # # get MAR trend
  # trend = ry %>%
  #   group_by(region_id) %>%
  #   filter(year %in% trend_years) %>%
  #   filter(!is.na(popsum)) %>%
  #   do(mdl = lm(status ~ year, data=.),
  #      adjust_trend = .$status[.$year == first_trend_year]) %>%
  #   summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
  #   ungroup()
  #
  # trend <- trend %>%
  #   mutate(trend = ifelse(trend>1, 1, trend)) %>%
  #   mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
  #   mutate(trend = round(trend, 4)) %>%
  #   select(region_id = region_id, score = trend) %>%
  #   mutate(dimension = "trend")
  #
  # # return scores
  # scores_mar = status %>%
  #   select(region_id = region_id,
  #          score     = status) %>%
  #   mutate(dimension='status') %>%
  #   rbind(trend) %>%
  #   mutate(goal='MAR')
  #
  # return(scores_mar)
  return(data.frame(goal = 'MAR',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = rep(NA, 16)))
}

FP <- function(layers, scores) {

  # # weights
  # w <-  SelectLayersData(layers, layers='fp_wildcaught_weight', narrow = TRUE) %>%
  #   select(region_id = id_num, w_FIS = val_num); head(w)
  #
  # # scores
  # s <- scores %>%
  #   filter(goal %in% c('FIS', 'MAR')) %>%
  #   filter(!(dimension %in% c('pressures', 'resilience'))) %>%
  #   left_join(w, by="region_id")  %>%
  #   mutate(w_MAR = 1 - w_FIS) %>%
  #   mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))
  #
  #
  # ## Some warning messages due to potential mismatches in data:
  # # NA score but there is a weight
  # tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  # if(dim(tmp)[1]>0){
  #   warning(paste0("Check: these regions have a FIS weight but no score: ",
  #                  paste(as.character(tmp$region_id), collapse = ", ")))}
  #
  # tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  # if(dim(tmp)[1]>0){
  #   warning(paste0("Check: these regions have a MAR weight but no score: ",
  #                  paste(as.character(tmp$region_id), collapse = ", ")))}
  #
  # # score, but the weight is NA or 0
  # tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  # if(dim(tmp)[1]>0){
  #   warning(paste0("Check: these regions have a FIS score but no weight: ",
  #                  paste(as.character(tmp$region_id), collapse = ", ")))}
  #
  # tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  # if(dim(tmp)[1]>0){
  #   warning(paste0("Check: these regions have a MAR score but no weight: ",
  #                  paste(as.character(tmp$region_id), collapse = ", ")))}
  #
  # s <- s  %>%
  #   group_by(region_id, dimension) %>%
  #   summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
  #   mutate(goal = "FP") %>%
  #   ungroup() %>%
  #   select(region_id, goal, dimension, score) %>%
  #   data.frame()
  #
  # # return all scores
  # return(rbind(scores, s))
  return(rbind(scores,
               data.frame(goal = 'FP',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = rep(NA, 16))))
}

AO <- function(layers) {

  ### Salt marsh, coastal forest based on extent from 30-meter rasters

  status_year <- layers$data$status_year
  year_span <- layers$data$year_span
  data_year <- get_data_year('AO', status_year, layers)

  ### get the data:
  closures <- layers$data[['ao_closures']]
  licenses <- layers$data[['ao_licenses']]
  shi      <- layers$data[['ao_spawn_hab_index']]
  salmon   <- layers$data[['ao_salmon']]

  ### assign weights to each layer
  component_wts <- c('shellfish_closures' = 1,
                     'first_nations_licenses' = 1,
                     'herring_spawn_hab_index' = 1,
                     'salmon' = 0)

  ### Calculate status for each component
  ### Closures:
  ### * proportion of year open for access; 0 closures = 100%
  closure_status <- closures %>%
    complete_years(year_span) %>%
    mutate(days_in_year = ifelse(lubridate::leap_year(year), 366, 365),
           status = 1 - (days_avg / days_in_year),
           component = 'shellfish_closures') %>%
    select(year, region_id, status, component)

  ### Licenses:
  ### * prop of licenses allocated to FNs, with some level (25%?) as target?
  ### * no net loss vs some rolling average?
  license_ref_pt <- .25 ### arbitrary at this point; how should we consider ref pt?
  license_status <- licenses %>%
    complete_years(year_span) %>%
    mutate(status = pct_fn / license_ref_pt,
           status = ifelse(status > 1, 1, status),
           component = 'first_nations_licenses') %>%
    select(year, region_id, status, component)
  # ggplot(licenses_target, aes(x = year, y = status, group = region_id, color = region_id)) + geom_line()

  ### Spawn habitat index
  ### * SHI vs historical reference point of mean SHI from 1940-1960.
  shi_hist_ref <- shi %>%
    complete_years(year_span) %>%
    filter(year %in% c(1940:1960)) %>%
    group_by(region_id) %>%
    summarize(shi_ref_pt = mean(shi_tot, na.rm = TRUE)) %>%
    ungroup()
  shi_status <- shi %>%
    complete_years(year_span) %>%
    left_join(shi_hist_ref, by = 'region_id') %>%
    mutate(shi_3yr_mean = (shi_tot + lag(shi_tot, 1) + lag(shi_tot, 2)) / 3,
           status = shi_3yr_mean / shi_ref_pt,
           status = ifelse(status > 1, 1, status),
           component = 'herring_spawn_hab_index') %>%
    select(year, region_id, status, component)

  ### Salmon
  ### * dummy for now
  salmon_status <- salmon %>%
    complete_years(year_span) %>%
    mutate(status = 0,
           component = 'salmon') %>%
    select(year, region_id, status, component)


  ### Combine all components by weighting
  ao_status <- bind_rows(closure_status, license_status, shi_status, salmon_status) %>%
    mutate(comp_wt = component_wts[component]) %>%
    group_by(region_id, year) %>%
    summarize(score = sum(status * comp_wt, na.rm = TRUE) / sum(comp_wt),
              score = round(score * 100, 5)) %>%
    ungroup() %>%
    mutate(goal      = 'AO',
           dimension = 'status')

  if(data_year == max(year_span)) {
    ao_status_components <- bind_rows(closure_status, license_status, shi_status, salmon_status) %>%
      filter(year %in% 2000:2017) %>%
      filter(!is.na(region_id)) %>%
      left_join(get_rgn_names(), by = c('region_id' = 'rgn_id')) %>%
      left_join(ao_status, by = c('year', 'region_id'))

    write_csv(ao_status_components, '~/github/ohibc/prep/ao/v2017/summary/ao_from_functions.csv')
  # ggplot(ao_status_components, aes(x = year, y = status, group = component, color = component)) +
  #   geom_line(aes(y = score), color = 'grey40', size = 1.5, alpha = .8) +
  #   geom_line(size = 1, alpha = .8) +
  #   facet_wrap( ~ rgn_name)
  }

  ## reference points
  write_ref_pts(goal   = "AO",
                method = "XXXXXXXX",
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  ao_trend   <- calc_trend(ao_status, trend_years)

  ao_scores <- ao_status %>%
    filter(!is.na(region_id)) %>%
    filter(year == data_year) %>%
    bind_rows(ao_trend) %>%
    select(region_id, goal, dimension, score)

  return(ao_scores)

}

CS <- function(layers) {

  ### Salt marsh, coastal forest based on extent from 30-meter rasters

  status_year <- layers$data$status_year
  data_year <- get_data_year('CS', status_year, layers)
  year_span <- layers$data$year_span

  # Carbon burial rates (gC m^-2^ yr^-1^)
  #
  # * Salt marsh: 218 +/- 24 gC m^-2^ yr^-1^ (mean +/- SE)
  # * Seagrasses: 138 +/- 38 gC m^-2^ yr^-1^
  #   * Coastal forests
  # * temperate: 5.1 +/- 1.0 gC m^-2^ yr^-1^
  #   * boreal: 4.6 +/- 2.1 gC m^-2^ yr^-1^
  #
  # Source: Mcleod et al. 2011. A blueprint for blue carbon: toward an
  # improved understanding of the role of vegetated coastal habitats in
  # sequestering CO2. Frontiers in Ecology 9(10): 552-560, DOI

  cs_values <- data.frame(hab = c('salt_marsh', 'coastal_forest', 'seagrass'),
                          cbr = c(       218.0,              4.6,     138.0))

  ### get the data:
  sm_health   <- layers$data[['hab_sm_health']] %>%
    select(year, region_id = rgn_id, area_hab = sm_area_km2) %>%
    arrange(region_id, year) %>%
    mutate(hab = 'salt_marsh')

  cf_health   <- layers$data[['hab_cf_health']] %>%
    select(year, region_id = rgn_id, area_hab = cf_area_km2) %>%
    arrange(region_id, year) %>%
    mutate(hab = 'coastal_forest')

  cs_status <- bind_rows(sm_health, cf_health) %>%
    left_join(cs_values, by = 'hab') %>%
    group_by(region_id, hab) %>%
    complete_years(year_span) %>%
    mutate(aref = first(area_hab)) %>%
    group_by(region_id, year) %>%
    summarize(score = sum(area_hab * cbr) / sum(aref * cbr)) %>%
    mutate(goal      = 'CS',
           dimension = 'status',
           score = ifelse(score > 1, 1, score) * 100) %>%
    ungroup()

  ## reference points
  write_ref_pts(goal   = "CS",
                method = "XXXXXXXX",
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  cs_trend   <- calc_trend(cs_status, trend_years)

  cs_scores <- cs_status %>%
    filter(year == data_year) %>%
    bind_rows(cs_trend) %>%
    select(region_id, goal, dimension, score)

  return(cs_scores)

  # ## set ranks for each habitat ### WHERE ARE THESE NUMBERS FROM
  # habitat.rank <- c('mangrove'         = 139,
  #                   'saltmarsh'        = 210,
  #                   'seagrass'         = 83)
  #

}

CP <- function(layers) {

  ### Salt marsh, coastal forest based on extent from 30-meter rasters
  status_year <- layers$data$status_year
  data_year <- get_data_year('CP', status_year, layers)
  year_span <- layers$data$year_span

  # Protection values
  # Protection weights are assigned based on vulnerability values from InVEST
  # Coastal Vulnerability Model (http://data.naturalcapitalproject.org/nightly-build/invest-users-guide/html/coastal_vulnerability.html):
  #
  #   | Vulnerability | Very Low |  Low  | Moderate |  High  | Very High |
  #   | ----          | :------: | :---: | :------: | :----: | :-------: |
  #   | Score         |     1    |   2   |     3    |    4   |     5     |
  #   | Natural Habs  | Coral reef; mangrove; coastal forest | High dune; marsh | Low dune | Seagrass; kelp | No habitat |
  #   | Prot. weight  |     4    |   3   |     2    |    1   |     0     |


  cp_values <- data.frame(hab  = c('salt_marsh', 'coastal_forest', 'seagrass'),
                          prot = c(      3,             4,              1    ))

  ### get the data:
  sm_prot <- layers$data[['cp_sm_health']] %>%
    select(year, region_id = rgn_id, area_hab = sm_expos_area_tot) %>%
    arrange(region_id, year) %>%
    mutate(hab = 'salt_marsh')

  cf_prot <- layers$data[['cp_cf_health']] %>%
    select(year, region_id = rgn_id, area_hab = cf_expos_area_tot) %>%
    arrange(region_id, year) %>%
    mutate(hab = 'coastal_forest')

  cp_status <- bind_rows(sm_prot, cf_prot) %>%
    left_join(cp_values, by = 'hab') %>%
    group_by(region_id, hab) %>%
    complete_years(year_span) %>%
    mutate(aref = first(area_hab)) %>%
    group_by(region_id, year) %>%
    summarize(score = sum(area_hab * prot) / sum(aref * prot)) %>%
    mutate(goal      = 'CP',
           dimension = 'status',
           score = ifelse(score > 1, 1, score) * 100) %>%
    ungroup()

  ## reference points
  write_ref_pts(goal   = "CP",
                method = "XXXXXXXX",
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year
  trend_years <- (data_year - 4) : data_year
  cp_trend   <- calc_trend(cp_status, trend_years)

  cp_scores <- cp_status %>%
    filter(year == data_year) %>%
    bind_rows(cp_trend) %>%
    select(region_id, goal, dimension, score)

  return(cp_scores)

  # ## read in layers
  # extent <- layers$data[['hab_extent']] %>%
  #   select(region_id, habitat, km2) %>%
  #   mutate(habitat = as.character(habitat))
  #
  # health <-  layers$data[['hab_health']] %>%
  #   select(region_id, habitat, health) %>%
  #   mutate(habitat = as.character(habitat))
  #
  # trend <-layers$data[['hab_trend']] %>%
  #   select(region_id, habitat, trend) %>%
  #   mutate(habitat = as.character(habitat))
  #
  #
  # ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  # mangrove_extent <- extent %>%
  #   filter(habitat %in% c('mangrove_inland1km','mangrove_offshore'))
  #
  # if (nrow(mangrove_extent) > 0){
  #   mangrove_extent <- mangrove_extent %>%
  #     group_by(region_id) %>%
  #     summarize(km2 = sum(km2, na.rm = TRUE)) %>%
  #     mutate(habitat='mangrove') %>%
  #     ungroup()
  # }
  #
  # extent <- extent %>%
  #   filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore')) %>%  #do not use all mangrove
  #   rbind(mangrove_extent)  #just the inland 1km and offshore
  #
  # ## join layer data
  # d <-  extent %>%
  #   full_join(health, by=c("region_id", "habitat")) %>%
  #   full_join(trend, by=c("region_id", "habitat"))
  #
  # ## set ranks for each habitat
  # habitat.rank <- c('coral'            = 4,
  #                   'mangrove'         = 4,
  #                   'saltmarsh'        = 3,
  #                   'seagrass'         = 1,
  #                   'seaice_shoreline' = 4)
  #
  # ## limit to CP habitats and add rank
  # d <- d %>%
  #   filter(habitat %in% names(habitat.rank)) %>%
  #   mutate(
  #     rank = habitat.rank[habitat],
  #     extent = ifelse(km2==0, NA, km2))
  #
  # ## output file to temp folder that describes how much each habitat
  # ## contributes to the score based on rank and extent
  # ## this output is for the dataplayground website
  # dp <- d %>%
  #   mutate(weighted_cont = rank*extent) %>%
  #   filter(!is.na(weighted_cont)) %>%
  #   group_by(region_id) %>%
  #   mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
  #   mutate(prop_score = round(prop_score, 3)) %>%
  #   select(region_id, habitat, prop_score)
  # write.csv(dp, 'temp/cp_hab_contributions.csv', row.names=FALSE)
  #
  # ## status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  # if (sum(d$km2, na.rm=TRUE) > 0){
  #   # status
  #   scores_CP <- d %>%
  #     filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
  #     group_by(region_id) %>%
  #     summarize(score = pmin(1, sum(rank * health * extent, na.rm=TRUE) /
  #                              (sum(extent * rank, na.rm=TRUE)) ) * 100) %>%
  #     mutate(dimension = 'status') %>%
  #     ungroup()
  #
  #   # trend
  #   d_trend <- d %>%
  #     filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
  #
  #   if (nrow(d_trend) > 0 ){
  #     scores_CP <- dplyr::bind_rows(
  #       scores_CP,
  #       d_trend %>%
  #         group_by(region_id) %>%
  #         summarize(
  #           score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
  #           dimension = 'trend'))
  #   } else { # if no trend score, assign NA
  #     scores_CP <- dplyr::bind_rows(
  #       scores_CP,
  #       d %>%
  #         group_by(region_id) %>%
  #         summarize(
  #           score = NA,
  #           dimension = 'trend'))
  #   }
  #
  #   ### output data file for checking and data review
  #   scores_check <- spread(scores_CP, dimension, score) %>%
  #     select(region_id, status, trend_score=trend)
  #
  #   d_check <- d %>%
  #     select(region_id, habitat, extent, health, trend, rank) %>%
  #     arrange(region_id, habitat) %>%
  #     left_join(scores_check, by="region_id")
  #   write.csv(d_check, sprintf('temp/cp_data_%s.csv', scenario), row.names=FALSE)
  #
  #   ## finalize scores_CP
  #   scores_CP <- scores_CP %>%
  #     mutate(
  #       goal = 'CP') %>%
  #     select(region_id=region_id, goal, dimension, score)
  #
  # } else { ## else -- if sum(d$km2) is not greater than 0
  #
  #   ## set status and trend to NA for all regions
  #   message('CP status and trend are NA, consider removing goal if no CP habitats in assessment area')
  #
  #   rgns <-layers$data[['rgn_labels']]
  #   scores_CP <- bind_rows(
  #     rgns %>%
  #       mutate(goal      = 'CP',
  #              dimension = 'status',
  #              score     = NA),
  #     rgns %>%
  #       mutate(goal      = 'CP',
  #              dimension = 'trend',
  #              score     = NA)) %>%
  #     select(goal, dimension, region_id = region_id, score)
  #
  # } ## end -- if (sum(d$km2) > 0)
  #
  # ## reference points
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "CP", method = "Health/condition variable based on current vs. historic extent",
  #                    reference_point = "varies for each region/habitat"))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  #
  # # return scores
  # return(scores_CP)
  return(data.frame(goal = 'CP',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = rep(NA, 16)))

}

TR <- function(layers) {

  # ## formula:
  # ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  # ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  # ##  Xtr = E * S
  #
  # ## read in layers
  # tr_data  <- full_join(
  #   layers$data[['tr_jobs_pct_tourism']] %>%
  #     select(-layer),
  #   layers$data[['tr_sustainability']] %>%
  #     select(-layer),
  #   by = c('region_id')) %>%
  #   filter(year <= scenario)
  #
  # tr_model <- tr_data %>%
  #   mutate(
  #     E   = Ep,
  #     S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
  #     Xtr = E * S ) %>%
  #   filter(year <= scenario & year > scenario - 5)
  # # five data years for trend calcs
  #
  # # regions with Travel Warnings
  # ### adjust the travel warning years...these always reflect the current year
  # ### but the other datasets will lag
  # if (exists('scenarios')) { ## if global scenarios
  #   scenario_year <- as.numeric(substring(scenario, 4,7))
  #   offset_years <- scenario_year - scenario
  #
  #   ## read in layers for regions with Travel Warnings
  #   rgn_travel_warnings <- layers$data[['tr_travelwarnings']] %>%
  #     select(region_id, year, multiplier) %>%
  #     mutate(year = year - offset_years)
  #
  #   ## incorporate Travel Warnings
  #   tr_model <- tr_model %>%
  #     left_join(rgn_travel_warnings, by = c('region_id', 'year')) %>%
  #     mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
  #     select(-multiplier)
  #
  # } ## end if (exists('scenarios'))
  #
  # ### Calculate status based on quantile reference (see function call for pct_ref)
  # tr_model <- tr_model %>%
  #   select(region_id, year, Xtr) %>%
  #   left_join(tr_model %>%
  #               group_by(year) %>%
  #               summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
  #             by = 'year') %>%
  #   mutate(
  #     Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) # rescale to qth percentile, cap at 1
  #
  # ## reference points
  # ref_point <- tr_model %>%
  #   filter(year == scenario) %>%
  #   select(Xtr_q) %>%
  #   unique()
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "TR", method = paste0('spatial: ', pct_ref, "th quantile"),
  #                    reference_point = ref_point$Xtr_q))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  #
  # adj_trend_year <- min(tr_model$year)
  #
  #
  # # calculate trend
  # tr_trend <- tr_model %>%
  #   filter(!is.na(Xtr_rq)) %>%
  #   arrange(year, region_id) %>%
  #   group_by(region_id) %>%
  #   do(mdl = lm(Xtr_rq ~ year, data=.),
  #      adjust_trend = .$Xtr_rq[.$year == adj_trend_year]) %>%
  #   summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
  #   ungroup() %>%
  #   mutate(trend = ifelse(trend>1, 1, trend)) %>%
  #   mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
  #   mutate(trend = round(trend, 4)) %>%
  #   select(region_id, score = trend) %>%
  #   mutate(dimension = "trend")
  #
  # # get status (as last year's value)
  # tr_status <- tr_model %>%
  #   arrange(year, region_id) %>%
  #   group_by(region_id) %>%
  #   summarize(
  #     dimension = 'status',
  #     score     = last(Xtr_rq) * 100)
  #
  # # bind status and trend by rows
  # tr_score <- bind_rows(tr_status, tr_trend) %>%
  #   mutate(goal = 'TR')
  #
  # if (conf$config$layer_region_labels=='rgn_global'){
  #   # assign NA for uninhabitated islands
  #   unpopulated = layers$data[['le_popn']] %>%
  #     group_by(region_id) %>%
  #     filter(count==0) %>%
  #     select(region_id)
  #   tr_score$score = ifelse(tr_score$region_id %in% unpopulated$region_id, NA, tr_score$score)
  # }
  #
  # # return final scores
  # scores_tr = tr_score %>%
  #   select(region_id=region_id, goal, dimension, score)
  #
  # return(scores_tr)
  return(data.frame(goal = 'TR',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = rep(NA, 16)))
}

LIV <- function(layers) {

  status_year <- layers$data$status_year
  data_year <- get_data_year('LIV', status_year, layers)
  year_span <- layers$data$year_span

  unempl_df <- layers$data[['liv_unemployment']] %>%
    select(-layer) %>%
    complete_years(year_span) %>%
    group_by(region_id) %>%
    arrange(region_id, year) %>%
    mutate(empl_rate = 1 - unemployment_rate,
           ref_pt = zoo::rollmean(empl_rate, k = 5, fill = NA, align = 'right')) %>%
  ### reference point is mean of past five years (incl current)
    ungroup()

  income_df <- layers$data[['liv_income']] %>%
    select(-layer) %>%
    complete_years(year_span) %>%
    mutate(ref_pt = max(median_income, na.rm = TRUE))
  ### reference point is max value across regions and years; make sure
  ### income is listed in equivalent dollars

  ### combine wages and jobs scores; calculate overall score
  liv_status <- unempl_df %>%
    mutate(jobs_score = empl_rate / ref_pt,
           jobs_score = ifelse(jobs_score > 1, 1, jobs_score)) %>%
    select(year, region_id, jobs_score) %>%
    left_join(income_df %>%
                mutate(wages_score = median_income / ref_pt,
                       wages_score = ifelse(wages_score > 1, 1, wages_score)) %>%
                select(year, region_id, wages_score),
              by = c('year', 'region_id')) %>%
    mutate(score = 100 * (jobs_score + wages_score) / 2,
           goal  = 'LIV',
           dimension = 'status') %>%
    select(year, region_id, score, goal, dimension)


  trend_years <- (data_year - 4) : data_year
  liv_trend   <- calc_trend(liv_status, trend_years)

  liv_scores <- liv_status %>%
    filter(year == data_year) %>%
    bind_rows(liv_trend) %>%
    select(region_id, goal, dimension, score)

  return(liv_scores)

}

ICO <- function(layers) {

  ### in goals.csv, allow status year to be NULL, so it can be reassigned
  ### for each iteration through calculate loop
  status_year <- layers$data$status_year
  data_year <- get_data_year('ICO', status_year, layers)

  ico_score <- layers$data[['ico_spp_risk_score']] %>%
    select(year, risk_score, sciname, iucn_sid, am_sid) %>%
    filter(!is.na(year)) %>%
    complete(year = full_seq(year, 1), nesting(sciname, iucn_sid, am_sid)) %>%
    group_by(sciname, iucn_sid, am_sid) %>%
    arrange(year) %>%
    fill(risk_score, .direction = 'up') %>%
    fill(risk_score, .direction = 'down') %>%
    ungroup()


  ico_rgns <- layers$data[['ico_spp_presence']] %>%
    select(iucn_sid, am_sid, region_id = rgn_id, sciname, comname)

  ### calculate base status across all years
  # STEP 1: take mean of subpopulation scores if necessary
  rgn_ico_status_spp <- ico_rgns %>%
    full_join(ico_score, by = c('sciname', 'iucn_sid', 'am_sid')) %>%
    group_by(region_id, sciname, comname, year) %>%
    summarize(spp_health = 1 - mean(risk_score, na.rm = TRUE)) %>%
    ungroup()

  ### identify spp with no score available (NE or DD)
  rgn_ico_no_score <- rgn_ico_status_spp %>%
    filter(is.nan(spp_health)) %>%
    select(region_id, sciname, comname)

  # STEP 2: take mean of populations within regions
  rgn_ico_status <- rgn_ico_status_spp %>%
    filter(!is.nan(spp_health)) %>%
    group_by(region_id, year) %>%
    summarize(score = mean(spp_health, na.rm = TRUE),
              n_spp = n()) %>%
    ungroup() %>%
    mutate('goal' = 'ICO')

  ### calculate trend for data_year
  trend_span       <- 10 ### trend based on 10 years of data, due to infrequency of IUCN assessments
  trend_yr_include <- c(data_year:(data_year - trend_span + 1))

  if(!all(trend_yr_include %in% rgn_ico_status$year)) {
    message('Calculating trend for ', data_year, ': status not available for ',
            paste(trend_yr_include[!trend_yr_include %in% rgn_ico_status$year], collapse = ', '))
  }

  rgn_ico_trend <- calc_trend(rgn_ico_status, trend_yr_include)

  ### format status for reporting
  rgn_ico_status <- rgn_ico_status %>%
    mutate(score = round(score * 100, 5)) %>%
    mutate(dimension = "status") %>%
    select(goal, year, region_id, score, dimension)

  ### write reference points
  write_ref_pts(goal   = "ICO",
                method = "scaled IUCN risk categories",
                ref_pt = NA)

  ### combine and return scores df
  scores_ico <- bind_rows(rgn_ico_status, rgn_ico_trend) %>%
    filter(year == data_year) %>%
    select(goal, dimension, region_id, score)

  ### For now, ICO trend is NA - not enough data to calculate trends
  scores_ico <- scores_ico %>%
    mutate(score = ifelse(dimension == 'trend', NA, score))

  return(scores_ico)

}

LSP <- function(layers, ref_pct_cmpa = 30, ref_pct_cp = 30) {

  status_year <- layers$data$status_year
  data_year <- get_data_year('LSP', status_year, layers)

  tot_area_rgn  <- layers$data[['lsp_tot_area_inland_ws']] %>%
    select(-layer) %>%
    left_join(x <- layers$data[['lsp_tot_area_offshore3nm']] %>%
                select(-layer)) %>%
    select(region_id = rgn_id, area_offshore = a_tot_3nm, area_inland = a_tot_inland) %>%
    distinct()

  prot_area_rgn  <- layers$data[['lsp_prot_area_offshore3nm']] %>%
    select(-layer) %>%
    left_join(x <- layers$data[['lsp_prot_area_inland_ws']] %>%
                select(-layer)) %>%
    rename(region_id = rgn_id, cp = a_prot_inland, cmpa = a_prot_3nm)

  ### get percent of total area that is protected for inland (cp) and
  ### offshore (cmpa) per year, and calculate status score
  rgn_yrs <- prot_area_rgn %>%
    full_join(tot_area_rgn, by = "region_id") %>%
    mutate(pct_cp    = pmin(cp   / area_inland   * 100, 100),
           pct_cmpa  = pmin(cmpa / area_offshore * 100, 100),
           status    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
    filter(!is.na(status))

  ### extract status based on specified year

  rgn_status <- rgn_yrs %>%
    select(year, region_id, score = status) %>%
    mutate(score = round(score * 100, 5),
           goal  = 'LSP',
           dimension = 'status')

  ### calculate trend
  trend_years <- (data_year - 4):data_year

  rgn_trend <-calc_trend(rgn_status, trend_years)

  ### reference points
  write_ref_pts(goal   = "LSP",
                method = paste0(ref_pct_cmpa, "% marine protected area; ",
                                ref_pct_cp, "% coastal protected area"),
                ref_pt = "varies by area of region's eez and 1 km inland")

  ### return scores
  scores_lsp <- rgn_status %>%
    filter(year == data_year) %>%
    bind_rows(rgn_trend) %>%
    select(goal, dimension, region_id, score)

  return(scores_lsp)

}

SP <- function(scores) {

  ### to calculate the four SP dimesions, average those dimensions for ICO and LSP
  sp_scores <- scores %>%
    filter(goal %in% c('ICO','LSP') &
             dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = 'SP') %>%
    select(region_id, goal, dimension, score)

  ### return all scores
  scores_sp <- bind_rows(scores, sp_scores)

  return(scores_sp)
}

CW <- function(layers) {

  status_year <- layers$data$status_year
  data_year <- get_data_year('CW', status_year, layers)
  year_span <- layers$data$year_span

  # # layers
  # lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash',
  #           'cw_chemical_trend', 'cw_nutrient_trend', 'cw_trash_trend', 'cw_pathogen_trend')
  #
  # d <-  SelectLayersData(layers, layers=lyrs)  %>%
  #   select(region_id = id_num, layer, value = val_num)
  #
  # ### function to calculate geometric mean:
  # geometric.mean2 <- function (x, na.rm = TRUE) {
  #   if (is.null(nrow(x))) {
  #     exp(mean(log(x), na.rm = TRUE))
  #   }
  #   else {
  #     exp(apply(log(x), 2, mean, na.rm = na.rm))
  #   }
  # }
  #
  #
  nutr_prs <- layers$data[['po_nutrient_3nm']] %>%
    complete_years(year_span) %>%
    rename(pressure = nutr_pressure)
  chem_prs <- layers$data[['po_chemical_3nm']] %>%
    complete_years(year_span) %>%
    rename(pressure = chem_pressure)
  trash_prs <- layers$data[['po_trash']] %>%
    rename(region_id = rgn_id) %>%
    left_join(nutr_prs %>% select(region_id, year),
              by = 'region_id') %>%
    complete_years(year_span) %>%
    rename(pressure = trash_pressure)

  patho_prs <- layers$data[['po_pathogens_closures']] %>%
    # filter(closure_type %in% c('biotoxins', 'sanitary closure')) %>%
    mutate(days_in_year = ifelse(lubridate::leap_year(year), 366, 365)) %>%
    group_by(rgn_id, year) %>%
    summarize(pressure = (sum(days_avg) / first(days_in_year)),
              layer = 'po_pathogens_closures') %>%
    ungroup() %>%
    complete_years(year_span)


  cw_pressure_df <- bind_rows(chem_prs, nutr_prs, trash_prs, patho_prs) %>%
    rename(component = layer)
  ### that last bit is because somewhere the layer dfs get a layer name column... ???

  cw_score_summary <- cw_pressure_df %>%
    filter(!is.na(pressure)) %>%
    mutate(component_score = 1 - pressure) %>%
    group_by(year, region_id) %>%
    summarize(n_components = n(),
              status    = prod(component_score)^(1/n_components), ### this finishes our geometric mean
              sources   = paste(component, collapse = ', '),
              pressures = paste(round(pressure, 4), collapse = ', ')) %>%
    ungroup()

  rgn_status <- cw_score_summary %>%
    select(year, region_id, score = status) %>%
    mutate(score = round(score * 100, 5),
           goal  = 'CW',
           dimension = 'status')

  ### calculate trend
  trend_years <- (data_year - 4):data_year

  rgn_trend <-calc_trend(rgn_status, trend_years)

  ### reference points
  write_ref_pts(goal   = "CW",
                method = 'Geometric mean of (1 - pressure); 0 = any component at max pressure, 1 = all components at zero pressure',
                ref_pt = NA)

  ### return scores
  scores_cw <- rgn_status %>%
    filter(year == data_year) %>%
    bind_rows(rgn_trend) %>%
    select(goal, dimension, region_id, score)

  return(scores_cw)
}

HAB <- function(layers) {
  ### Salt marsh based on extent from 30-meter rasters
  ### EBSA, soft bottom habitats based on trawl pressures
  ### Coastal forests excluded

  status_year <- layers$data$status_year
  data_year <- get_data_year('HAB', status_year, layers)
  year_span <- layers$data$year_span

  ### get the data:
  ebsa_health <- layers$data[['hab_ebsa_health']] %>%
    mutate(hab    = 'ebsa',
           status = 1 - trawled_area / total_ebsa_area) %>%
    complete_years(year_span)

  sb_health   <- layers$data[['hab_sb_health']] %>%
    mutate(hab    = 'soft_btm',
           status = 1 - mean_hr_area / max_hr_area) %>%
    complete_years(year_span)

  sm_health   <- layers$data[['hab_sm_health']] %>%
    group_by(rgn_id) %>%
    arrange(rgn_id, year) %>%
    mutate(hab    = 'salt_marsh',
           ref_pt = first(sm_area_km2),
           ref_yr = first(year),
           status = sm_area_km2 / ref_pt,
           status = ifelse(status > 1, 1, status)) %>%
    complete_years(year_span)

  # cf_health   <- layers$data[['hab_cf_health']] %>%
  #   group_by(rgn_id) %>%
  #   arrange(rgn_id, year) %>%
  #   mutate(hab    = 'coastal_forest',
  #          ref_pt = first(cf_area_km2),
  #          ref_yr = first(year),
  #          status = cf_area_km2 / ref_pt,
  #          status = ifelse(status > 1, 1, status)) %>%
  #   complete_years(year_span)

  hab_status <- bind_rows(ebsa_health, sb_health, sm_health) %>% # cf_health) %>%
    select(region_id, year, status, hab) %>%
    group_by(region_id, year) %>%
    summarize(score = mean(status)) %>%
    ungroup() %>%
    mutate(goal      = 'HAB',
           dimension = 'status',
           score = ifelse(score > 1, 1, score) * 100)

  ## reference points
  write_ref_pts(goal   = "HAB",
                method = "XXXXXXXX",
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year
  trend_years <- (data_year - 4) : data_year
  hab_trend   <- calc_trend(hab_status, trend_years)

  hab_scores <- hab_status %>%
    filter(year == data_year) %>%
    bind_rows(hab_trend) %>%
    select(region_id, goal, dimension, score)

  return(hab_scores)

}

SPP <- function(layers) {

  status_year <- layers$data$status_year
  data_year <- get_data_year('SPP', status_year, layers)

  spp_range_by_rgn <- layers$data[['spp_range_areas']] %>%
    select(region_id = rgn_id, spp_pct_area, iucn_sid, am_sid, spatial_source)
  spp_risk_by_yr   <- layers$data[['spp_risk_scores']] %>%
    select(year, iucn_sid, am_sid, risk_score, risk_source)
  spp_pop_trends   <- layers$data[['spp_pop_trends']] %>%
    select(iucn_sid, am_sid, pop_trend)


  ## reference points
  write_ref_pts(goal   = "SPP",
                method = "Average of IUCN risk categories, scaled to historic extinction",
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year.
  ### Linear trend calculated only for species with two or more assessments,
  ### and not using BC-specific scores; otherwise use the "population trend"
  ### method of calculation using these values:
  pop_trend_score_lookup <- c('increasing' = 0.025, 'decreasing' = -0.025, 'stable' = 0)

  ### For species with more than one assessment, calculate linear model trend
  spp_lm_trends <- spp_risk_by_yr %>%
    group_by(iucn_sid, am_sid) %>%
    filter(n() >= 2 & risk_source == 'iucn') %>%
    mutate(spp_health = 1 - risk_score) %>%
    do(trend_lm = lm(spp_health ~ year, data = .)[['coefficients']][['year']]) %>%
    mutate(trend_lm = trend_lm / .75, ### scale to species calculation
           trend_lm = round(trend_lm, 5))


  ### Build overall dataframe with species risk and trend
  spp_risk_expanded <- spp_risk_by_yr %>%
    filter(!is.na(risk_score)) %>%
    mutate(year = ifelse(is.na(year), data_year, year)) %>%
    group_by(iucn_sid, am_sid) %>%
    complete(year = layers$data$year_span) %>%
    arrange(year) %>%
    fill(risk_score, risk_source, .direction = 'down') %>%
    fill(risk_score, risk_source, .direction = 'up') %>%
    ungroup()

  spp_df <- spp_risk_expanded %>%
    inner_join(spp_range_by_rgn, by = c('iucn_sid', 'am_sid')) %>%
    left_join(spp_lm_trends, by = c('iucn_sid', 'am_sid')) %>%
    left_join(spp_pop_trends, by = c('iucn_sid', 'am_sid')) %>%
    mutate(pop_trend_score = pop_trend_score_lookup[pop_trend]) %>%
    mutate(trend_score = ifelse(!is.na(trend_lm), trend_lm, NA),
           trend_score = ifelse(is.na(trend_score) & !is.na(pop_trend_score), pop_trend_score, trend_score),
           trend_score = ifelse(risk_source != 'iucn', NA, trend_score), ### NA trend for BC-specific scores
           trend_score = ifelse(risk_score == 1, NA, trend_score)) %>% ### NA trend for extinct species
    filter(year == data_year) %>%
    select(region_id, spp_pct_area, iucn_sid, am_sid, risk_score, trend_score)


  spp_status <- spp_df %>%
    group_by(region_id) %>%
    summarize(mean_risk_score  = sum(risk_score * spp_pct_area) / n()) %>%
    ungroup() %>%
    mutate(score = 100 * (.75 - mean_risk_score) / .75,
           goal   = 'SPP',
           dimension = 'status')

  spp_trend <- spp_df %>%
    group_by(region_id) %>%
    filter(!is.na(trend_score)) %>%
    summarize(score = sum(trend_score * spp_pct_area) / n()) %>%
    ungroup() %>%
    ### Normalize by status to get proportional annual change (note,
    ### since trends are tiny, assume status ten years ago is nearly the same
    ### as status now; so simply use current status as normalizing factor):
    left_join(spp_status %>% rename(status = score), by = 'region_id') %>%
    mutate(score = score / (status / 100)) %>%
    mutate(goal   = 'SPP',
           dimension = 'trend')


  spp_scores <- spp_status %>%
    bind_rows(spp_trend) %>%
    select(region_id, goal, dimension, score)

  return(spp_scores)
}

BD <- function(scores) {

  bd_scores <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame() %>%
    select(region_id, goal, dimension, score)

  # return all scores
  return(rbind(scores, bd_scores))

}

