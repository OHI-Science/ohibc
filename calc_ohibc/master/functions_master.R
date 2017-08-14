
FIS <- function(layers) {


  ##### Gather parameters and layers #####
  ### * ram_b_bmsy, ram_f_fmsy, ram_catch
  ### * rgn_stock_wt_uniform, _saup, _dfo
  ### * ram_dfo_saup_lookup.csv

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  ram_b_bmsy        <- layers$data[['fis_ram_b_bmsy']] %>%
    rename(b_bmsy = value) %>%
    group_by(stock_id) %>%
    complete(year)
  ram_f_fmsy        <- layers$data[['fis_ram_f_fmsy']] %>%
    rename(f_fmsy = value)
  ram_catch         <- layers$data[['fis_ram_catch']] %>%
    rename(catch = value)
  rgn_stock_area  <- layers$data[['fis_stock_area']] %>%
    rename(region_id = rgn_id, stock_id = stockid)

  ### These parameters are based on conversation with Ian Perry, Karen Hunter,
  ### and Karin Bodtker on May 24 2017.
  b_bmsy_underexploit_penalty <- 0.25
  b_bmsy_underexploit_thresh  <- 3.00
  f_fmsy_underfishing_penalty <- 0.25
  f_fmsy_overfishing_thresh   <- 2.00

  ### Apply rolling mean to F/Fmsy
  ram_f_fmsy <- ram_f_fmsy %>%
    mutate(f_fmsy_raw = f_fmsy) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    # filter(!is.na(f_fmsy)) %>%
    mutate(f_fmsy = zoo::rollmean(f_fmsy_raw, k = 4, align = 'right', fill = NA)) %>%
    ungroup()

  stock_status_layers <- ram_b_bmsy %>%
    full_join(ram_f_fmsy, by = c('year', 'stock_id'))

  ########################################################.
  ##### run each fishery through the Kobe plot calcs #####
  ########################################################.
  ### * ram_b_bmsy, ram_f_fmsy


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
             fPrime = ifelse(is.na(fPrime), 0, fPrime), ### fill zeros everywhere unscored
             fPrime = ifelse(is.na(f_fmsy), NA, fPrime) ### but if no f_fmsy, reset to NA
      )
    return(fish_stat_df)
  }

  stock_status_df <- stock_status_layers %>%
    rescale_bprime_crit(bmax     = b_bmsy_underexploit_thresh,
                        bmax_val = b_bmsy_underexploit_penalty) %>%
    rescale_fprime_crit(fmax     = f_fmsy_overfishing_thresh,
                        fmin_val = f_fmsy_underfishing_penalty) %>%
    mutate(x_prod = ifelse(!is.na(fPrime), (fPrime * bPrime), bPrime),
           basis  = ifelse(!is.na(fPrime), 'F/Fmsy, B/Bmsy', 'B/Bmsy only')) %>%
    dplyr::select(year, stock_id,
                  score = x_prod,
                  basis,
                  bPrime, fPrime,
                  b_bmsy, f_fmsy)  %>%
    group_by(stock_id) %>%
    complete(year = status_yr_span) %>%
    arrange(year) %>%
    fill(score, basis, .direction = 'down') %>%
    ungroup()

  ##############################################################.
  ##### calculate distribution of fishery catch to regions #####
  ##############################################################.

  ### calculate weights within each region by regional catch
  catch_df <- ram_catch %>%
    left_join(rgn_stock_area, by = 'stock_id') %>%
    group_by(region_id, stock_id) %>%
    complete(year = status_yr_span) %>%
    arrange(year) %>%
    fill(catch, a_prop, .direction = 'down') %>%
    ungroup() %>%
    mutate(rgn_catch = catch * a_prop,
           rgn_catch = ifelse(is.na(rgn_catch), 0, rgn_catch))

  stock_score_df <- stock_status_df %>%
    filter(!is.na(score)) %>%
    group_by(stock_id) %>%
    arrange(stock_id, year) %>%
    fill(score, .direction = c('down')) %>%
    fill(score, .direction = c('up')) %>%
    ungroup() %>%
    full_join(catch_df, by = c('stock_id', 'year')) %>%
    select(region_id, year, stock_id, score, rgn_catch, basis) %>%
    filter(year %in% status_yr_span & !is.na(region_id))

  if(data_year == max(status_yr_span)) {
    ### note, this contains all years, but only write it once
    write_csv(stock_score_df, '~/github/ohibc/prep/fis/v2017/summary/fis_from_functions.csv')
  }

  ### plotting fishery catch weighting by region
  # stock_plot_df <- stock_score_df %>%
  #   group_by(region_id, year) %>%
  #   mutate(total_catch = sum(rgn_catch),
  #          rgn_catch_pct = rgn_catch / total_catch,
  #          total_score = sum(score * rgn_catch) / total_catch) %>%
  #   ungroup() %>%
  #   left_join(get_rgn_names(), by = c('region_id' = 'rgn_id'))
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
    filter(!is.na(score)) %>%
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

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  fis_trend   <- calc_trend(fis_status, trend_years)

  fis_scores <- fis_status %>%
    filter(year == data_year) %>%
    filter(!is.na(region_id)) %>%
    bind_rows(fis_trend) %>%
    select(goal, dimension, region_id, score)

  message('returning from FIS')

  return(fis_scores)

}

MAR <- function(layers) {

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  mar_harvest   <- layers$data[['mar_harvest_tonnes']] %>%
    select(-layer)
  mar_areas     <- layers$data[['mar_tenure_areas']] %>%
    select(-layer)
  mar_potential <- layers$data[['mar_potential']] %>%
    select(-layer)

  mar_all <- mar_areas %>%
    left_join(mar_potential, by = c('rgn_id', 'aq_type')) %>%
    left_join(mar_harvest, by = c('rgn_id', 'aq_type')) %>%
    mutate(ref_pt = area_km2 * potential) %>%
    left_join(get_rgn_names(), by = 'rgn_id') %>%
    filter(source == 'dfo')

  mar_f_df <- mar_all %>%
    filter(aq_type == 'finfish') %>%
    select(year, region_id = rgn_id, harvest_tonnes, ref, ref_pt, aq_type)

  mar_f_score <- mar_f_df %>%
    spread(key = ref, value = ref_pt) %>%
    mutate(f_score = harvest_tonnes / lowR_prod,
           f_score = ifelse(harvest_tonnes > lowR_prod, 1, f_score),
           f_score = ifelse(harvest_tonnes > highR_prod, (1 - (harvest_tonnes - highR_prod) / (max_prod - highR_prod) ), f_score),
           f_score = ifelse(harvest_tonnes > max_prod, 0, f_score)) %>%
    select(region_id, year, score = f_score, harvest_tonnes, aq_type) %>%
    filter(!is.na(score)) %>%
    complete_years(status_yr_span, method = 'zero')


  mar_b_df <- mar_all %>%
    filter(aq_type == 'shellfish') %>%
    select(year, region_id = rgn_id, ref, harvest_tonnes, ref_pt, aq_type)

  mar_b_score <- mar_b_df %>%
    spread(key = ref, value = ref_pt) %>%
    mutate(b_score = harvest_tonnes / lowR_prod,
           b_score = ifelse(harvest_tonnes > lowR_prod, 1, b_score)) %>%
    select(region_id, year, score = b_score, harvest_tonnes, aq_type) %>%
    filter(!is.na(score)) %>%
    complete_years(status_yr_span, method = 'zero')

  mar_status <- bind_rows(mar_f_score, mar_b_score) %>%
    group_by(region_id, year) %>%
    mutate() %>%
    summarize(score_wt    = sum(score * harvest_tonnes, na.rm = TRUE),
              harvest_tot = sum(harvest_tonnes, na.rm = TRUE),
              score = ifelse(harvest_tot > 0, score_wt / harvest_tot, 0),
              score = round(100 * score, 5)) %>%
    ungroup() %>%
    select(region_id, year, score) %>%
    mutate(goal = 'MAR',
           dimension = 'status')

  ## reference points
  write_ref_pts(goal   = "MAR",
                method = "XXXXXXXX",
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  mar_trend   <- calc_trend(mar_status, trend_years)

  mar_scores <- mar_status %>%
    filter(!is.na(region_id)) %>%
    filter(year == data_year) %>%
    bind_rows(mar_trend) %>%
    select(region_id, goal, dimension, score)

  return(mar_scores)

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

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  ### get the data:
  closures <- layers$data[['ao_closures']]
  licenses <- layers$data[['ao_licenses']]
  # licenses_ref <- layers$data[['ao_licenses_fn_pop']] ### consider FN pop % as ref point?
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
    complete_years(status_yr_span) %>%
    mutate(days_in_year = ifelse(lubridate::leap_year(year), 366, 365),
           status = 1 - (days_avg / days_in_year),
           component = 'shellfish_closures') %>%
    select(year, region_id, status, component)

  ### Licenses:
  ### * prop of licenses allocated to FNs, with some level (25%?) as target?
  ### * no net loss vs some rolling average?
  ### * Reference point idea: % FN licenses is equal to FN pop, with some minimum
  ###   threshold (say 10-15%) to account for high non-FN pops in SoG/WCVI regions
  license_ref_pt <- max(licenses$pct_fn, na.rm = TRUE)
  license_status <- licenses %>%
    complete_years(status_yr_span) %>%
    mutate(status = pct_fn / license_ref_pt,
           status = ifelse(status > 1, 1, status),
           component = 'first_nations_licenses') %>%
    select(year, region_id, status, component)
  # ggplot(licenses_target, aes(x = year, y = status, group = region_id, color = region_id)) + geom_line()

  ### Spawn habitat index
  ### * SHI vs historical reference point of mean SHI from 1940-1960.
  shi_hist_ref <- shi %>%
    complete_years(status_yr_span) %>%
    filter(year %in% c(1940:1960)) %>%
    group_by(region_id) %>%
    summarize(shi_ref_pt = mean(shi_tot, na.rm = TRUE)) %>%
    ungroup()
  shi_status <- shi %>%
    complete_years(status_yr_span) %>%
    left_join(shi_hist_ref, by = 'region_id') %>%
    mutate(shi_3yr_mean = (shi_tot + lag(shi_tot, 1) + lag(shi_tot, 2)) / 3,
           status = shi_3yr_mean / shi_ref_pt,
           status = ifelse(status > 1, 1, status),
           component = 'herring_spawn_hab_index') %>%
    select(year, region_id, status, component)

  ### Salmon
  ### * dummy for now
  salmon_status <- salmon %>%
    complete_years(status_yr_span) %>%
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

  if(data_year == max(status_yr_span)) {
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

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

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
    complete_years(status_yr_span) %>%
    group_by(region_id, hab) %>%
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
  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

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
    complete_years(status_yr_span) %>%
    group_by(region_id, hab) %>%
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

}

TR <- function(layers) {

  ### TR model includes Tourism Center visits and Park visits; these are
  ### summed per region and adjusted by changes in the province-wide
  ### visitors to cancel system-wide shifts in tourism (e.g. economics)

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  vc_visits     <- layers$data[['tr_vis_ctr_visits']] %>%
    select(-layer)
  vc_visits_all <- layers$data[['tr_vis_ctr_visits_all']] %>%
    rename(visits_all = visits) %>%
    select(-layer)
  park_visits   <- layers$data[['tr_park_visits']] %>%
    select(-layer)
  park_visits_all <- layers$data[['tr_park_visits_all']] %>%
    rename(visits_all = visits) %>%
    select(-layer)

  ### Sum visitor center (or park) visits within each region, then adjust by
  ### province-wide totals (by dividing by normalized province total visits).
  vc_visits_adj <- vc_visits %>%
    group_by(rgn_id, year) %>%
    summarize(visits = sum(visits)) %>%
    ungroup() %>%
    left_join(vc_visits_all, by = 'year') %>%
    mutate(visits_all_norm = visits_all / max(visits_all, na.rm = TRUE),
           visits_adj = visits / visits_all_norm)


  park_visits_adj <- park_visits %>%
    group_by(rgn_id, year) %>%
    summarize(visits = sum(visits_wt)) %>%
    ungroup() %>%
    left_join(park_visits_all, by = 'year') %>%
    mutate(visits_all_norm = visits_all / max(visits_all, na.rm = TRUE),
           visits_adj = visits / visits_all_norm)

  ### Create reference point by looking at rolling mean over prior five years.
  ### This also back- and forward-fills using LOCF (or FOCB) to complete
  ### year sequence, which affects reference points.
  vc_visits_ref <- vc_visits_adj %>%
    complete_years(status_yr_span) %>%
    group_by(region_id) %>%
    arrange(region_id, year) %>%
    mutate(ref_pt = zoo::rollmean(visits_adj, k = 5, fill = NA, align = 'right')) %>%
    ### reference point is mean of past five years (incl current)
    fill(ref_pt, .direction = 'up') %>%
    ungroup()

  park_visits_ref <- park_visits_adj %>%
    complete_years(status_yr_span) %>%
    group_by(region_id) %>%
    arrange(region_id, year) %>%
    mutate(ref_pt = zoo::rollmean(visits_adj, k = 5, fill = NA, align = 'right')) %>%
    ### reference point is mean of past five years (incl current)
    fill(ref_pt, .direction = 'up') %>%
    ungroup()

  ### Calculate scores for vis ctrs and parks

  vc_scores <- vc_visits_ref %>%
    mutate(vc_score = visits_adj / ref_pt,
           vc_score = ifelse(vc_score > 1, 1, vc_score)) %>%
    select(region_id, year, vc_score)

  park_scores <- park_visits_ref %>%
    mutate(park_score = visits_adj / ref_pt,
           park_score = ifelse(park_score > 1, 1, park_score)) %>%
    select(region_id, year, park_score)

  tr_status <- vc_scores %>%
    left_join(park_scores, by = c('year', 'region_id')) %>%
    rowwise() %>%
    mutate(score = mean(c(vc_score, park_score), na.rm = TRUE),
           score = round(100 * score, 5)) %>%
    ungroup() %>%
    select(year, region_id, score) %>%
    mutate(goal = 'TR',
           dimension = 'status') %>%
    filter(!is.nan(score))

  trend_years <- (data_year - 4) : data_year
  tr_trend <- calc_trend(tr_status, years = trend_years)

  tr_scores <- tr_status %>%
    filter(year == data_year) %>%
    bind_rows(tr_trend) %>%
    group_by(year, dimension, goal) %>%
    complete(region_id = 1:8) %>%
    ungroup()


  return(tr_scores)

}

LIV <- function(layers) {

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  unempl_df <- layers$data[['liv_unemployment']] %>%
    select(-layer) %>%
    complete_years(status_yr_span) %>%
    group_by(region_id) %>%
    arrange(region_id, year) %>%
    mutate(empl_rate = 1 - unemployment_rate,
           ref_pt = zoo::rollmean(empl_rate, k = 5, fill = NA, align = 'right')) %>%
    ### reference point is mean of past five years (incl current)
    ungroup()

  income_df <- layers$data[['liv_income']] %>%
    select(-layer) %>%
    complete_years(status_yr_span) %>%
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
  status_year <- layers$data$scenario_year
  data_year   <- status_year

  ico_risk_by_year <- layers$data[['ico_spp_risk_score']] %>%
    select(-layer)

  ico_trend <- layers$data[['ico_spp_trend']] %>%
    select(-layer)

  ico_rgns <- layers$data[['ico_spp_presence']] %>%
    select(-layer, region_id = rgn_id)

  ### calculate base status across all years
  # STEP 1: take mean of subpopulation scores if necessary

  ico_risk <- ico_risk_by_year %>%
    filter(!is.na(year)) %>%
    complete(year = full_seq(year, 1), nesting(sciname, iucn_sid, am_sid, comname)) %>%
    group_by(sciname, iucn_sid, am_sid, comname) %>%
    arrange(year) %>%
    fill(risk_score, risk_source, .direction = 'up') %>%
    fill(risk_score, risk_source, .direction = 'down') %>%
    ungroup()

  rgn_ico_spp <- ico_rgns %>%
    full_join(ico_risk, by = c('sciname', 'iucn_sid', 'am_sid', 'comname')) %>%
    group_by(region_id, sciname, comname, year) %>%
    mutate(spp_health = 1 - mean(risk_score, na.rm = TRUE)) %>%
    ungroup()

  ### identify spp with no score available (NE or DD)
  rgn_ico_no_score <- rgn_ico_spp %>%
    filter(is.nan(spp_health)) %>%
    select(region_id, sciname, comname)

  ### Linear trend calculated only for species with two or more assessments,
  ### and not using BC-specific scores; otherwise use the COSEWIC trend or
  ### "population trend" method of calculation using these values:
  ### For species with more than one assessment, calculate linear model trend
  ico_lm_trends <- ico_risk_by_year %>%
    group_by(iucn_sid, am_sid) %>%
    filter(n() >= 2 & risk_source == 'iucn') %>%
    mutate(spp_health = 1 - risk_score) %>%
    do(trend_lm = lm(spp_health ~ year, data = .)[['coefficients']]['year']) %>%
    mutate(trend_lm = 5 * round(trend_lm, 5))

  pop_trend_score_lookup <- c('increasing' = 0.025, 'decreasing' = -0.025, 'stable' = 0)

  rgn_ico_spp_scores <- rgn_ico_spp %>%
    left_join(ico_lm_trends, by = c('am_sid', 'iucn_sid')) %>%
    left_join(ico_trend, by = c('am_sid', 'iucn_sid', 'sciname', 'comname')) %>%
    mutate(pop_trend_score = pop_trend_score_lookup[iucn_pop_trend]) %>%
    mutate(trend_score = ifelse(!is.na(trend_lm), trend_lm, NA),
           trend_score = ifelse(is.na(trend_score) & !is.na(pop_trend_score), pop_trend_score, trend_score),
           trend_score = ifelse(risk_source != 'iucn', cosewic_trend, trend_score), ### COSEWIC or NA trend for BC-specific scores
           trend_score = ifelse(risk_score == 1, NA, trend_score)) %>% ### NA trend for extinct species
    filter(year == data_year) %>%
    select(region_id, iucn_sid, am_sid, risk_score, trend_score)

  # STEP 2: take mean of populations within regions
  rgn_ico_status <- rgn_ico_spp_scores %>%
    mutate(spp_health = 1 - risk_score) %>%
    filter(!is.nan(spp_health)) %>%
    group_by(region_id) %>%
    summarize(score = mean(spp_health, na.rm = TRUE),
              n_spp = n()) %>%
    ungroup() %>%
    mutate(goal      = 'ICO',
           score     = round(score * 100, 5),
           dimension = 'status') %>%
    select(goal, region_id, score, dimension)

  ### calculate trend for data_year
  rgn_ico_trend <- rgn_ico_spp_scores %>%
    filter(!is.nan(trend_score)) %>%
    group_by(region_id) %>%
    summarize(score = mean(trend_score, na.rm = TRUE),
              n_spp = n()) %>%
    ungroup() %>%
    mutate(goal      = 'ICO',
           score     = round(score, 5),
           dimension = 'trend') %>%
    select(goal, region_id, score, dimension)


  ### write reference points
  write_ref_pts(goal   = "ICO",
                method = "scaled IUCN risk categories",
                ref_pt = NA)

  ### combine and return scores df
  scores_ico <- bind_rows(rgn_ico_status, rgn_ico_trend) %>%
    select(goal, dimension, region_id, score)


  return(scores_ico)

}

LSP <- function(layers, ref_pct_cmpa = 30, ref_pct_cp = 30) {

  status_year <- layers$data$scenario_year
  data_year   <- status_year

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

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span


  nutr_prs <- layers$data[['po_nutrients_3nm']] %>%
    complete_years(status_yr_span) %>%
    rename(pressure = nutr_pressure)
  chem_prs <- layers$data[['po_chemicals_3nm']] %>%
    complete_years(status_yr_span) %>%
    rename(pressure = chem_pressure)
  trash_prs <- layers$data[['po_trash']] %>%
    rename(region_id = rgn_id) %>%
    left_join(nutr_prs %>% select(region_id, year),
              by = 'region_id') %>%
    complete_years(status_yr_span) %>%
    rename(pressure = trash_pressure)

  patho_prs <- layers$data[['po_pathogens']] %>%
    complete_years(status_yr_span) %>%
    rename(pressure = path_pressure)


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

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  ### get the data:
  ebsa_health <- layers$data[['hab_ebsa_health']] %>%
    mutate(hab    = 'ebsa',
           status = 1 - trawled_area / total_ebsa_area) %>%
    complete_years(status_yr_span)

  sb_health   <- layers$data[['hab_sb_health']] %>%
    mutate(hab    = 'soft_btm',
           status = 1 - mean_hr_area / max_hr_area) %>%
    complete_years(status_yr_span)

  sm_health   <- layers$data[['hab_sm_health']] %>%
    group_by(rgn_id) %>%
    arrange(rgn_id, year) %>%
    mutate(hab    = 'salt_marsh',
           ref_pt = first(sm_area_km2),
           ref_yr = first(year),
           status = sm_area_km2 / ref_pt,
           status = ifelse(status > 1, 1, status)) %>%
    complete_years(status_yr_span)

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

  status_year <- layers$data$scenario_year
  data_year   <- status_year

  spp_range_by_rgn <- layers$data[['spp_range_areas']] %>%
    select(region_id = rgn_id, spp_pct_area, iucn_sid, am_sid, spatial_source)
  spp_risk_by_yr   <- layers$data[['spp_risk_scores']] %>%
    select(year, iucn_sid, am_sid, risk_score, risk_source)
  spp_pop_trends   <- layers$data[['spp_pop_trends']] %>%
    select(iucn_sid, am_sid, iucn_pop_trend, cosewic_trend)


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
    do(trend_lm = lm(spp_health ~ year, data = .)[['coefficients']]['year']) %>%
    mutate(trend_lm = trend_lm / .75, ### scale to species calculation
           trend_lm = 5 * round(trend_lm, 5)) ### multiply by 5 for likely future state


  ### Build overall dataframe with species risk and trend
  spp_risk_expanded <- spp_risk_by_yr %>%
    filter(!is.na(risk_score)) %>%
    mutate(year = ifelse(is.na(year), data_year, year)) %>%
    group_by(iucn_sid, am_sid) %>%
    complete(year = layers$data$status_year_span) %>%
    arrange(year) %>%
    fill(risk_score, risk_source, .direction = 'down') %>%
    fill(risk_score, risk_source, .direction = 'up') %>%
    ungroup()

  spp_df <- spp_risk_expanded %>%
    inner_join(spp_range_by_rgn, by = c('iucn_sid', 'am_sid')) %>%
    left_join(spp_lm_trends, by = c('iucn_sid', 'am_sid')) %>%
    left_join(spp_pop_trends, by = c('iucn_sid', 'am_sid')) %>%
    mutate(pop_trend_score = pop_trend_score_lookup[iucn_pop_trend]) %>%
    mutate(trend_score = ifelse(!is.na(trend_lm), trend_lm, NA),
           trend_score = ifelse(is.na(trend_score) & !is.na(pop_trend_score), pop_trend_score, trend_score),
           trend_score = ifelse(risk_source != 'iucn', cosewic_trend, trend_score), ### COSEWIC or NA trend for BC-specific scores
           trend_score = ifelse(risk_score == 1, NA, trend_score)) %>% ### NA trend for extinct species
    filter(year == data_year) %>%
    select(region_id, spp_pct_area, iucn_sid, am_sid, risk_score, trend_score)


  spp_status <- spp_df %>%
    group_by(region_id) %>%
    filter(!is.na(risk_score)) %>%
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

