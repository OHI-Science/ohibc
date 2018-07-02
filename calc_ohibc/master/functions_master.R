
FIS <- function(layers) {
  ##### Gather parameters and layers #####
  ### * ram_b_bmsy, ram_f_fmsy, ram_catch
  ### * rgn_stock_wt_uniform, _saup, _dfo
  ### * ram_dfo_saup_lookup.csv

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  ram_b_bmsy    <- layers$data[['fis_ram_b_bmsy']] %>%
    select(stockid = stock_id, year, b_bmsy = value)
  ram_f_fmsy    <- layers$data[['fis_ram_f_fmsy']] %>%
    select(stockid = stock_id, year, f_fmsy = value)
  dfo_catch     <- layers$data[['fis_dfo_catch']] %>%
    select(rgn_id, stockid, year, rgn_ass_catch_prop)
  rgn_catch_sum <- layers$data[['fis_rgn_catch_summary']] %>%
    select(rgn_id, year, ass_catch_prop)

  ### These parameters are based on conversation with Ian Perry, Karen Hunter,
  ### and Karin Bodtker on May 24 2017.
  b_bmsy_underexploit_penalty <- 0.25
  b_bmsy_underexploit_thresh  <- 3.00
  f_fmsy_underfishing_penalty <- 0.25
  f_fmsy_overfishing_thresh   <- 2.00

  ### Apply rolling mean to F/Fmsy
  ram_f_fmsy <- ram_f_fmsy %>%
    mutate(f_fmsy_raw = f_fmsy) %>%
    arrange(stockid, year) %>%
    group_by(stockid) %>%
    # filter(!is.na(f_fmsy)) %>%
    mutate(f_fmsy = zoo::rollmean(f_fmsy_raw, k = 4, align = 'right', fill = NA)) %>%
    ungroup()

  stock_status_layers <- ram_b_bmsy %>%
    full_join(ram_f_fmsy, by = c('year', 'stockid'))

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
    ### this is used to create a 'virtual' B/Bmsy max where score drops
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
    underfishing_th <- 0.66; overfishing_th  <- 1.2
      ### changed underfishing to 0.66 under the 1/3 for the birds principle

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
           basis  = ifelse(!is.na(fPrime), 'F_Fmsy, B_Bmsy', 'B_Bmsy only')) %>%
    dplyr::select(year, stockid,
                  score = x_prod,
                  basis,
                  bPrime, fPrime,
                  b_bmsy, f_fmsy)  %>%
    group_by(stockid) %>%
    filter(year >= 1996) %>%
    complete_years(status_yr_span, method = 'carry', dir = 'forward') %>%
    ungroup()

  ### joing stock status data with dfo catch data then weighting stock scores with their proportional catch
  ### this is only done for assessed stocks. We incorporate a penalty for unassessed stocks later

  stock_score_catch <- stock_status_df %>%
    left_join(dfo_catch, by = c("stockid", "year")) %>%
    filter(!is.na(score) & !is.na(rgn_id)) %>%    ### remove rows with no stock score or rgn_id
      ### write_csv("output/stock_scores.csv") %>%  ##not sure if we want this?
    mutate(score_weighted = score * rgn_ass_catch_prop * 100) %>%
    group_by(year, rgn_id) %>%
    summarize(status_ass = sum(score_weighted)) %>%
      ### status_ass is the status when using assessed species weighted by their catch proportion
    ungroup()

  if(data_year == max(status_yr_span)) {
    ### note, this contains all years, but only write it once
    write_csv(stock_score_catch, '~/github/ohibc/prep/fis/v2017/summary/fis_from_functions.csv')  ## JA: i'm no longer sure we want to be saving this?
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
  #     geom_area(aes(group = stockid, fill = stockid)) +
  #     labs(title = first(rgn_plot_df$rgn_name),
  #          fill  = 'Stock ID',
  #          y     = 'Proportion of catch')
  #   print(rgn_plot)
  # }

  ### calculate FIS status
  fis_status <- stock_score_catch %>%
    left_join(rgn_catch_sum, by = c('year', 'rgn_id')) %>%
    mutate(score = (status_ass + (status_ass * ass_catch_prop))/2) %>%
             ### multiply the status by the % of catch assessed (this acts as a penalty for unassessed catch)
    select(year, rgn_id, score) %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span, method = 'none', dir = 'forward') %>%
    ungroup() %>%
    mutate(goal = 'FIS',
           dimension = 'status')


  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  fis_trend   <- calc_trend(fis_status, trend_years)

  fis_scores <- fis_status %>%
    filter(year == data_year) %>%
    filter(!is.na(region_id)) %>%
    bind_rows(fis_trend) %>%
    select(goal, dimension, region_id, score)

  # message('returning from FIS')

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
           f_score = ifelse(harvest_tonnes > lowR_prod, 1, f_score)) %>%
    select(region_id, year, score = f_score, harvest_tonnes, aq_type) %>%
    filter(!is.na(score)) %>%
    group_by(region_id) %>%
    complete_rgn_years(status_yr_span, method = 'none') %>%
    ungroup()


  mar_b_df <- mar_all %>%
    filter(aq_type == 'shellfish') %>%
    select(year, region_id = rgn_id, ref, harvest_tonnes, ref_pt, aq_type)

  mar_b_score <- mar_b_df %>%
    spread(key = ref, value = ref_pt) %>%
    mutate(b_score = harvest_tonnes / lowR_prod,
           b_score = ifelse(harvest_tonnes > lowR_prod, 1, b_score)) %>%
    select(region_id, year, score = b_score, harvest_tonnes, aq_type) %>%
    filter(!is.na(score)) %>%
    group_by(region_id) %>%
    complete_rgn_years(status_yr_span, method = 'none') %>%
    ungroup()

  mar_status <- bind_rows(mar_f_score, mar_b_score) %>%
    group_by(region_id, year) %>%
    summarize(score_wt    = sum(score * harvest_tonnes, na.rm = TRUE),
              harvest_tot = sum(harvest_tonnes, na.rm = TRUE),
              score = ifelse(harvest_tot > 0, score_wt / harvest_tot, NA),
              score = round(100 * score, 5)) %>%
    ungroup() %>%
    select(region_id, year, score) %>%
    mutate(goal = 'MAR',
           dimension = 'status')

  ## reference points
  write_ref_pts(goal   = 'MAR',
                method = 'XXXXXXXX',
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  mar_trend   <- calc_trend(mar_status, trend_years)

  mar_scores <- mar_status %>%
    filter(!is.na(region_id)) %>%
    filter(year == data_year) %>%
    bind_rows(mar_trend) %>%
    select(region_id, goal, dimension, score)

  # message('Returning from MAR')
  return(mar_scores)

}

SAL <- function(layers) {

  ##### Gather parameters and layers #####
  ### * sal_catch, sal_escapes
  # message('Now starting SAL')

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  sal_C        <- layers$data[['sal_catch']] %>%
    select(-layer)
  sal_E        <- layers$data[['sal_escapes']] %>%
    select(-layer)

  stocks <- sal_C %>%
    full_join(sal_E, by = c('rgn_id', 'year', 'stock'))

  #############################################################.
  ##### run each salmon stock through the E' and C' calcs #####
  #############################################################.

  ### Function for converting E/E_target values into a 0 - 1 score
  calc_e_prime <- function(E_Et) {
    delta_e <- .25 ### max overescape penalty
    c_v_e <- 0.6 ### Coefficient of variation of E/E_target

    m_e1 <- 1 / c_v_e
    m_e2 <- - (1 - delta_e)/(c_v_e)
    e_0_1 <- 1 - m_e1
    e_0_2 <- 1 - m_e2 * (1 + c_v_e)

    e_prime <- case_when(E_Et < 1.0 - c_v_e      ~ 0,
                         E_Et < 1.0                ~ e_0_1 + m_e1 * E_Et,
                         E_Et < 1.0 + c_v_e      ~ 1,
                         E_Et < 1.0 + 2 * c_v_e  ~ e_0_2 + m_e2 * E_Et,
                         E_Et >= 1.0 + 2 * c_v_e ~ delta_e,
                         TRUE                      ~ NA_real_)
  }

  ### Function for converting C/C_target values into a 0 - 1 score
  calc_c_prime <- function(C_Ct) {
    delta_c <- .25 ### max undercatch penalty
    gamma_c <- 0.4 ### undercatch buffer

    m_c1 <- (1 - delta_c) / (1 - gamma_c)
    c_0_1 <- delta_c

    c_prime <- case_when(C_Ct < 1.0 - gamma_c  ~ c_0_1 + m_c1 * C_Ct,
                         C_Ct < 1.0            ~ 1.0,
                         C_Ct < 2.0            ~ 2.0 - C_Ct,
                         C_Ct >= 2.0           ~ 0,
                         TRUE                  ~ NA_real_)
  }

  stocks_scored <- stocks %>%
    rowwise() %>%
    mutate(esc_score   = calc_e_prime(E_Et),
           catch_score = calc_c_prime(C_Ct),
           stock_score = prod(c(esc_score, catch_score), na.rm = TRUE)) %>%
    filter(!(is.na(esc_score) & is.na(catch_score))) %>% ### if both esc_score and catch_score are NA, drop the row
    ungroup() %>%
    select(rgn_id, stock, year,
           stock_score)

  sal_status <- stocks_scored %>%
    group_by(rgn_id, year) %>%
    summarize(score = mean(stock_score, na.rm = TRUE)) %>%
    ungroup() %>%
    complete_rgn_years(status_yr_span, method = 'none') %>%
    mutate(score     = 100 * score,
           goal      = 'SAL',
           dimension = 'status')

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  sal_trend   <- calc_trend(sal_status, trend_years)

  sal_scores <- sal_status %>%
    filter(year == data_year) %>%
    filter(!is.na(region_id)) %>%
    bind_rows(sal_trend) %>%
    select(goal, dimension, region_id, score)

  # message('returning from SAL')

  return(sal_scores)

}

FP <- function(layers, scores) {

  ### Here we will weight FIS, MAR, and SAL equally.  In BC, wild-capture
  ### fishing landed weight and mariculture harvest weights are approaching
  ### parity based on the DFO year-in-review literature.  While wild-caught
  ### salmon fall significantly lower in harvest, they are important in terms
  ### of cultural importance and are likely eaten in greater proportion
  ### relative to hake, which drive the wild-caught mass.
  ### As such, we've decided that an equal weighting across all three
  ### emphasizes the outsize importance of all three in BC's ability
  ### to provide sustainable seafood to its citizens and the globe.
  ### NOTE: for years in which any one subgoal score is NA, the other scores
  ### will equally contribute to the overall FP score (e.g. when MAR is NA,
  ### FIS and SAL scores will each contribute half of the FP score).


  wts <- data.frame(region_id = c(1:8),
                    w_FIS     = rep(.333, 8),
                    w_MAR     = rep(.333, 8),
                    w_SAL     = rep(.333, 8))

  message('getting FP scores')
  ### scores
  fp_w_wts <- scores %>%
    filter(goal %in% c('FIS', 'MAR', 'SAL')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(wts, by='region_id')  %>%
    mutate(weight = case_when(goal == 'FIS' ~ w_FIS,
                              goal == 'MAR' ~ w_MAR,
                              goal == 'SAL' ~ w_SAL))

  fp_combined <- fp_w_wts  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
    mutate(goal = 'FP') %>%
    ungroup() %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  ### return all scores
  return(rbind(scores, fp_combined))

}

AO <- function(layers) {

  ### Salt marsh, coastal forest based on extent from 30-meter rasters

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  ### get the data:
  closures <- layers$data[['ao_closures']] %>% select(-layer)
  licenses <- layers$data[['ao_licenses']] %>% select(-layer)
  licenses_ref <- layers$data[['ao_licenses_fn_pop']] %>% select(-layer)
  shi      <- layers$data[['ao_spawn_hab_index']] %>% select(-layer)
  sal_C    <- layers$data[['ao_sal_catch']] %>% select(-layer)
  sal_E    <- layers$data[['ao_sal_escapes']] %>% select(-layer)


  ### assign weights to each component
  component_wts <- c('shellfish_closures' = 1,
                     'fn_licenses' = 1,
                     'herring_spawn' = 1,
                     'salmon' = 1)

  ### Calculate status for each component

  ############################################################################=
  ### Closures:
  ############################################################################=
  ### * proportion of year open for access; 0 closures = 100%
  closure_status <- closures %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup() %>%
    mutate(days_in_year = ifelse(lubridate::leap_year(year), 366, 365),
           status = 1 - (days_avg / days_in_year),
           component = 'shellfish_closures') %>%
    select(year, region_id, status, component)

  ############################################################################=
  ### Licenses:
  ############################################################################=
  ### * prop of licenses allocated to FNs, with some level (25%?) as target?
  ### * no net loss vs some rolling average?
  ### * Reference point idea: % FN licenses is equal to FN pop, with some minimum
  ###   threshold (say 10-15%) to account for high non-FN pops in SoG/WCVI regions

  license_ref_min <- 0.15 ### set floor at 15% of licenses

  license_status <- licenses %>%
    filter(rgn_id != 7) %>%
    left_join(licenses_ref, by = 'rgn_id') %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup() %>%
    mutate(ref_pt = ifelse(pct_fn_pop > license_ref_min, pct_fn_pop, license_ref_min),
           status = pct_fn / ref_pt,
           status = ifelse(status > 1, 1, status),
           component = 'fn_licenses') %>%
    select(year, region_id, status, component)
  # ggplot(license_status, aes(x = year, y = status, group = region_id, color = region_id)) + geom_line()

  ############################################################################=
  ### Spawn habitat index
  ############################################################################=
  ### * SHI vs historical reference point of mean SHI from 1940-1960.
  shi_hist_ref <- shi %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup() %>%
    filter(year %in% c(1940:1960)) %>%
    group_by(region_id) %>%
    summarize(shi_ref_pt = mean(shi_tot, na.rm = TRUE)) %>%
    ungroup()
  shi_status <- shi %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup() %>%
    left_join(shi_hist_ref, by = 'region_id') %>%
    mutate(shi_3yr_mean = (shi_tot + lag(shi_tot, 1) + lag(shi_tot, 2)) / 3,
           status = shi_3yr_mean / shi_ref_pt,
           status = ifelse(status > 1, 1, status),
           component = 'herring_spawn') %>%
    select(year, region_id, status, component)

  ############################################################################=
  ### Salmon
  ############################################################################=
  ### Similar to SAL goal except:
  ### * status based only on Escapements if available; if not,
  ###   status based on Catch
  ### * Escapes not penalized for overescapement
  ### * Catch not penalized for underharvest
  stocks <- sal_C %>%
    full_join(sal_E, by = c('rgn_id', 'year', 'stock'))

  ##### run each salmon stock through the E' and C' calcs #####
  ### Function for converting E/E_target values into a 0 - 1 score
  calc_e_prime_ao <- function(E_Et) {
    c_v_e <- 0.6 ### Coefficient of variation of E/E_target
    m_e1 <- 1 / c_v_e
    e_0_1 <- 1 - m_e1
    e_prime <- case_when(E_Et < 1.0 - c_v_e      ~ 0,
                         E_Et < 1.0                ~ e_0_1 + m_e1 * E_Et,
                         TRUE                      ~ NA_real_)
  }

  ### Function for converting C/C_target values into a 0 - 1 score
  calc_c_prime_ao <- function(C_Ct) {
    c_prime <- case_when(C_Ct < 1.0            ~ 1.0,
                         C_Ct < 2.0            ~ 2.0 - C_Ct,
                         C_Ct >= 2.0           ~ 0,
                         TRUE                  ~ NA_real_)
  }

  stocks_scored <- stocks %>%
    rowwise() %>%
    mutate(esc_score   = calc_e_prime_ao(E_Et),
           catch_score = calc_c_prime_ao(C_Ct),
           stock_score = ifelse(is.na(esc_score), catch_score, esc_score)) %>%
    ungroup() %>%
    select(rgn_id, stock, year,
           stock_score)

  salmon_status <- stocks_scored %>%
    group_by(rgn_id, year) %>%
    summarize(status = mean(stock_score, na.rm = TRUE)) %>%
    ungroup() %>%
    complete_rgn_years(status_yr_span, method = 'none') %>%
    filter(!is.na(status)) %>% ### kinda cancels the complete_rgn_years, but just for form
    mutate(component = 'salmon')
  message('  * Yes, that is what we want - same score given to all regions')

  ############################################################################=
  ### Combine all components by weighting
  ############################################################################=
  ao_status <- bind_rows(closure_status, license_status, shi_status, salmon_status) %>%
    mutate(comp_wt = component_wts[component]) %>%
    group_by(region_id, year) %>%
    summarize(score = sum(status * comp_wt, na.rm = TRUE) / sum(comp_wt),
              score = round(score * 100, 5)) %>%
    ungroup() %>%
    mutate(goal      = 'AO',
           dimension = 'status')

  ao_status_components <- bind_rows(closure_status, license_status, shi_status, salmon_status) %>%
    filter(year %in% 2000:2017) %>%
    filter(!is.na(region_id)) %>%
    left_join(get_rgn_names(), by = c('region_id' = 'rgn_id')) %>%
    left_join(ao_status, by = c('year', 'region_id'))

  if(data_year == max(status_yr_span)) {
    write_csv(ao_status_components, '~/github/ohibc/prep/ao/v2017/summary/ao_from_functions.csv')
    # ggplot(ao_status_components, aes(x = year, y = status, group = component, color = component)) +
    #   geom_line(aes(y = score), color = 'grey40', size = 1.5, alpha = .8) +
    #   geom_line(size = 1, alpha = .8) +
    #   facet_wrap( ~ rgn_name)
  }

  ### write element weights to layers object for pressures/resilience calculations
  ao_weights <- ao_status_components %>%
    filter(year == status_year) %>%
    mutate(layer = 'element_wts_ao_components') %>%
    select(rgn_id = region_id, component, ao_comp_status = status, layer)

  layers$data$element_wts_ao_components <- ao_weights

  ### reference points
  write_ref_pts(goal   = 'AO',
                method = 'XXXXXXXX',
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year

  trend_years <- (data_year - 4) : data_year
  ao_trend   <- calc_trend(ao_status, trend_years)

  ao_scores <- ao_status %>%
    filter(!is.na(region_id)) %>%
    filter(year == status_year) %>%
    bind_rows(ao_trend) %>%
    select(region_id, goal, dimension, score)

  return(ao_scores)

}

CSS <- function(layers) {

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

  cs_values <- data.frame(habitat = c('salt_marsh', 'coastal_forest', 'seagrass'),
                          cbr = c(       218.0,              4.6,     138.0))

  ### get the data:
  sm_health   <- layers$data[['cs_sm_health']] %>%
    select(year, region_id = rgn_id, area_hab = sm_area_km2) %>%
    arrange(region_id, year) %>%
    mutate(habitat = 'salt_marsh')

  cf_health   <- layers$data[['cs_cf_health']] %>%
    select(year, region_id = rgn_id, area_hab = cf_area_km2) %>%
    arrange(region_id, year) %>%
    mutate(habitat = 'coastal_forest')

  cs_components <- bind_rows(sm_health, cf_health) %>%
    left_join(cs_values, by = 'habitat') %>%
    group_by(region_id, habitat) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup()

  cs_status <- cs_components %>%
    group_by(region_id, habitat) %>%
    mutate(aref = first(area_hab)) %>%
    group_by(region_id, year) %>%
    summarize(score = sum(area_hab * cbr) / sum(aref * cbr)) %>%
    mutate(goal      = 'CSS',
           dimension = 'status',
           score = ifelse(score > 1, 1, score) * 100) %>%
    ungroup()

  ## reference points
  write_ref_pts(goal   = 'CS',
                method = 'XXXXXXXX',
                ref_pt = NA)

  ### write element weights to layers object for pressures/resilience calculations
  cs_weights <- cs_components %>%
    filter(year == status_year) %>%
    mutate(cs_km2_x_storage = area_hab * cbr,
           layer = 'element_wts_cs_km2_x_storage') %>%
    select(rgn_id = region_id, habitat, cs_km2_x_storage, layer)

  layers$data$element_wts_cs_km2_x_storage <- cs_weights

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

CPP <- function(layers) {

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


  cp_values <- data.frame(habitat  = c('salt_marsh', 'coastal_forest', 'seagrass'),
                          prot     = c(      3,             4,              1    ))

  ### get the data:
  sm_prot <- layers$data[['cp_sm_health']] %>%
    select(year, region_id = rgn_id, expos_area_tot = sm_expos_area_tot) %>%
    arrange(region_id, year) %>%
    mutate(habitat = 'salt_marsh')

  cf_prot <- layers$data[['cp_cf_health']] %>%
    select(year, region_id = rgn_id, expos_area_tot = cf_expos_area_tot) %>%
    arrange(region_id, year) %>%
    mutate(habitat = 'coastal_forest')

  cp_components <- bind_rows(sm_prot, cf_prot) %>%
    left_join(cp_values, by = 'habitat') %>%
    group_by(region_id, habitat) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup()

  cp_status <- cp_components %>%
    group_by(region_id, habitat) %>%
    mutate(a_ref = first(expos_area_tot)) %>%
    group_by(region_id, year) %>%
    summarize(score = sum(expos_area_tot * prot) / sum(a_ref * prot)) %>%
    mutate(goal      = 'CPP',
           dimension = 'status',
           score = ifelse(score > 1, 1, score) * 100) %>%
    ungroup()

  ### write element weights to layers object for pressures/resilience calculations
  cp_weights <- cp_components %>%
    filter(year == status_year) %>%
    mutate(cp_km2_x_exposure_x_protection = expos_area_tot * prot,
           layer = 'element_wts_cp_km2_x_exposure_x_protection') %>%
    select(rgn_id = region_id, habitat, cp_km2_x_exposure_x_protection, layer)

  layers$data$element_wts_cp_km2_x_exposure_x_protection <- cp_weights

  ### reference points
  write_ref_pts(goal   = 'CP',
                method = 'XXXXXXXX',
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

ES <- function(scores) {

  ### combines carbon storage and coastal protection subgoals with a simple
  ### of the two

  s <- scores %>%
    filter(goal %in% c('CSS', 'CPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience')))

  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'ES') %>%
    ungroup() %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  ### return all scores: attach means to existing scores dataframe
  return(rbind(scores, s))

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
  park_visits   <- layers$data[['tr_park_visits']] %>%
    select(-layer)

  ### Sum visitor center (or park) visits within each region
  vc_visits_adj <- vc_visits %>%
    group_by(rgn_id, year) %>%
    summarize(visits = sum(visits)) %>%
    ungroup()


  park_visits_adj <- park_visits %>%
    group_by(rgn_id, year) %>%
    summarize(visits = sum(visits_wt)) %>%
    ungroup()

  ### Create reference point by looking at rolling mean over prior five years.
  ### This also back- and forward-fills using LOCF (or FOCB) to complete
  ### year sequence, which affects reference points.
  vc_visits_ref <- vc_visits_adj %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    arrange(region_id, year) %>%
    mutate(ref_pt = zoo::rollmean(visits, k = 5, fill = NA, align = 'right')) %>%
    ### reference point is mean of past five years (incl current)
    fill(ref_pt, .direction = 'up') %>%
    ungroup()

  park_visits_ref <- park_visits_adj %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    arrange(region_id, year) %>%
    mutate(ref_pt = zoo::rollmean(visits, k = 5, fill = NA, align = 'right')) %>%
    ### reference point is mean of past five years (incl current)
    fill(ref_pt, .direction = 'up') %>%
    ungroup()

  ### Calculate scores for vis ctrs and parks

  vc_scores <- vc_visits_ref %>%
    mutate(vc_score = visits / ref_pt,
           vc_score = ifelse(vc_score > 1, 1, vc_score)) %>%
    select(region_id, year, vc_score)

  park_scores <- park_visits_ref %>%
    mutate(park_score = visits / ref_pt,
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

LE <- function(scores) {

  ### combines LEF (LE First Nations) and LEO (LE other) subgoals with a simple
  ### of the two

  s <- scores %>%
    filter(goal %in% c('LEF', 'LEO')) %>%
    filter(!(dimension %in% c('pressures', 'resilience')))

  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'LE') %>%
    ungroup() %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  ### return all scores: attach means to existing scores dataframe
  return(rbind(scores, s))

}

LEF <- function(layers) {

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  ### NOTE: the 100 score ref point for employment is based on a rolling
  ### mean of the *lagged* empl rate, so the ref point for the current
  ### year DOES NOT include data from the current year.  E.g. for 2009,
  ### ref point is based on 2004-2008.
  ### The employment reference will be the higher of the FN or non-FN
  ### employment rates.
  ### The 100 score ref point for income is based on the highest median
  ### income across all OHIBC regions, including both First Nations and non-
  ### First Nations.
  ###
  ### NOTE: the 0 score ref point will remain at 0 median wage and 0 employment;
  ### other options were explored in data_prep_le.Rmd but rejected for a simpler model.

  empl_df <- layers$data[['le_unempl_fn']] %>%
    select(-layer, fn_unempl = mean_unempl) %>%
    full_join(layers$data[['le_unempl_nonfn']],
              by = c('year', 'rgn_id')) %>%
    select(-layer, nonfn_unempl = mean_unempl) %>%
    mutate(empl_rate_fn    = 1 - fn_unempl/100,
           empl_rate_nonfn = 1 - nonfn_unempl/100) %>%
    rowwise() %>%
    mutate(empl_rate_max = max(empl_rate_nonfn, empl_rate_fn)) %>%
    ungroup() %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    arrange(region_id, year) %>%
    mutate(empl_rate_lag = lag(empl_rate_max, 1, default = first(empl_rate_max)),
              ### note: the default back-fills the first value, i.e. 1996 value
           ref_pt = zoo::rollmean(empl_rate_lag, k = 5, fill = NA, align = 'right')) %>%
    ungroup() %>%
    select(region_id, year, empl_rate_fn, ref_pt)

  income_df <- layers$data[['le_income_fn']] %>%
    select(-layer, med_income_fn = med_adj_income) %>%
    left_join(layers$data[['le_income_nonfn']], by = c('year', 'rgn_id')) %>%
    select(-layer, med_income_nonfn = med_adj_income) %>%
    rowwise() %>%
    mutate(med_income_max = max(med_income_fn, med_income_nonfn)) %>%
    ungroup() %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup() %>%
    mutate(ref_pt = max(med_income_max, na.rm = TRUE)) %>%
    select(region_id, year, med_income_fn, ref_pt)
  ### reference point is max value across regions and years; make sure
  ### income is listed in equivalent dollars

  ### combine wages and jobs scores; calculate overall score
  lef_status <- empl_df %>%
    mutate(jobs_score = empl_rate_fn / ref_pt,
           jobs_score = ifelse(jobs_score > 1, 1, jobs_score)) %>%
    select(year, region_id, jobs_score) %>%
    left_join(income_df %>%
                mutate(wages_score = med_income_fn / ref_pt,
                       wages_score = ifelse(wages_score > 1, 1, wages_score)) %>%
                select(year, region_id, wages_score),
              by = c('year', 'region_id')) %>%
    mutate(score = 100 * (jobs_score + wages_score) / 2,
           goal  = 'LEF',
           dimension = 'status') %>%
    select(year, region_id, score, goal, dimension)


  trend_years <- (data_year - 4) : data_year
  lef_trend   <- calc_trend(lef_status, trend_years)

  lef_scores <- lef_status %>%
    filter(year == data_year) %>%
    bind_rows(lef_trend) %>%
    select(region_id, goal, dimension, score)

  return(lef_scores)

}

LEO <- function(layers) {

  status_year    <- layers$data$scenario_year
  data_year      <- status_year
  status_yr_span <- layers$data$status_year_span

  ### See methods notes in LEF

  empl_df <- layers$data[['le_unempl_fn']] %>%
    select(-layer, fn_unempl = mean_unempl) %>%
    full_join(layers$data[['le_unempl_nonfn']],
              by = c('year', 'rgn_id')) %>%
    select(-layer, nonfn_unempl = mean_unempl) %>%
    mutate(empl_rate_fn    = 1 - fn_unempl/100,
           empl_rate_nonfn = 1 - nonfn_unempl/100) %>%
    rowwise() %>%
    mutate(empl_rate_max = max(empl_rate_nonfn, empl_rate_fn)) %>%
    ungroup() %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    arrange(region_id, year) %>%
    mutate(empl_rate_lag = lag(empl_rate_max, 1, default = first(empl_rate_max)),
           ### note: the default back-fills the first value, i.e. 1996 value
           ref_pt = zoo::rollmean(empl_rate_lag, k = 5, fill = NA, align = 'right')) %>%
    ungroup() %>%
    select(region_id, year, empl_rate_nonfn, ref_pt)

  income_df <- layers$data[['le_income_fn']] %>%
    select(-layer, med_income_fn = med_adj_income) %>%
    left_join(layers$data[['le_income_nonfn']], by = c('year', 'rgn_id')) %>%
    select(-layer, med_income_nonfn = med_adj_income) %>%
    rowwise() %>%
    mutate(med_income_max = max(med_income_fn, med_income_nonfn)) %>%
    ungroup() %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup() %>%
    mutate(ref_pt = max(med_income_max, na.rm = TRUE)) %>%
    select(region_id, year, med_income_nonfn, ref_pt)
  ### reference point is max value across regions and years; make sure
  ### income is listed in equivalent dollars

  ### combine wages and jobs scores; calculate overall score
  leo_status <- empl_df %>%
    mutate(jobs_score = empl_rate_nonfn / ref_pt,
           jobs_score = ifelse(jobs_score > 1, 1, jobs_score)) %>%
    select(year, region_id, jobs_score) %>%
    left_join(income_df %>%
                mutate(wages_score = med_income_nonfn / ref_pt,
                       wages_score = ifelse(wages_score > 1, 1, wages_score)) %>%
                select(year, region_id, wages_score),
              by = c('year', 'region_id')) %>%
    mutate(score = 100 * (jobs_score + wages_score) / 2,
           goal  = 'LEO',
           dimension = 'status') %>%
    select(year, region_id, score, goal, dimension)


  trend_years <- (data_year - 4) : data_year
  leo_trend   <- calc_trend(leo_status, trend_years)

  leo_scores <- leo_status %>%
    filter(year == data_year) %>%
    bind_rows(leo_trend) %>%
    select(region_id, goal, dimension, score)

  return(leo_scores)

}


ICO <- function(layers) {

  ### in goals.csv, allow status year to be NULL, so it can be reassigned
  ### for each iteration through calculate loop
  status_year <- layers$data$scenario_year
  data_year   <- status_year
  status_yr_span <- layers$data$status_year_span

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
    group_by(sciname, iucn_sid, am_sid, comname) %>%
    complete_years(status_yr_span) %>%
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
  ### 'population trend' method of calculation using these values:
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
  write_ref_pts(goal   = 'ICO',
                method = 'scaled IUCN risk categories',
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
    full_join(tot_area_rgn, by = 'region_id') %>%
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
  write_ref_pts(goal   = 'LSP',
                method = paste0(ref_pct_cmpa, '% marine protected area; ',
                                ref_pct_cp, '% coastal protected area'),
                ref_pt = 'varies by area of region eez and 1 km inland')

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


  nutr_prs <- layers$data[['po_nutrient_3nm']] %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    rename(pressure = nutr_pressure) %>%
    ungroup()
  chem_prs <- layers$data[['po_chemical_3nm']] %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    rename(pressure = chem_pressure) %>%
    ungroup()
  trash_prs <- layers$data[['po_trash']] %>%
    rename(region_id = rgn_id) %>%
    left_join(nutr_prs %>% select(region_id, year),
              by = 'region_id') %>%
    group_by(region_id) %>%
    complete_rgn_years(status_yr_span) %>%
    rename(pressure = trash_pressure) %>%
    ungroup()

  patho_prs <- layers$data[['po_pathogen']] %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup() %>%
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
  write_ref_pts(goal   = 'CW',
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
    mutate(habitat = 'ebsa',
           status  = 1 - trawled_area / total_ebsa_area) %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup()

  sb_health   <- layers$data[['hab_sb_health']] %>%
    mutate(habitat = 'soft_bottom',
           status  = 1 - mean_hr_area / max_hr_area) %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup()

  sm_health   <- layers$data[['hab_sm_health']] %>%
    arrange(rgn_id, year) %>%
    mutate(habitat = 'salt_marsh',
           ref_pt  = first(sm_area_km2),
           ref_yr  = first(year),
           status  = sm_area_km2 / ref_pt,
           status  = ifelse(status > 1, 1, status)) %>%
    group_by(rgn_id) %>%
    complete_rgn_years(status_yr_span) %>%
    ungroup()

  hab_components <- bind_rows(ebsa_health, sb_health, sm_health) %>%
    select(region_id, year, status, habitat)

  hab_status <- hab_components %>%
    group_by(region_id, year) %>%
    summarize(score = mean(status)) %>%
    ungroup() %>%
    mutate(goal      = 'HAB',
           dimension = 'status',
           score = ifelse(score > 1, 1, score) * 100)

  ### reference points
  write_ref_pts(goal   = 'HAB',
                method = 'XXXXXXXX',
                ref_pt = NA)

  ### write element weights to layers object for pressures/resilience calculations
  hab_weights <- hab_components %>%
    filter(year == status_year) %>%
    group_by(region_id) %>%
    complete(habitat = unique(hab_components$habitat)) %>%
    ungroup() %>%
    mutate(hab_presence = !is.na(status)) %>%
    mutate(layer = 'element_wts_hab_pres_abs') %>%
    select(rgn_id = region_id, habitat, hab_presence, layer)

  layers$data$element_wts_hab_pres_abs <- hab_weights

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
  status_yr_span <- layers$data$status_year_span

  spp_range_by_rgn <- layers$data[['spp_range_areas']] %>%
    select(region_id = rgn_id, spp_pct_area, iucn_sid, am_sid, spatial_source)
  spp_risk_by_yr   <- layers$data[['spp_risk_scores']] %>%
    select(year, iucn_sid, am_sid, risk_score, risk_source)
  spp_pop_trends   <- layers$data[['spp_pop_trends']] %>%
    select(iucn_sid, am_sid, iucn_pop_trend, cosewic_trend)


  ## reference points
  write_ref_pts(goal   = 'SPP',
                method = 'Average of IUCN risk categories, scaled to historic extinction',
                ref_pt = NA)

  ### prepare scores (status and trend) for current status year.
  ### Linear trend calculated only for species with two or more assessments,
  ### and not using BC-specific scores; otherwise use the 'population trend'
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
    complete_years(status_yr_span, method = 'carry', dir = 'both') %>%
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
    summarize(score = mean(score, na.rm = TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame() %>%
    select(region_id, goal, dimension, score)

  # return all scores
  return(rbind(scores, bd_scores))

}

