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

FIS = function(layers, status_year) {

  # #catch data
  # c = SelectLayersData(layers, layers='fis_meancatch', narrow = TRUE) %>%
  #   select(
  #     region_id    = id_num,
  #     stock_id_taxonkey = category,
  #     year,
  #     catch          = val_num)
  # # b_bmsy data
  # b = SelectLayersData(layers, layer='fis_b_bmsy', narrow = TRUE) %>%
  #   select(
  #     region_id         = id_num,
  #     stock_id      = category,
  #     year,
  #     bmsy           = val_num)
  #
  # # The following stocks are fished in multiple regions and have high b/bmsy values
  # # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # # proportion of catch of these stocks.  The following corrects this problem:
  # #  filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47'))
  #
  # high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')
  #
  # b <- b %>%
  #   mutate(bmsy = ifelse(stock_id %in% high_bmsy, 1, bmsy))
  #
  #
  # # separate out the stock_id and taxonkey:
  # c <- c %>%
  #   mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
  #   mutate(taxon_key = str_sub(stock_id_taxonkey, -6, -1)) %>%
  #   mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7)) %>%
  #   mutate(catch = as.numeric(catch)) %>%
  #   mutate(year = as.numeric(as.character(year))) %>%
  #   mutate(region_id = as.numeric(as.character(region_id))) %>%
  #   mutate(taxon_key = as.numeric(as.character(taxon_key))) %>%
  #   select(region_id, year, stock_id, taxon_key, catch)
  #
  # # general formatting:
  # b <- b %>%
  #   mutate(bmsy = as.numeric(bmsy)) %>%
  #   mutate(region_id = as.numeric(as.character(region_id))) %>%
  #   mutate(year = as.numeric(as.character(year))) %>%
  #   mutate(stock_id = as.character(stock_id))
  #
  #
  # # ------------------------------------------------------------------------
  # # STEP 1. Calculate scores for Bbmsy values
  # # -----------------------------------------------------------------------
  # #  *************NOTE *****************************
  # #  These values can be altered
  # #  ***********************************************
  # alpha <- 0.5
  # beta <- 0.25
  # lowerBuffer <- 0.95
  # upperBuffer <- 1.05
  #
  # b$score = ifelse(b$bmsy < lowerBuffer, b$bmsy,
  #                  ifelse (b$bmsy >= lowerBuffer & b$bmsy <= upperBuffer, 1, NA))
  # b$score = ifelse(!is.na(b$score), b$score,
  #                  ifelse(1 - alpha*(b$bmsy - upperBuffer) > beta,
  #                         1 - alpha*(b$bmsy - upperBuffer),
  #                         beta))
  #
  #
  # # ------------------------------------------------------------------------
  # # STEP 1. Merge the b/bmsy data with catch data
  # # -----------------------------------------------------------------------
  # data_fis <- c %>%
  #   left_join(b, by=c('region_id', 'stock_id', 'year')) %>%
  #   select(region_id, stock_id, year, taxon_key, catch, bmsy, score)
  #
  #
  # # ------------------------------------------------------------------------
  # # STEP 2. Estimate scores for taxa without b/bmsy values
  # # Median score of other fish in the region is the starting point
  # # Then a penalty is applied based on the level the taxa are reported at
  # # -----------------------------------------------------------------------
  #
  # ## this takes the median score within each region
  # data_fis_gf <- data_fis %>%
  #   group_by(region_id, year) %>%
  #   mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
  #   ungroup()
  #
  # ## this takes the median score across all regions (when no stocks have scores within a region)
  # data_fis_gf <- data_fis_gf %>%
  #   group_by(year) %>%
  #   mutate(Median_score_global = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
  #   ungroup() %>%
  #   mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
  #   select(-Median_score_global)
  #
  # #  *************NOTE *****************************
  # #  In some cases, it may make sense to alter the
  # #  penalty for not identifying fisheries catch data to
  # #  species level.
  # #  ***********************************************
  #
  # penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
  #                            penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))
  #
  # data_fis_gf <- data_fis_gf %>%
  #   mutate(TaxonPenaltyCode = as.numeric(substring(taxon_key, 1, 1))) %>%
  #   left_join(penaltyTable, by='TaxonPenaltyCode') %>%
  #   mutate(score_gf = Median_score * penalty) %>%
  #   mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
  #   mutate(score = ifelse(is.na(score), score_gf, score))
  #
  #
  # gap_fill_data <- data_fis_gf %>%
  #   mutate(gap_fill = ifelse(is.na(penalty), "none", "median")) %>%
  #   select(region_id, stock_id, taxon_key, year, catch, score, gap_fill) %>%
  #   filter(year == status_year)
  # write.csv(gap_fill_data, 'temp/FIS_summary_gf.csv', row.names=FALSE)
  #
  # status_data <- data_fis_gf %>%
  #   select(region_id, stock_id, year, catch, score)
  #
  #
  # # ------------------------------------------------------------------------
  # # STEP 4. Calculate status for each region
  # # -----------------------------------------------------------------------
  #
  # # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # # the mean catch of taxon i is divided by the
  # # sum of mean catch of all species in region/year
  #
  # status_data <- status_data %>%
  #   group_by(year, region_id) %>%
  #   mutate(SumCatch = sum(catch)) %>%
  #   ungroup() %>%
  #   mutate(wprop = catch/SumCatch)
  #
  # status_data <- status_data %>%
  #   group_by(region_id, year) %>%
  #   summarize(status = prod(score^wprop)) %>%
  #   ungroup()
  #
  # # ------------------------------------------------------------------------
  # # STEP 5. Get yearly status and trend
  # # -----------------------------------------------------------------------
  #
  # status <-  status_data %>%
  #   filter(year==status_year) %>%
  #   mutate(
  #     score     = round(status*100, 1),
  #     dimension = 'status') %>%
  #   select(region_id=region_id, score, dimension)
  #
  # trend_years <- status_year:(status_year-4)
  # first_trend_year <- min(trend_years)
  #
  # trend <- status_data %>%
  #   filter(year %in% trend_years) %>%
  #   group_by(region_id) %>%
  #   do(mdl = lm(status ~ year, data=.),
  #      adjust_trend = .$status[.$year == first_trend_year]) %>%
  #   summarize(region_id = region_id,
  #             score = round(coef(mdl)['year']/adjust_trend * 5, 4),
  #             dimension = 'trend') %>%
  #   ungroup() %>%
  #   mutate(score = ifelse(score > 1, 1, score)) %>%
  #   mutate(score = ifelse(score < (-1), (-1), score))
  #
  # # assemble dimensions
  # scores <- rbind(status, trend) %>%
  #   mutate(goal='FIS') %>%
  #   filter(region_id != 255)
  # scores <- data.frame(scores)
  #
  # return(scores)
  return(data.frame(goal = 'FIS',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}

MAR <- function(layers, status_year) {
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
  #   filter(year <= status_year)
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
  #   filter(year == status_year) %>%
  #   select(region_id, status) %>%
  #   mutate(status = round(status*100, 2))
  #
  # trend_years <- (status_year-4):(status_year)
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
  # scores = status %>%
  #   select(region_id = region_id,
  #          score     = status) %>%
  #   mutate(dimension='status') %>%
  #   rbind(trend) %>%
  #   mutate(goal='MAR')
  #
  # return(scores)
  return(data.frame(goal = 'MAR',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}


FP <- function(layers, scores, status_year) {

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
  return(data.frame(goal = 'FP',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}


AO <- function(layers,
              status_year,
              Sustainability=1.0) {


  # r <- SelectLayersData(layers, layers = 'ao_access', narrow=TRUE) %>%
  #   select(region_id=id_num, access=val_num)
  # r <- na.omit(r)
  #
  # ry <- SelectLayersData(layers, layers = 'ao_need', narrow=TRUE) %>%
  #   select(region_id = id_num, year, need=val_num) %>%
  #   left_join(r, by="region_id")
  #
  # # model
  #
  # ry <- ry %>%
  #   mutate(Du = (1 - need) * (1 - access)) %>%
  #   mutate(status = (1 - Du) * Sustainability)
  #
  # # status
  # r.status <- ry %>%
  #   filter(year==status_year) %>%
  #   select(region_id, status) %>%
  #   mutate(status=status*100)
  #
  # # trend
  #
  # trend_years <- (status_year-4):(status_year)
  # adj_trend_year <- min(trend_years)
  #
  # r.trend = ry %>%
  #   group_by(region_id) %>%
  #   do(mdl = lm(status ~ year, data=., subset=year %in% trend_years),
  #      adjust_trend = .$status[.$year == adj_trend_year]) %>%
  #   summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
  #   ungroup() %>%
  #   mutate(trend = ifelse(trend>1, 1, trend)) %>%
  #   mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
  #   mutate(trend = round(trend, 4))
  #
  # ## reference points
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "AO", method = "??",
  #                    reference_point = NA))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  #
  # # return scores
  # scores = r.status %>%
  #   select(region_id, score=status) %>%
  #   mutate(dimension='status') %>%
  #   rbind(
  #     r.trend %>%
  #       select(region_id, score=trend) %>%
  #       mutate(dimension='trend')) %>%
  #   mutate(goal='AO') # dlply(scores, .(dimension), summary)
  # return(scores)
  return(data.frame(goal = 'AO',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}


CS <- function(layers, status_year) {

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
  # ## join layer data
  # d <-  extent %>%
  #   full_join(health, by=c("region_id", "habitat")) %>%
  #   full_join(trend, by=c("region_id", "habitat"))
  #
  # ## set ranks for each habitat
  # habitat.rank <- c('mangrove'         = 139,
  #                   'saltmarsh'        = 210,
  #                   'seagrass'         = 83)
  #
  # ## limit to CS habitats and add rank
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
  # write.csv(dp, 'temp/cs_hab_contributions.csv', row.names=FALSE)
  #
  # ## status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  # if (sum(d$km2, na.rm=TRUE) > 0){
  #   # status
  #   scores_CS <- d %>%
  #     filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
  #     group_by(region_id) %>%
  #     summarize(
  #       score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
  #       dimension = 'status') %>%
  #     ungroup()
  #
  #   # trend
  #   d_trend <- d %>%
  #     filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
  #   if (nrow(d_trend) > 0 ){
  #     scores_CS <- dplyr::bind_rows(
  #       scores_CS,
  #       d_trend %>%
  #         group_by(region_id) %>%
  #         summarize(
  #           score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
  #           dimension = 'trend')) %>%
  #       ungroup()
  #   } else { # if no trend score, assign NA
  #     scores_CS <- dplyr::bind_rows(
  #       scores_CS,
  #       d %>%
  #         group_by(region_id) %>%
  #         summarize(
  #           score = NA,
  #           dimension = 'trend'))
  #   }
  #
  #   ### output data file for checking and data review
  #   scores_check <- spread(scores_CS, dimension, score) %>%
  #     select(region_id, status, trend_score=trend)
  #
  #   d_check <- d %>%
  #     select(region_id, habitat, extent, health, trend, rank) %>%
  #     arrange(region_id, habitat) %>%
  #     left_join(scores_check, by="region_id")
  #   write.csv(d_check, sprintf('temp/cs_data_%s.csv', scenario), row.names=FALSE)
  #   ### end: output...
  #
  #   scores_CS <- scores_CS %>%
  #     mutate(
  #       goal = 'CS') %>%
  #     select(region_id=region_id, goal, dimension, score)
  #
  # } else { ## else -- if sum(d$km2) is not greater than 0
  #
  #   ## set status and trend to NA for all regions
  #   message('CS status and trend are NA, consider removing goal if no CS habitats in assessment area')
  #
  #   rgns <-layers$data[['rgn_labels']]
  #   scores_CS <- bind_rows(
  #     rgns %>%
  #       mutate(goal      = 'CS',
  #              dimension = 'status',
  #              score     = NA),
  #     rgns %>%
  #       mutate(goal      = 'CS',
  #              dimension = 'trend',
  #              score     = NA)) %>%
  #     select(goal, dimension, region_id = region_id, score)
  #
  # } ## end -- if (sum(d$km2) > 0)
  #
  # ## reference points
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "CS", method = "Health/condition variable based on current vs. historic extent",
  #                    reference_point = "varies for each region/habitat"))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  #
  # # return scores
  # return(scores_CS)
  return(data.frame(goal = 'CS',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}



CP <- function(layers, status_year) {

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
                    score = runif(16)))

}


TR <- function(layers, status_year, pct_ref = 90) {

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
  #   filter(year <= status_year)
  #
  # tr_model <- tr_data %>%
  #   mutate(
  #     E   = Ep,
  #     S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
  #     Xtr = E * S ) %>%
  #   filter(year <= status_year & year > status_year - 5)
  # # five data years for trend calcs
  #
  # # regions with Travel Warnings
  # ### adjust the travel warning years...these always reflect the current year
  # ### but the other datasets will lag
  # if (exists('scenarios')) { ## if global scenarios
  #   scenario_year <- as.numeric(substring(scenario, 4,7))
  #   offset_years <- scenario_year - status_year
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
  #   filter(year == status_year) %>%
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
  # scores = tr_score %>%
  #   select(region_id=region_id, goal, dimension, score)
  #
  # return(scores)
  return(data.frame(goal = 'TR',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}

LIV_ECO <- function(layers, subgoal, liv_workforcesize_year, eco_rev_adj_min_year, status_year){

  # g.component = c('LIV'='livelihood','ECO'='economy')[[subgoal]]
  #
  # # get status_model
  # status_model_long = SelectLayersData(
  #   layers, narrow = TRUE,
  #   layers=c('le_jobs_cur_base_value','le_jobs_ref_base_value','le_jobs_cur_adj_value','le_jobs_ref_adj_value',
  #            'le_rev_cur_base_value','le_rev_ref_base_value','le_rev_cur_adj_value','le_rev_ref_adj_value',
  #            'le_wage_cur_base_value','le_wage_ref_base_value','le_wage_cur_adj_value','le_wage_ref_adj_value'))
  # status_model = status_model_long %>%
  #   select(cntry_key = id_chr, sector = category, val_num, layer) %>%
  #   mutate(metric = str_replace(layer, 'le_(jobs|rev|wage)_(.*)', '\\1'),
  #          field  = str_replace(layer, 'le_(jobs|rev|wage)_(.*)', '\\2')) %>%
  #   reshape2::dcast(metric + cntry_key + sector ~ field, value.var='val_num')
  #
  # # get gdp per capita, at ppp
  # ppp = SelectLayersData(layers, layers='le_gdp_pc_ppp') %>%
  #   select(cntry_key=id_chr, year, usd=val_num)
  #
  # # country to region aggregation weight for livelihood
  # workforce_adj = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
  #   select(cntry_key=id_chr, year, jobs=val_num)
  #
  # # country to region aggregation weight for economy
  # rev_adj = SelectLayersData(layers, layers='le_revenue_adj') %>%
  #   select(cntry_key=id_chr, year, usd=val_num)
  #
  # # compute the corrected relative value per metric per country, for JOBS
  # status_jobs_rev = status_model %>%
  #   filter(ref_base_value != 0 & ref_adj_value != 0 & metric %in% c('jobs', 'rev')) %>%
  #   group_by(metric, cntry_key) %>%
  #   summarise(
  #     score    = (sum(cur_base_value, na.rm = TRUE) / sum(ref_base_value, na.rm = TRUE)) / (mean(cur_adj_value, na.rm = TRUE) / mean(ref_adj_value, na.rm = TRUE)),
  #     n_sector = n()) %>%
  #   arrange(metric, cntry_key)
  #
  # # compute the corrected relative value per metric per country, for WAGE
  # # 0. extract w'_i = (w_c/w_r)/(W_c/W_r) for each sector i per country
  # t0 = status_model %>%
  #   filter(metric=='wage' & ref_base_value != 0 & ref_adj_value != 0) %>%
  #   mutate(w_prime_i = (cur_base_value / ref_base_value) / (cur_adj_value / ref_adj_value)) %>%
  #   select(metric, cntry_key, sector, w_prime_i) %>%
  #   group_by(metric, cntry_key) %>%
  #   summarise(w_prime  = mean(w_prime_i, na.rm = TRUE),
  #             n_sector = n()) %>%
  #   arrange(metric, cntry_key)
  #
  # # 1. let w' = unweighted mean(w'_i) across all sector i per country
  # # 2. multiple w' by the most recent purchasing power parity (PPP) value for the country
  # p = ppp %>%
  #   arrange(cntry_key, year) %>%
  #   group_by(cntry_key) %>%
  #   summarise(year     = last(year),
  #             ppp_last = last(usd)) %>%
  #   filter(!is.na(ppp_last)) %>%
  #   arrange(cntry_key)
  # t2 = t0 %>%
  #   merge(p, by = 'cntry_key') %>%
  #   mutate(score = w_prime * ppp_last) %>%
  #   select(metric, cntry_key, score, n_sector) %>%
  #   arrange(metric, cntry_key)
  #
  # # 3. set the best country (PPP-adjusted average wage) equal to 1.0 and then rescale all countries to that max
  # max_wage_score = max(t2$score, na.rm = TRUE)
  # status_wage = t2 %>%
  #   mutate(score = score / max_wage_score)
  #
  # # combine the corrected relative values into a single status score
  # status_model_combined = ungroup(status_jobs_rev) %>%
  #   rbind(status_wage)
  # status_score = status_model_combined %>%
  #   # liv
  #   reshape2::dcast(cntry_key ~ metric, value.var='score') %>%
  #   group_by(cntry_key) %>%
  #   mutate(
  #     value     = mean(c(jobs, wage), na.rm = TRUE),
  #     component = 'livelihood') %>%
  #   select(cntry_key, component, value) %>%
  #   ungroup() %>%
  #   arrange(cntry_key, component, value) %>%
  #   # eco
  #   rbind(status_model_combined %>%
  #           filter(metric=='rev') %>%
  #           mutate(
  #             value     = score,
  #             component = 'economy') %>%
  #           select(cntry_key, component, value)) %>%
  #   # order
  #   filter(!is.na(value)) %>%
  #   arrange(cntry_key, component) %>%
  #   # clamp
  #   mutate(score = pmin(value, 1))
  #
  # # countries to regions
  # cntry_rgn = layers$data[['cntry_rgn']] %>%
  #   select(region_id, cntry_key) %>%
  #   merge(
  #     SelectLayersData(layers, layers='rgn_labels') %>%
  #       select(region_id=id_num, rgn_name=val_chr),
  #     by = 'region_id', all.x = TRUE) %>%
  #   arrange(rgn_name, cntry_key) %>%
  #   select(region_id, rgn_name, cntry_key)
  #
  # if (conf$config$layer_region_labels=='rgn_global') {
  #   # update country to region lookups
  #   # TODO: use name_to_rgn
  #   suppressWarnings({ # Warning message: In rbind_list__impl(environment()) : Unequal factor levels: coercing to character
  #     cntry_rgn = cntry_rgn %>%
  #       mutate(
  #         cntry_key = plyr::revalue(cntry_key, c(
  #           'SCG'            = 'MNE',  # used to be Serbia (no coast) and Montenegro (has coast) in Nature 2012
  #           'Aruba'          = 'ABW',  # ABW and ANT EEZs got split...
  #           'Bonaire'        = 'ANT',
  #           'Curacao'        = 'ANT',
  #           'Sint Eustatius' = 'ANT',
  #           'Saba'           = 'ANT',
  #           'Brunei'         = 'BRN',  # Brunei new country in Malaysia
  #           'Malaysia'       = 'MYS'))) %>%
  #       dplyr::bind_rows(
  #         data.frame(region_id=221, rgn_name='Northern Saint-Martin', cntry_key='BLM'),  # BLM is Saint Barthélemy, included within Northern Saint-Martin (MAF)
  #         data.frame(region_id=209, rgn_name=                'China', cntry_key='HKG'),  # add Hong Kong to China (CHN)
  #         data.frame(region_id=209, rgn_name=                'China', cntry_key='MAC'))  # add Macau to China (CHN)
  #   })
  #   cntry_landlocked = c('BDI','BOL','BWA','CHE','LUX','MKD','MWI','PRY','PSE','SCG','SVK','SWZ','TKM','UGA','ZMB')
  #
  #   # remove landlocked countries and check for any country codes still not matched
  #   status_score = filter(status_score, !cntry_key %in% cntry_landlocked)
  #   if (sum(!status_score$cntry_key %in% cntry_rgn$cntry_key) != 0){
  #     stop(sprintf('LIV_ECO status missing country to region lookup for: %s.', paste(setdiff(status_score$cntry_key, cntry_rgn$cntry_key), collapse=', ')))
  #   }
  # }
  #
  # # get weights, for 1) aggregating to regions and 2) georegionally gap filling
  # weights = workforce_adj %>%
  #   filter(year==liv_workforcesize_year) %>%
  #   select(cntry_key, w=jobs) %>%
  #   mutate(component='livelihood') %>%
  #   rbind(
  #     rev_adj %>%
  #       select(cntry_key, year, w=usd) %>%
  #       filter(year >= eco_rev_adj_min_year) %>%
  #       arrange(cntry_key, year) %>%
  #       group_by(cntry_key) %>%
  #       summarize(w=last(w)) %>%
  #       mutate(component='economy'))
  #
  # # aggregate countries to regions by weights
  # s_r = status_score %>%
  #   merge(cntry_rgn, by = 'cntry_key', all.x = TRUE) %>%
  #   merge(weights, by = c('cntry_key','component'), all.x = TRUE) %>%
  #   select(component, region_id, rgn_name, cntry_key, score, w) %>%
  #   arrange(component, rgn_name, cntry_key) %>%
  #   group_by(component, region_id, rgn_name) %>%
  #   summarize(cntry_w     = paste(cntry_key[!is.na(w)], collapse=','),
  #             cntry_w_na  = paste(cntry_key[is.na(w)], collapse=','),
  #             n           = n(),
  #             n_w_na      = sum(is.na(w)),
  #             score_w_avg = weighted.mean(score, w),
  #             score_avg   = mean(score),
  #             w_sum       = sum(w, na.rm = TRUE)) %>%
  #   mutate(score = ifelse(!is.na(score_w_avg), score_w_avg, score_avg)) %>%
  #   ungroup()
  # #print(filter(s_r, n>1) %>% as.data.frame())
  # # 2013:
  # #    component region_id                                            rgn_name           cntry_w cntry_w_na n n_w_na score_w_avg score_avg        w_sum     score
  # # 1    economy    116 Puerto Rico and Virgin Islands of the United States           PRI,VIR            2      0   0.8764356 0.4977907 1.012454e+11 0.8764356
  # # 2    economy    140                           Guadeloupe and Martinique           GLP,MTQ            2      0   0.3550632 0.3572914 1.876348e+10 0.3550632
  # # 3    economy    163                                       United States USA,Alaska,Hawaii            3      0   0.9982232 0.9437773 1.499130e+13 0.9982232
  # # 4    economy    209                                               China               CHN        HKG 2      1          NA 0.9925451 7.603540e+12 0.9925451
  # # 5    economy    224                                               Chile CHL,Easter Island            2      0   1.0000000 1.0000000 2.485850e+11 1.0000000
  # # 6 livelihood    116 Puerto Rico and Virgin Islands of the United States           PRI,VIR            2      0   0.5212928 0.5484682 1.508586e+06 0.5212928
  # # 7 livelihood    140                           Guadeloupe and Martinique                      GLP,MTQ 2      2          NA 0.9650846 0.000000e+00 0.9650846
  # # 8 livelihood    209                                               China           CHN,HKG            2      0   0.7381191 0.8684414 7.868545e+08 0.7381191
  # #s_r = select(s_r, component, region_id=region_id, rgn_name, score, w=w_sum) %>% head()
  #
  # # setup georegions gapfill by region
  # georegions = SelectLayersData(layers, layers='rgn_georegions') %>%
  #   select(region_id=id_num, level=category, georegion_id=val_num) %>%
  #   reshape2::dcast(region_id ~ level, value.var='georegion_id')
  #
  # data = s_r %>%
  #   filter(component==g.component) %>%
  #   as.data.frame() %>%
  #   select(region_id, score, w_sum)
  #
  # # georegional gap fill ----
  # if (conf$config$layer_region_labels=='rgn_global') {
  #
  #   # georegional gapfill, and output gapfill_georegions attributes
  #   if (!file.exists('temp')) dir.create('temp', recursive = TRUE)
  #   csv = sprintf('temp/eez2013_%s-status-gapfill-georegions.csv', subgoal)
  #   s_r_g = gapfill_georegions(
  #     data              = data,
  #     fld_id            = 'region_id',
  #     fld_value         = 'score',
  #     fld_weight        = 'w_sum',
  #     georegions        = georegions,
  #     ratio_weights     = FALSE,
  #     georegion_labels  = NULL,
  #     r0_to_NA          = TRUE,
  #     attributes_csv    = csv)
  # } else {
  #   s_r_g = data
  # }
  #
  # status = s_r_g %>%
  #   select(region_id=region_id, score) %>%
  #   mutate(
  #     goal      = subgoal,
  #     dimension = 'status',
  #     score     = score * 100) %>%
  #   arrange(region_id)
  #
  # # trend layers ----
  # le_unemployment     = layers$data[['le_unemployment']]
  # le_gdp              = layers$data[['le_gdp']]
  # le_jobs_sector_year = layers$data[['le_jobs_sector_year']]
  # le_rev_sector_year  = layers$data[['le_rev_sector_year']]
  # le_wage_sector_year = layers$data[['le_wage_sector_year']]
  #
  # suppressWarnings({ # Warning message: In rbind_list__impl(environment()) : Unequal factor levels: coercing to character
  #   #browser()
  #
  #   # adjustments
  #   adjustments = dplyr::bind_rows(
  #     le_unemployment %>%
  #       mutate(
  #         metric='jobs',
  #         value = 100 - percent) %>%
  #       select(metric, cntry_key, year, value),
  #     le_gdp %>%
  #       mutate(
  #         metric='rev') %>%
  #       select(metric, cntry_key, year, value=usd))
  #
  #   # metric-country-sector-year
  #   mcsy = dplyr::bind_rows(
  #     le_jobs_sector_year %>%
  #       mutate(metric='jobs'),
  #     le_rev_sector_year %>%
  #       mutate(metric='rev'),
  #     le_wage_sector_year %>%
  #       mutate(metric='wage')) %>%
  #     select(metric, cntry_key, sector, year, value)
  #
  # })
  #
  # # merge metric-country-sector-year with adjustments
  # mcsy = mcsy %>%
  #   select(metric, cntry_key, sector, year, base_value=value) %>%
  #   left_join(
  #     adjustments %>%
  #       select(metric, cntry_key, year, adj_value=value),
  #     by = c('metric','cntry_key','year')) %>%
  #   mutate(
  #     adj_value = ifelse(metric=='wage', 1, adj_value),
  #     value = base_value / adj_value) %>%
  #   arrange(metric, cntry_key, year)
  #
  # # trend per metric-country-sector, based on 5 intervals (6 years of data)
  # mcs =
  #   mcsy %>%
  #   # for clip-n-ship where cntry_key is one value, drops factor to integer so adding this bit
  #   mutate(
  #     cntry_key = as.character(cntry_key)) %>%
  #   filter(!is.na(value)) %>%
  #   group_by(metric, cntry_key, sector) %>%
  #   do(mdl = lm(value ~ year, data=.)) %>%
  #   summarize(
  #     metric    = metric,
  #     cntry_key = cntry_key,
  #     sector    = sector,
  #     trend     = max(-1, min(1, coef(mdl)[['year']] * 5))) %>%
  #   # get sums for weight
  #   left_join(
  #     mcsy %>%
  #       filter(!is.na(value)) %>%
  #       group_by(metric, cntry_key, sector) %>%
  #       summarize(
  #         value_sum = sum(value)),
  #     by = c('metric','cntry_key','sector'))
  #
  # # trend per metric-country
  # mc = dplyr::bind_rows(
  #   # wage: simple average of sectors
  #   mcs %>%
  #     group_by(metric, cntry_key) %>%
  #     filter(metric=='wage') %>%
  #     summarize(
  #       trend = mean(trend)),
  #   # jobs and rev: weighted average by total jobs or rev per sector
  #   mcs %>%
  #     group_by(metric, cntry_key) %>%
  #     filter(metric %in% c('jobs','rev')) %>%
  #     summarize(
  #       trend = weighted.mean(trend, value_sum)))
  #
  # # trend per goal-country
  # gc = dplyr::bind_rows(
  #   # LIV: avg(jobs, wage)
  #   mc %>%
  #     group_by(cntry_key) %>%
  #     filter(metric %in% c('jobs','wage') & !is.na(trend)) %>%
  #     summarize(
  #       score     = mean(trend),
  #       component = 'livelihood'),
  #   # ECO: rev
  #   mc %>%
  #     filter(metric %in% c('rev')) %>%
  #     mutate(
  #       component = 'economy',
  #       score     = trend)) %>%
  #   select(component, cntry_key, score)
  #
  # # remove landlocked countries and check for any country codes still not matched
  # if (conf$config$layer_region_labels=='rgn_global') {
  #   gc = filter(gc, !cntry_key %in% cntry_landlocked)
  #   if (sum(!gc$cntry_key %in% cntry_rgn$cntry_key) != 0){
  #     stop(sprintf('LIV_ECO trend missing country to region lookup for: %s.', paste(setdiff(gc$cntry_key, cntry_rgn$cntry_key), collapse=', ')))
  #   }
  # }
  #
  # # aggregate countries to regions by weights
  # # TODO: migrate to using name_to_rgn()
  # gr = gc %>%
  #   merge(cntry_rgn, by = 'cntry_key', all.x = TRUE) %>%
  #   merge(weights, by = c('cntry_key','component'), all.x = TRUE) %>%
  #   select(component, region_id, rgn_name, cntry_key, score, w) %>%
  #   arrange(component, rgn_name, cntry_key) %>%
  #   group_by(component, region_id, rgn_name) %>%
  #   summarize(cntry_w     = paste(cntry_key[!is.na(w)], collapse=','),
  #             cntry_w_na  = paste(cntry_key[is.na(w)], collapse=','),
  #             n           = n(),
  #             n_w_na      = sum(is.na(w)),
  #             score_w_avg = weighted.mean(score, w),
  #             score_avg   = mean(score),
  #             w_sum       = sum(w, na.rm = TRUE)) %>%
  #   mutate(score = ifelse(!is.na(score_w_avg), score_w_avg, score_avg)) %>%
  #   ungroup() %>%
  #   filter(!is.na(region_id))
  #
  # data = gr %>%
  #   filter(component==g.component) %>%
  #   as.data.frame() %>%
  #   select(region_id, score, w_sum)
  #
  # # georegional gap fill ----
  # if (conf$config$layer_region_labels=='rgn_global') {
  #
  #   # georegional gapfill, and output gapfill_georegions attributes
  #   if (!file.exists('temp')) dir.create('temp', recursive = TRUE)
  #   csv = sprintf('temp/eez2013_%s-trend-gapfill-georegions.csv', subgoal)
  #   rg = gapfill_georegions(
  #     data              = data,
  #     fld_id            = 'region_id',
  #     fld_value         = 'score',
  #     fld_weight        = 'w_sum',
  #     georegions        = georegions,
  #     ratio_weights     = FALSE,
  #     georegion_labels  = NULL,
  #     r0_to_NA          = TRUE,
  #     attributes_csv    = csv)
  #
  # } else {
  #   rg = data
  # }
  #
  # trend = rg %>%
  #   select(region_id=region_id, score) %>%
  #   mutate(
  #     goal      = subgoal,
  #     dimension = 'trend',
  #     score     = score) %>%
  #   arrange(region_id)
  #
  # scores = rbind(status, trend)
  # return(scores)
  return(data.frame(goal = 'LIV_ECO',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}

LE <- function(scores, layers, status_year){

  # if (eez2012){
  #   # replacing 2012 scores for ECO and LIV with 2013 data (email Feb 28, Ben H.)
  #   # ECO: Eritrea (just this one country)
  #   # LIV: Eritrea, Anguilla, Bermuda, Egypt, Ghana, Indonesia, Iceland, Saint Kitts,
  #   #      Sri Lanka, Brunei, Malaysia, Trinidad & Tobago, and Taiwan
  #
  #   # replacement data and region names
  #   scores_2013 <- read.csv('../eez2013/scores.csv')
  #   rgns = SelectLayersData(layers, layers='rgn_labels', narrow = TRUE) %>%
  #     select(region_id=id_num, label=val_chr) %>%
  #     arrange(label)
  #
  #   # ECO
  #   ECO_region_id_replace = subset(rgns, label=='Eritrea', 'region_id', drop = TRUE)
  #   scores = scores %>%
  #     filter(!(goal=='ECO' & dimension=='score' & region_id==ECO_region_id_replace)) %>%
  #     rbind(
  #       scores_2013 %>%
  #         filter(goal=='ECO' & dimension=='score' & region_id==ECO_region_id_replace))
  #
  #   # LIV
  #   LIV_rgns_label_replace = c('Eritrea','Anguilla','Bermuda','Egypt','Ghana','Indonesia','Iceland','Saint Kitts and Nevis','Sri Lanka','Brunei','Malaysia','Trinidad and Tobago','Taiwan')
  #   LIV_rgns_id_replace = subset(rgns, label %in% LIV_rgns_label_replace, 'region_id', drop = TRUE)
  #   stopifnot(length(LIV_rgns_label_replace)==length(LIV_rgns_id_replace))
  #   scores = scores %>%
  #     filter(!(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace)) %>%
  #     rbind(
  #       scores_2013 %>%
  #         filter(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace))
  # }
  #
  # # calculate LE scores
  # scores.LE = scores %>%
  #   filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
  #   reshape2::dcast(region_id + dimension ~ goal, value.var='score') %>%
  #   mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
  #   select(region_id, dimension, score) %>%
  #   mutate(goal  = 'LE')
  #
  # # rbind to all scores
  # scores = scores %>%
  #   rbind(scores.LE)
  #
  # # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
  # r_s_islands   = subset(SelectLayersData(layers, layers='rgn_georegions', narrow = TRUE),
  #                        category=='r2' & val_num==999, id_num, drop = TRUE)
  # r_unpopulated = subset(plyr::ddply(SelectLayersData(layers, layers='le_popn', narrow = TRUE), plyr::.(id_num), summarize,
  #                                    count = val_num[which.max(year)]),
  #                        is.na(count) | count==0, id_num, drop = TRUE)
  # scores[with(scores,
  #             goal %in% c('LIV','ECO','LE') &
  #               !dimension %in% c('pressures','resilience') &
  #               region_id %in% union(r_s_islands, r_unpopulated)),
  #        'score'] = NA
  #
  # # return scores
  # return(scores)
  return(data.frame(goal = 'LE',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
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



CW <- function(layers, status_year) {

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
  # d_pressures <- d %>%
  #   filter(layer %in% grep('po_', lyrs, value=TRUE))  %>%
  #   mutate(pressure = 1 - value) %>%  # invert pressures
  #   group_by(region_id) %>%
  #   summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
  #   mutate(score = score * 100) %>%
  #   mutate(dimension = "status") %>%
  #   ungroup()
  #
  # d_trends <- d %>%
  #   filter(layer %in% grep('_trend', lyrs, value=TRUE)) %>%
  #   mutate(trend = -1 * value)  %>%  # invert trends
  #   group_by(region_id) %>%
  #   summarize(score = mean(trend, na.rm = TRUE)) %>%
  #   mutate(dimension = "trend") %>%
  #   ungroup()
  #
  #
  # # return scores
  # scores = rbind(d_pressures, d_trends) %>%
  #   mutate(goal = "CW") %>%
  #   select(region_id, goal, dimension, score) %>%
  #   data.frame()
  #
  # ## reference points
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "CW", method = "spatial: pressures scaled from 0-1 at raster level",
  #                    reference_point = NA))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  # return(scores)
  return(data.frame(goal = 'CW',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}


HAB <- function(layers, status_year) {

  # ## get the data:
  # health <-  layers$data[['hab_health']] %>%
  #   select(region_id, habitat, health) %>%
  #   mutate(habitat = as.character(habitat))
  #
  # trend <-  layers$data[['hab_trend']] %>%
  #   select(region_id, habitat, trend) %>%
  #   mutate(habitat = as.character(habitat))
  #
  # extent <- layers$data[['hab_extent']] %>%
  #   select(region_id, habitat, extent=km2) %>%
  #   mutate(habitat = as.character(habitat))
  #
  # # join and limit to HAB habitats
  # d <- health %>%
  #   full_join(trend, by = c('region_id', 'habitat')) %>%
  #   full_join(extent, by = c('region_id', 'habitat')) %>%
  #   filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom')) %>%
  #   mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
  #   filter(!is.na(w))
  #
  # if(sum(d$w %in% 1 & is.na(d$trend)) > 0){
  #   warning("Some regions/habitats have extent data, but no trend data.  Consider estimating these values.")
  # }
  #
  # if(sum(d$w %in% 1 & is.na(d$health)) > 0){
  #   warning("Some regions/habitats have extent data, but no health data.  Consider estimating these values.")
  # }
  #
  #
  # ## calculate scores
  # status <- d %>%
  #   group_by(region_id) %>%
  #   filter(!is.na(health)) %>%
  #   summarize(
  #     score = pmin(1, sum(health) / sum(w)) * 100,
  #     dimension = 'status') %>%
  #   ungroup()
  #
  # trend <- d %>%
  #   group_by(region_id) %>%
  #   filter(!is.na(trend)) %>%
  #   summarize(
  #     score =  sum(trend) / sum(w),
  #     dimension = 'trend')  %>%
  #   ungroup()
  #
  # scores_HAB <- rbind(status, trend) %>%
  #   mutate(goal = "HAB") %>%
  #   select(region_id=region_id, goal, dimension, score)
  #
  # ## reference points
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "HAB", method = "Health/condition variable based on current vs. historic extent",
  #                    reference_point = "varies for each region/habitat"))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  # # return scores
  # return(scores_HAB)
  return(data.frame(goal = 'HAB',
                    region_id = rep(c(1:8), 2),
                    dimension = c(rep('status', 8), rep('trend', 8)),
                    score = runif(16)))
}


SPP <- function(layers, status_year) {
  # scores <-   SelectLayersData(layers, layers=c('spp_status'='status','spp_trend'='trend'), narrow = TRUE) %>%
  #   select(region_id = id_num, dimension = layer, score = val_num) %>%
  #   mutate(goal = 'SPP') %>%
  #   mutate(score = ifelse(dimension == 'status', score*100, score))
  #
  # ## reference points
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "SPP", method = "Average of IUCN risk categories, scaled to historic extinction",
  #                    reference_point = NA))
  # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  #
  #
  # return(scores)
         return(data.frame(goal = 'SPP',
                           region_id = rep(c(1:8), 2),
                           dimension = c(rep('status', 8), rep('trend', 8)),
                           score = runif(16)))
}

BD <- function(scores, status_year) {
  # d <- scores %>%
  #   filter(goal %in% c('HAB', 'SPP')) %>%
  #   filter(!(dimension %in% c('pressures', 'resilience'))) %>%
  #   group_by(region_id, dimension) %>%
  #   summarize(score = mean(score, na.rm=TRUE)) %>%
  #   mutate(goal = 'BD') %>%
  #   data.frame()
  #
  # # return all scores
  # return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
    return(data.frame(goal = 'BD',
                      region_id = rep(c(1:8), 2),
                      dimension = c(rep('status', 8), rep('trend', 8)),
                      score = runif(16)))
}


FinalizeScores <- function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','trend', 'status') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}
