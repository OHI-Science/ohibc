### from ohi-global 2013 functions.R
FIS = function(layers, status_year){
  # layers used: fis_meancatch, fis_b_bmsy, fis_proparea_saup2rgn

  # catch data
  c <-  SelectLayersData(layers, layers = 'fis_meancatch', narrow = TRUE) %>%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      catch          = val_num)

  # separate out the region ids:
  c <- c %>%
    separate(fao_saup_id, c("fao_id", "saup_id"), sep = "_") %>%
    separate(taxon_name_key, c("TaxonName", "TaxonKey"), sep = "_") %>%
    mutate(fao_id = as.numeric(fao_id),
           saup_id = as.numeric(saup_id),
           TaxonKey = as.numeric(TaxonKey)) %>%
    mutate(stock_id = paste(TaxonName, fao_id, sep = "_")) # identifier for linking assessed stocks with country_level catches


  # b_bmsy data
  b <-  SelectLayersData(layers, layer = 'fis_b_bmsy', narrow = TRUE) %>%
    select(
      fao_id         = id_num,
      TaxonName      = category,
      year,
      bmsy           = val_num)
  # Identifier taxa/fao region:
  b <- b %>%
    mutate(stock_id = paste(TaxonName, fao_id, sep = "_")) %>%
    mutate(TaxonName = as.character(TaxonName))


  # area data for saup to rgn conversion
  a <- SelectLayersData(layers, layer = 'fis_proparea_saup2rgn', narrow = TRUE) %>%
    select(saup_id = id_num, rgn_id = category, prop_area = val_num)

  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data and Taxon to species
  # -----------------------------------------------------------------------
  AssessedCatches <- inner_join(b, c, by = c("stock_id", "year", "TaxonName", "fao_id")) %>%
    filter(TaxonKey >= 600000) %>%
    mutate(penalty = 1)

  # ------------------------------------------------------------------------
  # STEP 2. Estimate status data for catch taxa without species status
  #     UnAssessedCatches: taxa with catch status data
  # -----------------------------------------------------------------------
  UnAssessedCatches <- c %>%
    filter(!(year %in% AssessedCatches$year & stock_id %in% AssessedCatches$stock_id))

  # 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year

  # Average status data for assessed stocks by FAO region for each year.
  # This is used as the starting estimate for unassesed stocks
  # Here, the Median b_bmsy was chosen to use (although in the past the minimum value was used)
  #  *************NOTE *****************************
  #  Using the minimum B/BMSY score as an starting point
  #  for the estimate of B/BMSY for unassessed taxa not
  #  identified to species level is very conservative.
  #  There is also the potential of the scores being driven by
  #  outliers.
  #  ***********************************************
  b_summary <- b %>%
    group_by(fao_id, year) %>%
    summarize(Medianb_bmsy = quantile(bmsy, probs = c(0.5)),
              Minb_bmsy = min(as.numeric(bmsy))) %>%
    ungroup() %>%
    data.frame()

  UnAssessedCatches <- UnAssessedCatches %>%
    left_join(b_summary, by = c('fao_id', 'year'))


  # 2b.  Create a penalty variable based on taxa level:
  UnAssessedCatches$TaxonPenaltyCode <- as.numeric(as.character(substring(UnAssessedCatches$TaxonKey,1,1)))

  # 2c. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
                             penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))
  # 2d.Merge with data
  UnAssessedCatches <- UnAssessedCatches %>%
    left_join(penaltyTable, by = "TaxonPenaltyCode")

  # ------------------------------------------------------------------------
  # STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
  # -----------------------------------------------------------------------

  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  ## Function to calculate score for different scenarios:
  score <- function(data, variable){
    #data <- AssessedCatches
    #variable <- "bmsy"
    ifelse(data[ ,variable]*data[, "penalty"]<lowerBuffer,
           data[ ,variable]*data[, "penalty"],
           ifelse(data[ ,variable]*data[, "penalty"]>upperBuffer,
                  ifelse(1-alpha*(data[ ,variable]*data[, "penalty"]
                                  -upperBuffer)>beta,
                         1-alpha*(data[ ,variable]*data[, "penalty"]-upperBuffer),beta),
                  1))
  }

  AssessedCatches$score <- score(data=AssessedCatches, variable="bmsy")

  # Median is used to calculate score for species with Taxon 6 coding
  UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
  UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")

  UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
  UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy")

  AllScores <- rbind(AssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                     UnAssessedCatchesT6[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                     UnAssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")])

  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------

  # 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region r, which is calculated as:

  AllScores <- AllScores %>%
    group_by(year, saup_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch) %>%
    data.frame()



  #  4b. The "score" and "weight" values per taxon per SAUP region are used to
  #    calculate a geometric weighted mean across taxa for each saup_id region
  geomMean <- AllScores %>%
    group_by(saup_id, year) %>%
    summarize(status_saup = prod(score^wprop)) %>%
    ungroup() %>%
    data.frame()

  # ------------------------------------------------------------------------
  # STEP 5. Convert status from saup spatial scale to OHI spatial scale
  # -----------------------------------------------------------------------
  # In many cases the ohi reporting regions are comprised of multiple saup regions.
  # To correct for this, the proportion of each saup area of the total area of the
  # OHI region was calculated. This was used to calculate Status from the Status_saup.
  # This type of adjustment is omitted if the data were collected at the same spatial
  # scale as the collecting region.

  # Join region names/ids to Geom data
  geomMean <- geomMean %>%
    inner_join(a, by = 'saup_id')

  # weighted mean scores
  StatusData <- geomMean %>%
    group_by(rgn_id, year) %>%
    summarize(Status = sum(status_saup*prop_area)) %>%
    ungroup() %>%
    data.frame()

  status <-  StatusData %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %>%
    select(region_id=rgn_id, dimension, score) %>%
    data.frame()

  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend
  # -----------------------------------------------------------------------
  trend <- StatusData %>%
    group_by(rgn_id) %>%
    do(mdl = lm(Status ~ year, data = .)) %>%
    summarize(region_id = rgn_id,
              score = coef(mdl)['year'] * 5) %>%
    ungroup() %>%
    mutate(score = round(score, 2)) %>%
    mutate(dimension = "trend") %>%
    select(region_id, dimension, score) %>%
    data.frame()

  # reference point data
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "FIS", method = "b/bmsy: modeled", reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  # assemble dimensions
  scores = rbind(status, trend) %>% mutate(goal='FIS')
  return(scores)
}

