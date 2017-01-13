# spp_fxn.R for OHIBC
# functions to support calculations of the species diversity subgoal

message('NOTE: spp_fxn.R requires that the following variables be set in the global environment (main script):')
message(sprintf('dir_goal_anx:  currently set to \'%s\'', dir_goal_anx))
message(sprintf('dir_goal_anx_global: currently set to \'%s\'', dir_goal_anx_global))
message(sprintf('scenario:       currently set to \'%s\'', scenario))

##############################################################################=
spp_rgn2cell <- function(poly_rgn,
                         rgn_tag   = '',
                         reload    = FALSE,
                         verbose   = FALSE) {
  ### Determines proportional area of each cell covered by region polygons.
  ### Returns list containing:
  ### * data frame of rgn_id, loiczid, and proportional area of loiczid cell covered by the rgn_id region.
  ### * raster of LOICZID values
  ##############################################################################=

  ### load Aquamaps LOICZID raster, and determine CRS
  loiczid_raster_file  <- file.path(dir_goal, 'spatial/loiczid.tif')
  loiczid_raster <- raster::raster(loiczid_raster_file)
  rst_p4s <- proj4string(loiczid_raster)
  message(sprintf('LOICZID raster loaded: CRS = %s ', rst_p4s))

  ### Crop LOICZID raster to rounded extents of region polygon
  poly_ext <- raster::extent(poly_rgn)
  poly_ext <- raster::extent(floor(poly_ext[1]), ceiling(poly_ext[2]), floor(poly_ext[3]), ceiling(poly_ext[4]))
  loiczid_raster <- raster::crop(loiczid_raster, poly_ext)

  rgn2cell_file <- file.path(dir_goal, sprintf('int/cell_id_by_rgn%s.csv', rgn_tag))

  if(!file.exists(rgn2cell_file) | reload) {
    ### File doesn't exist (or reload == TRUE), so create from scratch.

    ### get LOICZID per region, convert to data frame
    message('Extracting proportional area of LOICZID cells per region polygon.')
    region_prop <- raster::extract(loiczid_raster, poly_rgn,
                                   weights = TRUE, normalizeWeights = FALSE)
    names(region_prop) <- poly_rgn@data$rgn_id

    rgn_df <- data.frame()
    for (rgn_id in names(region_prop)) {
      temp_df <- data.frame(rgn_id,
                            loiczid   = unlist(region_prop[[rgn_id]])[, 1],
                            prop_area = unlist(region_prop[[rgn_id]])[, 2])
      rgn_df <- rbind(rgn_df, temp_df)
    }

    rgn_df <- rgn_df %>%
      mutate(rgn_id = as.integer(as.character(rgn_id))) %>%
      left_join(data.frame(rgn_id = poly_rgn@data$rgn_id, rgn_name = poly_rgn@data$rgn_name),
                by = 'rgn_id')

    ### Append the area for each cell, by LOICZID
    dir_data_am <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'aquamaps/d2015')
    file_loc    <- file.path(dir_data_am, 'csv/hcaf_truncated.csv')

    message(sprintf('Loading AquaMaps half-degree cell authority file.  \n  %s ', file_loc))
    am_cells <- data.table::fread(file_loc, stringsAsFactors = FALSE,
                      header = TRUE, showProgress = FALSE) %>%
      as.data.frame() %>%
      setNames(tolower(names(.))) %>%
      dplyr::select(loiczid, cell_area = cellarea)

    rgn_df <- rgn_df %>%
      left_join(am_cells, by = 'loiczid')

    ### Write finalized file
    message(sprintf('Writing loiczid/cell proportions/cell areas by region to: \n  %s', rgn2cell_file))
    write.csv(rgn_df, rgn2cell_file, row.names = FALSE)
  } else {
    ### File already exists, just read it
    message(sprintf('Reading loiczid cell proportions by region from: \n  %s', rgn2cell_file))
    rgn_df <- read_csv(rgn2cell_file)
  }

  return(list(rgn_df, loiczid_raster))
}


##############################################################################=
spp_am_cell_summary <- function(spp_info, am_spp_cells) {
  # Calculate category scores per cell for Aquamaps species.
  # * load AM species <-> cell lookup
  # * filter to appropriate cells (in regions, meets probability threshold)
  # * join spatial info: loiczid, region ID, cell area
  # * join species info: category score
  # * filter by cat score != NA
  # * summarize by loiczid - mean cat_score, count

  message('Generating cell-by-cell summary for Aquamaps species.')

  ### filter species info to just Aquamaps species with category info, and bind to
  ### am_spp_cells to attach cat_score
  spp_am_info <- spp_info %>%
    filter(str_detect(spatial_source, 'am')) %>%
    filter(!is.na(cat_score)) %>%
    dplyr::select(am_sid, cat_score, pr_score) %>%
    distinct()

  message(sprintf('Number of Aquamaps species: %d', nrow(spp_am_info)))

  ### filter out NAs and DDs
  am_spp_cells1 <- am_spp_cells %>%
    select(am_sid, loiczid) %>%
    left_join(spp_am_info, by = 'am_sid') %>%
    filter(!is.na(cat_score)) %>%
    filter(!is.na(loiczid)) %>%
    distinct()

  message('Grouping by cell and summarizing by mean category and n_spp for each, for AM spatial info.')
  am_cells_sum <- am_spp_cells1 %>%
    group_by(loiczid) %>%
    summarize(mean_cat_score        = mean(cat_score),
              n_cat_species         = n(),
              n_trend_species = sum(is.na(pr_score))) %>% ### those using provincial scores will be excluded from trend calcs
  mutate(source = 'aquamaps') %>%
    ungroup()

  return(invisible(am_cells_sum))
}


##############################################################################=
spp_iucn_cell_summary <- function(spp_info, iucn_spp_cells) {
  # Calculate category scores per cell for IUCN species.
  # * For each IUCN species group:
  #   * load IUCN species <-> cell lookup
  #   * filter to appropriate cells (in regions, meets probability threshold)
  #   * join spatial info: loiczid, region ID, cell area
  #   * join species info: category score
  #   * filter by cat score != NA
  #   * summarize by loiczid - mean cat_score, count
  # * Each summary data frame should be saved to a list, to be eventually rbind_all'ed

  message('Generating cell-by-cell summary for IUCN range-map species.')

  spp_iucn_info <- spp_info %>%
    filter(str_detect(spatial_source, 'iucn')) %>%
    # filter(!is.na(cat_score)) %>%
    dplyr::select(map_iucn_sid, map_subpop, cat_score, pr_score) %>%
    distinct()

  message(sprintf('Number of IUCN species: %d', length(unique(spp_iucn_info$map_iucn_sid))))

  ### Join species/cell lookup to species information
  iucn_spp_cells1 <- iucn_spp_cells %>%
    select(iucn_sid, loiczid, subpop) %>%
    distinct() %>%
    left_join(spp_iucn_info,
              by = c('iucn_sid' = 'map_iucn_sid', 'subpop' = 'map_subpop'))

  ### this next part ditches NA species (no cells or no category)
  iucn_spp_cells2 <- iucn_spp_cells1 %>%
    filter(!is.na(cat_score) & !is.na(loiczid)) %>%
    distinct()

  message('Grouping by cell and summarizing mean category and n_spp for each, for IUCN spatial info.')
  ### NOTE: Currently, ignores the proportional area of each species within
  ### a cell.  If there is *any* presence of the species within a cell, it
  ### is counted as being present everywhere within the cell.
  iucn_cells_sum <- iucn_spp_cells2 %>%
    group_by(loiczid) %>%
    summarize(mean_cat_score = mean(cat_score),
              n_cat_species = n(),
              n_trend_species = sum(is.na(pr_score))) %>% ### those using provincial scores will be excluded from trend calcs
    mutate(source = 'iucn')

  return(invisible(iucn_cells_sum))
}


##############################################################################=
spp_calc_cell_means <- function(am_cell_summary, iucn_cell_summary) {
  ### 2 input data frames:
  ### loiczid | mean_cat_score | n_cat_species | source
  ### calcs weighted score for each cell (by loiczid) from:
  ###   (mean IUCN category value * # of species) for both IUCN and AM data, divided by total species.
  summary_by_loiczid <- bind_rows(am_cell_summary, iucn_cell_summary) %>%
    group_by(loiczid, year) %>%
    summarize(weighted_mean_cat = sum(n_cat_species * mean_cat_score)/sum(n_cat_species),
              n_cat_species     = sum(n_cat_species),
              n_trend_species   = sum(n_trend_species)) %>%
    arrange(loiczid)

  return(summary_by_loiczid)
}


##############################################################################=
spp_calc_rgn_means <- function(summary_by_loiczid, rgn_cell_lookup) {
  ### Joins region-cell info to mean cat per cell.
  ### Groups by region IDs, and calcs area-weighted mean category values
  ### for all cells across entire region.  Cells only partly within a region are
  ### accounted for by multiplying cell area * proportionArea from rgn_cell_lookup.

  rgn_weighted_sums <- summary_by_loiczid %>%
    inner_join(rgn_cell_lookup,
               by = 'loiczid') %>%
    mutate(rgn_area = cell_area * prop_area,
           area_weighted_mean_cat   = weighted_mean_cat * rgn_area) %>%
    arrange(loiczid)

  region_sums <- rgn_weighted_sums %>%
    group_by(rgn_id, year) %>%
    summarize(rgn_mean_cat   = sum(area_weighted_mean_cat) / sum(rgn_area),
              mean_n_cat_spp = mean(n_cat_species * rgn_area) / mean(rgn_area),
              mean_n_trend_spp = mean(n_trend_species * rgn_area) / mean(rgn_area)) ### these will capture

  region_sums <- region_sums %>%
    mutate(status = ((1 - rgn_mean_cat) - 0.25) / 0.75)

  return(region_sums)
}


##############################################################################=
spp_append_bcsee <- function(spp_all) {
  bcsee_file <- file.path(dir_M, 'git-annex/bcprep/_raw_data',
                          'bc_species_and_ecosystems_explorer/d2016/bcsee_export.tsv')
  bcsee_all  <- read.delim(bcsee_file, sep = '\t', stringsAsFactors = FALSE) %>%
    dplyr::select(sciname = Scientific.Name, scisyn = Scientific.Name.Synonyms,
           com_name = English.Name,
           #        el_code = Element.Code,
           status_gl = Global.Status, status_pr = Prov.Status,
           date_gl = Global.Status.Review.Date, date_pr = Prov.Status.Review.Date,
           date_pr_change = Prov.Status.Change.Date)

  ### For spp with > 1 scientific synonyms, separate at ';', gather into
  ### a new sciname column to be rbind()ed to main list.
  ### Note: str_split() was not cooperating properly within mutate().
  bcsee_syn_semicolon <- bcsee_all %>%
    filter(str_detect(scisyn, ';')) %>%
    separate(scisyn, c('syn1', 'syn2', 'syn3'), sep = ';', fill = 'right', extra = 'drop', remove = TRUE) %>%
    gather(tmp, scisyn, syn1:syn3) %>%
    mutate(sciname = scisyn) %>%
    dplyr::select(-tmp, -scisyn)

  ### rbind() original list with list of scientific synonyms, now included as
  ### additional scientific names
  bcsee_no_syn <- bcsee_all %>%
    dplyr::select(-scisyn) %>%
    rbind(bcsee_syn_semicolon) %>%
    filter(!is.na(sciname) & sciname != '')

  ### Ditch subspecies, variants, and populations in sciname: separate out
  ### the cruft into new columns, then ditch 'em.
  bcsee_clean <- bcsee_no_syn %>%
    separate(sciname, c('sciname', 'var'), sep = ' var. ', fill = 'right', extra = 'drop', remove = TRUE) %>%
    separate(sciname, c('sciname', 'pop'), sep = ' pop. ', fill = 'right', extra = 'drop', remove = TRUE) %>%
    separate(sciname, c('sciname', 'ssp'), sep = ' ssp. ', fill = 'right', extra = 'drop', remove = TRUE) %>%
    dplyr::select(-var, -pop, -ssp) %>%
    distinct()

  ### Now the cleaned BCSEE data can be compared, via sciname, to spp_all.
  # nrow(bcsee_clean %>% filter(sciname %in% spp_all$sciname))
  ### 644 species in common; but how many have spatial info?
  spp_all1 <- spp_all %>%
    full_join(bcsee_clean, by = 'sciname') %>%
    filter(!(is.na(am_sid) & is.na(map_iucn_sid)))
  # nrow(spp_all1 %>% filter(!is.na(status_gl) | !is.na(status_pr)))
  ### 612 BCSEE listed species remain... woo!

  ### For species BCSEE species, compare IUCN ranking to Natureserve ranking (for global) and .
  status_gl_cat <- data.frame(status_gl       = c('G5', 'G4', 'G3', 'G2', 'G1', 'GX', 'GH', 'G3G4', 'G4G5', 'GNR'),
                              status_gl_score = c( 0.0,  0.2,  0.4,  0.6,  0.8,  1.0,  1.0,  0.3,    0.1,    NA))
  spp_all1 <- spp_all1 %>%
    left_join(status_gl_cat, by = 'status_gl')

  ### How to deal with Breeding, Nonbreeding, Migrant codes?
  ### For now, quick fix: separate, drop the second, ignore the codes.
  spp_all1 <- spp_all1 %>%
    separate(status_pr, c('status_pr1', 'status_pr2'), sep = ',', remove = FALSE, fill = 'right', extra = 'drop') %>%
    dplyr::select(-status_pr2) %>%
    mutate(status_pr1 = str_replace_all(status_pr1, '[ABNMUR?]', ''))

  status_pr_cat <- data.frame(status_pr1       = c('S5', 'S4', 'S3', 'S2', 'S1', 'SX', 'SH', 'S1S2', 'S2S3', 'S3S4', 'S4S5', 'S'),
                              status_pr_score = c( 0.0,  0.2,  0.4,  0.6,  0.8,  1.0,  1.0,   0.7,    0.5,    0.3,    0.1,  NA))
  spp_all1 <- spp_all1 %>%
    left_join(status_pr_cat,   by = 'status_pr1') %>%
    dplyr::select(-com_name, -status_pr1, -date_gl, -date_pr, -date_pr_change)

  return(spp_all1)
}

##############################################################################=
calc_trends_per_rgn <- function(sum_by_rgn, include_years, span) {

  spp_trend_list <- vector('list', length = length(include_years))

  for(i in 1:length(include_years)) { # i <- 1
    yr <- include_years[i] ### OHI year lags IUCN year by 1 year, so include 2011 for OHI2012 etc
    spp_rgn_status_current <- sum_by_rgn %>%
      filter(year <= yr & year > yr - span)
    spp_trend_list[[i]] <- spp_rgn_status_current %>%
      group_by(rgn_id) %>%
      do(trend_raw = lm(status ~ year, data = .)$coefficients['year']) %>%
      mutate(trend_raw = as.numeric(trend_raw),
             year  = yr) %>%
      ungroup()
  }

  spp_trend_df <- bind_rows(spp_trend_list)

  spp_status_trend_df <- sum_by_rgn %>%
    left_join(spp_trend_df, by = c('rgn_id', 'year')) %>%
    mutate(trend = trend_raw / lag(status, span) * 5) %>%
    filter(year %in% include_years)
  ### this divides by the status 10 years ago to get a % change,
  ### then multiplies by 5 to predict likely increase five years from now.
  ### Rounds to 5 decimals because annoying.


  return(spp_status_trend_df)

}

