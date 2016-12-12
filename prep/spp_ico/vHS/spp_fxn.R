# spp_fxn.R for OHIBC
# created Jun2015 by Casey O'Hara
# functions to support calculations of the species diversity subgoal

message('NOTE: spp_fxn.R requires that the following variables be set in the global environment (main script):')
message(sprintf('dir_anx:  currently set to \'%s\'', dir_anx))
message(sprintf('dir_anx_global: currently set to \'%s\'', dir_anx_global))
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
  loiczid_raster_file  <- file.path(dir_anx_global, 'rgns/loiczid_raster.grd')
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
spp_am_cell_summary <- function(am_cells_spp,
                                spp_info) {
  # Calculate category and trend scores per cell for Aquamaps species.
  # * load AM species <-> cell lookup
  # * filter to appropriate cells (in regions, meets probability threshold)
  # * join spatial info: loiczid, region ID, cell area
  # * join species info: category score and trend score
  # * filter by cat score != NA
  # * summarize by loiczid - mean cat_score, mean trend_score, count

  message('Generating cell-by-cell summary for Aquamaps species.')

  ### filter species info to just Aquamaps species with category info, and bind to
  ### am_cells_spp to attach cat_score and trend_score.
  spp_am_info <- spp_info %>%
    filter(str_detect(spatial_source, 'am')) %>%
    dplyr::select(am_sid, cat_score, trend_score) %>%
    distinct()

  message(sprintf('Number of Aquamaps species: %d', nrow(spp_am_info)))

  ### filter out NAs and DDs
  am_cells_spp1 <- am_cells_spp %>%
    select(am_sid, loiczid) %>%
    full_join(spp_am_info, by = 'am_sid') %>%
    filter(!is.na(cat_score)) %>%
    filter(!is.na(loiczid)) %>%
    distinct()

  message('Grouping by cell and summarizing by mean category, mean trend, and n_spp for each, for AM spatial info.')
  am_cells_spp_sum <- am_cells_spp1 %>%
    group_by(loiczid) %>%
    summarize(mean_cat_score        = mean(cat_score),     # no na.rm needed; already filtered
              mean_pop_trend_score  = mean(trend_score, na.rm = TRUE),
              n_cat_species         = n(),
              n_trend_species       = sum(!is.na(trend_score))) %>% # no na.rm needed; count all with cat_score
    mutate(source = 'aquamaps') %>%
    ungroup()

  return(invisible(am_cells_spp_sum))
}


##############################################################################=
spp_get_am_cells <- function(rgn2cell_df, n_max = -1, prob_filter = 0, reload = TRUE) {
### Loads the AquaMaps species to cell file; then left-joins it
### to region-to-cell lookup table.  Filters by probability level.

  am_cells_spp_file <- file.path(dir_goal,
                                 sprintf('int/am_cells_spp_prob%s.csv', prob_filter))

  if(!file.exists(am_cells_spp_file) | reload) {
    ### Load Aquamaps species per cell table
    message('Creating Aquamaps species per cell file')
    dir_data_am <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'aquamaps/d2015')
    spp_cell_file <- file.path(dir_data_am, 'csv/hcaf_sp_native_trunc.csv')

    message(sprintf('Loading AquaMaps cell-species data.  Large file! \n  %s ', spp_cell_file))
    am_cells_spp <- read_csv(spp_cell_file, n_max = n_max) %>%
      rename(am_sid = speciesid, prob = probability)

    ### filter out to just cells in BC regions
    am_cells_spp1 <- rgn2cell_df %>%
      left_join(am_cells_spp, by = 'loiczid')

    ### filter out below probability threshold
    am_cells_spp1 <- am_cells_spp1 %>%
      filter(prob >= prob_filter) %>%
      dplyr::select(-prob)

    message(sprintf('Writing Aquamaps species per cell file to: \n  %s', am_cells_spp_file))
    write_csv(am_cells_spp1, am_cells_spp_file)
  } else {
    message(sprintf('Reading Aquamaps species per cell file from: \n  %s', am_cells_spp_file))
    am_cells_spp1 <- read_csv(am_cells_spp_file)
  }

  return(am_cells_spp1)
}


##############################################################################=
spp_get_iucn_cells <- function(rgn2cell_df, reload = FALSE, verbose = FALSE) {
  iucn_cells_local_file <- file.path(dir_goal, 'int/iucn_cells_spp.csv')

  if(file.exists(iucn_cells_local_file) & !reload) {
    message('Reading local IUCN cells file: \n  ', iucn_cells_local_file)
    iucn_cells_spp1 <- read_csv(iucn_cells_local_file)
  } else {
    iucn_cells_global_file <- file.path(dir_anx_global, scenario, 'int/iucn_cells_spp.csv')
    if(file.exists(iucn_cells_global_file)) {

      message('Reading IUCN cells file from global assessment: \n  ', iucn_cells_global_file)
      iucn_cells_spp <- read_csv(iucn_cells_global_file, col_types = 'ciiddc')

    } else {

      message('Building IUCN species to cell table.  This might take a few minutes.')
      iucn_intsx_dir <- file.path(dir_anx_global, scenario, 'iucn_intersections')
      iucn_map_files <- file.path(iucn_intsx_dir,
                                  list.files(iucn_intsx_dir))

      ### read each into dataframe, within a list
      read_intersections <- function(spp_group) {
        if(verbose) message(sprintf('Reading intersections for %s...',
                                    str_replace(tolower(basename(spp_group)), '.csv', '')))
        spp_group_cells <- read_csv(spp_group)
        return(spp_group_cells)
      }
      iucn_cells_spp_list <- lapply(iucn_map_files, read_intersections)

      ### combine list of dataframes to single dataframe
      iucn_cells_spp      <- bind_rows(iucn_cells_spp_list)
      # This creates a full data frame of all IUCN species, across all species groups, for all cells.

      names(iucn_cells_spp) <- tolower(names(iucn_cells_spp))

    }
    message('Trimming global IUCN cells list to local cells only...')
    iucn_cells_spp1 <- rgn2cell_df %>%
      rename(prop_area_rgn = prop_area) %>%
      left_join(iucn_cells_spp %>%
                  rename(prop_area_spp = prop_area),
                by = 'loiczid')

    message('Writing local IUCN cell list to ', iucn_cells_local_file)
    write_csv(iucn_cells_spp1, iucn_cells_local_file)
  }
  return(iucn_cells_spp1)
}


##############################################################################=
spp_iucn_cell_summary <- function(iucn_cells_spp,
                                  spp_info) {
  # Calculate category and trend scores per cell for IUCN species.
  # * For each IUCN species group:
  #   * load IUCN species <-> cell lookup
  #   * filter to appropriate cells (in regions, meets probability threshold)
  #   * join spatial info: loiczid, region ID, cell area
  #   * join species info: category score and trend score
  #   * filter by cat score != NA
  #   * summarize by loiczid - mean cat_score, mean trend_score, count
  # * Each summary data frame should be saved to a list, to be eventually rbind_all'ed

  message('Generating cell-by-cell summary for IUCN range-map species.')

  spp_iucn_info <- spp_info %>%
    filter(str_detect(spatial_source, 'iucn')) %>%
    dplyr::select(iucn_sid, cat_score, trend_score) %>%
    distinct()

  message(sprintf('Number of IUCN species: %d', nrow(spp_iucn_info)))

  ### Join species/cell lookup to species information
  iucn_cells_spp1 <- iucn_cells_spp %>%
    select(iucn_sid, loiczid) %>%
    distinct() %>%
    full_join(spp_iucn_info,
              by = 'iucn_sid')

  ### this next part ditches NA species (no cells or no category)
  iucn_cells_spp1 <- iucn_cells_spp1 %>%
    filter(!is.na(cat_score) & !is.na(loiczid)) %>%
    distinct()

  message('Grouping by cell and summarizing mean category/trend and n_spp for each, for IUCN spatial info.')
  ### NOTE: Currently, ignores the proportional area of each species within
  ### a cell.  If there is *any* presence of the species within a cell, it
  ### is counted as being present everywhere within the cell.
  iucn_cells_spp_sum <- iucn_cells_spp1 %>%
    group_by(loiczid) %>%
    summarize(mean_cat_score = mean(cat_score),      # no na.rm needed; already filtered.
              mean_pop_trend_score = mean(trend_score, na.rm = TRUE),
              n_cat_species = n(),
              n_trend_species = sum(!is.na(trend_score))) %>% # no na.rm needed; count all with cat_score
    mutate(source = 'iucn')

    return(invisible(iucn_cells_spp_sum))
}


##############################################################################=
spp_calc_cell_means <- function(am_cell_summary, iucn_cell_summary, fn_tag = '') {
  ### 2 input data frames:
  ### loiczid | mean_cat_score | mean_pop_trend_score | n_cat_species | n_trend_species | source
  ### calcs weighted score for each cell (by loiczid) from:
  ###   (mean IUCN category value * # of species) for both IUCN and AM data, divided by total species.
  ###   (same for trend)
  summary_by_loiczid <- bind_rows(am_cell_summary, iucn_cell_summary) %>%
    group_by(loiczid, year) %>%
    summarize(weighted_mean_cat   = sum(n_cat_species   * mean_cat_score)/sum(n_cat_species),
              weighted_mean_trend = sum(n_trend_species * mean_pop_trend_score, na.rm = TRUE)/sum(n_trend_species),
              n_cat_species       = sum(n_cat_species),
              n_trend_species     = sum(n_trend_species)) %>%
    arrange(loiczid)

  write_csv(summary_by_loiczid, file.path(dir_goal, sprintf('summary/cell_spp_summary_by_loiczid%s.csv', fn_tag)))
  return(summary_by_loiczid)
}


##############################################################################=
spp_calc_rgn_means <- function(summary_by_loiczid, rgn_cell_lookup, rgn_tag = '') {
  ### Joins region-cell info to mean cat & trend per cell.
  ### Groups by region IDs, and calcs area-weighted mean category and trend values
  ### for all cells across entire region.  Cells only partly within a region are
  ### accounted for by multiplying cell area * proportionArea from rgn_cell_lookup.

  rgn_weighted_sums <- summary_by_loiczid %>%
    inner_join(rgn_cell_lookup,
               by = 'loiczid') %>%
    mutate(rgn_area = cell_area * prop_area,
           area_weighted_mean_cat   = weighted_mean_cat   * rgn_area,
           area_weighted_mean_trend = weighted_mean_trend * rgn_area) %>%
    arrange(loiczid)

  region_sums <- rgn_weighted_sums %>%
    group_by(rgn_id, year) %>%
    summarize(rgn_mean_cat   = sum(area_weighted_mean_cat)/sum(rgn_area),
              rgn_mean_trend = sum(area_weighted_mean_trend)/sum(rgn_area))

  region_sums <- region_sums %>%
    mutate(status = 100 * ((1 - rgn_mean_cat) - 0.25) / 0.75)

  region_summary_file <- file.path(dir_goal,
                                   sprintf('summary/rgn_summary%s.csv', rgn_tag))

  message(sprintf('Writing summary file of area-weighted mean category & trend per region:\n  %s', region_summary_file))
  write_csv(region_sums, region_summary_file)

  return(region_sums)
}


##############################################################################=
spp_append_bcsee <- function(spp_all) {
  bcsee_file <- file.path(dir_anx, '_raw_data/bc_species_and_ecosystems_explorer/d2016/bcsee_export.tsv')
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
  ### 664 species in common; but how many have spatial info?
  spp_all1 <- spp_all %>%
    full_join(bcsee_clean, by = 'sciname') %>%
    filter(!is.na(spatial_source))
  # nrow(spp_all1 %>% filter(!is.na(status_gl) | !is.na(status_pr)))
  ### 664 BCSEE listed species remain... woo!

  ### For species BCSEE species, compare IUCN ranking to Natureserve ranking (for global) and .
  status_gl_cat <- data.frame(status_gl       = c('G5', 'G4', 'G3', 'G2', 'G1', 'GX', 'GH', 'G3G4', 'G4G5', 'GNR'),
                              status_gl_score = c( 0.0,  0.2,  0.4,  0.6,  0.8,  1.0,  1.0,  0.3,    0.1,    NA))
  spp_all1 <- spp_all1 %>%
    left_join(status_gl_cat,   by = 'status_gl')

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
spp_plot_raster <- function(rast_data, rast_cells,
                            which_id,
                            poly_rgn,
                            title = '', scale_label = '',
                            scale_limits = c(0, 100),
                            by_id = 'loiczid',
                            rev_scale = FALSE) {
  require(ggplot2)
  require(RColorBrewer)
  require(maptools)

  rast <- raster::subs(rast_cells, rast_data, by = by_id, which = which_id)
  rast_pts <- raster::rasterToPoints(rast) %>%
    as.data.frame()
  names(rast_pts) <- c('long', 'lat', 'layer')
  rast_pts <- rast_pts %>%
    mutate(group = 1) ### need 'group' variable to plot below...

  if(rev_scale == TRUE) {
    cols <- rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme, low = blue
  } else {
    cols <- colorRampPalette(brewer.pal(11, 'Spectral'))(255) # rainbow color scheme, low = red
  }

  poly_rgn_df <- fortify(poly_rgn, region = 'rgn_id') %>%
    rename(rgn_id = id) %>%
    mutate(rgn_id = as.integer(rgn_id))

  poly_land    <- readShapePoly(fn = file.path(dir_rgn, 'ohibc_land_wgs84'), proj4string = CRS(p4s_opts[2]))
  poly_land_df <- fortify(poly_land)

  rast_plot <- ggplot(data = rast_pts, aes(x = long, y = lat, group = group, fill = layer)) +
    ### omit ticks, axis text:
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    ### set text style, title size and position, and legend position:
    theme(text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
          plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
          legend.position = 'right') +
    ### Blank background and grid:
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    scale_fill_gradientn(colours = cols, na.value = 'gray80',
                        limits = scale_limits) +
    geom_polygon(data = poly_land_df, color = 'gray70', fill = 'gray75', size = 0.1) +
    geom_raster(alpha = .8) +
    geom_polygon(data = poly_rgn_df,  color = 'gray20', fill = NA,       size = 0.1) +
    ### df_plot order: land polygons, then raster cells, then region borders
    labs(title = title,
         fill  = scale_label,
         x = NULL, y = NULL)

  print(rast_plot)
}

