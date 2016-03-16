dir_git <- '~/github/ohibc'
source(file.path(dir_git, 'src/R/common.R'))
### an OHIBC specific version of common.R
dir_anx <- file.path(dir_neptune_data, 'git-annex/bcprep')
dir_rgn <- file.path(dir_git, 'prep/regions')  ### github: general buffer region shapefiles

### goal specific folders and info
goal      <- 'pressures'
scenario  <- 'v2016'
dir_goal  <- file.path(dir_git, 'prep', goal, scenario)

rgn_lyr <- 'ohibc_rgn'

message(sprintf('Reading OHIBC regions shapefile...\n  %s/%s.shp', dir_rgn, rgn_lyr))
rgn_poly <- readOGR(dsn = path.expand(dir_rgn), layer = rgn_lyr,
                    verbose = FALSE, stringsAsFactors = FALSE)
rast_rgn <- raster(file.path(dir_rgn, 'ohibc_rgn_raster_1000m.tif'))


oa_files <- c(file.path(dir_goal, 'data/oa/oa_rescaled_2005-2014_1000pts.tif'),
              file.path(dir_goal, 'data/oa/oa_rescaled_2005-2014_1000pts_a.tif'),
              file.path(dir_goal, 'data/oa/oa_rescaled_2005-2014_1000pts_b.tif'),
              file.path(dir_goal, 'data/oa/oa_rescaled_2005-2014_1000pts_c.tif'),
              file.path(dir_goal, 'data/oa/oa_rescaled_2005-2014_1000pts_d.tif'))

oa_rgn_df <- data.frame()
rbrick_list <- vector('list', length = 5)
i <- 0

for(oa_file in oa_files) { # oa_file <- oa_files[1]
  i <- i + 1

  ### read brick for this iteration; stick the 2014 layer onto the
  ### rbrick_list (list across iterations)
  rbrick <- brick(oa_file)
  names(rbrick) <- paste('y', c(2005:2014), sep = '')

  rbrick_list[[i]] <- rbrick[['y2014']]

  ### calc regional mean across all years for this iteration, and
  ### tack onto oa_rgn_df (across all iterations)
  oa_rgn_mean <- zonal(rbrick, rast_rgn, fun = 'mean')

  oa_rgn_trial_df <- oa_rgn_mean %>%
    as.data.frame() %>%
    gather(year, pressure, y2005:y2014) %>%
    rename(rgn_id = zone) %>%
    mutate(pressure = round(pressure, 3),
           year     = as.integer(str_replace(year, 'y', ''))) %>%
    left_join(rgn_poly@data %>% select(rgn_id, rgn_name), by = 'rgn_id') %>%
    arrange(rgn_id)
  oa_rgn_df <- rbind(oa_rgn_df, oa_rgn_trial_df)
}

### turn rbrick_list into its own brick; calc deviation from means across
### all layers (iterations) in brick
rbrick1 <- brick(rbrick_list)
rbrick2 <- calc(rbrick1, fun = function(x) {mean(x) - x})
names(rbrick2) <- letters[1:5] ### rename layers as iterations

### create a new layer that finds the max deviation (by cell) across
### all iterations in the brick
rast_dev <- calc(rbrick2, fun = function(x) {max(abs(x))})

plot_rast(rast_dev,
          title = 'deviation from mean, sample = 1000 pts',
          scale_label = 'dev in oa',
          rev_scale = TRUE)

### create dataframe of region-by-region deviations from regional means
oa_rgn_df1 <- oa_rgn_df %>%
  group_by(rgn_id, year) %>%
  mutate(dev = mean(pressure) - pressure)

hist(oa_rgn_df1$dev) ### all deviations within +/- .004, most within +/- .001.

knitr::kable(oa_rgn_df1)
