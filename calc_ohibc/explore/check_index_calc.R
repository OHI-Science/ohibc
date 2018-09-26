### Does the overall index do an area-weighted mean of all
### region indices? or an index calculation of area-
### weighted goal scores?

### result: area-weighted mean of all region indices, so
### PO with all its NAs drives the overall score

source('~/github/ohibc/src/R/common.R')

dir_ohibc  <- '~/github/ohibc'
dir_calc   <- file.path(dir_ohibc, 'calc_ohibc')


scores <- read_csv(file.path(dir_calc, 'scores_all.csv')) %>%
  filter(year == 2016) %>%
  filter(dimension == 'score')

scores_index <- scores %>%
  filter(region_id == 0)

### is OHIBC index score the mean of all OHIBC-level goal scores?
sc1 <- scores_index %>%
  filter(goal != 'Index') %>%
  filter(nchar(goal) == 2) %>%
  summarize(index = mean(score))

### or is OHIBC index score the area-weighted mean of region-level goal scores?
rgn_areas <- read_csv(file.path(dir_git, 'prep/_spatial/v2017/output/rgn_areas.csv'))
scores_rgn <- scores %>%
  left_join(rgn_areas, by = c('region_id' = 'rgn_id')) %>%
  filter(goal == 'Index') %>%
  filter(region_id != 0) %>%
  summarize(index = sum(score * area_km2) / sum(area_km2))


gl <- read_csv('https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/global2017/Results/data/scores_eez2017.csv')
gl_long <- gl %>%
  rename(rgn_name = region_name, rgn_id = region_id) %>%
  gather(goal, score, -rgn_name, -rgn_id)

gl_index_ohicore <- gl_long %>%
  filter(rgn_id == 0 & goal == 'Index')
### 69.97
gl_score <- gl_long %>%
  filter(rgn_id == 0) %>%
  filter(nchar(goal) == 2) %>%
  summarize(index = mean(score))
### 69.607
