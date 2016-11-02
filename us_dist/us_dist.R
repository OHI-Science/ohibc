library(tidyverse)
library(rgdal)
library(geosphere)
library(tmap)

dir_M <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
           'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
           'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]
dir_dist <- file.path(dir_M, 'git-annex/bcprep/us_dist')

# spdf <- readOGR(file.path(dir_dist, 'tl_2016_us_cd115'), 'tl_2016_us_cd115')
spdf <- readOGR(file.path(dir_dist, 'tl_2013_us_cd113'), 'tl_2013_us_cd113')
names(spdf@data) <- tolower(names(spdf@data))

state_abbrs <- read_csv(file.path(dir_dist, 'fips_codes.csv')) %>%
  select(state_ab = `State Abbreviation`,
         statefp  = `State FIPS Code`) %>%
  distinct()

districts <- spdf@data %>%
  left_join(state_abbrs, by = 'statefp')

districts$per_km <- perimeter(spdf) / 1000
districts$area_km2 <- areaPolygon(spdf) / 1e6

districts <- districts %>%
  mutate(aspect_ratio = per_km^2/area_km2)

spdf@data <- spdf@data %>%
  left_join(districts)

dist_map <- tm_shape(spdf) +
  tm_polygons(col = 'aspect_ratio') +
  tm_text(text = 'state')

### what may indicate gerrymandering:
### - Candidates are elected with little or no opposition.
###   - very high voting imbalances; but the next should capture this too
### - Election results in a district are very different than the statewide
###   party divide, either extremely divided when the state is even or
###   extremely even when the state as a whole is divided.
###   - statewide vote balance compared to district vote balance
###     - per district, just an absolute of the difference?
###     - maybe pop-weighted sum of (difference squared) across the state?
###     - how to account for urban vs rural?
### - The shape of the district is bizarre, complex, and/or uneven, as if
###   intentionally drawn to exclude some voters and include others.
###   - a high aspect ratio; large border relative to area
###     - e.g. circle AR = (2*pi*r)^2/(pi*r^2) = 4pi or about 12.5
###     -      square AR = (4*L)^2/L^2 = 16 (e.g. Wyoming)
### - this within a state with multiple districts; ignore single-district states

votes_raw <- read_csv(file.path(dir_dist, 'fed_elec_2012_house_senate.csv')) %>%
  setNames(tolower(names(.)) %>%
             str_replace_all('%', 'pct') %>%
             str_replace_all('[:punct:]', '') %>%
             str_replace_all(' ', '_')) %>%
  select(
    state_ab = state_abbreviation,
    state_nm = state,
    district = d,
    # fec_id, i, candidate_name_first, candidate_name_last,
    candidate_name,
    # total_votes,
    party,
    # primary_votes, primary_pct, runoff_votes, runoff_pct,
    votes = general_votes,
    # general_pct, ge_runoff_election_votes_la, ge_runoff_election_pct_la,
    combined_ge_party_totals_ct_ny_sc,
    combined_pct_ct_ny_sc,
    ge_winner_indicator
    # footnotes, x24
  )

votes_clean <- votes_raw %>%
  filter(!is.na(votes) & !is.na(party)) %>%
    ### cuts to just candidate votes; eliminates blank rows and total rows
  filter(district != 'S') %>%
    ### these are senate races; let's focus on representative races only
  mutate(votes = ifelse(state_ab == 'FL' & district %in% c(15, 24), -1, votes),
           ### unopposed FL districts set to -1 vote as flag
         votes = as.integer(votes %>%
                                      str_replace_all(',', '')))

party_totals <- votes_clean %>%
  mutate(party = ifelse(str_detect(party, '^D'), 'D', party),
         party = ifelse(str_detect(party, '^R'), 'R', party)) %>%
  group_by(party) %>%
  summarize(votes = sum(votes))

### States with more than 8% not caught by '^D' and '^R':
### Maine: IFM is a Maine party; Angus King caucuses with Democrat
### Louisiana: lots of LIB and NPA
### Kansas: lots of LIB
### Maryland: lots of LIB and a few GRE
### Colorado:
### Arkansas:

parties_d <- c('IFM', 'GRE', 'WF', 'IDP', 'W(D)/D')
parties_r <- c('CRV', 'IAP', 'W(R)/R', 'UST')

### consolidate conservative and liberal parties into R and D;
### first pass, stick any D starts and R starts into those parties
### second pass, see if any big vote parties remain, then divide into R and D
votes_dist <- votes_clean %>%
  mutate(party = ifelse(str_detect(party, '^D'), 'D', party),
         party = ifelse(str_detect(party, '^R'), 'R', party),
         party = ifelse(party %in% parties_d, 'D', party),
         party = ifelse(party %in% parties_r, 'R', party),
         party = ifelse(!party %in% c('R', 'D'), 'other', party)) %>%
  group_by(state_ab, district, party) %>%
  summarize(votes = sum(votes)) %>%
  group_by(state_ab, district) %>%
  mutate(dist_tot = sum(votes)) %>%
  ungroup() %>%
  mutate(dist_pct = votes/dist_tot)

### calc state party percentages...
votes_state <- votes_dist %>%
  group_by(state_ab) %>%
  mutate(votes_state = sum(votes)) %>%
  group_by(state_ab, party) %>%
  summarize(party_pct = sum(votes)/first(votes_state)) %>%
  spread(party, party_pct) %>%
  left_join(state_lookup, by = 'state_ab') %>%
  rename(state_d = D, state_r = R, state_other = other) %>%
  mutate(state_d = ifelse(is.na(state_d), 0, state_d),
         state_r = ifelse(is.na(state_r), 0, state_r))


state_lookup <- votes_raw %>%
  select(state_ab, state_nm) %>%
  distinct() %>%
  left_join(districts %>%
              select(statefp, state_ab = state) %>%
              distinct(),
            by = 'state_ab') %>%
  filter(!is.na(state_ab) & !is.na(state_nm))

write_csv(state_lookup, 'us_dist/state_lookup.csv')

### create dataframe with statewide percentages and district percentages;
### include only states with more than one district...
### ignore any 'other' parties...
votes_df <- votes_dist %>%
  filter(party != 'other') %>%
  select(-votes) %>%
  group_by(state_ab, district) %>%
  spread(party, dist_pct) %>%
  rename(dist_d = D, dist_r = R) %>%
  mutate(dist_d = ifelse(is.na(dist_d), 0, dist_d),
         dist_r = ifelse(is.na(dist_r), 0, dist_r)) %>%
left_join(votes_state) %>%
  filter(district != '00') %>%
  select(-state_other)

votes_df <- votes_df %>%
  mutate(diff_d = state_d - dist_d,
         diff_r = state_r - dist_r)

### calc rms differences; compare diffs for R and D categories, should
### be similar...
votes_df_diffs <- votes_df %>%
  group_by(state_ab) %>%
  summarize(diff_r_rms = (sum(dist_tot * diff_r^2)/sum(dist_tot))^.5) %>%
  left_join(votes_df %>%
              group_by(state_ab) %>%
              summarize(diff_d_rms = (sum(dist_tot * diff_d^2)/sum(dist_tot))^.5),
            by = 'state_ab') %>%
  mutate(check_d_vs_r = (diff_d_rms - diff_r_rms)^2) %>%
  left_join(votes_state, by = 'state_ab')

plot(diff_r_rms ~ diff_d_rms, votes_df_diffs)

votes_dist_geom <- votes_df %>%
  full_join(districts %>%
              select(statefp, district = cd115fp, area_km2, aspect_ratio),
            by = c('statefp', 'district')) %>%
  full_join(votes_df_diffs %>%
              select(statefp, diff_r_rms, diff_d_rms),
            by = 'statefp')
