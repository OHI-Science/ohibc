source('~/github/ohibc/src/R/common.R')  ### an OHIBC specific version of common.R

scenario <- 'v2017'
goal     <- 'spp_ico'
dir_git  <- '~/github/ohibc'
dir_goal <- file.path(dir_git, 'prep', goal, scenario)
dir_rgn  <- file.path(dir_git, 'prep/spatial')

dir_goal_anx        <- file.path(dir_M, 'git-annex/bcprep/spp_ico', scenario)
dir_goal_global     <- file.path('~/github/ohiprep/globalprep/spp_ico', scenario)
dir_goal_anx_global <- file.path(dir_M, 'git-annex/globalprep/spp_ico', scenario)

dir_bcsee <- file.path(dir_M, 'git-annex/bcprep/_raw_data/bcsee')

xf <- list.files(file.path(dir_bcsee, 'd2017'), pattern = 'xls', full.names = TRUE)

xl <- lapply(xf, FUN = function(x) { # x <- xf[1]

  y <- readxl::read_excel(x,
                          skip = ifelse(str_detect(x, '2009|2008'), 0, 1),
                          col_types = 'text')
  y <- y %>%
    setNames(tolower(names(.))) %>%
    setNames(str_replace_all(names(.), ' ', '_'))
})

xdf <- xl %>%
  setNames(basename(xf)) %>%
  bind_rows(xl, .id = 'from_file')

asdf <- sapply(xl, FUN = function(x) glimpse((x)))
names(asdf) <- basename(xf)

xdf2 <- xdf %>%
  gather(rankyear, rank, ends_with('rank')) %>%
  gather(asdf, st_ch_cmts, contains('status_change_comment')) %>%
  select(from_file,
         sciname = scientific_name, engname = english_name,
         st_ch_cmts,
         rankyear, rank) %>%
  filter(!is.na(rank) & !is.na(st_ch_cmts)) %>%
  filter(str_detect(tolower(rank), '^s')) %>%
  mutate(rankyear = as.integer(str_extract(rankyear, '[0-9]{4}'))) %>%
  filter(from_file != '')

ohibc_spp <- read_csv(file.path(dir_goal, 'int/spp_list_clean.csv'))

status_pr_cat <- data.frame(rank       = c('S5', 'S4', 'S3', 'S2', 'S1', 'SX', 'SH', 'S1S2', 'S2S3', 'S3S4', 'S4S5', 'S', 'S2S4'),
                            rank_score = c( 0.0,  0.2,  0.4,  0.6,  0.8,  1.0,  1.0,   0.7,    0.5,    0.3,    0.1,   NA,   0.4))


bcsee <- ohibc_spp %>%
  left_join(xdf2, by = 'sciname') %>%
  filter(!is.na(pr_score) & !is.na(from_file)) %>%
  mutate(rank = str_replace_all(rank, '[ABNMUR?]', ''),
         rank = str_split(rank, ',')) %>%
  unnest(rank) %>%
  left_join(status_pr_cat, by = c('rank')) %>%
  select(iucn_sid, sciname, rank, rank_score, year = rankyear) %>%
  distinct() %>%
  group_by(iucn_sid, sciname, year) %>%
  filter(!is.na(rank_score)) %>%
  summarize(rank_score = mean(rank_score, na.rm = TRUE)) %>%
  ungroup()

bcsee_comments <- xdf2 %>%
  filter(sciname %in% bcsee$sciname) %>%
  select(sciname, engname, st_ch_cmts) %>%
  distinct()


lm_df <- bcsee %>%
  group_by(sciname) %>%
  do(int   = lm(rank_score ~ year, data = .)[['coefficients']][[1]])
lm_df$int <- unlist(lm_slope$int)

lm_df$slope <- bcsee %>%
  group_by(sciname) %>%
  do(slope  = lm(rank_score ~ year, data = .)[['coefficients']][[2]]) %>%
  .$slope %>% unlist()


bcsee_lm <- bcsee %>%
  group_by(sciname, iucn_sid) %>%
  complete(year = c(2011, 2015)) %>%
  ungroup() %>%
  left_join(lm_df,
            by = 'sciname') %>%
  group_by(sciname) %>%
  mutate(int   = round(int, 4),
         slope = round(slope, 4),
         score = int + slope * year)

ggplot(bcsee, aes(x = year, y = rank_score, col = sciname)) +
  geom_point(size = 2) +
  geom_line(data = bcsee_lm, aes(x = year, y = score, group = sciname, col = sciname))
