library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

dir_goal <- '~/github/ohibc/prep/spp_ico/v2016'
x <- read_csv(file.path(dir_goal, 'output/spp_status.csv'))
y <- read_csv(file.path(dir_goal, 'output/spp_status_nobirds.csv'))
z <- read_csv(file.path(dir_goal, 'output/spp_status_3nm.csv'))

df <- x %>%
  mutate(scenario = 'region') %>%
  bind_rows(y %>%
              mutate(scenario = 'nobirds')) %>%
  bind_rows(z %>%
              mutate(scenario = '3nm')) %>%
  left_join(x %>% rename(rgn_status = score), by = 'rgn_id')

### generic theme for all plots
ggtheme_basic <- theme(axis.ticks = element_blank(),
                       text = element_text(family = 'Helvetica', color = 'gray30', size = 8),
                       plot.title = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
                       legend.position = 'right')

ggtheme_plot <- ggtheme_basic +
  theme(panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        panel.background = element_blank())

rgn_names <- foreign::read.dbf(file.path(dir_goal, '../../spatial/ohibc_rgn.dbf')) %>%
  select(rgn_id, rgn_name)

df1 <- df %>%
  mutate(diff = score - rgn_status,
         scenario = factor(scenario, levels = c('region', '3nm', 'nobirds'))) %>%
  # filter(scenario != 'region') %>%
  left_join(rgn_names, by = 'rgn_id') %>%
  arrange(scenario)

df2 <- df1 %>% filter(scenario == 'region') %>%
  mutate(scenario = as.character(scenario))

scatterplot <- ggplot(df1, aes(x = rgn_name, y = score, color = scenario)) +
  ggtheme_plot +
  theme(panel.grid.major.y = element_blank(),
        axis.line.x = element_line(colour = "grey30"),
        axis.line.y = element_line(colour = "grey30")) +
  geom_line(data = df2,
            aes(y = score, group = scenario),
            size = 1) +
  geom_point(size = 2) +
  # geom_hline(yintercept = 0, color = 'grey30') +
  scale_color_manual(values = c('region' = 'grey60',
                                'nobirds' = '#4466ff',
                                '3nm' = '#445544')) +
  labs(x = '',
       y = 'SPP score',
       color = 'scenario') +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

ggsave(file.path(dir_goal, 'plot_scenarios.png'), width = 4, height = 3)


birds_only <- spp_all %>%
  filter(spp_group == 'BOTW' & !is.na(cat_score)) %>%
  filter(iucn_sid %in% iucn_cells_spp$iucn_sid)
mean(birds_only$cat_score)
# [1] 0.1754967
mean(spp_all$cat_score, na.rm = TRUE)
# [1] 0.05939031
# non-area weighted, BC birds are less healthy than global species average

iucn_cells_birds <- iucn_cells_spp %>%
  filter(iucn_sid %in% birds_only$iucn_sid) %>%
  group_by(iucn_sid) %>%
  summarize(n_cells = n()) %>%
  left_join(asdf %>% select(iucn_sid, cat_score), by = 'iucn_sid') %>%
  ungroup() %>%
  mutate(area_cat = n_cells*cat_score/max(n_cells))

iucn_cells_all <- iucn_cells_spp %>%
  left_join(spp_all %>% select(iucn_sid, cat_score), by = 'iucn_sid') %>%
  group_by(iucn_sid, cat_score) %>%
  summarize(n_cells = n()) %>%
  ungroup() %>%
  mutate(area_cat = cat_score*n_cells/max(n_cells))

iucn_cells_birds$area_cat %>% mean()
# [1] 0.06629762
iucn_cells_all$area_cat %>% mean(na.rm = TRUE)
# [1] 0.007293561

### Area weighted extinction risk category is significantly higher for BC birds than BC species in general

bc_lm <- lm(cat_score ~ n_cells, data = iucn_cells_birds)

bird_cats <- ggplot(iucn_cells_birds, aes(x = n_cells, y = cat_score)) +
  ggtheme_plot +
  geom_jitter(height = .05, width = 0, col = 'blue', alpha = .6) +
  geom_abline(intercept = coef(bc_lm)[1], slope = coef(bc_lm)[2], color = 'red')

ggsave(file.path(dir_goal, 'bird_cats.png'), width = 4, height = 3)

