library(dplyr)
library(tidyr)
library(ggplot2)

status_df <- data.frame('rgn_id' = rep(c('low', 'med', 'high'), each = 5),
                        'year'   = rep(2011:2015, times = 3),
                        'status' = c(6:10, 46:50, 86:90),
                        'type'   = 'status')

trend_df <- status_df %>%
  group_by(rgn_id) %>%
  do(slope = lm(status ~ year, .)) %>%
  mutate(trend = coef(slope)[2] * 5)

lfs_df <- status_df %>%
  left_join(trend_df, by = 'rgn_id') %>%
  group_by(rgn_id) %>%
  summarize(year = 2020,
            lfs_lin = last(status) + last(trend),
            lfs_pct = last(status) * (1 + last(trend)/100),
            lfs_linB = last(status) + .67*last(trend),
            lfs_pctB = last(status) * (1 + .67*last(trend)/100)) %>%
  gather(type, status, lfs_lin:lfs_pctB) %>%
  bind_rows(status_df) %>%
  mutate(rgn_id = factor(rgn_id, levels = c('high', 'med', 'low'))) %>%
  left_join(data.frame(type = c('lfs_lin', 'lfs_linB', 'lfs_pct', 'lfs_pctB'),
                       nx   = c(    1,          1,        -1,          -1),
                       ny   = c(  .25,          0,       .25,        -.25)),
            by = 'type')

lfs_plot <- ggplot(lfs_df, aes(x = year, y = status, color = rgn_id)) +
  geom_abline(slope = 1,
              intercept = c(6-2011, 46-2011, 86-2011),
              color = 'red',
              alpha = .5) +
  geom_point() +
  coord_cartesian(xlim = c(2010:2022)) +
  geom_text(data = lfs_df %>%
              filter(year == 2020),
            aes(label = type,
                x = year + nx,
                y = status + ny),
            hjust = 0.5,
            color = 'grey20',
            size = 3) +
  facet_wrap(~ rgn_id, ncol = 1, scales = 'free_y') +
  theme(legend.position = 'none')


lfs_plot
