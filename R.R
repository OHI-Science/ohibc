library(dplyr)
library(tidyr)
library(ggplot2)

z <- data.frame('z' = 1, 'score_i' = seq(1, 3.5, length.out = 51)) %>%
  full_join(data.frame('z' = 1, 'd' = seq(.2, 1.2, length.out = 6))) %>%
  select(-z)

z <- z %>%
  mutate(score_f = score_i - d,
         score_f = ifelse(score_f < 1, 1, score_f),
         d_max = score_i - 1,
         d_max = ifelse(d_max < 0, 0, d_max),
         n_loss = ifelse(d_max == 0, 1, (score_i - score_f)/d_max),
         decr = factor(d))

plotx <- ggplot(z, aes(y = n_loss, group = decr)) +
  geom_line(aes(x = score_i, color = decr)) +
  geom_line(aes(x = score_f, color = decr), linetype = 2) +
  labs(x = 'Aragonite sat state',
       y = 'Normalized loss (aka pressure score)',
       color = 'decr in sat state',
       title = 'OA pressure score as normalized loss') +
  annotate('text', x = 2.5, y = .9, hjust = 0, vjust = 0,
           label = 'solid = initial state, dotted = final state',
           size = 3)

plotx

