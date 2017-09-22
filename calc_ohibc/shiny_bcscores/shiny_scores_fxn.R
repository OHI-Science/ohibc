
library(tidyverse)
library(RColorBrewer)
library(stringr)

### generic theme for all plots
ggtheme_plot <- function(base_size = 9) {
  theme(axis.ticks = element_blank(),
        text             = element_text(family = 'Helvetica', color = 'gray30', size = base_size),
        plot.title       = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
        panel.background = element_blank(),
        legend.position  = 'right',
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90', size = .25),
        # panel.grid.major = element_blank(),
        legend.key       = element_rect(colour = NA, fill = NA),
        axis.line        = element_blank()) # element_line(colour = "grey30", size = .5))
}

# dir_calc   <- file.path('~/github/ohibc/calc_ohibc')
#
# all_scores <- read_csv(file.path(dir_calc, 'scores_all.csv')) %>%
#   group_by(goal, dimension) %>%
#   complete(region_id = c(1:8)) %>%
#   ungroup() %>%
#   filter(region_id != 0) %>%
#   filter(!goal %in% c('FP', 'BD', 'ES', 'SP', 'Index')) %>%
#   spread(dimension, score) %>%
#   left_join(get_rgn_names(), by = c('region_id' = 'rgn_id'))
#
# all_layer_files <- read_csv(file.path(dir_calc, 'explore/layers_full_list.csv')) %>%
#   filter(!str_detect(layer, 'element_wt')) %>%
#   filter(!is.na(dir_prep)) %>%
#   select(layer, target, target_element, dir_prep, filename, name_data_fld, dimension) %>%
#   distinct()
#
# target_elements <- all_layer_files %>%
#   filter(dimension %in% c('resilience', 'pressure')) %>%
#   select(target, target_element) %>%
#   unique()
#
# prs_res_files <- all_layer_files %>%
#   filter(dimension %in% c('resilience', 'pressure'))
#
# get_prs_res_layers <- function(file_df) {
#   # file_df <- all_layer_files; dimen = 'pressure'
#
#   prs_res_files <- file_df %>%
#     select(layer, dir_prep, filename, name_data_fld, dimension) %>%
#     mutate(dir_prep = str_replace(dir_prep, 'ohibc:', '~/github/ohibc/')) %>%
#     distinct()
#
#   prs_res_data <- lapply(seq_along(prs_res_files$layer), FUN = function(i) {
#       ### i <- 17
#       x <- read_csv(file.path(prs_res_files$dir_prep[i], prs_res_files$filename[i])) %>%
#         rename_(value = prs_res_files$name_data_fld[i]) %>%
#         mutate(dimension = prs_res_files$dimension[i])
#     }) %>%
#     setNames(prs_res_files$layer) %>%
#     bind_rows(.id = 'layer') %>%
#     select(layer, rgn_id, year, value, dimension)
#
#   prs_res_data <- prs_res_data %>%
#     filter(is.na(year)) %>%
#     group_by(layer, rgn_id) %>%
#     mutate(year = 2016) %>%
#     complete(year = 2000:2016) %>%
#     fill(value, dimension, .direction = 'up') %>%
#     ungroup() %>%
#     bind_rows(prs_res_data %>%
#                 filter(!is.na(year) & year >= 2000)) %>%
#     mutate(value = 100 * value) ### value for both prs and res is rescaled 0-100
#
#   return(prs_res_data)
# }
#
# prs_res_data <- get_prs_res_layers(prs_res_files) %>%
#   left_join(get_rgn_names(), by = 'rgn_id') %>%
#   filter(!is.na(rgn_id))
#
# write_csv(all_scores,      'data/all_scores.csv')
# write_csv(target_elements, 'data/target_elements.csv')
# write_csv(prs_res_data,    'data/prs_res_data.csv')
# write_csv(prs_res_files,   'data/prs_res_files.csv')

all_scores      <- read_csv('data/all_scores.csv') %>%
  mutate(trend1 = (trend + 1) * 50)
  ### trend1 is the rescaled trend, so values go from 0 (trend = -1) to
  ### 100 (trend = +1); then a second axis is placed to scale it
target_elements <- read_csv('data/target_elements.csv')
prs_res_data    <- read_csv('data/prs_res_data.csv')
prs_res_files   <- read_csv('data/prs_res_files.csv')

generate_plot <- function(goalname,
                          element = NA,
                          show_status = TRUE,
                          show_trend  = FALSE,
                          show_score  = FALSE,
                          show_prs = FALSE,
                          show_res = FALSE,
                          show_lfs = FALSE,
                          show_prs_layers = FALSE,
                          show_res_layers = FALSE,
                          fix_y = FALSE) {

  # goalname <- all_scores$goal[1]
  scores_tmp <- all_scores %>%
    # filter(!is.na(score)) %>%
    filter(goal == goalname)

  valid_rgns <- scores_tmp %>%
    .$region_id %>% unique()

  if(show_prs_layers | show_res_layers) dim_alpha = .2
  else dim_alpha = .6

  all_plot <- ggplot(scores_tmp,
                     aes(x = year, y = pressure, color = layer)) +
    ggtheme_plot(base_size = 14) +
    theme(axis.text.x = element_text(angle = 75)) +
    scale_x_continuous(breaks = scores_tmp$year[scores_tmp$year %% 5 == 0] %>%
                         unique() %>%
                         sort()) +
    guides(colour = guide_legend(override.aes = list(size = 3))) +
    facet_wrap( ~ rgn_name) +
    labs(x = 'year',
         y = goalname)

  if(fix_y) {
    all_plot <- all_plot +
      scale_y_continuous(limits = c(0, 100))
  }


  if(show_prs) {
    all_plot <- all_plot +
      geom_line(aes(y = pressures),
                color = 'red4',   alpha = dim_alpha, size = 1.5)
  }

  if(show_res) {
    all_plot <- all_plot +
      geom_line(aes(y = resilience),
                color = 'green4', alpha = dim_alpha, size = 1.5)
  }

  if(show_score) {
    all_plot <- all_plot +
      geom_line(aes(y = score),
                color = 'grey30', alpha = dim_alpha, size = 2)
  }

  if(show_lfs) {
    all_plot <- all_plot +
      geom_line(aes(y = future),
                color = 'grey30', alpha = dim_alpha, size = 1.5, linetype = '1111')
  }

  if(show_trend) {
    message('in show_trend')
    all_plot <- all_plot +
      geom_hline(yintercept = 50, color = 'red', alpha = dim_alpha) +
      geom_line(aes(y = trend1),
                color = 'blue4', alpha = dim_alpha, size = 1.5, linetype = '2121') +
      scale_y_continuous(sec.axis = sec_axis(~ . / 50 - 1, name = 'Trend -1 to +1'),
                         limits   = c(0, 100))
  }

  if(show_status) {
    all_plot <- all_plot +
      geom_line(aes(y = status),
                color = 'blue4', alpha = dim_alpha, size = 2)
  }

  if(show_prs_layers | show_res_layers) {
    dimen <- ifelse(show_prs_layers, 'pressure', 'resilience')
    prs_res_tmp <- prs_res_files %>%
      filter(target == goalname) %>%
      filter(dimension == dimen) %>%
      inner_join(prs_res_data, by = c('layer')) %>%
      select(layer, target_element, rgn_id, rgn_name, year, value) %>%
      filter(rgn_id %in% valid_rgns) %>%
      distinct()

    if(!(is.na(element) | element == 'NA')) {
      prs_res_tmp <- prs_res_tmp %>%
        filter(target_element == element)
        color_label <- paste0(dimen, ': ', goalname, '\n', element)
    } else color_label <- paste0(dimen, ': ', goalname)

    print(prs_res_tmp %>% select(layer, target_element) %>% distinct())


    all_plot <- all_plot +
      geom_line(data = prs_res_tmp,
                aes(group = layer, x = year, y = value, color = layer), alpha = .8, size = 1) +
      scale_color_manual(values = c(brewer.pal(n = 8, name = 'Dark2'),
                                    brewer.pal(n = 8, name = 'Pastel2'),
                                    'purple4', 'orange4', 'blue4')) +
      labs(color = color_label)
  }

  return(all_plot)
}

