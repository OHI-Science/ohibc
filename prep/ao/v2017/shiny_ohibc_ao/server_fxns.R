### server_fxns.R
library(stringr)
library(RColorBrewer)

### copy data from output to local
data_files <- list.files('../output', full.names = TRUE)
if(length(data_files) > 0) {
  file.copy(from = data_files,
            to = file.path('data', basename(data_files)),
            overwrite = TRUE)
}

shi_data  <- read_csv('data/ao_spawn_hab_index.csv')
clos_data <- read_csv('data/ao_closures.csv')
lic_data  <- read_csv('data/ao_licenses.csv')
rgn_names <- read_csv('data/rgn_names.csv')

# create a blank ggplot theme
# ggtheme_basic <- function(textsize = 10) {
#   theme_bw() +
#   theme_update(panel.grid.minor = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.background = element_blank(),
#           plot.background = element_blank(),
#           panel.border = element_blank(),
#           axis.line = element_blank(),
#           axis.ticks = element_blank(),
#           text = element_text(size = textsize),
#           plot.title = element_text(size = textsize * 1.5))
# }

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


### Spawn habitat index
calc_shi <- function(ref_type, ref_yrs, running_mean) {
  ### * SHI vs historical reference point of mean or median between given years
  shi_hist_ref <- shi_data %>%
    filter(year %in% ref_yrs[1]:ref_yrs[2]) %>%
    group_by(rgn_id) %>%
    summarize(shi_ref_pt = ifelse(ref_type == 'mean',
                                  mean(shi_tot, na.rm = TRUE),
                                  median(shi_tot, na.rm = TRUE))) %>%
    ungroup()

  shi_status <- shi_data %>%
    left_join(shi_hist_ref, by = 'rgn_id') %>%
    group_by(rgn_id) %>%
    arrange(rgn_id, year) %>%
    mutate(shi_3yr_mean = (shi_tot + lag(shi_tot, 1) + lag(shi_tot, 2)) / 3) %>%
    rowwise() %>%
    mutate(status = if_else(running_mean == TRUE,
                            shi_3yr_mean / shi_ref_pt,
                            shi_tot / shi_ref_pt),
           status = 100 * ifelse(status > 1, 1, status)) %>%
    ungroup()

  return(shi_status)
}

create_shi_plot <- function(shi_status, show_status, ref_yrs) {

  stat_scale <- 100 / max(shi_status$shi_3yr_mean, na.rm = TRUE)
  shi_status <- shi_status %>%
    mutate(status_adj = status / stat_scale)

  shi_plot <- ggplot(shi_status, aes(x = year, y = status)) +
    # ggtheme_basic() +
    ggtheme_plot() +
    annotate('rect', xmin = ref_yrs[1], xmax = ref_yrs[2],
             ymin = 0, ymax = Inf, fill = 'red', alpha = .1)


  if('shi_tot' %in% show_status) {
    shi_plot <- shi_plot +
      geom_line(aes(y = shi_tot), color = 'grey40', alpha = .8)
  }
  if('shi_3yr' %in% show_status) {
    shi_plot <- shi_plot +
      geom_line(aes(y = shi_3yr_mean), color = 'blue4', alpha = .8)
  }
  if('shi_status' %in% show_status) {
    shi_plot <- shi_plot +
      scale_y_continuous(sec.axis =  sec_axis( ~ . * stat_scale, name = 'Status (red)')) +
      geom_line(aes(y = status_adj), color = 'red4', alpha = .8)
  }

  shi_plot <- shi_plot +
    geom_hline(aes(yintercept = shi_ref_pt), color = 'darkgreen', alpha = .5) +
    labs(y = 'SHI total (grey) and 3-yr mean (blue)') +
    facet_wrap( ~ rgn_name, scales = 'free_y')

  return(shi_plot)
}


### Calculate status for each component
### Closures:
### * proportion of year open for access; 0 closures = 100%
# closure_status <- closures %>%
#   complete_years(year_span) %>%
#   mutate(days_in_year = ifelse(lubridate::leap_year(year), 366, 365),
#          status = 100 * (1 - (days_avg / days_in_year)),
#          component = 'shellfish_closures') %>%
#   select(year, region_id, status, component)
#


### Licenses
calc_lic <- function(ref_type, target_val) {
  ### ref_type <- 'lic_target'; target_val = .25
  lic_ref <- switch(ref_type,
                    'lic_target' = target_val,
                    'lic_mean'   = mean(lic_data$pct_fn, na.rm = TRUE),
                    'lic_max'    = max(lic_data$pct_fn,  na.rm = TRUE))

  lic_status <- lic_data %>%
    group_by(rgn_id) %>%
    mutate(lic_ref_pt = lic_ref,
           status = pct_fn / lic_ref,
           status = 100 * ifelse(status > 1, 1, status)) %>%
    ungroup() %>%
    left_join(rgn_names, by = 'rgn_id')

  return(lic_status)
}

create_lic_plot <- function(lic_status, show_status) {

  lic_status <- lic_status %>%
    mutate(pct_fn = pct_fn * 100) %>%
    filter(rgn_id != 7)

  lic_plot <- ggplot(lic_status, aes(x = year, y = pct_fn)) +
    # ggtheme_basic()
    ggtheme_plot()

  if('lic_pct' %in% show_status) {
    lic_plot <- lic_plot +
      geom_line(aes(y = pct_fn), color = 'grey40', alpha = .8)
  }
  if('lic_status' %in% show_status) {
    lic_plot <- lic_plot +
      scale_y_continuous(sec.axis =  sec_axis( ~ ., name = 'Status (red)')) +
      geom_line(aes(y = status), color = 'red4', alpha = .8)
  }

  lic_ref <- lic_status$lic_ref_pt[1] * 100
  lic_plot <- lic_plot +
    geom_hline(aes(yintercept = lic_ref), color = 'darkgreen', alpha = .5) +
    labs(y = 'Pct Licenses to First Nations (grey)') +
    facet_wrap( ~ rgn_name, scales = 'free_y')

  return(lic_plot)
}

### Calculate status for each component
### Closures:
### * proportion of year open for access; 0 closures = 100%
# closure_status <- closures %>%
#   complete_years(year_span) %>%
#   mutate(days_in_year = ifelse(lubridate::leap_year(year), 366, 365),
#          status = 100 * (1 - (days_avg / days_in_year)),
#          component = 'shellfish_closures') %>%
#   select(year, region_id, status, component)
#


### Licenses
calc_clos <- function(ref_type, target_val) {
  ### ref_type <- 'clos_target'; target_val = 250
  clos_status <- clos_data %>%
    group_by(rgn_id) %>%
    mutate(days_in_year = ifelse(lubridate::leap_year(year), 366, 365),
           open_ref_pt  = ifelse(ref_type == 'clos_pct', days_in_year, target_val),
           days_open    = days_in_year - days_avg,
           status       = days_open / open_ref_pt,
           status       = 100 * ifelse(status > 1, 1, status)) %>%
    ungroup() %>%
    left_join(rgn_names, by = 'rgn_id')

  return(clos_status)
}

create_clos_plot <- function(clos_status, show_status) {

  clos_status <- clos_status %>%
    filter(!is.na(rgn_name)) %>%
    mutate(status_adj = status * 366 / 100)

  clos_plot <- ggplot(clos_status, aes(x = year, y = days_avg)) +
    # ggtheme_basic()
    ggtheme_plot()

  if('clos_n' %in% show_status) {
    clos_plot <- clos_plot +
      geom_line(aes(y = days_open), color = 'grey40', alpha = .8)
  }
  if('clos_status' %in% show_status) {
    clos_plot <- clos_plot +
      scale_y_continuous(sec.axis =  sec_axis( ~ . / 366 * 100, name = 'Status (red)')) +
      geom_line(aes(y = status_adj), color = 'red4', alpha = .8)
  }

  open_ref <- clos_status$open_ref_pt[1]

  clos_plot <- clos_plot +
    geom_hline(aes(yintercept = open_ref), color = 'darkgreen', alpha = .5) +
    labs(y = 'Number days open (grey)') +
    facet_wrap( ~ rgn_name, scales = 'fixed')

  return(clos_plot)
}

