## Mel's color palette ----
ohi_reds <-  grDevices::colorRampPalette(
  c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090"),
  space="Lab")(65)
ohi_blues <-  grDevices::colorRampPalette(
  c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"))(35)

ohi_palette <- c(ohi_reds, ohi_blues)

goal_labels <- c('AO' = 'First Nations Resource Access Opportunity',
           'HAB' = 'BD: Habitats', 'SPP' = 'BD: Species', 'BD' = 'Biodiversity',
           'CPP' = 'HS: Coastal Protection', 'CSS' = 'HS: Carbon Storage', 'HS' = 'Habitat Services',
           'CW' = 'Clean Waters',
           'FIS' = 'FP: Wild-capture Fisheries', 'MAR' = 'FP: Mariculture',
           'SAL' = 'FP: Salmon', 'FP' = 'Food Provision',
           'ICO' = 'SP: Iconic Species', 'LSP' = 'SP: Lasting Special Places', 'SP' = 'Sense of Place',
           'LVF' = 'LV: First Nations Livelihoods', 'LVO' = 'LV: Non-First Nations Livelihoods', 'LV' = 'Livelihoods',
           'TR' = 'Tourism and Recreation')


plot_flower <- function(scores,
                        score_size = 12,
                        plot_clean = FALSE,
                        rgn_name = NULL) {
  ### Start with a scores df for the desired region, year, and dimension

  # ## if there are multiple years in the dataset and no year_plot argument,
  # ## the most recent year of data is dplyr::selected
  # if(is.na(year_plot)){
  #   scores <- scores %>%
  #     filter(year == max(year))
  # }
  #
  # ## filters the region of interest, otherwise all goals are printed
  # if ( !any(is.na(region_plot)) ){
  #   scores <- scores %>%
  #     filter(region_id %in% region_plot)
  # }

  # ## filter only score dimension
  # if(is.na(dim_plot)) {
  #   scores <- scores %>%
  #     filter(dimension == 'score')
  # } else {
  #   scores <- scores %>%
  #     filter(dimension == dim_plot)
  # }

  # labeling:: Index score for center labeling before join with conf
  score_index <- scores %>%
    filter(goal == "Index") %>%
    dplyr::select(region_id, score) %>%
    mutate(score = round(score))


  ## unique regions to plot
  # region_plots <- unique(scores$region_id)


  ## goals.csv configuration info----

  ## read in conf/goals.csv, start dealing with supra goals
  conf <-  read_csv(file.path(dir_calc, 'master/goals_master.csv'))
  goals_supra <- na.omit(unique(conf$parent))
  supra_lookup <- conf %>%
    filter(goal %in% goals_supra) %>%
    dplyr::select(parent = goal, name_supra = name)

  ## extract conf info for labeling
  conf <- conf %>%
    left_join(supra_lookup, by = 'parent') %>%
    filter(!(goal %in% goals_supra)) %>%
    dplyr::select(goal, order_color, order_hierarchy,
                  weight, name_supra, name_flower) %>%
    mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    arrange(order_hierarchy)

  ## join scores and conf ----
  score_df <- scores %>%
    inner_join(conf, by = "goal") %>%
    arrange(order_color)


  ## set up positions for the bar centers:
  ## cumulative sum of weights (incl current) minus half the current weight
  score_df <- score_df %>%
    group_by(region_id) %>%
    mutate(pos   = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
    mutate(pos_end = sum(weight)) %>%
    ungroup() %>%
    group_by(name_supra) %>%
    ## calculate position of supra goals before any unequal weighting (ie for FP)
    mutate(pos_supra  = ifelse(!is.na(name_supra), mean(pos), NA)) %>%
    ungroup() %>%
    filter(weight != 0) %>%
    ## set up for displaying NAs
    mutate(plot_NA = ifelse(is.na(score), 100, NA))


  ## create supra goal dataframe for position and labeling ----
  supra <- score_df %>%
    mutate(name_supra = ifelse(is.na(name_supra), name_flower, name_supra)) %>%
    mutate(name_supra = paste0(name_supra, "\n"),
           name_supra  = gsub("Coastal", "", name_supra, fixed = TRUE)) %>%
    dplyr::select(name_supra, pos_supra) %>%
    unique() %>%
    as.data.frame()

  ## calculate arc: stackoverflow.com/questions/38207390/making-curved-text-on-coord-polar ----
  supra_df <- supra %>%
    mutate(myAng = seq(-70, 250, length.out = dim(supra)[1])) %>%
    filter(!is.na(pos_supra))


  ## more labeling and parameters ----
  goal_labels <- score_df %>%
    dplyr::select(goal, name_flower)

  p_limits <- c(0, score_df$pos_end[1])
  blank_circle_rad <- 42
  light_line <- 'grey90'
  white_fill <- 'white'
  light_fill <- 'grey80'
  med_line   <- 'grey50'
  med_fill   <- 'grey52'
  dark_line  <- 'grey20'
  dark_fill  <- 'grey22'




  ## filenaming for labeling and saving ----
  # region_names_all <- bind_rows(
  #   data_frame(                             ## order regions to start with whole study_area
  #     region_id   = 0,
  #     region_name = 'OHIBC'),
  #   read_csv('spatial/regions_list.csv') %>%
  #     dplyr::select(region_id   = rgn_id,
  #                   region_name = rgn_name)) %>%
  #   mutate(flower_png = sprintf('%s/flower_%s.png',
  #                               dir_fig_save,
  #                               str_replace_all(region_name, ' ', '')))
  # ## write out filenames
  # readr::write_csv(region_names_all, 'reports/figures/regions_figs.csv')

  ## move into for loop only with region_names to plot
  # region_names <- region_names_all %>%
  #   filter(region_id %in% region_plots) %>%  ## filter only regions to plot
  #   distinct()                              ## in case region_id 0 was included in regions_list.csv


  ## loop through to save flower plot for each region ----
  # for (region in region_plots) { # region = 301

  ## filter region info, setup to plot ----
  plot_df <- score_df # %>%
    # filter(region_id == region)
  plot_score_index <- score_index # %>%
    # filter(region_id == region)

  # ## fig_name to save
  # fig_save <- region_names$flower_png[region_names$region_id == region]

  # ## labeling:: region name for title
  # region_name <- region_names %>%
  #   filter(region_id == region) %>%
  #   dplyr::select(region_name)

  ## set up basic plot parameters ----
  plot_obj <- ggplot(data = plot_df,
                     aes(x = pos, y = score, fill = score, width = weight))

  ## sets up the background/borders to the external boundary (100%) of plot
  plot_obj <- plot_obj +
    geom_bar(aes(y = 100),
             stat = 'identity', color = light_line, fill = NA, size = .2) +
    geom_bar(aes(y = -blank_circle_rad),
             stat = 'identity', color = 'white', fill = 'white', size = .2) +
    geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight),
                  size = 0.5, color = light_line, fill = NA, show.legend = NA)

  ## lays any NA bars on top of background, with darker grey:
  if(any(!is.na(plot_df$plot_NA))) {
    plot_obj <- plot_obj +
      geom_bar(aes(x = pos, y = plot_NA),
               stat = 'identity',
               color = med_line, fill = med_fill, alpha = .4,
               size = .2)
  }

  ## establish the basics of the flower plot
  plot_obj <- plot_obj +
    ## plot the actual scores on top of background/borders:
    geom_bar(stat = 'identity', color = dark_line, size = .2,
             show.legend = FALSE) +
    ## emphasize edge of petal
    geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                  size = 0.5, color = dark_line, show.legend = NA) +
    ## plot zero as a baseline:
    geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                  size = 0.5, color = dark_line, show.legend = NA) +
    ## turn linear bar chart into polar coordinates start at 90 degrees (pi*.5)
    coord_polar(start = pi * 0.5) +
    ## set petal colors to the red-yellow-blue color scale:
    scale_fill_gradientn(colours=ohi_palette, na.value="black",
                         limits = c(0, 100)) +
    ## use weights to assign widths to petals:
    scale_x_continuous(labels = plot_df$goal, breaks = plot_df$pos, limits = p_limits) +
    scale_y_continuous(limits = c(-blank_circle_rad,
                                  ifelse(plot_clean == TRUE,
                                         100, 160))) +
    theme(plot.margin = unit(c(0, 0, 0, 0), units = 'cm'))


  ## add center number and title
  plot_obj <- plot_obj +
    geom_text(data = score_index,
              inherit.aes = FALSE,
              aes(label = plot_score_index$score),
              x = 0, y = -blank_circle_rad,
              hjust = .5, vjust = .5,
              size = score_size,
              color = dark_line)

  if(!is.null(title)) {
    plot_obj <- plot_obj +
      labs(title  = rgn_name)
  }


  ### clean up the theme
  plot_obj <- plot_obj +
    ggtheme_plot() +
    theme(panel.grid.major = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank()) +
    theme(plot.title = element_text(hjust = .5, color = 'grey15'))

  ## add goal names
  if(!plot_clean) {
    plot_obj <- plot_obj +
      geom_text(aes(label = name_flower, x = pos, y = 125),
                hjust = .5, vjust = .5,
                size = 3,
                color = dark_line)


  ## position supra arc and names. x is angle, y is distance from center
  supra_rad  <- 155  ## supra goal radius from center

  plot_obj <- plot_obj +
    ## add supragoal arcs
    geom_errorbar(data = supra_df, inherit.aes = FALSE,
                  aes(x = pos_supra, ymin = supra_rad, ymax = supra_rad),
                  size = 0.25, show.legend = NA) +
    geom_text(data = supra_df, inherit.aes = FALSE,
              aes(label = name_supra, x = pos_supra, y = supra_rad, angle = myAng),
              hjust = .5, vjust = .5,
              size = 3,
              color = dark_line)
  }

  ### display/save options: print to graphics, save to file
  # print(plot_obj)

  ## save plot
  # ggsave(filename = fig_save,
  #        plot = plot_obj,
  #        device = "png",
  #        height = 6, width = 8, units = 'in', dpi = 300)


  return(plot_obj)
  ### ...then return the plot object for further use
  # return(invisible(plot_obj)) ## can't return with this for loop

}
