plot_flower_diff <- function(diff_df,
                             score_cols  = c('score1', 'score2'), ### col hdgs for scores - initial to final
                             score_ref   = 100,    ### scale from 0-1 or 0-100? default is 0-100
                             outline     = FALSE,   ### show the outer borders; default is nope
                             filename    = NULL,   ### give it a file name to save the plot; default is no save
                             center_text = NULL, ### pass it a number or label; default is blank
                             incl_center_diff = TRUE, ### overridden if center_text != NULL
                             incl_goal_labels  = TRUE, ### show goal labels? FALSE hides the goal labels
                             incl_legend = TRUE, ### show the legend? FALSE hides the legend
                             show_plot   = TRUE) {

  ### rename old and new score columns so they're easier to work with...
  tmp_names <- names(diff_df)
  names(diff_df)[tmp_names == score_cols[1]] <- 'score1'
  names(diff_df)[tmp_names == score_cols[2]] <- 'score2'

  ### set up positions for the bar centers:
  ### cumulative sum of weights (incl current) minus half the current weight

  diff_df1 <- diff_df %>%
    ungroup() %>%
    mutate(diff     = score2 - score1,
           diff     = diff * 100/score_ref,   ### if 0-1, turn into 0-100; otherwise leave as is
           score1   = score1 * 100/score_ref,   ### if 0-1, turn into 0-100; otherwise leave as is
           score2   = score2 * 100/score_ref,   ### if 0-1, turn into 0-100; otherwise leave as is
           pos      = cumsum(weight) - 0.5 * weight,
           pos_end  = last(pos) + 0.5 * last(weight))

  p_labels <- diff_df1$goal
  p_breaks <- diff_df1$pos
  p_limits <- c(0, diff_df1$pos_end[1])
  p_diff  <- round(weighted.mean(diff_df1$diff, diff_df1$weight, na.rm = TRUE), 2) %>%
    ifelse(. > 0, paste0("+", .), .)
  diff_df_na <- diff_df1 %>%
    mutate(diff = ifelse(is.na(diff), 100, NA))

  diff_df2 <- diff_df1 %>%
    mutate(diff_dir = ifelse(diff > 0, 'increase', 'no change'),
           diff_dir = ifelse(diff < 0, 'decrease', diff_dir),
           diff     = abs(diff),
           diff_ref = ifelse(score1 > score2, score2, score1),
           diff_end = score2) %>%
    gather(delta, value, c(diff_ref, diff)) %>%
    mutate(diff_dir = ifelse(delta == 'diff_ref', 'baseline', diff_dir),
           diff_label = factor(diff_dir, levels = c('baseline', 'decrease', 'increase', 'no change'))) %>%
    select(goal, weight, pos, pos_end, value, diff_label, diff_end)



  ### some parameters for the plot:
  blank_circle_dia <- 100
  light_line <- 'grey90'
  white_fill <- 'white'
  light_fill <- 'grey80'
  med_line   <- 'grey50'
  med_fill   <- 'grey52'
  dark_line  <- 'grey20'
  dark_fill  <- 'grey22'

  diff_df_blank <- diff_df2 %>% filter(diff_label == 'baseline')

  ### set up basic plot parameters.  Because of weirdness between
  ### scale_y_continuous and a stacked bar chart, I am seeding the ggplot
  ### with non-stacked data to avoid the issue. Then when plotting the
  ### actual data bars, I need to manually set data = diff_df2 and then the aes()
  plot_obj <- ggplot(data = diff_df_blank,
                     aes(x = pos, width = weight))

  if(outline) {
    plot_obj <- plot_obj +
      ### sets up the background/borders to the external boundary (100%) of plot:
      geom_bar(aes(y = 100),
               stat = 'identity', color = light_line, fill = white_fill, size = .2) +
      geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight),
                    size = 0.5, color = light_line, show.legend = NA) +
      ### lays any NA bars on top of background, with darker grey:
      geom_bar(data = diff_df_na, aes(x = pos, y = diff),
               stat = 'identity', color = light_line, fill = light_fill, size = .2)
  }

  ### establish the basics of the flower plot...
  plot_obj <- plot_obj +
    ### plots the actual diffs on top of background/borders:
      geom_bar(data = diff_df2,
               aes(x = pos, y = value, width = weight, fill = diff_label),
               stat = 'identity',
               color = med_line, alpha = 0.8, size = .2) +
    ### plot the final score as a darker bar:
      geom_errorbar(data = diff_df2,
                    aes(x = pos, ymin = diff_end, ymax = diff_end, width = weight),
                    size = 0.5, color = dark_line, show.legend = NA) +
    ### plot the baseline (score = 0) as a darker bar:
      geom_errorbar(data = diff_df2,
                    aes(x = pos, ymin = 0, ymax = 0, width = weight),
                    size = 0.2, color = dark_line, show.legend = NA) +
    ### turns linear bar chart into polar coordinates:
      coord_polar(start = - diff_df2$pos[1]/diff_df2$pos_end[1] * 2 * pi) +
    ### sets petal colors to the red-yellow-blue color scale:
      scale_fill_manual(breaks = c('decrease', 'increase', 'no change'),
                        values = c('baseline'  = med_fill,
                                   'decrease'  = 'red3',
                                   'no change' = 'grey50',
                                   'increase'  = 'green3')) +
    ### uses weights to assign widths to petals:
      scale_x_continuous(labels = p_labels, breaks = p_breaks, limits = p_limits) +
    ### setting the limits to a negative leaves an open hole in the middle (bars go from zero outward)
    ### if including goal labels, extend outer limits to make room for them.
      scale_y_continuous(breaks = NULL,
                         minor_breaks = NULL,
                         limits = c(-blank_circle_dia, ifelse(incl_goal_labels, 150, 100)))


  ### fill the center?
  ###   if center text is available use it; if not, see if center_diff is desired
  if(!is.null(center_text)) {
    plot_obj <- plot_obj +
      geom_text(aes(label = center_text), x = 0, y = -blank_circle_dia,
                hjust = .5, vjust = .5,
                color = dark_line)
  } else if(incl_center_diff) {
    plot_obj <- plot_obj +
      geom_text(aes(label = p_diff), x = 0, y = -blank_circle_dia,
                hjust = .5, vjust = .5,
                color = dark_line)
  }

  ### clean up the theme
  plot_obj <- plot_obj +
    ggtheme_plot +
    theme(panel.grid.major = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank())


  ### include or exclude goal labels; dynamic if no border
  if(incl_goal_labels) {
    ### if no outline, labels go near bars; otherwise place near outer edge
    goal_labels <- diff_df2 %>%
      filter(diff_label == 'baseline') %>%
      mutate(value = ifelse(is.na(value), 0, value),
             diff_end = ifelse(is.na(diff_end), 0, diff_end),
             max_score = ifelse(diff_end > value, diff_end, value),
             diff_label_y = ifelse(outline, 150, max_score + 50))
    plot_obj <- plot_obj +
        geom_text(data = goal_labels,
                  aes(label = goal, x = pos, y = diff_label_y),
                  hjust = .5, vjust = .5,
                  color = dark_line)
  }

  ### include or exclude the legend
  if(!incl_legend) {
    plot_obj <- plot_obj +
      theme(legend.position = 'none')
  } else {
    plot_obj <- plot_obj +
      labs(fill = 'change')
  }


  ### display/save options: print to graphics, save to file
  if(show_plot) {
    print(plot_obj)
  }

  if(!is.null(filename)) {
    ggsave(filename = filename,
           height = 6, width = 8, units = 'cm',
           plot = plot_obj)
  }

  ### ...then return the plot object for further use
  return(invisible(plot_obj))

}
