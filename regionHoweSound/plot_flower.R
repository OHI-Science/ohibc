plot_flower <- function(score_df,
                        score_ref   = 100,    ### scale from 0-1 or 0-100? default is 0-100
                        outline     = TRUE,   ### show the outer borders; default is yes indeedy
                        filename    = NULL,   ### give it a file name to save the plot; default is no save
                        center_text = NULL, ### pass it a number or label; default is blank
                        incl_center_score = TRUE, ### overridden if center_text != NULL
                        incl_goal_labels = TRUE, ### show goal labels? FALSE hides the goal labels
                        incl_legend = TRUE, ### show the legend? FALSE hides the legend
                        show_plot   = TRUE) {

  ### set up positions for the bar centers:
  ### cumulative sum of weights (incl current) minus half the current weight
  score_df <- score_df %>%
    mutate(score   = score * 100/score_ref,   ### if 0-1, turn into 0-100; otherwise leave as is
           pos     = cumsum(weight) - 0.5 * weight,
           pos_end = last(pos) + 0.5 * last(weight))

  score_df_na <- score_df %>%
    mutate(score = ifelse(is.na(score), 100, NA))

  p_labels <- score_df$goal
  p_breaks <- score_df$pos
  p_limits <- c(0, score_df$pos_end[1])
  p_score  <- round(weighted.mean(score_df$score, score_df$weight, na.rm = TRUE), 0)

  ### some parameters for the plot:
  blank_circle_dia <- 100
  light_line <- 'grey90'
  white_fill <- 'white'
  light_fill <- 'grey80'
  med_line   <- 'grey50'
  med_fill   <- 'grey52'
  dark_line  <- 'grey20'
  dark_fill  <- 'grey22'

  ### set up basic plot parameters
  plot_obj <- ggplot(data = score_df,
                     aes(x = pos, y = score, fill = score, width = weight))

  if(outline) {
    plot_obj <- plot_obj +
      ### sets up the background/borders to the external boundary (100%) of plot:
      geom_bar(aes(y = 100),
               stat = 'identity', color = light_line, fill = white_fill, size = .2) +
      geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight),
                    size = 0.5, color = light_line, show.legend = NA) +
      ### lays any NA bars on top of background, with darker grey:
      geom_bar(data = score_df_na, aes(x = pos, y = score),
               stat = 'identity', color = light_line, fill = light_fill, size = .2)
  }

  ### establish the basics of the flower plot...
  plot_obj <- plot_obj +
    ### plots the actual scores on top of background/borders:
      geom_bar(stat = 'identity', color = dark_line, size = .2) +
      geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                    size = 0.5, color = dark_line, show.legend = NA) +
    ### plot zero as a baseline:
      geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                    size = 0.5, color = dark_line, show.legend = NA) +
    ### turns linear bar chart into polar coordinates:
      coord_polar(start = - score_df$pos[1]/score_df$pos_end[1] * 2 * pi) +
    ### sets petal colors to the red-yellow-blue color scale:
      scale_fill_gradientn(colors = brewer.pal(n = 11, name = 'RdYlBu'), limits = c(0, 100),
                           breaks = seq(0, 100, 20), labels = seq(0, 100, 20)) +
    ### uses weights to assign widths to petals:
      scale_x_continuous(labels = p_labels, breaks = p_breaks, limits = p_limits) +
    ### setting the limits to a negative leaves an open hole in the middle (bars go from zero outward)
    ### if including goal labels, extend outer limits to make room for them.
      scale_y_continuous(limits = c(-blank_circle_dia, ifelse(incl_goal_labels, 150, 100)))


  ### fill the center?
  ###   if center text is available use it; if not, see if center_score is desired
  if(!is.null(center_text)) {
    plot_obj <- plot_obj +
      geom_text(aes(label = center_text), x = 0, y = -blank_circle_dia,
                hjust = .5, vjust = .5,
                color = dark_line)
  } else if(incl_center_score) {
    plot_obj <- plot_obj +
      geom_text(aes(label = p_score), x = 0, y = -blank_circle_dia,
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
    goal_labels <- score_df %>%
      mutate(goal_label_y = ifelse(outline, 150, max_score + 50))
    plot_obj <- plot_obj +
      geom_text(data = goal_labels,
                aes(label = goal, x = pos, y = goal_label_y),
                hjust = .5, vjust = .5,
                color = dark_line)
  }

  ### include or exclude the legend
  if(!incl_legend) {
    plot_obj <- plot_obj +
      theme(legend.position = 'none')
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
