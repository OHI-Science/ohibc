CalculateAllBC <- function (conf, layers) {

  if (exists('scores', envir = .GlobalEnv)) {
    rm(scores, envir = .GlobalEnv)
  }

  if ('Setup' %in% ls(conf$functions)) {
    message('Running Setup()...\n')
    conf$functions$Setup()
  }

  goals_X <- conf$goals %>%
    dplyr::filter(!is.na(preindex_function)) %>%
    dplyr::arrange(order_calculate)

  scores <- data.frame(year      = integer(0),
                       goal      = character(0),
                       dimension = character(0),
                       region_id = integer(0),
                       score     = numeric())

  for (i in 1:nrow(goals_X)) {
    ### i <- 1

    goal_i  <- goals_X$goal[i]

    message('Calculating Status and Trend for each region for ', goal_i,
            ' using function call: \n  ', goals_X$preindex_function[i])

    assign('scores', scores, envir = conf$functions)

    if (scores %>%
        filter(goal == goal_i & dimension %in% c('status', 'trend')) %>%
        nrow() != 0) {
      stop(sprintf('Scores were assigned to goal %s by previous goal function.', goal_i))
    }

    scores_g <- eval(parse(text = goals_X$preindex_function[i]),
                     envir = conf$functions)

    if (!all(c('status', 'trend') %in% unique(scores_g$dimension))) {
      stop(sprintf('Missing "status" or "trend" dimension in %s goal model\n', goal_i))
    }

    if (!all(unique(scores_g$dimension) %in% c('status', 'trend'))) {
      stop(sprintf('"status" and "trend" should be the only dimensions in %s goal model\n', goal_i))
    }

    if (nrow(scores_g) > 0) {
      scores <- scores %>%
        bind_rows(scores_g %>%
                select(year, goal, dimension, region_id, score))
    }
  }

  scores_P <- CalculatePressuresAll(layers, conf)
  scores   <- bind_rows(scores, scores_P)
  scores_R <- CalculateResilienceAll(layers, conf)
  scores   <- bind_rows(scores, scores_R)
  # scores   <- data.frame(scores)
  goals_G  <- as.character(scores %>%
                             filter(dimension == 'status') %>%
                             .$goal %>%
                             unique())

  for (goal_i in goals_G) {
    ### goal_i <- goals_G[1]

    message('Calculating Goal Score and Likely Future for each region for ', goal_i, '...')

    scores_wide <- scores %>%
      dplyr::filter(goal == goal_i) %>%
      tidyr::spread(dimension, score)

    for (col in c('status', 'trend', 'pressures', 'resilience')) {
      if (!col %in% names(scores_wide)) {
        warning('  missing ', col, ' dimension, assigning NA!')
        scores_wide[col] <- NA
      }
    }

    x <- CalculateGoalIndex(id       = scores_wide$region_id,
                            status   = scores_wide$status/100,
                            trend    = scores_wide$trend,
                            resilience = scores_wide$resilience/100,
                            pressure = scores_wide$pressures/100,
                            DISCOUNT = conf$config$goal_discount,
                            BETA     = conf$config$goal_beta,
                            default_trend = conf$config$default_trend)

    x$score <- x$score * 100
    x$xF <- x$xF * 100

    scores_G <- x %>%
      dplyr::select(region_id = id, future = xF, score) %>%
      tidyr::gather(dimension, score, -region_id) %>%
      dplyr::mutate(goal = goal_i) %>%
      dplyr::select(goal, dimension, region_id, score)

    scores <- bind_rows(scores, scores_G)
  }

  goals_Y    <- conf$goals %>%
    filter(!is.na(postindex_function))
  supragoals <- conf$goals %>%
    filter(is.na(parent)) %>%
    .$goal
  supragoals

  for (i in 1:nrow(goals_Y)) {
    message('Calculating post-Index function for each region for ', goals_Y$goal[i], '...')
    assign('scores', scores, envir = conf$functions)
    scores <- eval(parse(text = goals_Y$postindex_function[i]),
                   envir = conf$functions)
  }

  message('Calculating Index Score for each region using goal weights to combine goal scores...')

  scores <- bind_rows(scores,
                  scores %>%
                    dplyr::filter(dimension == 'score', goal %in% supragoals) %>%
                    merge(conf$goals %>%
                            dplyr::select(goal, weight)) %>%
                    dplyr::mutate(weight = as.numeric(weight)) %>%
                    dplyr::group_by(region_id) %>%
                    dplyr::summarise(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
                    dplyr::mutate(goal = 'Index',
                                  dimension = 'score') %>%
                    data.frame())

  message('Calculating Index Likely Future State for each region...')

  scores <- bind_rows(scores,
                  scores %>%
                    dplyr::filter(dimension == 'future', goal %in% supragoals) %>%
                    merge(conf$goals %>%
                            dplyr::select(goal, weight)) %>%
                    dplyr::group_by(region_id) %>%
                    dplyr::summarise(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
                    dplyr::mutate(goal = 'Index',
                                  dimension = 'future') %>%
                    data.frame())

  if ('PreGlobalScores' %in% ls(conf$functions)) {
    message('Calculating Post-process PreGlobalScores() function for each region...')
    scores <- conf$functions$PreGlobalScores(layers, conf, scores)
  }

  message('Calculating scores for ASSESSMENT AREA (region_id=0) by area weighting...')
  scores <- bind_rows(scores,
                  scores %>%
                    dplyr::filter(dimension %in% c('score', 'status', 'future')) %>%
                    merge(SelectLayersData(layers, layers = conf$config$layer_region_areas, narrow = TRUE) %>%
                            dplyr::select(region_id = id_num, area = val_num)) %>%
                    dplyr::group_by(goal, dimension) %>%
                    dplyr::summarise(score = weighted.mean(score, area, na.rm = TRUE), region_id = 0) %>%
                    ungroup())

  if ('FinalizeScores' %in% ls(conf$functions)) {
    message('Calculating FinalizeScores function...')
    scores <- conf$functions$FinalizeScores(layers, conf,
                                           scores)
  }

  stopifnot(sum(duplicated(scores[, c('region_id', 'goal',
                                      'dimension')])) == 0)

  return(scores)

}
