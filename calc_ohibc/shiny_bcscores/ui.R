
library(shiny)
source('shiny_scores_fxn.R')

goals_vec <- c('AO',
           'FIS', 'MAR', 'SAL',
           'CPP', 'CSS',
           'CW',
           'HAB', 'SPP',
           'ICO', 'LSP',
           'LE',
           'TR')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel('OHIBC Scores Visualizer'),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('target',  'Goal:', choices = goals_vec, selected = 'AO'),
      selectInput('element', 'Goal element:', choices = NA, selected = NA),
        ### if goal has elements, populate second dropdown for those elements, selecting the first as default.
      h6('Note: "Goal element" only affects display of pressures/resilience layers, not dimension scores'),
      checkboxGroupInput('dimensions', 'Dimensions:',
                         choices = c('status', 'trend',
                                     'score',
                                     'pressures', 'resilience',
                                     'future'),
                         selected = 'status'),
      radioButtons('show_layers', 'Show pressure/resilience layers?',
                   choices = c('pressure layers' = 'prs', 'resilience layers' = 'res', 'none' = 'none'),
                   selected = 'none'),
      checkboxInput('fix_y', 'Show values 0-100?', value = TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput('scores_plot')
    )
  )
))
