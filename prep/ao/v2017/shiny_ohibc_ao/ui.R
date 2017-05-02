#ui.r

library(tidyverse)
library(plotly)
library(shinythemes)

ui <- navbarPage('First Nations Resource Access Opportunities',
                 theme = shinytheme('cerulean'),

  tabPanel('Spawn Habitat Index',
    sidebarPanel(
      radioButtons('shi_ref', label = 'SHI Reference',
                   choices = list('Mean over time'   = 'shi_mean',
                                  'Median over time' = 'shi_median'),
                   selected = 'shi_mean'),
      sliderInput('shi_years', 'SHI ref year span',
                  min = 1940, max = 2016, value = c(1940, 1960), step = 1),
      checkboxInput('shi_running', 'SHI 3-year running mean?', value = TRUE),
      checkboxGroupInput('shi_show_status', 'Show on plot?',
                         choices = c('SHI total'     = 'shi_tot',
                                     'SHI roll mean' = 'shi_3yr',
                                     'SHI status'    = 'shi_status'),
                         selected = c('shi_3yr', 'shi_status'))
    ),
    mainPanel(
      plotOutput('shi_plot')
    )
  ),

  tabPanel('FN Licenses',
           sidebarPanel(
             radioButtons('lic_ref', label = 'FN Licenses Reference',
                          choices = list('Target'         = 'lic_target',
                                         'Mean over time' = 'lic_mean',
                                         'Max over time'  = 'lic_max'),
                          selected = 'lic_max'),
             sliderInput('lic_target_val', 'License %FN target',
                         min = 0, max = 1, value = .25, step = .05),
             checkboxGroupInput('lic_show_status', 'Show on plot?',
                                choices = c('Lic %'      = 'lic_pct',
                                            'Lic status' = 'lic_status'),
                                selected = c('lic_pct', 'lic_status'))
           ),
           mainPanel(
             plotOutput('lic_plot')
           )
  ),

  tabPanel('Closures',
           sidebarPanel(
             radioButtons('clos_ref', label = 'Closure reference',
                          choices = list('% of year open'   = 'clos_pct',
                                         'Target open days' = 'clos_target'),
                          selected = 'clos_pct'),
             sliderInput('clos_target_val', '# of open days',
                         min = 0, max = 365, value = 365, step = 5),
             checkboxInput('clos_seasonal', 'Include seasonal closures', value = TRUE),
             checkboxGroupInput('clos_show_status', 'Show on plot?',
                                choices = c('Closures'       = 'clos_n',
                                            'Closure status' = 'clos_status'),
                                selected = c('clos_n', 'clos_status'))
           ),
           mainPanel(
             plotOutput('clos_plot')
           )
  )
)
