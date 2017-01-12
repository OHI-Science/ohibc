#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(tidyverse)

library(shinythemes)

shinyUI(navbarPage("OHI", theme = shinytheme('cerulean'),
    tabPanel("OHI Modified Kobe Plot",
      fluidRow(
        column(3,
          h6('Combine F and B rescaled scores as a weighted mean.
             Adjust the parameters for F and B on separate tabs.'),
          radioButtons('f_b_mean',
                       label = "Mean calculation method",
                       choices = c('Weighted additive mean' = 'arith',
                                   'Weighted geometric mean' = 'geom'),
                       selected = 'geom'),
          sliderInput("f_b_weighting",
                      "Weighting between F and B in final calc",
                      min = 0,
                      max = 1,
                      step = .1,
                      value = .5),
          h6('A value of 0 means only consider F; 1 means only consider B;
             0.5 means weight both equally')
        ),

        # Show a plot of the generated distribution
        column(9,
           plotOutput("kobe_plot")
        )
      )
    ),

    tabPanel("B' vs. B/Bmsy",
      fluidRow(
        column(3,
               h5("Biomass score and rescaling"),
               h6("B/Bmsy max: B/Bmsy for unfished stock"),
               h6("This could be determined as a general rule across all
                  stocks, or set at a per-stock level (depending on data)."),
               sliderInput("b_bmsy_max",
                           "B/Bmsy max",
                           min = 1,
                           max = 5,
                           step = .1,
                           value = 3.0),
               h6("B' score at max B/Bmsy"),
               h6("B' = 0 at B/Bmsy max means an unfished stock earns
                  a score of zero. A non-zero value reduces the
                  penalty for underfished stocks."),
               sliderInput("bmax_val",
                           "B' score for unfished",
                           min = 0,
                           max = 1,
                           step = .1,
                           value = 0),
               h6("B/Bmsy underfished threshold"),
               h6("This value determines where a stock starts to be considered
                  'underfished' and thus incurs a penalty."),
               sliderInput("underfished_th",
                           "B/Bmsy threshold for underfished",
                           min = 0.8,
                           max = 2.5,
                           step = .1,
                           value = 1.5),
               hr()#,
               # actionButton('calc2',
               #              'Calculate plot')
        ),
        column(9,
               plotOutput('bPrime_plot')
        )
      )
    ),

    tabPanel("F' vs. B/Bmsy, F/Fmsy",
      fluidRow(
        column(3,
               h5('Fishing Pressure parameters'),
               h6("F/Fmsy max acceptable sustained fishing pressure"),
               h6("Based on a ", strong("rolling multi-year average"),
                  "of fishing pressure, to penalize", strong('sustained'),
                  "high/low sustained fishing pressures."),
               sliderInput("f_fmsy_max",
                           "F/Fmsy max",
                           min = 1,
                           max = 3.5,
                           step = .1,
                           value = 2),
               h6("F' score at zero fishing pressure"),
               h6("1 = no underfishing penalty"),
               sliderInput("fmin_val",
                           "F' value for no fishing",
                           min = 0,
                           max = 1,
                           step = .1,
                           value = 0),
               hr()#,
               # actionButton('calc3',
               #              'Calculate plot')
        ),
        column(9,
               plotOutput('fPrime_plot')
        )
      )
    )
  )
)
