
source('kobe_shiny_fxns.R')

ggtheme_plot <- theme(panel.border     = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_line(colour = 'gray90'),
                      panel.background = element_blank(),
                      axis.line        = element_blank(),
                      axis.ticks       = element_blank(),
                      text             = element_text(family = 'Helvetica', color = 'gray30', size = 12),
                      plot.title       = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
                      legend.position  = 'right',
                      legend.key       = element_rect(colour = NA, fill = NA))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ### set up some variables
  kobe <- reactiveValues() ### container for kobe plots and dataframe


  ### If a limit slider is changed, repopulate raw dataframe immediately
  kobe_df_raw <- reactive({
    message('in kobe_df_raw()')
    reso <- .01

    f_fmsy_extent <- round(input$f_fmsy_max, 1) + .1
    b_bmsy_extent <- round(input$b_bmsy_max, 1)
    message('Observed change in F/Fmsy max or B/Bmsy max; regenerating grid')

    f_fmsy_vec = rep(seq(0, f_fmsy_extent, reso), each  = round(b_bmsy_extent/reso) + 1)
    b_bmsy_vec = rep(seq(0, b_bmsy_extent, reso), times = round(f_fmsy_extent/reso) + 1)

    message('x cols: ', length(f_fmsy_vec))
    message('y rows: ', length(b_bmsy_vec))

    kobe$df_raw <- data.frame(f_fmsy = f_fmsy_vec,
                              b_bmsy = b_bmsy_vec)
  })

  kobe_hcr_df <- reactive({
    message('recalculating harvest control rule endpoint')
    kobe_hcr_df <- data.frame(b_bmsy = c(0, .4, .8, input$b_bmsy_max),
                              f_fmsy = c(0,  0,  1, 1))
  })

  kobe_dataframe <- reactive({
    message('input$f_b_mean = ', input$f_b_mean, '; class = ', class(input$f_b_mean))
    message('input$f_b_weighting = ', input$f_b_weighting, '; class = ', class(input$f_b_weighting))
    message('calculating kobe dataframe')

    f_b_weighting <- input$f_b_weighting
    df <- kobe_df_raw() %>%
      rescale_bprime_crit(overfished_th  = 0.8,
                          underfished_th = input$underfished_th,
                          bmax           = input$b_bmsy_max,
                          bmax_val       = input$bmax_val) %>%
      rescale_fprime_crit(bcrit = 0.4,
                          underfishing_th = 0.8, overfishing_th  = 1.2,
                          overfished_th   = 0.8,
                          fmax            = input$f_fmsy_max,
                          fmin_val        = input$fmin_val)
    if(input$f_b_mean == 'geom') {
      df <- df %>%
        mutate(x_mean = fPrime^(1 - f_b_weighting) * bPrime^(f_b_weighting),
               x_mean = x_mean^(1/(max(f_b_weighting, 1 - f_b_weighting))))
    } else {
      df <- df %>%
        mutate(x_mean = fPrime*(1 - f_b_weighting) + bPrime*(f_b_weighting))
    }
  })

  output$kobe_plot <- renderPlot({
    ggplot(data = kobe_dataframe(), aes(x = b_bmsy, y = f_fmsy)) +
      ggtheme_plot +
      scale_fill_distiller(palette = 'RdYlGn', direction = 1, limits = c(0, 1)) +
      scale_x_continuous(limits = c(0, input$b_bmsy_max)) +
      scale_y_continuous(limits = c(-.1, input$f_fmsy_max + .1)) +
      annotate(geom = 'text', label = 'critical', x = .15, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'cautious', x =  .5, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'healthy',  x = 1, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'underexploited',  x = 1.8, y = -.05,
               size = 3,
               color = 'grey20') +
      geom_raster(alpha = .8, aes(fill = x_mean)) +
      labs(title = 'FIS as function of B/Bmsy, F/Fmsy',
           fill = 'FIS',
           x = 'B/Bmsy',
           y = 'F/Fmsy') +
      geom_line(data = kobe_hcr_df(), aes(x = b_bmsy, y = f_fmsy), color = 'black', size = 1.5, alpha = .6)
  })

  output$fPrime_plot <- renderPlot({
    ggplot(data = kobe_dataframe(), aes(x = b_bmsy, y = f_fmsy)) +
      ggtheme_plot +
      scale_fill_distiller(palette = 'RdYlGn', direction = 1, limits = c(0, 1)) +
      scale_x_continuous(limits = c(0, input$b_bmsy_max)) +
      scale_y_continuous(limits = c(-.1, input$f_fmsy_max + .1)) +
      annotate(geom = 'text', label = 'critical', x = .15, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'cautious', x =  .5, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'healthy',  x = 1, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'underexploited',  x = 1.8, y = -.05,
               size = 3,
               color = 'grey20') +
      geom_raster(alpha = .8, aes(fill = fPrime)) +
      labs(title = "F' as function of B/Bmsy, F/Fmsy",
           fill = "F'",
           x = 'B/Bmsy',
           y = 'F/Fmsy') +
      geom_line(data = kobe_hcr_df(), aes(x = b_bmsy, y = f_fmsy), color = 'black', size = 1.5, alpha = .6)
  })

  output$bPrime_plot <- renderPlot({
    ggplot(data = kobe_dataframe(), aes(x = b_bmsy, y = f_fmsy)) +
      ggtheme_plot +
      scale_fill_distiller(palette = 'RdYlGn', direction = 1, limits = c(0, 1)) +
      scale_x_continuous(limits = c(0, input$b_bmsy_max)) +
      scale_y_continuous(limits = c(-.1, input$f_fmsy_max + .1)) +
      annotate(geom = 'text', label = 'critical', x = .15, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'cautious', x =  .5, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'healthy',  x = 1, y = -.05,
               size = 3,
               color = 'grey20') +
      annotate(geom = 'text', label = 'underexploited',  x = 1.8, y = -.05,
               size = 3,
               color = 'grey20') +
      geom_raster(alpha = .8, aes(fill = bPrime)) +
      labs(title = "B' as function of B/Bmsy",
           fill = "B'",
           x = 'B/Bmsy',
           y = 'F/Fmsy') +
      geom_line(data = kobe_hcr_df(), aes(x = b_bmsy, y = f_fmsy), color = 'black', size = 1.5, alpha = .6)
  })

})
