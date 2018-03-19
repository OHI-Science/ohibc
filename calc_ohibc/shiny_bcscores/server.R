
library(shiny)

### load data here


shinyServer(function(input, output, session)  {

  observeEvent(input$target, {
    elements <- target_elements %>%
      filter(target == input$target) %>%
      .$target_element


    updateSelectInput(session, inputId = "element",
                      choices = elements)
  })

  output$scores_plot <- renderPlot({
    goalname <- input$target
    element <- input$element
    show_status <- 'status' %in% input$dimensions
    show_trend  <- 'trend'  %in% input$dimensions
    show_score  <-  'score' %in% input$dimensions
    show_prs <-  'pressures' %in% input$dimensions
    show_res <-  'resilience' %in% input$dimensions
    show_lfs <-  'future' %in% input$dimensions
    show_prs_layers <- input$show_layers == 'prs'
    show_res_layers <- input$show_layers == 'res'
    fix_y <- input$fix_y

    generate_plot(goalname    = goalname,
                  element     = element,
                  show_status = show_status,
                  show_trend  = show_trend,
                  show_score  = show_score,
                  show_prs    = show_prs,
                  show_res    = show_res,
                  show_lfs    = show_lfs,
                  show_prs_layers = show_prs_layers,
                  show_res_layers = show_res_layers,
                  fix_y       = fix_y)
  }, height = 800)

  output$goal_desc <- reactiveUI(function(){
    file_to_show = file.path('pages', paste0(input$target, '.md'))
    print(file_to_show)

    includeMarkdown(file_to_show)
  })

})
