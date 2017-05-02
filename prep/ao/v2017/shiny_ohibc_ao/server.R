### server.R

source('server_fxns.R')

server <- shinyServer(function(input, output, session) {

  output$shi_plot <- renderPlot({
    calc_shi(input$shi_ref, input$shi_years, input$shi_running) %>%
      create_shi_plot(input$shi_show_status, input$shi_years)
  })

  output$lic_plot <- renderPlot({
    calc_lic(input$lic_ref, input$lic_target_val) %>%
      create_lic_plot(input$lic_show_status)
  })

  output$clos_plot <- renderPlot({
    calc_clos(input$clos_ref, input$clos_target_val) %>%
      create_clos_plot(input$clos_show_status)
  })

})
