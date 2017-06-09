### server.R

source('server_fxns.R')

server <- shinyServer(function(input, output, session) {
  
  output$fig2_plot <- renderPlotly({
    fig2_plot <- create_fig2_plot(input$fig2_show_all)
  })

  output$fig3_plot <- renderPlotly({
    if(input$fig3_georgn == 'Global') {
      fig3_plot <- create_fig3_plot_global(input$fig3_colors)
    } else {
      fig3_plot <- create_fig3_plot_georgn(input$fig3_georgn, 
                                           input$fig3_colors)
    }
    fig3_plot
  })
  
  output$fig4_plot <- renderPlot({
    fig4_plot <- create_fig4_plot(input$fig4_filter,
                                  input$fig4_georgn,
                                  input$fig4_overall)
    return(fig4_plot)
  })
  
  output$fig4_plot.ui <- renderUI({
    fig4_height <- case_when(
      input$fig4_filter == 'georgn' & input$fig4_georgn == 'Global'   ~ '1500px',
      input$fig4_filter == 'georgn' & input$fig4_georgn == 'Americas' ~ '240px',
      input$fig4_filter == 'georgn' & input$fig4_georgn == 'Southern Islands' ~ '240px',
      TRUE                                                            ~ '400px')
    plotOutput('fig4_plot', height = fig4_height)
  })
  
  # output$fig4_plotly <- renderPlotly({
  #   fig4_plot <- create_fig4_plot(input$fig4_filter, 
  #                                 input$fig4_georgn,
  #                                 input$fig4_overall)
  # })
  
  # output$fig4_plotly.ui <- renderUI({
  #   fig4_height <- ifelse((input$fig4_filter == 'georgn' & input$fig4_georgn == 'Global'),
  #                         '1500px', '400px')
  #   # fig4_height <- '401px'
  #   message('fig4_height = ', fig4_height)
  #   plotlyOutput('fig4_plotly', height = fig4_height)
  # })
  
  output$fig5a_plot <- renderPlotly({
    fig5a_plot <- create_fig5a_plot(input$fig5_colors, 
                                    input$fig5_georgn,
                                    input$fig5_lm)
  })
  output$fig5b_plot <- renderPlotly({
    fig5b_plot <- create_fig5b_plot(input$fig5_colors, 
                                    input$fig5_georgn,
                                    input$fig5_lm)
  })
  output$fig5c_plot <- renderPlotly({
    fig5c_plot <- create_fig5c_plot(input$fig5_colors, 
                                    input$fig5_georgn,
                                    input$fig5_lm)
  })
  output$fig5goal_plot <- renderPlotly({
    message('Here I am calling the rendering the Plotly output for ', input$fig5_goal)
    
    fig5goal_plot <- create_fig5goal_plot(input$fig5_colors, 
                                          input$fig5_georgn,
                                          input$fig5_lm,
                                          input$fig5_goal)
  })
  
  output$fig5goal_modeltext <- reactive(create_fig5goal_text(input$fig5_georgn, input$fig5_goal))
  
  output$fig5goal_plotly.ui <- renderUI({
    if(input$fig5_goal == 'Index') {
      return(list(plotlyOutput('fig5a_plot', height = '300px'), 
                  hr(), 
                  plotlyOutput('fig5b_plot', height = '300px'), 
                  hr(), 
                  plotlyOutput('fig5c_plot', height = '300px')))
    } else {
      message('Here I am calling the plotlyOutput for ', input$fig5_goal)
      return(list(plotlyOutput('fig5goal_plot', height = '400px'),
                  uiOutput('fig5goal_modeltext')
            )
        )
    }
  })
  
  
  
  output$fig6_plot <- renderPlotly({
    if(input$fig6_georgn == 'Global') {
      fig6_plot <- create_fig6_plot_global(input$fig6_colors)
    } else {
      fig6_plot <- create_fig6_plot_georgn(input$fig6_georgn, 
                                           input$fig6_colors)
    }
    fig6_plot
  })
  
  output$table_display <- renderDataTable({
      read_csv(file.path('tables', paste0(input$table_file, '.csv'))) 
    },
    options = list(paging = FALSE),
    escape = FALSE)
  output$table_title <- renderText({
    table_title <- ifelse(input$table_file == 'table1', 
                          'Table 1 Updates to status and trend data and models',
                          'Table 2 Updates to pressure data and models')
  })
})
