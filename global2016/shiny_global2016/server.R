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
  
  output$fig4_plot <- renderPlotly({
    fig4_plot <- create_fig4_plot(input$fig4_filter, 
                                  input$fig4_georgn,
                                  input$fig4_overall)
  })
  
  output$fig4_plot.ui <- renderUI({
    fig4_height <- ifelse((input$fig4_filter == 'georgn' & input$fig4_georgn == 'Global'),
                          '1500px', '400px')
    # fig4_height <- '401px'
    message('fig4_height = ', fig4_height)
    plotlyOutput('fig4_plot', height = fig4_height)
  })
  
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
    escape = FALSE)
})
