### server.R

source('src/server_setup.R')
source('src/server_fxns.R')

server <- shinyServer(function(input, output, session) {
  
  ##### Fig 1: Score maps #####################################################
  
  output$scoremap <- renderImage({
    # message('in output$scoremap: ', input$map_scen, ' ', input$map_goal)
    map_file <- get_map_file(input$map_scen, input$map_goal)
    # message('in output$scoremap, map_file = ', map_file)
    return(list(src = map_file))
  }, deleteFile = FALSE)
  
  output$scorehist <- renderPlot({
    if(input$map_scen == 'annual_change_2016') {
      scorehist <- create_fig1_trend_hist(input$map_goal)
    } else {
      scorehist <- create_fig1_score_hist(input$map_scen, input$map_goal)
    }
    return(scorehist)
  })
  
  ##### Fig 2: Trend by goal  #################################################
  
  output$fig2_plot <- renderPlotly({
    fig2_plot <- create_fig2_plot(input$fig2_show_all)
  })
  
  ##### Fig 3: Trend vs. Score ################################################
  
  output$fig3_plot <- renderPlotly({
    if(input$fig3_georgn == 'Global') {
      fig3_plot <- create_fig3_plot_global(input$fig3_colors)
    } else {
      fig3_plot <- create_fig3_plot_georgn(input$fig3_georgn, 
                                           input$fig3_colors)
    }
    fig3_plot
  })
  
  ##### Fig 4: Trend bar charts ###############################################
  
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
    plotOutput('fig4_plot', height = fig4_height, width = '700px')
  })
  
  ##### Fig 5: Model Eval #####################################################
  
  output$fig5a_plot <- renderPlotly({
    message('rendering fig5a_plot')
    fig5a_plot <- create_fig5_plot(input$fig5_colors, input$fig5_georgn, 
                                   input$fig5_lm, input$fig5_goal,
                                   y_var = 'score_2016', x_var = 'score_2012',
                                   y_lab = 'Observed 2016 score',
                                   x_lab = 'Observed 2012 score')
  })
  
  output$fig5b_plot <- renderPlotly({
    message('rendering fig5b_plot')
    fig5b_plot <- create_fig5_plot(input$fig5_colors, input$fig5_georgn, 
                                   input$fig5_lm, input$fig5_goal,
                                   y_var = 'status_2016', x_var = 'likely_future_state_2012',
                                   x_lab = 'Predicted 2016 status (from 2012 data)', 
                                   y_lab = 'Observed 2016 status')
  })
  output$fig5c_plot <- renderPlotly({
    message('rendering fig5c_plot')
    fig5c_plot <- create_fig5_plot(input$fig5_colors, input$fig5_georgn, 
                                   input$fig5_lm, input$fig5_goal,
                                   y_var = 'obs_change', x_var = 'pred_change', 
                                   y_lab = 'Observed change in status', 
                                   x_lab = 'Predicted change in status',
                                   lim_0_100 = FALSE)
  })
  
  ##### Fig 6: Rank Change ####################################################
  
  output$fig6_plot <- renderPlotly({
    if(input$fig6_georgn == 'Global') {
      fig6_plot <- create_fig6_plot_global(input$fig6_colors)
    } else {
      fig6_plot <- create_fig6_plot_georgn(input$fig6_georgn, 
                                           input$fig6_colors)
    }
    fig6_plot
  })
  
  ##### Data display and download #############################################
  
  output$data_display <- renderDataTable({
    data_view_df <- read_csv('tables/data_view.csv') 
    if(!'rgn' %in% input$data_view)
      data_view_df <- data_view_df %>%
        select(-georegion, -subregion, -country)
    if(!'goal' %in% input$data_view)
      data_view_df <- data_view_df %>%
        select(-goal)
    return(data_view_df)
  },
  options = list(paging = TRUE, pageLength = 100),
  escape = FALSE)
  
  download_df <- reactive({
    get_data_download(input$data_request)
  })
  
  output$data_download <- downloadHandler(
    filename = function() { 
      ifelse('all_vals' %in% input$data_request,
             'ohi_all_dimensions_2012_2016.csv',
             'ohi_scores_2012_2016.csv') },
    content  = function(file) {
      write_csv(download_df(), file)
    }
  )
  
  ##### Table display #########################################################
  
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
