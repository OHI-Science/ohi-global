### server.R

source('server_fxns.R')

server <- shinyServer(function(input, output, session) {
  

  ### For Trend v Status tab:  upon selecting a georegion and/or checkbox:

  ### For Species map tab:  upon a change in the spp_map$df OR the input$show_maps,
  ### create the map to show AM, IUCN, or Both
  # create_map <- observeEvent(
  #   {spp_map$df; input$show_maps}, 
  #     ### triggered by change in spp_map$am_rast (which may also include 
  #     ### change in iucn_rast) or a change in the selected maps to display
  #   {
  #     message('observed change in spp_map$df or input$show_maps; creating map')
  #     message('input$show_maps = ', input$show_maps, '; input$species = ', input$species)
  #     map_rast <- get_rast(spp_map$df, type = input$show_maps)
  #     map_obj  <- assemble_map_tmap(map_rast, spp = input$species)
  #     
  #     spp_map$map <- map_obj
  #   }
  # )
  
  # output$compare_map <- renderPlot({
  #   spp_map$map
  # }) 
  # 
  ### For Map Alignment tab
  output$tvs_plot <- renderPlotly({
    
    if(input$tvs_georegion == 'Global') {
      tvs_plot <- create_tvs_plot_global(input$tvs_colors)
    } else {
      tvs_plot <- create_tvs_plot_georgn(input$tvs_georegion, input$tvs_colors)
    }
    
    tvs_plot
    
  })
  
  output$rankchange_plot <- renderPlotly({
    
    if(input$rankchange_georgn == 'Global') {
      rankchange_plot <- create_rankchange_plot_global(input$rankchange_colors)
    } else {
      rankchange_plot <- create_rankchange_plot_georgn(input$rankchange_georgn, input$rankchange_colors)
    }
    
    rankchange_plot
    
  })
  
  output$fig2_plot <- renderPlotly({
    
    fig2_plot <- create_fig2_plot(input$fig2_show_all)
    
  })
  
  
  # 
  # output$barchart <- renderPlot({
  #   create_barchart(input$expert_rev)
  # })
  # 
  # output$mini_quad <- renderPlot({
  #   create_miniquad(input$species)
  # })
  # 
  # ### For Coral Depth tab
  # output$coral_quad <- renderPlot({
  #   create_coralquad(input$coral_spp)
  # })
  # 
  # output$coral_map <- renderPlot({
  #   create_coral_map(input$coral_spp)
  # })
  
  output$table_display <- renderDataTable({
      read_csv(file.path('tables', paste0(input$table_file, '.csv'))) 
    },
    escape = FALSE)
})
