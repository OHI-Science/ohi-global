### server.R

source('server_fxns.R')

server <- shinyServer(function(input, output, session) {
  
  ### Species Map Function ###
  spp_map   <- reactiveValues()
  coral_map <- reactiveValues()
  ### This function takes a single species scientific name as input, then grabs 
  ### all occurrence cells and associated probability per cell
  
  ### for Species Map tab: upon selecting a group, update species list choices
  observeEvent(input$spp_group, 
    {
      message('Observed change in spp_group; updating select input')
      spp_choices <- spp_list %>%
        filter(spp_group_text == input$spp_group) %>%
        distinct() %>%
        .$name %>%
        sort()
      updateSelectInput(session, inputId = "species",
                        choices = spp_choices)
    }
  )
  
  ### For Species map tab:  upon selecting a species, get map cells
  observeEvent(
    {input$species}, 
    {
      message('observed change in input$species; getting spp_map dataframe')
      spp_map$df <- get_spp_map_df(input$species)
      print(head(spp_map$df))
    }
  )
  
  ### For Species map tab:  upon a change in the spp_map$df OR the input$show_maps,
  ### create the map to show AM, IUCN, or Both
  create_map <- observeEvent(
    {spp_map$df; input$show_maps}, 
      ### triggered by change in spp_map$am_rast (which may also include 
      ### change in iucn_rast) or a change in the selected maps to display
    {
      message('observed change in spp_map$df or input$show_maps; creating map')
      message('input$show_maps = ', input$show_maps, '; input$species = ', input$species)
      map_rast <- get_rast(spp_map$df, type = input$show_maps)
      map_obj  <- assemble_map_tmap(map_rast, spp = input$species)
      
      spp_map$map <- map_obj
    }
  )
  
  output$compare_map <- renderPlot({
    spp_map$map
  }) 
  
  ### For Map Alignment tab
  output$quad_plot <- renderPlotly({
    create_quadplot(input$taxa_quad, input$expert_rev)
  })
  
  output$barchart <- renderPlot({
    create_barchart(input$expert_rev)
  })
  
  output$mini_quad <- renderPlot({
    create_miniquad(input$species)
  })

  ### For Coral Depth tab
  output$coral_quad <- renderPlot({
    create_coralquad(input$coral_spp)
  })
  
  output$coral_map <- renderPlot({
    create_coral_map(input$coral_spp)
  })
  
  # output$coral_barchart <- renderPlot({
  #   create_coral_barchart()
  # })
  
  
})
