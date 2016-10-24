shinyServer(function(input, output, session) {

  # hide compare ----
  observeEvent(input$sidebarmenu, {
    if (length(y$scenario_dirs) == 1){
      shinyjs::hide(selector = "a[data-value=='compare']")
    }
  })


  # get_scenario ----

  # write remote git commit hash every 5 seconds to compare with remote
  log_gitsha <- observe({
    # Invalidate this observer every 2 seconds (2000 milliseconds)
    invalidateLater(2000, session)

    # write remote git commit sha
    gh_write_remote(y$gh_slug, y$gh_branch_data, remote_sha_txt)
  })

  # monitor file for changes every 1 seconds (1000 milliseconds)
  fileReaderData <- reactiveFileReader(1000, session, remote_sha_txt, readLines)

  # get_scenario ----
  get_scenario = function(env=.GlobalEnv){

    switch(
      input$sel_type,

      # case: output
      output = {
        req(input$sel_output_goal)
        req(input$sel_output_goal_dimension)

        list(
          data = env$scores %>%
            filter(
              goal      == input$sel_output_goal,
              dimension == input$sel_output_goal_dimension) %>%
            select(
              rgn_id = region_id,
              value  = score) %>%
            select(rgn_id, value),
          label = sprintf('%s - %s', input$sel_output_goal, input$sel_output_goal_dimension),
          description = env$dims %>%
            filter(dimension == input$sel_output_goal_dimension)  %>%
            markdownToHTML(text = .$description, fragment.only=T)) },

      # case: input
      input = {
        req(input$sel_input_target_layer)

        env$fld_category = filter(env$layers, layer==input$sel_input_target_layer) %>% .$fld_category
        env$fld_year     = filter(env$layers, layer==input$sel_input_target_layer) %>% .$fld_year

        # get data
        env$data = env$d_lyrs %>%
          filter(layer == input$sel_input_target_layer) %>%
          mutate(
            rgn_id = fld_id_num,
            value  = fld_val_num)

        # if layer has category, filter
        if (!is.na(fld_category)){
          req(input$sel_input_target_layer_category)
          env$data = env$data %>%
            filter(fld_category == input$sel_input_target_layer_category)
        }

        # if layer has category year, filter
        if (!is.na(env$fld_year)){
          req(input$sel_input_target_layer_category_year)
          env$data = env$data %>%
            filter(fld_year == input$sel_input_target_layer_category_year)
        }

        # return list
        list(
          data = env$data %>%
            select(rgn_id, value),
          label = input$sel_input_target_layer,
          description = env$layers %>%
            filter(layer == input$sel_input_target_layer) %>%
            markdownToHTML(text = .$description, fragment.only=T))

    }) # end switch
  }

  ## get_selected() ----
  get_selected = reactive({

    req(input$sel_scenario)
    req(input$sel_type)

    if (input$sel_scenario != scenario){
      load_scenario(input$sel_scenario)
    }

    results = get_scenario()
    if (input$sidebarmenu == 'compare'){
      req(input$sel_scenario_b)
      env_a = new.env()
      env_b = new.env()
      load_scenario(input$sel_scenario  , env=env_a)
      load_scenario(input$sel_scenario_b, env=env_b)
      res_a = get_scenario(env_a)
      res_b = get_scenario(env_b)

      data = res_a$data %>%
        mutate(value_a = value) %>%
        select(rgn_id, value_a) %>%
        left_join(
          res_b$data %>%
            mutate(value_b = value) %>%
            select(rgn_id, value_b),
          by='rgn_id') %>%
        mutate(
          value = value_a - value_b)

      dif_scenario <<- sprintf('%s - %s', input$sel_scenario, input$sel_scenario_b)
      compare = bind_rows(
        res_a$data %>%
          mutate(
            scenario = input$sel_scenario),
        res_b$data %>%
          mutate(
            scenario = input$sel_scenario_b),
        data  %>%
          mutate(
            scenario = dif_scenario)) %>%
        select(rgn_id, scenario, value)

      results = list(
        data = data %>%
          select(rgn_id, value),
        compare = compare,
        label = res_a$label,
        description = res_a$description)
    }

    return(results)
    })

  ## output$ui_sel_output ----
  output$ui_sel_output <- renderUI({
    req(input$sel_output_goal)

    selectInput(
      'sel_output_goal_dimension',
      label    = '3. Choose dimension:',
      choices  = scores %>%
        filter(goal == input$sel_output_goal) %>%
        distinct(dimension) %>%
        .$dimension,
      selected = 'score')

    })

  # output$ui_sel_input ----
  output$ui_sel_input <- renderUI({
    req(input$sel_input_target)

    target_layers = with(
      layers_by_target %>%
        filter(target == input$sel_input_target) %>%
        mutate(label = sprintf('%s: %s', layer, name)),
      setNames(layer, label))

    ui = tagList(selectInput(
      'sel_input_target_layer',
      label    = '3. Choose layer:',
      choices  = target_layers,
      selected = ifelse(
        is.null(input$sel_input_target_layer),
        target_layers[1],
        input$sel_input_target_layer)))

    if (!is.null(input$sel_input_target_layer)){

      fld_category = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_category

      if (!is.na(fld_category)){

        categories = d_lyrs %>%
          filter(layer == input$sel_input_target_layer) %>%
          distinct(fld_category) %>%
          .$fld_category

        ui = tagList(ui, selectInput(
          'sel_input_target_layer_category',
          label    = sprintf('4. Choose %s:', fld_category),
          choices  = categories,
          selected = ifelse(
            is.null(input$sel_input_target_layer_category),
            categories[1],
            input$sel_input_target_layer_category)))

        fld_year = filter(layers, layer == input$sel_input_target_layer) %>% .$fld_year

        if (!is.na(fld_year) & !is.null(input$sel_input_target_layer_category)){
          years = d_lyrs %>%
            filter(
              layer        == input$sel_input_target_layer,
              fld_category == input$sel_input_target_layer_category) %>%
            distinct(fld_year) %>%
            .$fld_year

          ui = tagList(ui, selectInput(
          'sel_input_target_layer_category_year',
          label    = '5. Choose year:',
          choices  = years,
          selected = ifelse(
            is.null(input$sel_input_target_layer_category_year),
            years[1],
            input$sel_input_target_layer_category_year)))  }}}

   return(ui) })

  # output$var_description ----
  # update description of input layer or output goal dimension
  output$var_description = renderText({
    get_selected()$description })

  # output$table ----
  output$table = renderDataTable({

    if (input$sidebarmenu == 'compare'){
      rgns@data %>%
        select(rgn_id, rgn_name) %>%
        left_join(
          get_selected()$compare, by='rgn_id') %>%
        spread(scenario, value)
    } else {
      rgns@data %>%
        select(rgn_id, rgn_name) %>%
        left_join(get_selected()$data, by='rgn_id')
    }
  })

  # Compare sidebar, Plot boxplot ----

  output$ui_sel_scenario_b <- renderUI({
    req(input$sel_scenario)

    if (input$sidebarmenu == 'compare'){

      ui = selectInput(
        'sel_scenario_b',
        label    = '0.B. Choose other scenario:',
        choices  = setdiff(sort(y$scenario_dirs), input$sel_scenario))

    } else {
      ui = NULL
    }
    return(ui)
  })

  observeEvent(input$sidebarmenu, {
    if (input$sidebarmenu != 'compare'){
      updateSelectInput(session, 'sel_scenario', label = '0. Choose scenario:')
    } else {
      updateSelectInput(session, 'sel_scenario', label = '0.A. Choose scenario:')
    }
  })

  output$ui_boxplot <- renderUI({
    if (input$sidebarmenu != 'compare'){
      return(
        'This boxplot is currently only available when choosing to Compare scenarios from sidebar.')
    } else {
      exploding_boxplotOutput('boxplot', width='600px')
    }
  })

  output$boxplot <- renderExploding_boxplot({
    req(input$sidebarmenu, input$sel_scenario, input$sel_scenario_b)

    if (input$sidebarmenu != 'compare') return()

    d = rgns@data %>%
      select(rgn_id, rgn_name) %>%
      left_join(
        get_selected()$compare %>%
          filter(scenario == dif_scenario),
        by='rgn_id')

    #saveRDS(d, 'tmp_d.Rdata')
    #browser()
    #setwd('~/github/ohirepos/inst/app'); d = readRDS('tmp_d.Rdata')

    exploding_boxplot(
      d,
      y = 'value',
      group = 'scenario',
      color = 'scenario',
      label = 'rgn_name')
  })

  # output$map1 ----

  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change, so use reactiveValues
  v <- reactiveValues(hi_id = 0, msg = '', hi_freeze=F) # set default to GLOBAL = 0
  area_global <- round(sum(rgns@data$area_km2))

  output$map1 <- renderLeaflet({

    # get data from selection (get_selected() is the reactive function defined above)
    selected = get_selected()

    # debug
    isolate(v$msg <- paste(now_s(), '-- renderLeaflet()', br(), v$msg))

    # set color palette
    pal = colorNumeric(
      palette = 'RdYlBu',
      domain = selected$data$value)

    # plot map with rgns$id to lookup data$value
    id2col = function(ids, d=selected$data, col_id='rgn_id', col_val='value'){
      pal(d[match(ids, d[[col_id]]), col_val])
    }
    r_v = rgns@data %>% left_join(selected$data, by='rgn_id')

    if(!'map_shrink_pct' %in% names(y)) stop('Missing parameter map_shrink_pct in app.yml.')
    bbox_shrink = function(p, pct){
      b = bbox(p)
      dx = (b[3] - b[1]) * (pct/100)
      dy = (b[4] - b[2]) * (pct/100)
      c(b[1] + dx, b[2] + dy, b[3] - dx, b[4] - dy)
    }
    b = bbox_shrink(rgns, y$map_shrink_pct)

    map_render_mouseover = "
      function(el, t) {
        var defaultStyle = {
          // stroke, ie polygon border
            opacity:     0.5,
            weight:      2,
          // fill, ie polygon interior
            fillOpacity: 0.5,
        };
        var highlightStyle = {
          // stroke, ie polygon border
            opacity:     0.9,
            weight:      6,
          // fill, ie polygon interior
            fillOpacity: 0.9,
        };

        var myMap = this;
        var layers = myMap._layers;
        for(var i in layers) {
          var layer = layers[i];
          if(layer.label) {
            layer.on('mouseover',
              function(e) {
                this.setStyle(highlightStyle);
                // this.bringToFront();
            });
            layer.on('mouseout',
              function(e) {
                this.setStyle(defaultStyle);
                // this.bringToBack();
            });
          }
        }
      }"

    if ('projection' %in% names(y) && y$projection == 'Mollweide'){
      # [leaflet/proj4Leaflet.R#L36-L55 · rstudio/leaflet](https://github.com/rstudio/leaflet/blob/1bc41eebd5220735a309c5b4bcfae6784cc9026d/inst/examples/proj4Leaflet.R#L36-L55)
      # addProviderTiles('Stamen.TonerLite') does not work, so use countries polygons loaded in global.R

      leaflet(
        options =
          leafletOptions(
            crs=leafletCRS(crsClass="L.Proj.CRS", code='ESRI:53009',
                           proj4def= '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs',
                           resolutions = c(65536, 32768, 16384, 8192, 4096, 2048)))) %>%
        addGraticule(style= list(color= '#999', weight= 0.5, opacity= 1)) %>%
        addGraticule(sphere = TRUE, style= list(color= '#777', weight= 1, opacity= 0.25)) %>%
        #addProviderTiles('Stamen.TonerLite') %>% # cannot seem to use tiles with projected crs
        addPolygons(
          data=countries, group = 'land', weight = 1, color = '#4D4D4D') %>% # gplots::col2hex('gray30'): '#4D4D4D'
        addPolygons(
          data = rgns, group = 'regions',
          layerId = ~rgn_id,
          label = mapply(function(n, v) {
            HTML(sprintf("<em>%s:</em> %s", htmlEscape(n), htmlEscape(v)))},
            r_v$rgn_name, r_v$value, SIMPLIFY = F),
          labelOptions = lapply(1:nrow(r_v), function(x) {
            labelOptions(direction='auto') }),
          stroke = TRUE, opacity=0.5, weight=2, fillOpacity = 0.5, smoothFactor = 0.5,
          color = ~id2col(rgn_id)) %>%
        addLegend(
          "bottomright", pal = pal, opacity = 0.5,
          values = selected$data$value, title = selected$label) %>%
        fitBounds(lng1 = b[1], lat1 = b[2], lng2 = b[3], lat2 = b[4]) %>%
        htmlwidgets::onRender(map_render_mouseover)

    } else {

      leaflet() %>%
        addProviderTiles('Stamen.TonerLite') %>%
        addPolygons(
          data = rgns, group = 'regions',
          layerId = ~rgn_id,
          label = mapply(function(n, v) {
            HTML(sprintf("<em>%s:</em> %s", htmlEscape(n), htmlEscape(v)))},
            r_v$rgn_name, r_v$value, SIMPLIFY = F),
          labelOptions = lapply(1:nrow(r_v), function(x) {
            labelOptions(direction='auto') }),
          stroke = TRUE, opacity=0.5, weight=2, fillOpacity = 0.5, smoothFactor = 0.5,
          color = ~id2col(rgn_id)) %>%
        addLegend(
          "bottomright", pal = pal, opacity = 0.5,
          values = selected$data$value, title = selected$label) %>%
        fitBounds(lng1 = b[1], lat1 = b[2], lng2 = b[3], lat2 = b[4]) %>%
        htmlwidgets::onRender(map_render_mouseover)
      }

  })

  # zoom to region
  observeEvent(input$sel_rgn, {

    if (input$sel_rgn==y$study_area){
      b = bbox(rgns)
    } else {
      b = bbox(subset(rgns, rgn_name == input$sel_rgn))
    }

    leafletProxy('map1',session) %>%
      fitBounds(lng1 = b[1],lat1 = b[2],lng2 = b[3],lat2 = b[4])
  })

  # aster hover ----

  # handle mouseover/mouseout per leaflet::examples/shiny.R style
  observeEvent(input$map1_shape_mouseover, {
    #if (!v$hi_freeze && input$map1_shape_mouseover$group == 'regions'){
    if (!input$map1_shape_mouseover$group == 'regions') return()

    if (!v$hi_freeze){
      v$hi_id <- as.integer(sub('_hi', '', as.character(input$map1_shape_mouseover$id)))
      isolate(v$msg <- paste(now_s(), '-- map1_shape_mouseover | hi_id=', v$hi_id, br(), v$msg))
    }
  })
  observeEvent(input$map1_shape_mouseout, {
    #if (!v$hi_freeze && input$map1_shape_mouseover$group == 'regions'){
    if (!input$map1_shape_mouseover$group == 'regions') return()
    if (!v$hi_freeze){
      v$hi_id = 0
      isolate(v$msg <- paste(now_s(), '-- map1_shape_mouseout | hi_id=', v$hi_id, br(), v$msg))
    }
  })

  # click on country eez to freeze flower plot
  observeEvent(input$map1_shape_click, {

    #if (input$map1_shape_mouseover$group == 'regions'){
    if (!input$map1_shape_mouseover$group == 'regions') return()
      v$hi_id <- as.integer(sub('_hi', '', as.character(input$map1_shape_click$id)))
      v$hi_freeze <- T
      isolate(v$msg <- paste(now_s(), '-- map1_shape_click | hi_id=', v$hi_id, br(), v$msg))
    #}
  })

  # click anywhere on map to unfreeze flower plot and default back to global
  observeEvent(input$map1_click, {
    if (v$hi_freeze){
      v$hi_freeze <- F
      v$hi_id <- 0
    }
    isolate(v$msg <- paste(now_s(), '-- map1_click | hi_id=', v$hi_id, br(), v$msg))
  })

  # add shape on hover
  observeEvent(v$hi_id,{

    #req(input$map1) # otherwise [JS console error `Couldn't find map with id map`](https://github.com/rstudio/leaflet/issues/242)
    isolate(v$msg <- paste(now_s(), '-- observeEvent(hi_id) | hi_id=', v$hi_id, br(), v$msg))

  })

  # aster plot
  output$aster = renderAster({

    req(input$sel_scenario, input$sel_type, input$sel_output_goal, input$sel_output_goal_dimension)

    # if (input$sel_scenario != scenario){
    #   load_scenario(input$sel_scenario)
    # }

    # if default input Index score, show aster
    if (input$sel_type=='output' & input$sel_output_goal=='Index' & input$sel_output_goal_dimension=='score'){

      data = scores %>%
        filter(region_id == v$hi_id, dimension == 'score') %>%
        left_join(goals, by='goal') %>%
        filter(is.na(parent), !is.na(order_color)) %>%
        arrange(order_color) %>%
        mutate(label=name) %>%
        select(id=goal, order=order_color, score, weight, color, label)

      data = bind_rows(
        data %>%
          filter(!is.na(score)),
        data %>%
          filter(is.na(score)) %>%
          mutate(
            score = 100,
            color = '#BEBEBE')) # gplots::col2hex('gray')

      score = scores %>%
        filter( region_id == v$hi_id, dimension == 'score', goal == 'Index') %>%
        .$score

      ohiaster(data, score,
        background_color = "transparent",
        font_color = "black", stroke = "black", font_size_center = "12px", font_size = "8px",
        margin_top=5, margin_right=5, margin_bottom=5, margin_left=5)
    }
  })

  output$rgnInfo = renderText({

    if (v$hi_id == 0){
      txt = y$study_area
    } else {
      req(get_selected())

      # if hover, show region
      txt = rgns@data %>% filter(rgn_id==v$hi_id) %>% .$rgn_name

      # if frozen, make bold
      if (v$hi_freeze) txt = strong(txt)

      # add score
      txt = paste(
        txt, ':',
        get_selected()$data %>%
          filter(rgn_id == v$hi_id) %>%
          .$value)
    }
    format(txt)
  })

  output$ui_commit = renderText({
    sha_txt = sprintf('%s/%s', y$gh_slug, str_sub(local_sha, end=7))
    sha_url = sprintf('https://github.com/%s/commit/%s', y$gh_slug, local_sha)
    HTML(str_c(
      div(
        class='shiny-input-container', style='font-size:12px',
        'data:', a(sha_txt, href=sha_url))))
  })

  # output$hoverText <- renderText({
  #   if (v$hi_id == 0){
  #     sprintf("Global: %s km2", format(area_global, big.mark =','))
  #   } else {
  #     sprintf(
  #       "%s: %s km2",
  #       subset(rgns@data, rgn_id==v$hi_id, rgn_name, drop=T),
  #       format(round(subset(rgns@data, rgn_id==v$hi_id, area_km2, drop=T)), big.mark =','))
  #   }
  #   })

  observeEvent(fileReaderData(), {
    # get remote_sha from file
    remote_sha <- fileReaderData()

    # check if github remote differs from local
    if (devtools:::different_sha(remote_sha, local_sha)){

      # show progress bar while updating
      withProgress(
        message = sprintf('Updating data "%s" with newer commit(s) at github.com/%s', dir_data, y$gh_slug),
        value = 0, {

          # increment
          n = length(y$scenario_dirs) + 2

          # git fetch & overwrite
          incProgress(1/n, detail = 'git fetch & reset')
          system(sprintf('cd %s; git fetch; git reset --hard origin/%s', dir_data, y$gh_branch_data))
          local_sha <<- devtools:::git_sha1(path=dir_data, n=nchar(remote_sha))

          # redo [scenario].Rdata files
          for (i in 1:length(y$scenario_dirs)){
            scenario = sort(y$scenario_dirs)[i]
            rdata = sprintf('%s_%s.Rdata', y$gh_repo, scenario)

            incProgress((i+1)/n, detail = sprintf("creating scenario %s", rdata))
            unlink(rdata)
            create_scenario_rdata(scenario, rdata)
          }

          # load default scenario
          incProgress(n, detail = sprintf("loading selected scenario %s", input$sel_scenario))
          load_scenario(input$sel_scenario)
        })

      return(remote_sha)
    }

  })


  # elements tab ----

  get_network = reactive({
    # get network based on selection
    net = list()
    net$nodes = nodes %>%
      filter(
        group %in% c('Index','goal','subgoal', input$nav_dims)) # 'layer for status','layer for pressures','layer for resilience'
    net$edges = edges %>%
      filter(
        from %in% nodes$id,
        to %in% nodes$id)
    return(net)
  })

  output$sunburst <- renderSunburst({
    # prep Elements and paths for sunburstR ----
    # TODO:
    # - add description inset box: full name, category, description (and link) of element
    # - make clickable: a) hold on description box to click link there, b) clickable to link directly

    add_shiny(
      sunburst(
        paths %>% select(path, layer_weight),
        percent=F,
        colors = cols,
        legendOrder = c(names(layer_colors), goals$goal),
        explanation = "function(d){return d.name}",
        sortFunction = htmlwidgets::JS(sort_fxn)))

  })

  selection <- reactive({
    input$sunburst_mouseover
  })

  output$selection <- renderUI({
    v = selection()
    if (is.null(v)) return(NULL)

    n = length(v)
    g = goals %>%
        filter(goal == v[1])

    if (n == 1){
      return(HTML(sprintf(
        'goal: <b>%s</b>\n<br>description: <i>%s</i>', g$name, g$description)))
    }
    if (n >= 2){
      sg = goals %>%
        filter(goal == v[2])
      sg_name = ifelse(
        v[1] == v[2],
        sprintf('<b>%s</b> (%s)', g$name, v[1]),
        sprintf('<b>%s</b> (%s) > <b>%s</b> (%s)', g$name, v[1], sg$name, v[2]))
      sg_description = sg$description
    }
    if (n == 2){
      return(HTML(sprintf(
        'goal: %s (%s)\n<br>description: <i>%s</i>', sg_name, v[2], sg_description)))
    }
    #browser()
    if (n == 3){
      l_dim = c(p='pressure',r='resilience',s='status')[str_sub(v[3],1,1)]
      l_id = str_sub(v[3], 3)
      l = layers %>%
        filter(layer == l_id)
      return(HTML(sprintf(
        'goal: %s\n<br>%s layer: <b>%s</b> (%s) \n<br>description: <i>%s</i>', sg_name, l_dim, l$name, l_id, l$description)))
    }
    # TODO: populate description
    # el1: goal: name (code)
    #  el2: subgoal: name (code) if goal != subgoal
    #    el3: <status|pressures> layer: name (code)
    # description: descriptions[length(v)]

    })

  # message ----
  output$ui_msg <- renderUI({
    if (y$debug){
      fluidRow(
        box(
          title='Messages', color='yellow', collapsible = T, width = 12, collapsed=F,
          'debug=T in app.yml\n',
          HTML(v$msg))) }
    })

})
