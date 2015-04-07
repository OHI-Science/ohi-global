# see global.R

# # reactiveValues ----
# values = reactiveValues()
# dir_scenarios = dirname(dir_scenario)
# values$dirs_scenario <- grep('^scenario\\.', list.dirs(path=dir_scenarios, recursive=F), value=T)
#dirs_scenario = reactiveValues()
#sel_layer_choices = reactiveValues()

# shinyServer ----
# Define server logic required to summarize and view the selected dataset

shinyServer(function(input, output, session) {

  output$git_commit = renderUI({
    git_commit = sprintf('[%s] %s: %s', substr(git_head@sha, 1, 7), git2r::when(git_head), git_head@summary)
    tags$input(id='git_commit', type='hidden', value=git_commit) })

  output$ls_files = renderUI({
    ls_files = paste(list.files(dir_scenario), collapse='\n')
    tags$input(id='ls_files', type='hidden', value=ls_files) })

#   # scenario directories. monitor filesystem every 5 seconds for folders in dir.conf
#   observe({
#     invalidateLater(60 * 1000, session)  # 5 seconds, in milliseconds
#     if (exists('git_url')){
#       git2r::pull(repo)
#       git_head <<- git2r::commits(repo)[[1]]
#     }
#     dirs_scenario <<- grep('^[^\\.]', basename(list.dirs(path=dir_scenarios, recursive=F)), value=T)
#   })

  # tab_data ----
  tab_data =
    tabPanel(
      'Data', value='data',
      conditionalPanel(
        condition="input.tabsetFunction == 'data'",
        sidebarPanel(
          id='data-sidebar',
          selectInput(
            'sel_type',
            label='1. Choose variable type:',
            choices=c('Input Layer'='Layer', 'Output Score'='Score'),
            selected='Score', multiple=F, selectize=T),

          # data layer
          conditionalPanel(
            "input.sel_type == 'Layer'",
            selectInput(
              'sel_layer_target',
              label    = '2. Choose target (goal, pressures, resilience or spatial):',
              selected = sel_layer_target_choices[1],
              choices  = sel_layer_target_choices, multiple=F, selectize=T),
            selectInput(
              'sel_layer',
              label    = '3. Choose layer:',
              selected = sel_layer_choices[1],
              choices  = sel_layer_choices, multiple=F, selectize=T),
            conditionalPanel(
              "input.sel_layer_category != 'NA'",
              selectInput(
                'sel_layer_category',
                label    = '4. Choose category:',
                choices  = sel_layer_category_choices,
                selected = sel_layer_category_choices[1])),
            conditionalPanel(
              "input.sel_layer_year != 'NA'",
              selectInput(
                'sel_layer_year',
                label    = '5. Choose year:',
                choices  = sel_layer_year_choices,
                selected = sel_layer_year_choices[1]))),

          # data score
          conditionalPanel(
            "input.sel_type == 'Score'",
            selectInput(
              'sel_score_target',
              label='2. Choose target (index or goal):',
              choices=sel_score_target_choices,
              selected='Index'),
            selectInput(
              'sel_score_dimension',
              label='3. Choose dimension:',
              choices=sel_score_dimension_choices,
              selected='score')),
          p(htmlOutput('var_description')),
          verbatimTextOutput(outputId="var_details")),

        # TODO: use Select2 combo boxes and search field, see https://github.com/mostly-harmless/select2shiny

        # data main
        mainPanel(
          id='data-main',
          tabsetPanel(
            id='tabsetMap',
            tabPanel(
              'Map',
              value='data-map',
              mapOutput('map_container')),
            tabPanel(
              'Histogram',
              value='data-histogram',
              plotOutput('histogram')),
            #tabPanel('Summary',   value='data-summary',   verbatimTextOutput('summary')),
            tabPanel('Table',     value='data-table', dataTableOutput('table'))))))

  # tab_calculate ----
  tab_calculate = tabPanel(
    'Calculate',
    value='calculate',
    p(
      'Scenario path exists:',
      verbatimTextOutput(outputId='dir_scenario_exists')),
    conditionalPanel(
      condition="output.dir_scenario_exists == 'FALSE'",
      textInput('dir_scenario', 'Scenario directory to output:', value=dir_scenario),
      actionButton('btn_write','Write to disk')),
    conditionalPanel(
      condition='output.dir_scenario_exists == "TRUE"',
      #uiOutput('sel_scenario_dir'), # generates dir_conf
      #verbatimTextOutput(outputId="txt_conf_summary"),
      p(
        'Scenario path',
        verbatimTextOutput(
          outputId="show_dir_scenario")),
      actionButton('btn_calc','Calculate'),
      verbatimTextOutput(outputId="txt_calc_summary")))


  # tab_report ----
  if (file.exists(dir_scenario)){
    tab_report = tabPanel(
      'Report',
      value='report',
      p('Reports directory:', verbatimTextOutput(outputId='dir_reports')),
      textInput('txt_report_fn', 'Report filename to output:', value='report.html'),
      br('Include:'),
      checkboxInput('ck_flowers'    , 'Flowers'   , value = T),
      checkboxInput('ck_tables'     , 'Tables'    , value = T),
      br('Options:'),
      checkboxInput('ck_open_html'  , 'Open in new window', value = T),
      checkboxInput('ck_global_only', 'Global only (vs all regions which takes time)', value = T),
      checkboxInput('ck_overwrite'  , 'Overwrite existing figures', value = F),
      actionButton('btn_report','Generate Report'),
      verbatimTextOutput(outputId="txt_report_summary"))
  } else {
    tab_report = tabPanel(
      'Report',
      value='report',
      p('You must write this scenario to the filesystem (see Calculate tab) before generating a report.'))
  }


  # tab_compare ----
  tab_compare = tabPanel(
    'Compare',
    value='compare',
    sidebarLayout(
      sidebarPanel(
        p(em('Compare scores across branches/scenarios/commits: A versus B.')),
        uiOutput('ui_a_branch_scenario'),
        uiOutput('ui_a_commit'),
        uiOutput('ui_a_message'),
        uiOutput('ui_b_branch_scenario'),
        uiOutput('ui_b_commit'),
        uiOutput('ui_b_message'),

        selectInput(
          'sel_dimensions', 'Dimensions',
          choices  = ohi_dimensions,
          selected = ohi_dimensions,
          multiple=T, selectize=T),

        selectInput(
          'sel_goals', 'Goals',
          choices  = ohi_goals,
          selected = ohi_goals,
          multiple=T, selectize=T)),

      mainPanel(
        tabsetPanel(
          id='cmp_tabset',
          tabPanel(
            title='Plot',
            htmlOutput('cmp_plot')),
          tabPanel(
            title='Table',
            dataTableOutput('cmp_table'))))))

  # render tabsetpanel ----
  output$ui_tabsetpanel = renderUI({

    tab_panels = list(tab_data)
    if (!'report' %in% tabs_hide){
      tab_panels = c(tab_panels, list(tab_report))
    }
    if (!'calculate' %in% tabs_hide){
      tab_panels = c(tab_panels, list(tab_calculate))
    }
    if (!'compare' %in% tabs_hide){
      tab_panels = c(tab_panels, list(tab_compare))
    }
    do.call(tabsetPanel, c(list(id='tabsetFunction'), tab_panels))

  })

  # select layer ----
  observe({
    if (all(c('sel_layer_target') %in% names(input))){

      sel_layer_choices = with(subset(layer_targets, target==input$sel_layer_target),
                               setNames(layer, layer_label))
      updateSelectInput(session, 'sel_layer',
                        label='3. Choose layer:',
                        choices=sel_layer_choices,
                        selected=ifelse(length(sel_layer_choices)>0,
                                        sel_layer_choices[1],
                                        NULL))
    }
  }, priority=1)

  # select layer category ----
  observe({
    if (all(c('sel_layer') %in% names(input))){

      lyr = input$sel_layer
      lyr_fld_category = subset(layers$meta, layer==lyr, fld_category, drop=T)
      if (is.na(lyr_fld_category)){
        sel_layer_category_choices = NA
      } else {
        d = layers$data[[lyr]]
        sel_layer_category_choices = sort(as.character(unique(d[[lyr_fld_category]])))
      }
      updateSelectInput(session,  'sel_layer_category',
                        label    = sprintf('4. Choose %s category:', lyr_fld_category),
                        choices  = sel_layer_category_choices)
    }
  }, priority=2)

  # select layer year ----
  observe({
    if (all(c('sel_layer','sel_layer_category') %in% names(input))){

      # reactives
      lyr          = input$sel_layer
      lyr_category = input$sel_layer_category

      lyr_fld_year = subset(layers$meta, layer==lyr, fld_year, drop=T)

      cat(sprintf('lyr_fld_year:"%s"\n',lyr_fld_year))

      # TODO: with Layers > NP > rnky_np_harvest_relative, getting "Error in names(choices) <- choiceNames : attempt to set an attribute on NULL", should be fixed in future based on https://groups.google.com/forum/?pli=1#!topic/shiny-discuss/K7chwrMCvkU
      if (is.na(lyr_fld_year)){
        sel_layer_year_choices = NA
      } else {
        d = ohicore::SelectLayersData(layers, layers=lyr, narrow=T) # layers$data[[lyr]]
        if ('category' %in% names(d) && lyr_category != 'NA'){ # !is.na(input$sel_layer_category & input$sel_layer_category!='')){
          d = subset(d, category==lyr_category)
        }
        sel_layer_year_choices = as.character(sort(unique(d$year), decreasing=T))
      }
      updateSelectInput(session,  'sel_layer_year',
                        label    = '5. Choose year:',
                        choices  = sel_layer_year_choices)#,
                        #selected = sel_layer_year_choices[1])
    }
  }, priority=3)

  GetScenario <- reactive({

    # reactives
    branch_scenario = input$sel_branch_scenario

    # update data based on branch and scenario selected
    dir_scenario <<- file.path(dir_archive, branch_scenario)
    layers       <<- Layers(file.path(dir_scenario, 'layers.csv'), file.path(dir_scenario, 'layers'))
    conf         <<- Conf(file.path(dir_scenario, 'conf'))
    scores       <<- read.csv(file.path(dir_scenario, "scores.csv"), na.strings="")

    TRUE
  })

  # Layers: GetVar() ----
  GetVar <- reactive({

    get_scenario = GetScenario()

    #read.csv(file.path(dir_scenario, "scores.csv"), na.strings="") %>% filter(goal=='Index' & dimension=='score')
    #read.csv('ecu/draft/subcountry2014/scores.csv', na.strings='') %>% filter(goal=='Index' & dimension=='score')
    #read.csv('ecu/published/subcountry2014/scores.csv', na.strings='') %>% filter(goal=='Index' & dimension=='score')
    #read.csv('github/subcountry2014/scores.csv', na.strings='') %>% filter(goal=='Index' & dimension=='score')
    read.csv(file.path(dir_scenario, "scores.csv"), na.strings="") %>% filter(goal=='Index' & dimension=='score')
    
    v = list()
    #browser('GetVar top', expr=input$sel_layer=='mar_harvest_tonnes')
    if (input$sel_type == 'Layer'){

      # reactives
      lyr             = input$sel_layer
      lyr_category    = input$sel_layer_category
      lyr_year        = suppressWarnings(as.integer(input$sel_layer_year))
      lyr_target      = input$sel_layer_target

      # data and name
      d = ohicore::SelectLayersData(layers, layers=lyr, narrow=T)
      fld_id_num = subset(layers$meta, layer==lyr, fld_id_num, drop=T)
      if (!is.na(fld_id_num) && fld_id_num=='rgn_id'){
        d = rename(d, c('id_num'='rgn_id'))
        v$fld_id = 'rgn_id'
      } else {
        v$fld_id = ifelse(!is.na(fld_id_num), fld_id_num, subset(layers$meta, layer==lyr, fld_id_chr, drop=T))
      }

      x = lyr_label = subset(layer_targets, layer==lyr & target==lyr_target, layer_label, drop=T)
      if ('category' %in% names(d)){
        if (!lyr_category %in% d$category){
          lyr_category = sort(unique(d$category))[1]
          updateSelectInput(session, 'sel_layer_category', selected=lyr_category)
        }
        d = subset(d, category==lyr_category)
        x = sprintf('%s : %s', x, lyr_category)
      }

      if ('year' %in% names(d)){
        if (!lyr_year %in% na.omit(d$year)){
          lyr_year = max(d$year, na.rm=T)
        }
        d = subset(d, year==lyr_year)
        x = sprintf('%s : %s', x, as.character(lyr_year))
      }

      v$data  = d
      v$name  = x
      attr(v$name, 'target') = lyr_target
      v$description = paste0('<b>',lyr_label,'</b>:',
                             #renderMarkdown(text=subset(layers$meta, layer==lyr, description, drop=T)))
                             #markdownToHTML(text=subset(layers$meta, layer==lyr, description, drop=T), options=c('fragment_only')))
                             renderMarkdown(file=NULL, text=as.character(subset(layers$meta, layer==lyr, description, drop=T))))
      v$details = ''
      m = subset(layers$meta, layer==input$sel_layer)
      v$layer = lyr
      v$meta  = m
      for (f in names(m)){
        s = ifelse(is.numeric(m[[f]]), sprintf('%0.4g', m[[f]]), as.character(m[[f]]))
        v$details = paste0(v$details, sprintf('%s: %s\n', f, s))
      }
    } else if (input$sel_type == 'Score') {
      # reactives
      g = input$sel_score_target    # strsplit(v$name, ' - ')[[1]][1]
      m = input$sel_score_dimension # strsplit(v$name, ' - ')[[1]][2]

      v$name = sprintf('%s : %s', g, m)
      attr(v$name, 'target') = g
      attr(v$name, 'dimension') = m
      v$data = plyr::rename(subset(scores, goal==g & dimension==m, c(region_id, score)), c('region_id'='rgn_id', 'score'='val_num'))
      v$description = paste0('<b>', names(sel_score_target_choices[sel_score_target_choices==g]),'</b>: <em>',
                             ifelse(g=='Index',
                                    conf$config$index_description,
                                    as.character(subset(conf$goals, goal == g, description, drop=T))),
                             '</em>\n<br>\n',
                             '<b>', m,'</b>: <em>',conf$config$dimension_descriptions[[m]], '</em>')
      v$details = ''
    }
    v$summary = sprintf('%s\n\n  count, not NA: %s\n  min: %0.4g\n  mean: %0.4g\n  max: %0.4g\n\n', v$name, length(na.omit(v$data$val_num)), signif(min(v$data$val_num, na.rm=T), 4), mean(v$data$val_num, na.rm=T), max(v$data$val_num, na.rm=T))
    return(v)
  })

  # Data: info
  output$var_description = renderUI({ HTML(GetVar()$description) })

  output$var_details = renderPrint({ v = GetVar(); cat(v$summary, '\n\n', v$details) })

  # Data: Map ----
  output$map_container <- renderMap({
    v = GetVar()
    #browser('Data: Map', expr=input$sel_layer=='mar_harvest_tonnes')
    PlotMap(v=v,
            lon=ifelse(is.null(conf$config$map_lon), 0, conf$config$map_lon),
            lat=ifelse(is.null(conf$config$map_lat), 0, conf$config$map_lat),
            zoom=ifelse(is.null(conf$config$map_zoom), 2, conf$config$map_zoom))
  }, html_sub = c('"features": "#! regions !#",' = '"features": regions,'))


  # Layers: histogram ----
  output$histogram <- renderPlot({
    library(ggplot2)

    # get input data
    v = GetVar()
    bin.width = diff(range(v$data[,'val_num'], na.rm=T))/30

    # Histogram overlaid with kernel density curve
    p = ggplot(na.omit(v$data), aes(x=val_num))
    p = p + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                           binwidth=bin.width,
                           colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
    print(p)
  })

  # Layers: table ----
  output$table <- renderDataTable({
    d = rename(GetVar()$data, c('val_num'='value'))

    # HACK: assuming has rgn_id in layer
    d = merge(d, rgn_names, all.x=T)
    d = cbind(d[,c('rgn_id','rgn_name')], d[,!names(d) %in% c('layer','rgn_id','rgn_name'), drop=F])
    d
  })

  # Goals: aster ----
  output$aster <- renderPlot(width=800, height=850, {

    # get rescaled weights from slider inputs
    wts = get_wts(input)
    goals.wts = names(wts)
    cols.wts = cols.goals.all[goals.wts]

    # get data from results, so far assuming first line of results/regions_goals.csv
    # TODO: add dropdowns for: 1) different regions, 2) different schemes (ie weights)
    x = subset(scores, dimension=='score' & region_id==0)
    scores.wts  = x$score[match(names(wts), as.character(x$goal))] # regions_goals[1, goals.wts]
    index.score = weighted.mean(scores.wts, wts)

    # plot aster
    aster(lengths=scores.wts,
          max.length=100,
          widths=wts,
          disk=0.4,
          main='Global',
          fill.col=cols.wts,
          center=round(index.score),
          labels=paste(goals.wts, round(scores.wts), sep='\n'),
          label.cex=1.0,
          label.offset=0.1,
          cex=2.2,
          cex.main=2.5)

  })

  # Calculate: write ----
  output$show_dir_scenario = renderText({ cat(dir_scenario) })
  observe({
    if (all(!'calculate' %in% tabs_hide, file.exists(dir_scenario), c('btn_write') %in% names(input))){
      input$btn_write
      output$dir_scenario_exists = renderText({file.exists(input$dir_scenario)})
      output$show_dir_scenario   = renderText({input$dir_scenario})
      if (input$btn_write == 0){
        state_btn_write <<- 0
        #output$dir_scenario_exists = as.character()
      } else if (input$btn_write != state_btn_write){
        #output$dir_scenario_exists = as.character(file.exists(dir_scenario))
        isolate({
          state_btn_write <<- input$btn_write
          dir_scenario <<- input$dir_scenario
          ohicore::WriteScenario(
            scenario = list(
              conf = conf,
              layers = layers,
              scores = scores,
              spatial = dir_spatial,
              dir    = dir_scenario))
        })
        # = file.exists(dir_scenario)
      }
    }
  })

  #   # Calculate: sel_scenario_dir ----
  #   output$sel_scenario_dir <- renderUI({
  #     selectInput("dir_scenario", sprintf("Select scenario (from %s):", dir_scenarios),
  #                 values$dirs_scenario, selected=basename(dir_scenario))
  #   })

  #   # Calculate: txt_conf_summary ----
  #   output$txt_conf_summary <- renderPrint({
  #     cat('branch/scenario:', basenam(input$dir_scenario),'\n')
  #     if (is.null(input$dir_scenario)){
  #       config.R = file.path(dir_scenarios, 'conf','config.R')
  #     } else {
  #       config.R = file.path(dir_scenarios, input$dir_scenario, 'conf','config.R')
  #     }
  #     #config.check(config.R)
  #     #config.summary(config.R)
  #   })

  # Calculate: txt_calc_summary ----
  observe({

    if (all(!'calculate' %in% tabs_hide, 'btn_calc' %in% names(input))){

      input$btn_calc
      if (input$btn_calc == 0){
        state_btn_calc <<- 0
        output$txt_calc_summary <- renderText({''})
      } else if (input$btn_calc != state_btn_calc){
        isolate({
          state_btn_calc <<- input$btn_calc
          output$txt_calc_summary <- renderText({

            #browser('server.R -- Calculate: txt_calc_summary')

            # set scenario vars (previously in scenario.R)
            scenario=list(
              conf    = ohicore::Conf(file.path(dir_scenario, "conf")),
              layers  = ohicore::Layers(file.path(dir_scenario, "layers.csv"), file.path(dir_scenario, "layers")),
              scores  = read.csv(file.path(dir_scenario, "scores.csv"), na.strings=""),
              spatial = file.path(dir_scenario, "spatial"),
              dir     = dir_scenario)
            layers <<- scenario$layers
            conf   <<- scenario$conf

            cat(sprintf('Checking layers: %s)\n  having conf/Config.R/layers_id_fields: %s', file.path(dir_scenario, 'layers.csv'),paste(conf$config$layers_id_fields, collapse=', ')))
            cat(sprintf('   layers: %s)\n', file.path(dir_scenario, 'layers.csv')))

            # check and reload layers
            CheckLayers(file.path(dir_scenario, 'layers.csv'), file.path(dir_scenario, 'layers'), conf$config$layers_id_fields)
            scenario$layers = layers <<- ohicore::Layers(file.path(dir_scenario, 'layers.csv'), file.path(dir_scenario, 'layers'))

            # calculate scores
            setwd(dir_scenario)
            scores <<- ohicore::CalculateAll(scenario$conf, scenario$layers, debug=F)
            write.csv(scores, 'scores.csv', na='', row.names=F)

            sprintf('Scores calculated and output to: %s', file.path(dir_scenario, 'scores.csv'))
          })
        })

      }
    }
  })


  # Report: sel_compare ----
  output$sel_compare <- renderUI({
    selectInput("dir_compare", "Compare with scenario:",
                c('[None]',setdiff(dirs_scenario, basename(dir_scenario))),
                selected='[None]')
  })

  # Report: btn_report ----
  output$dir_reports  = renderText({ file.path(dir_scenario, 'reports') })
  observe({

    if (all(!'report' %in% tabs_hide & file.exists(dir_scenario), c('btn_report','btn_write','btn_calc') %in% names(input))){

      input$btn_report
      if (input$btn_report == 0){
        state_btn_report <<- 0
        output$txt_report_summary <- renderText({''})
      } else if (input$btn_report != state_btn_report){
        isolate({
          state_btn_report <<- input$btn_report
          ohicore::ReportScores(scenario = list(
            conf = conf,
            layers = layers,
            scores = scores,
            spatial = ifelse(file.exists(system.file('extdata/spatial.www2013', package='ohicore')),
                             system.file('extdata/spatial.www2013', package='ohicore'),
                             system.file('inst/extdata/spatial.www2013', package='ohicore'))),
            directory = file.path(dir_scenario,'reports'),
            filename = input$txt_report_fn,
            open_html=input$ck_open_html,
            do_flowers=input$ck_flowers, do_tables=input$ck_tables,
            overwrite=input$ck_overwrite, global_only=input$ck_global_only,
            # TODO: implement...
            do_maps=input$ck_maps, do_histograms=input$ck_histograms, do_equations=input$ck_equations, do_paths=input$ck_paths,
            debug=F)
          output$txt_report_summary <- renderText({
            #browser()
            cat('success')
          })
        })
      }

    }
  })


  # Compare ----

  if (!'compare' %in% tabs_hide){

    # TODO: link to commit log history --

    # reactive drop-downs of csv's based on dir_repo
    # csv_paths = reactive({
    #   p = list.files(input$dir_repo, 'scores\\.csv$', recursive=T)
    #   return(p)
    # })

    output$ui_a_branch_scenario <- renderUI({
      selectInput(
        'a_branch_scenario', 'A: branch/scenario',
        choices = branches_scenarios,
        selected = branches_scenarios[1])})

    output$ui_b_branch_scenario <- renderUI({
      selectInput(
        'b_branch_scenario', 'B: branch/scenario',
        choices = branches_scenarios,
        selected = branches_scenarios[2])})

    observe({
      if (all(c('a_branch_scenario') %in% names(input))){

        output$ui_a_commit <- renderUI({
          selectInput(
            'a_commit', 'A: commit',
            choices =
              sapply(
                branch_commits[[dirname(input$a_branch_scenario)]],
                function(x) setNames(x@sha, sprintf('%0.16s @%0.7s', when(x), x@sha))),
            selected = 1)})

      }})

    observe({
      if (all(c('b_branch_scenario') %in% names(input))){

        output$ui_b_commit <- renderUI({
          selectInput(
            'b_commit', 'B: commit',
            choices =
              sapply(
                branch_commits[[dirname(input$b_branch_scenario)]],
                function(x) setNames(x@sha, sprintf('%0.16s @%0.7s', when(x), x@sha))),
            selected = 1)})

      }})

    observe({
      if (all(c('a_branch_scenario', 'a_commit') %in% names(input))){

        output$ui_a_message <- renderUI({

          branch = dirname(input$a_branch_scenario)

          x = Filter(
            function(x) x@sha == input$a_commit,
            branch_commits[[branch]])[[1]]

          HTML(paste(
            'A: commit message',
            em(HTML(renderMarkdown(
              file=NULL,
              text=x@message))),
            HTML(renderMarkdown(
              file=NULL,
              text = sprintf(
                'See all [%s commits](https://github.com/%s/commits/%s)',
                branch, git_slug, branch)))))

        })

      }})

    observe({
      if (all(c('b_branch_scenario', 'b_commit') %in% names(input))){

        branch = dirname(input$b_branch_scenario)

        output$ui_b_message <- renderUI({
          x = Filter(
            function(x) x@sha == input$b_commit,
            branch_commits[[branch]])[[1]]

          HTML(paste(
            'B: commit message',
            em(HTML(renderMarkdown(
              file=NULL,
              text=x@message))),
            HTML(renderMarkdown(
              file=NULL,
              text = sprintf(
                'See all [%s commits](https://github.com/%s/commits/%s)',
                branch, git_slug, branch)))))
        })

      }})


    # reactive data: difference of A and B
    data <- reactive({

      # # debug
      # input = list()
      # input$dir_repo = '~/github/ohi-global'
      # input$csv_rgns = '~/github/ohi-global/eez2013/layers/rgn_labels.csv'
      # input$a_commit = kn[1]
      # input$b_commit = kn[2]
      # input$a_csv    = 'eez2013/scores.csv' # p[3]
      # input$b_csv    = 'eez2013/scores.csv' # p[3]
      # input$csv_rgns = 'eez2013/layers/rgn_labels.csv'

      #if (!'a_csv' %in% names(input)) return(reactive({NA}))
      #g_csv = isolate(sprintf('%s/%s/conf/goals.csv', dir_archive, input$a_branch_scenario))

      # get necessary goals and regions from most recent A branch
      a_dir_archive_scenario <- file.path(dir_archive, input$a_branch_scenario)
      csv_goals <- file.path(a_dir_archive_scenario, 'conf/goals.csv')
      conf      <- Conf(file.path(a_dir_archive_scenario, 'conf'))
      csv_rgns  <- sprintf('%s/layers/%s.csv', a_dir_archive_scenario, conf$config$layer_region_labels)

      a_csv     <- file.path(basename(input$a_branch_scenario), 'scores.csv')
      b_csv     <- file.path(basename(input$b_branch_scenario), 'scores.csv')

      # read in data
      a = read_git_csv(dir_repo, input$a_commit, a_csv, stringsAsFactors=F)
      b = read_git_csv(dir_repo, input$b_commit, b_csv, stringsAsFactors=F)
      r = read.csv(csv_rgns , stringsAsFactors=F)
      g = read.csv(csv_goals, stringsAsFactors=F) %>% arrange(order_color)

      # check column names
      stopifnot(names(a) == c('goal','dimension','region_id','score'))
      stopifnot(names(b) == c('goal','dimension','region_id','score'))
      
      # hard code names(r) to override ohi-global/antarcticaYYYY having sp_id, label
      #stopifnot(c('rgn_id','label') %in% names(r))
      names(r) = c('rgn_id','label')
      
      # merge
      d = a %>%
        base::merge(
          b,
          by=c('goal','dimension','region_id'),
          suffixes=c('.a','.b')) %>%
        dplyr::rename(rgn_id=region_id) %>%
        mutate(
          score.dif = score.a - score.b,
          score.na = is.na(score.a)!=is.na(score.b)) %>%
        left_join(
          rbind_list(
            r %>%
              select(rgn_id, rgn_name=label),
            data.frame(rgn_id=0, rgn_name='GLOBAL')),
          by='rgn_id') %>%
        # filter(abs(score_dif) > 0.01 | score_na == T) %>%
        arrange(rgn_id!=0, goal!='Index', dimension!='score', goal, desc(dimension), desc(abs(score.dif)), is.na(score.a), is.na(score.b)) %>%
        select(goal, dimension, rgn_id, rgn_name, score.a, score.b, score.dif) %>%
        mutate(
          goal      = factor(goal, c('Index', g$goal)),
          dimension = factor(dimension, ohi_dimensions),
          id        = row_number()) %>%
        filter(!is.na(score.dif), goal %in% input$sel_goals, dimension %in% input$sel_dimensions)

      d
    })


    # ggvis plot ----

    output$cmp_plot <- renderUI({
      ggvisOutput('cmp_ggvis')
    })

    hover_tip <- function(x) {
      if(is.null(x)) return(NULL)
      row = data()[data()$id == x$id, ]
      return(with(row, sprintf('%s (%d): %g<br />%s %s<br />score.a: %g<br />score.b: %g', rgn_name, rgn_id, score.dif, goal, dimension, score.a, score.b)))
    }

    observe({
      if (all(c('a_branch_scenario', 'a_commit', 'b_branch_scenario', 'b_commit') %in% names(input))){

        data %>%
          #filter(!is.na(score.dif) & dimension=='score') %>%
          mutate(
            goal      = as.character(goal),
            dimension = as.character(dimension)) %>%
          ggvis(~goal, ~score.dif) %>%
          add_axis('y', title='score difference (A - B)') %>%
          layer_points(key := ~id, fill = ~factor(dimension)) %>%
          group_by(goal) %>%
          add_tooltip(hover_tip, 'hover') %>%
          bind_shiny('cmp_ggvis')

        # table ----
        output$cmp_table <- renderDataTable(
          data(),
          options = list(pageLength = 10)
        )

      }
    })

  } # end if compare tab

})
