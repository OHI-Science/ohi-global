dashboardPage(

  dashboardHeader(title="Ocean Health Index"),

  dashboardSidebar(

    sidebarMenu(
      id = 'sidebarmenu',

      htmlOutput('ui_commit'),

      menuItem("Explore", tabName='explore',icon=icon("globe",lib='font-awesome'), selected=T),

      if(length(y$scenario_dirs) > 1){
        menuItem("Compare", tabName='compare',icon=icon("exchange",lib='font-awesome'), selected=F)
      } else {
        span()
      },

      selectInput(
        'sel_scenario',
        label    = '0. Choose scenario:',
        choices  = sort(y$scenario_dirs),
        selected = y$scenario_dirs[1]),

      conditionalPanel(
        "input.sidebarmenu == 'compare'",

        uiOutput('ui_sel_scenario_b')),

        selectInput(
          'sel_type',
          label='1. Choose type:',
          choices=c('Input Layer'='input', 'Output Score'='output'),
          selected='output'),

        conditionalPanel(
          condition = "input.sel_type == 'output'",

          selectInput(
            'sel_output_goal',
            label    = '2. Choose goal:',
            choices  = output_goals,
            selected = 'Index'),

          uiOutput('ui_sel_output')),

        conditionalPanel(
          condition = "input.sel_type == 'input'",

          selectInput(
            'sel_input_target',
            label    = '2. Choose target:',
            choices  = with(layer_targets, setNames(target, target_label))),

          uiOutput('ui_sel_input')),

        selectInput(
          'sel_rgn',
          label    = 'Zoom to region:',
          choices  = c(y$app_title,sort(as.character(rgns@data$rgn_name)))),

        htmlOutput('var_description', class='shiny-input-container') )),
  #),

  dashboardBody(
    tabItems(

      tabItem(
        tabName='explore',

        fluidRow(
          tabBox(width=12, selected='Map',

            tabPanel(
              'Map', #title    = 'Map', status='primary', collapsible=T,
              width=12,

              div(
                position = 'relative',

                # leaflet map
                leafletOutput('map1', height = 550),

                # hover text showing info on hover area
                absolutePanel(
                  bottom=10, left=10, style='background-color:white'),
                  #textOutput('hoverText')),

                # region info, possibly with aster chart
                absolutePanel(
                  top=10, right=10, # class='floater', # draggable=T, # style='background-color:white', # class='floater', # width='200px', height='200px',
                  div(class='well', style='margin-right: 15px; margin-top: 44px; text-align: right; overflow: hidden;',
                    htmlOutput('rgnInfo'),
                    conditionalPanel(
                      condition = "input.sel_type === 'output' & input.sel_output_goal=='Index' & input.sel_output_goal_dimension=='score' & input.sidebarmenu!='compare'",
                      style     = 'float:right; display:block;',
                      asterOutput(outputId = "aster", width='100px', height='100px')))))),

            tabPanel(
              'Table',
              dataTableOutput('table')),

            tabPanel(
              'Elements',
              sunburstOutput("sunburst"),
              uiOutput("selection")),

            if(length(y$scenario_dirs) > 1){
              tabPanel(
                'Plot',
                uiOutput('ui_boxplot'))
            } else {
              span()
            }
            
            )),

        uiOutput('ui_msg')

        ))))
