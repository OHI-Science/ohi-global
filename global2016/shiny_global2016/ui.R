#ui.r

library(tidyverse) 
library(stringr)
library(shinythemes)
library(plotly)

### create warning with date to help with log output
cat(file = stderr(), sprintf('\n\nExecuting shiny app: %s\n', Sys.time()))
tmp <- getwd()
cat(file = stderr(), sprintf('Current working directory: %s\n', tmp))
tmp <- paste('  ', list.files(), collapse = '\n')
cat(file = stderr(), tmp, '\n')



### initialize variables needed by ui.R
continents <- read_csv('data/georegion_labels2.csv') %>%
  .$continent %>%
  unique()

goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')
goal_names <- data.frame(goal_code = goals, 
                         goal = c('Index', 
                                  'Artisanal opportunities',
                                  'Species condition (Biodiversity)',
                                  'Biodiversity',
                                  'Habitat (Biodiversity)',
                                  'Coastal protection',
                                  'Carbon storage',
                                  'Clean water',
                                  'Economies',
                                  'Livelihoods & economies',
                                  'Livelihoods',
                                  'Fisheries (Food provision)',
                                  'Food provision',
                                  'Mariculture (Food provision)',
                                  'Iconic species (Sense of place)',
                                  'Sense of place',
                                  'Lasting special places (Sense of place)',
                                  'Natural products',
                                  'Tourism & recreation'))

ui <- navbarPage(
  
  # tags$head(includeScript("google_analytics.js")),
  title = '',
  theme = shinytheme('cerulean'),

  tabPanel('Abstract',
    sidebarPanel(
      includeMarkdown('pages/article_info.md'),
      includeMarkdown('pages/acknowledgments.md')
    ),
    mainPanel(
      includeMarkdown('pages/abstract.md')
    )
  ),

  # tabPanel('Figures',
  #   sidebarPanel(
  #     includeMarkdown('pages/article_info.md')
  #   ),
  #   mainPanel(
  #     h4('Figures from manuscript'),
  #     h5('Figure 1:'),
  #     includeMarkdown('pages/fig1.md'),
  #     hr(),
  #     h5('Figure 2:'),
  #     includeMarkdown('pages/fig2.md'),
  #     hr(),
  #     h5('Figure 3:'),
  #     includeMarkdown('pages/fig3.md'),
  #     hr(),
  #     h5('Figure 4:'),
  #     includeMarkdown('pages/fig4.md'),
  #     hr(),
  #     h5('Figure 5:'),
  #     includeMarkdown('pages/fig5.md'),
  #     hr(),
  #     h5('Figure 6:'),
  #     includeMarkdown('pages/fig6.md')
  #   )
  # ),

  tabPanel('Score maps',
    sidebarPanel(width = 3,
      includeMarkdown('pages/clean_side1.md'),
      selectInput('map_scen', 
                  choices = c('Score 2016'  = 'score_2016',
                              'Score 2015'  = 'score_2015',
                              'Score 2014'  = 'score_2014',
                              'Score 2013'  = 'score_2013',
                              'Score 2012'  = 'score_2012',
                              'Annual change 2016'  = 'annual_change_2016'),
                  selected = 'score_2016',
                  label = 'Scenario to display?'),
      HTML('<p><b>Note:</b> Score color scale is fixed from 0-100.',
           'Annual Change color scale is rescaled depending on the', 
           'values for each goal.'),
      selectInput('map_goal', choices = goal_names$goal,
                  selected  = 'Index',
                  label = 'Goal to display?'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/map_main1.md'),
      imageOutput('scoremap'),
      plotOutput('scorehist', height = '250px', width = '550px')
    )
  ),
  
  tabPanel('Trends by goal',
    sidebarPanel(width = 3,
      includeMarkdown('pages/clean_side1.md'),
      checkboxInput('fig2_show_all', 
                    label = 'Show individual countries?',
                    value = FALSE),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/fig2_tab_main1.md'),
      plotlyOutput('fig2_plot', height = '400px', width = '700px')
    )
  ),
  
  tabPanel('Trend v. score',
    sidebarPanel(width = 3,
      includeMarkdown('pages/clean_side1.md'),
      selectInput('fig3_georgn', 'Choose a georegion to view:',
                  choices = c('Global', continents %>% sort()),
                  selected = 'Global'),
      checkboxInput('fig3_colors', 
                    label = 'Color-code regions?',
                    value = FALSE),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/fig3_tab_main1.md'),
      plotlyOutput('fig3_plot', height = '400px', width = '700px')
    )
  ),
  
  tabPanel('Trend bars',
    sidebarPanel(width = 3,
      includeMarkdown('pages/fig4_tab_side1.md'),
      radioButtons('fig4_filter', 'Filter countries by:',
                   choices = c('High-mid-low' = 'himidlo',
                               'Global or georegion' = 'georgn')),
      selectInput('fig4_georgn', 'Choose a georegion to view:',
                  choices = c('Global', continents %>% sort()),
                  selected = 'Global'),
      checkboxInput('fig4_overall', 'Show overall trend (as black bar)?',
                    value = TRUE),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/fig4_tab_main1.md'),
      uiOutput('fig4_plot.ui')
      # uiOutput('fig4_plotly.ui')
    )
  ),
  
  tabPanel('Model eval',
    sidebarPanel(width = 3,
      includeMarkdown('pages/clean_side1.md'),
      selectInput('fig5_goal', 'Choose a goal to view:',
                  choices = goal_names %>%
                    filter(!goal_code %in% c('SP', 'LE', 'FP', 'BD')) %>%
                    .$goal,
                  selected  = 'Index'),
      selectInput('fig5_georgn', 'Choose a georegion to view:',
                  choices = c('Global', continents %>% sort()),
                  selected  = 'Global'),
      checkboxInput('fig5_colors', 
                    label = 'Color-code regions?',
                    value = FALSE),
      checkboxInput('fig5_lm',
                    label = 'Show linear model?',
                    value = TRUE),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/fig5_tab_main1.md'),
      # uiOutput('fig5goal_plotly.ui')
      plotlyOutput('fig5a_plot', height = '300px', width = '700px'),
      plotlyOutput('fig5b_plot', height = '300px', width = '700px'),
      plotlyOutput('fig5c_plot', height = '300px', width = '700px')
    )
  ),
  
  tabPanel('Rank change',
    sidebarPanel(width = 3,
      includeMarkdown('pages/clean_side1.md'),
      selectInput('fig6_georgn', 'Choose a georegion to view:',
                  choices = c('Global', continents %>% sort()),
                  selected  = 'Global'),
      checkboxInput('fig6_colors', 
                    label = 'Color-code regions?',
                    value = FALSE),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/fig6_tab_main1.md'),
      plotlyOutput('fig6_plot', height = '400px', width = '700px')
    )
  ),
  
  
  tabPanel('Tables',
    sidebarPanel(width = 3,
      includeMarkdown('pages/clean_side1.md'),
      radioButtons('table_file', label = 'Table from published paper: ',
                  choices = c('Table 1 Updates to status and trend data and models' = 'table1',
                              'Table 2 Updates to pressure data and models' = 'table2'),
                  selected = 'table1'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      h4(textOutput('table_title')),
      includeMarkdown('pages/table_main1.md'),
      dataTableOutput('table_display'),
      includeMarkdown('pages/table_main2.md')
    )
  ),
  
  tabPanel('Data',
    sidebarPanel(width = 3,
      includeMarkdown('pages/clean_side1.md'),
      checkboxGroupInput('data_view', label = 'View additional columns:',
                         choices = c('Region information' = 'rgn',
                                     'Full goal name'     = 'goal'),
                         selected = c()),
      p('Country, region, and goal names may be helpful to',
        'filter/search the data.'),
      hr(),
      h5('Download OHI score data'),
      checkboxGroupInput('data_request', 'Select data to include:',
                         choices = c('All dimensions (see below)' = 'all_vals',
                                     'Region information'    = 'rgn',
                                     'Goal long names'       = 'goal'),
                         selected = c('all_vals', 'rgn', 'goal')),
      downloadButton('data_download', label = 'Download data'),
      p('Selecting "All dimensions" will download OHI score values as well as',
        'values for current status, trend, likely future state, resilience, ',
        'and pressure.  Unchecking this will download only the score values.'),
      hr(),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/data_main1.md'),
      dataTableOutput('data_display'),
      includeMarkdown('pages/data_main2.md')
    )
  )

  # tabPanel('References',
  #   sidebarPanel(
  #     includeMarkdown('pages/article_info.md'),
  #     includeMarkdown('pages/acknowledgments.md')
  #   ),
  #   mainPanel(
  #     includeMarkdown('pages/references.md')
  #   )
  # )

)
