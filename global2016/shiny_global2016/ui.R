#ui.r

source('ui_setup.R')

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

  tabPanel('Figures',
    sidebarPanel(
      includeMarkdown('pages/article_info.md')
    ),
    mainPanel(
      h4('Figures from manuscript'),
      h5('Figure 1:'),
      includeMarkdown('pages/fig1.md'),
      hr(),
      h5('Figure 2:'),
      includeMarkdown('pages/fig2.md'),
      hr(),
      h5('Figure 3:'),
      includeMarkdown('pages/fig3.md'),
      hr(),
      h5('Figure 4:'),
      includeMarkdown('pages/fig4.md'),
      hr(),
      h5('Figure 5:'),
      includeMarkdown('pages/fig5.md'),
      hr(),
      h5('Figure 6:'),
      includeMarkdown('pages/fig6.md')
    )
  ),

  tabPanel('Map alignment',
    sidebarPanel(
      includeMarkdown('pages/align_tab_side1.md'),
      selectInput('taxa_quad', 'Choose a taxon:',
                  choices = c('all', unique(spp_list$spp_group_text) %>% sort()),
                  selected = 'all'),
      radioButtons('expert_rev', label = 'AquaMaps review status',
                   choices = list('All'             = 'all',
                                  'Expert reviewed' = 'expert'),
                   selected = 'all'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/align_tab_main1.md'),
      # plotlyOutput('quad_plot', height = '300px'),
      hr(),
      includeMarkdown('pages/align_tab_main2.md')
      # plotOutput('barchart', height = '300px')
    )
  ),

  tabPanel('Species maps',
    sidebarPanel(
      includeMarkdown('pages/map_tab_side1.md'),
      selectInput('spp_group', 'Select a taxonomic group:',
                  choices = unique(spp_list$spp_group_text) %>% sort()),
      selectInput('species', 'Then select a species:',
                  choices = unique(spp_list$name) %>% sort()),
      radioButtons('show_maps', label = 'Data source',
                   choices = list('AquaMaps' = 'am',
                                  'IUCN'     = 'iucn',
                                  'Both'     = 'both'),
                   selected = 'both'),
      includeMarkdown('pages/map_tab_side2.md'),
      # plotOutput('mini_quad', height = '150px'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      # includeMarkdown('pages/map_tab_main1.md'),
      # plotOutput('compare_map') #, width = '100%')
    )
  ),

  tabPanel('Coral depth',
    sidebarPanel(
      includeMarkdown('pages/coral_tab_side1.md'),
      selectInput('coral_spp', 'Select a coral species:',
                  choices = coral_spp_list$name %>%
                                     sort()),
      includeMarkdown('pages/coral_tab_side2.md'),
      # plotOutput('coral_quad', height = '150px'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/coral_tab_main1.md'),
      # plotOutput('coral_map'),
      includeMarkdown('pages/coral_tab_main2.md'),
      div(img(src='barchart_coral_quads.png', height = 158, width = 750), style="text-align: center;")
      # plotOutput('coral_barchart', height = '250px')
    )
  ),
  
  tabPanel('Tables',
    sidebarPanel(
      includeMarkdown('pages/article_info.md'),
      radioButtons('table_file', label = 'Table: ',
                  choices = c('Table 1 Updates to status and trend data and models' = 'table1',
                              'Table 2 Updates to pressure data and models' = 'table2'),
                  selected = 'table1')
    ),
    mainPanel(
      dataTableOutput('table_display')
    )
  ),

  tabPanel('References',
    sidebarPanel(
      includeMarkdown('pages/article_info.md'),
      includeMarkdown('pages/acknowledgments.md')
    ),
    mainPanel(
      includeMarkdown('pages/references.md')
    )
  )

)
