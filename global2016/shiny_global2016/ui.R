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

  tabPanel('Fig 2',
           sidebarPanel(
             # includeMarkdown('pages/fig2_tab_side1.md'),
             checkboxInput('fig2_show_all', 
                           label = 'Show individual countries?',
                           value = FALSE),
             includeMarkdown('pages/footer_sidebar.md')
           ),
           mainPanel(
             # includeMarkdown('pages/fig2_tab_main1.md'),
             plotlyOutput('fig2_plot', height = '300px'),
             hr()
             # includeMarkdown('pages/fig2_tab_main2.md')
           )
  ),
  
  tabPanel('Trend v Score',
    sidebarPanel(
      # includeMarkdown('pages/fig3_tab_side1.md'),
      selectInput('tvs_georegion', 'Choose a georegion to view:',
                  choices = c('Global', continents %>% sort()),
                  selected = 'Global'),
      checkboxInput('tvs_colors', 
                    label = 'Color-code regions?',
                    value = FALSE),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      # includeMarkdown('pages/fig3_tab_main1.md'),
      plotlyOutput('tvs_plot', height = '300px'),
      hr()
      # includeMarkdown('pages/fig3_tab_main2.md')
    )
  ),
  
  tabPanel('Change in Rank',
    sidebarPanel(
      # includeMarkdown('pages/fig6_tab_side1.md'),
      selectInput('rankchange_georgn', 'Choose a georegion to view:',
                  choices = c('Global', continents %>% sort()),
                  selected  = 'Global'),
      checkboxInput('rankchange_colors', 
                    label = 'Color-code regions?',
                    value = FALSE),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      # includeMarkdown('pages/fig6_tab_main1.md'),
      plotlyOutput('rankchange_plot', height = '300px'),
      hr()
      # includeMarkdown('pages/fig6_tab_main2.md')
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
