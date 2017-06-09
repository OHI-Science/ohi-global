### server_fxns.R


### Set up basic stuff

# create a blank ggplot theme
ggtheme_blank <- function(textsize = 10) {
  theme_bw() +
    theme_update(panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 text = element_text(size = textsize),
                 plot.title = element_text(size = textsize * 1.5))
}

ggtheme_grid <- function(textsize = 10) {
  theme_bw() +
    theme_update(panel.grid.minor = element_blank(),
                 panel.grid.major = element_line(color = 'grey92'),
                 panel.background = element_blank(),
                 plot.background = element_blank(),
                 panel.border = element_rect(fill = NA, color = 'grey92'),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 text = element_text(size = textsize),
                 plot.title = element_text(size = textsize * 1.5))
}


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

############################.
##### Load data frames #####
############################.

# setwd('~/github/ohi-global/global2016/shiny_global2016')
index_2016_all <- read_csv('data/scores_eez2016.csv') 
index_2016 <- index_2016_all %>%
  select(region_id, index_score = Index)

index_2012 <- read_csv('data/scores_eez2012.csv') %>%
  select(region_id, index_score = Index)

trend_2016_all <- read_csv('data/trends_2016.csv') 
trend_2016 <- trend_2016_all %>%
  select(region_id, index_trend = Index)

georgns <- read_csv('data/georegion_labels2.csv') %>%
  select(-world)

index_gl2016 <- index_2016$index_score[index_2016$region_id == 0]
trend_gl2016 <- trend_2016$index_trend[trend_2016$region_id == 0]



###################################################.
##### Functions for Fig 2 trend of scores tab #####
###################################################.

### NMLE trend plot
### set up dataframes:

nlme_results_noLE <- read_csv('data/nlme_results_noLE.csv') %>%
  mutate(trend = round(trend, 5),
         trend_wts = round(trend_wts, 5))

levels_goals <- c('Index', 
                  (nlme_results_noLE$goal[nlme_results_noLE$goal!='Index'][order(nlme_results_noLE$trend[nlme_results_noLE$goal!='Index'])] %>%
                     rev() %>%
                     as.character()))

nlme_results_noLE <- nlme_results_noLE %>%
  mutate(goal = factor(goal, levels = rev(levels_goals)),
         sig  = ifelse(sig == 0, 'not significant', 'significant'),
         sig_wts = ifelse(sig == 0, 'not significant', 'significant')) %>%
  arrange(goal)

nlme_df_noLE <- read_csv('data/nlme_data_noLE.csv') %>%
  mutate(trend = round(trend, 5),
         goal  = factor(goal, levels = rev(levels_goals))) %>%
  left_join(georgns %>%
              select(region_id, country),
            by = 'region_id')

create_fig2_plot <- function(fig2_show_pts) {
  
  if(!exists('fig2_show_pts')) fig2_show_pts <- TRUE # fig2_show_pts <- FALSE
  
  ### Initialize plot
  fig2_plot <- ggplot(nlme_results_noLE) +
    ggtheme_blank()
  
  ### Add greyed-in points for all rgn-level values
  if(fig2_show_pts == TRUE) {
    fig2_plot <- fig2_plot +
      geom_jitter(data = nlme_df_noLE,
                  aes(x = trend, y = goal, label = country),
                  height = .25, width = 0, color = 'grey80', alpha = .3)
    
    dot_alpha = 1.0
    
  } else {
    fig2_plot <- fig2_plot +
      xlim(c(-4, 1))
    
    dot_alpha = 0.7
    
  }
  
  fig2_plot <- fig2_plot +
    ### add zero line:
    geom_vline(xintercept = 0, color = 'red', linetype = 3) +
    ### add unweighted mean trend points:
    geom_point(aes(x = trend, y = goal), alpha = 0, size = 0) +
    ### add this in to set x axis
    geom_point(data = nlme_results_noLE %>%
                 filter(sig == 'significant'),
               aes(x = trend, y = goal, 
                   text = 'unweighted\nsignificant'),
               shape = 21, size = 3, 
               color = '#313695', fill = '#313695',
               alpha = dot_alpha) +
    ### add weighted points, and white circles for insignificant results
    geom_point(data = nlme_results_noLE %>%
                 filter(sig == 'not significant'),
               aes(x = trend, y = goal,
                   text = 'unweighted\nnot significant'),
               shape = 21, size = 3, 
               color = '#313695', fill = 'white',
               alpha = dot_alpha) +
    geom_point(data = nlme_results_noLE %>% 
                 filter(sig_wts == 'significant'),
               aes(x = trend_wts, y = goal, 
                   text = 'area weighted\nsignificant'),
               shape = 21, size = 3, 
               color = 'orange', fill = 'orange',
               alpha = dot_alpha) +
    # geom_point(data = nlme_results_noLE %>% 
    #              filter(sig_wts == 'not significant'),
    #            ### NO INSTANCES OF THIS: comment out
    #            aes(x = trend_wts, y = goal,
    #                text = 'area weighted\nnot significant'),
    #            shape = 21, size = 3, 
    #            color = 'orange', fill = 'white',
    #            alpha = dot_alpha) +
    ### labels, theme, and flip it
    labs(y = 'Average change in score per year') +
    theme(axis.title.x       = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey70', linetype = 'dashed'))
  
  if(fig2_show_pts == FALSE) {
    ### annotate Natural Products points
    fig2_plot <- fig2_plot +
      annotate('text', label = 'Unweighted',
               x = -3.9, y = 1.6,
               hjust = 0, ### tie to NP values
               family = 'Arial', color = '#313695', size = 2.6) +
      annotate('text', label = 'Area weighted',
               x = -2.8, y = 1.6,
               hjust = 0, ### tie to NP values
               family = 'Arial', color = 'orange', size = 2.6)
  }
  
  return(fig2_plot)
  
}

## another plot of the data (Fig 2b)

# data_trend <- data %>%
#   filter(!(goal %in% c('ECO', 'LIV', 'LE'))) %>%
#   group_by(scenario, goal) %>%
#   summarize(min_score = min(value, na.rm=TRUE),
#             max_score = max(value, na.rm=TRUE),
#             mean_score = mean(value, na.rm=TRUE)) %>%
#   ungroup() %>%
#   mutate(goal = factor(goal, levels=rev(nlme_results$goal))) %>%
#   filter(!is.na(goal))
# 
# 
# ggplot(data_trend, aes(x=scenario, y=mean_score)) +
#   geom_line() + 
#   facet_grid(goal ~ ., scales='free') + 
#   theme_bw() +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         #     strip.text.y = element_text(size = 8, angle = 0))
#         strip.text.y = element_blank())

##################################################.
##### Functions for Fig 3 trend-vs-score tab #####
##################################################.

fig3_df <- trend_2016 %>%
  left_join(index_2016, by = 'region_id') %>%
  left_join(georgns,    by = 'region_id') %>%
  filter(region_id != 0)

create_fig3_plot_global <- function(georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish continents
  ### by color; otherwise simply include as a key.
  
  lm_global <- lm_clean(index_trend ~ index_score, fig3_df)
  
  fig3_plot <- ggplot(fig3_df, aes(x = index_score, y = index_trend)) +
    ggtheme_grid()
  
  if(georgn_color) {
    fig3_plot <- fig3_plot +
      geom_point(aes(color = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.5) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig3_plot <- fig3_plot +
      geom_point(aes(key = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'darkblue', alpha = 0.5)
  }
  
  fig3_plot <- fig3_plot +
    ### add zero trend indicator:
    geom_hline(yintercept = 0, color = 'red', size = .5, linetype = 3, alpha = .3) +
    ### add global mean status indicator:
    geom_vline(xintercept = index_gl2016, color = 'red', size = .5, linetype = 3, alpha = .3) +
    ### add global linear model line:
    geom_abline(slope     = lm_global$slope,
                intercept = lm_global$intercept,
                color = 'darkred', size = 0.5) +
    annotate('text', x = 82, y = -2.5, 
             label = lm_global$mdl_text, 
             color = 'grey20', size = 3) +
    xlim(c(40, 100)) + 
    labs(y = 'Average change in OHI score per year)', 
         x = 'OHI score (2016)') 
  
  return(fig3_plot)
  
}

create_fig3_plot_georgn <- function(georgn, georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish subregions
  ### by color; otherwise simply include as a key.
  
  fig3_df_sub <- fig3_df %>%
    filter(continent == georgn)
  
  lm_global <- lm_clean(index_trend ~ index_score, fig3_df)
  lm_rgn    <- lm_clean(index_trend ~ index_score, fig3_df_sub)
  
  fig3_plot <- ggplot(fig3_df_sub) +
    ggtheme_grid() +
    geom_point(data = fig3_df, aes(x = index_score, y = index_trend),
               color = 'grey80', shape = 19, size = 2, alpha = 0.3)
  if(georgn_color) {
    fig3_plot <- fig3_plot +
      geom_point(aes(x = index_score, y = index_trend, 
                     color = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig3_plot <- fig3_plot +
      geom_point(aes(x = index_score, y = index_trend, 
                     key = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'darkblue', alpha = 0.5)
  }
  
  fig3_plot <- fig3_plot +
    ### add zero trend indicator:
    geom_hline(yintercept = 0, color = 'red', size = .5, linetype = 3, alpha = .3) +
    ### add global mean status indicator:
    geom_vline(xintercept = index_gl2016, color = 'red', size = .5, linetype = 3, alpha = .3) +
    ### add global linear model line:
    geom_abline(slope     = lm_global$slope, 
                intercept = lm_global$intercept,
                color = 'grey70', size = 0.5, alpha = .3) +
    ### add regional linear model line:
    geom_abline(slope     = lm_rgn$slope, 
                intercept = lm_rgn$intercept,
                color = 'darkred', size = 0.5, alpha = 1, fullrange = TRUE) +
    annotate('text', x = 82, y = -2.5, 
             label = lm_rgn$mdl_text, 
             color = 'grey20', size = 3) +
    xlim(c(40, 100)) + 
    labs(y = 'Average change in OHI score per year)', 
         x = 'OHI score (2016)') 
  
  return(fig3_plot)
  
}

##################################################.
##### Functions for Fig 4 trend barchart tab #####
##################################################.

fig4_df <- trend_2016_all %>%
  select(-country) %>%
  left_join(georgns,   by = 'region_id') %>%
  filter(region_id != 0) %>%
  select(-region_id, -SPP, -HAB, -ECO, -LIV, -FIS, -MAR, -ICO, -LSP) %>%
  gather('goal_code', 'trend', -country, -continent, -subregion) %>%
  left_join(goal_names, by = 'goal_code') %>%
  select(country, continent, goal, trend)

fig4_order <- fig4_df %>%
  filter(goal == 'Index') %>%
  arrange(trend)

fig4_df <- fig4_df %>%
  mutate(country = factor(country, levels = fig4_order$country))

create_fig4_plot <- function(fig4_filter, fig4_georgn, fig4_overall) {
  
  if(fig4_filter == 'himidlo') {
    ### set up dataframe of top 15, middle 10, bottom 15
    n_rgn <- length(fig4_order$country)
    countries_mid <- c('Johnston Atoll', 'Jarvis Island', 'Ile Europa', 'Libya', 'Russia',
                       'Cook Islands', 'Norfolk Island', 'Lithuania', 'Egypt', 'Sao Tome/Principe')
    fig4_order_sub <- rbind(fig4_order[1:15, ],
                            fig4_order[fig4_order$country %in% countries_mid, ],
                            fig4_order[(n_rgn - 14):n_rgn, ])
    fig4_df_sub <- fig4_df %>%
      filter(country %in% fig4_order_sub$country)  %>%
      mutate(country = factor(country, levels = fig4_order_sub$country))
    
  } else if (fig4_georgn != 'Global') {
    ### set up dataframe of countries in a given continent
    fig4_order_sub <- fig4_order %>%
      filter(continent == fig4_georgn)
    fig4_df_sub <- fig4_df %>%
      filter(country %in% fig4_order_sub$country)  %>%
      mutate(country = factor(country, levels = fig4_order_sub$country))
    
  } else {
    ### use entire dataframe
    fig4_df_sub <- fig4_df
    fig4_order_sub <- fig4_order
    
  }
  
  fig4_plot <- ggplot(data = fig4_df_sub %>%
                        filter(goal != 'Index'), 
                      aes(x = country, y = trend)) +
    ggtheme_blank(textsize = 10) +
    theme(panel.grid.major = element_line(color = 'grey92', size = .25))
  
  if(fig4_filter == 'himidlo') {
    ### plot a grey band behind the plot to distinguish the middle 10
    yrange <- fig4_df_sub %>%
      group_by(country) %>%
      summarize(tr = sum(trend, na.rm = TRUE)) %>%
      .$tr %>% range()
    
    fig4_plot <- fig4_plot +
      scale_x_discrete() + ### otherwise, geom_rect would coerce to continuous,
      ### which wouldn't work for geom_bar()
      annotate('rect', 
               xmin = 15.5, xmax = 26.5, ymin = yrange[1] - 3, ymax = yrange[2] + 3,
               color = NA, fill = 'grey50', alpha = .1) 
  }
  
  ### create the actual bar plot
  fig4_plot <- fig4_plot +
    geom_bar(aes(fill = goal), stat = 'identity') +
    scale_fill_manual(values = RColorBrewer::brewer.pal(10, 'Spectral')) +
    guides(fill = guide_legend(reverse = TRUE)) +
    geom_hline(yintercept = 0, color = 'white', size = .5)
  ### plotly messes up with coord_flip, so add as vline
  
  if(fig4_overall == TRUE) {
    ### show overall trend as little bars if desired
    fig4_plot <- fig4_plot +
      # geom_bar(data = fig4_df_sub %>%
      #            filter(goal == 'Index'),
      #          aes(text = 'Overall trend'),
      #          color = NA, fill = 'black',
      #          show.legend = FALSE,
      #          stat = 'identity',
      #          width = .2)
      geom_crossbar(data = fig4_df_sub %>%
                      filter(goal == 'Index'),
                    aes(text = 'Overall trend', ymin = trend, ymax = trend),
                    color = 'black', fill = NA,
                    show.legend = FALSE)
  }
  
  ### wrap up the plot
  fig4_plot <- fig4_plot +
    coord_flip(expand = FALSE) +
    labs(x = '',
         y = 'Average change in score per year',
         fill = 'Goal')
  
  if(fig4_filter == 'georgn' & fig4_georgn == 'Global') {
    ### For full global plot, shrink the text size
    fig4_plot <- fig4_plot +
      theme(axis.text.x = element_text(size = 6))
  }
  
  return(fig4_plot)
  
}

####################################################.
##### Functions for Fig 5 model evaluation tab #####
####################################################.

##### Note first section is for 'Index' selection

rad_df <- read_csv('data/radical_2016-11-17.csv') %>%
  # filter(goal != "Index") %>%
  filter(region_id != 0) %>%         # this weighted mean includes high seas and Antarctica
  filter(region_id != 300) %>%
  filter(region_id <= 250) %>%       # get rid of high seas regions
  filter(region_id != 213)  %>%
  left_join(georgns, by=c('region_id'))

data_goal <- function(rad_df, goal_code) { # goal_code <- "Index"
  
  data_g <- rad_df %>%
    filter(goal == goal_code)
  
  data_g <- data_g %>%
    filter(scenario %in% c(2012, 2016)) %>%
    mutate(dim_scen = paste(dimension, scenario, sep = "_")) %>%
    select(dim_scen, region_id, country, continent, value) %>%
    spread(dim_scen, value) %>%
    mutate(pred_change = likely_future_state_2012 - status_2012,
           obs_change  = status_2016 - status_2012,
           r_minus_p   = resilience_2012 - pressures_2012)
  
  return(data_g)
}

fig5_df_index <- read_csv('data/fig5_data.csv') %>%
  select(-country) %>%
  left_join(georgns, by = 'region_id')
# mod <- lm(score_2016 ~ score_2012, data = fig5_df)
# summary(mod)

lm_clean <- function(formula, data_df) {
  mdl <- lm(formula, data = data_df) %>%
    summary()
  
  form_text <- as.character(formula)[c(2, 1, 3)] %>%
    paste(collapse = ' ')
  
  if(is.nan(mdl$r.squared)) {
    return(list('mdl_text' = sprintf('%s: <br>  linear model not valid', form_text),
                'slope' = NA, 'intercept' = NA))
  } else {
    mdl_clean <- mdl %>%
      broom::tidy() %>%
      mutate(sig = ifelse(p.value < .1,     '*', ' (not sig)'),
             sig = ifelse(p.value < .01,   '**', sig),
             sig = ifelse(p.value < .001, '***', sig))
    mdl_slopes <- mdl_clean %>%
      filter(term != '(Intercept)') %>%
      mutate(slope_text = sprintf('slope (%s): %.3f%s', term, estimate, sig)) %>%
      summarize(slope_text = paste(slope_text, collapse = '<br>')) %>%
      .$slope_text
    
    mdl_text <- sprintf('<b><i>%s</i></b>:<br>%s<br>intercept: %.3f%s<br>adj. R<sup>2</sup>: %.3f',
                        form_text, 
                        mdl_slopes,
                        mdl_clean$estimate[1], mdl_clean$sig[1],
                        mdl$adj.r.squared)
    
    return(list('mdl_text' = mdl_text, 'slope' = mdl_clean$estimate[2:nrow(mdl_clean)], 'intercept' = mdl_clean$estimate[1]))
  }
}

create_fig5_plot <- function(fig5_colors, fig5_georgn, 
                             fig5_lm, fig5_goal, 
                             y_var, x_var,
                             y_lab, x_lab,
                             lim_0_100 = TRUE) {
  
  message('in create_fig5_plot')
  
  if(fig5_goal == 'Index') {
    fig5_df <- fig5_df_index
  } else {
    goal_code <- goal_names %>%
      mutate(goal_code = as.character(goal_code)) %>%
      filter(goal == fig5_goal) %>%
      .$goal_code
    
    fig5_df <- data_goal(rad_df, goal_code) 
  }
  
  if(fig5_georgn != 'Global') {
    fig5_df_sub <- fig5_df %>%
      filter(continent == fig5_georgn)
  } else {
    fig5_df_sub <- fig5_df
  }
  
  fig5_df_sub <- fig5_df_sub %>%
    select_('region_id', 'country', 'continent', x_var, y_var)
  
  message('Fig 5 for ', fig5_goal, ' in ', fig5_georgn, ' x = ', x_var, ' y = ', y_var)
  print(head(fig5_df_sub))
  
  fig5_plot <- ggplot(fig5_df_sub, aes_string(x = x_var, y = y_var)) +
    ggtheme_grid() +
    labs(x = x_lab, 
         y = y_lab)
  
  if(fig5_georgn != 'Global') {
    fig5_plot <- fig5_plot +
      geom_point(data = fig5_df, 
                 color = 'grey80', shape = 19, size = 1.75, 
                 alpha = 0.3)
  }
  
  if(fig5_colors) {
    fig5_plot <- fig5_plot +
      geom_point(aes(text = country, color = continent), 
                 shape = 19, size = 1.75, 
                 alpha = 0.5) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig5_plot <- fig5_plot +
      geom_point(aes(text = country), 
                 shape = 19, size = 1.75, 
                 color = 'darkblue', alpha = 0.5)
  }
  
  mdl_formula <- as.formula(sprintf('%s ~ %s', y_var, x_var))
  mdl_clean <- lm_clean(mdl_formula, fig5_df_sub) 
  
  if(!is.na(mdl_clean$slope)) {
    fig5_plot <- fig5_plot +
      geom_abline(slope = 1, intercept = 0, color = 'red', alpha = .3)
  }
  
  if(lim_0_100) {
    fig5_plot <- fig5_plot +
      xlim(0, 100) +
      ylim(0, 100)
  }
  
  if(fig5_lm) {
    ### print linear model line and box
    if(lim_0_100) {
      x_pt <- ifelse(mdl_clean$slope[1] > 0, 75, 25)
      y_pt <- 25
    } else {
      x_pt <- ifelse(mdl_clean$slope[1] > 0 | is.na(mdl_clean$slope[1]), 
                     (min(fig5_df_sub[ , x_var], na.rm = TRUE) + 4 * max(fig5_df_sub[ , x_var], na.rm = TRUE))/5,
                     (min(fig5_df_sub[ , x_var], na.rm = TRUE) * 4 + max(fig5_df_sub[ , x_var], na.rm = TRUE))/5)
      y_pt <- (min(fig5_df_sub[ , y_var],  na.rm = TRUE) * 4 + max(fig5_df_sub[ , y_var],  na.rm = TRUE))/5 + 1
    }
    fig5_plot <- fig5_plot +
      annotate('text', x = x_pt, y = y_pt,
               label = mdl_clean$mdl_text, 
               color = 'grey20', size = 3) +
      expand_limits(y = y_pt + 1)
    
    if(!is.na(mdl_clean$slope)) {
      fig5_plot <- fig5_plot +
        geom_abline(slope = mdl_clean$slope, intercept = mdl_clean$intercept,
                    color = 'darkred', size = 0.5)
    }
  }
  
  return(fig5_plot)
}





# create_fig5goal_text <- function(fig5_georgn, fig5_goal) {
#   ### fig5_goal <- 'Tourism & recreation'
#   ### fig5_goal <- 'Carbon storage'
#   message('in create_fig5goal_text, for ', fig5_goal, 'in ', fig5_georgn)
#   
#   goal_code <- goal_names %>%
#     mutate(goal_code = as.character(goal_code)) %>%
#     filter(goal == fig5_goal) %>%
#     .$goal_code
#   fig5goal_df <- data_goal(rad_df, goal_code) 
#   
#   if(fig5_georgn != 'Global') {
#     fig5goal_df <- fig5goal_df %>%
#       filter(continent == fig5_georgn)
#   }
#   
#   mdl1 <- lm_clean(obs_change ~ pred_change, data = fig5goal_df)
#   
#   mdl2 <- lm_clean(obs_change ~ trend_2012 + r_minus_p, data = fig5goal_df)
#   
#   mdl3 <- lm_clean(obs_change ~ trend_2012 + pressures_2012 + resilience_2012, data = fig5goal_df)
#   
#   fig5goal_text <- paste(ifelse(!is.na(mdl1$slope), 
#                                 '<b>Linear regression models (first model is plotted in dark red above):</b>', 
#                                 '<b>Linear regression models:</b>'),
#                          mdl1$mdl_text, mdl2$mdl_text, mdl3$mdl_text,
#                          sep = '<p>') %>%
#     str_replace_all('\\n', '<br>• ') %>%
#     str_replace_all('\\^2', '<sup>2</sup>')  
#   
#   return(fig5goal_text)
#   
# }

##################################################.
##### Functions for Fig 6 change-in-rank tab #####
##################################################.

rank_2016 <- index_2016 %>%
  select(region_id, score2016 = index_score) %>%
  filter(region_id != 0) %>%
  arrange(score2016) %>%
  mutate(rank2016 = min_rank(score2016))

rank_2012 <- index_2012 %>%
  select(region_id, score2012 = index_score) %>%
  filter(region_id != 0) %>%
  arrange(score2012) %>%
  mutate(rank2012 = min_rank(score2012))

fig6_all_df <- rank_2012 %>%
  left_join(rank_2016, by = 'region_id') %>%
  left_join(georgns,   by = 'region_id') %>%
  mutate(score_delta = score2016 - score2012,
         rank_delta  = rank2016 - rank2012)

create_fig6_plot_global <- function(georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish subregions
  ### by color; otherwise simply include as a key.
  
  fig6_df <- fig6_all_df
  
  fig6_plot <- ggplot(fig6_df, aes(x = score_delta, y = rank_delta)) +
    ggtheme_grid()
  
  if(georgn_color) {
    fig6_plot <- fig6_plot +
      geom_point(aes(color = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig6_plot <- fig6_plot +
      geom_point(aes(key = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'darkblue', alpha = 0.5)
  }
  
  fig6_mdl <- lm_clean(rank_delta ~ score_delta, fig6_all_df)
  
  fig6_plot <- fig6_plot +
    ### what are these points?
    # geom_point(data = fig6_df %>% 
    #              filter(score_delta > 0 & rank_delta < 0), 
    #            aes(key = subregion, text = paste0('country: ', country)),
    #            shape=19, size=1.75, color='#D73027', alpha=0.75) +
    # geom_point(data = fig6_df %>% 
    #              filter(score_delta < 0 & rank_delta > 0), 
    #            aes(key = subregion, text = paste0('country: ', country)),
    #            shape=19, size=1.75, color='#4575B4', alpha=0.75) +
    coord_cartesian(ylim = c(-125, 100), xlim = c(-20, 11)) +
    ### add global linear model line:
    geom_abline(slope = fig6_mdl$slope, intercept = fig6_mdl$intercept,
                color = 'darkred', size = 0.5) +
    annotate('text', x = 5, y = -60, 
             label = fig6_mdl$mdl_text, 
             color = 'grey20', size = 3) +
    labs(y = 'Rank change (2012 to 2016)', 
         x = 'Score change (2012 to 2016)') 
  
  return(fig6_plot)
  
}

create_fig6_plot_georgn <- function(georgn, georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish subregions
  ### by color; otherwise simply include as a key.
  
  fig6_df <- fig6_all_df %>%
    filter(continent == georgn)
  
  fig6_mdl_gl <- lm_clean(rank_delta ~ score_delta, fig6_all_df)
  fig6_mdl_rg <- lm_clean(rank_delta ~ score_delta, fig6_df)
  
  fig6_plot <- ggplot(fig6_df, aes(x = score_delta, y = rank_delta)) +
    ggtheme_grid() +
    geom_point(data = fig6_all_df,
               color = 'grey80', shape = 19, size = 2, alpha = 0.3) +
    ### add global linear model line:
    geom_abline(slope = fig6_mdl_gl$slope, intercept = fig6_mdl_gl$intercept,
                color = 'grey70', size = 0.5, alpha = .3)
  
  if(georgn_color) {
    fig6_plot <- fig6_plot +
      geom_point(aes(color = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig6_plot <- fig6_plot +
      geom_point(aes(key = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'darkblue', alpha = 0.5)
  }
  
  fig6_plot <- fig6_plot +
    coord_cartesian(ylim = c(-125, 100), xlim = c(-20, 11)) +
    ### add regional linear model line:
    geom_abline(slope = fig6_mdl_rg$slope, intercept = fig6_mdl_rg$intercept,
                color = 'darkred', size = 0.5) +
    annotate('text', x = 5, y = -60, 
             label = fig6_mdl_rg$mdl_text, 
             color = 'grey20', size = 3) +
    labs(y = 'Rank change (2012 to 2016)', 
         x = 'Score change (2012 to 2016)') 
  
  return(fig6_plot)
  
}
