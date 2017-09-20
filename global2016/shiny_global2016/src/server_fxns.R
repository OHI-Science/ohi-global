### server_fxns.R

####################################################.
##### Functions for Fig 1 score/trend maps tab #####
####################################################.

fig1_scores <- rad_df %>%
  filter(dimension == 'score')

fig1_colors <- c(colorRampPalette(brewer.pal(11, 'RdYlBu'))(101))

col_brks   <- seq(0, 100, by = 1)
col_assign <- cut(col_brks, breaks = col_brks, include.lowest = TRUE)
colors_df  <- data.frame(colors_plot = fig1_colors, col_assign, stringsAsFactors = FALSE)

fig1_scores$col_assign <- cut(fig1_scores$value, breaks = col_brks, include.lowest = TRUE)

fig1_scores <- left_join(fig1_scores, colors_df, by = 'col_assign') %>%
  arrange(value)

fig1_trend_df <- annual_change_df %>%
  arrange(goal_code, annual_change) %>%
  left_join(goal_names, by = c('goal_code')) %>%
  select(goal_code, goal, region_id, annual_change)

get_trend_breaks <- function(data_df) {
  x <- range(data_df$annual_change, na.rm = TRUE) %>% abs() %>% max()
  trend_breaks <- c(1, 2, 5, 10, 20, 30, 50)
  y <- min(trend_breaks[trend_breaks > x])
  col_brks  <- seq(-y, y, length.out = 101)
  # message('range value = ', 
  #         paste(range(data_df$annual_change, na.rm = TRUE), collapse = ' to '), 
  #         '; col_brks set to ', paste(col_brks, collapse = ', '))
  
  return(col_brks)
}


create_fig1_score_hist <- function(fig1_scenario, fig1_goal) {
  ### fig1_scenario <- 2016; fig1_goal <- 'Index'
  message(fig1_scenario, ' ', fig1_goal)
  goal_code <- goal_names$goal_code[goal_names$goal == fig1_goal] %>%
    as.character()
  fig1_scen_num <- str_extract(fig1_scenario, '[0-9]{4}') %>% as.integer()
  fig1_scores_sub <- fig1_scores %>%
    filter(scenario == fig1_scen_num) %>%
    filter(goal == goal_code)
  
  fig1_score_cols <- fig1_scores_sub$colors_plot %>% unique()
  
  fig1_hist <- ggplot(fig1_scores_sub, aes(x = value)) +
    ggtheme_grid() +
    geom_histogram(aes(fill = col_assign), color = NA, bins = 31) +  
    scale_fill_manual(values = fig1_score_cols, guide = FALSE) +
    xlim(c(0, 100)) +
    labs(y = 'Number of regions', 
         x = paste0(fig1_goal, ' score ', fig1_scen_num))
}


create_fig1_trend_hist <- function(fig1_goal) {
  ### fig1_goal <- 'Index'
  ### fig1_goal <- 'Species condition (Biodiversity)'
  fig1_trend_sub <- fig1_trend_df %>%
    filter(goal == fig1_goal) %>%
    distinct() %>%
    arrange(annual_change)
  
  trend_colors <- c(colorRampPalette(brewer.pal(11, 'RdYlBu'))(101))

  col_brks <- get_trend_breaks(fig1_trend_sub)
  col_assign <- cut(col_brks, breaks = col_brks, include.lowest = TRUE)
  colors_df  <- data.frame(colors_plot = trend_colors, col_assign, stringsAsFactors = FALSE)
  
  fig1_trend_sub$col_assign <- cut(fig1_trend_sub$annual_change, breaks = col_brks, include.lowest = TRUE)
  
  fig1_trend_sub <- left_join(fig1_trend_sub, colors_df, by = 'col_assign')
  
  fig1_trend_cols <- fig1_trend_sub$colors_plot %>% unique()
  
  fig1_trend_hist <- ggplot(fig1_trend_sub, aes(x = annual_change)) +
    ggtheme_grid() +
    geom_histogram(aes(fill = col_assign), bins = 25, center = 1.01, color = NA, size = .25) +
    scale_fill_manual(breaks = col_brks, values = fig1_trend_cols, guide = FALSE) +
    labs(y = 'Number of regions', 
         x = paste0(fig1_goal, ' average annual change')) +
    geom_vline(xintercept = 0, color = 'red', alpha = .3) +
    xlim(min(col_brks), max(col_brks))

}

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
              select(region_id, region_name),
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
                  aes(x = trend, y = goal, label = region_name),
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
               family = 'Arial', color = '#313695', size = 2.5) +
      annotate('text', label = 'Area weighted',
               x = -2.8, y = 1.6,
               hjust = 0, ### tie to NP values
               family = 'Arial', color = 'orange', size = 2.5)
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
      geom_point(aes(color = continent, text = paste0('region: ', region_name)),
                 shape = 19, size = 2, alpha = 0.5) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig3_plot <- fig3_plot +
      geom_point(aes(key = continent, text = paste0('region: ', region_name)),
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
             color = 'grey30', size = 2.5) +
    xlim(c(40, 100)) + 
    labs(y = 'Average change in OHI score per year', 
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
                     color = subregion, text = paste0('region: ', region_name)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig3_plot <- fig3_plot +
      geom_point(aes(x = index_score, y = index_trend, 
                     key = subregion, text = paste0('region: ', region_name)),
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
             color = 'grey30', size = 2.5) +
    xlim(c(40, 100)) + 
    labs(y = 'Average change in OHI score per year)', 
         x = 'OHI score (2016)') 
  
  return(fig3_plot)
  
}

##################################################.
##### Functions for Fig 4 trend barchart tab #####
##################################################.

fig4_df <- trend_2016_all %>%
  select(-region_name) %>%
  left_join(georgns, by = 'region_id') %>%
  filter(region_id != 0) %>%
  select(-region_id, -SPP, -HAB, -ECO, -LIV, -FIS, -MAR, -ICO, -LSP) %>%
  gather('goal_code', 'trend', -region_name, -continent, -subregion) %>%
  left_join(goal_names, by = 'goal_code') %>%
  select(region_name, continent, goal, trend)

fig4_order <- fig4_df %>%
  filter(goal == 'Index') %>%
  arrange(trend)

fig4_df <- fig4_df %>%
  mutate(region_name = factor(region_name, levels = fig4_order$region_name))

create_fig4_plot <- function(fig4_filter, fig4_georgn, fig4_overall) {
  
  if(fig4_filter == 'himidlo') {
    ### set up dataframe of top 15, middle 10, bottom 15
    n_rgn <- length(fig4_order$region_name)
    countries_mid <- c('Johnston Atoll', 'Jarvis Island', 'Ile Europa', 'Libya', 'Russia',
                       'Cook Islands', 'Norfolk Island', 'Lithuania', 'Egypt', 'Sao Tome/Principe')
    fig4_order_sub <- rbind(fig4_order[1:15, ],
                            fig4_order[fig4_order$region_name %in% countries_mid, ],
                            fig4_order[(n_rgn - 14):n_rgn, ])
    fig4_df_sub <- fig4_df %>%
      filter(region_name %in% fig4_order_sub$region_name)  %>%
      mutate(region_name = factor(region_name, levels = fig4_order_sub$region_name))
    
  } else if (fig4_georgn != 'Global') {
    ### set up dataframe of countries in a given continent
    fig4_order_sub <- fig4_order %>%
      filter(continent == fig4_georgn)
    fig4_df_sub <- fig4_df %>%
      filter(region_name %in% fig4_order_sub$region_name)  %>%
      mutate(region_name = factor(region_name, levels = fig4_order_sub$region_name))
    
  } else {
    ### use entire dataframe
    fig4_df_sub <- fig4_df
    fig4_order_sub <- fig4_order
    
  }
  
  fig4_plot <- ggplot(data = fig4_df_sub %>%
                        filter(goal != 'Index'), 
                      aes(x = region_name, y = trend)) +
    ggtheme_blank(textsize = 10) +
    theme(panel.grid.major = element_line(color = 'grey92', size = .25))
  
  if(fig4_filter == 'himidlo') {
    ### plot a grey band behind the plot to distinguish the middle 10
    yrange <- fig4_df_sub %>%
      group_by(region_name) %>%
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

data_goal <- function(rad_df, goal_code) { # goal_code <- 'Index'
  
  data_g <- rad_df %>%
    filter(goal == goal_code)
  
  data_g <- data_g %>%
    filter(scenario %in% c(2012, 2016)) %>%
    mutate(dim_scen = paste(dimension, scenario, sep = '_')) %>%
    select(dim_scen, region_id, region_name, continent, value) %>%
    spread(dim_scen, value) %>%
    mutate(pred_change = likely_future_state_2012 - status_2012,
           obs_change  = status_2016 - status_2012,
           r_minus_p   = resilience_2012 - pressures_2012)
  
  return(data_g)
}

fig5_df_index <- read_csv('data/fig5_data.csv') %>%
  select(-region_name) %>%
  left_join(georgns, by = 'region_id')
# mod <- lm(score_2016 ~ score_2012, data = fig5_df)
# summary(mod)

lm_clean <- function(formula, data_df) {
  mdl <- lm(formula, data = data_df) %>%
    summary()
  
  form_text <- as.character(formula)[c(2, 1, 3)] %>%
    paste(collapse = ' ')
  
  if(is.nan(mdl$r.squared)) {
    return(list('mdl_text' = sprintf('<b><i>%s</i></b>:<br>linear model not valid', form_text),
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
    
    mdl_text <- sprintf('<b><i>%s</i></b>:<br>%s<br>intercept: %.3f%s<br>R<sup>2</sup>: %.3f',
                        form_text, 
                        mdl_slopes,
                        mdl_clean$estimate[1], mdl_clean$sig[1],
                        mdl$r.squared)
    
    return(list('mdl_text' = mdl_text, 'slope' = mdl_clean$estimate[2:nrow(mdl_clean)], 'intercept' = mdl_clean$estimate[1]))
  }
}

create_fig5_plot <- function(fig5_colors, fig5_georgn, 
                             fig5_lm, fig5_goal, 
                             y_var, x_var,
                             y_lab, x_lab,
                             lim_0_100 = TRUE) {
  
  # message('in create_fig5_plot')
  
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
    select_('region_id', 'region_name', 'continent', x_var, y_var)
  
  # message('Fig 5 for ', fig5_goal, ' in ', fig5_georgn, ' x = ', x_var, ' y = ', y_var)

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
      geom_point(aes(text = region_name, color = continent), 
                 shape = 19, size = 1.75, 
                 alpha = 0.5) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig5_plot <- fig5_plot +
      geom_point(aes(text = region_name), 
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
                     (min(fig5_df_sub[ , x_var], na.rm = TRUE) + 3 * max(fig5_df_sub[ , x_var], na.rm = TRUE))/4,
                     (min(fig5_df_sub[ , x_var], na.rm = TRUE) * 3 + max(fig5_df_sub[ , x_var], na.rm = TRUE))/4)
      y_pt <- (min(fig5_df_sub[ , y_var],  na.rm = TRUE) * 3 + max(fig5_df_sub[ , y_var],  na.rm = TRUE))/4 + 1
    }
    fig5_plot <- fig5_plot +
      annotate('text', x = x_pt, y = y_pt,
               label = mdl_clean$mdl_text, 
               color = 'grey30', size = 2.5) +
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
      geom_point(aes(color = continent, text = paste0('region ', region_name)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig6_congo_gilbert <- fig6_df %>%
      filter(region_id %in% c(100, 119, 212))
    fig6_all_else <- fig6_df %>%
      filter(!region_id %in% c(100, 119, 212))
    fig6_plot <- fig6_plot +
      geom_point(data = fig6_all_else,
                 aes(key = continent, text = paste0('region ', region_name)),
                 shape = 19, size = 2, color = 'darkblue', alpha = 0.5) +
      geom_point(data = fig6_congo_gilbert,
                 aes(key = continent, text = paste0('region ', region_name)),
                 shape = 19, size = 2, color = 'darkorange3', alpha = 1) +
      annotate("text", x = -10, y =  -2, label = "Republique du Congo", 
               color = "darkorange3", size = 2.5) +
      annotate("text", x =  -5, y = -88, label = "Gilbert Islands (Kiribati)", 
               color = "darkorange3", size = 2.5) +
      annotate("text", x =   3, y = -15, label = "Saint Kitts and Nevis", 
               color = "darkorange3", size = 2.5)
       
  }
  
  fig6_mdl <- lm_clean(rank_delta ~ score_delta, fig6_all_df)
  
  fig6_plot <- fig6_plot +
    ### what are these points?
    # geom_point(data = fig6_df %>% 
    #              filter(score_delta > 0 & rank_delta < 0), 
    #            aes(key = subregion, text = paste0('region ', region_name)),
    #            shape=19, size=1.75, color='#D73027', alpha=0.75) +
    # geom_point(data = fig6_df %>% 
    #              filter(score_delta < 0 & rank_delta > 0), 
    #            aes(key = subregion, text = paste0('region ', region_name)),
    #            shape=19, size=1.75, color='#4575B4', alpha=0.75) +
    coord_cartesian(ylim = c(-125, 100), xlim = c(-20, 11)) +
    ### add global linear model line:
    geom_abline(slope = fig6_mdl$slope, intercept = fig6_mdl$intercept,
                color = 'darkred', size = 0.5) +
    annotate('text', x = 5, y = -60, 
             label = fig6_mdl$mdl_text, 
             color = 'grey30', size = 2.5) +
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
      geom_point(aes(color = subregion, text = paste0('region ', region_name)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    fig6_plot <- fig6_plot +
      geom_point(aes(key = subregion, text = paste0('region ', region_name)),
                 shape = 19, size = 2, color = 'darkblue', alpha = 0.5)
  }
  
  fig6_plot <- fig6_plot +
    coord_cartesian(ylim = c(-125, 100), xlim = c(-20, 11)) +
    ### add regional linear model line:
    geom_abline(slope = fig6_mdl_rg$slope, intercept = fig6_mdl_rg$intercept,
                color = 'darkred', size = 0.5) +
    annotate('text', x = 5, y = -60, 
             label = fig6_mdl_rg$mdl_text, 
             color = 'grey30', size = 2.5) +
    labs(y = 'Rank change (2012 to 2016)', 
         x = 'Score change (2012 to 2016)') 
  
  return(fig6_plot)
  
}

##### Plotting maps of scores and trends #####

get_map_file <- function(map_scen, map_goal) {
  goal_code <- goal_names[goal_names$goal == map_goal, 1] %>%
    as.character() %>% tolower()
  # message('goal code = ', goal_code, '; map_scen = ', map_scen)
  map_file <- sprintf('maps/%s/%s_%s.png', map_scen, goal_code, map_scen)
  # message('map file = ', map_file)
  # message('File exists? ', file.exists(map_file), list.files())
  return(map_file)
}

##### Displaying and downloading data #####

# data_view_df <- rad_df %>%
#   filter(dimension == 'score') %>%
#   mutate(scenario = paste0('score_', scenario)) %>%
#   spread(scenario, value) %>%
#   select(-dimension, georegion = continent, goal_code = goal) %>%
#   left_join(goal_names, by = 'goal_code')
# 
# write_csv(data_view_df, 'tables/data_view.csv')

# data_download_df <- rad_df %>%
#   select(-continent, -subregion, -region_name)
# rgn_lookup <- rad_df %>%
#   select(region_id, georegion = continent, subregion, region_name) %>%
#   distinct()
# goal_lookup <- goal_names
# 
# write_csv(data_download_df, 'tables/ohi_data_2012_2016.csv')
# write_csv(rgn_lookup,       'tables/rgn_lookup.csv')
# write_csv(goal_lookup,      'tables/goal_lookup.csv')

get_data_download <- function(data_request) {
  data_df <- read_csv('tables/ohi_data_2012_2016.csv') %>%
    select(year = scenario, region_id, goal_code = goal, dimension, value) %>%
    left_join(read_csv('tables/goal_lookup.csv'), by = 'goal_code') %>%
    left_join(read_csv('tables/rgn_lookup.csv'), by = 'region_id') %>%
    select(year, 
           region_id, region_name,
           goal_code, goal,
           dimension, value, 
           georegion, subregion)
  
  if(!'goal' %in% data_request) {
    data_df <- data_df %>%
      select(-goal)
  }
  if(!'rgn' %in% data_request) {
    data_df <- data_df %>%
      select(-region_name, -georegion, -subregion)
  }
  if(!'all_vals' %in% data_request) {
    data_df <- data_df %>%
      filter(dimension == 'score') %>%
      select(-dimension)
  }
  return(data_df)
}
