### server_fxns.R
# library(stringr)
# library(tmap)
# library(RColorBrewer)

### Set up basic stuff

# create a blank ggplot theme
ggtheme_basic <- function(textsize = 10) {
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

goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')
goal_names <- data.frame(goal = goals, 
                         goal_long = c("Index", 
                                       "Artisanal opportunities",
                                       "Species condition (Biodiversity)",
                                       "Biodiversity",
                                       "Habitat (Biodiversity)",
                                       "Coastal protection",
                                       "Carbon storage",
                                       "Clean water",
                                       "Economies",
                                       "Livelihoods & economies",
                                       "Livelihoods",
                                       "Fisheries (Food provision)",
                                       "Food provision",
                                       "Mariculture (Food provision)",
                                       "Iconic species (Sense of place)",
                                       "Sense of place",
                                       "Lasting special places (Sense of place)",
                                       "Natural products",
                                       "Tourism & recreation"))

############################.
##### Load data frames #####
############################.

# setwd('~/github/ohi-global/global2016/shiny_global2016')
index_2016 <- read_csv('data/scores_eez2016.csv') %>%
  select(region_id, country, index_score = Index)

index_2012 <- read_csv("data/scores_eez2012.csv") %>%
  select(region_id, country, index_score = Index)

trend_2016 <- read_csv('data/trends_2016.csv') %>%
  select(region_id, index_trend = Index)

georgns <- read_csv('data/georegion_labels.csv') %>%
  select(-world, -country) ### country already in index2016

index_gl2016 <- index_2016$index_score[index_2016$region_id == 0]
trend_gl2016 <- trend_2016$index_trend[trend_2016$region_id == 0]

### set up dataframes for plots
trend_v_score_df <- trend_2016 %>%
  left_join(index_2016, by = 'region_id') %>%
  left_join(georgns,   by = 'region_id') %>%
  filter(region_id != 0)

rank_2016 <- index_2016 %>%
  select(region_id, country, score2016 = index_score) %>%
  filter(region_id != 0) %>%
  arrange(score2016) %>%
  mutate(rank2016 = min_rank(score2016))

rank_2012 <- index_2012 %>%
  select(region_id, country, score2012 = index_score) %>%
  filter(region_id != 0) %>%
  arrange(score2012) %>%
  mutate(rank2012 = min_rank(score2012))

rankchange_all_df <- rank_2012 %>%
  left_join(rank_2016, by=c("region_id", "country")) %>%
  left_join(georgns, by = 'region_id') %>%
  mutate(score_delta = score2016 - score2012) %>%
  mutate(rank_delta = rank2016 - rank2012)


############################################.
##### Functions for trend-vs-score tab #####
############################################.

create_tvs_plot_global <- function(georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish continents
  ### by color; otherwise simply include as a key.
  
  message('in create_tvs_plot_global()')
  
  tvs_df <- trend_v_score_df
  
  tvs_plot <- ggplot2::ggplot(tvs_df, aes(x = index_score, y = index_trend)) +
    ggtheme_basic()
  
  if(georgn_color) {
    tvs_plot <- tvs_plot +
      geom_point(aes(color = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.5) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    tvs_plot <- tvs_plot +
      geom_point(aes(key = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'grey20', alpha = 0.5)
  }
  
  tvs_plot <- tvs_plot +
    ### add zero trend indicator:
    geom_hline(yintercept = 0, color = 'red', size = .5, linetype = 3, alpha = .5) +
    ### add global mean status indicator:
    geom_vline(xintercept = index_gl2016, color = 'red', size = .5, linetype = 3, alpha = .5) +
    ### add global linear model line:
    stat_smooth(method = lm, se = FALSE, color = 'grey20', size = 0.5) +
    labs(y = 'Average change in OHI score per year)', 
         x = 'OHI score (2016)') 
  
  return(tvs_plot)
  
}

create_tvs_plot_georgn <- function(georgn, georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish subregions
  ### by color; otherwise simply include as a key.
  
  message('in create_tvs_plot_georgn()')
  
  tvs_df <- trend_v_score_df %>%
    filter(continent == georgn)
  
  tvs_plot <- ggplot2::ggplot(tvs_df) +
    ggtheme_basic() +
    geom_point(data = trend_v_score_df, aes(x = index_score, y = index_trend),
               color = 'grey40', shape = 19, size = 2, alpha = 0.1)
    
  if(georgn_color) {
    tvs_plot <- tvs_plot +
      geom_point(aes(x = index_score, y = index_trend, 
                     color = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.7) +
    scale_color_brewer(palette = 'Dark2')
  } else {
    tvs_plot <- tvs_plot +
      geom_point(aes(x = index_score, y = index_trend, 
                     key = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'grey20', alpha = 0.5)
  }
  
  tvs_plot <- tvs_plot +
    ### add zero trend indicator:
    geom_hline(yintercept = 0, color = 'red', size = .5, linetype = 3, alpha = .5) +
    ### add global mean status indicator:
    geom_vline(xintercept = index_gl2016, color = 'red', size = .5, linetype = 3, alpha = .5) +
    ### add global linear model line:
    stat_smooth(data = trend_v_score_df,
                aes(x = index_score, y = index_trend),
                method = lm, se = FALSE,
                color = 'grey70', size = 0.5, alpha = .3) +
    ### add regional linear model line:
    stat_smooth(method = lm, se = FALSE, 
                aes(x = index_score, y = index_trend),
                color = 'grey20', size = 0.5, alpha = 1, fullrange = TRUE) +
    labs(y = 'Average change in OHI score per year)', 
         x = 'OHI score (2016)') 
  
  return(tvs_plot)
  
}

############################################.
##### Functions for change-in-rank tab #####
############################################.

create_rankchange_plot_global <- function(georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish subregions
  ### by color; otherwise simply include as a key.
  
  message('in create_rankchange_plot_georgn()')
  
  rankchange_df <- rankchange_all_df
  
  rankchange_plot <- ggplot2::ggplot(rankchange_df, aes(x = score_delta, y = rank_delta)) +
    ggtheme_basic()
  
  if(georgn_color) {
    rankchange_plot <- rankchange_plot +
      geom_point(aes(color = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    rankchange_plot <- rankchange_plot +
      geom_point(aes(key = continent, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'grey20', alpha = 0.5)
  }
  
  rankchange_plot <- rankchange_plot +
    ### what are these points?
    geom_point(data = rankchange_df %>% filter(score_delta > 0 & rank_delta < 0), 
               aes(key = subregion, text = paste0('country: ', country)),
               shape=19, size=1.75, color="#D73027", alpha=0.75) +
    geom_point(data = rankchange_df %>% filter(score_delta < 0 & rank_delta > 0), 
               aes(key = subregion, text = paste0('country: ', country)),
               shape=19, size=1.75, color="#4575B4", alpha=0.75) +
    coord_cartesian(ylim = c(-125, 100), xlim = c(-20, 11)) +
    ### add global linear model line:
    stat_smooth(data = rankchange_all_df,
                method = lm, se = FALSE,
                color = 'grey70', size = 0.5, alpha = .3) +
    labs(y = 'Rank change (2012 to 2016)', 
         x = 'Score change (2012 to 2016)') 
  
  return(rankchange_plot)
  
}

create_rankchange_plot_georgn <- function(georgn, georgn_color) {
  ### In this version, use georgn_color == TRUE to distinguish subregions
  ### by color; otherwise simply include as a key.
  
  message('in create_rankchange_plot_georgn()')
  
  rankchange_df <- rankchange_all_df %>%
    filter(continent == georgn)
  
  rankchange_plot <- ggplot2::ggplot(rankchange_df, aes(x = score_delta, y = rank_delta)) +
    ggtheme_basic() +
    geom_point(data = rankchange_all_df,
               color = 'grey40', shape = 19, size = 2, alpha = 0.1)
  
  if(georgn_color) {
    rankchange_plot <- rankchange_plot +
      geom_point(aes(color = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, alpha = 0.7) +
      scale_color_brewer(palette = 'Dark2')
  } else {
    rankchange_plot <- rankchange_plot +
      geom_point(aes(key = subregion, text = paste0('country: ', country)),
                 shape = 19, size = 2, color = 'grey20', alpha = 0.5)
  }
  
  rankchange_plot <- rankchange_plot +
    ### what are these points?
    # geom_point(data = rankchange_df %>% filter(score_delta > 0 & rank_delta < 0), 
    #            aes(key = subregion, text = paste0('country: ', country)),
    #            shape=19, size=1.75, color="#D73027", alpha=0.75) +
    # geom_point(data = rankchange_df %>% filter(score_delta < 0 & rank_delta > 0), 
    #            aes(key = subregion, text = paste0('country: ', country)),
    #            shape=19, size=1.75, color="#4575B4", alpha=0.75) +
    coord_cartesian(ylim = c(-125, 100), xlim = c(-20, 11)) +
    ### add global linear model line:
    stat_smooth(data = rankchange_all_df,
                method = lm, se = FALSE,
                color = 'grey70', size = 0.5, alpha = .3) +
    ### add regional linear model line:
    stat_smooth(method = lm, se = FALSE, 
                color = 'grey20', size = 0.5, alpha = 1, fullrange = TRUE) +
    labs(y = 'Rank change (2012 to 2016)', 
         x = 'Score change (2012 to 2016)') 
  
  return(rankchange_plot)
  
}

### NMLE trend plot
### set up dataframes:

nlme_results_noLE <- read_csv('data/nlme_results_noLE.csv') %>%
  mutate(trend = round(trend, 5),
         trend_wts = round(trend_wts, 5))

levels_goals <- c("Index", as.character(rev(nlme_results_noLE$goal_long[nlme_results_noLE$goal!="Index"][order(nlme_results_noLE$trend[nlme_results_noLE$goal!="Index"])])))

nlme_results_noLE <- nlme_results_noLE %>%
  mutate(goal_long = factor(goal_long, levels = rev(levels_goals))) %>%
  arrange(goal_long)

nlme_data_noLE <- read_csv('data/nlme_data_noLE.csv') %>%
  mutate(trend     = round(trend, 5),
         goal_long = factor(goal_long, levels = rev(levels_goals))) %>%
  left_join(index_2016 %>%
              select(region_id, country),
            by = 'region_id')

create_fig2_plot <- function(fig2_show_pts) {
  
  if(!exists('fig2_show_pts')) fig2_show_pts <- TRUE # fig2_show_pts <- FALSE
    
  ### Initialize plot
  fig2_plot <- ggplot(nlme_results_noLE, aes(x = trend, y = goal_long)) +
    ggtheme_basic()
  
  ### Add greyed-in points for all rgn-level values
  if(fig2_show_pts == TRUE) {
    fig2_plot <- fig2_plot +
      geom_jitter(data = nlme_data_noLE, aes(label = country),
                  alpha = .1)
    
    dot_alpha = 1.0
    
  } else {
    fig2_plot <- fig2_plot +
      xlim(c(-4, 1))
    
    dot_alpha = 0.7
    
  }
  
  fig2_plot <- fig2_plot +
    ### add unweighted mean trend points:
    geom_point(size = 3, color = "#313695", alpha = dot_alpha) +
    ### add zero line:
    geom_vline(xintercept = 0, color = "red", linetype = 3) +
    ### add weighted points, and white circles for insignificant results
    geom_point(data = nlme_results_noLE %>%
                 filter(sig == 0),
               aes(x = trend, y = goal_long), color = "white", size = 2) +
    geom_point(data = nlme_results_noLE,
               aes(x = trend_wts, y = goal_long), 
               size = 3, color = "orange", alpha = dot_alpha) +
    geom_point(data = nlme_results_noLE %>% 
                 filter(sig_wts == 0),
               aes(x = trend_wts, y = goal_long), 
               color = "white", size = 2) +
    ### labels, theme, and flip it
    labs(y = "Average change in score per year") +
    theme(axis.title.x       = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey70", linetype = "dashed"))
  
  if(fig2_show_pts == FALSE) {
    ### annotate Natural Products points
    fig2_plot <- fig2_plot +
      annotate("text", label = "Unweighted",
               x = -3.9, y = 1.6,
               hjust = 0, ### tie to NP values
               family = "Arial", color = "#313695", size = 2.6) +
      annotate("text", label = "Area weighted",
               x = -2.8, y = 1.6,
               hjust = 0, ### tie to NP values
               family = "Arial", color = "orange", size = 2.6)
  }
  
  return(fig2_plot)
  
}

## another plot of the data (Fig 2b)

# data_trend <- data %>%
#   filter(!(goal %in% c("ECO", "LIV", "LE"))) %>%
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
#   facet_grid(goal ~ ., scales="free") + 
#   theme_bw() +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         #     strip.text.y = element_text(size = 8, angle = 0))
#         strip.text.y = element_blank())
