source('~/github/ohiprep/src/R/common.R')
library(readr)
library(ggplot2)

goalname <- 'TR'
scenario <- c('eez2012', 'eez2013', 'eez2014', 'eez2015', 'eez2016')
dim_test <- 'score'

georegion_labels <- read_csv('~/github/ohicore/inst/extdata/georegion_labels.csv') %>%
  select(rgn_id, r1 = r1_label, r2 = r2_label, rgn_name = rgn_label) %>%
  group_by(r1) %>%
  mutate(n_r1 = n()) %>%
  ungroup()

scores_df <- lapply(scenario, FUN = function(x) {
      read_csv(file.path('~/github/ohi-global', x, 'scores.csv')) %>%
        mutate(scenario = x)
    }) %>%
  bind_rows() %>%
  filter(goal == goalname) %>%
  rename(rgn_id = region_id) %>%
  inner_join(georegion_labels, by = 'rgn_id')


status_line <- ggplot(scores_df %>% 
                        filter(dimension == dim_test), 
                      aes(x = scenario, y = score, group = rgn_id, color = r1)) +
  geom_line(size = 1, alpha = .8) +
  # scale_color_brewer(palette = 'RdYlBu') +
  labs(color = 'georegion',
       title = paste0(goalname, ' ', dim_test))
  
print(status_line)
ggsave(paste0('testing_goals_cco/', goalname, '_', dim_test, '.png'))

diffs_df <- scores_df %>%
  filter(dimension == dim_test) %>%
  group_by(rgn_id) %>%
  mutate(incr_score = first(score) < last(score),
         decr_score = first(score) > last(score)) %>%
  ungroup()

incr_df <- diffs_df %>% 
  filter(incr_score)
if(nrow(incr_df) > 0) {
  incr_status_line <- ggplot(incr_df, 
                             aes(x = scenario, y = score, group = rgn_id, color = r1)) +
    geom_line(size = 1, alpha = .8) +
    # scale_color_brewer(palette = 'RdYlBu') +
    labs(color = 'georegion',
         title = paste0('increase in ', goalname, ' ', dim_test, '; n = ', length(incr_df$rgn_id %>% unique())))
  
  print(incr_status_line)
  ggsave(paste0('testing_goals_cco/', goalname, '_', dim_test, '_incr.png'))
  
  incr_df1 <- incr_df %>%
    select(rgn_id, rgn_name, r1, n_r1) %>%
    distinct() %>%
    group_by(r1) %>%
    mutate(prop_incr = n()/n_r1) %>%
    ungroup() %>%
    select(r1, prop_incr) %>%
    distinct()
  
  print(incr_df1)
}

decr_df <- diffs_df %>% 
  filter(decr_score)
if(nrow(decr_df) > 0) {
  decr_status_line <- ggplot(decr_df, 
                             aes(x = scenario, y = score, group = rgn_id, color = r1)) +
    geom_line(size = 1, alpha = .8) +
    # scale_color_brewer(palette = 'RdYlBu') +
    labs(color = 'georegion',
         title = paste0('decrease in ', goalname, ' ', dim_test, '; n = ', length(decr_df$rgn_id %>% unique())))
  
  print(decr_status_line)
  ggsave(paste0('testing_goals_cco/', goalname, '_', dim_test, '_decr.png'))
  
  decr_df1 <- decr_df %>%
    select(rgn_id, rgn_name, r1, n_r1) %>%
    distinct() %>%
    group_by(r1) %>%
    mutate(prop_decr = n()/n_r1) %>%
    ungroup() %>%
    select(r1, prop_decr) %>%
    distinct()
  
  print(decr_df1)
}

same_df <- diffs_df %>% 
  filter(!decr_score & !incr_score)
if(nrow(same_df) > 0) {
  same_df_check <- same_df %>%
    select(-scenario) %>%
    distinct() %>%
    group_by(rgn_id) %>%
    mutate(dupes = n() > 1) %>%
    ungroup()
  
  same_status_pt <- ggplot(same_df %>% 
                              filter(!decr_score & !incr_score) %>%
                             filter(scenario == 'eez2016'), 
                           aes(x = scenario, y = score, color = r1)) +
    geom_jitter(height = 0) +
    # scale_color_brewer(palette = 'RdYlBu') +
    labs(color = 'georegion',
         title = paste0('no change in ', goalname, ' ', dim_test, '; n = ', length(same_df$rgn_id %>% unique())))
  
  print(same_status_pt)
  ggsave(paste0('testing_goals_cco/', goalname, '_', dim_test, '_same.png'))
  
  same_df1 <- same_df %>%
    select(rgn_id, rgn_name, r1, n_r1) %>%
    distinct() %>%
    group_by(r1) %>%
    mutate(prop_same = n()/n_r1) %>%
    ungroup() %>%
    select(r1, prop_same) %>%
    distinct()
  
  print(same_df1)
}

na_df <- scores_df %>%
  filter(dimension == dim_test & is.na(score)) %>%
  mutate(blah = paste0(scenario, '_', rgn_name))

message('NAs in ', goalname, ' ', dim_test, ':\n  ', paste0(na_df$blah, collapse = '\n  '))


### some additional testing/plotting to examine by specific regions
scores_df %>% 
  filter(str_detect(r2, 'Aust')) %>% ### filter by r2 to access sub-region names; by r1 for georegion names
  filter(dimension == dim_test) %>%
  .$score %>% 
  summary() 
  hist()

ggplot(scores_df %>% 
         filter(str_detect(r2, 'Aust')) %>% ### can filter by r1 regions or r2 subregions here;
         filter(dimension == dim_test), 
       aes(x = scenario, y = score, group = rgn_id, color = r2)) + ### color = r2 so codes by sub-regions
  geom_line(size = 1, alpha = .8) +
  # scale_color_brewer(palette = 'RdYlBu') +
  labs(color = 'georegion',
       title = paste0(goalname, ' ', dim_test))
