### server_setup.R

### Set up basic stuff
# library(sf)
library(RColorBrewer)

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
                 text = element_text(size = textsize, color = 'grey20'),
                 plot.title = element_text(size = textsize * 1.5, face = 'bold'))
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
                 text = element_text(size = textsize, color = 'grey20'),
                 plot.title = element_text(size = textsize * 1.5, face = 'bold'))
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

rad_df <- read_csv('data/radical_2016-11-17.csv') %>%
  # filter(goal != "Index") %>%
  filter(region_id != 0) %>%         # this weighted mean includes high seas and Antarctica
  filter(region_id != 300) %>%
  filter(region_id <= 250) %>%       # get rid of high seas regions
  filter(region_id != 213)  %>%
  left_join(georgns, by=c('region_id'))

# annual_change_df <- rad_df %>%
#   group_by(goal, region_id) %>%
#   filter(dimension == 'score') %>%
#   filter(!is.na(value)) %>%
#   do(annual_change = lm(value ~ scenario, data = .)[['coefficients']][['scenario']]) %>%
#   mutate(annual_change = round(annual_change, 5))
# 
# write_csv(annual_change_df, 'data/annual_change.csv')

annual_change_df <- read_csv('data/annual_change.csv')
