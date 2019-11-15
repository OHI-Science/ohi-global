###########################################
## Radical data preparation
###########################################
library(dplyr)
saveFile = "global2019"
dateFile = '2019-11-15'

# list of goals:
goalList <- c('AO', 'BD', 'CP', 'CS', 'CW', 'ECO', 'FIS', 'FP', 'HAB', 'ICO', 'LE', 'LIV', 'LSP', 'MAR', 'NP', 'SP', 'SPP', 'TR')

setwd(here::here("yearly_results/global2019"))

## currently pair 2015 Antarctica and 2014 HS data with 2014-2017 EEZ analysis

###########################################################
## Antarctica data: only gets reported as one summary value:
###########################################################

antarctica <- read.csv('https://raw.githubusercontent.com/OHI-Science/ohi-global-antarctica/master/antarctica2015/scores.csv', 
                       stringsAsFactors=FALSE) %>%
  filter(region_id == 0) %>%
  mutate(region_id = 213) 

# use this to fill in missing values:
allGoals_ant <- rbind(expand.grid(region_id = 213, 
                                  scenario = 2012:2019,
                            goal = goalList, 
                            dimension = c('future', 'score', 'status', 'pressures', 'resilience', 'trend')), 
                      expand.grid(region_id = 213, 
                                  scenario = 2012:2019,
                               goal= "Index",
                               dimension = c('future', 'score'))
)

                               
antarctica_all <- allGoals_ant %>%
  left_join(antarctica) %>%
  arrange(scenario, region_id, goal, dimension)

# replace 2012:2013 scores with NA values
antarctica_all <- antarctica_all %>%
  mutate(score = ifelse(scenario %in% c(2012:2013), NA, score))

###########################################################
## High Seas data
###########################################################
hs <- read.csv('https://raw.githubusercontent.com/OHI-Science/ohi-global-highseas/master/highseas2014/scores.csv', 
               stringsAsFactors=FALSE) %>%
  filter(region_id != 0)   

# use this to fill in missing values:
allGoals_hs <- rbind(expand.grid(region_id = unique(hs$region_id), 
                                 scenario = 2012:2019,
                        goal = goalList, 
                        dimension = unique(hs$dimension)),
                     expand.grid(region_id = unique(hs$region_id),
                                 scenario = 2012:2019,
                                 goal = "Index",
                                 dimension = c("future", "score")
                     )
)

hs_all <- allGoals_hs %>%
  left_join(hs) %>%
  arrange(scenario, region_id, goal, dimension)

# replace 2012:2013 scores with NA values 
hs_all <- hs_all %>%
  mutate(score = ifelse(scenario %in% c(2012:2013), NA, score))


###########################################################
## EEZ data
###########################################################
### area data
area <- read.csv('https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/eez/layers/rgn_area.csv') %>%
  select(region_id=rgn_id, area_km2)

eez <- read.csv(here(sprintf('yearly_results/%s/scores_%s.csv', saveFile, dateFile)),
                stringsAsFactors=FALSE)

eez <- eez %>%
  mutate(region_id = ifelse(region_id==0, 300, region_id)) %>%
  filter(region_id != 213) %>%   # add in real Antarctica data later
  select(region_id, scenario=year, goal, dimension, score)

all <- eez %>%
   bind_rows(hs_all) %>%
   bind_rows(antarctica_all) %>%
  left_join(area)

## Calculate global indices when region_id==0
goal_0 <- all %>%
  dplyr::filter(region_id !=300) %>%
  dplyr::filter(dimension %in% c('future', 'score', 'status')) %>%
  dplyr::group_by(scenario, goal, dimension) %>%
  dplyr::summarize(score = weighted.mean(score, area_km2, na.rm=TRUE)) %>%
  dplyr::mutate(region_id = 0) %>%
  dplyr::mutate(score = ifelse(scenario %in% 2012:2013, NA, score))


all <- all %>%
  bind_rows(goal_0) %>%
  mutate(dimension = ifelse(dimension == 'future', 'likely_future_state', dimension)) %>%
  mutate(score = round(score, 2)) %>%
  select(scenario, goal, dimension, region_id, value=score)

radical <- all %>%
  arrange(scenario, goal, dimension, region_id)

write.csv(radical, sprintf(here('yearly_results/%s/croscon_ohi_scores_%s.csv'), saveFile, dateFile), row.names=FALSE, na="")

## truncate to 5 years of data for website
current_year <- max(radical$scenario)
recent5years <- (current_year-4):current_year

radical_truncated <- radical %>%
  filter(scenario %in% recent5years)

summary(radical_truncated)
write.csv(radical_truncated, sprintf(here('yearly_results/%s/croscon_ohi_scores_%s_truncated.csv'), saveFile, dateFile), row.names=FALSE, na="")

## check against last year's dimensions
old <- read.csv(here("yearly_results/global2018/croscon_ohi_scores_2018-11-28.csv"))
dim(old) # 182504 rows
tmp <- filter(radical, scenario %in% 2012:2018) 
dim(tmp) 

# these should all be the same
table(radical$region_id > 250, radical$goal)
table(radical$region_id == 213, radical$goal) # Antarctica, not using this year?
table(radical$region_id <= 250, radical$goal)
table(radical$scenario)
