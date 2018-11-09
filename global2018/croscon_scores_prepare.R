###########################################
## Radical data preparation
###########################################
library(dplyr)
saveFile = "global2018"
dateFile = '2018-10-10'

# list of goals:
goalList <- c('AO', 'BD', 'CP', 'CS', 'CW', 'ECO', 'FIS', 'FP', 'HAB', 'ICO', 'LE', 'LIV', 'LSP', 'MAR', 'NP', 'SP', 'SPP', 'TR')

setwd(here::here("global2018"))

## currently pair 2015 Antarctica and 2014 HS data with 2014-2017 EEZ analysis

###########################################################
## Antarctica data: only gets reported as one summary value:
###########################################################

antarctica <- read.csv('../antarctica2015/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id == 0) %>%
  mutate(region_id = 213) 

# use this to fill in missing values:
allGoals_ant <- rbind(expand.grid(region_id = 213, 
                                  scenario = 2012:2017,
                            goal = goalList, 
                            dimension = c('future', 'score', 'status', 'pressures', 'resilience', 'trend')), 
                      expand.grid(region_id = 213, 
                                  scenario = 2012:2017,
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
hs <- read.csv('../highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id != 0)   

# use this to fill in missing values:
allGoals_hs <- rbind(expand.grid(region_id = unique(hs$region_id), 
                                 scenario = 2012:2017,
                        goal = goalList, 
                        dimension = unique(hs$dimension)),
                     expand.grid(region_id = unique(hs$region_id),
                                 scenario = 2012:2017,
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
area <- read.csv('../eez/layers/rgn_area.csv') %>%
  select(region_id=rgn_id, area_km2)

eez <- read.csv('../eez/scores.csv', stringsAsFactors=FALSE)

eez <- eez %>%
  mutate(region_id = ifelse(region_id==0, 300, region_id)) %>%
  filter(region_id != 213) %>%   # add in real Antarctica data later
  select(region_id, scenario=year, goal, dimension, score)

all <- eez %>%
  # bind_rows(hs_all) %>%
  # bind_rows(antarctica_all) %>%
  left_join(area)

## Calculate global indices when region_id==0
goal_0 <- all %>%
  dplyr::filter(region_id !=300) %>%
  dplyr::filter(dimension %in% c('future', 'score', 'status')) %>%
  dplyr::group_by(scenario, goal, dimension) %>%
  dplyr::summarize(score = weighted.mean(score, area_km2, na.rm=TRUE)) %>%
  dplyr::mutate(region_id = 0) %>%
  dplyr::mutate(score = ifelse(scenario %in% 2012:2013, NA, score)) # ??? reason for not including years 2012-2013?


all <- all %>%
  bind_rows(goal_0) %>%
  mutate(dimension = ifelse(dimension == 'future', 'likely_future_state', dimension)) %>%
  mutate(score = round(score, 2)) %>%
  select(scenario, goal, dimension, region_id, value=score)

radical <- all %>%
  arrange(scenario, goal, dimension, region_id)

write.csv(radical, sprintf('croscon_ohi_scores_%s.csv', dateFile), row.names=FALSE, na="")

## check against last year's dimensions
old <- read.csv("../global2017/croscon_ohi_scores_2017-11-22.csv")
dim(old) # 156,432 rows
tmp <- filter(radical, scenario %in% 2012:2017) 
dim(tmp) # 145,872 rows, doesn't match.. because not including High Seas and Antarctica perhaps?

# these should all be the same
table(radical$region_id > 250, radical$goal)
table(radical$region_id == 213, radical$goal) # Antarctica, not using this year?
table(radical$region_id <= 250, radical$goal)
table(radical$scenario)
