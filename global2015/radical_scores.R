###########################################
## Radical data preparation
###########################################
library(dplyr)

# list of goals:
goalList <- c('AO', 'BD', 'CP', 'CS', 'CW', 'ECO', 'FIS', 'FP', 'HAB', 'ICO', 'LE', 'LIV', 'LSP', 'MAR', 'NP', 'SP', 'SPP', 'TR')


## currently pair 2014 Antarctica and HS data with 2014 and 2015 EEZ analysis

###########################################################
## Antarctica data: only gets reported as one summary value:
###########################################################

antarctica <- read.csv('antarctica2014/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id == 0) %>%
  mutate(region_id = 213) 

# use this to fill in missing values:
allGoals_ant <- rbind(expand.grid(region_id = 213, 
                            goal = goalList, 
                            dimension = c('future', 'score', 'status', 'pressures', 'resilience', 'trend')), 
                      expand.grid(region_id = 213, 
                               goal= "Index",
                               dimension = c('future', 'score'))
)

                               
antarctica_all <- allGoals_ant %>%
  left_join(antarctica) %>%
  arrange(region_id, goal, dimension)

###########################################################
## High Seas data
###########################################################
hs <- read.csv('highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id != 0)   

# use this to fill in missing values:
allGoals_hs <- rbind(expand.grid(region_id = unique(hs$region_id), 
                        goal = goalList, 
                        dimension = unique(hs$dimension)),
                     expand.grid(region_id = unique(hs$region_id),
                                 goal = "Index",
                                 dimension = c("future", "score")
                     )
)

hs_all <- allGoals_hs %>%
  left_join(hs)


###########################################################
## EEZ data
###########################################################
### area data
area <- read.csv('eez2015/layers/rgn_area.csv') %>%
  select(region_id=rgn_id, area_km2)

radical <- data.frame()
### for scenario years 2014 and 2015
for(scenarioYear in 2014:2015){ #scenarioYear=2015

data <- read.csv(sprintf('eez%s/scores.csv', scenarioYear), stringsAsFactors=FALSE)

eez <- data %>%
  mutate(region_id = ifelse(region_id==0, 300, region_id)) %>%
  filter(region_id != 213)  # add in real Antarctica data later

all <- eez %>%
  bind_rows(hs_all) %>%
  bind_rows(antarctica_all) %>%
  left_join(area)

## Calculate global indices when region_id==0
goal_0 <- all %>%
  filter(region_id !=300) %>%
  filter(dimension %in% c('future', 'score', 'status')) %>%
  group_by(goal, dimension) %>%
  summarize(score = weighted.mean(score, area_km2, na.rm=TRUE)) %>%
  mutate(region_id = 0)
data.frame(goal_0)

all <- all %>%
  bind_rows(goal_0) %>%
  mutate(scenario = scenarioYear) %>%
  mutate(dimension = ifelse(dimension == 'future', 'likely_future_state', dimension)) %>%
  mutate(score = round(score, 2)) %>%
  select(scenario, goal, dimension, region_id, value=score)

radical <- bind_rows(radical, all)
}  

for(scenarioYear in 2012:2013){ #scenarioYear=2012
  
  data <- read.csv(sprintf('eez%s/scores.csv', scenarioYear), stringsAsFactors=FALSE)
  
  eez <- data %>%
    mutate(region_id = ifelse(region_id==0, 300, region_id)) %>%
    mutate(scenario = scenarioYear) %>%
    mutate(dimension = ifelse(dimension == 'future', 'likely_future_state', dimension)) %>%
    mutate(score = round(score, 2)) %>%
    select(scenario, goal, dimension, region_id, value=score)
  
  radical <- bind_rows(radical, eez)
}  

radical <- radical %>%
  arrange(scenario, goal, dimension, region_id)

write.csv(radical, sprintf('global2015/radical_%s.csv', Sys.Date()), row.names=FALSE, na="")

##################################
## checking calculations
##################################
goalsOnly <- c('AO', 'BD', 'CP', 'CS', 'CW', 'FP', 'LE', 'NP', 'SP', 'TR')


# this matches the region_id = 300 goals exactly (already calculated, these are the rgn_id==0 in the eez data)
rgnGoalSummary <- tmp %>%
  filter(!(region_id %in% c(300, 213))) %>%
  group_by(goal, dimension) %>%
  summarize(score_myCalc = weighted.mean(score, area_km2, na.rm=TRUE)) %>%
  mutate(score_myCalc = round(score_myCalc, 2)) %>%
  filter(dimension %in% c('likely_future_state', 'score', 'status')) %>%
  mutate(region_id = 300) %>%
  left_join(tmp) %>%
  ungroup()
data.frame(rgnGoalSummary)

# this matches the goal= Index (these are calculated for each region, just testing here)
rgnSummary <- tmp %>%
  filter(!(region_id %in% c(300, 213))) %>%
  filter(goal %in% goalsOnly) %>%
  group_by(region_id, dimension) %>%
  summarize(score_myCalc = mean(score, na.rm=TRUE)) %>%
  mutate(score_myCalc = round(score_myCalc, 2)) %>%
  filter(dimension %in% c('likely_future_state', 'score')) %>%
  mutate(goal = 'Index') %>%
  left_join(tmp) %>%
  ungroup()
data.frame(rgnSummary)

# this matches the goal="Index" and region_id=300 (need to calculate this for global scores that include eez, antarctica, high seas) 
eezGoalSummary <- data.frame(rgnSummary) %>%
  group_by(dimension) %>%
  summarize(score_myCalc = weighted.mean(score_myCalc, area_km2, na.rm=TRUE))
eezGoalSummary

filter(tmp, goal=="Index" & region_id==300)




