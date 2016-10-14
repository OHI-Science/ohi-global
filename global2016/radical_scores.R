if(radical1==TRUE){

###########################################
## Radical data preparation
###########################################
# library(dplyr)
# saveFile = "global2016"
# list of goals:
goalList <- c('AO', 'BD', 'CP', 'CS', 'CW', 'ECO', 'FIS', 'FP', 'HAB', 'ICO', 'LE', 'LIV', 'LSP', 'MAR', 'NP', 'SP', 'SPP', 'TR')


## currently pair 2015 Antarctica and 2014 HS data with 2014 and 2015 and 2016 EEZ analysis

###########################################################
## Antarctica data: only gets reported as one summary value:
###########################################################

antarctica <- read.csv('../../antarctica2015/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id == 0) %>%
  mutate(region_id = 213) 

# use this to fill in missing values:
allGoals_ant <- rbind(expand.grid(region_id = 213, 
                                  scenario = 2012:2016,
                            goal = goalList, 
                            dimension = c('future', 'score', 'status', 'pressures', 'resilience', 'trend')), 
                      expand.grid(region_id = 213, 
                                  scenario = 2012:2016,
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
hs <- read.csv('../../highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id != 0)   

# use this to fill in missing values:
allGoals_hs <- rbind(expand.grid(region_id = unique(hs$region_id), 
                                 scenario = 2012:2016,
                        goal = goalList, 
                        dimension = unique(hs$dimension)),
                     expand.grid(region_id = unique(hs$region_id),
                                 scenario = 2012:2016,
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
area <- read.csv(sprintf('../../eez%s/layers/rgn_area.csv', scenario)) %>%
  select(region_id=rgn_id, area_km2)

radical <- data.frame()

for(scenarioYear in 2012:2016){ #scenarioYear=2012

data <- read.csv(sprintf('../../eez%s/scores.csv', scenarioYear), stringsAsFactors=FALSE)

eez <- data %>%
  mutate(region_id = ifelse(region_id==0, 300, region_id)) %>%
  filter(region_id != 213) %>%  # add in real Antarctica data later
  mutate(scenario = scenarioYear)
  
  
hs_Syear <- hs_all[hs_all$scenario == scenarioYear, ]
ant_Syear <- antarctica_all[antarctica_all$scenario == scenarioYear, ]

all <- eez %>%
  bind_rows(hs_Syear) %>%
  bind_rows(ant_Syear) %>%
  left_join(area)

## Calculate global indices when region_id==0
goal_0 <- all %>%
  filter(region_id !=300) %>%
  filter(dimension %in% c('future', 'score', 'status')) %>%
  group_by(goal, dimension) %>%
  summarize(score = weighted.mean(score, area_km2, na.rm=TRUE)) %>%
  mutate(region_id = 0)

goal_0$scenario = scenarioYear

if(scenarioYear %in% 2012:2013){
  goal_0 <- goal_0 %>%
    mutate(score = NA)
}

all <- all %>%
  bind_rows(goal_0) %>%
  mutate(dimension = ifelse(dimension == 'future', 'likely_future_state', dimension)) %>%
  mutate(score = round(score, 2)) %>%
  select(scenario, goal, dimension, region_id, value=score)



radical <- bind_rows(radical, all)
}  


radical <- radical %>%
  arrange(scenario, goal, dimension, region_id)

write.csv(radical, sprintf('../radical_%s.csv', Sys.Date()), row.names=FALSE, na="")

}



