library(dplyr)

run_id <- "2017-11-6"

# list of goals:
goalList <- c('AO', 'BD', 'CP', 'CS', 'CW', 'ECO', 'FIS', 'FP', 'HAB', 'ICO', 'LE', 'LIV', 'LSP', 'MAR', 'NP', 'SP', 'SPP', 'TR')


### going to assume at this point that we are not pairing with HS or ANT data
## currently pair 2015 Antarctica and 2014 HS data with 2014 and 2015 and 2016 EEZ analysis

###########################################################
## Antarctica data: only gets reported as one summary value:
###########################################################

# antarctica <- read.csv('../../antarctica2015/scores.csv', stringsAsFactors=FALSE) %>%
#   filter(region_id == 0) %>%
#   mutate(region_id = 213) 
# 
# # use this to fill in missing values:
# allGoals_ant <- rbind(expand.grid(region_id = 213, 
#                                   scenario = 2012:2016,
#                             goal = goalList, 
#                             dimension = c('future', 'score', 'status', 'pressures', 'resilience', 'trend')), 
#                       expand.grid(region_id = 213, 
#                                   scenario = 2012:2016,
#                                goal= "Index",
#                                dimension = c('future', 'score'))
# )
# 
#                                
# antarctica_all <- allGoals_ant %>%
#   left_join(antarctica) %>%
#   arrange(scenario, region_id, goal, dimension)
# 
# # replace 2012:2013 scores with NA values
# antarctica_all <- antarctica_all %>%
#   mutate(score = ifelse(scenario %in% c(2012:2013), NA, score))
# 
# ###########################################################
# ## High Seas data
# ###########################################################
# hs <- read.csv('../../highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
#   filter(region_id != 0)   
# 
# # use this to fill in missing values:
# allGoals_hs <- rbind(expand.grid(region_id = unique(hs$region_id), 
#                                  scenario = 2012:2016,
#                         goal = goalList, 
#                         dimension = unique(hs$dimension)),
#                      expand.grid(region_id = unique(hs$region_id),
#                                  scenario = 2012:2016,
#                                  goal = "Index",
#                                  dimension = c("future", "score")
#                      )
# )
# 
# hs_all <- allGoals_hs %>%
#   left_join(hs) %>%
#   arrange(scenario, region_id, goal, dimension)
# 
# # replace 2012:2013 scores with NA values
# hs_all <- hs_all %>%
#   mutate(score = ifelse(scenario %in% c(2012:2013), NA, score))


## Make all HS data NA values
hs <- read.csv('highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id != 0) %>%
  mutate(score = NA)

# use this to fill in missing values:
allGoals_hs <- rbind(expand.grid(region_id = unique(hs$region_id),
                                 scenario = 2012:2017,
                        goal = goalList,
                        dimension = unique(hs$dimension)),
                     expand.grid(region_id = unique(hs$region_id),
                                 scenario = 2012:2017,
                                 goal = "Index",
                                 dimension = c("future", "score")))

hs_all <- allGoals_hs %>%
  left_join(hs) %>%
  arrange(scenario, region_id, goal, dimension) %>%
  rename(value = score)


###########################################################
## EEZ data
###########################################################
### area data
# area <- read.csv('eez/layers/rgn_area.csv') %>%
#   select(region_id=rgn_id, area_km2)

eez <- read.csv(sprintf("global2017/OHI_final_scores_%s.csv", run_id)) %>%
  select(region_id, scenario, goal, dimension, value)

## add in 300 (same as 0): area weighted mean of scores
mean <- eez %>%
  filter(region_id == 0) %>%
  mutate(region_id = 300)

eez <- rbind(eez, mean)

# eez <- data %>%
#   mutate(region_id = ifelse(region_id==0, 300, region_id)) %>%
#   filter(region_id != 213) %>%  # add in real Antarctica data later
#   mutate(scenario = scenarioYear)
  

# all <- eez %>%
#   bind_rows(hs_Syear) %>%
#   bind_rows(ant_Syear) %>%
#   left_join(area)

## Calculate global indices when region_id==0
# goal_0 <- all %>%
#   filter(region_id !=300) %>%
#   filter(dimension %in% c('future', 'score', 'status')) %>%
#   group_by(goal, dimension) %>%
#   summarize(score = weighted.mean(score, area_km2, na.rm=TRUE)) %>%
#   mutate(region_id = 0)



all <- bind_rows(eez, hs_all) %>%
  mutate(dimension = ifelse(dimension == 'future', 'likely_future_state', dimension)) %>%
  mutate(value = round(value, 2)) %>%
  select(scenario, goal, dimension, region_id, value) %>%
  arrange(scenario, goal, dimension, region_id)

write.csv(all, sprintf('global2017/croscon_OHI_scores_%s.csv', run_id), row.names=FALSE, na="")

### 0 and 300 should be the same
summary(all[all$region_id == 0, ]) 
summary(all[all$region_id == 300, ])

### values should all be NA
summary(all[all$region_id > 250 & all$region_id != 300, ])

dim(filter(all, scenario != 2017)) # should be 130360
