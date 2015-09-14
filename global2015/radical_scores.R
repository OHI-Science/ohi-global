if(radical1==TRUE){

###########################################
## Radical data preparation
###########################################
#library(dplyr)

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
                                  scenario = 2012:2015,
                            goal = goalList, 
                            dimension = c('future', 'score', 'status', 'pressures', 'resilience', 'trend')), 
                      expand.grid(region_id = 213, 
                                  scenario = 2012:2015,
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
hs <- read.csv('highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
  filter(region_id != 0)   

# use this to fill in missing values:
allGoals_hs <- rbind(expand.grid(region_id = unique(hs$region_id), 
                                 scenario = 2012:2015,
                        goal = goalList, 
                        dimension = unique(hs$dimension)),
                     expand.grid(region_id = unique(hs$region_id),
                                 scenario = 2012:2015,
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
area <- read.csv('eez2015/layers/rgn_area.csv') %>%
  select(region_id=rgn_id, area_km2)

radical <- data.frame()

for(scenarioYear in 2012:2015){ #scenarioYear=2012

data <- read.csv(sprintf('eez%s/scores.csv', scenarioYear), stringsAsFactors=FALSE)

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

write.csv(radical, sprintf('%s/radical_%s.csv', saveFile, Sys.Date()), row.names=FALSE, na="")

# ##################################
# ## checking calculations
# ##################################
# goalsOnly <- c('AO', 'BD', 'CP', 'CS', 'CW', 'FP', 'LE', 'NP', 'SP', 'TR')
# 
# 
# # this matches the region_id = 300 goals exactly (already calculated, these are the rgn_id==0 in the eez data)
# rgnGoalSummary <- radical %>%
#   filter(!(region_id %in% c(300, 213))) %>%
#   group_by(goal, dimension) %>%
#   summarize(score_myCalc = weighted.mean(score, area_km2, na.rm=TRUE)) %>%
#   mutate(score_myCalc = round(score_myCalc, 2)) %>%
#   filter(dimension %in% c('likely_future_state', 'score', 'status')) %>%
#   mutate(region_id = 300) %>%
#   left_join(tmp) %>%
#   ungroup()
# data.frame(rgnGoalSummary)
# 
# # this matches the goal= Index (these are calculated for each region, just testing here)
# rgnSummary <- radical %>%
#   filter(!(region_id %in% c(300, 213))) %>%
#   filter(goal %in% goalsOnly) %>%
#   group_by(region_id, dimension) %>%
#   summarize(score_myCalc = mean(score, na.rm=TRUE)) %>%
#   mutate(score_myCalc = round(score_myCalc, 2)) %>%
#   filter(dimension %in% c('likely_future_state', 'score')) %>%
#   mutate(goal = 'Index') %>%
#   left_join(tmp) %>%
#   ungroup()
# data.frame(rgnSummary)
# 
# # this matches the goal="Index" and region_id=300 (need to calculate this for global scores that include eez, antarctica, high seas) 
# eezGoalSummary <- data.frame(rgnSummary) %>%
#   group_by(dimension) %>%
#   summarize(score_myCalc = weighted.mean(score_myCalc, area_km2, na.rm=TRUE))
# eezGoalSummary
# 
# filter(tmp, goal=="Index" & region_id==300)

}


<<<<<<< HEAD

=======
>>>>>>> 4e4432ca98121580be656bf5534c243cf22ed687
###########################################################
## Data for Colin @Croscon, Sept 2015
###########################################################

# # Only report scores, no other dimensions.
# # Format scores like this: 
#   
# (Sub)Goal	Region_id	Region_label	Score
# Index	0	US West Coast	96.1
# Index	1	Northern California	96.6
# Index	2	Central California	96.27
# Index	3	Southern California	98.11
# Index	4	Oregon	67.36
# Index	5	Washington	47.2
# FIS	0	US West Coast	96.1
# FIS	1	Northern California	96.6

library(dplyr)
library(tidyr)
library(stringr)
dir_croscon = '~/github/ohi-global/global2015/croscon_scores'

goal_order = c('Index', 'FIS', 'FP', 'MAR', 'AO', 'CS', 'CP', 'TR', 'LIV', 'LE', 'ECO', 'ICO', 'SP', 'LSP', 'CW', 'HAB', 'BD', 'SPP', 'NP')

#######
# FIJI
#######


# get fiji rgn_id
fiji_info = read.csv('https://raw.githubusercontent.com/OHI-Science/ohi-fiji/master/fiji2013/layers/rgn_labels.csv') %>%
  filter(label == 'Fiji')

# read in fiji scores.csv and filter
fiji <- read.csv('https://raw.githubusercontent.com/OHI-Science/ohi-fiji/master/fiji2013/scores.csv') %>%
  filter(region_id == fiji_info$rgn_id, 
         dimension == 'score') %>%
  mutate(rgn_id    = 0, 
         rgn_label = 'Fiji')%>%
  select('(Sub)Goal'  = goal, 
         Region_id    = rgn_id,
         Region_label = rgn_label, 
         Score        = score)
fiji

# write.csv(fiji, file.path(dir_croscon, 'OHI Fiji Scores.csv'), row.names =F)
# have to arrange by goal_order by hand

#######
# BRAZIL
#######

br <- read.csv(file.path(dir_save, 'Elfes_Table3.csv'), strip.white=TRUE) %>%
  mutate(Region_id = 0:17) %>%
  gather(goal, score, -Region, -Region_id) %>%
  mutate(Region = str_replace_all(Region, ' \\([A-Z][A-Z]\\)', '')) %>%
  select('(Sub)Goal'  = goal, 
         Region_id,
         Region_label = Region, 
         Score        = score)
head(br, 20)    
    
write.csv(br, file.path(dir_croscon, 'OHI Brazil Scores.csv'), row.names =F)


#######
# US WEST COAST
#######

# read in from github
us = read.csv('https://raw.githubusercontent.com/OHI-Science/ohi-uswest/master/scores/scores_index.csv')

# clean up labeling
us$region_id[us$region_id == 8] = 0
us$region_label = as.character(us$region_label)
us$region_label[us$region_id == 0] = 'US West Coast'

us = us %>%
  arrange(region_id) %>%
  select(-region_code, -km2, -preservationist, -extractive, -nonextractive, -extreme, -cc_workshop) %>%
  filter(region_label != 'California Current Ocean') %>%
  dplyr::rename(Index = unweighted) %>%
  gather(goal, score, -region_id, -region_label) %>%
  mutate(score = round(score, 2))
head(us, 20)

us = us %>% 
 select('(Sub)Goal'   = goal, 
         Region_id    = region_id,
         Region_label = region_label, 
         Score        = score)
head(us, 20)    

write.csv(us, file.path(dir_croscon, 'OHI US West Coast Scores.csv'), row.names =F)

