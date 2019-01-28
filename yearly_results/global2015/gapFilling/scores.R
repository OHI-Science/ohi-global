### check and finalize data

library(dplyr)
library(tidyr)

scores <- read.csv('scores.csv')

# -----------------------------------------------------------
## Do a few checks to make sure everything is actually there
## and basically looks correct
# -----------------------------------------------------------


# should be 14 goals
if(length(table(scores$goal)) != 14){
  stop('There are not 14 goals/subgoals')
}

# should be 4 dimensions
if(length(table(scores$dimension)) != 4){
  stop('missing a dimension')
}

# every goal should have data for each dimension
if(sum(is.na(table(scores$goal, scores$dimension))) > 0 ){
  stop('One or more goals is missing the status/trend dimension')
}

# Make sure the number of regions calculated is about right...this is a loose one
if(sum(table(scores$goal, scores$dimension) < 100 | table(scores$goal, scores$dimension) > 250) > 0) {
  stop('One or more of the goals appears to have too many or too few values')
}

## make sure scores are calculated on the same scale
if(min(scores$score, na.rm=TRUE) < 0 | max(scores$score, na.rm=TRUE) > 1) {
  stop('Scores are outside 0 to 1 range...check this')
}

# -----------------------------------------------------------
## Fill in missing data with NAs, filter out irrelevant regions and goals
# -----------------------------------------------------------
# directly calculated goals
goals <- data.frame(goal = c("FIS", "MAR", "AO",  "TR",  "ICO", "LSP", "CW", "SPP", "HAB", "NP", "CP", "CS", "LIV", "ECO"),
                    supragoals =c("FP", "FP", "AO", "TR", "SP", "SP", "CW", "BD", "BD", "NP", "CP", "CS", "LE", "LE"))

dimension <- c("status", "trend", "pressure", "resilience")

## get rid of data when there is no status
scores_w <- spread(scores, dimension, score) %>%
  filter(!is.na(status))
scores_g <- gather(scores_w, "dimension", "score", pressure:trend)

## fill in missing data to have complete records for each region
regions <- read.csv('../../eez2015/layers/rgn_labels.csv') %>%
  filter(type == "eez") %>%
  filter(rgn_id <= 250) %>%
  filter(rgn_id != 213) %>%
  select(region_id = rgn_id)

blank <- expand.grid(goal=goals$goal, dimension=dimension, region_id=regions$region_id)

scores_complete <- blank %>%
  left_join(scores_g, by=c('goal', 'dimension', 'region_id')) %>%
  arrange(goal, dimension, region_id) %>%
  filter(!(goal %in% c("ECO", "LIV", "LE")))

# -----------------------------------------------------------
## Calculate Future and Scores 
# -----------------------------------------------------------

future_weights <- data.frame(dimension = c("status", "trend", "resilience", "pressure"),
                             fut_weight = c(0.5, 0.5*2/3, 0.5*1/3*1/2, 0.5*1/3*1/2))

future <- scores_complete %>%
  left_join(future_weights, by="dimension") %>%
  group_by(region_id, goal) %>%
  summarize(score = weighted.mean(score, fut_weight, na.rm=TRUE)) %>%
  mutate(dimension = "likely_future_state") %>%
  select(goal, dimension, region_id, score) %>%
  ungroup()

score <- rbind(filter(scores_complete, dimension=="status"), future) %>%
  group_by(region_id, goal) %>%
  summarize(score = mean(score, na.rm=TRUE)) %>%
  mutate(dimension = "score") %>%
  select(goal, dimension, region_id, score) %>%
  ungroup()

  
scores_all_dims <- rbind(scores_complete, future, score)



# -----------------------------------------------------------
## Calculate Supragoals 
# -----------------------------------------------------------

# get fishery weights
fish_weights <- read.csv('../../eez2015/layers/fp_wildcaught_weight.csv') %>%
  mutate(MAR = 1-w_fis) %>%
  select(region_id = rgn_id, FIS = w_fis, MAR) %>%
  gather('goal', 'weight', 2:3)


scores_supr <- scores_all_dims %>%
  left_join(goals, by="goal") %>%
  left_join(fish_weights, by=c('region_id', 'goal')) %>%
  mutate(weight = ifelse(is.na(weight), 1, weight)) %>%
  group_by(supragoals, region_id, dimension) %>%
  summarize(score = weighted.mean(score, weight)) %>%
  mutate(score = ifelse(dimension %in% c("pressure", "resilience") & supragoals %in% c("BD", "LE", "SP", "FP"), NA, score)) %>%
  select(goal=supragoals, dimension, region_id, score) %>%
  ungroup()

subgoals <- c("FIS", "MAR", "ICO", "LSP", "SPP", "HAB", "LIV", "ECO")
scores_sub <- filter(scores_all_dims, goal %in% subgoals)
  
scores_all_goals <- rbind(scores_supr, scores_sub)

# -----------------------------------------------------------
## Index scores for each country 
# -----------------------------------------------------------

conf <- read.csv('../../eez2015/conf/goals.csv')
supragoals <-  conf %>%
  filter(parent=="")
supragoals <- supragoals$goal

# all goals weighted equally, so just an average:
countryScores <- scores_all_goals %>%
  filter(goal %in% supragoals) %>%
  group_by(region_id, dimension) %>%
  summarize(score = mean(score, na.rm = TRUE)) %>%
  mutate(goal = "Index") %>%
  select(goal, dimension, region_id, score) %>%
  ungroup()

final_scores <- rbind(scores_all_goals, countryScores)  

#######################################
## Get average goal scores (weighted by country average)
area <- read.csv('../../eez2015/layers/rgn_area.csv') %>%
  select(region_id = rgn_id, area = area_km2)

goalScores <- final_scores %>%
  left_join(area, by=c('region_id')) %>%
  group_by(goal, dimension) %>%
  summarize(score = weighted.mean(score, area, na.rm=TRUE)) %>%
  mutate(region_id = 0) %>%
  select(goal, dimension, region_id, score) %>%
  ungroup()

final_scores <- rbind(final_scores, goalScores)  %>%
  mutate(score = round(score*100, 2)) %>%
  data.frame()

write.csv(final_scores, "scores.csv", row.names=FALSE)

