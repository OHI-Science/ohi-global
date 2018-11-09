## Separating out LIV and ECO results
## These subgoals have not been updated for several years due to a lack
## of data.  
## When data becomes available, we will make large changes to the functions used
## to calculate ECO and LIV.
## Given this, we have removed the goal calculations from functions.R, and now 
## just source the final status and trend. We made this decision because there were 
## a lot of outdated functions in ohicore that we couldn't delete because they were 
## still used in this function.

library(dplyr)
library(tidyr)

s <- read.csv("scores.csv")

## check how status/trend changes over years
## note: as expected, these values have not changed since 2013 assessment

check <- s %>%
  filter(goal %in% c("ECO", "LIV") & dimension=="status") %>%
  spread(year, score)
data.frame(check)
write.csv(check, "eez/archive/LIV_ECO_status_2018.csv", row.names=FALSE)

check_trend <- s %>%
  filter(goal %in% c("ECO", "LIV") & dimension=="trend") %>%
  spread(year, score)
data.frame(check_trend)
write.csv(check_trend, "eez/archive/LIV_ECO_trend_2018.csv", row.names=FALSE)


## saving status and trend data 
eco_stat <- s %>%
  filter(goal == "ECO" & dimension=="status") %>%
  filter(year %in% c(2012, 2013)) %>%
  select(rgn_id = region_id, year, status=score) %>%
  filter(rgn_id != 0)

summary(eco_stat)
write.csv(eco_stat, "../ohiprep_v2018/globalprep/le/v2018/output/eco_status.csv", row.names=FALSE)

eco_trend <- s %>%
  filter(goal == "ECO" & dimension=="trend") %>%
  filter(year %in% c(2012, 2013)) %>%
  select(rgn_id = region_id, year, trend=score) %>%
  filter(rgn_id != 0)
summary(eco_trend)
write.csv(eco_trend, "../ohiprep_v2018/globalprep/le/v2018/output/eco_trend.csv", row.names=FALSE)


liv_status <- s %>%
  filter(goal == "LIV" & dimension=="status") %>%
  filter(year %in% c(2012, 2013)) %>%
  select(rgn_id = region_id, year, status=score) %>%
  filter(rgn_id != 0)
summary(liv_status)
write.csv(liv_status, "../ohiprep_v2018/globalprep/le/v2018/output/liv_status.csv", row.names=FALSE)


liv_trend <- s %>%
  filter(goal == "LIV" & dimension=="trend") %>%
  filter(year %in% c(2012, 2013)) %>%
  select(rgn_id = region_id, year, trend=score) %>%
  filter(rgn_id != 0)
summary(liv_trend)
write.csv(liv_trend, "../ohiprep_v2018/globalprep/le/v2018/output/liv_trend.csv", row.names=FALSE)
