### Explore potential differences with updates to structure

## get all data

yearsofdata <- 2012:2016

old_data <- data.frame()
for(years in yearsofdata){
  new <- read.csv(sprintf("eez%s/scores.csv", years)) %>%
    mutate(year = years) %>%
    rename(old_score = score)
  
  old_data <- rbind(new, old_data)
  
}

new_data <- read.csv(sprintf('%s/scores.csv', scenario))

combine <- left_join(new_data, old_data) %>%
  mutate(diff = old_score - score)

summary(combine)

na_old <- combine %>%
  filter(is.na(old_score) & !is.na(score))

na_new <- combine %>%
  filter(is.na(score) & !is.na(old_score))

diff <- combine %>%
  filter(diff != 0)
table(diff$goal)


plot(combine$old_score[combine$goal=="ECO" & combine$year==2012 & combine$dimension=="score"], 
     combine$score[combine$goal=="ECO" & combine$year==2012 & combine$dimension=="score"])
abline(0,1, col="red")
plot(combine$old_score[combine$goal=="LIV" & combine$year==2012 & combine$dimension=="score"], 
     combine$score[combine$goal=="LIV" & combine$year==2012 & combine$dimension=="score"])
abline(0,1, col="red")

cp <- diff %>%
  filter(goal == "CP")
# These are all small insignificant changes (<= 0.1).  Can't remember what this was due to.  But not worth
# worrying about.

tr <- diff %>%
  filter(goal == "TR")
# Change in status because I realized that the separate years of tourism and sustainability data were not 
# compatitible with one another.  Now I use only the most recent year of sustainability for all years.  This
# is not ideal, but it doesn't appear that consistent methods were used in thier calculations.
# effects 2012 - 2014 years.  And two regions (12 adn 146 went from NA to a score due to having a sustainability score)

liv <- diff %>%
  filter(goal == "LIV")
table(liv$dimension)
table(liv$year)
table(liv$region_id)

eco <- diff %>%
  filter(goal == "ECO")
table(liv$dimension)
table(liv$year)
## Both are changes in trend (and, cascading future and score) and year 2012

# explore wages
wage2012 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2012_trend_wage_sector-year.csv") %>%
  rename(value2012 = value)
wage2013 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2013_trend_wage_sector-year.csv") %>%
  rename(value2013 = value)

wages <- full_join(wage2012, wage2013) %>%
  mutate(diff = value2012 - value2013)

filter(wages, is.na(value2013))
filter(wages, is.na(value2012))
filter(wages, diff != 0)

# appears the NA values are different between the two datasets....
wage2012 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2012_trend_wage_sector-year.csv") %>%
  mutate(analysis_year = 2012)
wage2013 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2013_trend_wage_sector-year.csv") %>%
  mutate(analysis_year = 2013)
wages <- rbind(wage2012, wage2013)
write.csv(wages, "../ohiprep/globalprep/le/v2013/data/le_eez_updated_trend_wage_sector-year.csv", row.names=FALSE)

# explore jobs
jobs2012 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2012_trend_jobs_sector-year.csv") %>%
  rename(value2012 = value)
jobs2013 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2013_trend_jobs_sector-year.csv") %>%
  rename(value2013 = value)

jobs <- full_join(jobs2012, jobs2013) %>%
  mutate(diff = value2012 - value2013)
filter(jobs, diff != 0)
plot(jobs$value2012, jobs$value2013)
abline(0,1, col="red")
## seems the numbers change for the datasets, these should be the same
jobs2012 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2012_trend_jobs_sector-year.csv") %>%
  mutate(analysis_year = 2012)
jobs2013 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2013_trend_jobs_sector-year.csv") %>%
  mutate(analysis_year = 2013)
jobs <- rbind(jobs2012, jobs2013)

write.csv(jobs, "../ohiprep/globalprep/le/v2013/data/le_eez_updated_trend_jobs_sector-year.csv", row.names=FALSE)


# explore revenue
rev2012 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2012_trend_rev_sector-year.csv") %>%
  rename(value2012 = value)
rev2013 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2013_trend_rev_sector-year.csv") %>%
  rename(value2013 = value)

rev <- full_join(rev2012, rev2013) %>%
  mutate(diff = value2012 - value2013)
filter(rev, diff != 0)
summary(rev)
plot(rev$value2012, rev$value2013)
abline(0,1, col="red")

tmp <- filter(rev, is.na(value2012))
filter(rev, is.na(value2013))
## seems the numbers change for the datasets, these should be the same
# appears the NA values are different between the two datasets....
rev_full <- rev %>%
  mutate(value = ifelse(is.na(value2013), value2012, value2013))

rev2012 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2012_trend_rev_sector-year.csv") %>%
  mutate(analysis_year = 2012)
rev2013 <- read.csv("../ohiprep/globalprep/le/v2013/data/le_eez2013_trend_rev_sector-year.csv") %>%
  mutate(analysis_year = 2013)
rev <- rbind(rev2012, rev2013)
write.csv(rev, "../ohiprep/globalprep/le/v2013/data/le_eez_updated_trend_rev_sector-year.csv", row.names=FALSE)

