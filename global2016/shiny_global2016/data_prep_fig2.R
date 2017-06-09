library(tidyverse) 

### prepare data for fig 2:
ptm <- proc.time()
nlme_data <- read_csv('data/radical_2016-11-17.csv') %>%
  filter(dimension == "score") %>%   # focus only on score data
  filter(region_id != 0) %>%         # this weighted mean includes high seas and Antarctica
  filter(region_id <= 250) %>%       # get rid of high seas regions
  filter(region_id != 213) %>%
  filter(!is.na(value))

nlme_data_lm <- nlme_data %>%
  group_by(goal, region_id) %>%
  do(mdl = lm(value ~ scenario, data = .))

nlme_results_lm <- broom::tidy(nmle_data_lm, mdl) %>%
  ungroup() %>%
  filter(term == "scenario") %>%
  arrange(goal, estimate) %>%
  left_join(goal_names) %>%
  left_join(index_2016 %>% select(region_id, country), by = 'region_id') %>%
  select(goal, 
         goal_long, 
         country, region_id, 
         average_change_per_year = estimate, 
         p.value) %>%
  data.frame()


nlme_results <- data.frame(goal = goals, trend = NA, p_value = NA)

for(goal in goals){ # goal = "AO"
  mod_data <- nmle_data[nmle_data$goal == goal, ]
  mdl = nlme::lme(value ~ 1 + scenario, random = ~1 | region_id, data = mod_data, method="REML")
  nlme_results$trend[nlme_results$goal == goal] <- mdl$coefficients$fixed[2]
  nlme_results$p_value[nlme_results$goal==goal] <- summary(mdl)$tTable[2,"p-value"]
}

results_lm_summary <- nlme_results_lm %>%
  group_by(goal, goal_long) %>%
  summarize(quant_5  = quantile(average_change_per_year, c(0.05)),
            quant_95 = quantile(average_change_per_year, c(0.95)))


nlme_results <- nlme_results %>%
  left_join(results_lm_summary, by="goal") %>%
  arrange(trend) %>%
  mutate(sig = ifelse(p_value < 0.05, "1", "0"))

### nlme model with eez area weights
eez_area <- read_csv("data/rgn_area.csv")

data_wts <- nlme_data %>%
  left_join(eez_area, by="region_id")

nlme_results_wts <- data.frame(goal = goals, trend_wts = NA, p_value_wts = NA)

for(goal in goals){ # goal = "AO"
  mod_data <- data_wts[data_wts$goal == goal, ]
  mdl = nlme::lme(value ~ 1 + scenario, random = ~1 | region_id, data = mod_data, method="REML", weights=~1/area_km2)
  nlme_results_wts$trend_wts[nlme_results_wts$goal == goal] <- mdl$coefficients$fixed[2]
  nlme_results_wts$p_value_wts[nlme_results_wts$goal==goal] <- summary(mdl)$tTable[2,"p-value"]
}

nlme_results <- nlme_results %>%
  left_join(nlme_results_wts, by="goal") %>%
  arrange(trend) %>%
  mutate(sig_wts = ifelse(p_value_wts < 0.05, "1", "0"))



nlme_results_noLE <- nlme_results %>%
  filter(!(goal %in% c("ECO", "LIV", "LE")))

nlme_data_noLE <- nlme_data_lm %>%
  filter(!goal %in% c('ECO', 'LIV', 'LE')) %>%
  mutate(trend = mdl[['coefficients']][2]) %>%
  select(-mdl) %>%
  left_join(goal_names, by = 'goal')

proc.time() - ptm

write_csv(nlme_results_noLE, 'data/nlme_results_noLE.csv')
write_csv(nlme_data_noLE, 'data/nlme_data_noLE.csv')

