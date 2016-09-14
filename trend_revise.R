trend_years <- (status_year-4):(status_year)
first_trend_year <- min(trend_years)

trend = ry %>%
  group_by(rgn_id) %>%
  do(mdl = lm(status ~ year, data=., subset=year %in% trend_years),
     adjust_trend = .$status[.$year == first_trend_year]) %>%
  summarize(rgn_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
  ungroup() %>%
  mutate(trend = ifelse(trend>1, 1, trend)) %>%
  mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
  mutate(trend = round(trend, 2)) %>%
  select(region_id = rgn_id, score = trend) %>%
  mutate(dimension = "trend")

old_trend <- read.csv('scores.csv') %>%
  filter(goal=="MAR") %>%
  filter(dimension=="trend") %>%
  select(region_id, old_score=score) %>%
  left_join(trend, by="region_id")
plot(old_trend$old_score, old_trend$score, xlab="old trends", ylab="new trends")
abline(0,1, col="red")
write.csv(old_trend, "../changePlot_figures/MAR_trend_compare_eez2016.csv", row.names=FALSE)

filter(ry, rgn_id==6 & year %in% trend_years)
