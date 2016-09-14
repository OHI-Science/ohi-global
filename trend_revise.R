trend <- status_data %>%
  filter(year %in% trend_years) %>%
  group_by(rgn_id) %>%
  do(mdl = lm(status ~ year, data=.),
     adjust_trend = .$status[.$year == first_trend_year]) %>%
  summarize(region_id = rgn_id,
            score = round(coef(mdl)['year']/adjust_trend * 5, 2),
            dimension = 'trend') %>%
  ungroup() %>%
  mutate(score = ifelse(score > 1, 1, score)) %>%
  mutate(score = ifelse(score < -1, -1, score))

old_trend <- read.csv('scores.csv') %>%
  filter(goal=="FIS") %>%
  filter(dimension=="trend") %>%
  select(region_id, old_score=score) %>%
  left_join(trend, by="region_id")
plot(old_trend$old_score, old_trend$score, xlab="old trends", ylab="new trends")
abline(0,1, col="red")
filter(status_data, rgn_id==33)