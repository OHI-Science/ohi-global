#### see what is happening with tourism...and whether we can use all years of 
#### travel warnings

library(here)
library(dplyr)
library(ggplot2)
library(broom)

tour <- read.csv(here("eez/layers/tr_jobs_pct_tourism.csv"))

tour_summary <- tour %>%
  filter(year>=2013 & year <=2017) %>%
  group_by(year) %>%
  summarize(total_tourists = sum(Ep, na.rm=TRUE))

mod <- lm(Ep ~ year + as.factor(rgn_id), data=filter(tour, year>=2013 & year<=2017))
summary(mod)

mod <- lm(Ep ~ year, data=filter(tour, year>=2013 & year<=2017))
summary(mod)


tourism_mod = tour %>% 
  filter(!is.na(Ep)) %>%
  filter(year >= 2013 & year <= 2017) %>%
  group_by(rgn_id) %>%
  do(fit_tourism = lm(Ep ~ year, data = .))

# get the coefficients by group in a tidy data_frame
tourismCoef = tidy(tourism_mod, fit_tourism) %>%
  filter(term=="year")
# other stuff
augment(tourism_mod, fit_tourism)

hist(tourismCoef$estimate)


# check tr scores, declining trend
# see if this generally matches the trend data we see for scores

scores <- read.csv(here("global2018/Results/data/trends_2018.csv")) %>%
  select(rgn_id = region_id, TR)

tourismCoef <- left_join(tourismCoef, scores, by="rgn_id")
plot(tourismCoef$estimate, tourismCoef$TR)
abline(v=0, col="red")
abline(h=0, col="red")
identify(tourismCoef$estimate, tourismCoef$TR, label=tourismCoef$rgn_id)
tourismCoef[c(120), ]
