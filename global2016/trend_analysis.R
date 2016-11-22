
# load libraries:
library(broom)
library(tidyr)
library(dplyr)
library(ggplot2)
library(nlme)

# set up the data:
goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')
goal_names <- data.frame(goal=goals, long_goal=c("Index", 
                                                 "Artisanal opportunities",
                                                 "Species condition (subgoal)",
                                                 "Biodiversity",
                                                 "Habitat (subgoal)",
                                                 "Coastal protection",
                                                 "Carbon storage",
                                                 "Clean water",
                                                 "Economies",
                                                 "Livelihoods & economies",
                                                 "Livelihoods",
                                                 "Fisheries (subgoal)",
                                                 "Food provisioning",
                                                 "Mariculture (subgoal)",
                                                 "Iconic species (subgoal)",
                                                 "Sense of place",
                                                 "Lasting special places (subgoal)",
                                                 "Natural products",
                                                 "Tourism & recreation"))

## General settings to control
scenario <- "2016" #identify scenario of focus (this can be changed to obtain data for other years)

## General files to load
rgn_names <- read.csv(sprintf('../../eez%s/layers/rgn_global.csv', scenario)) %>%
  dplyr::select(region_id = rgn_id, country = label) %>%
  dplyr::mutate(country = as.character(country))

rgn_names$country[rgn_names$region_id == 212] <- "Gilbert Islands (Kiribati)"
rgn_names$country[rgn_names$region_id == 148] <- "Line Islands (Kiribati)"
rgn_names$country[rgn_names$region_id == 157] <- "Phoenix Islands (Kiribati)"

radicalFile = '2016-11-17' #date extension on the radical data files that are used for all tables/figures



### prepare data:
data <- read.csv(sprintf('../radical_%s.csv', radicalFile)) 

data <- data %>%
  filter(dimension == "score") %>%   # focus only on score data
  filter(region_id != 0) %>%         # this weighted mean includes high seas and Antarctica
  filter(region_id <= 250) %>%       # get rid of high seas regions
  filter(region_id != 213) %>%
  filter(!is.na(value))


data_lm <- data %>%
  group_by(goal, region_id) %>%
  do(mdl = lm(value ~ scenario, data = .))

#data.frame(glance(data_lm, mdl))
results_lm <- tidy(data_lm, mdl) %>%
  ungroup() %>%
  filter(term == "scenario") %>%
  arrange(goal, estimate) %>%
  left_join(goal_names) %>%
  left_join(rgn_names) %>%
  select(goal=long_goal, region_name = country, average_change_per_year = estimate, p.value) %>%
  data.frame()

results_lm$goal <- factor(results_lm$goal, levels = goals)


## Analysis of overall region index scores
g <- ggplot(filter(results_lm, goal == "Index"), aes(x=average_change_per_year)) +
  geom_histogram(color = "gray", fill="gray") +
  labs(y="Number of regions", x="Trend (avg. change in score per year)") + 
  geom_vline(xintercept=0, color="red") + 
  theme_bw()

plot(g)

data$region_id <- factor(data$region_id)
model0 <- gls(value ~ scenario,  data = filter(data, goal=="Index"))
model <- lme(value ~ scenario, random = ~1 | region_id, data = filter(data, goal=="Index"))
summary(model)
