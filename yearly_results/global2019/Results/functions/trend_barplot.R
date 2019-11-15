### Stacked bar plot of goal trends for each region

# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(RColorBrewer)
# library(here)
# 
# 
# goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')
# goal_names <- data.frame(goal=goals, long_goal=c("Index", 
#                                                  "Artisanal opportunities",
#                                                  "Species condition (subgoal)",
#                                                  "Biodiversity",
#                                                  "Habitat (subgoal)",
#                                                  "Coastal protection",
#                                                  "Carbon storage",
#                                                  "Clean water",
#                                                  "Economies",
#                                                  "Livelihoods & economies",
#                                                  "Livelihoods",
#                                                  "Fisheries (subgoal)",
#                                                  "Food provisioning",
#                                                  "Mariculture (subgoal)",
#                                                  "Iconic species (subgoal)",
#                                                  "Sense of place",
#                                                  "Lasting special places (subgoal)",
#                                                  "Natural products",
#                                                  "Tourism & recreation"))


data <- read.csv(here(sprintf("yearly_results/%s/Results/data/trends_2019.csv", saveFile))) %>%
  filter(region_id != 0) %>%
  select(-region_id, -SPP, -HAB, -ECO, -LIV, -FIS, -MAR, -ICO, -LSP) %>%
  gather("goal", "trend", -country) %>%
  left_join(goal_names, by='goal') %>%
  select(country, goal=long_goal, trend)

index <- filter(data, goal=="Index") %>%
  mutate(country = as.character(country)) %>%
  arrange(trend)

data <- data %>%
  mutate(country=as.character(country)) %>%
  mutate(country = factor(country, levels=index$country))

myPalette <- brewer.pal(10, "Spectral")

p <- ggplot(data=filter(data, goal != "Index"), aes(x=country, y=trend, fill=goal)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=myPalette) +
  coord_flip() +
  theme_bw() +
  labs(y="Average change in score per year", x="") + 
  guides(fill=guide_legend(reverse=TRUE)) +
  theme(axis.text.y=element_text(size=10),
        legend.justification=c(1,0), legend.position=c(.3,.875)) +
  geom_point(data=index, aes(y=trend, x=country), fill="black", shape="|") +
  geom_hline(yintercept=0, color="black")

#p

#ggsave('Trend_barplot.pdf', height=25, width=10)
ggsave(here(sprintf('yearly_results/%s/Results/figures/Trend_barplot.png', saveFile)), height=25, width=10)

