### Stacked bar plot of goal trends for each region

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

setwd("global2016/Reporting")

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


data <- read.csv("data/trends_2016.csv") %>%
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
  labs(y="Trend", x="") + 
  guides(fill=guide_legend(reverse=TRUE)) +
  theme(axis.text.y=element_text(size=10),
        legend.justification=c(1,0), legend.position=c(.3,.875)) +
  geom_point(data=index, aes(y=trend, x=country), fill="black", shape="|") +
  geom_hline(yintercept=0, color="black")
p
ggsave('Trend_barplot.pdf', height=25, width=10)
ggsave('Trend_barplot.png', height=25, width=10)

max <- length(index$country)
countries_mid <- c("Johnston Atoll", "Jarvis Island", "Ile Europa", "Libya", "Russia",
                   "Cook Islands", "Norfolk Island", "Lithuania", "Egypt", "Sao Tome and Principe")

regions_subset <- rbind(index[1:15, ], index[index$country %in% countries_mid, ], index[(max-14):max, ])
regions_subset <- as.character(regions_subset$country)  

data_subset <- data %>%
  mutate(country = as.character(country)) %>%
  filter(country %in% regions_subset) %>%
  mutate(country = factor(country, levels=index$country))
  
index_subset <- filter(data_subset, goal=="Index")  

p <- ggplot(data=filter(data_subset, goal != "Index"), aes(x=country, y=trend, fill=goal)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=myPalette) +
  coord_flip() +
  theme_bw() +
  labs(y="Trend", x="") + 
  guides(fill=guide_legend(reverse=TRUE)) +
  theme(axis.text.y=element_text(size=10),
        legend.justification=c(1,0), legend.position=c(.3,.60)) +
  geom_point(data=index_subset, aes(y=trend, x=country), fill="black", shape="|", size=4) +
  geom_hline(yintercept=0, color="black")
p
ggsave('Trend_barplot_subset.pdf', height=8, width=10)
ggsave('Trend_barplot_subset.png', height=8, width=10)

