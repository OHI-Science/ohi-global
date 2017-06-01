#####################################
## This code takes some of the 
## information in this file and 
## creates a layers_xx.csv file in the 
## format used by the toolbox
#####################################

library(dplyr)
library(tidyr)

targets <- read.csv('eez_layers_meta_data/layers_eez_targets.csv', stringsAsFactors=FALSE) %>%
  mutate(dimension = ifelse(dimension %in% c("status", "trend"), NA, dimension)) %>%
  filter(!is.na(dimension) | !is.na(goal)) %>%
  mutate(goal = ifelse(is.na(goal), dimension, goal)) %>%
  mutate(target = paste(goal, dimension, sep=' ')) %>%
  mutate(target = gsub(" NA", "", target)) %>%
  unique() %>%
  group_by(layer) %>%
  summarize(targets = paste(target, collapse= "")) %>%
  data.frame()

tb <- read.csv('eez_layers_meta_data/layers_eez_file_locations.csv')

layers <- tb %>%
  spread(variable, variable_data) %>%
  left_join(targets, by="layer")

# add the meta data
meta <- read.csv("eez_layers_meta_data/layers_eez_base.csv")

layers <- layers %>%
  left_join(meta, by="layer")

write.csv(layers, "layers_eez.csv", row.names=FALSE)
