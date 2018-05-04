#####################################
## This code takes some of the 
## information in this file and 
## creates a layers_xx.csv file in the 
## format used by the toolbox
#####################################

library(dplyr)
library(tidyr)

targets <- read.csv('https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/eez_layers_meta_data/layers_eez_targets.csv', stringsAsFactors=FALSE) %>%
  mutate(dimension = ifelse(dimension %in% c("status", "trend"), NA, dimension)) %>%
  filter(!is.na(dimension) | !is.na(goal)) %>%
  mutate(goal = ifelse(is.na(goal), dimension, goal)) %>%
  mutate(target = paste(goal, dimension, sep=' ')) %>%
  mutate(target = gsub(" NA", "", target)) %>%
  unique() %>%
  group_by(layer) %>%
  summarize(targets = paste(target, collapse= "")) %>%
  data.frame()


# add the meta data
meta <- read.csv("https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/eez_layers_meta_data/layers_eez_base.csv")
layers <- left_join(meta, targets, by="layer") %>%
  select(layer, dir, fn, ingest, name_data_fld, targets, name, units, description)


write.csv(layers, "layers.csv", row.names=FALSE)
