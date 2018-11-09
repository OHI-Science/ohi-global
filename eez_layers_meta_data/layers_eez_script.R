#####################################
## This code takes some of the 
## information in this file and 
## creates a layers_xx.csv file in the 
## format used by the toolbox
#####################################

#library(dplyr)
#library(tidyr)

targets <- read.csv('../eez_layers_meta_data/layers_eez_targets.csv', stringsAsFactors=FALSE) %>%
  dplyr::mutate(dimension = ifelse(dimension %in% c("status", "trend"), NA, dimension)) %>%
  dplyr::filter(!is.na(dimension) | !is.na(goal)) %>%
  dplyr::mutate(goal = ifelse(is.na(goal), dimension, goal)) %>%
  dplyr::mutate(target = paste(goal, dimension, sep=' ')) %>%
  dplyr::mutate(target = gsub(" NA", "", target)) %>%
  unique() %>%
  dplyr::group_by(layer) %>%
  dplyr::summarize(targets = paste(target, collapse= "")) %>%
  data.frame()


# add the meta data
meta <- read.csv("../eez_layers_meta_data/layers_eez_base.csv")
layers <- meta %>% 
  dplyr::left_join(targets, by="layer") %>%
  dplyr::select(layer, dir, fn, ingest, name_data_fld, targets, name, units, description)


write.csv(layers, "layers.csv", row.names=FALSE)
