################################################
## If a data layer is added to the conf pressures/resilience
## matrices, then this script will take that information
## to update the layers_eez_targets.csv file
####################################################

targets <- read.csv("eez_layers_meta_data/layers_eez_targets.csv")

p_matrix <- read.csv(sprintf("%s/conf/pressures_matrix.csv", s_folder)) %>%
  gather("layer", "weight", -(1:3)) %>%
  group_by(layer, goal) %>%
  summarize(total_weight = sum(weight, na.rm=TRUE)) %>%
  filter(!is.na(total_weight)) %>%
  filter(total_weight > 0) %>%
  mutate(dimension = "pressure") %>%
  select(layer, goal, dimension) %>%
  data.frame()

r_matrix <- read.csv(sprintf("%s/conf/resilience_matrix.csv", s_folder), na.strings=c(""," ","NA")) %>%
  gather("layer", "weight", -(1:2)) %>%
  mutate(weight = ifelse(is.na(weight), NA, 1)) %>%
  group_by(layer, goal) %>%
  summarize(total_weight = sum(weight, na.rm=TRUE)) %>%
  filter(!is.na(total_weight)) %>%
  filter(total_weight > 0) %>%
  mutate(dimension = "resilience") %>%
  select(layer, goal, dimension) %>%
  data.frame()


## checking that everything is copacetic
targets_pressure <- filter(targets, dimension == "pressure")
targets_resilience <- filter(targets, dimension == "resilience")

tmp <- setdiff(targets_pressure$layer, p_matrix$layer)
if(length(tmp) > 0){
  warning(paste0("These pressure layers are included in targets, but not the pressure matrix:",
                 paste(as.character(tmp), collapse = ", ")))}


tmp <- setdiff(targets_resilience$layer, r_matrix$layer)
if(length(tmp) > 0){
  warning(paste0("These resilience layers were included in layers_eez_targets.csv, but not the resilience matrix:",
                 paste(as.character(tmp), collapse = ", ")))}

#### finish up
targets <- targets %>%
  filter(!(dimension %in% c("pressure", "resilience"))) %>%
  rbind(p_matrix) %>%
  rbind(r_matrix)

write.csv(targets, "eez_layers_meta_data/layers_eez_targets.csv", row.names=FALSE)

