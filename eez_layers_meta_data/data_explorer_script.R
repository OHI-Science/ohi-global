###########################################
### Prepare metadata for Data Explorer
### Out of date (needs to be modified,
### but hopefully we will not need to deal
### with this again)
###########################################

# extra goals:
extra_goals <- data.frame(layer = c("cp_hab_contributions", "cs_hab_contributions"),
                          goal = c("CP", "CS"))

# typical goals:
goals <- read.csv('eez_layers_meta_data/layers_eez_targets.csv') %>%
  filter(variable == "targets") %>%
  select(layer, goal=variable_data) %>%
  rbind(extra_goals)

# Meta data
meta <- read.csv('eez_layers_meta_data/layers_eez_base.csv') %>%
  filter(variable %in% c("description", "units"))

# Data explorer data
de <- read.csv('eez_layers_meta_data/layers_eez_dataexplorer.csv')

de <- de %>%
  rbind(meta) 

de_final <- goals %>%
  left_join(de, by="layer") %>%  
  group_by(layer, goal, variable) %>%
  summarize(variable_data = paste(variable_data, collapse="; ")) %>%
  spread(variable, variable_data) %>%
  mutate(scenario = 2016) %>%
  mutate(location = ifelse(layer %in% c("cp_hab_contributions", "cs_hab_contributions"), "temp", "layers")) %>%
  mutate(region_id = NA) %>%
  mutate(value = NA) %>%
  mutate(url = NA) %>%
  mutate(dimension = ifelse(goal == "pressures", "pressures", NA),
         dimension = ifelse(goal == "resilience", "resilience", dimension),
         dimension = ifelse(grepl("trend", layer), "trend", dimension),
         dimension = ifelse(is.na(dimension), "status", dimension),
         dimension = ifelse(goal=="spatial", NA, dimension)) %>%
  select(component_id=layer,
         radical_include,
         component_label=short_name,
         goal,
         dimension,
         scenario,
         region_id,
         value,
         units,
         url,
         source=source_citation, 
         extra=description,
         location) %>%
  mutate(units = ifelse(units %in% c("boolean", "extent*rank", "extent*storage", "IUCN risk category",
                                     "label", "percent", "proportion", "region id", "scaled", "scaled 0-1",
                                     "score", "status score", "trend", "trend score", "UN regions", "value"), 
                        NA, units)) %>%
  data.frame()

table(de_final$location)
table(de_final$units)
table(de_final$dimension)

write.csv(de_final, "eez_layers_meta_data/layers_2016.csv", row.names=FALSE)
