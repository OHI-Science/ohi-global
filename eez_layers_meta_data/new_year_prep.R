# Preparing files for a new assessment year!

# layers_eez_base.R : remains the same!

# layers_eez_data_sources.csv remains the same!
data_s <- read.csv("../eez_layers_meta_data/layers_eez_data_sources.csv")

# gapfill: simplified
gf <- read.csv("../eez_layers_meta_data/layers_eez_gapfill.csv") %>%
  filter(variable=="file_loc_2017") %>%
  select(layer, gf_file = variable_data)

write.csv(gf, "../eez_layers_meta_data/layers_eez_gapfill.csv", row.names=FALSE)


# methods: eliminate previous years
# this table requires this long format because there can be multiple variables for each data layers
# ie. multiple datasources and dataprep files
methods <- read.csv("../eez_layers_meta_data/layers_eez_methods.csv") %>%
  filter(variable %in% c("dataprep_url_2018", "methods_update_2018", "ds_reference")) %>%
  mutate(variable = gsub("_2018", "", variable))

table(methods$variable)
write.csv(methods, "../eez_layers_meta_data/layers_eez_methods.csv", row.names=FALSE)


### Update the eez/scenario_data_years.csv to include an additional year of data
yrs <- read.csv("conf/scenario_data_years.csv")
new_yr <- yrs %>%
  filter(scenario_year == 2017) %>%
  mutate(scenario_year = 2018)

yrs <- rbind(yrs, new_yr)

write.csv(yrs, "conf/scenario_data_years.csv", row.names=FALSE)
