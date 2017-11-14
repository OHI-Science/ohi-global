#### Checking that all meta data is aligned

library(dplyr)
library(tidyr)

# base
base <- read.csv("eez_layers_meta_data/layers_eez_base.csv")


# targets
targets <- read.csv("eez_layers_meta_data/layers_eez_targets.csv")

setdiff(base$layer, targets$layer) # alien species actually doesn't get included as a pressure due to all values < 1
setdiff(targets$layer, base$layer)

# methods
methods <- read.csv("eez_layers_meta_data/layers_eez_methods.csv")
setdiff(base$layer, methods$layer)
setdiff(methods$layer, base$layer) # NP calculation (exposure) is there, but it is calculated in functions.R

## specific methods
table(methods$variable) # reference done below

meth_prep <- dplyr::filter(methods, variable == "dataprep_url_2017")
setdiff(meth_prep$layer, base$layer)
setdiff(base$layer, meth_prep$layer)

meth_update <- dplyr::filter(methods, variable == "methods_update_2017")
setdiff(meth_update$layer, base$layer)
setdiff(base$layer, meth_update$layer)

# sources
source <- read.csv("eez_layers_meta_data/layers_eez_data_sources.csv")
meth_refs <- dplyr::filter(methods, variable == "ds_reference")
setdiff(source$ds_reference, meth_refs$variable_data)
setdiff(meth_refs$variable_data, source$ds_reference)

# layer files
layers <- list.files("global_supplement/layers_info", pattern="Rmd")
layers <- gsub(".Rmd", "", layers)
setdiff(base$layer, layers)
setdiff(layers, base$layer)


#gapfilling data
gap <- read.csv("eez_layers_meta_data/layers_eez_gapfill.csv") %>%
  filter(variable == "file_loc_2017")
setdiff(gap$layer, base$layer)
setdiff(base$layer, gap$layer)


### Modifications for 2017
### gapfilling no longer needs to have separate directories and file names
### will update the latest file

gaps <- read.csv("eez_layers_meta_data/layers_eez_gapfill.csv", stringsAsFactors = FALSE)

gaps <- spread(gaps, variable, variable_data) %>%
  mutate(file_loc_2015 = paste(dir_gap_fill_2015, fn_gap_fill_2015, sep="/")) %>%
  select(layer, file_loc_2015) %>%
  gather("variable", "variable_data", -1)

gaps_2017 <- gaps %>%
  mutate(variable = "file_loc_2017") %>%
  rbind(gaps)

write.csv(gaps_2017, "eez_layers_meta_data/layers_eez_gapfill.csv", row.names = FALSE)
