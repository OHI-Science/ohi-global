#### Checking that all meta data is aligned

library(dplyr)
library(tidyr)
library(here)

# base
base <- read.csv(here("metadata_documentation/layers_eez_base.csv"))


# targets
targets <- read.csv(here("metadata_documentation/layers_eez_targets.csv"))

setdiff(base$layer, targets$layer) # alien species actually doesn't get included as a pressure due to all values < 1
                                   # soft-bottom extent not used for anything
setdiff(targets$layer, base$layer)

# methods
dataprep <- read.csv("metadata_documentation/layers_eez_dataprep.csv")

setdiff(dataprep$layer, base$layer)
setdiff(base$layer, dataprep$layer)

update <- read.csv("metadata_documentation/layers_eez_updates.csv")
setdiff(update$layer, base$layer)
setdiff(base$layer, update$layer)

refs <- read.csv("metadata_documentation/layers_eez_reference.csv")
setdiff(refs$layer, base$layer)
setdiff(base$layer, refs$layer)

# sources
refs <- read.csv("metadata_documentation/layers_eez_reference.csv")
source <- read.csv("metadata_documentation/layers_eez_data_sources.csv")
setdiff(source$ds_reference, refs$reference)
setdiff(refs$reference, source$ds_reference)


# layer files
layers <- list.files(here("metadata_documentation/ohi_model/layers_info"), pattern="Rmd")
layers <- gsub(".Rmd", "", layers)
setdiff(base$layer, layers)
setdiff(layers, base$layer)


#gapfilling data
gap <- read.csv(here("metadata_documentation/layers_eez_gapfill.csv"))
setdiff(gap$layer, base$layer) # NP_exposure_gf created in functions.R
setdiff(base$layer, gap$layer)

