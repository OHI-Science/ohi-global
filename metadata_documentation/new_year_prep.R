# Preparing files for a new assessment year!
# This adds an additional assessment year to the eez/conf/scenario_data_years.csv and 
#   is used to create a layers worksheet used to keep track of progress while updating layers.


library(dplyr)
library(here)


### scenario_data_year prep
# The data year is the same as the previous assessment year.
# As each data layer is added, the data year will be manually updated
# directly within the eez/conf/scenario_data_years.csv
# NOTE: The years will need to be updated

# NOTE: After this, update the calculate_scores.R file with the latest links, etc.
# Then run calculate_scores.R to make sure there are no changes

yrs <- read.csv(here("eez/conf/scenario_data_years.csv"))
new_yr <- yrs %>%
  filter(scenario_year == 2020) %>%  # indicate year of previous assessment
  mutate(scenario_year = 2021) %>%       # indicate this year's assessment
  arrange(layer_name, scenario_year, data_year)

yrs <- rbind(yrs, new_yr) %>%
  arrange(layer_name, scenario_year, data_year)

write.csv(yrs, here("eez/conf/scenario_data_years.csv"), row.names=FALSE)


#### layers spreadsheet
#   (this is created here, but is typically copied to a GoogleSheet to make easier to work with)
# delete the created file after copying to Google sheets!

targets <- read.csv(here('metadata_documentation/layers_eez_targets.csv'), stringsAsFactors=FALSE) %>%
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
meta <- read.csv(here("metadata_documentation/layers_eez_base.csv"))

layers <- meta %>% 
  dplyr::left_join(targets, by="layer") %>%
  dplyr::mutate(notes = NA,
                "prep status" = NA) %>%
  dplyr::select("layer name" = name, layer, directory = dir, "file name" = fn, targets, notes, "prep status")

write.csv(layers, "metadata_documentation/layers_forGoogleSheet.csv", row.names=FALSE)
