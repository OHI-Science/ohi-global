
##################################
### Steps 1-3 should be done once at the beginning of the assessment year
### Steps 4-11 are done as each data layer is updated
##################################


## STEP 1: download ohicore package
## Install the appropriate ohicore:
library(devtools)
#devtools::install_github("ohi-science/ohicore@master") # typicaly this version will be used
devtools::install_github("ohi-science/ohicore@dev") # used when testing new code in ohicore
#devtools::install_github("ohi-science/ohicore@master_a2015") # used if assessment was done prior to 2016 and not updated

## STEP 2:
## identify repo where data will be taken from: 
repo_loc <- "https://raw.githubusercontent.com/OHI-Science/ohiprep_v2018/master/"

# STEP 3: Scenario years in this year's assessment
scenario_years <- c(2012:2018)

#***

## STEP 4: Set up

## load packages:
library(ohicore)
library(zoo)
library(stringr)
library(readr)

## source file path info depending on operating system
source('https://raw.githubusercontent.com/OHI-Science/ohiprep_v2018/master/src/R/common.R')


# STEP 5: Set repository name
setwd("eez")


#############################
## After updating a data layer
#############################

# STEP 6: Be sure to push all ohiprep changes!!

# STEP 7: Update the newest layer's file location and file name in eez_layers_meta_data/layers_eez_base.csv

# STEP 8: Make sure the appropriate data year is entered in conf/scenario_data_years.csv

# STEP 9: Run following to update the layers.csv file with the latest information in layers_eez_base.csv and to reset 
source("../eez_layers_meta_data/layers_eez_script.R")

# If more complex changes are made to layer: such as changes to layer names, removing/adding layers, etc
# run the following scripts: 
# + eez_layers_meta_data/check_layer_harmony.R to make sure that the layer names across all the metadata files 
#   are consistent, add information to the meta data files as necessary
# + eez_layers_meta_data/update_targets_with_pre_res.R to update eez_layers_meta_data/layers_eez_targets.csv 
#    if layers are added to the resilience/pressures matrices in the eez/conf folder. 
#    matrices to the 


# STEP 10: Run scenarios!

## Read in the layers.csv file with paths to the data files
g <- read.csv("layers.csv", stringsAsFactors = FALSE, na.strings='')


## establish locations of layers and save information
lyrs = g %>%
  dplyr::filter(ingest==T) %>%
  dplyr::mutate(dir = gsub("ohiprep:", repo_loc, dir)) %>%
  dplyr::mutate(
    path_in        = file.path(dir, fn),
    #path_in_exists = file.exists(path_in),
    filename = sprintf('%s.csv', layer),
    path_out = sprintf('layers/%s.csv', layer)) %>%
  dplyr::select(
    targets, layer, name, description,
    fld_value=name_data_fld, units,
    path_in, filename, path_out, ingest) %>%  # path_in_exists
  dplyr::arrange(targets, layer)


# copy layers into layers folder
for (j in 1:nrow(lyrs)){ # j=4
  tmp <- read.csv(lyrs$path_in[j])
  write.csv(tmp, lyrs$path_out[j], row.names=FALSE)
}

# delete extraneous files
files_extra = setdiff(list.files('layers'), as.character(lyrs$filename))
unlink(sprintf('layers/%s', files_extra))

# layers registry (this includes files that are ingest=FALSE)
lyrs_reg = lyrs %>%
  dplyr::select(
    targets, ingest, layer, name, description,
    fld_value, units, filename)
write.csv(lyrs_reg, 'layers.csv', row.names=F, na='')


# Run check on layers
conf   = ohicore::Conf(sprintf('conf'))

ohicore::CheckLayers(layers.csv = sprintf('layers.csv'),
            layers.dir = sprintf('layers'),
            flds_id    = conf$config$layers_id_fields)


# calculate scores for each year scenario and save to a single csv file:

scores_all_years <- data.frame()

for (s_year in scenario_years){  # s_year=2018

  print(sprintf("For assessment year %s", s_year))
  
  conf   <-  ohicore::Conf('conf')
  layers <-  ohicore::Layers(layers.csv = 'layers.csv', layers.dir = 'layers')
  layers$data$scenario_year <-  s_year

  # clear out the file that keeps track of reference points for each scenario year
  
  if(file.exists(sprintf('temp/reference_pts_%s.csv', s_year)))
  {file.remove(sprintf('temp/reference_pts_%s.csv', s_year))}
  
  ref_pts <- data.frame(year   = as.integer(),
                        goal   = as.character(),
                        method = as.character(),
                        reference_point = as.character())
  write_csv(ref_pts, sprintf('temp/reference_pts_%s.csv', s_year))
  
  
  # calculate scores
  scores_sy <- ohicore::CalculateAll(conf, layers) %>%
    dplyr::mutate(year = s_year)

  scores_all_years <- rbind(scores_all_years, scores_sy)

}

# save results
write.csv(scores_all_years, 'scores.csv', na='', row.names=F)


# STEP 11: Review results

### Some methods for visualizing the data


ohicore::score_check(commit="previous", scenario_year=2017,
            file_name="lsp", save_csv = TRUE, NA_compare = TRUE)

compare <- read.csv("score_check/ico_diff_data_2018-07-06.csv") 

tmp <- compare %>%
  dplyr::filter(is.na(old_score) & !is.na(score) & goal == "NP")

table(tmp$goal)

tmp <- compare %>%
  dplyr::filter(change>0)
table(tmp$goal)

dplyr::filter(compare, is.na(old_score), !is.na(score))

library(ggplot2)
ggplot(dplyr::filter(compare, year==2017 & dimension=="score" & goal == "MAR"), aes(old_score, score)) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  theme_bw()


dplyr::filter(compare, is.na(old_score) & !is.na(score))

# looking within a goal:
scatterPlot(repo="ohi-global", scenario="eez", commit="previous", 
            goal="CP", dim="pressures", fileSave="CP_pressure_eez2015")

goalHistogram(scenario="eez2016", goal="AO", dim="status", fileSave="AO_need_eez2016")


# STEP 12: Summarize results in an issue to update team members!
# STEP 13: Update metadata files