
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
repo_loc <- "https://raw.githubusercontent.com/OHI-Science/ohiprep_v2019/gh-pages/"

# STEP 3: Scenario years in this year's assessment
scenario_years <- c(2012:2019)

#***

## STEP 4: Set up

## load packages:
library(ohicore)
library(zoo)
library(stringr)
library(readr)
library(here)

## source file path info depending on operating system
source('https://raw.githubusercontent.com/OHI-Science/ohiprep_v2019/gh-pages/workflow/R/common.R')


#############################
## After updating a data layer
#############################

# STEP 5: Be sure to push all ohiprep changes!!

# STEP 6: Update the newest layer's file location and file name in eez_layers_meta_data/layers_eez_base.csv

# STEP 7: Make sure the appropriate data year is entered in eez/conf/scenario_data_years.csv

# STEP 8: Run following to update the layers.csv file with the latest information in layers_eez_base.csv and to reset 
source(here("eez_layers_meta_data/layers_eez_script.R"))

# If more complex changes are made to layer: such as changes to layer names, removing/adding layers, etc
# run the following scripts: 
# + eez_layers_meta_data/check_layer_harmony.R to make sure that the layer names across all the metadata files 
#   are consistent, add information to the meta data files as necessary
# + eez_layers_meta_data/update_targets_with_pre_res.R to update eez_layers_meta_data/layers_eez_targets.csv 
#    if layers are added to the resilience/pressures matrices in the eez/conf folder. 
#    matrices to the 


# STEP 9: Run scenarios!

## Read in the layers.csv file with paths to the data files
g <- read.csv(here("eez/layers.csv"), stringsAsFactors = FALSE, na.strings='')


## establish locations of layers and save information
lyrs = g %>%
  dplyr::filter(ingest==TRUE) %>%
  dplyr::mutate(dir = gsub("ohiprep:", repo_loc, dir)) %>%
  dplyr::mutate(
    path_in        = file.path(dir, fn),
    #path_in_exists = file.exists(path_in),
    filename = sprintf('%s.csv', layer),
    path_out = sprintf(here('eez/layers/%s.csv'), layer)) %>%
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

write.csv(lyrs_reg, here('eez/layers.csv'), row.names=F, na='')


# Run check on layers
conf   = ohicore::Conf(here('eez/conf'))

ohicore::CheckLayers(layers.csv = here('eez/layers.csv'),
            layers.dir = here('eez/layers'),
            flds_id    = conf$config$layers_id_fields)



# calculate scores for each year scenario and save to a single csv file:

## General function to calculate scores
get_scores <- function(s_year){ # s_year=2019

#  s_year <- as.numeric(s_year)
  print(sprintf("For assessment year %s", s_year))

  # set the scenario year
  layers$data$scenario_year <-  s_year

  # clear out the file that keeps track of reference points for each scenario year

  if(file.exists(sprintf(here('eez/temp/reference_pts_%s.csv'), s_year)))
  {file.remove(sprintf(here('eez/temp/reference_pts_%s.csv'), s_year))}

  ref_pts <- data.frame(year   = as.integer(),
                        goal   = as.character(),
                        method = as.character(),
                        reference_point = as.character())
  write_csv(ref_pts, sprintf(here('eez/temp/reference_pts_%s.csv'), s_year))


  # calculate scores
  scores_sy <- ohicore::CalculateAll(conf, layers) %>%
    dplyr::mutate(year = s_year)

}

## Apply function
### set up conf and layer objects
conf   <-  ohicore::Conf(here('eez/conf'))
layers <-  ohicore::Layers(layers.csv = here('eez/layers.csv'), layers.dir = here('eez/layers'))

#scorelist = lapply(X=2018, FUN=get_scores)
scorelist = lapply(X=scenario_years, FUN=get_scores) # 38 warnings were generated (nothing of concern)
scores_all_years <- dplyr::bind_rows(scorelist)


# save results
write.csv(scores_all_years, here('eez/scores.csv'), na='', row.names=F)


# STEP 10: Review results

### Some methods for visualizing the data

## final commit from last year: 1fdf7f2
# Link being sourced here is incorrect, need to change it!
ohicore::score_check(commit="1fdf7f2", scenario_year=2018,
            file_name="np_4pts_10yrs_final_commit", save_csv = TRUE, NA_compare = TRUE)


compare <- read.csv(here("eez/score_check/np_4pts_10yrs_previous_diff_data_2019-08-16.csv")) 

tmp <- dplyr::filter(compare, is.na(score) & !is.na(old_score)) %>%
  dplyr::filter(year==2018, dimension=="status")

library(ggplot2)
p <- ggplot(dplyr::filter(compare, year==2018 & dimension=="status" & goal == "FIS"), aes(x=old_score, y=score)) +
  geom_point(aes(text = paste0("rgn = ", rgn_name)), shape=19) +
  geom_abline(slope=1, intercept=0) +
  labs(x="catch weighting", y="no catch weighting") + 
  theme_bw()

plotly_fig <- plotly::ggplotly(p)
htmlwidgets::saveWidget(plotly::as_widget(plotly_fig), "tmp_file.html", 
                        selfcontained = TRUE)


dplyr::filter(compare, is.na(old_score) & !is.na(score))


# looking within a goal:
source("../../ohiprep_v2018/src/R/VisGlobal.R")
scatterPlot(repo="ohi-global", scenario="eez", commit="e0ed46b", filter_year=2017,
            goal="FIS", dim="status", fileSave="FIS_old_new_compare")

goalHistogram(scenario="eez2016", goal="AO", dim="status", fileSave="AO_need_eez2016")


# STEP 11: Summarize results in an issue to update team members!
# STEP 12: Update metadata files