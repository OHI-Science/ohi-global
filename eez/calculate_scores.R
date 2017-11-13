
# STEP 1: Be sure to pull ohiprep before running!!

# STEP 2: Install the appropriate ohicore, if necessary:
library(devtools)
devtools::install_github("ohi-science/ohicore@master") # typicaly this version will be used
devtools::install_github("ohi-science/ohicore@dev") # used when testing new code in ohicore
#devtools::install_github("ohi-science/ohicore@master_a2015") # used if assessment was done prior to 2016 and not updated

# STEP 3: Set repository name
setwd("../ohi-global/eez")


# STEP 4: If changes are made to any of the files in eez_layers_meta_data, run these:
# updates the layers_eez.csv file to be toolbox compatible


source("../eez_layers_meta_data/layers_eez_script.R") 

source("../eez_layers_meta_data/update_targets_with_pre_res.R") 


# STEP 4: Revise relevant code below and run assessments!
library(ohicore)
library(zoo)
library(stringr)
library(readr)


source('../../ohiprep/src/R/common.R')



## Read in the layers.csv file with paths to the data files
g <- read.csv("layers.csv", stringsAsFactors = FALSE, na.strings='')    

# filters the data and determines whether the file is available
    lyrs = g %>%
      filter(ingest==T) %>%
      mutate(
        path_in        = file.path(dir, fn),
        path_in_exists = file.exists(path_in),
        filename = sprintf('%s.csv', layer),
        path_out = sprintf('layers/%s.csv', layer)) %>%
      select(
        targets, layer, name, description,
        fld_value=name_data_fld, units,
        path_in, path_in_exists, filename, path_out) %>%
          arrange(targets, layer)
    
# confirms that all data layers are available based on file paths 
    if (nrow(filter(lyrs, !path_in_exists)) != 0){
      message('The following layers paths do not exist:\n')
      print(filter(lyrs, !path_in_exists) %>% select(layer, path_in), row.names=F)
      stop('Data cannot be found - check file paths/names in layers.csv' )
    }
    

# copy layers into specific scenario / layers file 
    for (j in 1:nrow(lyrs)){ # j=4
      stopifnot(file.copy(lyrs$path_in[j], lyrs$path_out[j], overwrite=T))
    }
    
# delete extraneous files
    files_extra = setdiff(list.files('layers'), as.character(lyrs$filename))
    unlink(sprintf('layers/%s', files_extra))
    
# layers registry (this includes files that are ingest=FALSE)
    lyrs_reg = lyrs %>%
      select(
        targets, layer, name, description,
        fld_value, units, filename)
    write.csv(lyrs_reg, 'layers.csv', row.names=F, na='')


# Run check on layers      
    conf   = Conf(sprintf('conf'))
    
    CheckLayers(layers.csv = sprintf('layers.csv'), 
                layers.dir = sprintf('layers'), 
                flds_id    = conf$config$layers_id_fields)


# calculate scores for each year scenario and save to a single csv file:
   
  scenario_years <- c(2012:2017)
  
  scores_all_years <- data.frame()
   
  for (s_year in scenario_years){  # s_year=2017
      
    conf   <-  Conf('conf')
    layers <-  Layers(layers.csv = 'layers.csv', layers.dir = 'layers')
    layers$data$scenario_year <-  s_year 
    
    # calculate scores
    scores_sy <- CalculateAll(conf, layers) %>%
      mutate(year = s_year) 
      
    scores_all_years <- rbind(scores_all_years, scores_sy)

  }

write.csv(scores_all_years, 'scores.csv', na='', row.names=F)
   

### Some methods for visualizing the data


score_check(commit="previous", scenario_year=2017, 
            file_name="eez2017_rand", save_csv = TRUE, NA_compare = TRUE)

compare <- read.csv("../score_check/eez2016_np_fix_diff_data_2017-10-06.csv")
dplyr::filter(compare, is.na(old_score), !is.na(score))

library(ggplot2)
ggplot(filter(compare, year==2016 & dimension=="score" & goal == "NP"), aes(old_score, score)) +
  geom_point() + 
  geom_abline(slope=1, intercept=0) +
  theme_bw()


filter(compare, is.na(old_score) & !is.na(score))

# looking within a goal:
scatterPlot(repo="ohi-global", scenario="eez2015", commit="previous", goal="CP", dim="pressures", fileSave="CP_pressure_eez2015")

goalHistogram(scenario="eez2016", goal="AO", dim="status", fileSave="AO_need_eez2016")

##
read.csv("eez/scores.csv") %>%
  filter(goal %in% c("FIS", "MAR"), region_id == 188, dimension=="score")
