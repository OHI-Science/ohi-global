
# STEP 1: Be sure to pull ohiprep before running!!

# STEP 2: Install the appropriate ohicore, if necessary:
library(devtools)
#devtools::install_github("ohi-science/ohicore@master") # typicaly this version will be used
devtools::install_github("ohi-science/ohicore@dev") # used when testing new code in ohicore
#devtools::install_github("ohi-science/ohicore@master_a2015") # used if assessment was done prior to 2016 and not updated

# STEP 3: If changes are made to any of the files in eez_layers_meta_data, run these:
# updates the layers_eez.csv file to be toolbox compatible
source("eez_layers_meta_data/layers_eez_script.R") 

# updates the "layers_eez_targets.csv" file when a pressure/resilience data layer is added 
# to the pressures or resilience matrix in 
s_folder <- "eez" #indicate the scenario with the pressures/resilience matrix data
source("eez_layers_meta_data/update_targets_with_pre_res.R") 


# STEP 4: Revise relevant code below and run assessments!
library(ohicore)
library(zoo)
library(stringr)
library(readr)

setwd("../ohi-global")

source('../ohiprep/src/R/common.R')


### scenario
scenario <- "eez"

## Read in the layers.csv file with paths to the data files
g <- read.csv(sprintf("layers_%s.csv", scenario), stringsAsFactors = FALSE, na.strings='')    


# replaces 'ohiprep' portion of the filepath with the full file paths
# 'ohiprep' files are located here: https://github.com/OHI-Science/ohiprep
    g$dir = gsub("ohiprep:", "../ohiprep/", g$dir)
    
# filters the data and determines whether the file is available
    lyrs = g %>%
      filter(ingest==T) %>%
      mutate(
        path_in        = file.path(dir, fn),
        path_in_exists = file.exists(path_in),
        filename = sprintf('%s.csv', layer),
        path_out = sprintf('%s/layers/%s.csv', scenario, layer)) %>%
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
    files_extra = setdiff(list.files(sprintf('%s/layers',scenario)), as.character(lyrs$filename))
    unlink(sprintf('%s/layers/%s', scenario, files_extra))
    
# layers registry (this includes files that are ingest=FALSE)
    lyrs_reg = lyrs %>%
      select(
        targets, layer, name, description,
        fld_value, units, filename)
    write.csv(lyrs_reg, sprintf('%s/layers.csv', scenario), row.names=F, na='')


# Run check on layers      
    conf   = Conf(sprintf('%s/conf', scenario))
    
    CheckLayers(layers.csv = sprintf('%s/layers.csv', scenario), 
                layers.dir = sprintf('%s/layers', scenario), 
                flds_id    = conf$config$layers_id_fields)



# calculate scores for each year scenario and save to a single csv file:
   
  scenario_years <- c(2012:2017)
  
  scores_all_years <- data.frame()
   
  for (s_year in scenario_years){  # s_year=2012
      
    conf   <-  Conf(sprintf('%s/conf', scenario))
    layers <-  Layers(layers.csv = sprintf('%s/layers.csv', scenario), layers.dir = sprintf('%s/layers', scenario))
    layers$data$scenario_year <-  s_year 
    
    # calculate scores
    scores_sy <- CalculateAll(conf, layers) %>%
      mutate(year = s_year) 
      
    scores_all_years <- rbind(scores_all_years, scores_sy)

  }

write.csv(scores_all_years, sprintf('%s/scores.csv', scenario), na='', row.names=F)
   

### Some methods for visualizing the data

source('../ohiprep/src/R/VisGlobal.R')
### make a plot to compare different commits within a scenario

change_plot(repo = "ohi-global", scenario="eez", commit="previous", scenario_year=2016, 
            fileSave="eez2016_lsp", save_csv = TRUE)

compare <- read.csv("changePlot_figures/eez2016_lsp_diff_data_2017-08-25.csv")
ggplot(filter(compare, year==2016 & dimension=="status"), aes(old_score, score)) +
  geom_point()

## check for changes in NA's
## Both should equal 0
NA_compare <- compare %>%
  filter(year < 2017) %>%
  mutate(NA_same = ifelse(is.na(score) & is.na(old_score), 1, 0)) %>%
  mutate(NA_new = ifelse(is.na(score), 1, 0)) %>%
  mutate(NA_old = ifelse(is.na(old_score), 1, 0)) %>%
  mutate(diff_new = NA_new - NA_same) %>%
  mutate(diff_old = NA_old - NA_same) %>%
  summarize(new = sum(diff_new),
            old = sum(diff_old))
NA_compare


# looking within a goal:
scatterPlot(repo="ohi-global", scenario="eez2015", commit="previous", goal="CP", dim="pressures", fileSave="CP_pressure_eez2015")

goalHistogram(scenario="eez2016", goal="AO", dim="status", fileSave="AO_need_eez2016")

