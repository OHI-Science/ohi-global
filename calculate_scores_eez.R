
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
s_folder <- "eez2016" #indicate the scenario with the most recent pressures/resilience matrix data
source("eez_layers_meta_data/update_targets_with_pre_res.R") 


# STEP 4: Revise relevant code below and run assessments!
library(ohicore)
library(zoo)
library(stringr)

setwd("../ohi-global")

source('../ohiprep/src/R/common.R')

# new paths based on host machine
dirs = list(
  neptune_data  = dir_M, 
  neptune_local = dir_M,
  ohiprep       = '../ohiprep',
  ohicore       = '../ohicore')


do.layercopy  = T
do.layercheck = T
do.calculate  = T
do.other      = F

# scenario list (need to add new scenarios here)
scenarios = list(
  eez2016     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2016a',
    fld_fn       = 'fn_2016a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T)  
  # ,
  # eez2015     = list(
  #   layer   = 'layers_eez',
  #   fld_dir      = 'dir_2015a',
  #   fld_fn       = 'fn_2015a',
  #   f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
  #   do           = T) ,
  # 
  # eez2014     = list(
  #   layer   = 'layers_eez',
  #   fld_dir      = 'dir_2014a',
  #   fld_fn       = 'fn_2014a',
  #   f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
  #   do           = T),
  # 
  # eez2013     = list(
  #   layer   = 'layers_eez',
  #   fld_dir      = 'dir_2013a',
  #   fld_fn       = 'fn_2013a',
  #   f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
  #   do           = T),
  # 
  # eez2012     = list(
  #   layer   = 'layers_eez',
  #   fld_dir      = 'dir_2012a',
  #   fld_fn       = 'fn_2012a',
  #   f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
  #   do           = T)
)

### sync functions.R: 
# overwrite eez2012, eez2013, eez2014, eez2015 with eez2016
for (dir in c('eez2012','eez2013', 'eez2014', 'eez2015')){
  stopifnot(file.copy('eez2016/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}

for (i in 1:length(scenarios)){  #i=1
  
  # vars
  scenario   = names(scenarios)[[i]]
  fld_dir    = scenarios[[i]][['fld_dir']]
  fld_fn     = scenarios[[i]][['fld_fn']]
  layer = scenarios[[i]][['layer']]
  do         = scenarios[[i]][['do']]
  
  print(scenario)
  print(fld_dir)
  print(fld_fn)
  print(do)
  
  
  if (!do) next()
  
  cat(sprintf('\nScenario: %s\n', scenario))
  
  # create dirs
  dirs_scenario = c(scenario, sprintf('%s/%s', scenario, c('temp','layers','conf','spatial')))
  for (dir in dirs_scenario) {
    if (!file.exists(dir)) dir.create(dir, showWarnings=F)
  }
  
  if (do.layercopy){
    
    ## Read in the layers.csv file with paths to the data files
    g <- read.csv(sprintf("%s.csv", layer), stringsAsFactors = FALSE, na.strings='')    
    
    # carry forward file paths and names when no data for 2014 and/or 2015
    if (as.numeric(gsub("[a-z]", "", scenario)) > 2013){
      g = g %>%
        dplyr::mutate(
          dir_2014a = ifelse(is.na(dir_2014a), dir_2013a, dir_2014a),
          fn_2014a = ifelse(is.na(fn_2014a), fn_2013a, fn_2014a)) %>%
        dplyr::mutate(
          dir_2015a = ifelse(is.na(dir_2015a), dir_2014a, dir_2015a),
          fn_2015a = ifelse(is.na(fn_2015a), fn_2014a, fn_2015a))%>%
        dplyr::mutate(
          dir_2016a = ifelse(is.na(dir_2016a), dir_2015a, dir_2016a),
          fn_2016a = ifelse(is.na(fn_2016a), fn_2015a, fn_2016a))
    }
    
    # replaces 'ohiprep' and 'neptune_data' parts of the filepath with the full file paths
    # 'ohiprep' files are located here: https://github.com/OHI-Science/ohiprep
    # 'neptune_data' files are located on the NCEAS Neptune server
    g$dir_in = sapply(
      str_split(g[[fld_dir]], ':'),   
      function(x){ sprintf('%s/%s', dirs[x[1]], x[2])})
    
    g$fn_in = g[[fld_fn]]
    
    # filters the data and determines whether the file is available, saves a copy to tmp folder
    lyrs = g %>%
      filter(ingest==T) %>%
      mutate(
        path_in        = file.path(dir_in, fn_in),
        path_in_exists = file.exists(path_in),
        filename = sprintf('%s.csv', layer),
        path_out = sprintf('%s/layers/%s', scenario, filename)) %>%
      select(
        targets, layer, name, description,
        fld_value=name_data_fld, units,
        path_in, path_in_exists, filename, path_out) %>%
          arrange(targets, layer)
    
    # checks that all data layers are available based on file paths 
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
  }
  
  if (do.layercheck){
    # load conf
    conf   = Conf(sprintf('%s/conf', scenario))
    
    # run checks on layers
    CheckLayers(layers.csv = sprintf('%s/layers.csv', scenario), 
                layers.dir = sprintf('%s/layers', scenario), 
                flds_id    = conf$config$layers_id_fields)
    # system(sprintf('open %s/layers.csv', scenario))
  }
  
  if (do.calculate){
    # calculate scores from directory of scenario
    setwd(sprintf('%s', scenario)) # load_all(dirs$ohicore)
    
    # load configuration and layers
    conf   = Conf('conf')
    layers = Layers(layers.csv = 'layers.csv', layers.dir = 'layers')
    layers$data$scenario_year <-  as.numeric(substring(scenario, 4,7)) 
    
    # calculate scores
    scores = CalculateAll(conf, layers)
    write.csv(scores, 'scores.csv', na='', row.names=F)
    
    # restore working directory
    setwd('..') 
    
  }
  
}



### Some methods for visualizing the data

source('../ohiprep/src/R/VisGlobal.R')
### make a plot to compare different commits within a scenario

change_plot(repo = "ohi-global", scenario="eez2016", commit="ce63333", 
            fileSave="eez2016_new_ohicore_etc", save_csv = TRUE)

## check for changes in NA's
## Both should equal 0
compare <- read.csv('changePlot_figures/eez2016_new_ohicore_etc2_diff_data_2017-06-26.csv')
NA_compare <- compare %>%
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

