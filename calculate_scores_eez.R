
# STEP 1: be sure to pull ohiprep

library(devtools)
#devtools::install_github("ohi-science/ohicore@dev_resil") 
devtools::install_github("ohi-science/ohicore@dev") 
#devtools::install_github("ohi-science/ohicore@master")
#devtools::install_github("ohi-science/ohicore@master_a2015")
#install_github('rCharts', 'ramnathv')
library(ohicore)
library(zoo)

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
    do           = T) , 

  eez2015     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2015a',
    fld_fn       = 'fn_2015a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T) ,

  eez2014     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2014a',
    fld_fn       = 'fn_2014a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T),

  eez2013     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T),

  eez2012     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2012a',
    fld_fn       = 'fn_2012a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T)
)

### sync functions.R: 
# overwrite eez2012, eez2013, eez2014, eez2015 with eez2016
for (dir in c('eez2012','eez2013', 'eez2014', 'eez2015')){
  stopifnot(file.copy('eez2016/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}



for (i in 1:length(scenarios)){  #i=2
  
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
        fld_value, units,
        path_in, path_in_exists, filename, path_out) %>%
      arrange(targets, layer)
    write.csv(lyrs, sprintf('%s/temp/layers_1-ingest.csv', scenario), na='', row.names=F)
    
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
    
    # layers registry
    write.csv(dplyr::select(lyrs, -path_in, -path_in_exists, -path_out), sprintf('%s/layers.csv', scenario), row.names=F, na='')
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
  
    
    # calculate scores
    #try({    })
    scores = CalculateAll(conf, layers)
    write.csv(scores, 'scores.csv', na='', row.names=F)
    
    # restore working directory
    setwd('..') 
    
    # archive scores on disk (out of github, for easy retrieval later)
    csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores_%s_%s.csv', 
                  dirs$neptune_data, scenario, format(Sys.Date(), '%Y-%m-%d'))
    write.csv(scores, csv, na='', row.names=F)    
  }
  
  if (do.other){
    # spatial  
    for (f in scenarios[[scenario]][['f_spatial']]){ # f = f_spatial[1]
      stopifnot(file.exists(f))
      file.copy(f, sprintf('%s/spatial/%s', scenario, basename(f)))
    }
    
    # delete old shortcut files
    for (f in c('launchApp.bat','launchApp.command','launchApp_code.R','scenario.R')){
      path = sprintf('%s/%s',scenario,f)
      if (file.exists(path)) unlink(path)
    }
    
    # save shortcut files not specific to operating system
    write_shortcuts(scenario, os_files=0)
    
    # launch on Mac # setwd('~/github/ohi-global/eez2013'); launch_app()
    #system(sprintf('open %s/launch_app.command', scenario))
  }
}


### make a plot to compare different commits within a scenario

change_plot(repo = "ohi-global", scenario="eez2015", commit="previous", 
           fileSave="eez2015_tr_update_new_TCCI", save_csv=FALSE, save_png=FALSE)

source('../ohiprep/src/R/VisGlobal.R')
# looking within a goal:
scatterPlot(repo="ohi-global", scenario="antarctica2014", commit="previous", goal="HAB", dim="pressure", fileSave="antarctica_hd_sea_ice_2014")
goalHistogram(scenario="antarctica2014", goal="HAB", dim="status", fileSave="HAB_new_sea_ice")
