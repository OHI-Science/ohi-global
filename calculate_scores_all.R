
# STEP 1: be sure to pull ohiprep

library(devtools)
#devtools::install_github("ohi-science/ohicore@dev") # when testing changes to ohicore
#devtools::install_github("ohi-science/ohicore@master")
library(ohicore)
library(zoo)

setwd("~/ohi-global")

source('../ohiprep/src/R/common.R')

# new paths based on host machine
dirs = list(
  neptune_data  = dir_neptune_data, 
  neptune_local = dir_neptune_local,
  ohiprep       = '../ohiprep',
  ohicore       = '../ohicore')


do.layercopy  = T
do.layercheck = T
do.calculate  = T
do.other      = F

# scenario list (need to add new scenarios here)
scenarios = list(
#   eez2015     = list(
#     layer   = 'layers_eez',
#     fld_dir      = 'dir_2015a',
#     fld_fn       = 'fn_2015a',
#     f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
#     do           = T) ,  
#   eez2014     = list(
#     layer   = 'layers_eez',
#     fld_dir      = 'dir_2014a',
#     fld_fn       = 'fn_2014a',
#     f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
#     do           = T),
#   eez2013     = list(
#     layer   = 'layers_eez',
#     fld_dir      = 'dir_2013a',
#     fld_fn       = 'fn_2013a',
#     f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
#     do           = T),
#   eez2012     = list(
#     layer   = 'layers_eez',
#     fld_dir      = 'dir_2012a',
#     fld_fn       = 'fn_2012a',
#     f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
#     do           = T) 
#  ,
  antarctica2014 = list(
    layer   = 'layers_antarctica',
    fld_dir      = 'dir_2014a',
    fld_fn       = 'fn_2014a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T),
  antarctica2015 = list(
    layer   = 'layers_antarctica',
    fld_dir      = 'dir_2015a',
    fld_fn       = 'fn_2015a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T)
#   , 
#   highseas2014   = list(
#     layer   = 'layers_highseas',
#     fld_dir      = 'dir_2014a',
#     fld_fn       = 'fn_2014a',
#     f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
#     do           = T), 
#   highseas2015   = list(
#     layer   = 'layers_highseas',
#     fld_dir      = 'dir_2015a',
#     fld_fn       = 'fn_2015a',
#     f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
#     do           = T)
  )

### sync functions.R: 
# overwrite eez2012, eez2014, eez2015, with eez2013
for (dir in c('eez2012','eez2014', 'eez2015')){
  stopifnot(file.copy('eez2013/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}

# overwrite antarctica2015 with antarctica2014
for (dir in c('antarctica2015')){
  stopifnot(file.copy('antarctica2014/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}

# overwrite highseas2015 with highseas2014
for (dir in c('highseas2015')){
  stopifnot(file.copy('highseas2014/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}


for (i in 1:length(scenarios)){  #i=3
  
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
#     # load Google spreadsheet for copying layers
#     cat(sprintf('\n  Google spreadsheet editable URL:\n    https://docs.google.com/spreadsheet/ccc?key=%s\n', google_key) )
#     g.url = sprintf('https://docs.google.com/spreadsheets/d/%s/export?gid=0&format=csv', scenarios[[i]][['google_key']])
#     g = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='', stringsAsFactors=F)
#     write.csv(g, sprintf('%s/temp/layers_0-google.csv', scenario), na='', row.names=F)

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
          fn_2015a = ifelse(is.na(fn_2015a), fn_2014a, fn_2015a))
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
    write.csv(select(lyrs, -path_in, -path_in_exists, -path_out), sprintf('%s/layers.csv', scenario), row.names=F, na='')
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
    layers = Layers('layers.csv','layers')
  
    
    # calculate scores
    #try({    })
    scores = CalculateAll(conf, layers, debug=T)
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
## for some reason, the devtools package needs to be turned off for this to work 
detach("package:devtools", unload=TRUE)
source('../ohiprep/src/R/VisGlobal.R')
changePlot(repo="~/ohi-global", scenario="antarctica2014", commit="previous", 
           fileSave="antarctica2014_hd_sea_ice")
compare <- read.csv('figures/DataCheck/eez2015_Hackathon_julie_updates_diff_data_2015-10-21.csv')
difs_only <- filter(compare, change != 0)
table(difs_only$dimension)
tmp <- filter(compare, is.na(score) & !is.na(old_score))
tmp <- filter(compare, !is.na(score) & is.na(old_score))

# looking within a goal:
scatterPlot(repo="~/ohi-global", scenario="antarctica2014", commit="previous", goal="HAB", dim="pressure", fileSave="antarctica_hd_sea_ice_2014")
goalHistogram(scenario="antarctica2014", goal="HAB", dim="status", fileSave="HAB_new_sea_ice")

#   scenario options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
#   commit options: 'final_2014' (the final commit for the 2014 analysis), 'previous' (previous commit), a commit code (ie., 'e30e7a4')
#   saved to: ohi-global/figures/DataCheck with name from fileSave argument
### this code needs to be redone!
#source('global2014/merge_scores.R')

## Scatterplot for Antarctica:
scatterPlot <- function(repo="~/ohi-global", scenario="eez2013", commit="previous", goal, dim="score", fileSave){
  #   scenario <- "eez2013"  ## options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
  #   commit <- "final_2014"   ## 'final_2014', 'previous', a commit code (ie., 'e30e7a4')
  #   fileSave <- 'LSP_trend_data'
  #   goal <- 'LSP'
  ## Useful code: repository(repo)
  ## Useful code: commits(repo)
  
  # devtools::install_github('ropensci/git2r') # to get latest version
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  if(commit=="previous"){
    commit2 = substring(commits(repository(repo))[[1]]@sha, 1, 7)
  } else{
    if (commit == "final_2014"){
      commit2 = '4da6b4a'
    } else {commit2 = commit}
  }
  path = paste0(scenario, '/scores.csv')
  
#   names <- read.csv(sprintf("%s/layers/rgn_labels.csv", scenario)) %>%
#     filter(type=="eez") %>%
#     select(region_id=rgn_id, label)
  
  data_old <- read_git_csv(repo, commit2, path) %>%
    select(goal, dimension, region_id, old_score=score)
  
  criteria <- ~dimension == dim
  
  data_new <- read.csv(file.path(repo, path)) %>%
    left_join(data_old, by=c('goal', 'dimension', 'region_id')) %>%
    mutate(change = score-old_score) %>%
    filter_(criteria) %>%
    group_by(goal) %>% 
    mutate(mean = mean(change, na.rm=TRUE),
           sd =  sd(change, na.rm=TRUE)) %>%
    ungroup()
  
  data_new <- data_new[data_new$goal==goal,]  
  
  ggplot(data_new, aes(x=old_score, y=score)) +
    geom_point(shape=19) +
    theme_bw() + 
    labs(title=paste(scenario, goal, dim, commit, sep=": "), y="New scores", x="Scores from previous analysis") +
    geom_abline(slope=1, intercept=0, color="red") + 
    xlim(c(0,100))
  
  ggsave(file.path(repo, 'figures/DataCheck', paste0(fileSave, "_scatterPlot_", Sys.Date(), '.png')), width=10, height=8)
}



## look into following code:
# # DEBUG comparison for 2013a
#source('../ohidev/report/compare_scores.R')
#suppressWarnings(source('../ohidev/report/visualizeScores/visualizeScores.R'))

# prepare data for Radical 2012 and 2013 eez (need to add Antarctica and High Seas)
#source('../ohidev/report/radical.R')

# comparison 2014a
# source('../ohidev/report/compare_scenarios.R')

# library(git2r)
# devtools::install_github('ropensci/git2r')


repository(repo)
lookup(repository(repo), hex)
o=tree(lookup(repository(repo), hex))
dirname(path)
tools::file_path_sans_ext(basename(path))
git2r::content(o[basename(path)])
git2r::content(o)



data_new %>%
  filter(goal=="LSP",
         dimension=="score",
         region_id==2)

t=0
x=.5937
r=.2
p=.2
DISCOUNT=1
BETA=0.67

DISCOUNT * (1 + (BETA * t) + ((1-BETA) * (r - p))) * x

(1 + (0.67*trend) + ((1-0.67)*(resilience-pressures)))*status

