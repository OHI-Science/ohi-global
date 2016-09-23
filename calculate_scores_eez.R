
# STEP 1: be sure to pull ohiprep

### NOTE: There is a warning that some regions have a MAR score, but a zero weight
###  This seems fine.  It appears to be because we use a running average to calculate MAR scores
###  but a running average isn't used to calculate the weights.  In these cases, the score is entirely based
###  on the FIS score.

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
    do           = T),

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

change_plot(repo = "ohi-global", scenario="eez2016", commit="previous", 
           fileSave="eez2016_fileNameChanges", save_csv=FALSE, save_png=FALSE)

data <- read.csv('changePlot_figures/NP_trend_compare_eez2016.csv')
plot(data$old_score, data$score, xlab="old trends", ylab="new trends")
abline(0,1, col="red")

data <- read.csv('eez2016/scores.csv') %>%
  filter(goal =="LSP") %>%
  filter(dimension == "future")

source('../ohiprep/src/R/VisGlobal.R')
# looking within a goal:
scatterPlot(repo="ohi-global", scenario="eez2014", commit="previous", goal="AO", dim="status", fileSave="AO_need_eez2014")

goalHistogram(scenario="eez2014", goal="AO", dim="status", fileSave="AO_need_eez2014_no_ln_trans_max_hist")

## make an interactive table

library(hwriter)
names <- read.csv("eez2013/layers/rgn_labels.csv") %>%
  filter(type=="eez") %>%
  select(region_id=rgn_id, label)


data <- read.csv('eez2015/scores.csv')%>%
  filter(goal=='FIS', dimension=="status") %>%
  filter(region_id !=0) %>%
  left_join(names) %>%
  select(country=label, goal, dimension, score) %>%
  arrange(score)
hwrite(data, "changePlot_figures/status_eez2015_FIS.html", br=TRUE, center=TRUE, border=0, 
       row.style=list(goal='text-align:center'))


tmp <- read.csv('changePlot_figures/MAR_trend_compare_eez2016.csv')
plot(tmp$old_score, tmp$score, xlab="old trends", ylab="new trends")
abline(0,1, col="red")
#### compare fis data from previous commit
repo = "ohi-global"
scenario="eez2016"
commit="previous"

repo2 <- sprintf("../%s", repo)

if (commit == "previous") {
  commit2 = substring(git2r::commits(git2r::repository(repo2))[[1]]@sha, 
                      1, 7)
} else {
  if (commit == "final_2014") {
    commit2 = "4da6b4a"
  } else {
    commit2 = commit
  }
}

tmp <- git2r::remote_url(git2r::repository(repo2))
org <- stringr::str_split(tmp, "/")[[1]][4]
path = paste0(scenario, "/layers/fis_meancatch.csv")
data_old <- read_git_csv(paste(org, repo, sep = "/"), commit2, path) %>%
  filter(rgn_id==188) %>%
  filter(year==2010) %>%
  select(rgn_id, stock_id_taxonkey, year, mean_catch_old=mean_catch)

data_new <- read.csv('eez2016/layers/fis_meancatch.csv') %>%
  filter(year==2010) %>%
  filter(rgn_id==188) %>%
  left_join(data_old)

path = paste0(scenario, "/layers/fis_b_bmsy.csv")
data_old <- read_git_csv(paste(org, repo, sep = "/"), commit2, path) %>%
  filter(rgn_id==188) %>%
  filter(year==2010) %>%
  select(rgn_id, stock_id, year, bbmsy_old=bbmsy) %>%
  arrange(stock_id)

data_new <- read.csv('eez2016/layers/fis_b_bmsy.csv') %>%
  filter(year==2010) %>%
  filter(rgn_id==188) %>%
#  left_join(data_old) %>%
  arrange(stock_id)

####################################################  
###### Checking different fishery scores

repo = "ohi-global"
scenario="eez2015"
commit="final_2015"

if (commit == "previous") {
  commit2 = substring(git2r::commits(git2r::repository(repo2))[[1]]@sha, 
                      1, 7)
} else {
  if (commit == "final_2015") {
    commit2 = "1d4dcb1"                              ### final_2014 == "4da6b4a"
  } else {
    commit2 = commit
  }
}

repo2 <- sprintf("../%s", repo)
tmp <- git2r::remote_url(git2r::repository(repo2))
org <- stringr::str_split(tmp, "/")[[1]][4]
path = paste0(scenario, "/scores.csv")
data_old <- read_git_csv(paste(org, repo, sep = "/"), commit2, path) %>%
  filter(goal == "FIS") %>%
  filter(dimension == "score") %>%
  select(region_id, data2015_methods2015=score)
write.csv(data_old, "changePlot_figures/FIS_compare_data/scores_2015data_2015methods.csv", row.names=FALSE)

## Model 2
data <- read.csv('eez2016/scores.csv')%>%
  filter(goal == "FIS") %>%
  filter(dimension == "score") %>%
  select(region_id, data2016_methods2015=score)
write.csv(data, "changePlot_figures/FIS_compare_data/scores_2016data_2015methods.csv", row.names=FALSE)

## Model 3
data <- read.csv('eez2016/scores.csv')%>%
  filter(goal == "FIS") %>%
  filter(dimension == "score") %>%
  select(region_id, data2016_methods_taxa_penalty=score)
write.csv(data, "changePlot_figures/FIS_compare_data/scores_2016data_taxa_penalty.csv", row.names=FALSE)

## Model 4
data <- read.csv('eez2016/scores.csv')%>%
  filter(goal == "FIS") %>%
  filter(dimension == "score") %>%
  select(region_id, data2016_methods_wt_mean=score)
write.csv(data, "changePlot_figures/FIS_compare_data/scores_2016data_wt_mean.csv", row.names=FALSE)

## Model 5
data <- read.csv('eez2016/scores.csv')%>%
  filter(goal == "FIS") %>%
  filter(dimension == "score") %>%
  select(region_id, data2016_methods_taxa_penalty_wt_mean=score)
write.csv(data, "changePlot_figures/FIS_compare_data/scores_2016data_taxa_penalty_wt_mean.csv", row.names=FALSE)

## Model 6
data <- read.csv('eez2016/scores.csv')%>%
  filter(goal == "FIS") %>%
  filter(dimension == "score") %>%
  select(region_id, data2016_methods_penalty_no_underfish=score)
write.csv(data, "changePlot_figures/FIS_compare_data/scores_2016data_no_underfish_pen.csv", row.names=FALSE)

##

### Now compare the data
s1 <- read.csv("changePlot_figures/FIS_compare_data/scores_2015data_2015methods.csv")
s2 <- read.csv("changePlot_figures/FIS_compare_data/scores_2016data_2015methods.csv")
s3 <- read.csv("changePlot_figures/FIS_compare_data/scores_2016data_taxa_penalty.csv") 
s4 <- read.csv("changePlot_figures/FIS_compare_data/scores_2016data_wt_mean.csv") 
s5 <- read.csv("changePlot_figures/FIS_compare_data/scores_2016data_taxa_penalty_wt_mean.csv")
s6 <- read.csv("changePlot_figures/FIS_compare_data/scores_2016data_no_underfish_pen.csv")

data <- cbind(s1,s2,s3,s4,s5,s6)
names(data)
repeats <- which(names(data)=="region_id")
repeats <- repeats[-1]
data <- data[, -repeats]

labels <- read.csv('eez2016/layers/rgn_labels.csv') %>%
  select(region_id=rgn_id, country=label) %>%
  unique()

data <- data %>%
  left_join(labels)

write.csv(data, 'changePlot_figures/FIS_compare_data/allData.csv', row.names=FALSE)
