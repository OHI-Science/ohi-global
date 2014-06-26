# transfer global scenarios out of ohicore
# TODO: create true regions_gcs.js for Antarctica & High Seas

setwd('~/github/ohi-global')

# get paths based on host machine
dirs = list(
  neptune_data  = '/Volumes/data_edit', 
  neptune_local = '/Volumes/local_edit',
  ohiprep       = '../ohiprep',
  ohicore       = '../ohicore')

# load ohicore, development mode
library(devtools)
load_all(dirs$ohicore)

do.layercopy  = T
do.layercheck = T
do.calculate  = T
do.other      = F

# scenarios
scenarios = list(
  eez2013     = list(
    google_key   = '0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js'),
    do           = T),
  eez2012     = list(
    google_key   = '0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE',
    fld_dir      = 'dir_2012a',
    fld_fn       = 'fn_2012a',
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js'),
    do           = T),
  antarctica2014 = list(
    google_key   = '0ArcIhYsFwBeNdHNxNk1iRHc1S05KLWsyb0ZtZjRjZnc',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js'),
    do           = F),
  highseas2014   = list(
    google_key   = '0ArcIhYsFwBeNdG9KVlJ6M0ZxV1dtVDJDQ3FLVWJQWFE',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js'),
    do           = F))

# sync functions.R: overwrite eez2012 and eez2014 with eez2013 (note LE's use of eez2013 argument)
for (dir in c('eez2012','eez2014')){
  stopifnot(file.copy('eez2013/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}

for (i in 1:length(scenarios)){ # i=1
  
  # vars
  scenario   = names(scenarios)[[i]]
  fld_dir    = scenarios[[i]][['fld_dir']]
  fld_fn     = scenarios[[i]][['fld_fn']]
  google_key = scenarios[[i]][['google_key']]
  do         = scenarios[[i]][['do']]
  
  if (!do) next()
  cat(sprintf('\nScenario: %s\n', scenario))
  
  # create dirs
  dirs_scenario = c(scenario, sprintf('%s/%s', scenario, c('tmp','layers','conf','spatial')))
  for (dir in dirs_scenario) {
    if (!file.exists(dir)) dir.create(dir, showWarnings=F)
  }

  if (do.layercopy){
    # load Google spreadsheet for copying layers
    cat(sprintf('\n  Google spreadsheet editable URL:\n    https://docs.google.com/spreadsheet/ccc?key=%s\n', google_key) )
    g.url = sprintf('https://docs.google.com/spreadsheet/pub?key=%s&output=csv', scenarios[[i]][['google_key']])
    g0 = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='')
    write.csv(g0, sprintf('%s/tmp/layers_0-google.csv', scenario), na='', row.names=F)
    
    # swap dir
    g0$dir_in = sapply(
      str_split(g0[[fld_dir]], ':'),   
      function(x){ sprintf('%s/%s', dirs[x[1]], x[2])})
    g0$fn_in = g0[[fld_fn]]
    
    # filter
    lyrs = g0 %.%
      filter(ingest==T) %.%
      mutate(
        path_in        = file.path(dir_in, fn_in),
        path_in_exists = file.exists(path_in),
        filename = sprintf('%s.csv', layer),
        path_out = sprintf('%s/layers/%s', scenario, filename)) %.%
      select(
        targets, layer, layer_old, name, description, 
        fld_value, units,
        path_in, path_in_exists, filename, path_out) %.%
      arrange(targets, layer)
    write.csv(lyrs, sprintf('%s/tmp/layers_1-ingest.csv', scenario), na='', row.names=F)
    
    if (nrow(filter(lyrs, !path_in_exists)) != 0){
      message('The following layers paths do not exist:\n')
      print(filter(lyrs, !path_in_exists) %.% select(layer, path_in), row.names=F)
      stop('Resolve paths in google doc with filesystem.')
    }
  
    # copy layers
    for (j in 1:nrow(lyrs)){ # j=45
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
    
    # load configuration and layers
    conf   = Conf(sprintf('%s/conf', scenario))
    layers = Layers(layers.csv = sprintf('%s/layers.csv', scenario), 
                    layers.dir = sprintf('%s/layers', scenario))
    
    # calculate scores from directory of scenario
    setwd(sprintf('~/github/ohi-global/%s', scenario)) # load_all(dirs$ohicore)
    scores = CalculateAll(conf, layers, debug=T)  
    setwd('~/github/ohi-global')
    write.csv(scores, sprintf('%s/scores.csv', scenario), na='', row.names=F)

    # archive scores on disk (out of github, for easy retrieval later)
    csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores_%s_%s.csv', dirs$neptune_data, scenario, format(Sys.Date(), '%Y-%m-%d'))
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
    system(sprintf('open %s/launch_app.command', scenario))
  }
}

# run comparison report
source('../ohidev/report/compare_scores.R')