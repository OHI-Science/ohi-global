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

# scenarios
scenarios = list(
  eez2013     = list(
    name_old     = 'Global2013.www2013',    
    google_key   = '0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js')),
  eez2012     = list(
    name_old     = 'Global2012.www2013',
    google_key   = '0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE',
    fld_dir      = 'dir_2012a',
    fld_fn       = 'fn_2012a',
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js')),
  antarctica2014 = list(
    name_old     = 'Antarctica2013.a2014',
    google_key   = '0ArcIhYsFwBeNdHNxNk1iRHc1S05KLWsyb0ZtZjRjZnc',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js')),
  highseas2014   = list(
    name_old     = 'HighSeas2013.a2014',
    google_key   = '0ArcIhYsFwBeNdG9KVlJ6M0ZxV1dtVDJDQ3FLVWJQWFE',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a'),
    f_spatial    = c('../ohicore/inst/extdata/spatial.www2013/regions_gcs.js'))

# read-only: https://docs.google.com/spreadsheet/pub?key=[google_key]
# editable:  https://docs.google.com/spreadsheet/ccc?key=[google_key]

for (i in 1:length(scenarios)){ # i=1
  
  # vars
  scenario_old = scenarios[[i]][['name_old']]
  scenario_new = names(scenarios)[[i]]
  fld_dir      = scenarios[[i]][['fld_dir']]
  fld_fn       = scenarios[[i]][['fld_fn']]
  dir_conf_in  = sprintf('%s/inst/extdata/conf.%s'  , dirs$ohicore, scenario_old)

  cat(sprintf('\nScenario: %s -> %s\n', scenario_old, scenario_new))
  
  # create dirs
  dirs_scenario = c(scenario_new, sprintf('%s/%s', scenario_new, c('tmp','layers','conf','spatial')))
  for (dir in dirs_scenario) { # dir = dirs_scenario[3]
    if (!file.exists(dir)) dir.create(dir, showWarnings=F)
  }

  # load Google spreadsheet for copying layers
  cat(sprintf('\n  Google spreadsheet editable URL:\n    https://docs.google.com/spreadsheet/ccc?key=%s\n', scenarios[[i]][['google_key']]) )
  g.url = sprintf('https://docs.google.com/spreadsheet/pub?key=%s&output=csv', scenarios[[i]][['google_key']])
  g0 = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='')
  write.csv(g0, sprintf('%s/tmp/layers_0-google.csv', scenario_new), na='', row.names=F)

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
      path_out = sprintf('%s/layers/%s', scenario_new, filename)) %.%
    select(
      targets, layer, layer_old, name, description, 
      fld_value, units,
      path_in, path_in_exists, filename, path_out) %.%
    arrange(targets, layer)
  write.csv(lyrs, sprintf('%s/tmp/layers_1-ingest.csv', scenario_new), na='', row.names=F)
  
  if (nrow(filter(lyrs, !path_in_exists)) != 0){
    message('The following layers paths do not exist:\n')
    print(filter(lyrs, !path_in_exists) %.% select(layer, path_in), row.names=F)
    stop('Resolve paths in google doc with filesystem.')
  }

  # copy layers
  for (j in 1:nrow(lyrs)){ # j=1
    stopifnot(file.copy(lyrs$path_in[j], lyrs$path_out[j], overwrite=T))
  }
    
  # delete extraneous files
  files_extra = setdiff(list.files(sprintf('%s/layers',scenario_new)), as.character(lyrs$filename))
  unlink(sprintf('%s/layers/%s', scenario_new, files_extra))
  
  # layers registry
  write.csv(select(lyrs, -path_in, -path_in_exists, -path_out), sprintf('%s/layers.csv', scenario_new), row.names=F, na='')
  
  # order for layers for substitution old to new name in files
  lyrs = lyrs %.%
    arrange(desc(nchar(as.character(layer_old))))  
  
  # copy configuration files
  conf_files = c('config.R','functions.R','goals.csv','pressures_matrix.csv','resilience_matrix.csv','resilience_weights.csv')
  for (f in conf_files){ # f = conf_files[1]
    
    f_in  = sprintf('%s/%s', dir_conf_in, f)
    f_out = sprintf('%s/conf/%s', scenario_new, f)
    
    cat(sprintf('%s sub layer_old with layer (new):\n', f))
    
    
    # substitute old layer names with new
    s = readLines(f_in, warn=F, encoding='UTF-8')
    for (i in 1:nrow(lyrs)){ # i=1
      cat(sprintf('  %s -> %s\n', lyrs$layer_old[i], lyrs$layer[i]))
      s = gsub(lyrs$layer_old[i], lyrs$layer[i], s, fixed=T)
      
      # fix LE for new scores path
      s = gsub('inst/extdata/scores.Global2013.www2013.csv', '../eez2013/scores.csv', s, fixed=T)      
    }
    writeLines(s, f_out)    
  }
  
  # load conf
  conf   = Conf(sprintf('%s/conf', scenario_new))
  
  # run checks on layers
  CheckLayers(layers.csv = sprintf('%s/layers.csv', scenario_new), 
              layers.dir = sprintf('%s/layers', scenario_new), 
              flds_id    = conf$config$layers_id_fields)
  
  # load layers
  layers = Layers(layers.csv = sprintf('%s/layers.csv', scenario_new), 
                  layers.dir = sprintf('%s/layers', scenario_new))
  
  # calculate scores from directory of scenario
  setwd(sprintf('~/github/ohi-global/%s', scenario_new)) # load_all(dirs$ohicore)
  scores = CalculateAll(conf, layers, debug=T)  
  setwd('~/github/ohi-global')
  write.csv(scores, sprintf('%s/scores.csv', scenario_new), na='', row.names=F)
  
  # archive scores on disk (out of github, for easy retrieval later)
  csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores_%s_%s.csv', dirs$neptune_data, scenario_new, format(Sys.Date(), '%Y-%m-%d'))
  write.csv(scores, csv, na='', row.names=F)
   
  # spatial  
  for (f in scenarios[[scenario_new]][['f_spatial']]){ # f = f_spatial[1]
    stopifnot(file.exists(f))
    file.copy(f, sprintf('%s/spatial/%s', scenario_new, basename(f)))
  }
  
   # delete old shortcut files
   for (f in c('launchApp.bat','launchApp.command','launchApp_code.R','scenario.R')){
     path = sprintf('%s/%s',scenario_new,f)
     if (file.exists(path)) unlink(path)
   }
   
  # save shortcut files not specific to operating system
  write_shortcuts(scenario_new, os_files=0)
  
  # launch on Mac # setwd('~/github/ohi-global/eez2013'); launch_app()
  system(sprintf('open %s/launch_app.command', scenario_new))
}

# create global scenario to combine the scores
if (!file.exists('global2014')) dir.create('global2014', showWarnings=F)